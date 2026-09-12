open Core
open Runner
module P = Agent_protocol
module H = Execution_host

type finish =
  | Release
  | Cancel
[@@deriving sexp]

let probe =
  {|<shell_access id="probe-runtime" cwd="${workspace}">
<capabilities sandbox="direct_unsafe" network="false" child_processes="true" arbitrary_code="true" privilege_change="false">
  <read path="${tool_dir}"/><read path="${workspace}"/><write path="${workspace}"/>
</capabilities>
<backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
<limits wall_time="10s" max_stdin="0B" stdout="4KiB" stderr="4KiB" total_output="8KiB"/>
<policy default="allow"/><audit format="none"/>
</shell_access>
<tool name="probe" type="shell" mode="script" runtime="probe-runtime" result="structured"
      script="${tool_dir}/probe.sh" interpreter="/bin/sh" executable="false"/>|}
;;

let probe_script =
  {|#!/bin/sh
set -eu
printf '%s\n' "$$" > probe.pid
printf 'started\n' >> probe.started
while [ ! -f probe.release ]; do /bin/sleep 0.02; done
printf 'probe-complete\n'
|}
;;

let root ~probe binding =
  "<developer>Execute the asynchronous evaluation.</developer>\n"
  ^ "<authoring_context policy=\"manual\"/>\n"
  ^ probe
  ^ "\n\
     <script id=\"observer\" language=\"chatml\" kind=\"moderator\" \
     api=\"extensibility-v1\" src=\"candidate.chatml\"/>\n"
  ^ binding
;;

let field = Execution_cases.field

let sources ~probe candidate =
  [ "agent.chatmd", root ~probe (field candidate "binding" |> Jsonaf.string_exn)
  ; "candidate.chatml", field candidate "source" |> Jsonaf.string_exn
  ; "input.json", field candidate "input_schema" |> Jsonaf.to_string
  ; "output.json", field candidate "output_schema" |> Jsonaf.to_string
  ]
;;

let binding_validation ~env candidate =
  match candidate with
  | `Object fields
    when List.equal
           String.equal
           [ "binding"; "input_schema"; "output_schema"; "source" ]
           (List.map fields ~f:fst |> List.sort ~compare:String.compare) ->
    (match
       Chatmd_source_bundle.create
         ~root_file:"agent.chatmd"
         ~sources:(sources ~probe:"<tool name=\"probe\"/>" candidate)
         ()
       |> Result.ok_or_failwith
       |> Prompt.Chat_markdown.parse_source_bundle ~dir:(Eio.Stdenv.cwd env)
     with
     | { root =
           [ Developer _
           ; Authoring_context _
           ; Tool (Builtin "probe")
           ; Extension_script _
           ; Tool (Extension tool)
           ]
       ; _
       } ->
       (match tool.name, tool.implementation, tool.uses, tool.completion_schema with
        | "begin_work", Moderator "observer", [], None -> Valid
        | _ ->
          Invalid
            ( Capability
            , "expected begin_work owned by observer with the host's moderator \
               capabilities" ))
     | _ -> Invalid (Capability, "unexpected background declaration")
     | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
     | exception exn -> Invalid (Semantics, Exn.to_string exn))
  | _ -> Invalid (Semantics, "expected source, binding, input_schema and output_schema")
;;

let notifications (snapshot : P.Snapshot.t) =
  List.filter snapshot.canonical_history.entries ~f:(fun entry ->
    match entry.provenance with
    | Runtime_notification _ -> true
    | _ -> false)
;;

let validate ~env ~host ~capabilities candidate =
  match binding_validation ~env candidate with
  | Invalid _ as failure -> failure
  | Valid ->
    Chat_response.Authoring_validation.validate
      ~env
      ~host
      ~capabilities
      (`Object
          [ "version", `Number "1"
          ; "target", `String "moderator"
          ; "source", field candidate "source"
          ; "tools", `Array [ `String "probe" ]
          ])
    |> Reference_backend.classification
;;

let wait env ready =
  let rec loop () =
    match ready () with
    | true -> ()
    | false ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      loop ()
  in
  loop ()
;;

let execute_checked ?audit ?replay_job_delivery ~env ~finish candidate =
  match binding_validation ~env candidate with
  | Invalid (kind, message) -> Failed (kind, message)
  | Valid ->
    let initial_id = ref None in
    let pid = ref None in
    let after_ack ~workspace embedded initial =
      H.require (List.length initial.P.Snapshot.jobs = 1) "expected exactly one probe job";
      let job_id =
        match H.outcome initial "begin" with
        | Pending (Job id, `Object fields) ->
          let json = `Object fields in
          H.require
            (match Jsonaf.member "job_id" json, Jsonaf.member "status" json with
             | Some (`String actual), Some (`String "accepted") ->
               String.equal actual (P.Id.Job.to_string id) && List.length fields = 2
             | _ -> false)
            "background acknowledgement lost its job ID or accepted status";
          id
        | _ ->
          raise
            (H.Scenario_failure
               "background candidate did not acknowledge with its job reference")
      in
      initial_id := Some job_id;
      H.require
        (List.is_empty (notifications initial))
        "completion was published before probe release";
      let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
      wait env (fun () ->
        Eio.Path.is_file (file "probe.started") && Eio.Path.is_file (file "probe.pid"));
      pid := Some (Eio.Path.load (file "probe.pid") |> String.strip |> Pid.of_string);
      let current = H.snapshot embedded in
      let job = List.find_exn current.jobs ~f:(fun job -> P.Id.Job.equal job.id job_id) in
      (match job.status with
       | Running -> ()
       | _ -> raise (H.Scenario_failure "probe was not running at acknowledgement"));
      H.require
        (String.equal (Eio.Path.load (file "probe.started")) "started\n")
        "probe started more than once";
      match finish with
      | Release ->
        Eio.Path.save ~create:(`Exclusive 0o600) (file "probe.release") "release"
      | Cancel ->
        ignore
          (H.request
             embedded
             (Job_cancel
                { session_id = H.session_id embedded
                ; attachment_id = (H.attachment embedded).id
                ; job_id
                ; idempotency_key =
                    P.Idempotency_key.of_string "evaluation:cancel" |> H.get
                })
           : P.Method_result.t)
    in
    let settled snapshot =
      H.require
        (List.length snapshot.P.Snapshot.jobs <= 1)
        "unexpected additional probe job";
      H.require
        (List.for_all snapshot.P.Snapshot.extension_status ~f:(fun status ->
           match status.kind, status.state with
           | Moderator_execution, ("failed" | "failed.retired") -> false
           | _ -> true))
        "background moderator execution failed";
      H.require
        (List.length (notifications snapshot) <= 1)
        "duplicate completion notification";
      let process_stopped =
        match !pid with
        | None -> false
        | Some pid ->
          (match
             Eio_unix.run_in_systhread (fun () -> Signal_unix.send Signal.zero (`Pid pid))
           with
           | `No_such_process -> true
           | `Ok -> false)
      in
      List.length snapshot.P.Snapshot.jobs = 1
      && List.for_all snapshot.jobs ~f:(fun job ->
        match job.delivery with
        | Delivered _ -> true
        | _ -> false)
      && List.length (notifications snapshot) = 1
      && process_stopped
    in
    let snapshot =
      H.run
        ?audit
        ?replay_job_delivery
        ~env
        ~background:
          { after_ack
          ; settled
          ; final_requests =
              (match finish with
               | Release -> 3
               | Cancel -> 2)
          }
        ~sources:(sources ~probe candidate @ [ "probe.sh", probe_script ])
        ~workspace_files:[]
        ~calls:[ "begin", "begin_work", `Object [] ]
        ()
    in
    let job = List.hd_exn snapshot.jobs in
    H.require
      (P.Id.Job.equal job.id (Option.value_exn !initial_id))
      "completed job differs from the acknowledged job";
    let completion = P.Job.terminal_completion job |> H.get in
    let notification = List.hd_exn (notifications snapshot) in
    let data =
      match
        Agent_session.History_codec.of_protocol notification
        |> H.get
        |> History_entry.item
      with
      | Openai.Responses.Item.Input_message
          { role = User; content = Text { text; _ } :: _; _ } ->
        String.lsplit2_exn text ~on:'\n' |> snd |> Jsonaf.of_string
      | _ -> failwith "notification did not use supported user-message framing"
    in
    let delivered = P.Completion.of_json (Jsonaf.member_exn "completion" data) |> H.get in
    H.require
      (P.Completion.equal delivered (Option.value_exn completion))
      "notification changed the retained job completion";
    (match P.Invocation.work_of_json (Jsonaf.member_exn "work" data) |> H.get with
     | Job id when P.Id.Job.equal id job.id -> ()
     | _ -> raise (H.Scenario_failure "notification lost the job correlation"));
    let correct =
      match finish, completion with
      | Release, Some (Succeeded (`String text)) ->
        let value = Jsonaf.of_string text in
        String.equal
          (Jsonaf.member_exn "stdout" value |> Jsonaf.string_exn)
          "probe-complete\n"
      | Cancel, Some (Cancelled _) -> true
      | _ -> false
    in
    (match correct with
     | true -> Passed
     | _ ->
       Failed
         ( Semantics
         , "background completion or process cleanup did not satisfy the scenario" ))
;;

let execute ?audit ?replay_job_delivery ~env ~finish candidate =
  match execute_checked ?audit ?replay_job_delivery ~env ~finish candidate with
  | result -> result
  | exception H.Scenario_failure message -> Failed (Semantics, message)
  | exception Eio.Time.Timeout ->
    Failed (Infrastructure, "background scenario exceeded its host deadline")
;;
