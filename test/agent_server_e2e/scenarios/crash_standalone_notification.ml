open Core
module F = Crash_recovery_fixture
module P = Agent_protocol
module B = Support.Background_fixture
module C = Support.Config_fixture
module Process = Support.Process_manager

let source =
  {|<developer>Call watch once.</developer>
<shell_access id="fixture" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="false" child_processes="true" arbitrary_code="true" privilege_change="false">
    <read path="${tool_dir}"/><read path="${workspace}"/><write path="${workspace}"/>
  </capabilities>
  <backends merge="replace"><direct when="macos"/><direct when="linux"/></backends>
  <limits wall_time="10s" max_stdin="0B" stdout="256KiB" stderr="4KiB" total_output="260KiB"/>
  <policy default="allow"/><audit format="none"/>
</shell_access>
<tool name="work" type="shell" mode="script" runtime="fixture" result="structured" script="${tool_dir}/work.sh" interpreter="/bin/sh" executable="false"/>
<script id="watch" language="chatml" kind="tool" max_output="512KiB" max_value="1MiB">
let run ctx input =
  let* job = Job.start_tool("work", `Object([])) in
  Task.pure(`Pending(`Job(job), `String("accepted")))
</script>
<tool name="watch" type="chatml" script="watch" entrypoint="run" input_schema="any.json" output_schema="string.json" completion_schema="any.json"><uses tool="work"/></tool>
|}
;;

let work =
  {|#!/bin/sh
set -eu
printf 'executed\n' >> standalone.effect
while [ ! -f work.release ]; do /bin/sleep 0.02; done
i=0
while [ "$i" -lt 2048 ]; do
  printf 'PRIVATE-RETAINED-RESULT-0123456789012345678901234567890123456789\n'
  i=$((i + 1))
done
|}
;;

let with_host env environment fixture boundary f =
  Eio.Switch.run (fun sw ->
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"standalone-notification"
        ~arguments:[ "standalone-notification"; C.config_path fixture; boundary ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "notification-host-ready";
        F.with_client ~sw env fixture (fun client -> f child client)))
;;

let state env fixture session =
  B.checkpoint env fixture session
  |> Option.value_exn ~message:"native checkpoint missing"
;;

let result_reference (delivery : P.Delivery.t) =
  let projection =
    Option.value_exn
      delivery.completion_projection
      ~message:"completion projection missing"
  in
  match projection.result_reference with
  | Some reference -> reference
  | None ->
    raise_s
      [%sexp
        "native crash result did not produce a reference"
      , (projection : P.Completion_projection.t)
      , (delivery.context.completion : P.Completion.t)]
;;

let frames state =
  List.filter
    state.Agent_session.Session_state.conversation.canonical_history
    ~f:(fun entry ->
      match entry.P.History.provenance with
      | Runtime_notification _ -> true
      | _ -> false)
;;

let settled state =
  Option.is_none state.Agent_session.Session_state.active_operation
  &&
  match state.deliveries with
  | [ { status = Committed _; wake_disposition = Some (Accepted_wake _); _ } ] -> true
  | _ -> false
;;

let read_artifact client session ~reopen (reference : P.Job_artifact.t) =
  let attached, _ =
    B.attach client session.B.summary (sprintf "standalone:read:%d" reopen)
  in
  let bytes = Buffer.create 4096 in
  let rec read offset =
    match
      F.request
        client
        (Blob_read
           { session_id = reference.session_id
           ; attachment_id = attached.attachment_id
           ; blob_id = reference.blob.id
           ; offset
           ; max_bytes = 16384
           })
    with
    | Blob_read chunk ->
      Buffer.add_string bytes (Base64.decode_exn chunk.data_base64);
      (match chunk.eof with
       | true -> ()
       | false -> read chunk.next_offset)
    | _ -> F.fail "artifact read returned another method"
  in
  read 0L;
  Buffer.contents bytes |> Jsonaf.of_string |> P.Completion.of_json |> F.protocol_ok
;;

let run env environment boundary =
  let fixture = F.fixture env environment ("standalone-notification-" ^ boundary) in
  F.write env (C.prompt_path fixture) source;
  let directory = Filename.dirname (C.prompt_path fixture) in
  F.write env (Filename.concat directory "any.json") "true";
  F.write env (Filename.concat directory "string.json") {|{"type":"string"}|};
  F.write env (Filename.concat (Filename.dirname (C.config_path fixture)) "work.sh") work;
  F.write
    env
    (C.config_path fixture)
    (C.configuration fixture ()
     |> String.substr_replace_all
          ~pattern:"(tool_default deny)"
          ~with_:"(tool_default allow)"
     |> String.substr_replace_all
          ~pattern:"(manifest_authorization deny)"
          ~with_:"(manifest_authorization assume_authorized)");
  let effect_path = Filename.concat (C.physical_workspace fixture) "standalone.effect" in
  let session, before =
    with_host env environment fixture boundary (fun child client ->
      let session = B.create client "standalone:create" in
      ignore
        (F.request
           client
           (Session_send_message
              { session_id = session.summary.id
              ; attachment_id = session.attachment_id
              ; content = { kind = Plain_text; text = "Call watch."; attachments = [] }
              ; idempotency_key = F.key "standalone:send"
              })
         : P.Method_result.t);
      F.await_marker env child "notification-provider 2 frames=0";
      F.write env (Filename.concat (C.physical_workspace fixture) "work.release") "finish";
      (match boundary with
       | "pending" -> ()
       | _ ->
         ignore
           (B.await env "standalone intent" (fun () ->
              let state = state env fixture session in
              Option.some_if (List.length state.deliveries = 1) ())
            : unit);
         F.write
           env
           (Filename.concat (Filename.dirname (C.config_path fixture)) "provider.release")
           "finish");
      F.await_marker env child ("notification-boundary " ^ boundary);
      F.kill env child;
      let before = state env fixture session in
      F.require (Option.is_none before.moderator) "standalone session gained a moderator";
      F.require
        (List.length before.deliveries = 1)
        "standalone intent missing or duplicated";
      let delivery = List.hd_exn before.deliveries in
      let job = List.hd_exn before.jobs in
      let reference = result_reference delivery in
      P.Job_result_reference.validate_job reference job |> F.protocol_ok;
      F.require
        (Option.is_some reference.artifact)
        "crash fixture did not persist an artifact";
      (match job.delivery with
       | Delivered _ -> ()
       | _ -> F.fail "intent and job marker were not atomic");
      F.require_equal
        "single original effect"
        [%sexp_of: string]
        "executed\n"
        (F.read env effect_path);
      (match boundary, delivery.status, delivery.wake_disposition with
       | "pending", Pending, None ->
         F.require (List.is_empty (frames before)) "pending data already inserted"
       | "committed", Committed _, Some Pending_wake ->
         F.require (List.length (frames before) = 1) "committed frame missing"
       | "accepted", Committed _, Some (Accepted_wake id) ->
         F.require
           (P.Id.Operation.equal id (Option.value_exn before.active_operation).id)
           "wrong accepted operation"
       | _ -> F.fail "standalone crash boundary was missed");
      session, before)
  in
  let old_delivery = List.hd_exn before.deliveries in
  let old_job = List.hd_exn before.jobs in
  let prior_frames = ref (frames before) in
  for reopen = 1 to 2 do
    with_host env environment fixture "recover" (fun child client ->
      let recovered =
        try
          B.await env "standalone notification recovery" (fun () ->
            let current = state env fixture session in
            Option.some_if (settled current) current)
        with
        | exn ->
          let current = state env fixture session in
          raise_s
            [%sexp
              "native recovery did not settle"
            , (boundary : string)
            , (reopen : int)
            , (exn : Exn.t)
            , (current.lifecycle : Agent_session.Session_state.Lifecycle.t)
            , (current.failure : P.Error.t option)
            , (List.map current.deliveries ~f:(fun value ->
                 value.status, value.wake_disposition)
               : (P.Delivery.status * P.Delivery.wake_disposition option) list)
            , ((Process.stdout child).contents : string)]
      in
      F.require
        (Option.is_none recovered.failure && Option.is_none recovered.moderator)
        "recovery changed standalone runtime state";
      let delivery = List.hd_exn recovered.deliveries in
      F.require
        (P.Id.Delivery.equal old_delivery.context.id delivery.context.id)
        "delivery identity changed";
      F.require
        (P.Delivery.equal_context old_delivery.context delivery.context)
        "delivery payload changed";
      F.require_equal
        "retained business job"
        P.Job.sexp_of_t
        old_job
        (List.hd_exn recovered.jobs);
      let current_frames = frames recovered in
      F.require
        (List.length current_frames = 1)
        "standalone history lost or repeated data";
      (match !prior_frames with
       | [] -> prior_frames := current_frames
       | previous ->
         F.require_equal
           "stable native frame"
           [%sexp_of: P.History.entry list]
           previous
           current_frames);
      let reference = result_reference delivery in
      let completion =
        read_artifact client session ~reopen (Option.value_exn reference.artifact)
      in
      let stored = P.Job.terminal_result old_job |> F.protocol_ok |> Option.value_exn in
      F.require
        (P.Stored_completion.matches stored completion |> F.protocol_ok)
        "artifact result changed on recovery";
      let expected_calls =
        if (not (String.equal boundary "accepted")) && reopen = 1 then 1 else 0
      in
      for _ = 1 to 10 do
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.03;
        let calls =
          String.split_lines (Process.stdout child).contents
          |> List.count ~f:(String.is_prefix ~prefix:"notification-provider ")
        in
        F.require (calls = expected_calls) "standalone wake was lost or replayed"
      done;
      F.require_equal
        "no repeated external work"
        [%sexp_of: string]
        "executed\n"
        (F.read env effect_path);
      F.require_equal
        "native recovery wake accounting"
        [%sexp_of: int]
        (if String.equal boundary "accepted" then 0 else 1)
        (Option.value_exn recovered.automatic_turn_budget).followup_turns;
      F.kill env child)
  done
;;

let test env environment =
  List.iter [ "pending"; "committed"; "accepted" ] ~f:(run env environment)
;;
