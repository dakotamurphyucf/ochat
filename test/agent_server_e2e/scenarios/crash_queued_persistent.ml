open Core
module F = Crash_recovery_fixture
module B = Support.Background_fixture
module C = Support.Config_fixture
module P = Agent_protocol
module Response = Openai.Responses.Response_stream
module Request = Chat_response.Background_request
module Store = Agent_store.Prompt_artifact_store
module Sessions = Agent_store.Session_store
module Delegations = Agent_store.Delegation_store
module Submission = Agent_session.Managed_submission

let parent_source =
  {|<developer>Queue watch once.</developer>
<tool name="reviewer" agent="child.chatmd" local persistence="optional"/>
<script id="watch" language="chatml" kind="tool">
let run ctx input =
  let* job = Job.start_tool("reviewer", `Object([
    { key = "input"; value = `String("Run the retained nested tool.") };
    { key = "mode"; value = `String("persistent") }
  ])) in
  Task.pure(`Pending(`Job(job), `String("accepted")))
</script>
<tool name="watch" type="chatml" script="watch" entrypoint="run" input_schema="any.json" output_schema="string.json" completion_schema="any.json"><uses tool="reviewer"/></tool>
|}
;;

let child_source label =
  sprintf
    {|<developer>persistent child %s</developer>
<tool name="nested" agent="nested.chatmd" local source_mode="captured"/>|}
    label
;;

let nested_source label = sprintf "<developer>nested child %s</developer>" label

let answer id text =
  Response.Output_item_done
    { item =
        Output_message
          { role = Assistant
          ; id
          ; content = [ { annotations = []; text; _type = "output_text" } ]
          ; status = "completed"
          ; phase = None
          ; _type = "message"
          }
    ; output_index = 0
    ; type_ = "response.output_item.done"
    }
;;

let nested_call =
  let arguments = {|{"input":"Answer from the retained nested source."}|} in
  let call : Openai.Responses.Function_call.t =
    { name = "nested"
    ; arguments = ""
    ; call_id = "retained-nested-call"
    ; _type = "function_call"
    ; id = Some "retained-nested-item"
    ; status = None
    }
  in
  [ Response.Output_item_added
      { item = Function_call call
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { arguments
      ; item_id = "retained-nested-item"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ; Output_item_done
      { item = Function_call { call with arguments; status = Some "completed" }
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ]
;;

let texts inputs =
  List.concat_map inputs ~f:(function
    | Openai.Responses.Item.Input_message message ->
      List.filter_map message.content ~f:(function
        | Text { text; _ } -> Some (String.strip text)
        | Image _ -> None)
    | _ -> [])
;;

let require_source inputs selected original =
  let actual = texts inputs in
  F.require (List.mem actual selected ~equal:String.equal) "wrong selected child source";
  F.require
    (not (List.mem actual original ~equal:String.equal))
    "child execution used original captured A"
;;

let tool_output_text = function
  | Openai.Responses.Tool_output.Output.Text text -> Some text
  | Content [ Input_text { text } ] -> Some text
  | Content _ -> None
;;

let run_child env ~config_path ~boundary =
  let calls = ref 0 in
  let provider ~sw:_ ~inputs =
    Int.incr calls;
    Eio.Flow.copy_string
      (sprintf "persistent-provider %d\n" !calls)
      (Eio.Stdenv.stdout env);
    match boundary, !calls with
    | "job-intent", 1 -> Stdlib.List.to_seq Crash_notification_host.call
    | "recover", 1 ->
      require_source inputs "persistent child B" "persistent child A";
      Stdlib.List.to_seq nested_call
    | "recover", 2 ->
      require_source inputs "nested child B" "nested child A";
      Stdlib.List.to_seq [ answer "nested-B-answer" "nested B result" ]
    | "recover", 3 ->
      require_source inputs "persistent child B" "persistent child A";
      F.require
        (List.exists inputs ~f:(function
           | Openai.Responses.Item.Function_call_output { output; _ } ->
             Option.exists (tool_output_text output) ~f:(fun text ->
               String.equal (String.strip text) "nested B result")
           | _ -> false))
        "persistent child did not receive its selected nested result";
      Stdlib.List.to_seq [ answer "persistent-B-answer" "persistent B result" ]
    | "recover", 4 ->
      F.require
        (List.exists
           (texts inputs)
           ~f:(String.is_prefix ~prefix:"Ochat runtime notification."))
        "parent did not receive the persistent child notification";
      Stdlib.Seq.empty
    | _ -> F.fail "unexpected provider call across persistent source recovery"
  in
  Crash_notification_host.run ~standalone:true ~provider env ~config_path ~boundary
;;

let with_host env environment fixture boundary f =
  Eio.Switch.run (fun sw ->
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"queued-persistent"
        ~arguments:[ "queued-persistent"; C.config_path fixture; boundary ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "notification-host-ready";
        F.with_client ~sw env fixture (fun client -> f child client)))
;;

let state env fixture session =
  B.checkpoint env fixture session
  |> Option.value_exn ~message:"persistent source checkpoint missing"
;;

let selection (job : P.Job.t) =
  Request.of_json ~policy:Chat_response.One_off_request.default_policy job.payload
  |> F.protocol_ok
  |> Request.agent_artifact
  |> Option.value_exn ~message:"Factory did not capture the queued persistent source"
;;

let artifact env fixture id =
  let data = Agent_store.Data_root.create ~env ~path:(C.data_dir fixture) |> F.store_ok in
  Store.open_existing ~env ~root:(Agent_store.Data_root.prompt_artifacts_path data)
  |> F.store_ok
  |> fun store -> Store.load store id |> F.store_ok
;;

let contains_source (artifact : Store.Artifact.t) path contents =
  List.exists artifact.sources ~f:(fun source ->
    String.equal source.Store.Source.relative_path path
    && String.equal source.contents contents)
;;

let check_selected env fixture selected =
  let saved = artifact env fixture (Request.Agent_artifact.revision_id selected) in
  F.require
    (String.equal
       saved.manifest_sha256
       (Chatml_source.Digest.to_hex (Request.Agent_artifact.manifest_sha256 selected)))
    "selected persistent artifact manifest changed";
  F.require (String.equal saved.root_chatmd (child_source "B")) "artifact root is not B";
  F.require
    (contains_source saved "nested.chatmd" (nested_source "B"))
    "selected artifact did not capture nested B"
;;

let ledger env fixture =
  Eio.Switch.run (fun sw ->
    let store =
      Sessions.open_existing
        ~env
        ~sw
        ~root:(C.data_dir fixture)
        ~process_start_identity:None
        ~lock_nonce:"queued-persistent-proof"
      |> F.store_ok
    in
    Exn.protect
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () -> Sessions.close store |> F.store_ok))
      ~f:(fun () ->
        F.require
          (List.length (Sessions.list_sessions store) = 2)
          "child creation duplicated";
        match
          Delegations.with_records
            (Sessions.delegations store)
            ~max_records:8
            ~max_bytes:1_048_576
            ~f:(fun records -> Ok records)
          |> F.store_ok
        with
        | [ record ] -> record
        | _ -> F.fail "persistent delegation record missing or duplicated"))
;;

let result json =
  let fields = P.Json_codec.fields json |> F.protocol_ok in
  let id =
    P.Json_codec.required_as fields "session_id" P.Id.Session.of_json |> F.protocol_ok
  in
  let status =
    P.Json_codec.required_as fields "status" P.Json_codec.string |> F.protocol_ok
  in
  F.require (String.equal status "completed") "persistent submission did not complete";
  let receipt = P.Json_codec.required fields "receipt" |> F.protocol_ok in
  let receipt_fields = P.Json_codec.fields receipt |> F.protocol_ok in
  let receipt_id =
    P.Json_codec.required_as receipt_fields "receipt_id" P.History.Id.of_json
    |> F.protocol_ok
  in
  id, receipt_id
;;

let completion delivery =
  match delivery.P.Delivery.context.completion with
  | Succeeded json -> result json
  | Failed _ | Cancelled _ | Expired -> F.fail "persistent job did not succeed"
;;

let require_child_history entries =
  let module Payload = History_entry.Payload in
  let semantics =
    List.map entries ~f:(fun entry ->
      Agent_session.History_codec.of_canonical entry
      |> F.protocol_ok
      |> History_entry.payload
      |> Payload.semantic
      |> Payload.Semantic.view)
  in
  F.require
    (List.exists semantics ~f:(function
       | Message { role; content; _ } ->
         Payload.Role.equal role Assistant
         && List.exists content ~f:(function
           | Text { text; _ } -> String.equal text "persistent B result"
           | Refusal _ | Image _ | Unknown _ -> false)
       | Call _ | Result _ | Reasoning _ | Unknown _ -> false))
    "persistent child lost its actual B answer";
  F.require
    (List.exists semantics ~f:(function
       | Result { output; _ } ->
         (match output with
          | Text text | Content [ Text { text; _ } ] ->
            String.equal text "nested B result"
          | Content _ -> false)
       | Message _ | Call _ | Reasoning _ | Unknown _ -> false))
    "persistent child lost the actual selected nested output"
;;

let test env environment =
  let fixture = F.fixture env environment "queued-persistent-source" in
  let directory = Filename.dirname (C.prompt_path fixture) in
  let child_path = Filename.concat directory "child.chatmd" in
  let nested_path = Filename.concat directory "nested.chatmd" in
  F.write env (C.prompt_path fixture) parent_source;
  F.write env child_path (child_source "A");
  F.write env nested_path (nested_source "A");
  F.write env (Filename.concat directory "any.json") "true";
  F.write env (Filename.concat directory "string.json") {|{"type":"string"}|};
  F.write
    env
    (C.config_path fixture)
    (C.configuration fixture ()
     |> String.substr_replace_all
          ~pattern:"(tool_default deny)"
          ~with_:"(tool_default allow)");
  let session, original, parent_revision =
    with_host env environment fixture "job-intent" (fun child client ->
      let session = B.create client "queued-persistent:create" in
      F.write env child_path (child_source "B");
      F.write env nested_path (nested_source "B");
      ignore
        (F.request
           client
           (Session_send_message
              { session_id = session.summary.id
              ; attachment_id = session.attachment_id
              ; content = { kind = Plain_text; text = "Call watch."; attachments = [] }
              ; idempotency_key = F.key "queued-persistent:send"
              ; timing = P.Pending_input.Timing.Safe_boundary
              })
         : P.Method_result.t);
      F.await_marker env child "notification-boundary job-intent";
      F.kill env child;
      let saved = state env fixture session in
      let original =
        match saved.jobs with
        | [ job ] -> job
        | _ -> F.fail "expected one queued persistent job"
      in
      (match original.status, original.attempt, original.started_at, original.launch with
       | Queued, 0, None, Some _ -> ()
       | _ -> F.fail "persistent child ran before committed queue boundary");
      let parent = artifact env fixture saved.spec.prompt_revision_id in
      F.require
        (contains_source parent "child.chatmd" (child_source "A")
         && contains_source parent "nested.chatmd" (nested_source "A"))
        "parent did not retain original A sources";
      check_selected env fixture (selection original);
      session, original, saved.spec.prompt_revision_id)
  in
  Eio.Path.unlink (F.path env child_path);
  Eio.Path.unlink (F.path env nested_path);
  let previous = ref None in
  for reopen = 1 to 2 do
    with_host env environment fixture "recover" (fun child client ->
      ignore
        (F.await_notifications
           env
           child
           client
           session
           ~provider_prefix:"persistent-provider "
           ~calls:(if reopen = 1 then 4 else 0)
           ~count:1
         : P.Public.Snapshot.Fields.t);
      let observed = F.get client session.summary.id in
      let completed_result =
        match observed.jobs with
        | [ { result = Some result; _ } ] -> result
        | _ -> F.fail "expected one completed persistent child job"
      in
      let child_id, receipt_id = result completed_result in
      let child_summary = (F.get client child_id).session in
      let child_session, _ =
        B.attach client child_summary (sprintf "child:attach:%d" reopen)
      in
      F.kill env child;
      let saved = state env fixture session in
      let child_state = state env fixture child_session in
      require_child_history child_state.conversation.canonical_history;
      let job, delivery =
        match saved.jobs, saved.deliveries with
        | [ job ], [ delivery ] -> job, delivery
        | _ -> F.fail "persistent queue or delivery duplicated"
      in
      F.require (P.Id.Job.equal original.id job.id) "recovery replaced persistent job";
      F.require
        (Jsonaf.exactly_equal original.payload job.payload)
        "queued source request changed";
      F.require
        (Option.equal P.Job.equal_launch original.launch job.launch)
        "persistent launch owner changed";
      F.require
        (P.Id.Prompt_revision.equal parent_revision saved.spec.prompt_revision_id)
        "parent revision changed while selecting child code";
      (match job.status, job.attempt, job.delivery with
       | Succeeded, 1, Delivered _ -> ()
       | _ -> F.fail "persistent job did not execute and deliver exactly once");
      F.require
        (Option.equal P.Invocation.equal_work delivery.context.work (Some (Job job.id)))
        "persistent notification belongs to another job";
      [%test_eq: P.Id.Session.t * P.History.Id.t]
        (completion delivery)
        (child_id, receipt_id);
      check_selected env fixture (selection job);
      let record = ledger env fixture in
      (match
         ( record.stage
         , record.revocation
         , record.admission.lifetime
         , record.admission.authored_tool )
       with
       | Linked, None, Owned, Some { name = "reviewer"; _ } -> ()
       | _ -> F.fail "persistent delegation is not the admitted authored owner");
      F.require
        (P.Id.Session.equal child_id record.admission.child_session_id
         && P.Id.Prompt_revision.equal
              (Request.Agent_artifact.revision_id (selection job))
              record.admission.revision_id)
        "delegation did not retain queued B identities";
      F.require
        (Option.equal
           Delegations.Reference.equal
           child_state.spec.delegation
           (Some (Delegations.reference record)))
        "child checkpoint is not bound to its original reservation";
      let submission =
        match child_state.managed_submissions with
        | [ submission ] -> submission
        | _ -> F.fail "persistent input receipt was duplicated"
      in
      F.require
        (P.History.Id.equal receipt_id submission.history_id)
        "wrong persistent receipt";
      (match submission.status with
       | Terminal (Some _, Completed) -> ()
       | Deferred | Ready | Assigned _ | Terminal _ ->
         F.fail "submission lacks actual completion");
      let retained =
        ( job
        , delivery
        , record
        , submission
        , saved.conversation.canonical_history
        , child_state.conversation.canonical_history )
      in
      match !previous with
      | None -> previous := Some retained
      | Some expected ->
        F.require_equal
          "second restart retains persistent code, identities, result and histories"
          [%sexp_of:
            P.Job.t
            * P.Delivery.t
            * Delegations.record
            * Submission.t
            * P.History.entry list
            * P.History.entry list]
          expected
          retained)
  done
;;
