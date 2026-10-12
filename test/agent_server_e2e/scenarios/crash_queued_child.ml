open Core
module F = Crash_recovery_fixture
module B = Support.Background_fixture
module C = Support.Config_fixture
module P = Agent_protocol
module Request = Chat_response.Background_request
module Store = Agent_store.Prompt_artifact_store

let source =
  {|<developer>Call watch once.</developer>
<tool name="child" agent="child.chatmd" local/>
<script id="watch" language="chatml" kind="tool">
let run ctx input =
  let* job = Job.start_tool("child", `Object([
    { key = "input"; value = `String("Run selected child.") }
  ])) in
  Task.pure(`Pending(`Job(job), `String("accepted")))
</script>
<tool name="watch" type="chatml" script="watch" entrypoint="run" input_schema="any.json" output_schema="string.json" completion_schema="string.json"><uses tool="child"/></tool>
|}
;;

let selected_source = "<developer>selected child B</developer>"

let answer =
  let open Openai.Responses.Response_stream in
  Output_item_done
    { item =
        Output_message
          { role = Assistant
          ; id = "selected-child-answer"
          ; content = [ { annotations = []; text = "selected B"; _type = "output_text" } ]
          ; status = "completed"
          ; phase = None
          ; _type = "message"
          }
    ; output_index = 0
    ; type_ = "response.output_item.done"
    }
;;

let run_child env ~config_path ~boundary =
  let calls = ref 0 in
  let provider ~sw:_ ~inputs =
    incr calls;
    let texts =
      List.concat_map inputs ~f:(function
        | Openai.Responses.Item.Input_message message ->
          List.filter_map message.content ~f:(function
            | Text { text; _ } -> Some (String.strip text)
            | Image _ -> None)
        | _ -> [])
    in
    Eio.Flow.copy_string
      (sprintf "notification-provider %d\n" !calls)
      (Eio.Stdenv.stdout env);
    match boundary, !calls with
    | "job-intent", 1 -> Stdlib.List.to_seq Crash_notification_host.call
    | "recover", 1 ->
      F.require
        (List.mem texts "selected child B" ~equal:String.equal)
        "recovered child did not use its selected source";
      F.require
        (not (List.mem texts "original child A" ~equal:String.equal))
        "recovered child used the parent's older captured source";
      Stdlib.List.to_seq [ answer ]
    | "recover", 2 ->
      F.require
        (List.exists texts ~f:(String.is_prefix ~prefix:"Ochat runtime notification."))
        "recovered parent lacks the completed child's notification";
      Stdlib.Seq.empty
    | _ -> F.fail "unexpected provider call across selected-child recovery"
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
        ~case:"queued-child"
        ~arguments:[ "queued-child"; C.config_path fixture; boundary ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "notification-host-ready";
        F.with_client ~sw env fixture (fun client -> f child client)))
;;

let state env fixture session =
  B.checkpoint env fixture session
  |> Option.value_exn ~message:"selected child checkpoint missing"
;;

let selection (job : P.Job.t) =
  Request.of_json ~policy:Chat_response.One_off_request.default_policy job.payload
  |> F.protocol_ok
  |> Request.agent_artifact
  |> Option.value_exn ~message:"actual Factory did not capture queued child code"
;;

let artifact env fixture revision_id =
  let data = Agent_store.Data_root.create ~env ~path:(C.data_dir fixture) |> F.store_ok in
  let store =
    Store.open_existing ~env ~root:(Agent_store.Data_root.prompt_artifacts_path data)
    |> F.store_ok
  in
  Store.load store revision_id |> F.store_ok
;;

let check_artifact env fixture selected =
  let artifact = artifact env fixture (Request.Agent_artifact.revision_id selected) in
  F.require
    (String.equal
       artifact.manifest_sha256
       (Chatml_source.Digest.to_hex (Request.Agent_artifact.manifest_sha256 selected)))
    "retention changed the selected artifact manifest";
  F.require
    (String.equal artifact.root_chatmd selected_source)
    "selected artifact is not B"
;;

let test env environment =
  let fixture = F.fixture env environment "selected-child-source" in
  let directory = Filename.dirname (C.prompt_path fixture) in
  let child_path = Filename.concat directory "child.chatmd" in
  F.write env (C.prompt_path fixture) source;
  F.write env child_path "<developer>original child A</developer>";
  F.write env (Filename.concat directory "any.json") "true";
  F.write env (Filename.concat directory "string.json") {|{"type":"string"}|};
  F.write
    env
    (C.config_path fixture)
    (C.configuration fixture ()
     |> String.substr_replace_all
          ~pattern:"(tool_default deny)"
          ~with_:"(tool_default allow)");
  let session, original =
    with_host env environment fixture "job-intent" (fun child client ->
      let session = B.create client "selected-child:create" in
      F.write env child_path selected_source;
      ignore
        (F.request
           client
           (Session_send_message
              { session_id = session.summary.id
              ; attachment_id = session.attachment_id
              ; content = { kind = Plain_text; text = "Call watch."; attachments = [] }
              ; idempotency_key = F.key "selected-child:send"
              ; timing = P.Pending_input.Timing.Safe_boundary
              })
         : P.Method_result.t);
      F.await_marker env child "notification-boundary job-intent";
      F.kill env child;
      let saved = state env fixture session in
      F.require (List.length saved.jobs = 1) "expected exactly one admitted child";
      let parent = artifact env fixture saved.spec.prompt_revision_id in
      F.require
        (List.exists parent.sources ~f:(fun source ->
           String.equal source.Store.Source.relative_path "child.chatmd"
           && String.equal source.contents "<developer>original child A</developer>"))
        "parent did not retain its original child A";
      let job = List.hd_exn saved.jobs in
      (match job.status, job.attempt, job.started_at, job.launch with
       | Queued, 0, None, Some _ -> ()
       | _ -> F.fail "child ran before the durable admission boundary");
      check_artifact env fixture (selection job);
      session, job)
  in
  Eio.Path.unlink (F.path env child_path);
  let previous = ref None in
  for reopen = 1 to 2 do
    with_host env environment fixture "recover" (fun child client ->
      ignore
        (F.await_notifications
           env
           child
           client
           session
           ~provider_prefix:"notification-provider "
           ~calls:(if reopen = 1 then 2 else 0)
           ~count:1
         : P.Public.Snapshot.Fields.t);
      F.kill env child;
      let saved = state env fixture session in
      F.require (List.length saved.jobs = 1) "recovery duplicated a child job";
      let job = List.hd_exn saved.jobs in
      F.require (P.Id.Job.equal original.id job.id) "recovery replaced the child job";
      F.require
        (Jsonaf.exactly_equal original.payload job.payload)
        "recovery changed the admitted source request";
      F.require
        (Option.equal P.Job.equal_launch original.launch job.launch)
        "recovery changed the launch owner";
      (match job.status, job.attempt, job.delivery with
       | Succeeded, 1, Delivered _ -> ()
       | _ -> F.fail "selected child did not execute and deliver exactly once");
      let delivery = List.hd_exn saved.deliveries in
      F.require
        (Option.equal P.Invocation.equal_work delivery.context.work (Some (Job job.id)))
        "notification belongs to another child job";
      (match delivery.context.completion with
       | Succeeded (`String "selected B") -> ()
       | _ -> F.fail "selected child result was not retained");
      check_artifact env fixture (selection job);
      let retained = job, delivery, saved.conversation.canonical_history in
      match !previous with
      | None -> previous := Some retained
      | Some expected ->
        F.require_equal
          "second restart retains selected code result and history"
          [%sexp_of: P.Job.t * P.Delivery.t * P.History.entry list]
          expected
          retained)
  done
;;
