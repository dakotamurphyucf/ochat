open Core
module F = Crash_recovery_fixture
module B = Support.Background_fixture
module C = Support.Config_fixture
module P = Agent_protocol
module I = P.Invocation
module J = P.Job

let source =
  {|<developer>Call watch once.</developer>
<tool name="append_to_file"><write path="${workspace}"/></tool>
<script id="watch" language="chatml" kind="tool">
let run ctx input =
  let* job = Job.start_tool("append_to_file", `Object([
    { key = "path"; value = `String("EFFECT_PATH") },
    { key = "content"; value = `String("executed") }
  ])) in
  Task.pure(`Pending(`Job(job), `String("accepted")))
</script>
<tool name="watch" type="chatml" script="watch" entrypoint="run" input_schema="any.json" output_schema="string.json" completion_schema="string.json"><uses tool="append_to_file"/></tool>
|}
;;

let with_host env environment fixture boundary f =
  Eio.Switch.run (fun sw ->
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"queued-launch"
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
  |> Option.value_exn ~message:"queued launch checkpoint missing"
;;

let root state =
  List.find_exn state.Agent_session.Session_state.invocations ~f:(fun invocation ->
    I.equal_origin invocation.context.origin Model
    && String.equal invocation.context.tool_name "watch")
;;

let test env environment =
  let fixture = F.fixture env environment "queued-chatml-launch" in
  let marker = Filename.concat (C.physical_workspace fixture) "queued.effect" in
  F.write
    env
    (C.prompt_path fixture)
    (String.substr_replace_all
       source
       ~pattern:"\"EFFECT_PATH\""
       ~with_:(Jsonaf.to_string (`String marker)));
  let directory = Filename.dirname (C.prompt_path fixture) in
  F.write env (Filename.concat directory "any.json") "true";
  F.write env (Filename.concat directory "string.json") {|{"type":"string"}|};
  F.write
    env
    (C.config_path fixture)
    (C.configuration fixture ()
     |> String.substr_replace_all
          ~pattern:"(tool_default deny)"
          ~with_:"(tool_default allow)");
  let session, before =
    with_host env environment fixture "job-intent" (fun child client ->
      let session = B.create client "queued:create" in
      ignore
        (F.request
           client
           (Session_send_message
              { session_id = session.summary.id
              ; attachment_id = session.attachment_id
              ; content = { kind = Plain_text; text = "Call watch."; attachments = [] }
              ; idempotency_key = F.key "queued:send"
              })
         : P.Method_result.t);
      F.await_marker env child "notification-boundary job-intent";
      F.kill env child;
      let before = state env fixture session in
      F.require (List.length before.jobs = 1) "intent did not persist exactly one job";
      let job = List.hd_exn before.jobs in
      let invocation = root before in
      (match job.status, job.attempt, job.started_at, job.launch, invocation.status with
       | ( Queued
         , 0
         , None
         , Some { owner = Invocation owner; _ }
         , Resolved (Pending (Job id, `String "accepted")) ) ->
         F.require
           (P.Id.Invocation.equal owner invocation.context.id && P.Id.Job.equal id job.id)
           "job intent and owner outcome disagree"
       | _ -> F.fail "missed the committed intent before launch boundary");
      F.require
        (Option.is_none invocation.output_entry_id)
        "acknowledgement already published";
      F.require (List.is_empty before.deliveries) "unlaunched work produced a delivery";
      F.require
        (not (Eio.Path.is_file (F.path env marker)))
        "worker ran before intent was released";
      session, before)
  in
  let original_job = List.hd_exn before.jobs in
  let original_invocation = root before in
  let previous = ref None in
  for reopen = 1 to 2 do
    with_host env environment fixture "recover" (fun child client ->
      let snapshot =
        F.await_notifications
          env
          child
          client
          session
          ~provider_prefix:"notification-provider "
          ~calls:(if reopen = 1 then 1 else 0)
          ~count:1
      in
      F.kill env child;
      let recovered = state env fixture session in
      let job = List.hd_exn recovered.jobs in
      let invocation = root recovered in
      F.require
        (List.length recovered.jobs = 1 && P.Id.Job.equal original_job.id job.id)
        "recovery duplicated or replaced the saved job";
      (match job.status, job.attempt, job.delivery with
       | Succeeded, 1, Delivered _ -> ()
       | _ -> F.fail "queued job did not execute and deliver exactly once");
      F.require
        (Option.equal J.equal_launch original_job.launch job.launch)
        "recovery changed launch ownership";
      F.require
        (I.equal_context original_invocation.context invocation.context)
        "recovery changed the owner context";
      (match original_invocation.status, invocation.status with
       | Resolved expected, Published actual ->
         F.require
           (I.equal_outcome expected actual)
           "recovery changed the acknowledgement"
       | _ -> F.fail "owner response was not published");
      F.require_equal
        "one external execution"
        [%sexp_of: string]
        "\nexecuted"
        (F.read env marker);
      let delivery = List.hd_exn recovered.deliveries in
      F.require
        (Option.equal I.equal_work delivery.context.work (Some (Job job.id)))
        "notification belongs to another job";
      (match delivery.context.completion with
       | Succeeded (`String output) ->
         F.require
           (String.is_prefix output ~prefix:"Content appended to ")
           "native job did not report append success"
       | _ -> F.fail "queued job delivered a different completion");
      F.require_equal
        "live and saved history"
        [%sexp_of: P.History.entry list]
        snapshot.canonical_history.entries
        recovered.conversation.canonical_history;
      let retained =
        job, invocation, delivery, recovered.conversation.canonical_history
      in
      (match !previous with
       | None -> previous := Some retained
       | Some expected ->
         F.require_equal
           "second daemon preserves job, receipt, notification and history"
           [%sexp_of: J.t * I.t * P.Delivery.t * P.History.entry list]
           expected
           retained);
      Agent_session.Invocation_history.validate_retained
        ~history:recovered.conversation.canonical_history
        invocation
      |> F.protocol_ok)
  done
;;
