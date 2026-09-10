open Core
module F = Crash_recovery_fixture
module P = Agent_protocol
module B = Support.Background_fixture
module C = Support.Config_fixture
module Process = Support.Process_manager

let source =
  {|
<developer>Call watch once.</developer>
<script id="publisher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { invocation = ""; sent = false }
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("ready"))) in
  Task.pure({ invocation = p.context.invocation_id; sent = false })
| `Turn_end -> (match state.sent with
  | true -> Task.pure(state)
  | false ->
    let reference = { key = "finished"; invocation_id = `Some(state.invocation); work = `None } in
    let* first = Notification.publish(reference, `Succeeded(`String("one")), `Request_turn) in
    let* second = Notification.publish(reference, `Succeeded(`String("two")), `Request_turn) in
    Task.pure({ invocation = state.invocation; sent = true }))
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="publisher" input_schema="any.json" output_schema="string.json"/>
|}
;;

let with_host env environment fixture boundary f =
  Eio.Switch.run (fun sw ->
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"notification"
        ~arguments:[ "notification"; C.config_path fixture; boundary ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "notification-host-ready";
        F.with_client ~sw env fixture (fun client -> f child client)))
;;

let state env fixture session =
  B.checkpoint env fixture session
  |> Option.value_exn ~message:"moderator crash checkpoint missing"
;;

let frames state =
  List.filter
    state.Agent_session.Session_state.conversation.canonical_history
    ~f:(fun entry ->
      match entry.P.History.provenance with
      | Runtime_notification _ -> true
      | _ -> false)
;;

let no_pending state =
  List.for_all state.Agent_session.Session_state.deliveries ~f:(fun delivery ->
    match delivery.P.Delivery.status, delivery.wake_disposition with
    | Committed _, Some (Accepted_wake _) -> true
    | _ -> false)
;;

let run env environment boundary =
  let fixture = F.fixture env environment ("notification-" ^ boundary) in
  F.write env (C.prompt_path fixture) source;
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
    with_host env environment fixture boundary (fun child client ->
      let session = B.create client "notification:create" in
      ignore
        (F.request
           client
           (Session_send_message
              { session_id = session.summary.id
              ; attachment_id = session.attachment_id
              ; content =
                  { kind = Plain_text; text = "Call watch once."; attachments = [] }
              ; idempotency_key = F.key "notification:send"
              })
         : P.Method_result.t);
      F.await_marker env child ("notification-boundary " ^ boundary);
      F.kill env child;
      let before = state env fixture session in
      F.require
        (List.length before.deliveries = 2)
        "crash did not persist both notifications";
      F.require
        (List.exists before.invocations ~f:(fun invocation ->
           match invocation.P.Invocation.context.origin with
           | Model ->
             (match invocation.status with
              | Published _ -> true
              | _ -> false)
           | _ -> false))
        "notification survived without its acknowledged tool response";
      (match boundary with
       | "pending" ->
         F.require (List.is_empty (frames before)) "pending crash already inserted data";
         F.require
           (List.for_all before.deliveries ~f:(fun value ->
              match value.status with
              | Pending -> true
              | _ -> false))
           "pending boundary was missed"
       | "committed" ->
         F.require (List.length (frames before) = 2) "committed data is missing";
         F.require
           (List.for_all before.deliveries ~f:(fun value ->
              match value.wake_disposition with
              | Some Pending_wake -> true
              | _ -> false))
           "committed wake boundary was missed"
       | _ ->
         F.require
           (List.length (frames before) = 2 && no_pending before)
           "accepted boundary was missed";
         let operation =
           Option.value_exn
             before.active_operation
             ~message:"accepted wake has no active operation"
         in
         (match operation.kind with
          | Turn User_submit -> ()
          | _ -> F.fail "accepted wake was not associated with the original user turn");
         F.require
           (List.for_all before.deliveries ~f:(fun value ->
              match value.wake_disposition with
              | Some (Accepted_wake id) -> P.Id.Operation.equal id operation.id
              | _ -> false))
           "wake acceptance refers to a different operation");
      session, before)
  in
  let expected_ids =
    List.map before.deliveries ~f:(fun value -> value.P.Delivery.context.id)
    |> List.sort ~compare:P.Id.Delivery.compare
  in
  let previous_frames = ref (frames before) in
  for reopen = 1 to 2 do
    with_host env environment fixture "recover" (fun child client ->
      let recovered =
        B.await env "notification recovery settlement" (fun () ->
          let current = state env fixture session in
          Option.some_if
            (no_pending current && Option.is_none current.active_operation)
            current)
      in
      let seen_frames = frames recovered in
      F.require
        (List.length seen_frames = 2)
        "recovery lost or duplicated notification data";
      F.require_equal
        "retained delivery identities"
        [%sexp_of: P.Id.Delivery.t list]
        expected_ids
        (List.map recovered.deliveries ~f:(fun value -> value.context.id)
         |> List.sort ~compare:P.Id.Delivery.compare);
      (match !previous_frames with
       | [] -> previous_frames := seen_frames
       | previous ->
         F.require_equal
           "stable notification frames"
           [%sexp_of: P.History.entry list]
           previous
           seen_frames);
      let expected_calls =
        if (not (String.equal boundary "accepted")) && reopen = 1 then 1 else 0
      in
      for _ = 1 to 10 do
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.03;
        let lines = String.split_lines (Process.stdout child).contents in
        let calls =
          List.count lines ~f:(String.is_prefix ~prefix:"notification-provider ")
        in
        F.require (calls = expected_calls) "saved wake was lost or repeated after restart"
      done;
      let snapshot = F.get client session.summary.id in
      F.require
        (Option.is_none snapshot.session.active_operation)
        "recovery has an unexpected active turn";
      let current = state env fixture session in
      F.require
        (Option.is_none current.failure && no_pending current)
        "recovery left a failure or pending wake";
      let expected_turns =
        match boundary with
        | "accepted" -> 0
        | _ -> 1
      in
      F.require_equal
        (sprintf "wake accounting (%s, reopen %d)" boundary reopen)
        [%sexp_of: int]
        expected_turns
        (Option.value_exn
           current.automatic_turn_budget
           ~message:"recovered moderator automatic budget missing")
          .followup_turns;
      F.kill env child)
  done
;;

let test env environment =
  List.iter [ "pending"; "committed"; "accepted" ] ~f:(run env environment)
;;
