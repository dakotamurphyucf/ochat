open! Core
module P = Agent_protocol

let prompt =
  {|<developer>Offline run admission conformance.</developer>
<script id="conformance_run" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(Run.finish(`Object([
  { key = "kind"; value = `String("finish") },
  { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
  { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)
</script>|}
;;

let fail message = raise_s [%sexp "run admission conformance", (message : string)]

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "run admission protocol error", (error : P.Error.t)]
;;

let non_history result =
  match checked result with
  | P.Public.Result.Non_history value -> P.Public.Result.Non_history.value value
  | _ -> fail "expected non-history result"
;;

let snapshot request session_id =
  match request (P.Command.Session_get { session_id; history = None }) |> checked with
  | P.Public.Result.Session_get snapshot -> P.Public.Snapshot.fields snapshot
  | _ -> fail "expected session snapshot"
;;

let check (created : P.Public.Result.Create.t) ~request ~key_prefix =
  let key suffix = P.Idempotency_key.of_string (key_prefix ^ suffix) |> checked in
  let writer =
    match created.attachment with
    | Some attached -> attached.attachment
    | None -> fail "creation did not return its writer attachment"
  in
  let reader =
    match
      request
        (P.Command.Session_attach
           { session_id = created.session.id
           ; requested_mode = Read_only
           ; subscribe = false
           ; after_sequence = None
           ; reclaim_token = None
           ; idempotency_key = key ":reader"
           })
      |> checked
    with
    | P.Public.Result.Session_attach attached -> attached.attachment
    | _ -> fail "expected read-only attachment"
  in
  let before = snapshot request created.session.id in
  let start attachment ~generation ~revision ~key =
    P.Run_start.create
      ~session_id:created.session.id
      ~attachment_id:attachment.P.Session.Attachment.id
      ~generation
      ~expected_revision:revision
      ~mode:Workflow
      ~input:Authored_start
      ~key
    |> checked
    |> fun request -> P.Command.Session_run_start request
  in
  let reject command expected =
    match request command with
    | Error error when P.Error.equal_code error.code expected -> ()
    | Error error -> raise_s [%sexp "unexpected run rejection", (error : P.Error.t)]
    | Ok _ -> fail "invalid run admission succeeded"
  in
  reject
    (start
       reader
       ~generation:before.session.generation
       ~revision:before.revision
       ~key:(key ":read-only"))
    Permission_denied;
  reject
    (start
       writer
       ~generation:before.session.generation
       ~revision:(Int64.succ before.revision)
       ~key:(key ":stale-revision"))
    Conflict;
  reject
    (start
       writer
       ~generation:(Int.succ before.session.generation)
       ~revision:before.revision
       ~key:(key ":stale-generation"))
    Conflict;
  let after_rejections = snapshot request created.session.id in
  if
    not
      (Int64.equal before.revision after_rejections.revision
       && Int64.equal before.latest_event_sequence after_rejections.latest_event_sequence
      )
  then fail "rejected admission changed the session";
  let command =
    start
      writer
      ~generation:before.session.generation
      ~revision:before.revision
      ~key:(key ":admit")
  in
  let receipt =
    match request command |> non_history with
    | Session_run_start receipt -> receipt
    | _ -> fail "expected run admission receipt"
  in
  if
    not
      (P.Run_receipt.Kind.equal receipt.kind Admission
       && P.Idempotency_key.equal receipt.key (key ":admit")
       && Int.equal receipt.source.generation before.session.generation)
  then fail "admission receipt lost its original identity";
  (match request command |> non_history with
   | Session_run_start replay when P.Run_receipt.equal receipt replay -> ()
   | _ -> fail "same-key retry did not return the original receipt");
  reject
    (start
       writer
       ~generation:before.session.generation
       ~revision:(Int64.succ before.revision)
       ~key:(key ":admit"))
    Idempotency_conflict;
  (match
     request
       (P.Command.Command_receipt
          { method_name = P.Command.method_name command
          ; original_params = P.Command.params command
          })
     |> non_history
   with
   | Command_receipt (Committed (Accepted_run original))
     when P.Id.Session.equal original.session_id created.session.id
          && P.Run_receipt.equal original.receipt receipt -> ()
   | _ -> fail "receipt reconciliation lost the committed admission");
  Run_read_conformance.check receipt ~session_id:created.session.id ~request;
  let after = snapshot request created.session.id in
  if not (P.Session.equal_desired_state after.session.desired_state Running)
  then fail "run admission did not retain a running session"
;;
