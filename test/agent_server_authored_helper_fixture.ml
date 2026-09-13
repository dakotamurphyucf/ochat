open Core
module P = Agent_protocol
module State = Agent_session.Session_state

type caller =
  | Owner
  | Read_only
  | Foreign

type retained =
  { child : P.Id.Session.t
  ; receipt : string
  ; history : P.History.entry list
  }

let field json name = Jsonaf.member_exn name json
let text json name = field json name |> Jsonaf.string_exn

let complete = function
  | P.Invocation.Complete value -> value
  | outcome -> raise_s [%sexp "authored helper failed", (outcome : P.Invocation.outcome)]
;;

let target child extra = `Object (("session_id", P.Id.Session.to_json child) :: extra)

let stop child =
  target
    child
    [ "mode", `String "cancel"; "idempotency_key", `String "authored-helper-stop" ]
;;

let unchanged (expected : State.t) actual =
  [%test_eq: Sexp.t] (State.sexp_of_t expected) (State.sexp_of_t actual)
;;

let history_equal expected actual =
  [%test_eq: Sexp.t]
    ([%sexp_of: P.History.entry list] expected)
    ([%sexp_of: P.History.entry list] actual)
;;

let read ~bridge child receipt =
  let page =
    bridge Read_only "read" (target child [ "receipt_id", `String receipt ]) |> complete
  in
  [%test_eq: string] "completed" (text (field page "receipt") "status");
  assert (
    String.is_substring
      (Jsonaf.to_string (field page "items"))
      ~substring:"persisted helper answer")
;;

let before_restart ~state ~named ~bridge =
  let created = named (`Object [ "input", `String "Create an authored specialist." ]) in
  [%test_eq: string] "completed" (text created "status");
  let child =
    field created "session_id"
    |> P.Id.Session.of_json
    |> Agent_server_test_support.protocol_ok
  in
  let target = target child in
  let initial = state child in
  let observed = bridge Read_only "status" (target []) |> complete in
  [%test_eq: string] (P.Id.Session.to_string child) (text observed "session_id");
  List.iter
    [ ( Read_only
      , "send"
      , target
          [ "message", `String "Must be denied."
          ; "idempotency_key", `String "readonly-authored"
          ] )
    ; Foreign, "read", target []
    ]
    ~f:(fun (caller, operation, args) ->
      (match bridge caller operation args with
       | Fail error -> [%test_eq: string] "agent.management.denied" error.code
       | outcome ->
         raise_s
           [%sexp "authored helper widened authority", (outcome : P.Invocation.outcome)]);
      unchanged initial (state child));
  let continued =
    named (target [ "input", `String "Continue the same named instance." ])
  in
  [%test_eq: string] (P.Id.Session.to_string child) (text continued "session_id");
  [%test_eq: string] "completed" (text continued "status");
  let request =
    target
      [ "message", `String "A request through the confined helper."
      ; "idempotency_key", `String "authored-helper-send"
      ]
  in
  let receipt =
    bridge Owner "send" request |> complete |> fun value -> text value "receipt_id"
  in
  let waited =
    bridge
      Read_only
      "wait"
      (target [ "receipt_id", `String receipt; "timeout_ms", `Number "5000" ])
    |> complete
  in
  [%test_eq: string] "receipt_terminal" (text waited "reason");
  read ~bridge child receipt;
  let finished = state child in
  let replay = bridge Owner "send" request |> complete in
  [%test_eq: string] receipt (text replay "receipt_id");
  unchanged finished (state child);
  [%test_eq: int] 3 (List.length finished.managed_submissions);
  let stopped = bridge Owner "stop" (stop child) |> complete in
  [%test_eq: string] "stopped" (text stopped "progress");
  let history = finished.conversation.canonical_history in
  history_equal history (state child).conversation.canonical_history;
  { child; receipt; history }
;;

let after_restart ~(state : P.Id.Session.t -> State.t) ~bridge retained =
  let before = state retained.child in
  (match before.lifecycle.desired, before.lifecycle.observed with
   | Stopped, Stopped -> ()
   | _ -> failwith "authored helper child restarted without authorization");
  history_equal retained.history before.conversation.canonical_history;
  read ~bridge retained.child retained.receipt;
  let replay = bridge Owner "stop" (stop retained.child) |> complete in
  [%test_eq: string] "stopped" (text replay "progress");
  history_equal retained.history (state retained.child).conversation.canonical_history;
  print_endline
    "authored named sessions share confined helper lifecycle, readonly/foreign checks, \
     retries and restart reads PASS"
;;
