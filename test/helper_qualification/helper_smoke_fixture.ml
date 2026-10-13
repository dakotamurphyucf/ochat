open! Core
module P = Agent_protocol

(* Each value is scoped to one real daemon epoch. The callbacks use the same
   admitted client, helper grants and immutable state observations as the full
   qualification; this module only selects a shorter sequence of operations. *)
type t =
  { create_parent : key:string -> P.Id.Session.t
  ; state : P.Id.Session.t -> Agent_session.Session_state.t
  ; invoke : P.Id.Session.t -> string -> Jsonaf.t -> P.Invocation.status
  ; bridge : name:string -> P.Id.Session.t -> string -> Jsonaf.t -> P.Invocation.outcome
  ; start_watch : P.Id.Session.t -> (string * Jsonaf.t) list -> P.Id.Subscription.t
  ; await_timer : P.Id.Session.t -> P.Id.Subscription.t -> unit
  ; await_watch :
      require_wake:bool -> P.Id.Session.t -> P.Id.Subscription.t -> P.Completion.t
  ; check_watches : P.Id.Session.t -> unit
  ; check_capabilities : P.Id.Session.t -> unit
  ; check_child_tools : P.Id.Session.t -> unit
  ; await : (unit -> bool) -> unit
  ; pause_child : unit -> unit
  ; release_child : unit -> unit
  ; request_private_read : unit -> unit
  ; child_calls : unit -> int
  ; session_count : unit -> int
  ; start_child : P.Id.Session.t -> (unit, P.Error.t) result
  ; stop_child : P.Id.Session.t -> unit
  ; child_request : Jsonaf.t
  }

type retained =
  { parent : P.Id.Session.t
  ; child : P.Id.Session.t
  ; receipt : string
  ; interrupted_watch : P.Id.Subscription.t
  ; child_calls_at_shutdown : int
  }

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let field = Jsonaf.member_exn
let text value name = field name value |> Jsonaf.string_exn

let complete = function
  | P.Invocation.Complete value -> value
  | outcome ->
    raise_s [%sexp "helper smoke operation failed", (outcome : P.Invocation.outcome)]
;;

let bridge t ?(name = "session_bridge") parent operation arguments =
  t.bridge ~name parent operation arguments
;;

let target child = [ "session_id", P.Id.Session.to_json child ]

let initial t =
  let parent = t.create_parent ~key:"smoke-parent" in
  t.await (fun () -> Option.is_none (t.state parent).active_operation);
  t.check_capabilities parent;
  let envelope =
    `Object
      [ "version", `Number "1"
      ; "operation", `String "create"
      ; "arguments", t.child_request
      ]
  in
  List.iter
    [ "bad_helper_digest"
    ; "bad_helper_environment"
    ; "bad_helper_arguments"
    ; "bad_helper_roots"
    ]
    ~f:(fun name ->
      let before = t.session_count () in
      (match
         t.invoke parent name (`Object [ "stdin", `String (Jsonaf.to_string envelope) ])
       with
       | Published (Complete (`String source)) ->
         let error = Jsonaf.of_string source |> field "error" in
         [%test_eq: string] "denied" (text error "code");
         [%test_eq: string]
           "command denied: session helper execution differs from its operator grant"
           (text error "message")
       | status ->
         raise_s [%sexp "unsafe helper was accepted", (status : P.Invocation.status)]);
      [%test_eq: int] before (t.session_count ()));
  let created = bridge t parent "create" t.child_request |> complete in
  let child = text created "session_id" |> P.Id.Session.of_string |> ok in
  t.await (fun () -> Option.is_none (t.state child).active_operation);
  t.check_child_tools child;
  let replay = bridge t parent "create" t.child_request |> complete in
  [%test_eq: string] (P.Id.Session.to_string child) (text replay "session_id");
  let target = target child in
  let before_denials = t.state child in
  let denied caller name operation arguments =
    (match bridge t ~name caller operation (`Object arguments) with
     | Fail error -> [%test_eq: string] "agent.management.denied" error.code
     | outcome ->
       raise_s [%sexp "helper smoke widened authority", (outcome : P.Invocation.outcome)]);
    [%test_eq: Sexp.t]
      (Agent_session.Session_state.sexp_of_t before_denials)
      (Agent_session.Session_state.sexp_of_t (t.state child))
  in
  denied
    parent
    "session_view"
    "send"
    (target @ [ "message", `String "denied"; "idempotency_key", `String "smoke-readonly" ]);
  let foreign = t.create_parent ~key:"smoke-foreign" in
  denied foreign "session_bridge" "status" target;
  t.pause_child ();
  t.request_private_read ();
  let before_calls = t.child_calls () in
  let sent =
    bridge
      t
      parent
      "send"
      (`Object
          (target
           @ [ "message", `String "answer now"; "idempotency_key", `String "smoke-send" ]
          ))
    |> complete
  in
  t.await (fun () -> t.child_calls () > before_calls);
  let receipt = text sent "receipt_id" in
  let watch = t.start_watch parent (target @ [ "receipt_id", `String receipt ]) in
  t.await_timer parent watch;
  assert (Option.is_some (t.state child).active_operation);
  t.release_child ();
  (match t.await_watch ~require_wake:true parent watch with
   | Succeeded page ->
     assert (
       String.is_substring (Jsonaf.to_string page) ~substring:"persisted helper answer")
   | result -> raise_s [%sexp "helper smoke watcher failed", (result : P.Completion.t)]);
  let results =
    (t.state child).invocations
    |> List.filter_map ~f:(fun invocation ->
      match invocation.status with
      | Published (Complete value)
        when String.equal invocation.context.tool_name "read_file" ->
        Some (Jsonaf.to_string value)
      | _ -> None)
  in
  assert (
    List.exists results ~f:(fun value ->
      String.is_substring value ~substring:"outside the configured read roots"));
  assert (
    List.for_all results ~f:(fun value ->
      not (String.is_substring value ~substring:"HELPER_PRIVATE_FIXTURE_CONTENT")));
  let page =
    bridge t parent "read" (`Object (target @ [ "receipt_id", `String receipt ]))
    |> complete
  in
  assert (String.is_substring (Jsonaf.to_string page) ~substring:"persisted helper answer");
  t.check_watches parent;
  t.pause_child ();
  let before_calls = t.child_calls () in
  let interrupted =
    bridge
      t
      parent
      "send"
      (`Object
          (target
           @ [ "message", `String "interrupted by restart"
             ; "idempotency_key", `String "smoke-interrupted"
             ]))
    |> complete
  in
  t.await (fun () -> t.child_calls () > before_calls);
  let interrupted_watch =
    t.start_watch parent (target @ [ "receipt_id", field "receipt_id" interrupted ])
  in
  t.await_timer parent interrupted_watch;
  assert (Option.is_some (t.state child).active_operation);
  { parent
  ; child
  ; receipt
  ; interrupted_watch
  ; child_calls_at_shutdown = t.child_calls ()
  }
;;

let recover t retained =
  [%test_eq: int] retained.child_calls_at_shutdown (t.child_calls ());
  (match t.await_watch ~require_wake:false retained.parent retained.interrupted_watch with
   | Failed error -> [%test_eq: string] "watcher.target_failed" error.code
   | result ->
     raise_s [%sexp "interrupted receipt was not reported", (result : P.Completion.t)]);
  [%test_eq: int] retained.child_calls_at_shutdown (t.child_calls ());
  t.check_watches retained.parent;
  let page =
    bridge
      t
      retained.parent
      "read"
      (`Object (target retained.child @ [ "receipt_id", `String retained.receipt ]))
    |> complete
  in
  assert (String.is_substring (Jsonaf.to_string page) ~substring:"persisted helper answer");
  t.stop_child retained.child
;;

let revoked t retained =
  let before = t.child_calls () in
  bridge t retained.parent "status" (`Object (target retained.child))
  |> complete
  |> ignore;
  (match t.start_child retained.child with
   | Error _ -> ()
   | Ok () -> t.await (fun () -> Option.is_some (t.state retained.child).failure));
  [%test_eq: int] before (t.child_calls ())
;;
