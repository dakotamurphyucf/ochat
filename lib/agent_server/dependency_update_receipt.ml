open! Core
module Selection = Agent_protocol.Prompt_dependency_selection
module Store = Agent_store.Idempotency_store
module P = Agent_protocol

type t =
  { store : Store.t
  ; mutex : Eio.Mutex.t
  ; now : unit -> P.Timestamp.t
  }

let create ~store ~mutex ~now = { store; mutex; now }
let error code message = P.Error.create code ~message ~retryable:false ()

let pending () =
  error
    Interrupted
    "the original dependency update outcome is unknown; duplicate execution is suppressed"
;;

let completion_uncertain () =
  error
    Interrupted
    "dependency update receipt completion is uncertain; publication is not rolled back"
;;

let persistence_error failure =
  match failure with
  | Agent_store.Store_error.Admission_capacity _ ->
    Agent_store.Store_error.to_protocol_error failure
  | Locked _
  | Missing _
  | Schema_too_new _
  | Migration_required _
  | Document _
  | Framing _
  | Corrupt _
  | Io _ ->
    P.Error.create
      Persistence_error
      ~message:(Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t failure))
      ~retryable:false
      ()
;;

(* A guard or storage exception must leave the lock normally. Eio otherwise
   poisons a write mutex when its callback raises. Callback identity/backtrace are
   transported only across this one owned lock invocation. *)
let locked t f =
  match
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match f () with
      | value -> Ok value
      | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  with
  | Ok value -> value
  | Error (exn, trace) -> Exn.raise_with_original_backtrace exn trace
;;

module Admission = struct
  type t =
    | Replay of (Selection.Update_result.t, P.Error.t) Result.t
    | Execute
end

let replay request = function
  | Store.Pending -> Error (pending ())
  | Failure error -> Error error
  | Success json ->
    let open Result.Let_syntax in
    let%bind result = Selection.Update_result.of_json json in
    if Selection.Update_result.matches_request result request
    then Ok result
    else Error (pending ())
;;

let claim t ~key ~digest ~request ~authorize =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%bind () = authorize () in
    match Store.lookup t.store ~key ~request_digest:digest with
    | Replay record -> Ok (Admission.Replay (replay request record.outcome))
    | Conflict _ ->
      Error (error Idempotency_conflict "idempotency key was used for another request")
    | Missing ->
      let created_at = t.now () in
      let expires_at =
        P.Timestamp.to_time_ns created_at
        |> Fn.flip Time_ns.add (Time_ns.Span.of_day 1.)
        |> P.Timestamp.of_time_ns
      in
      let%map (_ : Store.record) =
        Store.record
          t.store
          { key
          ; request_digest = digest
          ; accepted_transaction_sequence = None
          ; outcome = Pending
          ; created_at
          ; expires_at = Some expires_at
          ; retention = Protected
          }
        |> Result.map_error ~f:persistence_error
      in
      Admission.Execute)
;;

let complete t ~key ~digest result =
  locked t (fun () ->
    let outcome =
      match result with
      | Ok value -> Store.Success (Selection.Update_result.to_json value)
      | Error error -> Store.Failure error
    in
    match
      Store.complete
        t.store
        ~key
        ~request_digest:digest
        ~accepted_transaction_sequence:None
        ~outcome
    with
    | Ok _ -> result
    | Error _ -> Error (completion_uncertain ()))
;;

let dispatch t ~principal_id ~request ~authorize ~checkpoint ~execute =
  let open Result.Let_syntax in
  let%bind canonical =
    Selection.Update_request.to_json request |> P.Json_codec.canonical_string
  in
  let digest = Digestif.SHA256.digest_string canonical |> Digestif.SHA256.to_hex in
  let key =
    Store.Key.
      { principal_id
      ; session_id = None
      ; method_name = "prompt.dependencies.update"
      ; idempotency_key = Selection.Update_request.idempotency_key request
      }
  in
  Eio.Fiber.check ();
  let%bind admission = claim t ~key ~digest ~request ~authorize in
  match admission with
  | Admission.Replay result -> result
  | Execute ->
    (* Pending acknowledgment precedes this first borrowed operation checkpoint.
       Once an outcome is acknowledged, terminalization invokes neither guard nor
       checkpoint: it records the already-admitted effect. *)
    checkpoint ();
    (match execute ~checkpoint with
     | Ok result ->
       if Selection.Update_result.matches_request result request
       then complete t ~key ~digest (Ok result)
       else Error (pending ())
     | Error (Dependency_administration.Failure.Rejected failure) ->
       complete t ~key ~digest (Error failure)
     | Error (Uncertain _) -> Error (pending ()))
;;
