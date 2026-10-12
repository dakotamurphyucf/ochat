open! Core
module P = Agent_protocol
module Store = Agent_store.Idempotency_store

type t =
  { factory : Session_factory.t
  ; registry : Session_registry.t
  ; store : Store.t
  ; mutex : Eio.Mutex.t
  ; now : unit -> P.Timestamp.t
  }

let create ~factory ~registry ~store ~mutex ~now =
  { factory; registry; store; mutex; now }
;;

let error code message = P.Error.create code ~message ~retryable:false ()

let unknown () =
  error Interrupted "the original seed outcome is unknown; creation is not retried"
;;

let locked t f =
  match
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match f () with
      | value -> Ok value
      | exception exn -> Error (exn, Backtrace.Exn.most_recent ()))
  with
  | Ok value -> value
  | Error (exn, trace) -> Exn.raise_with_original_backtrace exn trace
;;

module Admission = struct
  type t =
    | Replay of Store.record
    | Execute
end

let claim t ~key ~digest =
  locked t (fun () ->
    let open Result.Let_syntax in
    match Store.lookup t.store ~key ~request_digest:digest with
    | Replay record ->
      if Store.equal_retention record.retention Protected
      then Ok (Admission.Replay record)
      else Error (error Persistence_error "seed receipt lacks protected retention")
    | Conflict _ ->
      Error (error Idempotency_conflict "seed key binds another complete request")
    | Missing ->
      let created_at = t.now () in
      let expires_at =
        P.Timestamp.to_time_ns created_at
        |> Fn.flip Time_ns.add (Time_ns.Span.of_day 1.)
        |> P.Timestamp.of_time_ns
      in
      let%map _ =
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
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      in
      Admission.Execute)
;;

let replay (record : Store.record) =
  match record.outcome with
  | Pending -> Error (unknown ())
  | Failure failure -> Error failure
  | Success json -> P.Session_seed.Result.of_json json
;;

let complete t ~key ~digest ~sequence ~outcome =
  locked t (fun () ->
    Store.complete
      t.store
      ~key
      ~request_digest:digest
      ~accepted_transaction_sequence:sequence
      ~outcome)
;;

let exact_success key digest session_id (record : Store.record) =
  Store.Key.compare record.key key = 0
  && String.equal record.request_digest digest
  && Store.equal_retention record.retention Protected
  && Option.equal Int64.equal record.accepted_transaction_sequence (Some 1L)
  &&
  match record.outcome with
  | Success json ->
    (match P.Session_seed.Result.of_json json with
     | Ok value -> P.Id.Session.equal value.session_id session_id
     | Error _ -> false)
  | Pending | Failure _ -> false
;;

let publish_created t ~key ~digest entry =
  let run () =
    let open Result.Let_syntax in
    let%bind handle =
      Result.of_option
        entry.Session_registry.store_handle
        ~error:(error Persistence_error "created seed has no durable Store handle")
    in
    let session_id = Agent_store.Session_store.Handle.session_id handle in
    let result = { P.Session_seed.Result.session_id } in
    let%bind receipt =
      complete
        t
        ~key
        ~digest
        ~sequence:(Some 1L)
        ~outcome:(Success (P.Session_seed.Result.to_json result))
      |> Result.map_error ~f:(fun _ -> unknown ())
    in
    let%bind () =
      if exact_success key digest session_id receipt then Ok () else Error (unknown ())
    in
    (* Durable Success precedes the existing ownership transfer. No initializer,
       conversion or borrowed checkpoint occurs in this publication phase. *)
    let%map () =
      Session_registry.add t.registry ~session_id entry
      |> Result.map_error ~f:(fun _ -> unknown ())
    in
    result
  in
  match run () with
  | Ok _ as success -> success
  | Error failure as rejected ->
    Session_registry.rollback_recovered
      t.registry
      ~primary:(Session_registry.Cleanup_failure.rejected failure)
      [ entry ];
    rejected
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Session_registry.rollback_recovered
      t.registry
      ~primary:(Session_registry.Cleanup_failure.raised exn backtrace)
      [ entry ];
    Exn.raise_with_original_backtrace exn backtrace
;;

let exact_failure key digest failure (record : Store.record) =
  Store.Key.compare record.key key = 0
  && String.equal record.request_digest digest
  && Store.equal_retention record.retention Protected
  && Option.is_none record.accepted_transaction_sequence
  &&
  match record.outcome with
  | Failure actual ->
    Jsonaf.exactly_equal (P.Error.to_json actual) (P.Error.to_json failure)
  | Pending | Success _ -> false
;;

let dispatch t ~principal ~request ~authorize ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = authorize () in
  Eio.Fiber.check ();
  let%bind canonical =
    P.Json_codec.canonical_string (P.Session_seed.Request.to_json request)
  in
  let digest = Digestif.SHA256.(digest_string canonical |> to_hex) in
  let key =
    Store.Key.
      { principal_id = principal.P.Principal.id
      ; session_id = None
      ; method_name = "session.seed"
      ; idempotency_key = P.Session_seed.Request.idempotency_key request
      }
  in
  let%bind admission = claim t ~key ~digest in
  let%bind () = authorize () in
  match admission with
  | Admission.Replay record -> replay record
  | Execute ->
    let%bind command_audit =
      Store.Command_audit.encode { key; request_digest = digest; protected_record = true }
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    in
    checkpoint ();
    Eio.Fiber.check ();
    (match
       Session_factory.seed_session
         t.factory
         ~principal
         ~request
         ~command_audit
         ~checkpoint
     with
     | Created entry -> publish_created t ~key ~digest entry
     | Uncertain _ -> Error (unknown ())
     | Rejected failure ->
       let%bind receipt =
         complete t ~key ~digest ~sequence:None ~outcome:(Failure failure)
         |> Result.map_error ~f:(fun _ -> unknown ())
       in
       if exact_failure key digest failure receipt
       then Error failure
       else Error (unknown ()))
;;
