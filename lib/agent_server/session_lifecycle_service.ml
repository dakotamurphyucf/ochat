open! Core
module P = Agent_protocol
module S = Agent_store.Session_store
module R = Agent_store.Session_archive_record
module Receipts = Session_lifecycle_receipts
module Actor = Agent_session.Session_actor
module State = Agent_session.Session_state

type t =
  { store : S.t
  ; registry : Session_registry.t
  ; receipts : Receipts.t
  ; now : unit -> P.Timestamp.t
  ; read_owned_session : S.Handle.t -> (State.t, P.Error.t) Result.t
  ; validate_removal : State.t -> (unit, P.Error.t) Result.t
  }

module Request = struct
  type t =
    | Delete of P.Session.Delete_request.t
    | Restore of P.Session_lifecycle.Request.t
    | Resume of P.Session_lifecycle.Request.t

  let session_id = function
    | Delete request -> request.session_id
    | Restore request | Resume request ->
      P.Session_lifecycle.Request.expected request
      |> P.Session_lifecycle.Expected.reference
      |> P.Session_ref.session_id
  ;;

  let action = function
    | Delete { policy = Archive; _ } -> R.Outcome.Archive
    | Delete { policy = Remove; _ } -> Remove
    | Restore _ -> Restore
    | Resume _ -> Resume
  ;;

  let attachment = function
    | Delete request -> Some request.attachment_id
    | Restore _ | Resume _ -> None
  ;;
end

module Failure = struct
  type disposition =
    | No_effect
    | Recovery_required
  [@@deriving equal, sexp_of]

  type t =
    { error : P.Error.t
    ; disposition : disposition
    ; completion_error : Agent_store.Store_error.t option
    ; cleanup_error : Session_registry.Cleanup_failure.t option
    }

  let error t = t.error
  let disposition t = t.disposition
  let completion_error t = t.completion_error
  let cleanup_error t = t.cleanup_error
end

let create ~store ~registry ~idempotency ~now ~read_owned_session ~validate_removal =
  { store
  ; registry
  ; receipts = Receipts.create ~store:idempotency ~server_id:(S.server_id store)
  ; now
  ; read_owned_session
  ; validate_removal
  }
;;

let fail code message = Error (P.Error.create code ~message ~retryable:false ())
let stored result = Result.map_error result ~f:Agent_store.Store_error.to_protocol_error

let validate_request t request state lifecycle =
  let summary = State.summary state in
  match request with
  | Request.Delete request ->
    if not (Int64.equal request.expected_revision summary.revision)
    then fail Conflict "session revision does not match"
    else if not (String.equal request.confirmation (P.Id.Session.to_string summary.id))
    then fail Invalid_request "deletion confirmation must equal the session ID"
    else if Option.is_some state.active_operation
    then fail Conflict "session has an active foreground operation"
    else (
      match state.lifecycle.observed with
      | P.Session.Stopped -> Ok (R.revision lifecycle)
      | Queued_for_slot
      | Starting
      | Recovering
      | Idle
      | Running_turn _
      | Compacting _
      | Waiting_for_permission _
      | Stopping
      | Failed _ -> fail Invalid_state "session must be stopped before deletion")
  | Restore request | Resume request ->
    let expected = P.Session_lifecycle.Request.expected request in
    if
      not
        (P.Id.Server.equal
           (P.Session_ref.server_id (P.Session_lifecycle.Expected.reference expected))
           (S.server_id t.store))
    then fail Invalid_request "lifecycle target belongs to another host"
    else if
      (not
         (Int.equal summary.generation (P.Session_lifecycle.Expected.generation expected)))
      || not
           (Int64.equal
              summary.revision
              (P.Session_lifecycle.Expected.session_revision expected))
    then fail Conflict "session lifecycle canonical anchor changed"
    else Ok (P.Session_lifecycle.Expected.lifecycle_revision expected)
;;

let prepare t handle observation state ~expected ~action ~key ~request_digest =
  let open Result.Let_syntax in
  let summary = State.summary state in
  let%bind anchor =
    R.Anchor.create
      ~generation:summary.generation
      ~session_revision:summary.revision
      ~latest_event_sequence:summary.latest_event_sequence
  in
  let current_time = t.now () in
  let%bind transition =
    R.prepare
      (S.Lifecycle.Observation.value observation)
      ~expected
      ~anchor
      ~action
      ~key
      ~request_digest
      ~now:current_time
  in
  let%map prepared =
    S.prepare_lifecycle
      t.store
      handle
      observation
      ~current_entry:(Session_factory.index_entry state)
      ~transition
      ~now:current_time
    |> stored
  in
  transition, prepared
;;

let complete t handle ~key ~request_digest =
  S.complete_lifecycle_outcome
    t.store
    handle
    ~key
    ~request_digest
    ~complete:(Receipts.complete t.receipts ~key ~request_digest)
  |> stored
;;

let projection_unchanged transition =
  let previous = R.Prepared.previous transition in
  let next = R.Prepared.next transition in
  R.Status.equal (R.status previous) (R.status next)
  && R.Admission.equal (R.admission previous) (R.admission next)
  && R.Revision.equal (R.revision previous) (R.revision next)
;;

let replay t handle ~key ~request_digest receipt =
  let open Result.Let_syntax in
  let%bind () = complete t handle ~key ~request_digest in
  Receipts.result t.receipts ~key receipt.R.Receipt.outcome |> stored
;;

let with_cleanup t reservation ~owner ~cleanup ~outcome_uncertain ~cleanup_error f =
  let original =
    match f () with
    | result -> `Result result
    | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  Eio.Cancel.protect (fun () ->
    let secondary =
      match cleanup () with
      | Ok () -> None
      | Error error -> Some (Session_registry.Cleanup_failure.rejected error)
      | exception exn ->
        Some
          (Session_registry.Cleanup_failure.raised
             exn
             (Stdlib.Printexc.get_raw_backtrace ()))
    in
    Option.iter secondary ~f:(fun failure ->
      let primary =
        match original with
        | `Result (Error error) -> Session_registry.Cleanup_failure.rejected error
        | `Raised (exn, backtrace) ->
          Session_registry.Cleanup_failure.raised exn backtrace
        | `Result (Ok _) -> failure
      in
      outcome_uncertain := true;
      cleanup_error := Some failure;
      Session_registry.retain_cleanup t.registry reservation ~owner ~primary ~failure);
    match original, secondary with
    | `Raised (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
    | `Result (Error error), _ -> Error error
    | `Result (Ok value), None -> Ok value
    | `Result (Ok _), Some failure ->
      (match Session_registry.Cleanup_failure.exception_and_backtrace failure with
       | Some (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
       | None -> Error (Session_registry.Cleanup_failure.error failure)))
;;

let publish t reservation handle prepared ~key ~request_digest ~retire ~outcome_uncertain =
  outcome_uncertain := true;
  let open Result.Let_syntax in
  match (S.Lifecycle.Prepared.outcome prepared).action with
  | R.Outcome.Remove ->
    let%bind removal = S.begin_removal t.store handle prepared |> stored in
    let%bind _ = Session_registry.commit_removal t.registry reservation removal in
    let%bind () =
      S.finish_removal
        t.store
        removal
        ~complete:(Receipts.complete_receipt t.receipts)
        ~retire
      |> stored
    in
    Receipts.result t.receipts ~key (S.Lifecycle.Prepared.outcome prepared) |> stored
  | Archive | Restore | Resume ->
    let%bind installed = S.publish_lifecycle t.store handle prepared |> stored in
    let%bind _ =
      Session_registry.commit_lifecycle t.registry reservation ~handle installed
    in
    let%bind () = complete t handle ~key ~request_digest in
    Receipts.result t.receipts ~key (S.Lifecycle.Installed.outcome installed) |> stored
;;

let execute_indexed
      t
      reservation
      ~key
      ~request_digest
      ~authorize
      ~outcome_uncertain
      ~cleanup_error
      request
      indexed
  =
  let open Result.Let_syntax in
  let%bind () = authorize indexed.Agent_store.Session_index.Entry.session in
  Eio.Switch.run (fun sw ->
    let%bind handle =
      S.open_session
        t.store
        ~sw
        ~actor_lock_nonce:(P.Id.Transaction.create () |> P.Id.Transaction.to_string)
        indexed.session.id
      |> stored
    in
    let closed = ref false in
    let close () =
      Eio.Cancel.protect (fun () ->
        if !closed
        then Ok ()
        else (
          match S.close_session t.store handle with
          | Ok () ->
            closed := true;
            Ok ()
          | Error _ as failure -> failure))
    in
    let run () =
      let%bind state = t.read_owned_session handle in
      let%bind () = authorize (State.summary state) in
      let%bind observation = S.read_lifecycle t.store handle |> stored in
      let lifecycle = S.Lifecycle.Observation.value observation in
      let%bind receipt = R.lookup lifecycle ~key ~request_digest ~now:(t.now ()) in
      match receipt with
      | Some receipt -> replay t handle ~key ~request_digest receipt
      | None ->
        outcome_uncertain := false;
        let%bind () =
          match request with
          | Request.Delete _
            when (not indexed.archived) && R.Admission.equal indexed.admission Automatic
            ->
            fail Invalid_state "active session deletion requires a live writer attachment"
          | Delete _ | Restore _ | Resume _ -> Ok ()
        in
        let%bind expected = validate_request t request state lifecycle in
        let action = Request.action request in
        let%bind () =
          match action with
          | Remove -> t.validate_removal state
          | Archive | Restore | Resume -> Ok ()
        in
        let%bind _, prepared =
          prepare t handle observation state ~expected ~action ~key ~request_digest
        in
        Eio.Cancel.protect (fun () ->
          publish
            t
            reservation
            handle
            prepared
            ~key
            ~request_digest
            ~outcome_uncertain
            ~retire:(fun _ -> close ()))
    in
    with_cleanup
      t
      reservation
      ~owner:(Session_registry.Cleanup_owner.handle ~store:t.store handle)
      ~cleanup:(fun () -> close () |> stored)
      ~outcome_uncertain
      ~cleanup_error
      run)
;;

let execute_loaded
      t
      reservation
      entry
      ~key
      ~request_digest
      ~authorize
      ~validate_attachment
      ~outcome_uncertain
      ~cleanup_error
      request
  =
  let open Result.Let_syntax in
  let%bind handle =
    Result.of_option
      entry.Session_registry.store_handle
      ~error:
        (P.Error.create
           Persistence_error
           ~message:"session has no durable lifecycle owner"
           ~retryable:false
           ())
  in
  let%bind original_state = Actor.state entry.actor in
  let%bind () = authorize (State.summary original_state) in
  let%bind observation = S.read_lifecycle t.store handle |> stored in
  let lifecycle = S.Lifecycle.Observation.value observation in
  let%bind receipt = R.lookup lifecycle ~key ~request_digest ~now:(t.now ()) in
  match receipt with
  | Some receipt ->
    let%bind fence =
      Actor.begin_lifecycle
        entry.actor
        ~attachment_id:None
        ~expected_generation:original_state.identity.generation
        ~expected_revision:original_state.counters.revision
    in
    Eio.Cancel.protect (fun () ->
      let release () =
        match S.Handle.metadata_checked handle with
        | Ok _ -> Actor.abort_lifecycle entry.actor fence
        | Error failure -> Error (Agent_store.Store_error.to_protocol_error failure)
      in
      with_cleanup
        t
        reservation
        ~owner:(Session_registry.Cleanup_owner.fence entry fence)
        ~cleanup:release
        ~outcome_uncertain
        ~cleanup_error
        (fun () -> replay t handle ~key ~request_digest receipt))
  | None ->
    outcome_uncertain := false;
    let%bind () =
      match Request.attachment request with
      | None -> Ok ()
      | Some attachment -> validate_attachment (Request.session_id request) attachment
    in
    let%bind expected = validate_request t request original_state lifecycle in
    let%bind fence =
      Actor.begin_lifecycle
        entry.actor
        ~attachment_id:(Request.attachment request)
        ~expected_generation:original_state.identity.generation
        ~expected_revision:original_state.counters.revision
    in
    let retired = ref false in
    let joined = ref false in
    let closed = ref false in
    let released = ref false in
    let abort () =
      if !released || !retired
      then Ok ()
      else
        Result.map (Actor.abort_lifecycle entry.actor fence) ~f:(fun () ->
          released := true)
    in
    let retire _ =
      if not !closed
      then (
        entry.close ();
        closed := true);
      Ok ()
    in
    let run () =
      let action = Request.action request in
      let%bind transition, prepared =
        prepare t handle observation original_state ~expected ~action ~key ~request_digest
      in
      if projection_unchanged transition
      then
        Eio.Cancel.protect (fun () ->
          outcome_uncertain := true;
          let%bind installed = S.publish_lifecycle t.store handle prepared |> stored in
          if not (S.Lifecycle.Installed.is_current installed handle)
          then fail Conflict "lifecycle result is no longer current"
          else (
            let%bind () = complete t handle ~key ~request_digest in
            let%bind () = abort () in
            Receipts.result t.receipts ~key (S.Lifecycle.Installed.outcome installed)
            |> stored))
      else (
        let%bind () =
          match action with
          | Remove -> t.validate_removal original_state
          | Archive | Restore | Resume -> Ok ()
        in
        Eio.Cancel.protect (fun () ->
          let%bind () = Actor.retire_lifecycle entry.actor fence in
          retired := true;
          Runtime_owner.close_and_wait entry.runtime;
          joined := true;
          let%bind state = Actor.lifecycle_state entry.actor fence in
          let%bind () = authorize (State.summary state) in
          let%bind () =
            match action with
            | Remove -> t.validate_removal state
            | Archive | Restore | Resume -> Ok ()
          in
          let%bind observation = S.read_lifecycle t.store handle |> stored in
          let%bind _, prepared =
            prepare t handle observation state ~expected ~action ~key ~request_digest
          in
          publish
            t
            reservation
            handle
            prepared
            ~key
            ~request_digest
            ~retire
            ~outcome_uncertain))
    in
    let cleanup () =
      Eio.Cancel.protect (fun () ->
        if !closed
        then Ok ()
        else if !retired
        then (
          if not !joined
          then (
            (* Preserve an original failed join while retrying its actual owned
               finalizer. Actor/store closure cannot precede successful joining. *)
            Runtime_owner.close_and_wait entry.runtime;
            joined := true);
          let%bind () =
            match S.Handle.metadata_checked handle with
            | Error failure ->
              (* Uncertain projection keeps the fenced owner retained for checked
                 shutdown cleanup; never detach it using an unverified index. *)
              Error (Agent_store.Store_error.to_protocol_error failure)
            | Ok _ ->
              Session_registry.commit_retired t.registry reservation ~store:t.store
              |> Result.map ~f:ignore
          in
          retire handle |> stored)
        else abort ())
    in
    let owner =
      (* Retire may happen during [run]; choose the actual capability afterwards. *)
      fun () ->
      if !retired
      then Session_registry.Cleanup_owner.entry entry
      else Session_registry.Cleanup_owner.fence entry fence
    in
    let run_with_cleanup () =
      let result =
        match run () with
        | result -> `Result result
        | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      with_cleanup
        t
        reservation
        ~owner:(owner ())
        ~cleanup
        ~outcome_uncertain
        ~cleanup_error
        (fun () ->
           match result with
           | `Result result -> result
           | `Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
    in
    run_with_cleanup ()
;;

let execute t ~key ~request_digest ~authorize ~validate_attachment request =
  let session_id = Request.session_id request in
  if
    (not
       (Option.equal
          P.Id.Session.equal
          key.Agent_store.Idempotency_store.Key.session_id
          (Some session_id)))
    || not
         (R.Outcome.accepts_method (Request.action request) ~method_name:key.method_name)
  then
    Error
      { Failure.error =
          P.Error.invalid_request
            "lifecycle command differs from its original receipt scope"
      ; disposition = Recovery_required
      ; completion_error = None
      ; cleanup_error = None
      }
  else (
    let cleanup_error = ref None in
    let result =
      Session_registry.with_lifecycle t.registry session_id (fun reservation ->
        (* Until checked lookup proves absence, an earlier attempt may own proof.
           Publication flips this before invoking any fallible storage operation. *)
        let outcome_uncertain = ref true in
        let result =
          match Session_registry.Lifecycle_reservation.target reservation with
          | Loaded entry ->
            execute_loaded
              t
              reservation
              entry
              ~key
              ~request_digest
              ~authorize
              ~validate_attachment
              ~outcome_uncertain
              ~cleanup_error
              request
          | Indexed indexed ->
            execute_indexed
              t
              reservation
              ~key
              ~request_digest
              ~authorize
              ~outcome_uncertain
              ~cleanup_error
              request
              indexed
          | Absent -> fail Session_not_found "session does not exist"
        in
        Ok
          (Result.map_error result ~f:(fun error ->
             if !outcome_uncertain
             then
               { Failure.error
               ; disposition = Recovery_required
               ; completion_error = None
               ; cleanup_error = !cleanup_error
               }
             else
               (* Complete while retaining the reservation. Failed acknowledgement
                  preserves the primary rejection and the actual recovery diagnostic. *)
               Eio.Cancel.protect (fun () ->
                 match Receipts.reject t.receipts ~key ~request_digest error with
                 | Ok () ->
                   { Failure.error
                   ; disposition = No_effect
                   ; completion_error = None
                   ; cleanup_error = !cleanup_error
                   }
                 | Error completion_error ->
                   { Failure.error
                   ; disposition = Recovery_required
                   ; completion_error = Some completion_error
                   ; cleanup_error = !cleanup_error
                   }))))
    in
    match result with
    | Ok outcome -> outcome
    | Error error ->
      Error
        { Failure.error
        ; disposition = Recovery_required
        ; completion_error = None
        ; cleanup_error = !cleanup_error
        })
;;

let recover_removals t =
  let open Result.Let_syntax in
  let%bind ids = S.pending_removal_ids t.store |> stored in
  List.fold_result ids ~init:() ~f:(fun () session_id ->
    Session_registry.with_lifecycle t.registry session_id (fun reservation ->
      Eio.Switch.run (fun sw ->
        let%bind removal =
          S.open_removal
            t.store
            ~sw
            ~actor_lock_nonce:(P.Id.Transaction.create () |> P.Id.Transaction.to_string)
            session_id
          |> stored
        in
        let%bind _ = Session_registry.commit_removal t.registry reservation removal in
        Eio.Cancel.protect (fun () ->
          S.finish_removal
            t.store
            removal
            ~complete:(Receipts.complete_receipt t.receipts)
            ~retire:(fun handle -> S.close_session t.store handle)
          |> stored))))
;;
