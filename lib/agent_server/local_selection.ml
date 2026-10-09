open! Core
module P = Agent_protocol
module S = Agent_store.Session_store
module Current = S.Lifecycle.Current
module Expected = P.Session_lifecycle.Expected
module Actor = Agent_session.Session_actor
module Registry = Session_registry

type t =
  { sw : Eio.Switch.t
  ; store : S.t
  ; registry : Registry.t
  ; factory : Session_factory.t
  ; now : unit -> P.Timestamp.t
  ; job_result_max_count : int
  ; job_result_max_bytes : int
  }

let create ~sw ~now ~job_result_max_count ~job_result_max_bytes ~store ~registry ~factory =
  { sw; store; registry; factory; now; job_result_max_count; job_result_max_bytes }
;;

let fail code message = Error (P.Error.create code ~message ~retryable:false ())
let stored result = Result.map_error result ~f:Agent_store.Store_error.to_protocol_error

let authorize principal summary =
  if not (P.Principal.has_scope principal Own_sessions)
  then fail Permission_denied "session selection requires owner scope"
  else if Authorization.session_visible_to principal summary
  then Ok ()
  else fail Permission_denied "session is not visible to this principal"
;;

let validate t expected current =
  let entry = Current.entry current in
  let reference = Expected.reference expected in
  if not (P.Id.Server.equal (P.Session_ref.server_id reference) (S.server_id t.store))
  then fail Invalid_request "session selection belongs to another host"
  else if
    (not (P.Id.Session.equal entry.session.id (P.Session_ref.session_id reference)))
    || (not (Int.equal entry.session.generation (Expected.generation expected)))
    || (not (Int64.equal entry.session.revision (Expected.session_revision expected)))
    || not
         (P.Session_lifecycle.Revision.equal
            entry.lifecycle_revision
            (Expected.lifecycle_revision expected))
  then fail Conflict "session selection anchor changed"
  else if
    entry.archived
    || not (Agent_store.Session_archive_record.Admission.equal entry.admission Automatic)
  then fail Invalid_state "session requires explicit restore or resume before selection"
  else Ok ()
;;

let checked_entry t session_id =
  let open Result.Let_syntax in
  let%bind entry =
    Agent_store.Session_index.find_checked (S.session_index t.store) session_id |> stored
  in
  Result.of_option
    entry
    ~error:
      (P.Error.create Session_not_found ~message:"session is absent" ~retryable:false ())
;;

(* A cleanup failure retains the actual capability before this ID's reservation
   releases. Successful cleanup never masks the original rejection/cancellation. *)
let with_cleanup t reservation ~owner ~cleanup f =
  let original =
    match f () with
    | result -> `Result result
    | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  Eio.Cancel.protect (fun () ->
    let secondary =
      match cleanup () with
      | Ok () -> None
      | Error error -> Some (Registry.Cleanup_failure.rejected error)
      | exception exn ->
        Some (Registry.Cleanup_failure.raised exn (Stdlib.Printexc.get_raw_backtrace ()))
    in
    Option.iter secondary ~f:(fun failure ->
      let primary =
        match original with
        | `Result (Error error) -> Registry.Cleanup_failure.rejected error
        | `Raised (exn, backtrace) -> Registry.Cleanup_failure.raised exn backtrace
        | `Result (Ok _) -> failure
      in
      Registry.retain_cleanup t.registry reservation ~owner ~primary ~failure);
    match original, secondary with
    | `Raised (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
    | `Result (Error error), _ -> Error error
    | `Result (Ok value), None -> Ok value
    | `Result (Ok _), Some failure ->
      (match Registry.Cleanup_failure.exception_and_backtrace failure with
       | Some (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
       | None -> Error (Registry.Cleanup_failure.error failure)))
;;

let qualify_recovered t entry =
  let open Result.Let_syntax in
  let qualify () =
    let%bind () =
      Job_scheduler.reconcile_entry
        entry
        ~max_count:t.job_result_max_count
        ~max_total_bytes:t.job_result_max_bytes
    in
    let%bind () = Schedule_scheduler.reconcile_entry entry ~startup_time:(t.now ()) in
    let%bind handle =
      Result.of_option
        entry.Registry.store_handle
        ~error:
          (P.Error.create
             Internal_error
             ~message:"selected owner has no Handle"
             ~retryable:false
             ())
    in
    let%bind indexed = checked_entry t (S.Handle.session_id handle) in
    let%map current =
      S.current_lifecycle t.store handle ~current_entry:indexed |> stored
    in
    entry, current
  in
  match qualify () with
  | Ok _ as success -> success
  | Error error as failure ->
    Registry.rollback_recovered
      t.registry
      ~primary:(Registry.Cleanup_failure.rejected error)
      [ entry ];
    failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Registry.rollback_recovered
      t.registry
      ~primary:(Registry.Cleanup_failure.raised exn backtrace)
      [ entry ];
    Exn.raise_with_original_backtrace exn backtrace
;;

let select_indexed t reservation principal expected indexed =
  let open Result.Let_syntax in
  let%bind handle =
    S.open_session
      t.store
      ~sw:t.sw
      ~actor_lock_nonce:(P.Id.Transaction.create () |> P.Id.Transaction.to_string)
      indexed.Agent_store.Session_index.Entry.session.id
    |> stored
  in
  let transferred = ref false in
  with_cleanup
    t
    reservation
    ~owner:(Registry.Cleanup_owner.handle ~store:t.store handle)
    ~cleanup:(fun () ->
      if !transferred then Ok () else S.close_session t.store handle |> stored)
    (fun () ->
       let%bind () =
         if Session_factory.owns_handle t.factory handle
         then Ok ()
         else fail Invalid_state "session Factory belongs to another Store"
       in
       let%bind current =
         S.current_lifecycle t.store handle ~current_entry:indexed |> stored
       in
       let%bind () = authorize principal (Current.entry current).session in
       let%bind () = validate t expected current in
       Registry.select_indexed
         t.registry
         reservation
         ~store:t.store
         ~handle
         ~current
         ~authorize:(authorize principal)
         ~recover:(fun () ->
           transferred := true;
           let%bind entry = Session_factory.recover_owned_session t.factory handle in
           qualify_recovered t entry))
;;

let select_loaded t reservation principal expected entry =
  let open Result.Let_syntax in
  let%bind handle =
    Result.of_option
      entry.Registry.store_handle
      ~error:
        (P.Error.create
           Invalid_state
           ~message:"selection requires a durable owner"
           ~retryable:false
           ())
  in
  let%bind () = authorize principal (S.Handle.metadata handle).session in
  let%bind fence =
    Actor.begin_lifecycle
      entry.actor
      ~attachment_id:None
      ~expected_generation:(Expected.generation expected)
      ~expected_revision:(Expected.session_revision expected)
  in
  with_cleanup
    t
    reservation
    ~owner:(Registry.Cleanup_owner.fence entry fence)
    ~cleanup:(fun () -> Actor.abort_lifecycle entry.actor fence)
    (fun () ->
       let%bind indexed = checked_entry t (S.Handle.session_id handle) in
       let%bind current =
         S.current_lifecycle t.store handle ~current_entry:indexed |> stored
       in
       let%bind () = authorize principal (Current.entry current).session in
       let%bind () = validate t expected current in
       if Current.is_current current handle && not (Registry.is_closing t.registry)
       then Ok entry
       else fail Conflict "selected owner became unavailable")
;;

let select t ~principal ~expected =
  let reference = Expected.reference expected in
  if not (P.Id.Server.equal (P.Session_ref.server_id reference) (S.server_id t.store))
  then fail Invalid_request "session selection belongs to another host"
  else
    Registry.with_lifecycle
      t.registry
      (P.Session_ref.session_id reference)
      (fun reservation ->
         match Registry.Lifecycle_reservation.target reservation with
         | Absent -> fail Session_not_found "session is absent"
         | Indexed indexed -> select_indexed t reservation principal expected indexed
         | Loaded entry -> select_loaded t reservation principal expected entry)
;;
