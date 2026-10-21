open! Core

type 'stage phase =
  | Allocated
  | Prepared of 'stage
  | Attempted
  | Retained
  | Discarded

type 'stage t =
  { lock : Result_publication_lock.t
  ; mutable phase : 'stage phase
  }

let create () = { lock = Result_publication_lock.create (); phase = Allocated }

let restored_attempted () =
  { lock = Result_publication_lock.create (); phase = Attempted }
;;

let store_reentry () = Error (Store_error.Corrupt "recursive result preparation")

let protocol_reentry () =
  Error
    (Agent_protocol.Error.create
       Invalid_state
       ~message:"recursive result preparation"
       ~retryable:false
       ())
;;

let stage_locked t ~stage =
  match t.phase with
  | Retained | Discarded -> Error (Store_error.Corrupt "result preparation has ended")
  | (Allocated | Prepared _ | Attempted) as phase ->
    let open Result.Let_syntax in
    let%map prepared = stage () in
    (match phase with
     | Allocated | Prepared _ -> t.phase <- Prepared prepared
     | Attempted -> ()
     | Retained | Discarded -> assert false)
;;

let ensure_prepared t ~stage =
  Result_publication_lock.with_owner t.lock ~on_reentry:store_reentry (fun () ->
    stage_locked t ~stage)
;;

let ensure_attempted t ~stage =
  Result_publication_lock.with_owner t.lock ~on_reentry:store_reentry (fun () ->
    let open Result.Let_syntax in
    let%map () = stage_locked t ~stage in
    t.phase <- Attempted)
;;

let already_resolved () =
  Agent_protocol.Error.create
    Already_resolved
    ~message:"result artifact preparation has ended"
    ~retryable:false
    ()
;;

let commit t ~persist ~cleanup =
  Result_publication_lock.with_owner t.lock ~on_reentry:protocol_reentry (fun () ->
    match t.phase with
    | Retained | Discarded -> Error (already_resolved ())
    | Allocated ->
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:"result bytes are not prepared"
           ~retryable:true
           ())
    | Prepared _ | Attempted ->
      t.phase <- Attempted;
      let open Result.Let_syntax in
      let%map result = persist () in
      t.phase <- Retained;
      ignore (cleanup () : (unit, Store_error.t) result);
      result)
;;

let acknowledge_installed t ~cleanup =
  Result_publication_lock.with_owner t.lock ~on_reentry:store_reentry (fun () ->
    match t.phase with
    | Retained -> Ok ()
    | Attempted ->
      t.phase <- Retained;
      ignore (cleanup () : (unit, Store_error.t) result);
      Ok ()
    | Allocated | Prepared _ | Discarded ->
      Error (Store_error.Corrupt "result selection was not attempted"))
;;

let discard t ~discard =
  Result_publication_lock.with_owner t.lock ~on_reentry:store_reentry (fun () ->
    match t.phase with
    | Retained -> Error (Store_error.Corrupt "cannot discard a retained result artifact")
    | Allocated | Attempted ->
      Error
        (Store_error.Corrupt
           "result artifact requires durable reference reconciliation before discard")
    | Discarded -> Ok ()
    | Prepared stage ->
      let open Result.Let_syntax in
      let%map () = discard stage in
      t.phase <- Discarded)
;;
