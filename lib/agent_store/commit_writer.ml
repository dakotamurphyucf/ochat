open Core

type committed =
  { transaction_sequence : int64
  ; transaction_hash : string
  ; journal_position : Journal.append_result
  }

module Reply = struct
  type t =
    | Returned of (committed, Store_error.t) result
    | Raised of
        { exception_ : exn
        ; backtrace : Stdlib.Printexc.raw_backtrace
        }
end

type request =
  | Commit of
      { transaction : Transaction.t
      ; durability : Journal_segment.durability
      ; resolver : Reply.t Eio.Promise.u
      }
  | Close of unit Eio.Promise.u

type availability =
  | Available
  | Unavailable of Store_error.t
  | Closed

type t =
  { requests : request Eio.Stream.t
  ; session_id : Agent_protocol.Id.Session.t
  ; stopped : unit Eio.Promise.t
  ; mutable availability : availability
  }

type state =
  { next_sequence : int64
  ; previous_hash : string option
  }

let same_session left right = Agent_protocol.Id.Session.compare left right = 0

let validate t state transaction =
  let open Result.Let_syntax in
  let%bind () = Transaction.validate transaction in
  if not (same_session t.session_id transaction.Transaction.session_id)
  then Error (Store_error.Corrupt "commit transaction belongs to another session")
  else if not (Int64.equal transaction.transaction_sequence state.next_sequence)
  then Error (Store_error.Corrupt "commit transaction sequence is discontinuous")
  else if
    not
      (Option.equal
         String.equal
         transaction.previous_transaction_hash
         state.previous_hash)
  then Error (Store_error.Corrupt "commit transaction hash chain is discontinuous")
  else Ok ()
;;

let commit_one t journal state ~durability transaction =
  let open Result.Let_syntax in
  let%bind () = validate t state transaction in
  let payload = Transaction.encode transaction in
  let transaction_hash = Transaction.hash transaction in
  let%map journal_position = Journal.append journal ~durability ~flags:0 ~payload in
  ( { transaction_sequence = transaction.transaction_sequence
    ; transaction_hash
    ; journal_position
    }
  , { next_sequence =
        (if Int64.equal state.next_sequence Int64.max_value
         then state.next_sequence
         else Int64.succ state.next_sequence)
    ; previous_hash = Some transaction_hash
    } )
;;

let resolve_commit t journal state transaction durability resolver =
  match t.availability with
  | Unavailable error ->
    Eio.Promise.resolve resolver (Reply.Returned (Error error));
    state
  | Closed ->
    Eio.Promise.resolve
      resolver
      (Reply.Returned (Error (Store_error.Corrupt "commit writer is closed")));
    state
  | Available ->
    let attempted =
      try Ok (commit_one t journal state ~durability transaction) with
      | exception_ -> Error (exception_, Stdlib.Printexc.get_raw_backtrace ())
    in
    (match attempted with
     | Ok (Ok (committed, next_state)) ->
       if Int64.equal state.next_sequence Int64.max_value
       then
         t.availability
         <- Unavailable (Store_error.Corrupt "transaction sequence is exhausted");
       Eio.Promise.resolve resolver (Reply.Returned (Ok committed));
       next_state
     | Ok (Error error) ->
       t.availability <- Unavailable error;
       Eio.Promise.resolve resolver (Reply.Returned (Error error));
       state
     | Error (exception_, backtrace) ->
       (* Journal counters may lag a completed physical write. No later commit
          may use this head until the canonical journal has been reopened. *)
       t.availability
       <- Unavailable
            (Store_error.Corrupt
               "commit writer requires journal recovery after interrupted commit");
       Eio.Promise.resolve resolver (Reply.Raised { exception_; backtrace });
       (* An injected Timeout/Cancelled is an operation failure. Actual owning
          context cancellation must still terminate this worker promptly. *)
       Eio.Fiber.check ();
       state)
;;

let rec run t journal state =
  match Eio.Stream.take t.requests with
  | Close resolver ->
    t.availability <- Closed;
    Eio.Promise.resolve resolver ()
  | Commit { transaction; durability; resolver } ->
    resolve_commit t journal state transaction durability resolver |> run t journal
;;

let create
      ~sw
      ~journal
      ~session_id
      ~next_transaction_sequence
      ~previous_transaction_hash
      ~queue_capacity
  =
  if queue_capacity <= 0
  then Error (Store_error.Corrupt "commit writer queue capacity must be positive")
  else if Int64.(next_transaction_sequence <= zero)
  then Error (Store_error.Corrupt "next transaction sequence must be positive")
  else (
    let stopped, stopped_resolver = Eio.Promise.create () in
    let t =
      { requests = Eio.Stream.create queue_capacity
      ; session_id
      ; stopped
      ; availability = Available
      }
    in
    let state =
      { next_sequence = next_transaction_sequence
      ; previous_hash = previous_transaction_hash
      }
    in
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~f:(fun () -> run t journal state)
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
            t.availability <- Closed;
            Eio.Promise.resolve stopped_resolver ())));
    Ok t)
;;

let await_reply t ~promise ~enqueue ~if_stopped =
  Eio.Fiber.first
    (fun () ->
       enqueue ();
       Eio.Promise.await promise)
    (fun () ->
       Eio.Promise.await t.stopped;
       (* Stopping can race an already resolved success or exceptional reply.
         Its exact result always wins over the generic lifetime failure. *)
       match Eio.Promise.peek promise with
       | Some reply -> reply
       | None -> if_stopped)
;;

let closed_error () = Store_error.Corrupt "commit writer is closed"

let commit t ~durability transaction =
  match t.availability with
  | Closed -> Error (closed_error ())
  | Unavailable failure -> Error failure
  | Available ->
    let promise, resolver = Eio.Promise.create () in
    let reply =
      await_reply
        t
        ~promise
        ~enqueue:(fun () ->
          Eio.Stream.add t.requests (Commit { transaction; durability; resolver }))
        ~if_stopped:(Reply.Returned (Error (closed_error ())))
    in
    (match reply with
     | Reply.Returned result -> result
     | Raised { exception_; backtrace } ->
       Exn.raise_with_original_backtrace exception_ backtrace)
;;

let close t =
  match t.availability with
  | Closed -> ()
  | Available | Unavailable _ ->
    let promise, resolver = Eio.Promise.create () in
    await_reply
      t
      ~promise
      ~enqueue:(fun () -> Eio.Stream.add t.requests (Close resolver))
      ~if_stopped:()
;;
