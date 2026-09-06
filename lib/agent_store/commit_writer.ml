open Core

type committed =
  { transaction_sequence : int64
  ; transaction_hash : string
  ; journal_position : Journal.append_result
  }

type request =
  | Commit of
      { transaction : Transaction.t
      ; durability : Journal_segment.durability
      ; resolver : (committed, Store_error.t) result Eio.Promise.u
      }
  | Close of unit Eio.Promise.u

type t =
  { requests : request Eio.Stream.t
  ; session_id : Agent_protocol.Id.Session.t
  ; mutable closed : bool
  }

type state =
  { next_sequence : int64
  ; previous_hash : string option
  ; failed : Store_error.t option
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
  let transaction_hash = Transaction.hash transaction in
  let%map journal_position =
    Journal.append journal ~durability ~flags:0 ~payload:(Transaction.encode transaction)
  in
  ( { transaction_sequence = transaction.transaction_sequence
    ; transaction_hash
    ; journal_position
    }
  , { next_sequence = Int64.succ state.next_sequence
    ; previous_hash = Some transaction_hash
    ; failed = None
    } )
;;

let resolve_commit t journal state transaction durability resolver =
  match state.failed with
  | Some error ->
    Eio.Promise.resolve resolver (Error error);
    state
  | None ->
    (match commit_one t journal state ~durability transaction with
     | Ok (committed, state) ->
       Eio.Promise.resolve resolver (Ok committed);
       state
     | Error error ->
       Eio.Promise.resolve resolver (Error error);
       { state with failed = Some error })
;;

let rec run t journal state =
  match Eio.Stream.take t.requests with
  | Close resolver ->
    t.closed <- true;
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
    let t = { requests = Eio.Stream.create queue_capacity; session_id; closed = false } in
    let state =
      { next_sequence = next_transaction_sequence
      ; previous_hash = previous_transaction_hash
      ; failed = None
      }
    in
    Eio.Fiber.fork ~sw (fun () -> run t journal state);
    Ok t)
;;

let commit t ~durability transaction =
  if t.closed
  then Error (Store_error.Corrupt "commit writer is closed")
  else (
    let promise, resolver = Eio.Promise.create () in
    Eio.Stream.add t.requests (Commit { transaction; durability; resolver });
    Eio.Promise.await promise)
;;

let close t =
  if not t.closed
  then (
    let promise, resolver = Eio.Promise.create () in
    Eio.Stream.add t.requests (Close resolver);
    Eio.Promise.await promise)
;;
