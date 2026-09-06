open Core

type t =
  { schema_version : int
  ; session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; transaction_sequence : int64
  ; previous_transaction_hash : string option
  ; session_revision : int64
  ; first_event_sequence : int64 option
  ; last_event_sequence : int64 option
  ; accepted_at_ns : int64
  ; command_audit : string option
  ; delta : string
  ; durable_events : string list
  }
[@@deriving sexp]

module Persisted = struct
  type t =
    { schema_version : int
    ; session_id : string
    ; generation : int
    ; transaction_sequence : int64
    ; previous_transaction_hash : string option
    ; session_revision : int64
    ; first_event_sequence : int64 option
    ; last_event_sequence : int64 option
    ; accepted_at_ns : int64
    ; command_audit : string option
    ; delta : string
    ; durable_events : string list
    }
  [@@deriving bin_io]
end

let current_schema_version = 1

let nonnegative name value =
  if Int64.(value < zero)
  then Error (Store_error.Corrupt (name ^ " must be nonnegative"))
  else Ok ()
;;

let validate_event_range first last events =
  match first, last, events with
  | None, None, [] -> Ok ()
  | Some first, Some last, _ when Int64.(first >= zero && last >= first) ->
    let expected = Int64.(last - first + one) in
    if Int64.equal expected (Int64.of_int (List.length events))
    then Ok ()
    else Error (Store_error.Corrupt "durable event range does not match event count")
  | _ -> Error (Store_error.Corrupt "durable event range is inconsistent")
;;

let create
      ~session_id
      ~generation
      ~transaction_sequence
      ~previous_transaction_hash
      ~session_revision
      ~first_event_sequence
      ~last_event_sequence
      ~accepted_at_ns
      ~command_audit
      ~delta
      ~durable_events
  =
  let open Result.Let_syntax in
  if generation < 0
  then Error (Store_error.Corrupt "transaction generation must be nonnegative")
  else (
    let%bind () = nonnegative "transaction sequence" transaction_sequence in
    let%bind () = nonnegative "session revision" session_revision in
    let%bind () = nonnegative "accepted timestamp" accepted_at_ns in
    let%map () =
      validate_event_range first_event_sequence last_event_sequence durable_events
    in
    { schema_version = current_schema_version
    ; session_id
    ; generation
    ; transaction_sequence
    ; previous_transaction_hash
    ; session_revision
    ; first_event_sequence
    ; last_event_sequence
    ; accepted_at_ns
    ; command_audit
    ; delta
    ; durable_events
    })
;;

let validate transaction =
  let open Result.Let_syntax in
  if transaction.schema_version > current_schema_version
  then Error (Store_error.Schema_too_new transaction.schema_version)
  else if transaction.schema_version < current_schema_version
  then Error (Store_error.Migration_required transaction.schema_version)
  else if transaction.generation < 0
  then Error (Store_error.Corrupt "transaction generation must be nonnegative")
  else (
    let%bind () = nonnegative "transaction sequence" transaction.transaction_sequence in
    let%bind () = nonnegative "session revision" transaction.session_revision in
    let%bind () = nonnegative "accepted timestamp" transaction.accepted_at_ns in
    validate_event_range
      transaction.first_event_sequence
      transaction.last_event_sequence
      transaction.durable_events)
;;

let to_persisted (transaction : t) =
  Persisted.
    { schema_version = transaction.schema_version
    ; session_id = Agent_protocol.Id.Session.to_string transaction.session_id
    ; generation = transaction.generation
    ; transaction_sequence = transaction.transaction_sequence
    ; previous_transaction_hash = transaction.previous_transaction_hash
    ; session_revision = transaction.session_revision
    ; first_event_sequence = transaction.first_event_sequence
    ; last_event_sequence = transaction.last_event_sequence
    ; accepted_at_ns = transaction.accepted_at_ns
    ; command_audit = transaction.command_audit
    ; delta = transaction.delta
    ; durable_events = transaction.durable_events
    }
;;

let encode transaction =
  (match validate transaction with
   | Ok () -> ()
   | Error error -> raise_s [%sexp "invalid transaction", (error : Store_error.t)]);
  Bin_prot.Utils.bin_dump ~header:false Persisted.bin_writer_t (to_persisted transaction)
  |> Bigstring.to_string
;;

let of_persisted (persisted : Persisted.t) =
  let open Result.Let_syntax in
  if persisted.Persisted.schema_version > current_schema_version
  then Error (Store_error.Schema_too_new persisted.schema_version)
  else if persisted.schema_version < current_schema_version
  then Error (Store_error.Migration_required persisted.schema_version)
  else (
    let%bind session_id =
      Agent_protocol.Id.Session.of_string persisted.session_id
      |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
    in
    create
      ~session_id
      ~generation:persisted.generation
      ~transaction_sequence:persisted.transaction_sequence
      ~previous_transaction_hash:persisted.previous_transaction_hash
      ~session_revision:persisted.session_revision
      ~first_event_sequence:persisted.first_event_sequence
      ~last_event_sequence:persisted.last_event_sequence
      ~accepted_at_ns:persisted.accepted_at_ns
      ~command_audit:persisted.command_audit
      ~delta:persisted.delta
      ~durable_events:persisted.durable_events)
;;

let decode encoded =
  try Bin_prot.Reader.of_string Persisted.bin_reader_t encoded |> of_persisted with
  | exn -> Error (Store_error.Corrupt ("transaction decode failed: " ^ Exn.to_string exn))
;;

let hash transaction = Digestif.SHA256.(digest_string (encode transaction) |> to_hex)
