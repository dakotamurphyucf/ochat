open! Core
module Row = Developer_command_row
module Receipt = Developer_command_receipt
module Limits = Developer_command_limits
module P = Agent_protocol

type t =
  { rows : Row.t list
  ; count : int
  ; encoded_bytes : int
  }

let empty = { rows = []; count = 0; encoded_bytes = 0 }
let rows t = t.rows
let count t = t.count
let encoded_bytes t = t.encoded_bytes

let find t key =
  List.find t.rows ~f:(fun row -> P.Idempotency_key.equal (Row.key row) key)
;;

let ( let* ) result f = Result.bind result ~f

let limit message =
  Error (Store_error.Document (Document_schema.Error.Limit_exceeded message))
;;

let of_rows rows ~commands ~poll ~check_owner =
  let keys = Hash_set.create (module String) in
  let ids = Hash_set.create (module String) in
  let rec loop count encoded_bytes = function
    | [] -> Ok { rows; count; encoded_bytes }
    | row :: rest ->
      poll ();
      let* () =
        if count >= Limits.max_commands commands
        then limit "developer command count exceeds limit"
        else if Row.encoded_bytes row > Limits.max_bytes commands - encoded_bytes
        then limit "developer command rows exceed aggregate byte limit"
        else Ok ()
      in
      let* () =
        match Row.phase row with
        | Terminal { receipt; _ }
          when Receipt.encoded_bytes receipt > Limits.max_terminal_bytes commands ->
          limit "developer command terminal exceeds byte limit"
        | Reserved | Waiting_permission | Execution_started | Terminal _ -> Ok ()
      in
      let key = P.Idempotency_key.to_string (Row.key row) in
      let id = Receipt.Id.to_string (Row.id row) in
      let* () =
        if Hash_set.mem keys key || Hash_set.mem ids id
        then Error (Store_error.Corrupt "duplicate developer command key or ID")
        else Ok ()
      in
      Hash_set.add keys key;
      Hash_set.add ids id;
      let* () = check_owner row in
      loop (count + 1) (encoded_bytes + Row.encoded_bytes row) rest
  in
  loop 0 0 rows
;;

let append t row ~commands ~poll ~check_owner =
  of_rows (t.rows @ [ row ]) ~commands ~poll ~check_owner
;;

let change t ~expected ~change ~limits ~commands ~poll ~check_owner =
  let rec replace reversed = function
    | [] -> Error Row.Error.Conflict
    | row :: rest ->
      poll ();
      if P.Idempotency_key.equal (Row.key row) (Row.key expected)
      then
        if not (Document_schema.Json.equal (Row.to_json row) (Row.to_json expected))
        then Error Row.Error.Conflict
        else
          let* next = Row.apply row change ~limits ~poll in
          of_rows (List.rev_append reversed (next :: rest)) ~commands ~poll ~check_owner
          |> Result.map_error ~f:(fun error -> Row.Error.Store error)
      else replace (row :: reversed) rest
  in
  replace [] t.rows
;;

let interrupted t ~limits ~commands ~poll =
  let* reversed, changed =
    List.fold_result t.rows ~init:([], 0) ~f:(fun (reversed, changed) row ->
      poll ();
      match Row.phase row with
      | Terminal _ -> Ok (row :: reversed, changed)
      | Reserved | Waiting_permission | Execution_started ->
        let value =
          match Row.kind row with
          | Check -> Receipt.Check Interrupted
          | Fixture_run -> Fixture Interrupted
          | Live_run -> Live Interrupted
        in
        let* receipt = Receipt.create value ~limits ~poll |> Document_fields.store in
        let* next =
          Row.apply row (Finish receipt) ~limits ~poll
          |> Result.map_error ~f:(function
            | Row.Error.Conflict -> Store_error.Corrupt "invalid command interruption"
            | Store error -> error)
        in
        Ok (next :: reversed, changed + 1))
  in
  let* recovery =
    of_rows (List.rev reversed) ~commands ~poll ~check_owner:(fun _ -> Ok ())
  in
  Ok (recovery, changed)
;;
