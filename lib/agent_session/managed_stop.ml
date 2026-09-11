open Core
module P = Agent_protocol
module D = Agent_store.Delegation_store

type t =
  { id : P.Id.Transaction.t
  ; reference : D.Reference.t
  ; key : P.Idempotency_key.t
  ; mode : P.Session.stop_mode
  ; generation : int
  ; stop_epoch : int64
  ; accepted_at : P.Timestamp.t
  }
[@@deriving equal, sexp]

let same_key left right =
  D.Reference.equal left.reference right.reference
  && P.Idempotency_key.equal left.key right.key
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    D.validate_reference t.reference
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  match t.generation >= 0 && Int64.(t.stop_epoch >= 0L) with
  | true -> Ok ()
  | false -> Error (P.Error.invalid_request "invalid managed stop receipt")
;;

let create ~reference ~key ~mode ~generation ~stop_epoch ~now =
  let t =
    { id = P.Id.Transaction.create ()
    ; reference
    ; key
    ; mode
    ; generation
    ; stop_epoch
    ; accepted_at = now
    }
  in
  Result.map (validate t) ~f:(fun () -> t)
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; "receipt_id", P.Id.Transaction.to_json t.id
    ; "session_id", P.Id.Session.to_json t.reference.child_session_id
    ; "idempotency_key", P.Idempotency_key.to_json t.key
    ; ( "mode"
      , `String
          (match t.mode with
           | Graceful -> "graceful"
           | Cancel -> "cancel") )
    ; "generation", `Number (Int.to_string t.generation)
    ; "stop_epoch", `Number (Int64.to_string t.stop_epoch)
    ; "accepted_at", P.Timestamp.to_json t.accepted_at
    ]
;;
