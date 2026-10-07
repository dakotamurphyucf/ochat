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

module X = Persistence_codec
module J = Agent_protocol.Json_codec

let reference_of_jsonaf json =
  D.reference_of_jsonaf json
  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
;;

let stop_mode_to_jsonaf = function
  | P.Session.Cancel -> `String "cancel"
  | Graceful -> `String "graceful"
;;

let stop_mode_of_jsonaf =
  J.enum ~name:"stop mode" [ "cancel", P.Session.Cancel; "graceful", P.Session.Graceful ]
;;

let storage_to_jsonaf (t : t) =
  `Object
    [ "id", P.Id.Transaction.to_json t.id
    ; "reference", D.reference_to_jsonaf t.reference
    ; "key", P.Idempotency_key.to_json t.key
    ; "mode", stop_mode_to_jsonaf t.mode
    ; "generation", X.integer_json t.generation
    ; "stop_epoch", X.int64_json t.stop_epoch
    ; "accepted_at", P.Timestamp.to_json t.accepted_at
    ]
;;

let storage_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind id = X.required fields "id" P.Id.Transaction.of_json in
  let%bind reference = X.required fields "reference" reference_of_jsonaf in
  let%bind key = X.required fields "key" P.Idempotency_key.of_json in
  let%bind mode = X.required fields "mode" stop_mode_of_jsonaf in
  let%bind generation = X.required fields "generation" X.integer in
  let%bind stop_epoch = X.required fields "stop_epoch" X.nonnegative_int64 in
  let%bind accepted_at = X.required fields "accepted_at" P.Timestamp.of_json in
  let t : t = { id; reference; key; mode; generation; stop_epoch; accepted_at } in
  let%map () = validate t in
  t
;;

let storage_shape =
  X.shape_exn
    [ "id", Document_schema.Shape.value
    ; "reference", D.reference_shape
    ; "key", Document_schema.Shape.value
    ; "mode", Document_schema.Shape.value
    ; "generation", Document_schema.Shape.value
    ; "stop_epoch", Document_schema.Shape.value
    ; "accepted_at", Document_schema.Shape.value
    ]
;;

let to_jsonaf = storage_to_jsonaf
let of_jsonaf = storage_of_jsonaf
let shape = storage_shape
