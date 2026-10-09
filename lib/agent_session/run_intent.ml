open! Core
module P = Agent_protocol
module J = P.Json_codec
module X = Persistence_codec

module Disposition = struct
  type t =
    | Pending
    | Consumed of P.Id.Operation.t option
    | Retired
  [@@deriving equal]

  let to_json = function
    | Pending -> `Object [ "kind", `String "pending" ]
    | Retired -> `Object [ "kind", `String "retired" ]
    | Consumed operation ->
      `Object
        [ "kind", `String "consumed"
        ; "operation_id", X.option_json P.Id.Operation.to_json operation
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    match kind with
    | "pending" -> Ok Pending
    | "retired" -> Ok Retired
    | "consumed" ->
      let%map operation =
        J.required_as fields "operation_id" (X.nullable P.Id.Operation.of_json)
      in
      Consumed operation
    | _ -> Error (P.Error.invalid_request "unsupported run intent disposition")
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

type t =
  { receipt : P.Run_receipt.t
  ; execution_id : P.Id.Moderator_execution.t
  ; action : P.Run_action.t
  ; disposition : Disposition.t
  }
[@@deriving equal]

let create ~(receipt : P.Run_receipt.t) ~execution_id ~action =
  let open Result.Let_syntax in
  let%bind _ =
    P.Id.Moderator_execution.of_json (P.Id.Moderator_execution.to_json execution_id)
  in
  let%bind () = P.Run_action.validate action in
  match receipt.kind with
  | Admission | Terminal ->
    Error (P.Error.invalid_request "run intent requires an action receipt")
  | Action -> Ok { receipt; execution_id; action; disposition = Pending }
;;

let consume t ~operation_id =
  let open Result.Let_syntax in
  let%bind () =
    match operation_id with
    | None -> Ok ()
    | Some id ->
      Result.map (P.Id.Operation.of_json (P.Id.Operation.to_json id)) ~f:(fun _ -> ())
  in
  match t.disposition with
  | Pending -> Ok { t with disposition = Consumed operation_id }
  | Consumed previous when Option.equal P.Id.Operation.equal previous operation_id -> Ok t
  | Consumed _ | Retired ->
    Error (P.Error.invalid_request "run intent already has a different disposition")
;;

let retire t =
  match t.disposition with
  | Pending -> { t with disposition = Retired }
  | Consumed _ | Retired -> t
;;

let to_jsonaf t =
  `Object
    [ "receipt", P.Run_receipt.to_json t.receipt
    ; "execution_id", P.Id.Moderator_execution.to_json t.execution_id
    ; "action", P.Run_action.to_json t.action
    ; "disposition", Disposition.to_json t.disposition
    ]
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind receipt = J.required_as fields "receipt" P.Run_receipt.of_json in
  let%bind execution_id =
    J.required_as fields "execution_id" P.Id.Moderator_execution.of_json
  in
  let%bind action = J.required_as fields "action" P.Run_action.of_json in
  let%bind disposition = J.required_as fields "disposition" Disposition.of_json in
  let%map t = create ~receipt ~execution_id ~action in
  { t with disposition }
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_jsonaf t)

let t_of_sexp sexp =
  match of_jsonaf (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;

let shape = Run_record_shapes.intent
