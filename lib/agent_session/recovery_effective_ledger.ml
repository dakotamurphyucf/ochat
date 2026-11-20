open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Record = Recovery_effective_record
module Scope = P.Recovery_window.Scope
module Windows = Recovery_window_ledger

let invalid reason = Error (P.Error.invalid_request reason)
let max_preparations = 64
let max_members = 8192

let limits =
  D.Limits.create
    ~max_bytes:(2 * 1024 * 1024)
    ~max_depth:32
    ~max_fields:(16 * max_members)
    ~max_nodes:(22 * max_members)
  |> Result.map_error ~f:(fun error -> Error.create_s [%sexp (error : D.Error.t)])
  |> Or_error.ok_exn
;;

let validate_encoded = D.Json.validate ~limits

type t =
  { current_incarnation : int64 option
  ; records : Record.t list
  }

let records t = t.records
let current_incarnation t = t.current_incarnation

let rotate_incarnation t =
  match t.current_incarnation with
  | None -> t
  | Some _ -> { t with current_incarnation = None }
;;

let to_jsonaf t =
  `Object
    [ "version", `Number "1"
    ; "current_incarnation", X.option_json X.int64_json t.current_incarnation
    ; "preparations", X.list_json Record.to_jsonaf t.records
    ]
;;

type admission_error =
  | Capacity
  | Invalid of P.Error.t

let capacity_error =
  P.Error.invalid_request "effective preparation exceeds its retained evidence capacity"
;;

let admit t =
  let open Result.Let_syntax in
  let fail message = Error (Invalid (P.Error.invalid_request message)) in
  if List.is_empty t.records
  then fail "effective preparation ledger has no records"
  else if Stdlib.List.compare_length_with t.records max_preparations > 0
  then Error Capacity
  else (
    let%bind last, _ =
      List.fold_result t.records ~init:(None, 0) ~f:(fun (last, count) record ->
        let source_revision = Record.source_revision record in
        let count = count + List.length (Record.members record) in
        if
          Option.exists last ~f:(fun previous ->
            Int64.(Record.source_revision previous >= source_revision))
        then fail "effective preparation ledger has duplicate or unordered evidence"
        else if count > max_members
        then Error Capacity
        else Ok (Some record, count))
    in
    let%bind () =
      match t.current_incarnation, last with
      | None, _ -> Ok ()
      | Some epoch, Some record when Int64.equal epoch (Record.overlay_incarnation record)
        -> Ok ()
      | Some _, (None | Some _) ->
        fail "effective preparation incarnation is not committed"
    in
    let%map () =
      validate_encoded (to_jsonaf t)
      |> Result.map_error ~f:(function
        | Limit_exceeded _ -> Capacity
        | _ -> Invalid (P.Error.invalid_request "invalid effective preparation encoding"))
    in
    t)
;;

let has_record_capacity t =
  Option.for_all t ~f:(fun ledger ->
    Stdlib.List.compare_length_with ledger.records max_preparations < 0)
;;

let add_if_room previous record =
  let revision = Record.source_revision record in
  let epoch = Record.overlay_incarnation record in
  let expected, records =
    match previous with
    | None -> revision, []
    | Some previous ->
      Option.value previous.current_incarnation ~default:revision, previous.records
  in
  if not (Int64.equal epoch expected)
  then invalid "effective preparation uses another overlay incarnation"
  else (
    match admit { current_incarnation = Some epoch; records = records @ [ record ] } with
    | Ok added -> Ok (Some added)
    | Error Capacity -> Ok None
    | Error (Invalid error) -> Error error)
;;

let add previous record =
  let%bind.Result added = add_if_room previous record in
  Result.of_option added ~error:capacity_error
;;

let validate_state t ~windows ~publication_revision =
  match windows with
  | None -> invalid "effective preparation requires its window ledger"
  | Some windows ->
    let scopes = List.map (Windows.windows windows) ~f:Windows.Window.scope in
    List.fold_result t.records ~init:() ~f:(fun () record ->
      if
        Int64.(Record.source_revision record < publication_revision)
        && List.mem scopes (Record.scope record) ~equal:Scope.equal
      then Ok ()
      else invalid "effective preparation names an unrelated window or uncommitted source")
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () =
    validate_encoded json
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "invalid effective preparation encoding")
  in
  let%bind fields = X.object_ json in
  let%bind _ = X.required fields "version" (P.Json_codec.bounded_int ~min:1 ~max:1) in
  let%bind current_incarnation =
    X.required fields "current_incarnation" (X.nullable X.nonnegative_int64)
  in
  let decode_records = function
    | `Array records when Stdlib.List.compare_length_with records max_preparations <= 0 ->
      List.map records ~f:Record.of_jsonaf |> Result.all
    | _ -> invalid "invalid effective preparation count"
  in
  let%bind records = X.required fields "preparations" decode_records in
  admit { current_incarnation; records }
  |> Result.map_error ~f:(function
    | Capacity -> capacity_error
    | Invalid error -> error)
;;

let shape =
  X.shape_exn
    [ "version", D.Shape.value
    ; "current_incarnation", D.Shape.value
    ; "preparations", X.array_shape_exn ~identity_field:"id" Record.shape
    ]
;;
