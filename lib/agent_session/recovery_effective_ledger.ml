open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Record = Recovery_effective_record
module Archive = Record.Archive
module Attempt = Recovery_effective_attempt
module Scope = P.Recovery_window.Scope
module Windows = Recovery_window_ledger

let invalid reason = Error (P.Error.invalid_request reason)
let max_preparations = 64
let max_members = Agent_protocol.Recovery_window.max_members

let limits ~archive_budget ~attempt_budget =
  D.Limits.create
    ~max_bytes:((2 * 1024 * 1024) + archive_budget + attempt_budget)
    ~max_depth:32
    ~max_fields:
      ((16 * max_members)
       + (if archive_budget > 0 then 6 * max_preparations else 0)
       + if attempt_budget > 0 then 16 * max_preparations else 0)
    ~max_nodes:
      ((22 * max_members)
       + (if archive_budget > 0 then 5 * max_members else 0)
       + if attempt_budget > 0 then 24 * max_preparations else 0)
  |> Result.map_error ~f:(fun error -> Error.create_s [%sexp (error : D.Error.t)])
  |> Or_error.ok_exn
;;

let preparation_limits = limits ~archive_budget:0 ~attempt_budget:0

(* A single grouped archive header per preparation and at most one triple of
   bounded decimal positions per member. Reserve their encoded growth
   separately so a qualified transfer cannot consume preparation capacity. *)
let storage_limits = limits ~archive_budget:(256 * 1024) ~attempt_budget:(64 * 1024)
let validate_encoded = D.Json.validate ~limits:storage_limits

type t =
  { current_incarnation : int64 option
  ; records : Record.t list
  }

let records t = t.records
let current_incarnation t = t.current_incarnation

let same_storage_reservation t other =
  phys_equal t other
  || (Option.equal Int64.equal t.current_incarnation other.current_incarnation
      && List.equal Record.same_storage_reservation t.records other.records)
;;

let rotate_incarnation t =
  match t.current_incarnation with
  | None -> t
  | Some _ -> { t with current_incarnation = None }
;;

let encode t ~record =
  `Object
    [ "version", `Number "1"
    ; "current_incarnation", X.option_json X.int64_json t.current_incarnation
    ; "preparations", X.list_json record t.records
    ]
;;

let to_jsonaf t = encode t ~record:Record.to_jsonaf

let has_live_sources t =
  List.exists t.records ~f:(fun record ->
    match Record.custody record with
    | Live -> true
    | Lost | Archived _ -> false)
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
    let ordinals = Hash_set.create (module Int64) in
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
        else (
          match Record.attempt record with
          | Some attempt when Hash_set.mem ordinals (Attempt.ordinal attempt) ->
            fail "actual root attempt appears under more than one preparation"
          | attempt ->
            Option.iter attempt ~f:(fun attempt ->
              Hash_set.add ordinals (Attempt.ordinal attempt));
            Ok (Some record, count)))
    in
    let%bind () =
      match t.current_incarnation, last with
      | None, _ -> Ok ()
      | Some epoch, Some record when Int64.equal epoch (Record.overlay_incarnation record)
        -> Ok ()
      | Some _, (None | Some _) ->
        fail "effective preparation incarnation is not committed"
    in
    let classify = function
      | D.Error.Limit_exceeded _ -> Capacity
      | _ -> Invalid (P.Error.invalid_request "invalid effective preparation encoding")
    in
    let%bind () =
      D.Json.validate
        ~limits:preparation_limits
        (encode t ~record:Record.to_preparation_jsonaf)
      |> Result.map_error ~f:classify
    in
    let%map () = validate_encoded (to_jsonaf t) |> Result.map_error ~f:classify in
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

let relocate t ~transfer =
  let open Result.Let_syntax in
  if not (has_live_sources t)
  then Ok t
  else (
    let%bind records = List.map t.records ~f:transfer |> Result.all in
    admit { t with records }
    |> Result.map_error ~f:(function
      | Capacity -> capacity_error
      | Invalid error -> error))
;;

let update_attempt t ~source_revision attempt =
  let open Result.Let_syntax in
  let%bind found, records =
    List.fold_result t.records ~init:(false, []) ~f:(fun (found, records) record ->
      if Int64.equal (Record.source_revision record) source_revision
      then (
        let%map changed = Record.update_attempt record attempt in
        true, changed :: records)
      else Ok (found, record :: records))
  in
  if not found
  then invalid "actual attempt has no retained preparation"
  else
    admit { t with records = List.rev records }
    |> Result.map_error ~f:(function
      | Capacity -> capacity_error
      | Invalid error -> error)
;;

let validate_state t ~windows ~publication_revision ~accounting ~has_archive =
  match windows with
  | None -> invalid "effective preparation requires its window ledger"
  | Some windows ->
    let scopes = List.map (Windows.windows windows) ~f:Windows.Window.scope in
    List.fold_result t.records ~init:() ~f:(fun () record ->
      let archive_ok =
        match Record.custody record with
        | Live | Lost -> true
        | Archived reference ->
          Int64.(Archive.revision reference < publication_revision)
          && has_archive reference
      in
      if
        Int64.(Record.source_revision record < publication_revision)
        && List.mem scopes (Record.scope record) ~equal:Scope.equal
        && archive_ok
        && Record.validate_attempt record ~accounting
      then Ok ()
      else invalid "effective preparation names an unrelated or uncommitted source")
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
