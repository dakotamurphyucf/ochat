open! Core
module P = Agent_protocol
module D = Document_schema
module Digest = Chatml_source.Digest
module X = Persistence_codec

type kind = Recovery_window_location.Source.archive_kind =
  | Summary
  | Rollover
[@@deriving equal]

let invalid reason = Error (P.Error.invalid_request reason)
let max_request = Inference_runtime.Recovery_input.max_input_count

module Position = struct
  type t =
    { request_ordinal : int
    ; effective_ordinal : int
    ; canonical_ordinal : int option
    }
  [@@deriving equal]

  let create ~request_ordinal ~effective_ordinal ~canonical_ordinal =
    let source_position valid =
      valid >= 0 && valid < Recovery_window_record.max_members
    in
    if
      request_ordinal < 0
      || request_ordinal >= max_request
      || (not (source_position effective_ordinal))
      || not (Option.for_all canonical_ordinal ~f:source_position)
    then invalid "effective archive position is outside admitted coordinates"
    else Ok { request_ordinal; effective_ordinal; canonical_ordinal }
  ;;

  let request_ordinal t = t.request_ordinal
  let effective_ordinal t = t.effective_ordinal
  let canonical_ordinal t = t.canonical_ordinal

  let to_jsonaf t =
    `Array
      [ X.host_counter_to_json t.request_ordinal
      ; X.host_counter_to_json t.effective_ordinal
      ; X.option_json X.host_counter_to_json t.canonical_ordinal
      ]
  ;;

  let of_jsonaf = function
    | `Array [ request; effective; canonical ] ->
      let open Result.Let_syntax in
      let%bind request_ordinal = X.host_counter_of_json request in
      let%bind effective_ordinal = X.host_counter_of_json effective in
      let%bind canonical_ordinal = X.nullable X.host_counter_of_json canonical in
      create ~request_ordinal ~effective_ordinal ~canonical_ordinal
    | _ -> invalid "effective source position must contain three coordinates"
  ;;
end

type t =
  { kind : kind
  ; operation : P.Id.Operation.t
  ; revision : int64
  ; digest : Digest.t
  ; positions : Position.t list
  }
[@@deriving equal]

let kind t = t.kind
let operation t = t.operation
let revision t = t.revision
let digest t = t.digest
let positions t = t.positions

let find t ~request_ordinal =
  List.find t.positions ~f:(fun position ->
    Int.equal request_ordinal (Position.request_ordinal position))
;;

let create ~kind ~operation ~revision ~digest positions =
  let open Result.Let_syntax in
  if
    Int64.(revision < 0L)
    || List.is_empty positions
    || Stdlib.List.compare_length_with positions max_request > 0
  then invalid "effective archive proof is outside its position bound"
  else (
    let effective_positions = Int.Hash_set.create () in
    let canonical_positions = Int.Hash_set.create () in
    let%map _ =
      List.fold_result positions ~init:(-1) ~f:(fun previous position ->
        let request = Position.request_ordinal position in
        let effective = Position.effective_ordinal position in
        let canonical = Position.canonical_ordinal position in
        if
          request <= previous
          || Hash_set.mem effective_positions effective
          || Option.exists canonical ~f:(Hash_set.mem canonical_positions)
        then invalid "duplicate or unordered effective archive position"
        else (
          Hash_set.add effective_positions effective;
          Option.iter canonical ~f:(Hash_set.add canonical_positions);
          Ok request))
    in
    { kind; operation; revision; digest; positions })
;;

let to_jsonaf t =
  `Object
    [ ( "kind"
      , `String
          (match t.kind with
           | Summary -> "summary"
           | Rollover -> "rollover") )
    ; "operation", P.Id.Operation.to_json t.operation
    ; "revision", X.int64_json t.revision
    ; "sha256", `String (Digest.to_hex t.digest)
    ; "positions", X.list_json Position.to_jsonaf t.positions
    ]
;;

let kind_of_json = function
  | `String "summary" -> Ok Summary
  | `String "rollover" -> Ok Rollover
  | _ -> invalid "unknown effective archive kind"
;;

let digest_of_json json =
  let open Result.Let_syntax in
  let%bind value = P.Json_codec.string json in
  Digest.of_hex value
  |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid archive SHA-256")
;;

let positions_of_json = function
  | `Array values when Stdlib.List.compare_length_with values max_request <= 0 ->
    List.map values ~f:Position.of_jsonaf |> Result.all
  | _ -> invalid "too many effective archive position proofs"
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind kind = X.required fields "kind" kind_of_json in
  let%bind operation = X.required fields "operation" P.Id.Operation.of_json in
  let%bind revision = X.required fields "revision" X.nonnegative_int64 in
  let%bind digest = X.required fields "sha256" digest_of_json in
  let%bind positions = X.required fields "positions" positions_of_json in
  create ~kind ~operation ~revision ~digest positions
;;

let shape = X.fields_shape [ "kind"; "operation"; "revision"; "sha256"; "positions" ]
