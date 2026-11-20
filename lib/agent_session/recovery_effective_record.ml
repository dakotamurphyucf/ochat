open! Core
module P = Agent_protocol
module X = Persistence_codec
module J = P.Json_codec
module D = Document_schema
module I = Inference_runtime.Recovery_input
module Projection = Recovery_effective_projection
module Scope = P.Recovery_window.Scope
module Citation = P.Recovery_window.Citation
module Version = P.Recovery_item_version
module Digest = Chatml_source.Digest
module Operation = P.Id.Operation

let invalid reason = Error (P.Error.invalid_request reason)

module Member = struct
  type t =
    { citation : Citation.t
    ; request_ordinal : int
    ; effective_ordinal : int
    ; canonical_ordinal : int option
    }

  let citation t = t.citation
  let request_ordinal t = t.request_ordinal
  let effective_ordinal t = t.effective_ordinal
  let canonical_ordinal t = t.canonical_ordinal

  let source t projection ~remaining_bytes =
    match Projection.at projection ~ordinal:t.effective_ordinal with
    | Some row
      when P.History.Id.equal
             (Citation.occurrence t.citation)
             (Projection.Row.occurrence row)
           && Option.equal
                Int.equal
                t.canonical_ordinal
                (Projection.Row.canonical_ordinal row) ->
      let version, remaining_bytes = Projection.Row.version row ~remaining_bytes in
      ( Option.some_if
          (Option.exists version ~f:(Version.equal (Citation.version t.citation)))
          row
      , remaining_bytes )
    | Some _ | None -> None, remaining_bytes
  ;;

  let to_jsonaf t =
    `Object
      [ "occurrence", P.History.Id.to_json (Citation.occurrence t.citation)
      ; "version", Version.to_json (Citation.version t.citation)
      ; "request_ordinal", X.host_counter_to_json t.request_ordinal
      ; "effective_ordinal", X.host_counter_to_json t.effective_ordinal
      ; "canonical_ordinal", X.option_json X.host_counter_to_json t.canonical_ordinal
      ]
  ;;

  let of_jsonaf scope json =
    let open Result.Let_syntax in
    let%bind fields = X.object_ json in
    let%bind occurrence = X.required fields "occurrence" P.History.Id.of_json in
    let%bind version = X.required fields "version" Version.of_json in
    let%bind citation = Citation.create scope ~occurrence ~version in
    let%bind request_ordinal =
      X.required fields "request_ordinal" X.host_counter_of_json
    in
    let%bind effective_ordinal =
      X.required fields "effective_ordinal" X.host_counter_of_json
    in
    let%map canonical_ordinal =
      X.required fields "canonical_ordinal" (X.nullable X.host_counter_of_json)
    in
    { citation; request_ordinal; effective_ordinal; canonical_ordinal }
  ;;

  let shape =
    X.fields_shape
      [ "occurrence"
      ; "version"
      ; "request_ordinal"
      ; "effective_ordinal"
      ; "canonical_ordinal"
      ]
  ;;
end

type t =
  { preparation_id : string
  ; configuration_digest : Digest.t
  ; scope : Scope.t
  ; operation : Operation.t
  ; source_revision : int64
  ; overlay_incarnation : int64
  ; input_count : int
  ; members : Member.t list
  }

let preparation_id t = t.preparation_id
let scope t = t.scope
let operation t = t.operation
let source_revision t = t.source_revision
let overlay_incarnation t = t.overlay_incarnation
let input_count t = t.input_count
let members t = t.members

let valid_preparation_id id =
  Result.is_ok (Inference.Observation.Observation_id.of_string id)
;;

let key t = Int64.to_string t.source_revision

let create t =
  let open Result.Let_syntax in
  if
    (not (valid_preparation_id t.preparation_id))
    || Int64.(
         t.source_revision < 0L
         || t.overlay_incarnation < 0L
         || t.overlay_incarnation > t.source_revision)
    || t.input_count < 0
    || t.input_count > I.max_input_count
    || Stdlib.List.compare_length_with t.members t.input_count > 0
  then invalid "invalid effective preparation coordinates"
  else (
    let occurrences = Hash_set.create (module P.History.Id) in
    let positions = Int.Hash_set.create () in
    let max_members = Recovery_window_record.max_members in
    let valid_position ordinal = ordinal >= 0 && ordinal < max_members in
    let%map _ =
      List.fold_result t.members ~init:(-1) ~f:(fun previous member ->
        let ordinal = Member.request_ordinal member in
        let effective = Member.effective_ordinal member in
        let citation = Member.citation member in
        let occurrence = Citation.occurrence citation in
        let origin_valid =
          match Citation.version citation with
          | Canonical _ -> false
          | Effective { origin; _ } ->
            (match origin with
             | Unchanged _ -> Option.is_some (Member.canonical_ordinal member)
             | Inserted { incarnation; _ } ->
               Int64.equal incarnation t.overlay_incarnation
               && Option.is_none (Member.canonical_ordinal member)
             | Replaced { incarnation; _ } ->
               Int64.equal incarnation t.overlay_incarnation
               && Option.is_some (Member.canonical_ordinal member)
             | Captured { session_revision } ->
               Int64.equal session_revision t.source_revision)
        in
        if
          ordinal <= previous
          || ordinal >= t.input_count
          || (not origin_valid)
          || (not (Scope.equal (Citation.scope citation) t.scope))
          || (not (valid_position effective))
          || (not (Option.for_all (Member.canonical_ordinal member) ~f:valid_position))
          || Hash_set.mem occurrences occurrence
          || Hash_set.mem positions effective
        then invalid "invalid or duplicate effective preparation member"
        else (
          Hash_set.add occurrences occurrence;
          Hash_set.add positions effective;
          Ok ordinal))
    in
    t)
;;

let capture input ~projection ~scope ~operation ~source_revision ~overlay_incarnation =
  let open Result.Let_syntax in
  let%bind members, _ =
    List.fold_result
      (I.rows input)
      ~init:([], I.max_projection_bytes)
      ~f:(fun (members, remaining_bytes) (input : I.row) ->
        match Projection.find projection input.occurrence with
        | None -> Ok (members, remaining_bytes)
        | Some row ->
          let version, remaining_bytes = Projection.Row.version row ~remaining_bytes in
          (match version with
           | Some (Effective { digest; _ } as version)
             when Digest.equal input.digest digest ->
             let%map citation =
               Citation.create scope ~occurrence:input.occurrence ~version
             in
             let member : Member.t =
               { citation
               ; request_ordinal = input.ordinal
               ; effective_ordinal = Projection.Row.effective_ordinal row
               ; canonical_ordinal = Projection.Row.canonical_ordinal row
               }
             in
             member :: members, remaining_bytes
           | Some _ | None -> Ok (members, remaining_bytes)))
  in
  create
    { preparation_id = I.preparation_id input
    ; configuration_digest = I.configuration_digest input
    ; scope
    ; operation
    ; source_revision
    ; overlay_incarnation
    ; input_count = I.input_count input
    ; members = List.rev members
    }
;;

let validate_source t projection =
  List.fold_result
    t.members
    ~init:I.max_projection_bytes
    ~f:(fun remaining_bytes member ->
      let row, remaining_bytes = Member.source member projection ~remaining_bytes in
      if Option.is_some row
      then Ok remaining_bytes
      else invalid "effective prepared member is not in the exact source projection")
  |> Result.map ~f:ignore
;;

let resolve t projection citation =
  let open Option.Let_syntax in
  let%bind member =
    List.find t.members ~f:(fun member ->
      Citation.equal citation (Member.citation member))
  in
  let%bind row =
    fst (Member.source member projection ~remaining_bytes:I.max_projection_bytes)
  in
  Projection.Row.entry row
;;

let to_jsonaf t =
  `Object
    [ "id", `String (key t)
    ; "preparation_id", `String t.preparation_id
    ; "configuration_sha256", `String (Digest.to_hex t.configuration_digest)
    ; "scope", Scope.to_json t.scope
    ; "operation", Operation.to_json t.operation
    ; "source_revision", X.int64_json t.source_revision
    ; "overlay_incarnation", X.int64_json t.overlay_incarnation
    ; "input_count", X.host_counter_to_json t.input_count
    ; "members", X.list_json Member.to_jsonaf t.members
    ]
;;

let digest_of_json json =
  let open Result.Let_syntax in
  let%bind hex = J.string json in
  Digest.of_hex hex
  |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid preparation SHA-256")
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind id = X.required fields "id" J.string in
  let%bind preparation_id = X.required fields "preparation_id" J.string in
  let%bind configuration_digest =
    X.required fields "configuration_sha256" digest_of_json
  in
  let%bind scope = X.required fields "scope" Scope.of_json in
  let%bind operation = X.required fields "operation" Operation.of_json in
  let%bind source_revision = X.required fields "source_revision" X.nonnegative_int64 in
  let%bind () =
    if String.equal id (Int64.to_string source_revision)
    then Ok ()
    else invalid "preparation key differs from its source revision"
  in
  let%bind overlay_incarnation =
    X.required fields "overlay_incarnation" X.nonnegative_int64
  in
  let%bind input_count = X.required fields "input_count" X.host_counter_of_json in
  let decode_members = function
    | `Array values when Stdlib.List.compare_length_with values I.max_input_count <= 0 ->
      List.map values ~f:(Member.of_jsonaf scope) |> Result.all
    | `Array _ -> invalid "effective preparation exceeds its input membership bound"
    | _ -> invalid "effective preparation members must be an array"
  in
  let%bind members = X.required fields "members" decode_members in
  create
    { preparation_id
    ; configuration_digest
    ; scope
    ; operation
    ; source_revision
    ; overlay_incarnation
    ; input_count
    ; members
    }
;;

let shape =
  X.shape_exn
    [ "id", D.Shape.value
    ; "preparation_id", D.Shape.value
    ; "configuration_sha256", D.Shape.value
    ; ( "scope"
      , X.shape_exn
          [ "session", Recovery_window_ledger.Source.owner_shape
          ; "generation", D.Shape.value
          ; "window", D.Shape.value
          ] )
    ; "operation", D.Shape.value
    ; "source_revision", D.Shape.value
    ; "overlay_incarnation", D.Shape.value
    ; "input_count", D.Shape.value
    ; "members", X.array_shape_exn ~identity_field:"occurrence" Member.shape
    ]
;;
