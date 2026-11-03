open! Core
open Instruction_model
module D = Document_schema
module S = Agent_store.Prompt_program_bundle
module E = D.Encoding

type t =
  { loaded : Loaded.t list
  ; storage : S.t
  }

let loaded t = t.loaded
let to_storage t = t.storage
let kind = "ochat.instruction-content"

let document_limits =
  D.Limits.create ~max_bytes:262_144 ~max_depth:16 ~max_fields:8192 ~max_nodes:16384
  |> function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let document result = Result.map_error result ~f:(fun value -> Error.Document value)
let storage result = Result.map_error result ~f:(fun value -> Error.Storage value)
let invalid message = Error (Error.Invalid_capture message)

let key (id : Id.t) =
  Root_id.to_string id.root ^ "/" ^ Source.Portable_path.to_string id.member
;;

let kind_name = function
  | Kind.Project_guidance -> "project"
  | Skill -> "skill"
;;

let metadata_json metadata =
  let optional = function
    | None -> `Null
    | Some value -> `String value
  in
  `Object
    [ "name", optional metadata.Metadata.name
    ; "description", optional metadata.description
    ; ( "annotations"
      , `Object
          (List.map metadata.annotations ~f:(fun (key, value) -> key, `String value)) )
    ]
;;

let encode_row context loaded =
  let observed = loaded.Loaded.observed in
  E.object_
    context
    [ ("key", fun e -> E.string e (key observed.id))
    ; ("root", fun e -> E.string e (Root_id.to_string observed.id.root))
    ; ("member", fun e -> E.string e (Source.Portable_path.to_string observed.id.member))
    ; ("kind", fun e -> E.string e (kind_name observed.kind))
    ; ("digest", fun e -> E.string e (Capture.Digest.to_hex observed.digest))
    ; ("byte_length", fun e -> E.number e observed.byte_length)
    ; ("body_start_byte", fun e -> E.number e loaded.body_start_byte)
    ; ("metadata", fun e -> E.raw e (metadata_json observed.metadata))
    ]
;;

let preflight values ~limits ~poll =
  List.fold_result
    values
    ~init:(0, 0, String.Set.empty)
    ~f:(fun (count, bytes, seen) loaded ->
      poll ();
      let length = String.length loaded.Loaded.contents in
      let id = key loaded.observed.id in
      if
        count >= Limits.files limits
        || length > Limits.file_bytes limits
        || length > Limits.total_bytes limits - bytes
      then Error Error.Limit_exceeded
      else if Set.mem seen id
      then invalid "duplicate instruction identity"
      else Ok (count + 1, bytes + length, Set.add seen id))
  |> Result.map ~f:(fun _ -> ())
;;

let capture values ~limits ~checkpoint =
  bounded limits ~checkpoint (fun poll ->
    let open Result.Let_syntax in
    let%bind () = preflight values ~limits ~poll in
    let%bind payload =
      document
        (E.render
           ~limits:document_limits
           ~byte_limit_context:"instruction metadata"
           ~checkpoint:poll
           ~f:(fun e ->
             E.object_ e [ ("instructions", fun e -> E.array e values ~f:encode_row) ]))
    in
    let%bind metadata =
      document
        (D.Document.create_with_checkpoint
           ~limits:document_limits
           ~checkpoint:poll
           ~kind
           ~version:1
           ~payload)
    in
    let%bind payloads =
      List.fold_result values ~init:String.Map.empty ~f:(fun payloads loaded ->
        poll ();
        let digest = Capture.Digest.to_hex loaded.Loaded.observed.digest in
        if Map.mem payloads digest
        then Ok payloads
        else (
          let%map payload = storage (S.Payload.create loaded.contents) in
          Map.set payloads ~key:digest ~data:payload))
    in
    let%map storage = storage (S.create ~metadata ~payloads:(Map.data payloads)) in
    { loaded = values; storage })
;;

let required json name =
  match D.Json.field json ~name with
  | Value value -> Ok value
  | Absent | Null -> invalid ("missing instruction field: " ^ name)
;;

let string json name =
  Result.bind (required json name) ~f:(function
    | `String text -> Ok text
    | _ -> invalid ("invalid instruction string: " ^ name))
;;

let decode_row row payloads ~limits ~remaining ~poll =
  let open Result.Let_syntax in
  let%bind root = string row "root" in
  let%bind root =
    Root_id.of_string root |> Result.map_error ~f:(fun _ -> Error.Invalid_identity)
  in
  let%bind member = string row "member" in
  let%bind id =
    Capture.Id.create ~package_namespace:(Root_id.to_string root) ~relative_path:member
    |> Result.map_error ~f:(fun _ -> Error.Invalid_identity)
  in
  let%bind kind_value = string row "kind" in
  let%bind kind =
    match kind_value with
    | "project" -> Ok Kind.Project_guidance
    | "skill" -> Ok Kind.Skill
    | _ -> invalid "invalid instruction kind"
  in
  let%bind digest = string row "digest" in
  let%bind expected =
    Capture.Digest.of_hex digest |> Result.map_error ~f:(fun _ -> Error.Invalid_identity)
  in
  let%bind payload =
    Map.find payloads digest
    |> Result.of_option ~error:(Error.Invalid_capture "missing instruction payload")
  in
  let%bind () =
    if S.Payload.byte_length payload > remaining
    then Error Error.Limit_exceeded
    else Ok ()
  in
  let%bind source =
    Capture.create
      id
      ~contents:(S.Payload.contents payload)
      ~expected_digest:(Some expected)
      ~max_bytes:(Some (Limits.file_bytes limits))
      ~checkpoint:poll
    |> Result.map_error ~f:(fun _ ->
      Error.Invalid_capture "instruction payload revision mismatch")
  in
  let%bind loaded = Instruction_loaded.construct source ~root ~kind ~limits ~poll in
  (* Compare all owned assertions against independently derived values. Unknown
     compatible row fields stay in the original document and are never erased. *)
  let%bind expected_row =
    document
      (E.render
         ~limits:document_limits
         ~byte_limit_context:"instruction row"
         ~checkpoint:poll
         ~f:(fun e -> encode_row e loaded))
  in
  match expected_row with
  | `Object fields ->
    let%map () =
      List.fold_result fields ~init:() ~f:(fun () (name, expected) ->
        poll ();
        let%bind actual = required row name in
        if D.Json.equal actual expected
        then Ok ()
        else invalid ("instruction assertion mismatch: " ^ name))
    in
    loaded
  | _ -> invalid "invalid encoded instruction row"
;;

let of_storage stored ~limits ~checkpoint =
  bounded limits ~checkpoint (fun poll ->
    let open Result.Let_syntax in
    let metadata = S.Manifest.metadata (S.manifest stored) in
    let%bind () = document (D.Document.validate metadata ~limits:document_limits) in
    let%bind _ =
      document
        (D.Json.validate_and_measure_with_checkpoint
           ~limits:document_limits
           ~checkpoint:poll
           (D.Document.json metadata))
    in
    let%bind () =
      if not (String.equal (D.Document.kind metadata) kind)
      then
        document
          (Error
             (D.Error.Wrong_kind { expected = kind; actual = D.Document.kind metadata }))
      else if D.Document.version metadata <> 1
      then
        document
          (Error
             (D.Error.Unsupported_version
                { kind; version = D.Document.version metadata; target = 1 }))
      else (
        match D.Document.required_semantics metadata with
        | [] -> Ok ()
        | semantics :: _ ->
          document (Error (D.Error.Required_semantics_unknown semantics)))
    in
    let payloads =
      List.fold (S.payloads stored) ~init:String.Map.empty ~f:(fun map value ->
        Map.set map ~key:(S.Payload.sha256 value) ~data:value)
    in
    let%bind rows = required (D.Document.payload metadata) "instructions" in
    let%bind rows =
      match rows with
      | `Array rows -> Ok rows
      | _ -> invalid "invalid instruction array"
    in
    let rec decode rows count bytes output consumed =
      poll ();
      match rows with
      | [] ->
        if Set.length consumed <> Map.length payloads
        then invalid "surplus instruction payload"
        else Ok (List.rev output)
      | row :: rest ->
        if count >= Limits.files limits
        then Error Error.Limit_exceeded
        else (
          (* Bound actual bytes before hashing, including legal empty files
             when the aggregate allowance has been exactly consumed. *)
          let remaining = Limits.total_bytes limits - bytes in
          let%bind value = decode_row row payloads ~limits ~remaining ~poll in
          let digest = Capture.Digest.to_hex value.Loaded.observed.digest in
          decode
            rest
            (count + 1)
            (bytes + String.length value.contents)
            (value :: output)
            (Set.add consumed digest))
    in
    let%bind values = decode rows 0 0 [] String.Set.empty in
    let%map () = preflight values ~limits ~poll in
    { loaded = values; storage = stored })
;;
