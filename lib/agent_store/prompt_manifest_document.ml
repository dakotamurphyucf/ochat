open Core
module D = Document_schema
module F = Document_fields
module M = Prompt_manifest
module P = Agent_protocol

type t = M.t D.Extension_carrier.t

let max_bytes = 262144

let limits =
  F.limits ~max_bytes
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let identifier decode json =
  Result.bind (F.string json) ~f:(fun text -> F.protocol (decode text))
;;

let integer json =
  Result.bind (F.decimal json) ~f:(fun n ->
    if Int64.(n <= of_int Int.max_value)
    then Ok (Int64.to_int_exn n)
    else F.invalid "schema_version" "counter exceeds native range")
;;

let optional json name decode =
  match D.Json.field json ~name with
  | Absent -> Ok None
  | Null -> F.invalid name "optional field must be absent or a value"
  | Value value -> Result.map (decode value) ~f:Option.some
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind version = F.required json "version" integer in
  let%bind revision_id =
    F.required json "revision_id" (identifier P.Id.Prompt_revision.of_string)
  in
  let%bind prompt_definition_id =
    optional json "prompt_definition_id" (identifier P.Id.Prompt_definition.of_string)
  in
  let%bind canonical_source = optional json "canonical_source" F.string in
  let%bind root_relative_path = F.required json "root_relative_path" F.string in
  let%bind root_sha256 = F.required json "root_sha256" F.digest in
  let%bind sources = F.required json "sources" F.array in
  let%bind sources =
    List.map sources ~f:(fun source ->
      let%bind relative_path = F.required source "relative_path" F.string in
      let%map sha256 = F.required source "sha256" F.digest in
      M.Source.{ relative_path; sha256 })
    |> Result.all
  in
  let%bind parser_schema_version = F.required json "parser_schema_version" integer in
  let%bind runtime_schema_version = F.required json "runtime_schema_version" integer in
  let%bind shell_manifest_sha256 = optional json "shell_manifest_sha256" F.digest in
  let%bind created_at = F.required json "created_at" (identifier P.Timestamp.of_string) in
  let result =
    M.
      { version
      ; revision_id
      ; prompt_definition_id
      ; canonical_source
      ; root_relative_path
      ; root_sha256
      ; sources
      ; parser_schema_version
      ; runtime_schema_version
      ; shell_manifest_sha256
      ; created_at
      }
  in
  let%map () = M.validate result in
  result
;;

let to_json (t : M.t) =
  let optional name value ~f = Option.map value ~f:(fun value -> name, f value) in
  `Object
    ([ "version", F.decimal_json (Int64.of_int t.version)
     ; "revision_id", `String (P.Id.Prompt_revision.to_string t.revision_id)
     ; "root_relative_path", `String t.root_relative_path
     ; "root_sha256", `String t.root_sha256
     ; ( "sources"
       , `Array
           (List.map t.sources ~f:(fun source ->
              `Object
                [ "relative_path", `String source.M.Source.relative_path
                ; "sha256", `String source.sha256
                ])) )
     ; "parser_schema_version", F.decimal_json (Int64.of_int t.parser_schema_version)
     ; "runtime_schema_version", F.decimal_json (Int64.of_int t.runtime_schema_version)
     ; "created_at", `String (P.Timestamp.to_string t.created_at)
     ]
     @ List.filter_opt
         [ optional "prompt_definition_id" t.prompt_definition_id ~f:(fun id ->
             `String (P.Id.Prompt_definition.to_string id))
         ; optional "canonical_source" t.canonical_source ~f:(fun text -> `String text)
         ; optional "shell_manifest_sha256" t.shell_manifest_sha256 ~f:(fun text ->
             `String text)
         ])
;;

let shape =
  let source = F.shape [ "relative_path", D.Shape.value; "sha256", D.Shape.value ] in
  let sources =
    D.Shape.array source ~identity_field:(Some "relative_path")
    |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  F.shape
    [ "version", D.Shape.value
    ; "revision_id", D.Shape.value
    ; "prompt_definition_id", D.Shape.value
    ; "canonical_source", D.Shape.value
    ; "root_relative_path", D.Shape.value
    ; "root_sha256", D.Shape.value
    ; "sources", sources
    ; "parser_schema_version", D.Shape.value
    ; "runtime_schema_version", D.Shape.value
    ; "shell_manifest_sha256", D.Shape.value
    ; "created_at", D.Shape.value
    ]
;;

let codec () =
  D.Domain_codec.create
    ~limits
    ~kind:"store.prompt_manifest"
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~decode:of_json
    ~encode:(fun value ->
      let open Result.Let_syntax in
      let%map () = M.validate value in
      to_json value)
;;

let create value =
  let open Result.Let_syntax in
  let%map () = M.validate value in
  D.Extension_carrier.of_authored_value value
;;

let value = D.Extension_carrier.value

let of_document document =
  let open Result.Let_syntax in
  let%bind codec = codec () in
  D.Domain_codec.decode codec document
;;

let to_document t =
  let open Result.Let_syntax in
  let%bind codec = codec () in
  D.Domain_codec.encode codec t
;;

let stored_revision_id document =
  let open Result.Let_syntax in
  let%bind () =
    if String.equal (D.Document.kind document) "store.prompt_manifest"
    then Ok ()
    else F.invalid "kind" "not a prompt manifest"
  in
  F.required
    (D.Document.payload document)
    "revision_id"
    (identifier P.Id.Prompt_revision.of_string)
;;

type document = t

module Publication = struct
  type t =
    { document : document
    ; bytes : string
    ; sha256 : string
    }

  let document t = t.document
  let bytes t = t.bytes
  let sha256 t = t.sha256
  let digest bytes = Digestif.SHA256.(digest_string bytes |> to_hex)

  let create known =
    let open Result.Let_syntax in
    let%bind document = create known in
    let%map envelope = to_document document in
    let bytes = D.Document.to_string envelope in
    { document; bytes; sha256 = digest bytes }
  ;;

  let of_bytes bytes ~revision_id ~expected_sha256 =
    let open Result.Let_syntax in
    let%bind _ = F.digest (`String expected_sha256) |> F.store in
    if String.length bytes > max_bytes
    then Error (Store_error.Document (D.Error.Limit_exceeded "manifest bytes"))
    else if not (String.equal (digest bytes) expected_sha256)
    then Error (Store_error.Corrupt "original prompt manifest digest mismatch")
    else (
      let%bind original = D.Document.decode ~limits bytes |> F.store in
      let%bind original_id = stored_revision_id original |> F.store in
      if not (P.Id.Prompt_revision.equal original_id revision_id)
      then
        Error
          (Store_error.Corrupt "original prompt manifest identity differs from directory")
      else (
        let%map document = of_document original |> F.store in
        { document; bytes; sha256 = expected_sha256 }))
  ;;
end
