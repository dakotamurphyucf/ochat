open! Core
module D = Document_schema
module F = Document_fields
module Source = Captured_source_kind

let authored_semantics = "delegation.captured-authored-source.v1"

let limits_exn ~max_bytes ~max_depth ~max_fields ~max_nodes =
  D.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let limits = limits_exn ~max_bytes:262144 ~max_depth:64 ~max_fields:8192 ~max_nodes:16384

let reservation_limits =
  limits_exn ~max_bytes:65536 ~max_depth:32 ~max_fields:4096 ~max_nodes:8192
;;

let definition_limits =
  limits_exn ~max_bytes:262144 ~max_depth:32 ~max_fields:4096 ~max_nodes:8192
;;

type projection =
  { reservation : Jsonaf.t
  ; definition : Jsonaf.t
  ; initial_spec_sha256 : string
  ; source_kind : Source.t
  }

type t =
  { projection : projection
  ; document : D.Document.t
  }

let source_kind t = t.projection.source_kind
let reservation t = t.projection.reservation
let definition t = t.projection.definition
let initial_spec_sha256 t = t.projection.initial_spec_sha256
let to_json t = D.Document.json t.document
let equal a b = D.Json.equal (D.Document.json a.document) (D.Document.json b.document)

let slice json name limits =
  F.required json name (fun json ->
    let%map.Result () = D.Json.validate ~limits json in
    json)
;;

let decode json =
  let open Result.Let_syntax in
  let%bind reservation = slice json "reservation" reservation_limits in
  let%bind definition = slice json "definition" definition_limits in
  let%bind initial_spec_sha256 = F.required json "initial_spec_sha256" F.digest in
  let%map source_kind =
    match D.Json.field json ~name:"authored_source" with
    | Absent -> Ok Source.Artifact
    | Null -> F.invalid "authored_source" "authored source cannot be null"
    | Value json ->
      Source.Authored_tool.of_json json
      |> Result.map ~f:(fun source -> Source.Authored_tool source)
  in
  { reservation; definition; initial_spec_sha256; source_kind }
;;

let encode p =
  let fields =
    [ "reservation", p.reservation
    ; "definition", p.definition
    ; "initial_spec_sha256", `String p.initial_spec_sha256
    ]
  in
  match p.source_kind with
  | Artifact -> Ok (`Object fields)
  | Authored_tool selector ->
    let%map.Result selector = Source.Authored_tool.to_json selector in
    `Object (fields @ [ "authored_source", selector ])
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"delegation.captured_origin"
    ~version:1
    ~shape:
      (F.shape
         [ "reservation", D.Shape.value
         ; "definition", D.Shape.value
         ; "initial_spec_sha256", D.Shape.value
         ; "authored_source", Source.Authored_tool.shape
         ])
    ~supported_semantics:[ authored_semantics ]
    ~decode
    ~encode
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_document document =
  let open Result.Let_syntax in
  (* Complete guard precedes any recursive codec or child processing. *)
  let%bind () = D.Document.validate document ~limits in
  let%bind carrier = D.Domain_codec.decode codec document in
  let projection = D.Extension_carrier.value carrier in
  let marked =
    List.mem
      (D.Document.required_semantics document)
      authored_semantics
      ~equal:String.equal
  in
  let authored =
    match projection.source_kind with
    | Artifact -> false
    | Authored_tool _ -> true
  in
  if Bool.equal marked authored
  then Ok { projection; document }
  else
    F.invalid
      "required_semantics"
      "captured authored source semantics differ from selector"
;;

let to_document t = Ok t.document

let create_with_source source_kind ~reservation ~definition ~initial_spec_sha256 =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate ~limits:reservation_limits reservation in
  let%bind () = D.Json.validate ~limits:definition_limits definition in
  let%bind initial_spec_sha256 = F.digest (`String initial_spec_sha256) in
  let%bind document =
    D.Domain_codec.encode
      codec
      (D.Extension_carrier.of_authored_value
         { reservation; definition; initial_spec_sha256; source_kind })
  in
  let%bind document =
    match source_kind with
    | Artifact -> Ok document
    | Authored_tool _ ->
      (match D.Document.json document with
       | `Object fields ->
         D.Document.inspect
           ~limits
           (`Object
               (List.filter fields ~f:(fun (name, _) ->
                  not (String.equal name "required_semantics"))
                @ [ "required_semantics", `Array [ `String authored_semantics ] ]))
       | _ -> assert false)
  in
  of_document document
;;

let create ~reservation ~definition ~initial_spec_sha256 =
  create_with_source Artifact ~reservation ~definition ~initial_spec_sha256
;;

let create_authored ~authored_tool ~reservation ~definition ~initial_spec_sha256 =
  create_with_source
    (Authored_tool authored_tool)
    ~reservation
    ~definition
    ~initial_spec_sha256
;;

let sexp_of_t t = Sexp.Atom (Jsonaf.to_string (D.Document.json t.document))

let t_of_sexp sexp =
  let result =
    match sexp with
    | Sexp.Atom bytes -> D.Document.decode ~limits bytes |> Result.bind ~f:of_document
    | List _ -> F.invalid "captured_origin" "expected a bounded document atom"
  in
  match result with
  | Ok value -> value
  | Error error ->
    Sexplib0.Sexp_conv.of_sexp_error (Sexp.to_string_hum (D.Error.sexp_of_t error)) sexp
;;
