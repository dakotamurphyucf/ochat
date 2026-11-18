open! Core
module P = Agent_protocol
module I = Instruction_content
module R = Instruction_source_request
module J = P.Json_codec
module Effect = Instruction_native_admission.Effect

type t =
  | Refresh of R.Selection.t option
  | Read of R.t
  | Activate of
      { request : R.t
      ; capture_id : string
      ; expected_revision : int64
      ; key : P.Idempotency_key.t
      }
  | Deactivate of
      { all : bool
      ; members : R.Member.t list option
      ; expected_revision : int64
      ; key : P.Idempotency_key.t
      }
  | Status

let limits =
  Document_schema.Limits.create
    ~max_bytes:131072
    ~max_depth:12
    ~max_fields:1024
    ~max_nodes:2048
  |> function
  | Ok limits -> limits
  | Error e -> raise_s [%sexp (e : Document_schema.Error.t)]
;;

let domain result =
  Result.map_error result ~f:(fun _ ->
    P.Error.invalid_request "invalid bounded instruction request")
;;

let decode_text_exn text =
  Document_schema.Json.decode ~limits text
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (Document_schema.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let fields json ~names =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  if
    List.exists (J.to_alist fields) ~f:(fun (name, _) ->
      not (List.mem names name ~equal:String.equal))
  then Error (P.Error.invalid_request "unknown instruction request field")
  else Ok fields
;;

let root json =
  let%bind.Result text = J.string json in
  I.Root_id.of_string text |> domain
;;

let member json =
  let open Result.Let_syntax in
  let%bind f = fields json ~names:[ "root"; "member"; "sha256" ] in
  let%bind root = J.required_as f "root" root in
  let%bind path = J.required_as f "member" J.string in
  let%bind member = Source.Portable_path.create path |> domain in
  let%bind digest = J.required_as f "sha256" J.string in
  let%bind digest = Chatml_source.Digest.of_hex digest |> domain in
  Ok (R.Member.create ~root ~member ~digest)
;;

let members json =
  match json with
  | `Array values when List.length values <= 64 -> List.map values ~f:member |> Result.all
  | `Null | `True | `False | `Number _ | `String _ | `Object _ | `Array _ ->
    Error (P.Error.invalid_request "instruction members must be bounded array")
;;

let selection json =
  let open Result.Let_syntax in
  let%bind f = fields json ~names:[ "kind"; "roots"; "scope" ] in
  let%bind kind =
    J.required_as
      f
      "kind"
      (J.enum ~name:"instruction kind" [ "project", `Project; "skills", `Skills ])
  in
  let%bind roots = J.required_as f "roots" (J.list root) in
  let%bind scope = J.required f "scope" in
  let%bind selection =
    match kind, roots, scope with
    | `Project, [ root ], `String path ->
      let%map scope =
        if String.is_empty path
        then Ok I.Scope.root
        else Source.Portable_path.create path |> domain |> Result.map ~f:I.Scope.of_path
      in
      R.Selection.Project { root; scope }
    | `Skills, roots, `Null -> Ok (R.Selection.Skills roots)
    | `Project, _, _ | `Skills, _, _ ->
      Error (P.Error.invalid_request "instruction selection kind/root/scope mismatch")
  in
  let%map () = R.Selection.validate selection in
  selection
;;

let revision json =
  let%bind.Result text = J.string json in
  match Int64.of_string_opt text with
  | Some n when Int64.(n >= 0L) && String.equal (Int64.to_string n) text -> Ok n
  | Some _ | None ->
    Error
      (P.Error.invalid_request
         "instruction revision must be canonical nonnegative decimal")
;;

let decode ~effect_kind json =
  let open Result.Let_syntax in
  let%bind () = Document_schema.Json.validate ~limits json |> domain in
  let names =
    match effect_kind with
    | Effect.Refresh -> [ "selection" ]
    | Status -> []
    | Read -> [ "selection"; "members" ]
    | Activate -> [ "selection"; "members"; "capture_id"; "expected_revision"; "key" ]
    | Deactivate -> [ "kind"; "members"; "expected_revision"; "key" ]
  in
  let%bind f = fields json ~names in
  let request () =
    let%bind selection = J.required_as f "selection" selection in
    let%bind members = J.required_as f "members" members in
    R.create ~selection ~members
  in
  let mutation () =
    let%bind expected_revision = J.required_as f "expected_revision" revision in
    let%map key = J.required_as f "key" P.Idempotency_key.of_json in
    expected_revision, key
  in
  match effect_kind with
  | Status -> Ok Status
  | Refresh ->
    let%bind raw = J.required f "selection" in
    let%map selection =
      match raw with
      | `Null -> Ok None
      | json -> selection json |> Result.map ~f:Option.some
    in
    Refresh selection
  | Read ->
    let%map request = request () in
    Read request
  | Activate ->
    let%bind request = request () in
    let%bind capture_id = J.required_as f "capture_id" J.string in
    let%bind _ = Chatml_source.Digest.of_hex capture_id |> domain in
    let%map expected_revision, key = mutation () in
    Activate { request; capture_id; expected_revision; key }
  | Deactivate ->
    let%bind kind =
      J.required_as
        f
        "kind"
        (J.enum
           ~name:"deactivate kind"
           [ "all", `All; "project", `Project; "skills", `Skills ])
    in
    let%bind raw = J.required f "members" in
    let%bind all, members =
      match kind, raw with
      | `All, `Null -> Ok (true, None)
      | `Project, `Null -> Ok (false, None)
      | `Skills, raw ->
        let%map members = members raw in
        false, Some members
      | (`All | `Project), _ ->
        Error (P.Error.invalid_request "deactivate members must be null")
    in
    let%map expected_revision, key = mutation () in
    Deactivate { all; members; expected_revision; key }
;;

let text = `Object [ "type", `String "string" ]

let array item =
  `Object [ "type", `String "array"; "items", item; "maxItems", `Number "64" ]
;;

let object_ properties =
  `Object
    [ "type", `String "object"
    ; "properties", `Object properties
    ; "required", `Array (List.map properties ~f:(fun (name, _) -> `String name))
    ; "additionalProperties", `False
    ]
;;

let nullable schema =
  `Object [ "anyOf", `Array [ schema; `Object [ "type", `String "null" ] ] ]
;;

let enum values =
  `Object
    [ "type", `String "string"
    ; "enum", `Array (List.map values ~f:(fun value -> `String value))
    ]
;;

let selection_schema =
  object_
    [ "kind", enum [ "project"; "skills" ]; "roots", array text; "scope", nullable text ]
;;

let members_schema = array (object_ [ "root", text; "member", text; "sha256", text ])

let parameters = function
  | Effect.Status -> object_ []
  | Refresh -> object_ [ "selection", nullable selection_schema ]
  | Read -> object_ [ "selection", selection_schema; "members", members_schema ]
  | Activate ->
    object_
      [ "selection", selection_schema
      ; "members", members_schema
      ; "capture_id", text
      ; "expected_revision", text
      ; "key", text
      ]
  | Deactivate ->
    object_
      [ "kind", enum [ "all"; "project"; "skills" ]
      ; "members", nullable members_schema
      ; "expected_revision", text
      ; "key", text
      ]
;;
