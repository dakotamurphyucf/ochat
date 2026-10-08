open! Core
module D = Document_schema
module Presence = History_entry.Payload.Presence

module Error = struct
  type t =
    | Json of D.Error.t
    | Invalid_field of
        { field : string
        ; reason : string
        }
    | Duplicate_setting of string
    | Duplicate_tool_name of string
    | Duplicate_asset_reference of string
    | Duplicate_history_id of History_entry.Id.t
  [@@deriving equal, sexp_of]
end

let invalid field reason = Error (Error.Invalid_field { field; reason })
let json_error result = Result.map_error result ~f:(fun error -> Error.Json error)
let validate_json json ~limits = D.Json.validate ~limits json |> json_error

let nonempty field text =
  if String.is_empty (String.strip text) then invalid field "must be nonempty" else Ok ()
;;

let field json name = D.Json.field json ~name

let required_string json name =
  match field json name with
  | Value (`String value) -> Result.map (nonempty name value) ~f:(fun () -> value)
  | Absent -> invalid name "is required"
  | Null | Value _ -> invalid name "must be a string"
;;

let optional_string json name =
  match field json name with
  | Absent | Null -> Ok None
  | Value (`String value) -> Result.map (nonempty name value) ~f:(fun () -> Some value)
  | Value _ -> invalid name "must be a string or null"
;;

let optional_field name = function
  | None -> []
  | Some value -> [ name, `String value ]
;;

let presence_field name ~f = function
  | Presence.Absent -> []
  | Null -> [ name, `Null ]
  | Value value -> [ name, f value ]
;;

let validate_presence field = function
  | Presence.Value `Null -> invalid field "Value null is ambiguous; use Null"
  | Absent | Null | Value _ -> Ok ()
;;

let expect_object json =
  match json with
  | `Object _ -> Ok ()
  | `Array _ | `String _ | `Number _ | `True | `False | `Null ->
    invalid "value" "must be an object"
;;

let update_member_exn json name replacement =
  match json with
  | `Object fields ->
    let found = List.exists fields ~f:(fun (key, _) -> String.equal key name) in
    let fields =
      List.filter_map fields ~f:(fun (key, value) ->
        if String.equal key name
        then Option.map replacement ~f:(fun value -> key, value)
        else Some (key, value))
    in
    `Object
      (if found
       then fields
       else fields @ Option.to_list (Option.map replacement ~f:(fun value -> name, value)))
  | `Array _ | `String _ | `Number _ | `True | `False | `Null ->
    failwith "validated inference value is not an object"
;;

module Setting = struct
  type provenance =
    | Execution_override
    | Captured_prompt
    | Profile_default
  [@@deriving equal, sexp_of]

  type t =
    { name : string
    ; value : Jsonaf.t Presence.t
    ; provenance : provenance
    ; json : Jsonaf.t
    }

  let provenance_name = function
    | Execution_override -> "execution_override"
    | Captured_prompt -> "captured_prompt"
    | Profile_default -> "profile_default"
  ;;

  let decode_provenance json =
    match field json "provenance" with
    | Value (`String "execution_override") -> Ok Execution_override
    | Value (`String "captured_prompt") -> Ok Captured_prompt
    | Value (`String "profile_default") -> Ok Profile_default
    | Absent | Null | Value _ -> invalid "provenance" "is not a selected provenance"
  ;;

  let of_json json ~limits =
    let open Result.Let_syntax in
    let%bind () = validate_json json ~limits in
    let%bind () = expect_object json in
    let%bind name = required_string json "name" in
    let%map provenance = decode_provenance json in
    let value =
      match field json "value" with
      | Absent -> Presence.Absent
      | Null -> Null
      | Value value -> Value value
    in
    { name; value; provenance; json }
  ;;

  let create ~name ~value ~provenance ~limits =
    let open Result.Let_syntax in
    let%bind () = validate_presence "value" value in
    let json =
      `Object
        ([ "name", `String name; "provenance", `String (provenance_name provenance) ]
         @ presence_field "value" ~f:Fn.id value)
    in
    of_json json ~limits
  ;;

  let with_value t ~value ~provenance ~limits =
    let open Result.Let_syntax in
    let%bind () = validate_json t.json ~limits in
    let%bind () = validate_presence "value" value in
    let replacement =
      match value with
      | Presence.Absent -> None
      | Null -> Some `Null
      | Value value -> Some value
    in
    let json = update_member_exn t.json "value" replacement in
    let json =
      update_member_exn json "provenance" (Some (`String (provenance_name provenance)))
    in
    of_json json ~limits
  ;;

  let name t = t.name
  let value t = t.value
  let provenance t = t.provenance
  let to_json t = t.json
  let equal a b = D.Json.equal a.json b.json
end

module Auth_binding = struct
  type t =
    { method_ : string
    ; credential_reference : string
    ; json : Jsonaf.t
    }

  let of_json json ~limits =
    let open Result.Let_syntax in
    let%bind () = validate_json json ~limits in
    let%bind () = expect_object json in
    let%bind method_ = required_string json "method" in
    let%map credential_reference = required_string json "credential_reference" in
    { method_; credential_reference; json }
  ;;

  let create ~method_ ~credential_reference ~limits =
    of_json
      (`Object
          [ "method", `String method_
          ; "credential_reference", `String credential_reference
          ])
      ~limits
  ;;

  let method_ t = t.method_
  let credential_reference t = t.credential_reference
  let to_json t = t.json
  let equal a b = D.Json.equal a.json b.json
end

module Target = struct
  type t =
    { adapter : string
    ; profile : string
    ; profile_revision : string option
    ; account : string option
    ; auth_binding : Auth_binding.t Presence.t
    ; endpoint : string
    ; model : string
    ; settings : Setting.t list
    ; json : Jsonaf.t
    }

  let of_json json ~limits =
    let open Result.Let_syntax in
    let%bind () = validate_json json ~limits in
    let%bind () = expect_object json in
    let%bind adapter = required_string json "adapter" in
    let%bind profile = required_string json "profile" in
    let%bind profile_revision = optional_string json "profile_revision" in
    let%bind account = optional_string json "account" in
    let%bind auth_binding =
      match field json "auth_binding" with
      | Absent -> Ok Presence.Absent
      | Null -> Ok Presence.Null
      | Value json ->
        Result.map (Auth_binding.of_json json ~limits) ~f:(fun value ->
          Presence.Value value)
    in
    let%bind endpoint = required_string json "endpoint" in
    let%bind model = required_string json "model" in
    let%bind settings =
      match field json "settings" with
      | Value (`Array values) ->
        List.map values ~f:(fun value -> Setting.of_json value ~limits) |> Result.all
      | Absent | Null | Value _ -> invalid "settings" "must be an array"
    in
    let%map () =
      match
        List.find_a_dup (List.map settings ~f:Setting.name) ~compare:String.compare
      with
      | None -> Ok ()
      | Some name -> Error (Error.Duplicate_setting name)
    in
    { adapter
    ; profile
    ; profile_revision
    ; account
    ; auth_binding
    ; endpoint
    ; model
    ; settings
    ; json
    }
  ;;

  let create
        ~adapter
        ~profile
        ~profile_revision
        ~account
        ~endpoint
        ~model
        ~settings
        ~limits
    =
    `Object
      ([ "adapter", `String adapter
       ; "profile", `String profile
       ; "endpoint", `String endpoint
       ; "model", `String model
       ; "settings", `Array (List.map settings ~f:Setting.to_json)
       ]
       @ optional_field "profile_revision" profile_revision
       @ optional_field "account" account)
    |> fun json -> of_json json ~limits
  ;;

  let with_auth_binding t ~binding ~limits =
    let open Result.Let_syntax in
    let%bind _ = of_json t.json ~limits in
    let replacement =
      match binding with
      | Presence.Absent -> None
      | Null -> Some `Null
      | Value value -> Some (Auth_binding.to_json value)
    in
    update_member_exn t.json "auth_binding" replacement
    |> fun json -> of_json json ~limits
  ;;

  let auth_binding t = t.auth_binding

  let with_model t ~model ~limits =
    let open Result.Let_syntax in
    let%bind _ = of_json t.json ~limits in
    update_member_exn t.json "model" (Some (`String model))
    |> fun json -> of_json json ~limits
  ;;

  let with_setting t ~name ~value ~provenance ~limits =
    let open Result.Let_syntax in
    let%bind _ = of_json t.json ~limits in
    let%bind settings =
      match
        List.find t.settings ~f:(fun setting -> String.equal (Setting.name setting) name)
      with
      | Some _ ->
        List.map t.settings ~f:(fun setting ->
          if String.equal (Setting.name setting) name
          then Setting.with_value setting ~value ~provenance ~limits
          else Ok setting)
        |> Result.all
      | None ->
        let%map setting = Setting.create ~name ~value ~provenance ~limits in
        t.settings @ [ setting ]
    in
    update_member_exn
      t.json
      "settings"
      (Some (`Array (List.map settings ~f:Setting.to_json)))
    |> fun json -> of_json json ~limits
  ;;

  let adapter t = t.adapter
  let profile t = t.profile
  let profile_revision t = t.profile_revision
  let account t = t.account
  let endpoint t = t.endpoint
  let model t = t.model
  let settings t = t.settings
  let to_json t = t.json
  let equal a b = D.Json.equal a.json b.json
  let validate t ~limits = of_json t.json ~limits |> Result.map ~f:(fun _ -> ())
end

module Asset = struct
  type kind =
    | Image
    | Document of { filename : string option }
  [@@deriving equal, sexp_of]

  type t =
    { reference : string
    ; kind : kind
    ; media_type : string
    ; bytes : string
    }

  let create ~reference ~kind ~media_type ~bytes ~max_bytes =
    let open Result.Let_syntax in
    let%bind () =
      if max_bytes <= 0
      then invalid "max_bytes" "must be positive"
      else if String.length bytes > max_bytes
      then Error (Error.Json (D.Error.Limit_exceeded "asset bytes"))
      else Ok ()
    in
    let%bind () = nonempty "reference" reference in
    let%bind () = nonempty "media_type" media_type in
    let%bind () =
      match kind with
      | Image | Document { filename = None } -> Ok ()
      | Document { filename = Some filename } -> nonempty "filename" filename
    in
    let fields =
      [ "reference", `String reference; "media_type", `String media_type ]
      @
      match kind with
      | Image -> []
      | Document { filename } -> optional_field "filename" filename
    in
    let%map () = validate_json (`Object fields) ~limits:D.Limits.default in
    { reference; kind; media_type; bytes }
  ;;

  let reference t = t.reference
  let kind t = t.kind
  let media_type t = t.media_type
  let bytes t = t.bytes

  let equal a b =
    String.equal a.reference b.reference
    && equal_kind a.kind b.kind
    && String.equal a.media_type b.media_type
    && String.equal a.bytes b.bytes
  ;;

  let admission_json t =
    `Object
      ([ "reference", `String t.reference
       ; ( "kind"
         , `String
             (match t.kind with
              | Image -> "image"
              | Document _ -> "document") )
       ; "media_type", `String t.media_type
       ; "data", `String ""
       ]
       @
       match t.kind with
       | Image -> []
       | Document { filename } -> optional_field "filename" filename)
  ;;
end

module Tool_spec = struct
  module Custom_format = struct
    type t =
      | Text
      | Grammar of
          { syntax : [ `Lark | `Regex ]
          ; definition : string
          }
    [@@deriving equal, sexp_of]

    let to_json = function
      | Text -> `Object [ "type", `String "text" ]
      | Grammar { syntax; definition } ->
        `Object
          [ "type", `String "grammar"
          ; ( "syntax"
            , `String
                (match syntax with
                 | `Lark -> "lark"
                 | `Regex -> "regex") )
          ; "definition", `String definition
          ]
    ;;
  end

  type view =
    | Function of
        { parameters : Jsonaf.t Presence.t
        ; strict : bool Presence.t
        }
    | Custom of { format : Custom_format.t Presence.t }

  type t =
    { name : string
    ; description : string Presence.t
    ; output_schema : Jsonaf.t Presence.t
    ; view : view
    ; json : Jsonaf.t
    }

  let create ~name ~description ~output_schema ~view ~limits =
    let open Result.Let_syntax in
    let%bind () = validate_presence "output_schema" output_schema in
    let%bind () =
      match view with
      | Function { parameters; _ } -> validate_presence "parameters" parameters
      | Custom _ -> Ok ()
    in
    let%bind () = nonempty "name" name in
    let%bind () =
      match view with
      | Function _ | Custom { format = Absent | Null | Value Text } -> Ok ()
      | Custom { format = Value (Grammar { definition; _ }) } ->
        nonempty "definition" definition
    in
    let specific =
      match view with
      | Function { parameters; strict } ->
        [ "kind", `String "function" ]
        @ presence_field "parameters" ~f:Fn.id parameters
        @ presence_field "strict" ~f:(fun value -> if value then `True else `False) strict
      | Custom { format } ->
        [ "kind", `String "custom" ]
        @ presence_field "format" ~f:Custom_format.to_json format
    in
    let json =
      `Object
        ([ "name", `String name ]
         @ specific
         @ presence_field "description" ~f:(fun value -> `String value) description
         @ presence_field "output_schema" ~f:Fn.id output_schema)
    in
    let%map () = validate_json json ~limits in
    { name; description; output_schema; view; json }
  ;;

  let name t = t.name
  let description t = t.description
  let output_schema t = t.output_schema
  let view t = t.view

  let kind t =
    match t.view with
    | Function _ -> History_entry.Payload.Call_kind.Function
    | Custom _ -> History_entry.Payload.Call_kind.Custom
  ;;

  let equal a b = D.Json.equal a.json b.json
  let admission_json t = t.json
end

type t =
  { target : Target.t
  ; history : History_entry.t list
  ; tools : Tool_spec.t list
  ; assets : Asset.t list
  ; encoded_bytes : int
  }

let create ~target ~history ~tools ~assets ~limits =
  let open Result.Let_syntax in
  let%bind () =
    match
      List.find_a_dup
        (List.map history ~f:History_entry.id)
        ~compare:History_entry.Id.compare
    with
    | None -> Ok ()
    | Some id -> Error (Error.Duplicate_history_id id)
  in
  let%bind () =
    match List.find_a_dup (List.map tools ~f:Tool_spec.name) ~compare:String.compare with
    | None -> Ok ()
    | Some name -> Error (Error.Duplicate_tool_name name)
  in
  let%bind () =
    match
      List.find_a_dup (List.map assets ~f:Asset.reference) ~compare:String.compare
    with
    | None -> Ok ()
    | Some reference -> Error (Error.Duplicate_asset_reference reference)
  in
  let%bind () =
    List.map history ~f:(fun entry ->
      History_entry.Payload.validate (History_entry.payload entry)
      |> Result.map_error ~f:(fun reason ->
        Error.Invalid_field { field = "history"; reason }))
    |> Result.all_unit
  in
  let skeleton =
    `Object
      [ "target", Target.to_json target
      ; ( "history"
        , `Array
            (List.map history ~f:(fun entry ->
               `Object
                 [ "id", `String (History_entry.Id.to_string (History_entry.id entry))
                 ; "payload", History_entry.Payload.to_json (History_entry.payload entry)
                 ])) )
      ; "tools", `Array (List.map tools ~f:Tool_spec.admission_json)
      ; "assets", `Array (List.map assets ~f:Asset.admission_json)
      ]
  in
  let%bind base = D.Json.validate_and_measure ~limits skeleton |> json_error in
  (* Base64 is ASCII without JSON escaping. Empty placeholder strings already
     count quotes/nodes/keys/depth. Check groups before multiplication or encoding;
     no asset body is copied/encoded just to admit the immutable request. *)
  let%map remaining =
    List.fold_result
      assets
      ~init:(D.Limits.max_bytes limits - base)
      ~f:(fun remaining asset ->
        let bytes = String.length (Asset.bytes asset) in
        let groups = (bytes / 3) + if bytes % 3 = 0 then 0 else 1 in
        if groups > remaining / 4
        then Error (Error.Json (D.Error.Limit_exceeded "request asset base64 bytes"))
        else Ok (remaining - (groups * 4)))
  in
  { target
  ; history
  ; tools
  ; assets
  ; encoded_bytes = D.Limits.max_bytes limits - remaining
  }
;;

let target t = t.target
let history t = t.history
let tools t = t.tools
let assets t = t.assets
let encoded_bytes t = t.encoded_bytes
