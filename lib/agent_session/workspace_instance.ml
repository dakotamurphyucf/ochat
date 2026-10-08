open Core

type source_kind =
  | Physical
  | Temporary of Workspace_definition.temporary_location
  | Current
[@@deriving compare, equal, sexp]

type canonical_identity =
  { native_path : string
  ; device : int64
  ; inode : int64
  }
[@@deriving compare, equal, sexp]

type cleanup_completion =
  { completed_at : Agent_protocol.Timestamp.t
  ; reason : string
  }
[@@deriving sexp]

type t =
  { id : Agent_protocol.Id.Workspace_instance.t
  ; definition_id : Agent_protocol.Id.Workspace_definition.t option
  ; source_kind : source_kind
  ; configured_root : string option
  ; canonical_root : canonical_identity
  ; conflict_domain : string
  ; access : Workspace_definition.access
  ; cleanup : Workspace_definition.cleanup option
  ; server_created : bool
  ; created_at : Agent_protocol.Timestamp.t
  ; cleanup_completion : cleanup_completion option
  }
[@@deriving sexp]

let with_cleanup_completion t cleanup_completion =
  { t with cleanup_completion = Some cleanup_completion }
;;

module X = Persistence_codec
module J = Agent_protocol.Json_codec
module P = Agent_protocol
module W = Workspace_definition

let source_to_jsonaf = function
  | Physical -> `Object [ "kind", `String "physical" ]
  | Current -> `Object [ "kind", `String "current" ]
  | Temporary location ->
    `Object
      [ "kind", `String "temporary"
      ; ( "location"
        , `String
            (match location with
             | W.System_tmp -> "system_tmp"
             | Session_dir -> "session_dir") )
      ]
;;

let source_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind kind = X.required fields "kind" J.string in
  match kind with
  | "physical" -> Ok Physical
  | "current" -> Ok Current
  | "temporary" ->
    Result.map
      (X.required
         fields
         "location"
         (J.enum
            ~name:"temporary location"
            [ "system_tmp", W.System_tmp; "session_dir", W.Session_dir ]))
      ~f:(fun location -> Temporary location)
  | _ -> Error (P.Error.invalid_request "invalid workspace source")
;;

let source_shape =
  X.tagged_shape_exn
    ~discriminator:"kind"
    [ "physical", X.fields_shape [ "kind" ]
    ; "current", X.fields_shape [ "kind" ]
    ; "temporary", X.fields_shape [ "kind"; "location" ]
    ]
;;

let access_to_jsonaf = function
  | W.Read_only -> `String "read_only"
  | Shared_write -> `String "shared_write"
  | Exclusive -> `String "exclusive"
;;

let access_of_jsonaf =
  J.enum
    ~name:"workspace access"
    [ "read_only", W.Read_only; "shared_write", W.Shared_write; "exclusive", W.Exclusive ]
;;

let cleanup_to_jsonaf = function
  | W.On_session_stop -> `String "on_session_stop"
  | On_session_delete -> `String "on_session_delete"
  | Retain -> `String "retain"
;;

let cleanup_of_jsonaf =
  J.enum
    ~name:"workspace cleanup"
    [ "on_session_stop", W.On_session_stop
    ; "on_session_delete", W.On_session_delete
    ; "retain", W.Retain
    ]
;;

let canonical_to_jsonaf (t : canonical_identity) =
  `Object
    [ "native_path", X.text_json t.native_path
    ; "device", X.int64_json t.device
    ; "inode", X.int64_json t.inode
    ]
;;

let canonical_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind native_path = X.required fields "native_path" J.string in
  let%bind device = X.required fields "device" X.nonnegative_int64 in
  let%bind inode = X.required fields "inode" X.nonnegative_int64 in
  let t : canonical_identity = { native_path; device; inode } in
  Ok t
;;

let canonical_shape =
  X.shape_exn
    [ "native_path", Document_schema.Shape.value
    ; "device", Document_schema.Shape.value
    ; "inode", Document_schema.Shape.value
    ]
;;

let completion_to_jsonaf (t : cleanup_completion) =
  `Object
    [ "completed_at", P.Timestamp.to_json t.completed_at; "reason", X.text_json t.reason ]
;;

let completion_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind completed_at = X.required fields "completed_at" P.Timestamp.of_json in
  let%bind reason = X.required fields "reason" J.string in
  let t : cleanup_completion = { completed_at; reason } in
  Ok t
;;

let completion_shape =
  X.shape_exn
    [ "completed_at", Document_schema.Shape.value; "reason", Document_schema.Shape.value ]
;;

let storage_to_jsonaf (t : t) =
  `Object
    [ "id", P.Id.Workspace_instance.to_json t.id
    ; "definition_id", (X.option_json P.Id.Workspace_definition.to_json) t.definition_id
    ; "source_kind", source_to_jsonaf t.source_kind
    ; "configured_root", (X.option_json X.text_json) t.configured_root
    ; "canonical_root", canonical_to_jsonaf t.canonical_root
    ; "conflict_domain", X.text_json t.conflict_domain
    ; "access", access_to_jsonaf t.access
    ; "cleanup", (X.option_json cleanup_to_jsonaf) t.cleanup
    ; "server_created", X.bool_json t.server_created
    ; "created_at", P.Timestamp.to_json t.created_at
    ; "cleanup_completion", (X.option_json completion_to_jsonaf) t.cleanup_completion
    ]
;;

let storage_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind id = X.required fields "id" P.Id.Workspace_instance.of_json in
  let%bind definition_id =
    X.required fields "definition_id" (X.nullable P.Id.Workspace_definition.of_json)
  in
  let%bind source_kind = X.required fields "source_kind" source_of_jsonaf in
  let%bind configured_root = X.required fields "configured_root" (X.nullable J.string) in
  let%bind canonical_root = X.required fields "canonical_root" canonical_of_jsonaf in
  let%bind conflict_domain = X.required fields "conflict_domain" J.string in
  let%bind access = X.required fields "access" access_of_jsonaf in
  let%bind cleanup = X.required fields "cleanup" (X.nullable cleanup_of_jsonaf) in
  let%bind server_created = X.required fields "server_created" J.bool in
  let%bind created_at = X.required fields "created_at" P.Timestamp.of_json in
  let%bind cleanup_completion =
    X.required fields "cleanup_completion" (X.nullable completion_of_jsonaf)
  in
  let t : t =
    { id
    ; definition_id
    ; source_kind
    ; configured_root
    ; canonical_root
    ; conflict_domain
    ; access
    ; cleanup
    ; server_created
    ; created_at
    ; cleanup_completion
    }
  in
  Ok t
;;

let storage_shape =
  X.shape_exn
    [ "id", Document_schema.Shape.value
    ; "definition_id", X.nullable_shape Document_schema.Shape.value
    ; "source_kind", source_shape
    ; "configured_root", X.nullable_shape Document_schema.Shape.value
    ; "canonical_root", canonical_shape
    ; "conflict_domain", Document_schema.Shape.value
    ; "access", Document_schema.Shape.value
    ; "cleanup", X.nullable_shape Document_schema.Shape.value
    ; "server_created", Document_schema.Shape.value
    ; "created_at", Document_schema.Shape.value
    ; "cleanup_completion", X.nullable_shape completion_shape
    ]
;;

let to_jsonaf = storage_to_jsonaf

let of_jsonaf json =
  let%bind.Result t = storage_of_jsonaf json in
  if
    String.is_empty t.conflict_domain
    || not (Filename.is_absolute t.canonical_root.native_path)
  then Error (P.Error.invalid_request "invalid canonical workspace identity")
  else Ok t
;;

let shape = storage_shape
