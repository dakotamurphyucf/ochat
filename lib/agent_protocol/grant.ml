open Core

type scope =
  | Exact_session
  | Prefix_session
  | Durable_exact
[@@deriving compare, equal, sexp]

type state =
  | Active
  | Revoked
  | Expired
[@@deriving compare, equal, sexp]

type t =
  { id : Id.Grant.t
  ; session_id : Id.Session.t
  ; principal_id : Id.Principal.t
  ; tool_name : string
  ; identity_digest : string
  ; scope : scope
  ; state : state
  ; created_at : Timestamp.t
  ; expires_at : Timestamp.t option
  ; revoked_at : Timestamp.t option
  ; revocation_reason : string option
  }
[@@deriving sexp]

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let scope_values =
  [ "exact_session", Exact_session
  ; "prefix_session", Prefix_session
  ; "durable_exact", Durable_exact
  ]
;;

let scope_to_string scope =
  List.Assoc.find_exn
    (List.map scope_values ~f:(fun (name, scope) -> scope, name))
    scope
    ~equal:equal_scope
;;

let scope_of_json = Json_codec.enum ~name:"grant scope" scope_values
let state_values = [ "active", Active; "revoked", Revoked; "expired", Expired ]

let state_to_string state =
  List.Assoc.find_exn
    (List.map state_values ~f:(fun (name, state) -> state, name))
    state
    ~equal:equal_state
;;

let state_of_json = Json_codec.enum ~name:"grant state" state_values

let validate t =
  if String.is_empty t.tool_name || String.is_empty t.identity_digest
  then Error (Protocol_error.invalid_request "grant identity fields must be nonempty")
  else if equal_state t.state Revoked && Option.is_none t.revoked_at
  then Error (Protocol_error.invalid_request "revoked grant lacks revocation time")
  else Ok t
;;

let to_json t =
  let fields =
    [ Some ("id", Id.Grant.to_json t.id)
    ; Some ("session_id", Id.Session.to_json t.session_id)
    ; Some ("principal_id", Id.Principal.to_json t.principal_id)
    ; Some ("tool_name", `String t.tool_name)
    ; Some ("identity_digest", `String t.identity_digest)
    ; Some ("scope", `String (scope_to_string t.scope))
    ; Some ("state", `String (state_to_string t.state))
    ; Some ("created_at", Timestamp.to_json t.created_at)
    ; optional_field "expires_at" t.expires_at Timestamp.to_json
    ; optional_field "revoked_at" t.revoked_at Timestamp.to_json
    ; optional_field "revocation_reason" t.revocation_reason (fun value -> `String value)
    ]
    |> List.filter_opt
  in
  `Object fields
;;

let decode_identity fields =
  let open Result.Let_syntax in
  let%bind id = Json_codec.required_as fields "id" Id.Grant.of_json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind principal_id =
    Json_codec.required_as fields "principal_id" Id.Principal.of_json
  in
  let%bind tool_name = Json_codec.required_as fields "tool_name" Json_codec.string in
  let%map identity_digest =
    Json_codec.required_as fields "identity_digest" Json_codec.string
  in
  id, session_id, principal_id, tool_name, identity_digest
;;

let decode_lifecycle fields =
  let open Result.Let_syntax in
  let%bind scope = Json_codec.required_as fields "scope" scope_of_json in
  let%bind state = Json_codec.required_as fields "state" state_of_json in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%bind expires_at = Json_codec.optional_as fields "expires_at" Timestamp.of_json in
  let%bind revoked_at = Json_codec.optional_as fields "revoked_at" Timestamp.of_json in
  let%map revocation_reason =
    Json_codec.optional_as fields "revocation_reason" Json_codec.string
  in
  scope, state, created_at, expires_at, revoked_at, revocation_reason
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id, session_id, principal_id, tool_name, identity_digest =
    decode_identity fields
  in
  let%bind scope, state, created_at, expires_at, revoked_at, revocation_reason =
    decode_lifecycle fields
  in
  validate
    { id
    ; session_id
    ; principal_id
    ; tool_name
    ; identity_digest
    ; scope
    ; state
    ; created_at
    ; expires_at
    ; revoked_at
    ; revocation_reason
    }
;;

module List_request = struct
  type t =
    { page : Page.Request.t
    ; session_id : Id.Session.t option
    ; principal_id : Id.Principal.t option
    ; state : state option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ optional_field "session_id" t.session_id Id.Session.to_json
      ; optional_field "principal_id" t.principal_id Id.Principal.to_json
      ; optional_field "state" t.state (fun state -> `String (state_to_string state))
      ]
      |> List.filter_opt
    in
    `Object (Page.Request.to_fields t.page @ fields)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind page = Page.Request.of_fields fields in
    let%bind session_id = Json_codec.optional_as fields "session_id" Id.Session.of_json in
    let%bind principal_id =
      Json_codec.optional_as fields "principal_id" Id.Principal.of_json
    in
    let%map state = Json_codec.optional_as fields "state" state_of_json in
    { page; session_id; principal_id; state }
  ;;
end

module Revoke_request = struct
  type t =
    { grant_id : Id.Grant.t
    ; session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; reason : string
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "grant_id", Id.Grant.to_json t.grant_id
      ; "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "reason", `String t.reason
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind grant_id = Json_codec.required_as fields "grant_id" Id.Grant.of_json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind attachment_id =
      Json_codec.required_as fields "attachment_id" Id.Attachment.of_json
    in
    let%bind reason = Json_codec.required_as fields "reason" Json_codec.string in
    let%bind idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    if String.is_empty reason
    then Error (Protocol_error.invalid_request "grant revocation reason must be nonempty")
    else Ok { grant_id; session_id; attachment_id; reason; idempotency_key }
  ;;
end

module Revoke_result = struct
  type nonrec t =
    { grant : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object (("grant", to_json t.grant) :: Mutation_result.to_fields t.mutation)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind grant = Json_codec.required_as fields "grant" of_json in
    let%map mutation = Mutation_result.of_fields fields in
    { grant; mutation }
  ;;
end
