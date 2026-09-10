open Core

module T = struct
  type t =
    | List_prompts
    | List_workspaces
    | Create_sessions
    | View_session_transcript
    | Send_messages
    | Own_sessions
    | Answer_approvals
    | View_security_state
    | Manage_grants
    | Read_audit
    | Stop_sessions
    | Delete_sessions
    | Administer_configuration
    | Diagnostics
    | Submit_ingress
  [@@deriving compare, equal, sexp]
end

include T
include Comparable.Make (T)

let to_string = function
  | List_prompts -> "prompt.list"
  | List_workspaces -> "workspace.list"
  | Create_sessions -> "session.create"
  | View_session_transcript -> "session.transcript.read"
  | Send_messages -> "session.message.send"
  | Own_sessions -> "session.own"
  | Answer_approvals -> "permission.respond"
  | View_security_state -> "security.read"
  | Manage_grants -> "grant.manage"
  | Read_audit -> "audit.read"
  | Stop_sessions -> "session.stop"
  | Delete_sessions -> "session.delete"
  | Administer_configuration -> "configuration.admin"
  | Diagnostics -> "diagnostics.read"
  | Submit_ingress -> "ingress.submit"
;;

let of_string = function
  | "prompt.list" -> Ok List_prompts
  | "workspace.list" -> Ok List_workspaces
  | "session.create" -> Ok Create_sessions
  | "session.transcript.read" -> Ok View_session_transcript
  | "session.message.send" -> Ok Send_messages
  | "session.own" -> Ok Own_sessions
  | "permission.respond" -> Ok Answer_approvals
  | "security.read" -> Ok View_security_state
  | "grant.manage" -> Ok Manage_grants
  | "audit.read" -> Ok Read_audit
  | "session.stop" -> Ok Stop_sessions
  | "session.delete" -> Ok Delete_sessions
  | "configuration.admin" -> Ok Administer_configuration
  | "diagnostics.read" -> Ok Diagnostics
  | "ingress.submit" -> Ok Submit_ingress
  | encoded -> Error (Protocol_error.invalid_request ("unknown scope: " ^ encoded))
;;

let to_json t = `String (to_string t)

let of_json = function
  | `String encoded -> of_string encoded
  | _ -> Error (Protocol_error.invalid_request "scope must be a JSON string")
;;

let set_to_json scopes = `Array (Core.Set.to_list scopes |> List.map ~f:to_json)

let set_of_json json =
  let open Result.Let_syntax in
  let%bind scopes = Json_codec.list of_json json in
  let set = Set.of_list scopes in
  if Int.equal (Core.Set.length set) (List.length scopes)
  then Ok set
  else Error (Protocol_error.invalid_request "scope array contains duplicates")
;;
