(** Authorization scopes carried by authenticated principals. *)

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
[@@deriving compare, equal, sexp]

include Core.Comparable.S with type t := t

(** [to_string t] returns the stable lowercase dotted wire name. *)
val to_string : t -> string

(** [of_string encoded] parses a stable authorization scope. *)
val of_string : string -> (t, Error.t) result

(** [to_json t] encodes one scope as a JSON string. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes one scope. *)
val of_json : Jsonaf.t -> (t, Error.t) result

(** [set_to_json scopes] encodes scopes in stable sorted order. *)
val set_to_json : Set.t -> Jsonaf.t

(** [set_of_json json] decodes a scope array and rejects duplicates. *)
val set_of_json : Jsonaf.t -> (Set.t, Error.t) result
