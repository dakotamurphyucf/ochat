(** Authoritative durable-session metadata; schema and data-schema versions are independent. *)
type t =
  { schema_version : int
  ; session : Agent_protocol.Session.t
  ; prompt_artifact : string
  ; workspace_identity : string
  ; data_schema_version : int
  }
[@@deriving sexp]
