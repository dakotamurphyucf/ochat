(** Model-facing contract for authored persistence-enabled agent tools. The
    decoder selects behavior only; a supplied session ID never grants authority.
    Admission/continuation must check the caller and pinned authored instance. *)
type mode =
  | One_off
  | Persistent
[@@deriving equal, sexp]

type call =
  { input : string
  ; mode : mode
  ; session_id : Agent_protocol.Id.Session.t option
  }
[@@deriving equal, sexp]

val description
  :  Prompt.Chat_markdown.agent_tool
  -> Prompt.Chat_markdown.agent_persistence
  -> string

(** Optional fields use an ordinary JSON schema, for a non-strict provider tool.
    Only [input] is required; optional mode defaults to one-off. *)
val parameters : Prompt.Chat_markdown.agent_persistence -> Jsonaf.t

val decode
  :  Prompt.Chat_markdown.agent_persistence
  -> Jsonaf.t
  -> (call, Agent_protocol.Error.t) result
