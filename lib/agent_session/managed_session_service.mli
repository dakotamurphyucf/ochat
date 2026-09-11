(** Host-owned management operations. A target ID is never authorization: the host
    must authenticate the borrowed caller, its recorded relationship and current
    policy before each operation and result disclosure. No client attachment or
    human approval authority is supplied by this service. *)
type t =
  { status :
      Native_tool_invocation.borrowed
      -> Agent_protocol.Id.Session.t
      -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result
  }

(** Bounded metadata only, excluding transcript, tool arguments, permission
    details, failure messages, workspace paths and private delegation records. *)
val status_json : Session_state.t -> Jsonaf.t
