(** Host-owned management operations. A target ID is never authorization: the host
    must authenticate the borrowed caller, its recorded relationship and current
    policy before each operation and result disclosure. No client attachment or
    human approval authority is supplied by this service. *)
type wait_target =
  | Receipt of Agent_protocol.History.Id.t
  | Output of
      { cursor : Agent_protocol.Page.Cursor.t
      ; receipt_id : Agent_protocol.History.Id.t option
      }

(** [wait] accepts a bounded 0–30000ms monotonic wait outside the actor. Output
    targets retain the cursor's original query; receipt targets require actual
    terminal processing. The host must revalidate authority during quiet waits
    and before disclosure. Timeout/caller cancellation must not stop the child. *)

type t =
  { status :
      Native_tool_invocation.borrowed
      -> Agent_protocol.Id.Session.t
      -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result
  ; send :
      Native_tool_invocation.borrowed
      -> Agent_protocol.Id.Session.t
      -> key:Agent_protocol.Idempotency_key.t
      -> message:string
      -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result
  ; read :
      Native_tool_invocation.borrowed
      -> Agent_protocol.Id.Session.t
      -> receipt_id:Agent_protocol.History.Id.t option
      -> cursor:Agent_protocol.Page.Cursor.t option
      -> limit:int
      -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result
  ; wait :
      Native_tool_invocation.borrowed
      -> Agent_protocol.Id.Session.t
      -> target:wait_target
      -> timeout_ms:int
      -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result
  }

(** Bounded metadata only, excluding transcript, tool arguments, permission
    details, failure messages, workspace paths and private delegation records. *)
val status_json : Session_state.t -> Jsonaf.t
