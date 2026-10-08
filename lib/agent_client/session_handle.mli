open! Core

(** A synchronized attached-session client used by terminal and automation
    front-ends. At most one subscribed handle owns notification reduction per
    connection. Unsubscribed handles may coexist, including with that reader;
    they do not consume events or update their projection from notifications.
    Each handle owns renewal of its own attachment lease. *)

type t

val initialize
  :  Connection.t
  -> implementation_name:string
  -> implementation_version:string
  -> (Agent_protocol.Initialize.Response.t, Agent_protocol.Error.t) result

val attach
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Connection.t
  -> session_id:Agent_protocol.Id.Session.t
  -> mode:Agent_protocol.Session.attachment_mode
  -> ?subscribe:bool
  -> ?after_sequence:int64
  -> ?reclaim_token:string
  -> ?previous_projection:Projection.t
  -> ?on_update:(Projection.t -> unit)
  -> ?on_error:(Agent_protocol.Error.t -> unit)
  -> unit
  -> (t, Agent_protocol.Error.t) result

val create
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Connection.t
  -> spec:Agent_protocol.Session.Spec.t
  -> mode:Agent_protocol.Session.attachment_mode
  -> ?subscribe:bool
  -> ?on_update:(Projection.t -> unit)
  -> ?on_error:(Agent_protocol.Error.t -> unit)
  -> unit
  -> (t, Agent_protocol.Error.t) result

val session_id : t -> Agent_protocol.Id.Session.t
val attachment : t -> Agent_protocol.Session.Attachment.t

(** [reclaim_token t] returns the one-time owner credential issued for the
    current owner attachment. Clients that need reconnect across a changed
    authenticated identity must protect this value as a secret. *)
val reclaim_token : t -> string option

val projection : t -> Projection.t
val last_error : t -> Agent_protocol.Error.t option

(** Closure means notification/renewal reduction has stopped; a stream error
    retains a stale snapshot and requires repair. This does not mean the server
    attachment was detached. [close] still attempts detach exactly once. *)
val await_closed : t -> unit

val is_closed : t -> bool

val start
  :  t
  -> queue_if_limited:bool
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val stop
  :  t
  -> mode:Agent_protocol.Session.stop_mode
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val send_message
  :  t
  -> Agent_protocol.Session.Message_content.t
  -> (Agent_protocol.Method_result.Send_message.t, Agent_protocol.Error.t) result

(** [delete_history t ~expected_revision id] requests a writer-authorized history
    mutation. The actor rejects stale revisions or active work. Projection
    replacement arrives through subscription events, not optimistic local edits. *)
val delete_history
  :  t
  -> expected_revision:int64
  -> Agent_protocol.History.Id.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val compact
  :  t
  -> expected_revision:int64 option
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val cancel_operation
  :  t
  -> Agent_protocol.Id.Operation.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val respond_permission
  :  t
  -> permission_id:Agent_protocol.Id.Permission.t
  -> permission_generation:int
  -> choice:Agent_protocol.Permission.choice
  -> reason:string option
  -> (Agent_protocol.Permission.t, Agent_protocol.Error.t) result

(** [revoke_grant t ~grant_id ~reason] revokes an active session grant through
    the current write attachment. *)
val revoke_grant
  :  t
  -> grant_id:Agent_protocol.Id.Grant.t
  -> reason:string
  -> (Agent_protocol.Grant.t, Agent_protocol.Error.t) result

(** [read_audit t ~limit] reads the newest authorized audit page scoped to the
    attached session. *)
val read_audit
  :  t
  -> limit:int
  -> (Agent_protocol.Audit.t Agent_protocol.Page.t, Agent_protocol.Error.t) result

val reset
  :  t
  -> expected_revision:int64
  -> keep_history:bool
  -> keep_tasks:bool
  -> keep_cache:bool
  -> keep_workspace:bool
  -> keep_grants:bool
  -> keep_labels:bool
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val rebuild
  :  t
  -> expected_revision:int64
  -> prompt_choice:Agent_protocol.Session.Rebuild_request.prompt_choice
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val export
  :  t
  -> format:Agent_protocol.Session.Export_request.format
  -> revision:int64 option
  -> (Agent_protocol.Method_result.Export.t, Agent_protocol.Error.t) result

(** [download_blob t ~blob ~output] streams and validates a session-owned blob
    through transport-neutral bounded protocol reads. *)
val download_blob
  :  t
  -> blob:Agent_protocol.Blob.Metadata.t
  -> output:_ Eio.Flow.sink
  -> (unit, Agent_protocol.Error.t) result

val delete
  :  t
  -> expected_revision:int64
  -> policy:Agent_protocol.Session.Delete_request.policy
  -> confirmation:string
  -> (Agent_protocol.Method_result.Delete.t, Agent_protocol.Error.t) result

val detach : t -> (unit, Agent_protocol.Error.t) result
val close : t -> unit

(** Revision-aware organization edit using the handle's retained command identity.
    Streaming execution does not advance this metadata revision. *)
val update_metadata
  :  t
  -> expected_metadata_revision:int64
  -> patch:Agent_protocol.Session_metadata.Patch.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result
