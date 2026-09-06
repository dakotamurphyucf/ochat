open! Core

(** Streams one authenticated session blob through bounded [blob.read]
    requests, validating identity, byte length, and SHA-256 before success. *)
val download
  :  connection:Connection.t
  -> session_id:Agent_protocol.Id.Session.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> blob:Agent_protocol.Blob.Metadata.t
  -> output:_ Eio.Flow.sink
  -> (unit, Agent_protocol.Error.t) result

(** [install_atomic ~path ~download] streams into an exclusive sibling file,
    syncs it and replaces [path] only after [download] validates successfully.
    Remove the sibling on failure or cancellation; preserve an existing target.
    The callback must validate the complete byte length and digest before success.
    Propagate [Eio.Cancel.Cancelled] unchanged after cancellation-protected cleanup;
    convert other IO/callback exceptions to [Error].
    This guarantees atomic visibility, not directory durability after power loss. *)
val install_atomic
  :  path:_ Eio.Path.t
  -> download:(Eio.Flow.sink_ty Eio.Resource.t -> (unit, Agent_protocol.Error.t) result)
  -> unit Or_error.t
