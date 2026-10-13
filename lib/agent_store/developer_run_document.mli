(** The single versioned developer-run authority document. The invocation and
    job arrays are present in version one; this foundation admits only empty
    arrays until their typed transition codec is implemented. *)
open! Core

type epoch
type t

val create
  :  owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> t

val host_id : t -> Agent_protocol.Id.Server.t
val run_id : t -> Agent_protocol.Id.Developer_run.t
val principal_id : t -> Agent_protocol.Id.Principal.t
val revision : t -> int64
val epochs : t -> epoch list
val epoch_number : epoch -> int
val epoch_capture : epoch -> Chatml_source.Digest.t
val epoch_phase : epoch -> [ `Active | `Interrupted | `Closed ]
val epoch_revision : epoch -> int64

val append_epoch
  :  t
  -> owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> max_epochs:int
  -> (t, Store_error.t) result

val set_phase
  :  t
  -> epoch:int
  -> phase:[ `Active | `Interrupted | `Closed ]
  -> (t, Store_error.t) result

val to_bytes
  :  t
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> (string, Store_error.t) result

val of_bytes
  :  string
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> (t, Store_error.t) result
