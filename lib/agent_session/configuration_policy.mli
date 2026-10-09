open! Core

(** Host-only selection ports. Callbacks may yield while acquiring metadata
    locks and synchronizing disk state; execute outside the actor mailbox.
    No inference dispatch/login is performed. Credentials remain resolved by
    the existing dispatcher; resolution rechecks the captured profile binding. *)
type t =
  { select_profile :
      current:Inference.Request.Target.t
      -> profile:string
      -> (Inference.Request.Target.t, Agent_protocol.Error.t) Result.t
  ; approve :
      current:Inference.Request.Target.t
      -> proposed:Inference.Request.Target.t
      -> (unit, Agent_protocol.Error.t) Result.t
  ; resolve :
      Inference.Request.Target.t
      -> (Inference_runtime.Context.t, Agent_protocol.Error.t) Result.t
  }

(** Resolve/approve outside the mailbox, check retained opaque history, then
    explicit options via adapter preparation without dispatch. The returned
    Context supplies the existing pure current-binding check at final admission.
    This allocates no actor attempt/ledger row. *)
val validate
  :  t
  -> current:Inference.Request.Target.t
  -> proposed:Inference.Request.Target.t
  -> history:History_entry.t list
  -> (Inference_runtime.Context.t, Agent_protocol.Error.t) Result.t
