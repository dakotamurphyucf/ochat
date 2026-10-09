(** Process-local host authorization retained by one session actor. Durable run
    receipts never rebuild this registry. Recovery must explicitly reacquire the
    current host authorization or durably interrupt the old run before effects. *)
type t

type binding

val empty : t
val find : t -> Agent_protocol.Id.Run.t -> binding option

val add
  :  t
  -> run:Agent_protocol.Run.t
  -> scope:Run_admission.Scope.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (t, Agent_protocol.Error.t) result

val scope : binding -> Run_admission.Scope.t

(** Admission provenance only; transport detach/lease expiry does not revoke the
    host-owned workflow. Current scope authorization decides continuation rights. *)
val attachment_id : binding -> Agent_protocol.Id.Attachment.t

(** Release closures after every successful durable actor installation. Only
    current-generation live runs keep their binding; terminal, retired source and
    absent run rows release it. O(live bindings * log retained runs), with no copy
    of the durable run table. This never grants or reconstructs authority. *)
val retain_current : t -> index:Run_state.t option -> generation:int -> t
