open! Core

(** Private actual canonical backend association. Only the first-party persistence
    adapter constructs this from the SAME commit closure it returns to its Actor.
    This carries no disposal, execution, journal retry or acknowledgement proof. *)
type commit =
  ?observation:Session_commit_observation.t
  -> command_audit:Document_schema.Document.t option
  -> previous:Session_state.t
  -> Session_transition.t
  -> (unit, Agent_protocol.Error.t) result

type t

val create : writer:Agent_store.Commit_writer.t -> commit:commit -> t

(** Reject a foreign domain, physical commit closure or writer session before
    Actor startup. Session identity is only an additional negative check. *)
val validate_exn : t -> session_id:Agent_protocol.Id.Session.t -> commit:commit -> unit

(** Exact native Unavailable on SAME writer; not Closed or joined worker. Data
    alone is not proof that this startup attempted an uncertain root write. *)
val requires_recovery : t -> bool
