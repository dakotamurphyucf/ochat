open! Core

(** Finite execution driver inside the existing schedule scheduler. It owns no
    timer, fiber, Store writer or alternate executor. Same creating Eio domain. *)
type t

val create
  :  factory:Session_factory.t
  -> registry:Session_registry.t
  -> host:Recurrence_execution_host.t
  -> t

(** Scan at most 32 distinct occurrence candidates from a rotating immutable-key
    cursor. Reconcile each pending occurrence before any retry of that occurrence;
    a nonterminal, absent, busy or denied row does not starve independent targets.
    At most one activation/retry action occurs per pass. Only the actual completed
    Factory envelope supplies live NoEffects or frozen Retry custody; recovered
    rows reconcile without replay. Unreadable historical occupancy conservatively defers local new work.

    All work and actual source/permit cleanup join in this caller's cancellable
    scheduler fiber. The driver retains one current joined attempt per actual
    retained pending occurrence, bounded by the existing catalog/frontier profiles,
    plus deny-only in-flight target metadata. Failed attempts without joined retry
    evidence remain retained to prevent silently replacing their request. It retains no source lease or
    execution permit registry. Foreign/replaced entries cannot inherit this data.
    An ordinal-zero source failure before controller recording may re-enter only
    the SAME immutable request after genuine joined NoEffects and repeated actual
    controller absence/current target checks. Recorded predecessors use the actual
    retry owner; decoded absence never supplies that eligibility.
    An error can follow independent factual progress and is never proof of no
    effects. Actual execution errors take precedence over scheduling deferrals. *)
val process_entry : t -> Session_registry.entry -> (unit, Agent_protocol.Error.t) result

(** Drop process-local completed envelopes; never cancels or certifies source
    cleanup. The existing scheduler cancels and joins its actual sweep fibers. *)
val close : t -> unit
