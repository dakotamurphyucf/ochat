(** Original-domain cache-maintenance scheduler.
    Does not acquire or prove Store
    authority: the Factory's fixed bound attempt retains and checks actual owners.
    Does not depend on Registry/Runtime/Recovery_owner, avoiding an import cycle. *)
open! Core

type t

type attempted =
  | Completed
  | Deferred

(** Allocate before Actor services. No fiber or external resource yet. *)
val create : unit -> t

(** Nonyielding/nonblocking, single coalesced dirty generation. After close no-op.
    Called only after actual existing inline snapshot installation succeeds. *)
val request : t -> unit

(** Bind ONCE after actual Actor/runtime exist. May start before publication:
    [attempt] must return Deferred unless actual per-ID Loaded entry tuple matches.
    It runs only on the creating domain. The fixed Factory callback protects
    the whole attempt, including preliminary read-only recovery work outside the
    Actor mailbox. Separate Actor quiescent requests acquire NONWAITING per-ID
    Registry lifecycle for capture and final acceptance. Active scopes defer
    without reserving their lifecycle; competing lifecycle owners defer without
    Actor reentry. Preliminary work grants no pruning authority; final acceptance
    rechecks the actual owners and captured State/persistence basis before the
    original excluded pruning checks.
    Registry global locks must never be held across an Actor wait. The generic
    scheduler never acquires arbitrary owner capabilities.
    A pending request wakes worker; Deferred/Error retries with finite internal
    monotonic backoff, independent of global maintenance config. No document queue.
    Duplicate binding/closed binding is an invariant error before allocating fiber.
    Allocate/adopt owner before any yielding bind or later init step. *)
val bind_exn
  :  t
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.Mono.t
  -> attempt:(unit -> (attempted, Agent_protocol.Error.t) result)
  -> unit

(** Latest expected maintenance error; None initially/after completed success.
    Original-domain observation only, not a proof or test-only cache inspector.
    A returned Error keeps dirty work. Raised unexpected exception is retained by
    worker terminal result and re-raised, with original rawtrace, by the first
    close/join observer; subsequent idempotent joins succeed so retained resource
    cleanup can retry. Diagnostic failure remains retained, not rewritten as an
    expected error or allowed to kill the shared daemon switch. *)
val last_error : t -> Agent_protocol.Error.t option

(** Idempotent, also valid unbound. Stop request admission/wake worker; join any
    actual protected attempt, including preliminary recovery work, before returning.
    Does not hold Runtime/Registry locks or close writer/Handle. Preserve unexpected failure identity/rawtrace.
    Caller joins this before Actor, writer or Handle release. Factory entry close
    stops it before its Runtime join; generic Registry failure cleanup may already
    retire Runtime first, while Actor/writer/Handle remain alive. No Runtime access
    or mutex is permitted in the worker. *)
val close_and_wait : t -> unit
