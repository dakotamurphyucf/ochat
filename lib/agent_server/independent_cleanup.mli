open! Core

module Failure : sig
  type t =
    | Protocol of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  val resume : ('a, t) result -> ('a, Agent_protocol.Error.t) result
end

(** Creating-domain custody of the existing graph/Switch bracket only. No
    Owner/Cell dependency or execution permission. Outer Owner ancestry
    finalizers require their separate actual reports. *)
type t

module Report : sig
  type t

  type status =
    | Never_entered
    | Joined
    | Failed
    | Incomplete
  [@@deriving compare, equal, sexp_of]

  val status : t -> status

  (** Actual seal, queued-hook, then finish failures, in execution order.
      Preserves duplicate exception objects and each actual raw backtrace.
      Source/worker errors are never invented as release-hook failures. *)
  val failures : t -> Failure.t list

  (** SAME immutable actual producer carriers in the same execution order. A
      Switch invocation owns at most one group; its raw individual hook failures
      remain separately available through [failures] and [release_report]. When
      that group carries a raw Switch aggregate, its supporting parts contain
      that opaque result followed by every actual release failure. These are
      supporting parts of one invocation, not a deduplicated leaf set or separate
      producer identities: Eio may omit a Cancelled-valued hook from the aggregate.
      This broader context does not classify a worker as a hook or
      change the flat hook payload/order in [failures]. *)
  val diagnostics : t -> Agent_session.Cleanup_diagnostic.t list

  (** Exact original Switch release evidence, never inferred from failures.
      None only for genuine [finish_without_entry]. *)
  val release_report : t -> Eio.Switch.Release_report.t option

  (** Pure projection of this immutable actual cleanup report and its explicit
      selected carrier. Outer Error is a lifecycle refusal when evidence is
      incomplete without an actual failure. No graph work, delivery or minting. *)
  val observation
    :  t
    -> ( (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
         , Agent_protocol.Error.t )
         result
end

module Outcome : sig
  type 'a t

  (** Exact existing primary precedence: original callback protocol/raised
      failure wins; otherwise actual Switch/graph cleanup may fail the result.
      This data does not certify cleanup. Raised outcomes keep original traces. *)
  val result : 'a t -> ('a, Failure.t) result

  val cleanup : 'a t -> Report.t

  (** Exact original raw Switch result and full release evidence. *)
  val switch_outcome : 'a t -> ('a, Agent_protocol.Error.t) result Eio.Switch.Outcome.t

  (** Actual selected raw unit result paired with its SAME diagnostic carriers.
      This neither replays the operation nor consumes legacy delivery. *)
  val observation
    :  unit t
    -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
end

(** No scope allocation. Capture before the first actual ancestry yield. *)
val create : unit -> t

(** Enter the single existing Switch exactly once using public run_reported.
    Raised callback/seal control still reaches Switch cancellation unchanged.
    Registered actual graphs seal before join and finish after Switch exit.
    Registration is creating-domain only and open only while [f] is active;
    it closes before sealing on either callback exit. Each physical graph may
    register once. Escaped late/foreign/duplicate registration rejects before
    custody mutation, so no unsealed Pending graph can produce Joined.
    Reentry/foreign-domain use rejects as a lifecycle invariant violation. *)
val run
  :  t
  -> f:
       (sw:Eio.Switch.t
        -> register_tracking:(Graph_tracking.t -> unit)
        -> ('a, Agent_protocol.Error.t) result)
  -> 'a Outcome.t

(** Unit-result observed callback using the SAME bracket/registration/finalizer
    implementation as [run]. The callback observation is retained before raw
    projection; an escaping callback is captured at that actual invocation.
    No callback or release hook is executed twice. A negative supplied result
    with no meaningful declared primary retains its groups plus one actual
    callback-boundary carrier. Creating-domain/reentry rules match [run]. *)
val run_observed
  :  t
  -> f:
       (sw:Eio.Switch.t
        -> register_tracking:(Graph_tracking.t -> unit)
        -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> unit Outcome.t

(** Immutable ORIGINAL completed cleanup report. None while the original bracket
    is live; factual retries never replace this report or its carriers. *)
val original_report : t -> Report.t option

(** Latest completed bracket/graph-retry report. None before actual exit.
    While a factual retry runs, returns the prior immutable report.
    Previously returned Outcome/Report values remain immutable. *)
val report : t -> Report.t option

(** Fixed ancestry bracket calls this after its outer scopes exit. If [run] was
    never entered, closes that fresh holder as Never_entered and prevents later
    entry. Already finished brackets are unchanged; active run/retry rejects. No Switch
    report or successful outer Owner cleanup is fabricated. *)
val finish_without_entry : t -> unit

(** After actual Switch join, retry only failed factual graph legs on SAME
    graphs. Never rerun f/build/ancestry, successful legs or hooks. Graph success
    cannot erase an actual hook failure/incomplete join. Before exit returns a
    finite Conflict. One creating-domain retry owns the factual legs at a time;
    a concurrent/reentrant call returns finite Conflict without replay or join.
    Does not certify outer Owner finalizers. *)
val retry : t -> (unit, Agent_protocol.Error.t) result

(** Pure creating-domain status; no graph effect or diagnostic allocation. Fixed
    means no eligible failed factual leg, or an unjoined Switch. Immutable hook
    negatives do not prevent Retryable when an eligible graph leg remains. *)
type retry_status =
  | Original_running
  | Busy
  | Retryable
  | Joined
  | Fixed
[@@deriving equal, sexp_of]

val retry_status : t -> retry_status

(** Outer Error is original-running/busy lifecycle refusal or an unjoined
    report with no actual failure to project, before graph work.
    Inner observation is the actual same-holder graph retry/fixed result; it
    preserves actual raw failure and producer carriers; missing evidence never
    creates a diagnostic group. Positive graph legs and
    original callbacks/hooks never replay. Uses the same implementation as
    [retry]; creating-domain misuse retains the existing invariant exception. *)
val retry_observed
  :  t
  -> ( (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
       , Agent_protocol.Error.t )
       result
