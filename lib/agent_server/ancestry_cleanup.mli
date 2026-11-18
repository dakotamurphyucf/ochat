open! Core
module P = Agent_protocol
module CO = Agent_session.Close_observation

(** PRIVATE creating-domain holder for the one actual graph and the finite
    original ancestor lease edges. It grants no admission or cleanup authority.
    Registration and sealing are trusted original-bracket operations only. *)
type t

(** Private roles are recorded only at the original producer boundary. *)
module Control_role : sig
  type t =
    | Preparation
    | Cleanup
    | Unknown
end

module Own_post_callback_check : sig
  (** Immutable original evidence of the actual post-callback cancellation check.
      The selected group is one cancellation only and is not a callback alias.
      This carries no cleanup or execution authority and consumes no delivery. *)
  type t

  val diagnostic : t -> Agent_session.Cleanup_diagnostic.t

  (** Match SAME selected group, excluding independent Local/factual groups.
      Pure metadata validation; actual owner publication owns acknowledgment. *)
  val eligible
    :  t
    -> selected:Agent_session.Cleanup_diagnostic.t
    -> excluded:Agent_session.Cleanup_diagnostic.t list
    -> bool
end

module Snapshot : sig
  type t

  (** Current graph report and exact captured lease snapshots only. Callback,
      Local work and immutable scope controls are excluded. Missing or unfinished
      evidence is an outer refusal, never a new diagnostic or positive receipt. *)
  val cleanup : t -> ((unit, P.Error.t) result CO.t, P.Error.t) result

  (** Actual own-scope controls in structural selection priority. An actual outer
      own control that replaces a callback primary precedes that callback's
      prior own controls; forwarding the SAME callback primary adds no override.
      The original encounter view remains separate. Each CO keeps its incoming
      diagnostic order. Forwarded callback carriers and
      normally reported factual cleanup are not controls. Each negative retains
      its explicit selected carrier. No normalization happens in this getter. *)
  val fixed_controls : t -> ((unit, P.Error.t) result CO.t list, P.Error.t) result

  (** SAME actual priority/carriers as fixed_controls, omitting only explicitly
      tagged Preparation. Cleanup/Unknown and missing-origin refusals remain.
      Original diagnostics and custody are unchanged; no success is inferred. *)
  val cleanup_controls : t -> ((unit, P.Error.t) result CO.t list, P.Error.t) result

  val original_view : t -> Cleanup_original_view.t

  (** Existing graph and every captured lease's positive custody evidence;
      independent of diagnostic completeness, groups and callback success. *)
  val custody_joined : t -> bool

  val own_post_callback_checks : t -> Own_post_callback_check.t list
end

module Retry : sig
  type t =
    | Not_started of P.Error.t
    | Completed of Snapshot.t
    | Stopped of
        { refusal : P.Error.t
        ; current : Snapshot.t
        }
end

val create : unit -> t

(** Legacy reported chain receives this SAME already supplied graph holder. *)
val create_with_graph : Independent_cleanup.t -> t

val graph : t -> Independent_cleanup.t

(** Enter this original bracket once, before any validation or admission IO.
    Fresh and active originals both observe None, but reentrant/reused entry
    rejects before effects. *)
val begin_original : t -> unit

(** No effects or refresh. None until original sealing; while retry runs this
    returns the prior immutable snapshot. Foreign-domain access is outer Error. *)
val snapshot : t -> (Snapshot.t option, P.Error.t) result

(** Actual original preparation may fail before Driver fork invokes the chain.
    Fresh closes the actual unentered graph through the same seal path and
    returns that immutable snapshot; Complete returns the SAME snapshot. Running
    and retrying refuse before effects. Existing graph state validation and any
    actual failure evidence remain intact; no entered Switch/lease is invented. *)
val finish_without_entry : t -> (Snapshot.t, P.Error.t) result

(** Existing graph retry, then exact lease retries in inner-to-outer unwind
    order. Only eligible factual duties execute. No callback, constructor, hook
    or successful duty replays. Partial completed progress remains published if
    a later lifecycle refusal stops the fold. No diagnostic is minted on refusal. *)
val retry_observed : t -> Retry.t

module Edge : sig
  type t
end

(** Capture before the original Owner admission can yield. *)
val capture_lease
  :  t
  -> owner:Runtime_owner.t
  -> Runtime_owner.Cleanup_recovery.t
  -> Edge.t

(** The actual Owner can finish before its capture callback ran (for example
    cancellation entering its Cancel context). Validate the SAME returned holder
    against an existing capture, or enroll that actual returned holder now.
    This is nonyielding and does not pretend capture preceded original entry. *)
val finish_capture
  :  t
  -> owner:Runtime_owner.t
  -> captured:Edge.t option
  -> actual:Runtime_owner.Cleanup_recovery.t
  -> Edge.t

(** Record at the actual unwind with both values still in hand. None callback
    means f was not entered. A negative primary with the SAME callback selected
    carrier forwards it; a distinct selected primary is this edge's own control.
    Positive Scope.primary selects only factual cleanup. Missing provenance is
    retained as refusal, without guessing from payloads or diagnostic ordering. *)
val record_lease
  :  t
  -> Edge.t
  -> callback:(unit, P.Error.t) result CO.t option
  -> outcome:unit Runtime_owner.Lease_outcome.t
  -> unit

(** Legacy generic original chain wrappers retain the same actual edge facts.
    They do not claim an observed callback partition. A negative unavailable
    partition stays unavailable; no fabricated carrier or hidden value boxing. *)
val record_legacy_lease : t -> Edge.t -> outcome:'a Runtime_owner.Lease_outcome.t -> unit

(** Already captured actual original validation/registration/seal control, once
    at its producer boundary. Known observations forward their SAME carriers. *)
val record_control : t -> role:Control_role.t -> (unit, P.Error.t) result CO.t -> unit

(** After all original scopes exit. Finishes the actual graph without entry only
    when it never entered; freezes an immutable view of every recorded edge.
    Reentry, late registration and wrong-domain mutation are invariants. *)
val seal : t -> unit

(** Paired SAME seal operation for the observed adapter; no recapture after raw resume. *)
val seal_observed : t -> (unit, P.Error.t) result CO.t
