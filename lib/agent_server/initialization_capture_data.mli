open! Core

(** Private factual association supplied only by an original Owner bracket and
    consumed by the actual Factory initialization callsites. No authority is
    issued and no callback/End execution is retained. *)
type t

type boundary
type end_outcome = Agent_session.Callback_cleanup.Operation.terminal_outcome

val create : actor:Agent_session.Session_actor.t -> register:(boundary -> unit) -> t

(** Same-domain exact Actor check precedes original registration. Factory must
    install its existing End finalizer before calling this operation. *)
val initialization
  :  t
  -> actor:Agent_session.Session_actor.t
  -> scope:Agent_session.Session_actor.Initialization_scope.t
  -> boundary

val same_scope : boundary -> boundary -> bool
val record_original_end : boundary -> end_outcome -> unit

(** Creating-domain invariant is checked before access; misuse raises rather
    than pretending to be a cleanup failure. Public recovery checks its domain
    before reaching this private observer. *)
val snapshot
  :  boundary
  -> (Agent_session.Callback_cleanup.Cleanup_snapshot.t, Agent_protocol.Error.t) Result.t

val original_end : boundary -> end_outcome option

(** Immutable original diagnostic views. The original End observation freezes
    its known callback/End groups and completeness. The SAME Init recipe may
    retain a later observed write-once callback slot and include its stable group
    in a fresh view; previously returned views/payloads never change, and original
    incomplete finish evidence never becomes complete through later release.
    This data carries no custody or cleanup execution authority. *)
val original_diagnostics : boundary -> Original_cleanup_report.t

val original_diagnostics_sealed : boundary -> bool

(** SAME original producer groups are retained by actual Completion and its
    write-once failure slot. A newly observed slot owns one bounded carrier here;
    repeated current selections forward it. This never rewrites the frozen
    original diagnostic view or claims that its incomplete Init is sealed. The
    caller obtains all arguments from this boundary's actual current snapshot. *)
val completion_diagnostic
  :  boundary
  -> completion:Agent_session.Callback_cleanup.Completion.t
  -> slot:Agent_session.Callback_cleanup.Completion_snapshot.Failure_slot.kind
  -> failure:Agent_session.Callback_cleanup.Failure.t
  -> Original_cleanup_report.Segment.t

val end_diagnostic : boundary -> Original_cleanup_report.Segment.t option

(** Called only by the SAME actual Init recipe, with this boundary's returned
    immutable claim observation, before building its fresh original child view.
    Unknown write-once slots acquire bounded stable origins here; previously
    issued original views and the original completeness fact stay unchanged. *)
val observe_completion
  :  boundary
  -> Agent_session.Callback_cleanup.Completion_observation.t
  -> Original_cleanup_report.Segment.t list
