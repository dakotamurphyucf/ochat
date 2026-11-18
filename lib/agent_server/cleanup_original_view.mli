open! Core

(** Immutable original diagnostic view supplied by an actual cleanup owner.
    This read-only facade preserves the SAME private report and diagnostic
    carrier identities; it exposes no recipe construction or cleanup authority.
    Fresh observations may include an originally selected child's later outcome,
    without changing any previously returned view. *)
type t = Original_cleanup_report.t

(** Original producer order with SAME-carrier aliases coalesced. This pure read
    does not consume legacy delivery. An empty list is not cleanup success. *)
val diagnostics : t -> Agent_session.Cleanup_diagnostic.t list

(** Whether all originally selected diagnostic facts in this view are known.
    This is independent of current custody, disposal and retry eligibility. *)
val diagnostics_complete : t -> bool
