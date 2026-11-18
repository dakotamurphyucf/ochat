open! Core

(** Immutable original diagnostic views. These data carry no cleanup execution,
    ownership, completion or disposal authority. Actual Owner recipes supply
    original completed outcomes and exact child views in their existing order. *)
module Segment : sig
  type t = Disposal_report_data.Diagnostic.t

  (** One actual original producer/control owns this carrier. Repeated views
      and prior-failure copies retain the SAME value; independently executed
      outcomes use distinct values, including when payloads are identical. *)
  val create : Agent_session.Callback_cleanup.Failure.t list -> t
end

type t

module Piece : sig
  type report = t

  type t =
    | Own of Segment.t
    | Dependency_copy of Segment.t
    | Child of report
end

(** Freeze one pure view. An incomplete child remains incomplete even if its
    completed original segments currently contain no failures. A later fresh
    child/parent observation never mutates this value. *)
val create : diagnostics_complete:bool -> Piece.t list -> t

(** Actual-owned occurrence order. Suppress a dependency copy only if the SAME
    producer segment is owned in this graph; otherwise retain it once. These
    operations never consume legacy delivery. Traversal expands shared child
    views; linear identity membership checks make worst-case cost quadratic in
    the expanded piece count. *)
val diagnostics : t -> Disposal_report_data.Diagnostic.t list

val diagnostics_complete : t -> bool
