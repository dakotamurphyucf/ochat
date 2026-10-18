(** Pure bounded presentation of existing REPL receipts. The evaluator and
    typing owner finalize each receipt once; this module never retypes,
    re-evaluates, re-freezes or owns a Task. It has no Host, source or actor
    authority. Only the caller may retain the original frozen receipts. *)

open! Core
module Evaluator = Chatml_repl_execution

module Statement : sig
  type t

  type task_retention =
    | Not_a_task
    | Retained
    | Limit_reached
  [@@deriving equal, sexp_of]

  val outcome : t -> Evaluator.Receipt.outcome
  val value_preview : t -> string option
  val diagnostic : t -> Evaluator.Diagnostic.t option
  val expression_type : t -> Chatml_type_display.t option
  val binding_count : t -> int
  val task_retention : t -> task_retention

  (** True when a textual field could not fit the requested presentation
      budget. The outcome and binding count still describe the real receipt;
      a truncated display is never presented as a complete annotation. *)
  val text_limited : t -> bool
end

module Presentation : sig
  type t

  (** Bounded execution-order prefix. [total_statements] is the exact receipt
      count even when the prefix is shorter. *)
  val statements : t -> Statement.t list

  val total_statements : t -> int

  (** False if the prefix is short or any displayed text was omitted or
      truncated. *)
  val complete : t -> bool

  val charged_bytes : t -> int
end

module Error : sig
  type t =
    | Invalid_max_bytes
    | Invalid_selection
    | Foreign_snapshot
    | Output_limit
  [@@deriving sexp_of]
end

(** Conservative fixed framing, digest/span and exact source identity bytes
    retained by a location. Shared with private receipt accounting; it is not
    an encoded-wire size claim. *)
val location_charge : Chatml_source.Location.t -> int

(** Conservatively charge fixed per-entry framing, emitted UTF-8 text and
    retained diagnostic source identity against [max_bytes]
    before allocation. The installed display limits independently bound graph
    scan, depth, work and each emitted type. A too-small budget gives a
    truthful prefix/limited field, never a larger uncharged result. Rendering
    uses roots from each receipt's own immutable snapshot and preserves
    cancellation exceptions/backtraces from [checkpoint]. *)
val present
  :  display_limits:Chatml_type_display.Limits.t
  -> max_bytes:int
  -> Evaluator.Receipt.t list
  -> checkpoint:(unit -> unit)
  -> (Presentation.t, Error.t) result

module Selector : sig
  (** [Binding_index] is zero-based in the receipt's ordered frozen bindings. *)
  type t =
    | Expression
    | Binding_index of int
  [@@deriving equal, sexp_of]
end

module Inspected : sig
  type t

  val binding_name : t -> string option
  val rendered : t -> Chatml_type_display.t
end

(** Render one snapshot-owned root from the original retained receipt. Fixed
    framing and an exact binding name are charged before type rendering; too
    little budget returns [Output_limit]. A
    missing expression or out-of-range binding is an invalid selection; a
    foreign root is rejected by Chatml_type_display rather than resolved from
    another graph. The caller handles exact-key detail expiry and current
    source/actor admission before and after this possibly yielding operation.
    [max_bytes] is intersected with the installed display limits. *)
val inspect
  :  display_limits:Chatml_type_display.Limits.t
  -> max_bytes:int
  -> Evaluator.Receipt.t
  -> selector:Selector.t
  -> checkpoint:(unit -> unit)
  -> (Inspected.t, Error.t) result
