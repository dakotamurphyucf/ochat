open! Core

(** Exact local writer bookkeeping for the paired legacy idle completion.
    Pure ownership/phase data only; this module never writes, authorizes a source
    or interprets decoded session fields as permission. *)
module Kind : sig
  type t =
    | Reconcile_foreground
    | Checkpoint
    | Suppress_automatic
    | Stop
    | Callback_failure
    | Start_compaction
    | Start_ordinary_turn
    | Retire_turn
    | Retire_compaction
    | Reconcile_terminal
  [@@deriving equal]
end

module Selected : sig
  type t =
    | Requested of Kind.t
    | Admission_hold of Agent_protocol.Id.Goal.t
end

module Diagnostic : sig
  type t =
    | Returned of Agent_protocol.Error.t
    | Raised of exn * Backtrace.t
end

module Acknowledgement : sig
  type t

  val previous : t -> Session_state.t
  val candidate : t -> Session_transition.t
  val selected : t -> Selected.t
end

type t

type phase =
  | Not_attempted
  | Attempted
  | Installed of Acknowledgement.t

(** Capture an actual next-revision candidate produced by the real planner.
    Owner identity and physical immutable State/candidate identity deliberately
    form part of the lifecycle contract. [attempted] is the SAME generic flag
    supplied to the existing writer, not a canonical-only descendant flag. *)
val capture
  :  owner:unit ref
  -> previous:Session_state.t
  -> requested:Session_transition.t
  -> kind:Kind.t
  -> attempted:bool ref
  -> (t, Agent_protocol.Error.t) result

val previous : t -> Session_state.t
val requested : t -> Session_transition.t
val kind : t -> Kind.t
val attempted : t -> bool ref
val phase : t -> phase
val diagnostic : t -> Diagnostic.t option

(** Original final-check owner/immutable basis, before the actual commit. *)
val check_before_writer
  :  t
  -> owner:unit ref
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** The private actual commit site selects its requested candidate or its actual
    derived Goal Hold metadata. Retains a prebuilt ACK payload BEFORE writer;
    no success is represented until [install] at actual State assignment.
    Selects once and refuses retry after a real attempt. *)
val select_actual
  :  t
  -> owner:unit ref
  -> state:Session_state.t
  -> candidate:Session_transition.t
  -> selected:Selected.t
  -> (unit, Agent_protocol.Error.t) result

(** Closed nonfailing State-install step, after validated actual persistence ACK.
    The exact prebuilt candidate/State and owner must match; an internal misuse
    raises rather than inventing an ACK. No allocation, callback or RPC. *)
val install
  :  t
  -> owner:unit ref
  -> candidate:Session_transition.t
  -> state:Session_state.t
  -> unit

val retain_failure : t -> Agent_protocol.Error.t -> unit
val retain_exception : t -> exn -> Backtrace.t -> unit
