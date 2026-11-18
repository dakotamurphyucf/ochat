open Core

(** Pure admitted zone data. This module performs no IO and is not a TZif reader.
    Offsets apply only within explicit inclusive UTC coverage. A loader must not
    extend the final offset beyond data it actually understands (including future
    rules). Loading/provenance admission belong to the runtime host. *)
type t

module Provenance : sig
  type t [@@deriving equal, sexp_of]

  (** [sha256] is lowercase hexadecimal byte provenance, never execution authority.
      [source] is a nonempty bounded description of the admitted data version. *)
  val create : source:string -> sha256:string -> (t, Timing_error.t) Result.t

  val source : t -> string
  val sha256 : t -> string
end

module Transition : sig
  type t =
    { at : Time_ns.Alternate_sexp.t
    ; offset_seconds : int
    }
  [@@deriving equal, sexp]
end

(** Names are bounded relative slash-separated identifiers, never filesystem
    grants. Coverage must be ordered. Transitions are strictly increasing and
    strictly after [from], at or before [until]. Offsets are integral seconds in
    [-86400,86400]. Count is bounded before derived allocations. *)
val create
  :  name:string
  -> provenance:Provenance.t
  -> from:Time_ns.Alternate_sexp.t
  -> until:Time_ns.Alternate_sexp.t
  -> initial_offset_seconds:int
  -> transitions:Transition.t list
  -> (t, Timing_error.t) Result.t

val name : t -> string
val validate_name : string -> (unit, Timing_error.t) Result.t
val provenance : t -> Provenance.t
val coverage : t -> Time_ns.Alternate_sexp.t * Time_ns.Alternate_sexp.t
val offset_seconds : t -> Time_ns.Alternate_sexp.t -> (int, Timing_error.t) Result.t

module Resolution : sig
  type t =
    | Candidates of Time_ns.Alternate_sexp.t list
    | Gap of { ends_at : Time_ns.Alternate_sexp.t }
  [@@deriving equal, sexp]
end

(** Enumerate and sort all exact UTC candidates by offset/roundtrip, independent
    of midnight assumptions and shift size. Gap means the entire missing local
    interval is proved by an admitted transition. Return Out_of_coverage when
    completeness cannot be established; the complete possible offset window of
    24 hours on either side must be covered. Never substitute local time or UTC.
    Candidate lists are bounded by the finite validated offset set. *)
val resolve
  :  t
  -> date:Date.t
  -> ofday:Time_ns.Ofday.t
  -> (Resolution.t, Timing_error.t) Result.t

val to_local
  :  t
  -> Time_ns.Alternate_sexp.t
  -> (Date.t * Time_ns.Ofday.t, Timing_error.t) Result.t
