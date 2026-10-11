open Core

(** Private parser slot ownership. No content acquisition or display authority. *)
module Selection : sig
  type t =
    | Dynamic
    | Local_capture
    | Builtin of string
  [@@deriving equal, sexp_of]

  val of_attributes : src:string -> local:bool -> capture:bool -> t Or_error.t
end

module Source : sig
  type t

  val create
    :  Chatmd_parser_input.Range.t
    -> src:string
    -> selection:Selection.t
    -> local:bool
    -> cleanup_html:bool
    -> markdown:bool
    -> t
end

module Owner : sig
  type t

  val create : unit -> t
end

module Slot : sig
  type t [@@deriving compare, sexp_of]

  include Comparator.S with type t := t

  val create : top_level:int -> items:int list -> t
  val top_level : t -> int
  val items : t -> int list
end

type t

val create : Owner.t -> source:Source.t -> slot:Slot.t -> ordinal:int -> t
val belongs : t -> Owner.t -> bool
val slot : t -> Slot.t
val range : t -> Chatmd_parser_input.Range.t
val ordinal : t -> int
val src : t -> string
val selection : t -> Selection.t
val local : t -> bool
val cleanup_html : t -> bool
val markdown : t -> bool
