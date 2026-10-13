open Core

module Limits : sig
  type t

  val create
    :  max_items:int
    -> max_member_depth:int
    -> max_member_edges:int
    -> max_label_bytes:int
    -> max_work_steps:int
    -> type_display:Chatml_type_display.Limits.t
    -> t Or_error.t
end

type failure =
  | Work_limit
  | Label_byte_limit
  | Member_edge_limit

exception Exhausted of failure

type t

val create : Limits.t -> checkpoint:(unit -> unit) -> t
val checkpoint : t -> unit
val max_items : t -> int
val max_work_steps : t -> int
val max_member_depth : t -> int
val member_edge : t -> bool
val item : t -> bool
val remaining_labels : t -> int
val display_session : t -> Chatml_type_display.Session.session
val charge_display : t -> string -> unit
val equal_string : t -> string -> string -> bool
val equal_source_id : t -> Chatml_source.Id.t -> Chatml_source.Id.t -> bool

(** Deterministic ASCII display spelling for arbitrary captured label bytes.
    Printable ASCII except backslash is preserved; other bytes become [\\xHH].
    Work and encoded output bytes are charged before each append. *)
val label : t -> string -> string

val run : t -> f:(unit -> 'a) -> ('a, failure) Result.t
