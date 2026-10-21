open! Core

(** Terminal-only projection. Source highlighting uses the shared ChatML
    grammar/theme; all remote text is bounded, UTF-8 repaired and control
    characters escaped before it reaches Notty. *)
type t

val create : max_display_bytes:int -> (t, Agent_protocol.Error.t) result
val image : t -> State.t -> width:int -> height:int -> Notty.image * (int * int) option
