open! Core

(** Terminal-only projection. Source highlighting uses the shared ChatML
    grammar/theme; all remote text is bounded, UTF-8 repaired and control
    characters escaped before it reaches Notty. Complete source up to the
    renderer's [max_display_bytes] budget is highlighted before viewport
    clipping, preserving multiline lexical state. Larger source is displayed
    safely without syntax colors. Expression types and binding signatures come
    only from authoritative receipt/inspection data; a statement value is never
    substituted for the values of its bindings. *)
type t

val create : max_display_bytes:int -> (t, Agent_protocol.Error.t) result
val image : t -> State.t -> width:int -> height:int -> Notty.image * (int * int) option
