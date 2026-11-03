open! Core

(** Bounded, read-only REPL value presentation. This never calls user code,
    forces a Task, or traverses closure/module environments. *)
module Limits : sig
  type t

  (** Byte/node/depth bounds must be in 1..65536, 1..4096 and 1..128,
      respectively; columns must be in 8..512. Invalid bounds return [Error]. *)
  val create
    :  max_bytes:int
    -> max_nodes:int
    -> max_depth:int
    -> columns:int
    -> t Or_error.t
end

(** Render runtime constructors directly with quoted strings and compact or
    multiline layouts. Byte, node and depth limits mark omitted content with
    [...]. Output is valid UTF-8 and at most [max_bytes] bytes; malformed string
    bytes and terminal controls are escaped. Cyclic references are depth/node
    bounded. Tasks show their unevaluated shape without traversing effects. *)
val to_string : limits:Limits.t -> Chatml.Chatml_lang.value -> string
