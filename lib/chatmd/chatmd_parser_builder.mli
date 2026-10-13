(** The single grammar delegates construction to a per-invocation builder.
    Primitive offsets are ignored by the ordinary semantic builder. *)
module Text_fragment : sig
  type t

  (** Lexer-owned emitted bytes and their actual half-open effective-input byte
      interval. A located builder validates the bytes before publishing the
      fragment; construction alone is not proof of an authored mapping. Empty
      RAW bodies have equal endpoints. *)
  val create : text:string -> start_byte:int -> end_byte:int -> t

  val text : t -> string
  val start_byte : t -> int
  val end_byte : t -> int
end

module type S = sig
  type node

  val text : Text_fragment.t list -> node

  val element
    :  Chatmd_ast.tag
    -> Chatmd_ast.attribute list
    -> children:node list
    -> start_byte:int
    -> end_byte:int
    -> node
end

module Legacy : S with type node = Chatmd_ast.node
