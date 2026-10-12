(** The single grammar delegates construction to a per-invocation builder.
    Primitive offsets are ignored by the ordinary semantic builder. *)
module type S = sig
  type node

  val text : string -> node

  val element
    :  Chatmd_ast.tag
    -> Chatmd_ast.attribute list
    -> children:node list
    -> start_byte:int
    -> end_byte:int
    -> node
end

module Legacy : S with type node = Chatmd_ast.node
