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

module Legacy = struct
  type node = Chatmd_ast.node

  let text text = Chatmd_ast.Text text

  let element tag attributes ~children ~start_byte:_ ~end_byte:_ =
    Chatmd_ast.Element (tag, attributes, children)
  ;;
end
