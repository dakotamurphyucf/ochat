open! Core

module Text_fragment = struct
  type t =
    { text : string
    ; start_byte : int
    ; end_byte : int
    }

  let create ~text ~start_byte ~end_byte = { text; start_byte; end_byte }
  let text t = t.text
  let start_byte t = t.start_byte
  let end_byte t = t.end_byte
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

module Legacy = struct
  type node = Chatmd_ast.node

  let text fragments =
    Chatmd_ast.Text (String.concat ~sep:"" (List.map fragments ~f:Text_fragment.text))
  ;;

  let element tag attributes ~children ~start_byte:_ ~end_byte:_ =
    Chatmd_ast.Element (tag, attributes, children)
  ;;
end
