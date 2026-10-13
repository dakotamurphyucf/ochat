(** Input-owned metadata constructed by the grammar alongside semantic nodes,
    never aligned through a second traversal or a source-text search. *)
type t

val node : t -> Chatmd_ast.node
val children : t -> t list
val range : t -> Chatmd_parser_input.Range.t option
val text_fragments : t -> Chatmd_parser_builder.Text_fragment.t list
val input : t -> Chatmd_parser_input.t

module Make (Owner : sig
    val input : Chatmd_parser_input.t
    val checkpoint : unit -> unit
  end) : Chatmd_parser_builder.S with type node = t
