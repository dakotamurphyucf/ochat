open! Core

type t =
  { node : Chatmd_ast.node
  ; children : t list
  ; range : Chatmd_parser_input.Range.t option
  }

let node t = t.node
let children t = t.children
let range t = t.range

module Make (Owner : sig
    val input : Chatmd_parser_input.t
    val checkpoint : unit -> unit
  end) =
struct
  type node = t

  let text text = { node = Chatmd_ast.Text text; children = []; range = None }

  let element tag attributes ~children ~start_byte ~end_byte =
    let range =
      Chatmd_parser_input.Range.create
        Owner.input
        ~start_byte
        ~end_byte
        ~checkpoint:Owner.checkpoint
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Chatmd_parser_input.Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    let node = Chatmd_ast.Element (tag, attributes, List.map children ~f:node) in
    { node; children; range = Some range }
  ;;
end
