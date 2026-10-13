open! Core

type t =
  { node : Chatmd_ast.node
  ; children : t list
  ; range : Chatmd_parser_input.Range.t option
  ; text_fragments : Chatmd_parser_builder.Text_fragment.t list
  ; input : Chatmd_parser_input.t
  }

let node t = t.node
let children t = t.children
let range t = t.range
let text_fragments t = t.text_fragments
let input t = t.input

module Make (Owner : sig
    val input : Chatmd_parser_input.t
    val checkpoint : unit -> unit
  end) =
struct
  type node = t

  let text fragments =
    let module Fragment = Chatmd_parser_builder.Text_fragment in
    let source = Chatmd_parser_input.effective Owner.input in
    List.iter fragments ~f:(fun fragment ->
      Owner.checkpoint ();
      let text = Fragment.text fragment in
      let start_byte = Fragment.start_byte fragment in
      let end_byte = Fragment.end_byte fragment in
      if
        start_byte < 0
        || end_byte < start_byte
        || end_byte > String.length source
        || not (Int.equal (String.length text) (end_byte - start_byte))
      then failwith "ChatMD text fragment has invalid source interval";
      for offset = 0 to String.length text - 1 do
        if offset mod 4096 = 0 then Owner.checkpoint ();
        if not (Char.equal text.[offset] source.[start_byte + offset])
        then failwith "ChatMD text fragment differs from its parser input"
      done);
    { node = Chatmd_ast.Text (String.concat ~sep:"" (List.map fragments ~f:Fragment.text))
    ; children = []
    ; range = None
    ; text_fragments = fragments
    ; input = Owner.input
    }
  ;;

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
    { node; children; range = Some range; text_fragments = []; input = Owner.input }
  ;;
end
