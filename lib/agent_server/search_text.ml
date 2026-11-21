open! Core
module P = Agent_protocol
module Literal = P.History_text.Literal

type t = Literal.t

let create = Literal.create

module Match = struct
  type t =
    { part_index : int
    ; snippet : P.Search_snippet.t
    }
end

let find t entry =
  let rec find_parts = function
    | [] -> Ok None
    | (part : Search_entry.Part.t) :: rest ->
      let open Result.Let_syntax in
      let%bind found = Literal.find t part in
      (match found with
       | None -> find_parts rest
       | Some hit -> Ok (Some Match.{ part_index = part.index; snippet = hit.snippet }))
  in
  find_parts (Search_entry.parts entry)
;;
