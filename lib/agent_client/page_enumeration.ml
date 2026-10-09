open! Core
module P = Agent_protocol

let collect (initial : P.Page.Request.t) ~max_items ~max_pages ~read =
  let open Result.Let_syntax in
  let invalid message = Error (P.Error.invalid_request message) in
  if max_items <= 0 || max_pages <= 0 || Option.is_some initial.cursor
  then invalid "enumeration requires positive bounds and a fresh query"
  else (
    let%bind _ = P.Page.Request.create ~limit:initial.limit () in
    let rec loop request pages count reversed seen =
      let%bind page = read request in
      let length = List.length page.P.Page.items in
      if length > max_items - count
      then invalid "enumeration exceeds max_items"
      else (
        let count = count + length in
        let reversed = List.rev_append page.items reversed in
        match page.next_cursor with
        | None -> Ok (List.rev reversed)
        | Some cursor ->
          let encoded = P.Page.Cursor.to_string cursor in
          if Set.mem seen encoded
          then invalid "enumeration received a repeated cursor"
          else if pages >= max_pages
          then invalid "enumeration exceeds max_pages"
          else
            loop
              { initial with cursor = Some cursor }
              (pages + 1)
              count
              reversed
              (Set.add seen encoded))
    in
    loop initial 1 0 [] String.Set.empty)
;;
