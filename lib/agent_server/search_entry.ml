open! Core
module P = Agent_protocol
module H = P.Public_history

module Part = struct
  type t =
    { index : int
    ; text : string
    }
end

type t =
  { history_id : P.History.Id.t
  ; content_revision : P.History.Content_revision.t
  ; parts : Part.t list
  ; accounted_bytes : int
  }

let resource_limit () =
  P.Error.create
    Resource_limit
    ~message:"search entry exceeds 4096 parts or 1 MiB of readable text"
    ~retryable:false
    ()
;;

let admit (entry : H.t) content =
  let open Result.Let_syntax in
  if List.length content > 4096
  then Error (resource_limit ())
  else (
    let%map _, text_bytes, reversed =
      List.fold_result content ~init:(0, 0, []) ~f:(fun (index, bytes, reversed) part ->
        match part with
        | H.Visible.Image _ | Redacted_part _ -> Ok (index + 1, bytes, reversed)
        | Text text | Refusal text ->
          let length = String.length text in
          if length > 1_048_576 - bytes
          then Error (resource_limit ())
          else if not (Stdlib.String.is_valid_utf_8 text)
          then Error (P.Error.invalid_request "search entry is not valid UTF-8")
          else Ok (index + 1, bytes + length, Part.{ index; text } :: reversed))
    in
    if List.is_empty reversed
    then None
    else
      Some
        { history_id = entry.id
        ; content_revision = entry.content_revision
        ; parts = List.rev reversed
        ; accounted_bytes =
            text_bytes
            + (64 * List.length reversed)
            + 128
            + String.length (P.History.Id.to_string entry.id)
        })
;;

let of_public (entry : H.t) =
  match entry.provenance with
  | P.History.Moderator_inserted
  | Moderator_replaced _
  | Runtime_notification _
  | Runtime_authoring _ -> Ok None
  | Canonical ->
    let visible =
      match entry.body with
      | H.Redacted _ -> None
      | Visible visible -> Some visible
      | Full payload -> H.Visible.of_semantic (History_entry.Payload.semantic payload)
    in
    (match visible with
     | None | Some (H.Visible.Reasoning _) -> Ok None
     | Some (Message { role; content; form = _; phase = _ }) ->
       (match role with
        | History_entry.Payload.Role.System | Developer | Tool -> Ok None
        | User | Assistant -> admit entry content))
;;

let history_id t = t.history_id
let content_revision t = t.content_revision
let parts t = t.parts
let accounted_bytes t = t.accounted_bytes

let navigation_context t =
  let buffer = Buffer.create 2048 in
  let rec prefix text length =
    if length = String.length text || length = 0
    then length
    else (
      let byte = Char.to_int text.[length] in
      if byte < 128 || byte >= 192 then length else prefix text (length - 1))
  in
  let rec append first = function
    | [] -> false
    | (part : Part.t) :: rest ->
      let separator = if first then 0 else 1 in
      let available = 2048 - Buffer.length buffer in
      if available < separator
      then true
      else (
        if not first then Buffer.add_char buffer '\n';
        let bytes =
          prefix part.text (Int.min (available - separator) (String.length part.text))
        in
        Buffer.add_substring buffer part.text ~pos:0 ~len:bytes;
        if bytes < String.length part.text then true else append false rest)
  in
  let truncated = append true t.parts in
  P.Search_navigation.Entry.create
    ~history_id:t.history_id
    ~content_revision:t.content_revision
    ~text:(Buffer.contents buffer)
    ~truncated
;;
