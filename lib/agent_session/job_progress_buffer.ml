open Core
module P = Agent_protocol.Job_progress

type t = { mutable value : P.t option }

let create () = { value = None }
let snapshot t = t.value

let valid (update : Ochat_function.Progress.t) =
  let text =
    match update.update with
    | Append text | Replace text -> text
  in
  String.length text <= P.max_update_bytes && Stdlib.String.is_valid_utf_8 text
;;

let suffix text =
  let offset = Int.max 0 (String.length text - P.max_channel_bytes) in
  let rec boundary offset =
    match offset < String.length text && Char.to_int text.[offset] land 0xc0 = 0x80 with
    | true -> boundary (offset + 1)
    | false -> offset
  in
  let offset = boundary offset in
  String.drop_prefix text offset, offset > 0
;;

let update t (update : Ochat_function.Progress.t) =
  let sequence = Option.value_map t.value ~default:0 ~f:(fun value -> value.P.sequence) in
  match sequence < P.max_updates && valid update with
  | false -> ()
  | true ->
    let channel =
      match update.channel with
      | `Assistant -> P.Assistant
      | `Reasoning -> Reasoning
      | `Stdout -> Stdout
      | `Stderr -> Stderr
      | `Activity -> Activity
    in
    let channels =
      Option.value_map t.value ~default:[] ~f:(fun value -> value.P.channels)
    in
    let previous =
      List.find channels ~f:(fun item -> P.equal_channel item.channel channel)
    in
    let text, truncated =
      match update.update with
      | Replace text -> text, false
      | Append text ->
        let text, truncated =
          suffix (Option.value_map previous ~default:"" ~f:(fun item -> item.text) ^ text)
        in
        text, truncated || Option.exists previous ~f:(fun item -> item.truncated)
    in
    let item = P.{ channel; text; truncated } in
    let channels =
      item
      :: List.filter channels ~f:(fun item -> not (P.equal_channel item.channel channel))
      |> List.sort ~compare:(fun a b -> P.compare_channel a.channel b.channel)
    in
    t.value <- Some { sequence = sequence + 1; channels }
;;
