open! Core

let boundaries text =
  Uuseg_string.fold_utf_8
    `Grapheme_cluster
    (fun (offset, offsets) cluster ->
       let offset = offset + String.length cluster in
       offset, offset :: offsets)
    (0, [ 0 ])
    text
  |> snd
  |> List.rev
;;

let floor text pos =
  boundaries text
  |> List.take_while ~f:(fun offset -> offset <= pos)
  |> List.last
  |> Option.value ~default:0
;;

let ceil text pos =
  boundaries text
  |> List.find ~f:(fun offset -> offset >= pos)
  |> Option.value ~default:(String.length text)
;;

let previous text pos = floor text (pos - 1)
let next text pos = ceil text (pos + 1)

let uchar u =
  let buffer = Buffer.create 4 in
  Stdlib.Buffer.add_utf_8_uchar buffer u;
  Buffer.contents buffer
;;
