open Core
module Id = Agent_protocol.Id.Blob

type node =
  { mutable ending : Id.t option
  ; children : node Char.Table.t
  }

type t =
  { root : node
  ; max_length : int
  ; found : Id.t Hash_set.t
  ; mutable tail : string
  }

let node () = { ending = None; children = Char.Table.create () }

let create ids =
  let open Result.Let_syntax in
  let root = node () in
  let%map max_length =
    List.fold_result ids ~init:0 ~f:(fun longest id ->
      let text = Id.to_string id in
      let%map _ = Id.of_string text in
      let rec insert parent offset =
        match offset = String.length text with
        | true -> parent.ending <- Some id
        | false ->
          let child = Hashtbl.find_or_add parent.children text.[offset] ~default:node in
          insert child (offset + 1)
      in
      insert root 0;
      Int.max longest (String.length text))
  in
  { root; max_length; found = Hash_set.create (module Id); tail = "" }
;;

let begin_root t = t.tail <- ""

let feed ?ignore t chunk =
  let contents =
    match String.is_empty t.tail with
    | true -> chunk
    | false -> t.tail ^ chunk
  in
  let length = String.length contents in
  let rec walk parent offset =
    Option.iter parent.ending ~f:(fun id ->
      match Option.exists ignore ~f:(Id.equal id) with
      | true -> ()
      | false -> Hash_set.add t.found id);
    match offset < length with
    | false -> ()
    | true ->
      Option.iter
        (Hashtbl.find parent.children contents.[offset])
        ~f:(fun child -> walk child (offset + 1))
  in
  for offset = 0 to length - 1 do
    match Hashtbl.find t.root.children contents.[offset] with
    | None -> ()
    | Some child -> walk child (offset + 1)
  done;
  t.tail <- String.suffix contents (Int.min length (Int.max 0 (t.max_length - 1)))
;;

let referenced t id = Hash_set.mem t.found id
let references t = Hash_set.to_list t.found |> List.sort ~compare:Id.compare
