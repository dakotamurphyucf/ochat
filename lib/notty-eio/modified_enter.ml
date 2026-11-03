open! Core

type t =
  { mutable suffix : string
  ; mutable paste : bool
  }

let create () = { suffix = ""; paste = false }
let escape_pending t = (not t.paste) && String.equal t.suffix "\027"

let flush_escape t =
  if escape_pending t
  then (
    t.suffix <- "";
    "\027")
  else ""
;;

let paste_start = "\027[200~"
let paste_end = "\027[201~"
let shifted = [ "\027[13;2u"; "\027[27;2;13~" ]

let feed t chunk =
  let source = t.suffix ^ chunk in
  t.suffix <- "";
  let output = Buffer.create (String.length source) in
  let rec loop offset =
    if offset < String.length source
    then
      if not (Char.equal source.[offset] '\027')
      then (
        Buffer.add_char output source.[offset];
        loop (offset + 1))
      else (
        let remaining = String.length source - offset in
        let candidates = if t.paste then [ paste_end ] else paste_start :: shifted in
        match
          List.find candidates ~f:(fun sequence ->
            String.length sequence <= remaining
            && String.is_substring_at source ~pos:offset ~substring:sequence)
        with
        | Some sequence ->
          if String.equal sequence paste_start
          then (
            t.paste <- true;
            Buffer.add_string output sequence)
          else if String.equal sequence paste_end
          then (
            t.paste <- false;
            Buffer.add_string output sequence)
          else Buffer.add_string output "\027\n";
          loop (offset + String.length sequence)
        | None ->
          if
            List.exists candidates ~f:(fun sequence ->
              remaining < String.length sequence
              && String.is_prefix sequence ~prefix:(String.drop_prefix source offset))
          then t.suffix <- String.drop_prefix source offset
          else (
            Buffer.add_char output source.[offset];
            loop (offset + 1)))
  in
  loop 0;
  Buffer.contents output
;;

let%test_unit "modified Enter is chunk-independent and paste stays literal" =
  let input = "a" ^ List.hd_exn shifted ^ "b" ^ List.last_exn shifted ^ "c" in
  let expected = "a\027\nb\027\nc" in
  for split = 0 to String.length input do
    let t = create () in
    let first = feed t (String.prefix input split) in
    let second = feed t (String.drop_prefix input split) in
    let output = first ^ second ^ flush_escape t in
    assert (String.equal output expected)
  done;
  let t = create () in
  let output =
    String.to_list input
    |> List.map ~f:(fun c -> feed t (String.of_char c))
    |> String.concat
  in
  assert (String.equal (output ^ flush_escape t) expected);
  let input = paste_start ^ input ^ paste_end in
  let t = create () in
  let output =
    String.to_list input
    |> List.map ~f:(fun c -> feed t (String.of_char c))
    |> String.concat
  in
  assert (String.equal (output ^ flush_escape t) input);
  let t = create () in
  assert (String.is_empty (feed t "\027"));
  assert (escape_pending t);
  assert (String.equal (flush_escape t) "\027");
  assert (not (escape_pending t));
  List.iter [ "\r"; "\n"; "\027[A"; "\027[13;5u"; "é" ] ~f:(fun input ->
    let t = create () in
    let output = feed t input in
    assert (String.equal (output ^ flush_escape t) input))
;;

let%test_unit "timeouts preserve fragmented paste markers and modified Enter" =
  (* The initial standalone ESC outside paste is ambiguous with an Escape key.
     Once another marker byte arrives, no timeout can discard its prefix. *)
  List.iter (paste_start :: shifted) ~f:(fun sequence ->
    for split = 2 to String.length sequence - 1 do
      let t = create () in
      let first = feed t (String.prefix sequence split) in
      assert (not (escape_pending t));
      assert (String.is_empty (flush_escape t));
      let second = feed t (String.drop_prefix sequence split) in
      let expected = if String.equal sequence paste_start then sequence else "\027\n" in
      assert (String.equal (first ^ second) expected)
    done);
  for split = 1 to String.length paste_end - 1 do
    let t = create () in
    let start = feed t paste_start in
    let partial_end = feed t (String.prefix paste_end split) in
    assert (not (escape_pending t));
    assert (String.is_empty (flush_escape t));
    let end_ = feed t (String.drop_prefix paste_end split) in
    let shifted_enter = feed t (List.hd_exn shifted) in
    assert (String.equal (start ^ partial_end ^ end_) (paste_start ^ paste_end));
    assert (String.equal shifted_enter "\027\n")
  done;
  let t = create () in
  assert (String.equal (feed t paste_start) paste_start);
  let output =
    String.to_list (List.hd_exn shifted ^ paste_end)
    |> List.map ~f:(fun c ->
      let output = feed t (String.of_char c) in
      assert (not (escape_pending t));
      output ^ flush_escape t)
    |> String.concat
  in
  assert (String.equal output (List.hd_exn shifted ^ paste_end));
  assert (String.equal (feed t (List.hd_exn shifted)) "\027\n")
;;
