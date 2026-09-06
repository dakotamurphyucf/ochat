open! Core

let outside_fences text =
  let _, lines =
    String.split_lines text
    |> List.fold ~init:(false, []) ~f:(fun (inside, lines) line ->
      if String.is_prefix (String.strip line) ~prefix:"```"
      then not inside, lines
      else if inside
      then inside, lines
      else inside, line :: lines)
  in
  List.rev lines
;;

let heading_text title =
  let links = Re.Perl.compile_pat "!?\\[([^\\]]+)\\]\\([^)]*\\)" in
  let html = Re.Perl.compile_pat "<[^>]+>" in
  Re.replace links ~f:(fun group -> Re.Group.get group 1) title
  |> Re.replace_string html ~by:""
  |> String.strip
;;

let slug title =
  heading_text title
  |> String.lowercase
  |> String.filter_map ~f:(fun char ->
    if Char.is_whitespace char
    then Some '-'
    else if
      Char.is_alphanum char
      || Char.equal char '-'
      || Char.equal char '_'
      || Char.to_int char >= 128
    then Some char
    else None)
;;

let atx_heading line =
  let pattern = Re.Perl.compile_pat "^ {0,3}#{1,6}[ \\t]+(.*)$" in
  let closing = Re.Perl.compile_pat "[ \\t]+#+[ \\t]*$" in
  Option.map (Re.exec_opt pattern line) ~f:(fun group ->
    Re.Group.get group 1 |> Re.replace_string closing ~by:"")
;;

let setext_underline line =
  let stripped = String.strip line in
  (not (String.is_empty stripped))
  && (String.for_all stripped ~f:(Char.equal '=')
      || String.for_all stripped ~f:(Char.equal '-'))
;;

let headings text =
  outside_fences text
  |> List.fold ~init:(None, []) ~f:(fun (previous, titles) line ->
    match atx_heading line with
    | Some title -> None, title :: titles
    | None when setext_underline line ->
      None, Option.value_map previous ~default:titles ~f:(fun title -> title :: titles)
    | None ->
      let previous = if String.is_empty (String.strip line) then None else Some line in
      previous, titles)
  |> snd
  |> List.rev
;;

let anchors text =
  let counts = String.Table.create () in
  headings text
  |> List.map ~f:(fun title ->
    let base = slug title in
    let count = Hashtbl.find counts base |> Option.value ~default:0 in
    Hashtbl.set counts ~key:base ~data:(count + 1);
    if count = 0 then base else base ^ "-" ^ Int.to_string count)
;;

let html_images text =
  let pattern = Re.Perl.compile_pat "<img[^>]*src=\"([^\"]+)\"" in
  let inline = Re.(compile (seq [ char '`'; rep (compl [ char '`' ]); char '`' ])) in
  String.concat_lines (outside_fences text)
  |> Re.replace_string inline ~by:""
  |> Re.all pattern
  |> List.map ~f:(fun group -> Re.Group.get group 1)
;;

let is_valid_anchor text anchor =
  List.mem (anchors text) anchor ~equal:String.equal
  || String.is_substring text ~substring:("id=\"" ^ anchor ^ "\"")
  || String.is_substring text ~substring:("name=\"" ^ anchor ^ "\"")
;;
