open Core
module Captured = Chatml_source

let max_bytes = 16 * 1024
let max_related = 8
let bounded text = String.prefix text max_bytes

module Site = struct
  type t =
    { source : Captured.t
    ; location : Captured.Location.t
    }

  let create source ~span ~checkpoint =
    Captured.of_span source span ~checkpoint
    |> Or_error.map ~f:(fun location -> { source; location })
  ;;

  let source t = t.source
  let location t = t.location
  let sexp_of_t t = Captured.Location.sexp_of_t t.location
end

module Related = struct
  type t =
    { site : Site.t
    ; message : string
    }
  [@@deriving sexp_of]

  let create site ~message = { site; message = bounded message }
  let site t = t.site
  let message t = t.message
end

type t =
  { message : string
  ; span : Source.span option
  ; site : Site.t option
  ; related : Related.t list
  }
[@@deriving sexp_of]

let create ?span ?site ?(related = []) message =
  let span =
    match site with
    | Some site -> Some (Captured.Location.span (Site.location site))
    | None -> span
  in
  { message = bounded message; span; site; related = List.take related max_related }
;;

let with_fallback t site =
  match t.site, site with
  | Some _, _ | None, None -> t
  | None, Some site ->
    { t with site = Some site; span = Some (Captured.Location.span (Site.location site)) }
;;

let source_label source ~checkpoint =
  let buffer = Buffer.create 128 in
  let component text =
    let length = Int.min 512 (String.length text) in
    for index = 0 to length - 1 do
      if index mod 64 = 0 then checkpoint ();
      let char = String.get text index in
      match char with
      | ' ' .. '~' when not (Char.equal char '\\') -> Buffer.add_char buffer char
      | _ -> Buffer.add_string buffer (sprintf "\\%03d" (Char.to_int char))
    done;
    if length < String.length text then Buffer.add_string buffer "..."
  in
  let id = Captured.id source in
  component (Captured.Id.package_namespace id);
  Buffer.add_char buffer ':';
  component (Captured.Id.relative_path id);
  Buffer.contents buffer
;;

let excerpt contents span ~checkpoint =
  let available = span.Source.right.offset - span.left.offset in
  let length = Int.min available 4096 in
  let text = String.sub contents ~pos:span.left.offset ~len:length in
  let escaped =
    String.exists text ~f:(function
      | ' ' .. '~' -> false
      | _ -> true)
  in
  if not escaped
  then text, length < available, false
  else (
    let buffer = Buffer.create 1024 in
    let rec loop index =
      if index mod 64 = 0 then checkpoint ();
      if index = length
      then length < available
      else (
        let piece =
          match text.[index] with
          | '\n' -> "\\n"
          | '\r' -> "\\r"
          | '\t' -> "\\t"
          | '\\' -> "\\\\"
          | ' ' .. '~' as char -> String.of_char char
          | char -> sprintf "\\%03d" (Char.to_int char)
        in
        if Buffer.length buffer + String.length piece > 4096
        then true
        else (
          Buffer.add_string buffer piece;
          loop (index + 1)))
    in
    let truncated = loop 0 in
    Buffer.contents buffer, truncated, true)
;;

let render ?(show_source = true) t ~label ~checkpoint =
  checkpoint ();
  let buffer = Buffer.create 1024 in
  let truncated = ref false in
  let append text =
    let remaining = max_bytes - Buffer.length buffer in
    let length = Int.min remaining (String.length text) in
    if length < String.length text then truncated := true;
    Buffer.add_substring buffer text ~pos:0 ~len:length
  in
  let site site =
    checkpoint ();
    let source = Site.source site in
    let span = Captured.Location.span (Site.location site) in
    if show_source
    then (
      append (source_label source ~checkpoint);
      append ": ");
    if Int.equal span.left.line span.right.line
    then
      append
        (sprintf
           "line %i, characters %i-%i:\n%i|    "
           span.left.line
           span.left.column
           span.right.column
           span.left.line)
    else
      append
        (sprintf
           "line %i, column %i to line %i, column %i:\n%i|    "
           span.left.line
           span.left.column
           span.right.line
           span.right.column
           span.left.line);
    let preview, shortened, escaped =
      excerpt (Captured.contents source) span ~checkpoint
    in
    append preview;
    if shortened then append "... [excerpt truncated]";
    if escaped
    then append "\n      (escaped byte preview; coordinates refer to source bytes)"
    else if Int.equal span.left.line span.right.line
    then (
      append (String.make (Int.min 80 span.left.column + 3) ' ');
      append "\n      ";
      append
        (String.make (Int.min 80 (Int.max 1 (span.right.column - span.left.column))) '^'));
    append "\n\n"
  in
  Option.iter t.site ~f:site;
  append label;
  append ": ";
  append t.message;
  List.iter t.related ~f:(fun related ->
    checkpoint ();
    append "\nRelated: ";
    append (Related.message related);
    append "\n";
    site (Related.site related));
  checkpoint ();
  let text = Buffer.contents buffer in
  if !truncated
  then (
    let marker = "... [truncated]" in
    String.prefix text (max_bytes - String.length marker) ^ marker)
  else text
;;

let render_legacy t ~source ~label ~checkpoint =
  match t.site with
  | Some _ -> render t ~label ~checkpoint
  | None ->
    let site =
      Option.bind t.span ~f:(fun span ->
        let id =
          Captured.Id.create
            ~package_namespace:"ochat.single-file.v1"
            ~relative_path:"main.chatml"
          |> Or_error.ok_exn
        in
        let source =
          Captured.create
            id
            ~contents:source
            ~expected_digest:None
            ~max_bytes:None
            ~checkpoint
          |> Or_error.ok_exn
        in
        Site.create source ~span ~checkpoint |> Result.ok)
    in
    render ~show_source:false (with_fallback t site) ~label ~checkpoint
;;
