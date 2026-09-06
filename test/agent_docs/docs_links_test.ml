open! Core

let run () =
  let text =
    "# Start\n\n\
     Quick start\n\
     -----------\n\n\
     ### [`create`](../../lib/example.mli)\n\n\
     ## Details <a id=\"stable-details\"></a>\n\n\
     ## Repeated\n\n\
     ## Repeated\n\n\
     ## Closing ###\n\n\
     ```ocaml\n\
     # Not a heading\n\
     ```\n"
  in
  let expected =
    [ "start"; "quick-start"; "create"; "details"; "repeated"; "repeated-1"; "closing" ]
  in
  if not (List.equal String.equal (Docs_links.anchors text) expected)
  then failwith "Markdown heading extraction regression";
  if not (Docs_links.is_valid_anchor text "stable-details")
  then failwith "explicit Markdown anchor regression";
  if Docs_links.is_valid_anchor text "missing-heading"
  then failwith "invalid Markdown anchor accepted"
;;
