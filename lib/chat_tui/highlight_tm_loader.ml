open Core
include Terminal_highlighting.Highlight_tm_loader

(* Preserve the legacy path-based helper. New frontends load through their
   Eio filesystem capability and pass decoded JSON to the shared loader. *)
let add_grammar_jsonaf_file registry ~path =
  let open Or_error.Let_syntax in
  let%bind contents = Or_error.try_with (fun () -> In_channel.read_all path) in
  let%bind json = Jsonaf.parse contents in
  add_grammar_jsonaf registry json
;;
