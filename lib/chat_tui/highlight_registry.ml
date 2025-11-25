open Core

let reg_lazy : Highlight_tm_loader.registry Lazy.t =
  lazy
    (let r = Highlight_tm_loader.create_registry () in
     (match Highlight_grammars.add_ocaml r with
      | Ok () -> ()
      | Error e -> printf "failed to load OCaml grammar: %s\n" (Error.to_string_hum e));
     (match Highlight_grammars.add_dune r with
      | Ok () -> ()
      | Error e -> printf "failed to load Dune grammar: %s\n" (Error.to_string_hum e));
     (match Highlight_grammars.add_opam r with
      | Ok () -> ()
      | Error e -> printf "failed to load OPAM grammar: %s\n" (Error.to_string_hum e));
     (match Highlight_grammars.add_shell r with
      | Ok () -> ()
      | Error e -> printf "failed to load Shell grammar: %s\n" (Error.to_string_hum e));
     (match Highlight_grammars.add_diff r with
      | Ok () -> ()
      | Error e -> printf "failed to load Diff grammar: %s\n" (Error.to_string_hum e));
     (match Highlight_grammars.add_json r with
      | Ok () -> ()
      | Error e -> printf "failed to load JSON grammar: %s\n" (Error.to_string_hum e));
     (match Highlight_grammars.add_html r with
      | Ok () -> ()
      | Error e -> printf "failed to load HTML grammar: %s\n" (Error.to_string_hum e));
     (match Highlight_grammars.add_markdown r with
      | Ok () -> ()
      | Error e -> printf "failed to load Markdown grammar: %s\n" (Error.to_string_hum e));
     r)
;;

let get () = Lazy.force reg_lazy
