open Core

let load name add registry =
  match add registry with
  | Ok () -> ()
  | Error error ->
    printf "failed to load %s grammar: %s\n" name (Error.to_string_hum error)
;;

let create () =
  let registry = Highlight_tm_loader.create_registry () in
  [ "OCaml", Highlight_grammars.add_ocaml
  ; "Python", Highlight_grammars.add_python
  ; "Rust", Highlight_grammars.add_rust
  ; "JavaScript", Highlight_grammars.add_javascript
  ; "TypeScript", Highlight_grammars.add_typescript
  ; "Dune", Highlight_grammars.add_dune
  ; "OPAM", Highlight_grammars.add_opam
  ; "Shell", Highlight_grammars.add_shell
  ; "Diff", Highlight_grammars.add_diff
  ; "ochat-apply-patch", Highlight_grammars.add_ochat_apply_patch
  ; "JSON", Highlight_grammars.add_json
  ; "HTML", Highlight_grammars.add_html
  ; "Markdown", Highlight_grammars.add_markdown
  ]
  |> List.iter ~f:(fun (name, add) -> load name add registry);
  registry
;;

let create_with_sources sources =
  let registry = create () in
  List.fold_result sources ~init:registry ~f:(fun registry source ->
    Highlight_tm_loader.add_grammar_jsonaf
      registry
      (Highlight_grammar_discovery.Source.json source)
    |> Result.map ~f:(fun () -> registry))
;;

let current = ref None
let current_generation = ref 0

let get () =
  match !current with
  | Some registry -> registry
  | None ->
    let registry = create () in
    current := Some registry;
    registry
;;

let replace registry =
  current := Some registry;
  Int.incr current_generation
;;

let generation () = !current_generation
