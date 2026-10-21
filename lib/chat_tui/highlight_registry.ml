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
  ; "ChatML", Highlight_grammars.add_chatml
  ; "ChatMD", Highlight_grammars.add_chatmd
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

module Baseline = struct
  type t =
    { registry : Highlight_tm_loader.registry
    ; chatml : TmLanguage.grammar option
    ; chatmd : TmLanguage.grammar option
    }

  let capture registry =
    { registry
    ; chatml = Highlight_tm_loader.find_grammar_by_lang_tag registry "chatml"
    ; chatmd = Highlight_tm_loader.find_grammar_by_lang_tag registry "chatmd"
    }
  ;;

  let uses_bundled_grammar t engine ~language =
    if not (Highlight_tm_engine.uses_registry engine ~registry:t.registry)
    then false
    else (
      let hint, expected =
        match language with
        | `Chatml -> "chatml", t.chatml
        | `Chatmd -> "chatmd", t.chatmd
      in
      match expected, Highlight_tm_loader.find_grammar_by_lang_tag t.registry hint with
      | Some expected, Some current -> phys_equal expected current
      | None, _ | _, None -> false)
  ;;
end

let create_with_baseline () =
  let registry = create () in
  registry, Baseline.capture registry
;;

let create_with_sources_and_baseline sources =
  let registry, baseline = create_with_baseline () in
  List.fold_result sources ~init:() ~f:(fun () source ->
    Highlight_tm_loader.add_grammar_jsonaf
      registry
      (Highlight_grammar_discovery.Source.json source)
    |> Result.map ~f:(fun () -> ()))
  |> Result.map ~f:(fun () -> registry, baseline)
;;

let create_with_sources sources =
  create_with_sources_and_baseline sources |> Result.map ~f:fst
;;

let current = ref None
let current_generation = ref 0

let get () =
  match !current with
  | Some (registry, _) -> registry
  | None ->
    let registry, baseline = create_with_baseline () in
    current := Some (registry, Some baseline);
    registry
;;

let current_baseline () = Option.bind !current ~f:snd

let replace ?baseline registry =
  Option.iter baseline ~f:(fun baseline ->
    if not (phys_equal registry baseline.Baseline.registry)
    then invalid_arg "Highlight_registry.replace: baseline belongs to another registry");
  current := Some (registry, baseline);
  Int.incr current_generation
;;

let generation () = !current_generation
