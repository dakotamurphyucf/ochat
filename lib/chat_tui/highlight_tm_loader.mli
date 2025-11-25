(** Load and look up TextMate grammars.

    A registry is a collection of TextMate grammars, backed by the
    [textmate-language] library. This module provides helpers to:
    {ul
    {- construct a fresh registry}
    {- add grammars from Jsonaf values or JSON files}
    {- resolve a grammar by a language tag or a Markdown info string}}

    Invariants
    {ul
    {- A grammar must be added to the registry before it can be looked up.}
    {- Language tags are matched case-insensitively and expanded to a small
       set of common aliases (e.g. ["ocaml"], ["ml"], ["mli"]).}}

    See also: {!module:Highlight_tm_engine} for turning grammars into
    highlighted spans and {!module:Highlight_theme} for theming. *)

type registry = TmLanguage.t

(** [create_registry ()] creates an empty grammar registry.

    Example – start a registry to load grammars into:
    {[
      let reg = Chat_tui.Highlight_tm_loader.create_registry ()
    ]} *)
val create_registry : unit -> registry

(** [add_grammar_jsonaf reg json] parses a TextMate grammar from [json]
    and adds it to [reg].

    Numbers in [Jsonaf] are strings; the function interprets them as
    integers when possible and as floats otherwise.

    @return [Ok ()] on success, or an [Error] if the JSON cannot be converted
    to a valid grammar.

    Example – add a grammar already parsed with Jsonaf:
    {[
      let reg = Chat_tui.Highlight_tm_loader.create_registry () in
      let json = Jsonaf.of_string "{\"scopeName\":\"source.ocaml\"}" in
      let (_ : unit Core.Or_error.t) =
        Chat_tui.Highlight_tm_loader.add_grammar_jsonaf reg json
      in
      ()
    ]} *)
val add_grammar_jsonaf : registry -> Jsonaf.t -> unit Core.Or_error.t

(** [add_grammar_jsonaf_file reg ~path] reads [path], parses it as JSON via
    {!Jsonaf.parse}, converts it to a TextMate grammar, and adds it to [reg].

    @return [Ok ()] on success, or an [Error] if the file cannot be read or the
    contents are not a valid grammar.

    Example – load a grammar file shipped with the application:
    {[
      let reg = Chat_tui.Highlight_tm_loader.create_registry () in
      match
        Chat_tui.Highlight_tm_loader.add_grammar_jsonaf_file reg ~path:"ocaml.tmLanguage.json"
      with
      | Ok () -> ()
      | Error e -> Core.eprintf "grammar load failed: %s\n" (Core.Error.to_string_hum e)
    ]} *)
val add_grammar_jsonaf_file : registry -> path:string -> unit Core.Or_error.t

(** [find_grammar_by_lang_tag reg lang] looks up a grammar in [reg] using a
    language tag like ["ocaml"], ["bash"], ["diff"], etc.

    Matching is case-insensitive. The function tries a small set of aliases for
    common languages and then queries the registry by:
    {ol
    {- display name}
    {- scope name (e.g. ["source.ocaml"]) }
    {- filetype/extension}}

    @return [Some grammar] if a match is found, otherwise [None].

    Example – resolve the OCaml grammar after loading it:
    {[
      let reg = Chat_tui.Highlight_tm_loader.create_registry () in
      let (_ : unit Core.Or_error.t) =
        Chat_tui.Highlight_tm_loader.add_grammar_jsonaf_file reg ~path:"ocaml.tmLanguage.json"
      in
      Core.Option.is_some
        (Chat_tui.Highlight_tm_loader.find_grammar_by_lang_tag reg "ocaml")
      = true
    ]} *)
val find_grammar_by_lang_tag : registry -> string -> TmLanguage.grammar option

(** [find_grammar_for_info_string reg info] extracts the first word from a
    Markdown code fence info string (e.g. [Some "ocaml linenos"]) and looks up
    the corresponding grammar via {!find_grammar_by_lang_tag}.

    Blank or [None] values return [None].

    Example – resolve from a fence header:
    {[
      let reg = Chat_tui.Highlight_tm_loader.create_registry () in
      let (_ : unit Core.Or_error.t) =
        Chat_tui.Highlight_tm_loader.add_grammar_jsonaf_file reg ~path:"ocaml.tmLanguage.json"
      in
      match Chat_tui.Highlight_tm_loader.find_grammar_for_info_string reg (Some "ocaml linenos") with
      | Some _ -> ()
      | None -> failwith "missing grammar"
    ]} *)
val find_grammar_for_info_string : registry -> string option -> TmLanguage.grammar option
