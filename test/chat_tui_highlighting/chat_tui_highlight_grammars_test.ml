open Core

let%expect_test "bundled grammars load and resolve" =
  let open Chat_tui in
  let registry = Highlight_tm_loader.create_registry () in
  Highlight_grammars.add_ocaml registry |> Or_error.ok_exn;
  Highlight_grammars.add_python registry |> Or_error.ok_exn;
  Highlight_grammars.add_rust registry |> Or_error.ok_exn;
  Highlight_grammars.add_javascript registry |> Or_error.ok_exn;
  Highlight_grammars.add_typescript registry |> Or_error.ok_exn;
  Highlight_grammars.add_html registry |> Or_error.ok_exn;
  Highlight_grammars.add_markdown registry |> Or_error.ok_exn;
  let is_registered lang =
    Highlight_tm_loader.find_grammar_by_lang_tag registry lang |> Option.is_some
  in
  [ "ocaml"
  ; "python"
  ; "py"
  ; "rust"
  ; "rs"
  ; "javascript"
  ; "js"
  ; "jsx"
  ; "typescript"
  ; "ts"
  ; "tsx"
  ; "html"
  ; "htm"
  ; "markdown"
  ]
  |> List.iter ~f:(fun lang -> printf "%s=%b\n" lang (is_registered lang));
  [%expect
    {|
    ocaml=true
    python=true
    py=true
    rust=true
    rs=true
    javascript=true
    js=true
    jsx=true
    typescript=true
    ts=true
    tsx=true
    html=true
    htm=true
    markdown=true
    |}]
;;

let%expect_test "bundled language grammars tokenize representative source" =
  let open Chat_tui in
  let registry = Highlight_tm_loader.create_registry () in
  Highlight_grammars.add_python registry |> Or_error.ok_exn;
  Highlight_grammars.add_rust registry |> Or_error.ok_exn;
  Highlight_grammars.add_javascript registry |> Or_error.ok_exn;
  Highlight_grammars.add_typescript registry |> Or_error.ok_exn;
  Highlight_grammars.add_html registry |> Or_error.ok_exn;
  let engine =
    Highlight_tm_engine.create ~theme:Highlight_theme.github_dark
    |> Highlight_tm_engine.with_registry ~registry
  in
  [ "python", "def greet(name): return f\"hello {name}\""
  ; "rust", "fn main() { let value: i32 = 42; }"
  ; "javascript", "const greet = (name) => `hello ${name}`;"
  ; "jsx", "const view = <Button title=\"Save\">{label}</Button>;"
  ; "typescript", "const value: number = 42;"
  ; "tsx", "const view: JSX.Element = <Button>{label}</Button>;"
  ; "html", {|<main class="content"><strong>Hello</strong></main>|}
  ]
  |> List.iter ~f:(fun (lang, text) ->
    let _, info =
      Highlight_tm_engine.highlight_text_with_info engine ~lang:(Some lang) ~text
    in
    printf "%s fallback=%b\n" lang (Option.is_some info.fallback));
  [%expect
    {|
    python fallback=false
    rust fallback=false
    javascript fallback=false
    jsx fallback=false
    typescript fallback=false
    tsx fallback=false
    html fallback=false
    |}]
;;

let%expect_test "JSON and apply-patch grammars expose semantic scopes and styles" =
  let open Chat_tui in
  let registry = Highlight_tm_loader.create_registry () in
  Highlight_grammars.add_diff registry |> Or_error.ok_exn;
  Highlight_grammars.add_json registry |> Or_error.ok_exn;
  Highlight_grammars.add_ochat_apply_patch registry |> Or_error.ok_exn;
  let engine =
    Highlight_tm_engine.create ~theme:Highlight_theme.github_dark
    |> Highlight_tm_engine.with_registry ~registry
  in
  let inspect ~lang ~text prefixes =
    let lines, info =
      Highlight_tm_engine.highlight_text_with_scopes_with_info
        engine
        ~lang:(Some lang)
        ~text
    in
    let spans = List.concat lines in
    printf "%s fallback=%b\n" lang (Option.is_some info.fallback);
    List.iter prefixes ~f:(fun prefix ->
      let matching =
        List.filter spans ~f:(fun { Highlight_tm_engine.scopes; _ } ->
          List.exists scopes ~f:(String.is_prefix ~prefix))
      in
      printf
        "%s scope=%b styled=%b\n"
        prefix
        (not (List.is_empty matching))
        (List.exists matching ~f:(fun { Highlight_tm_engine.attr; _ } ->
           not (Notty.A.equal attr Notty.A.empty))))
  in
  inspect
    ~lang:"json"
    ~text:{|{"name":"value","count":12,"ready":true,"missing":null}|}
    [ "entity.name.type.json"
    ; "string.quoted.double.json"
    ; "constant.numeric.json"
    ; "constant.language.json"
    ; "punctuation.separator.json"
    ];
  inspect
    ~lang:"ochat-apply-patch"
    ~text:
      "*** Begin Patch\n\
       *** Update File: lib/x.ml\n\
       *** Move to: lib/y.ml\n\
       @@ let f\n\
       -old\n\
       +new\n\
      \ context\n\
       *** End Patch"
    [ "meta.ochatpatch.boundary.begin"
    ; "entity.name.filename.ochatpatch"
    ; "meta.ochatpatch.change-context"
    ; "markup.deleted.ochatpatch"
    ; "markup.inserted.ochatpatch"
    ; "text.diff.context.ochatpatch"
    ; "meta.ochatpatch.boundary.end"
    ];
  [%expect
    {|
    json fallback=false
    entity.name.type.json scope=true styled=true
    string.quoted.double.json scope=true styled=true
    constant.numeric.json scope=true styled=true
    constant.language.json scope=true styled=true
    punctuation.separator.json scope=true styled=true
    ochat-apply-patch fallback=false
    meta.ochatpatch.boundary.begin scope=true styled=true
    entity.name.filename.ochatpatch scope=true styled=true
    meta.ochatpatch.change-context scope=true styled=true
    markup.deleted.ochatpatch scope=true styled=true
    markup.inserted.ochatpatch scope=true styled=true
    text.diff.context.ochatpatch scope=true styled=true
    meta.ochatpatch.boundary.end scope=true styled=true
    |}]
;;

let%expect_test "bundled HTML grammar recognizes tags and attributes" =
  let open Chat_tui in
  let registry = Highlight_tm_loader.create_registry () in
  Highlight_grammars.add_html registry |> Or_error.ok_exn;
  let engine =
    Highlight_tm_engine.create ~theme:Highlight_theme.github_dark
    |> Highlight_tm_engine.with_registry ~registry
  in
  let lines, info =
    Highlight_tm_engine.highlight_text_with_scopes_with_info
      engine
      ~lang:(Some "html")
      ~text:{|<main class="content">Hello &amp;</main>|}
  in
  let spans = List.concat lines in
  let reconstructed =
    List.map lines ~f:(fun line ->
      line |> List.map ~f:(fun { Highlight_tm_engine.text; _ } -> text) |> String.concat)
    |> String.concat ~sep:"\n"
  in
  let has_scope prefix =
    List.exists spans ~f:(fun { Highlight_tm_engine.scopes; _ } ->
      List.exists scopes ~f:(String.is_prefix ~prefix))
  in
  printf "fallback=%b\n" (Option.is_some info.fallback);
  printf
    "reconstructs-input=%b\n"
    (String.equal reconstructed {|<main class="content">Hello &amp;</main>|});
  [ "entity.name.tag"
  ; "entity.other.attribute-name"
  ; "string.quoted.double"
  ; "constant.character.entity"
  ; "punctuation.definition.tag"
  ]
  |> List.iter ~f:(fun prefix -> printf "%s=%b\n" prefix (has_scope prefix));
  [%expect
    {|
    fallback=false
    reconstructs-input=true
    entity.name.tag=true
    entity.other.attribute-name=true
    string.quoted.double=true
    constant.character.entity=true
    punctuation.definition.tag=true
    |}]
;;

let%expect_test "VS Code grammar scopes receive semantic theme styles" =
  let open Chat_tui in
  let registry = Highlight_tm_loader.create_registry () in
  Highlight_grammars.add_python registry |> Or_error.ok_exn;
  Highlight_grammars.add_rust registry |> Or_error.ok_exn;
  Highlight_grammars.add_javascript registry |> Or_error.ok_exn;
  Highlight_grammars.add_typescript registry |> Or_error.ok_exn;
  let engine =
    Highlight_tm_engine.create ~theme:Highlight_theme.github_dark
    |> Highlight_tm_engine.with_registry ~registry
  in
  let has_styled_scope ~lang ~text ~scope_prefix ~expected =
    let lines, info =
      Highlight_tm_engine.highlight_text_with_scopes_with_info
        engine
        ~lang:(Some lang)
        ~text
    in
    Option.is_none info.fallback
    && List.concat lines
       |> List.exists ~f:(fun { Highlight_tm_engine.attr; scopes; _ } ->
         List.exists scopes ~f:(String.is_prefix ~prefix:scope_prefix)
         && Notty.A.equal attr expected)
  in
  let cases =
    [ ( "python declaration"
      , has_styled_scope
          ~lang:"python"
          ~text:"def greet(): pass"
          ~scope_prefix:"storage."
          ~expected:(Highlight_styles.fg_hex "#F97583") )
    ; ( "rust declaration"
      , has_styled_scope
          ~lang:"rust"
          ~text:"fn main() { let mut value = 1; }"
          ~scope_prefix:"storage."
          ~expected:(Highlight_styles.fg_hex "#F97583") )
    ; ( "javascript declaration"
      , has_styled_scope
          ~lang:"javascript"
          ~text:"async function greet() {}"
          ~scope_prefix:"storage."
          ~expected:(Highlight_styles.fg_hex "#F97583") )
    ; ( "typescript declaration"
      , has_styled_scope
          ~lang:"typescript"
          ~text:"interface Item { readonly value: number }"
          ~scope_prefix:"storage."
          ~expected:(Highlight_styles.fg_hex "#F97583") )
    ; ( "tsx component"
      , has_styled_scope
          ~lang:"tsx"
          ~text:"const view = <Button />;"
          ~scope_prefix:"support.class.component"
          ~expected:(Highlight_styles.fg_hex "#B392F0") )
    ]
  in
  List.iter cases ~f:(fun (name, styled) -> printf "%s=%b\n" name styled);
  [%expect
    {|
    python declaration=true
    rust declaration=true
    javascript declaration=true
    typescript declaration=true
    tsx component=true
    |}]
;;

let%expect_test "VS Code grammar bundles register companion scopes" =
  let open Chat_tui in
  let registry = Highlight_tm_loader.create_registry () in
  Highlight_grammars.add_python registry |> Or_error.ok_exn;
  Highlight_grammars.add_rust registry |> Or_error.ok_exn;
  Highlight_grammars.add_javascript registry |> Or_error.ok_exn;
  Highlight_grammars.add_typescript registry |> Or_error.ok_exn;
  [ "source.python"
  ; "source.regexp.python"
  ; "source.rust"
  ; "source.js"
  ; "source.js.jsx"
  ; "source.ts"
  ; "source.tsx"
  ]
  |> List.iter ~f:(fun scope ->
    let found = TmLanguage.find_by_scope_name registry scope |> Option.is_some in
    printf "%s=%b\n" scope found);
  [%expect
    {|
    source.python=true
    source.regexp.python=true
    source.rust=true
    source.js=true
    source.js.jsx=true
    source.ts=true
    source.tsx=true
    |}]
;;
