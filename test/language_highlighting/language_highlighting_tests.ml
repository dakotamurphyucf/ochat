open Core
module Highlight = Language_highlighting.Highlight_snapshot
module Terminal = Terminal_highlighting

let limits ?(work = 100_000) () =
  Highlight.Limits.create
    ~max_source_bytes:16_384
    ~max_identity_bytes:4096
    ~max_spans:4096
    ~max_issues:32
    ~max_work_units:work
    ~max_nesting:64
  |> Or_error.ok_exn
;;

let chatml_source text =
  let id =
    Chatml_source.Id.create ~package_namespace:"test" ~relative_path:"module.chatml"
    |> Or_error.ok_exn
  in
  Chatml_source.create
    id
    ~contents:text
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let chatmd_input ~authored ~effective =
  Chatmd_parser_input.create
    ~role:Root
    ~path:"prompt.chatmd"
    ~authored
    ~effective
    ~max_input_bytes:16_384
    ~checkpoint:ignore
  |> Result.ok
  |> Option.value_exn
;;

let displayed snapshot =
  let text = Highlight.source_contents snapshot in
  Highlight.spans snapshot
  |> List.map ~f:(fun span ->
    let range = Highlight.Span.range span in
    ( Highlight.Span.role span
    , String.sub
        text
        ~pos:(Highlight.Range.start_byte range)
        ~len:(Highlight.Range.end_byte range - Highlight.Range.start_byte range) ))
;;

let%expect_test "selected ChatML lexical vocabulary follows compiler token boundaries" =
  let text = "let* total = 1 + 2 ;; module M = struct let value = `Ready end" in
  let snapshot =
    Highlight.scan_chatml (chatml_source text) ~limits:(limits ()) ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let scanner_bounds =
    Highlight.spans snapshot
    |> List.map ~f:(fun span ->
      let range = Highlight.Span.range span in
      Highlight.Range.start_byte range, Highlight.Range.end_byte range)
  in
  let lexbuf = Lexing.from_string text in
  let rec compiler_tokens acc =
    match Chatml.Chatml_lexer.token lexbuf with
    | Chatml.Chatml_tokens.EOF -> List.rev acc
    | _ -> compiler_tokens ((Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) :: acc)
  in
  let compiler_bounds = compiler_tokens [] in
  assert (
    List.equal
      (fun (a, b) (c, d) -> Int.equal a c && Int.equal b d)
      scanner_bounds
      compiler_bounds);
  print_s [%sexp (displayed snapshot : (Highlight.Role.t * string) list)];
  [%expect
    {|
    ((Keyword let*) (Identifier total) (Operator =) (Number_literal 1)
     (Operator +) (Number_literal 2) (Punctuation ";;") (Keyword module)
     (Identifier M) (Operator =) (Keyword struct) (Keyword let)
     (Identifier value) (Operator =) (Variant_tag `Ready) (Keyword end))
    |}]
;;

let%expect_test "nested comments, CRLF, incomplete strings and bounded prefix" =
  let text = "(* outer (* inner *) done *)\r\nlet value = \"open" in
  let snapshot =
    Highlight.scan_chatml (chatml_source text) ~limits:(limits ()) ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  print_s [%sexp (displayed snapshot : (Highlight.Role.t * string) list)];
  print_s
    [%sexp
      (List.map (Highlight.issues snapshot) ~f:Highlight.Issue.kind
       : Highlight.Issue.kind list)];
  [%expect
    {|
    ((Comment "(* outer (* inner *) done *)") (Keyword let) (Identifier value)
     (Operator =) (String_literal "\"open"))
    (Open_string) |}];
  let long = String.make 10_000 'a' in
  let bounded =
    Highlight.scan_chatml
      (chatml_source long)
      ~limits:(limits ~work:500 ())
      ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  print_s [%sexp (Highlight.status bounded : Highlight.Status.t)];
  assert (
    List.for_all (Highlight.spans bounded) ~f:(fun span ->
      Highlight.Range.end_byte (Highlight.Span.range span) <= 0));
  [%expect {| (Truncated (next_byte 0) (reason Work_limit)) |}]
;;

let%expect_test "ChatMD views, script source spelling, and snapshot membership" =
  let authored =
    "<developer>Hi</developer>\r\n<script language=\"chatml\">let x = 1</script>"
  in
  let effective = "<developer>Prepared</developer>" in
  let input = chatmd_input ~authored ~effective in
  let authored_snapshot =
    Highlight.scan_chatmd input ~view:Authored ~limits:(limits ()) ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let effective_snapshot =
    Highlight.scan_chatmd input ~view:Effective ~limits:(limits ()) ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let first = List.hd_exn (Highlight.spans authored_snapshot) in
  assert (Highlight.contains_span authored_snapshot first);
  assert (not (Highlight.contains_span effective_snapshot first));
  let repeated_snapshot =
    Highlight.scan_chatmd input ~view:Authored ~limits:(limits ()) ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  assert (not (Highlight.contains_span repeated_snapshot first));
  print_s [%sexp (displayed authored_snapshot : (Highlight.Role.t * string) list)];
  [%expect
    {|
    ((Punctuation <) (Tag_name developer) (Punctuation >) (Punctuation </)
     (Tag_name developer) (Punctuation >) (Punctuation <) (Tag_name script)
     (Attribute_name language) (Attribute_value "\"chatml\"") (Punctuation >)
     (Keyword let) (Identifier x) (Operator =) (Number_literal 1)
     (Punctuation </) (Tag_name script) (Punctuation >))
    |}];
  List.iter
    [ "<script language=\"python\">let x = 1</script>"
    ; "<script language=\"chatml\" src=\"other.chatml\">let x = 1</script>"
    ]
    ~f:(fun authored ->
      let snapshot =
        Highlight.scan_chatmd
          (chatmd_input ~authored ~effective:authored)
          ~view:Authored
          ~limits:(limits ())
          ~checkpoint:ignore
        |> Result.ok
        |> Option.value_exn
      in
      assert (
        List.exists (Highlight.spans snapshot) ~f:(fun span ->
          Highlight.Role.equal (Highlight.Span.role span) Script_body));
      assert (
        not
          (List.exists (Highlight.spans snapshot) ~f:(fun span ->
             Highlight.Role.equal (Highlight.Span.role span) Keyword))))
;;

let%expect_test
    "RAW inside a script owns its close-tag spelling and later ChatML remains lexical"
  =
  let source =
    "<script language=chatml>let first = 1 RAW|</script>|RAW let second = 2</script >"
  in
  let snapshot =
    Highlight.scan_chatmd
      (chatmd_input ~authored:source ~effective:source)
      ~view:Authored
      ~limits:(limits ())
      ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  print_s [%sexp (displayed snapshot : (Highlight.Role.t * string) list)];
  print_s
    [%sexp
      (Highlight.issues snapshot |> List.map ~f:Highlight.Issue.kind
       : Highlight.Issue.kind list)];
  [%expect
    {|
    ((Punctuation <) (Tag_name script) (Attribute_name language)
     (Attribute_value chatml) (Punctuation >) (Keyword let) (Identifier first)
     (Operator =) (Number_literal 1) (Raw_delimiter RAW|) (Raw_text </script>)
     (Raw_delimiter |RAW) (Keyword let) (Identifier second) (Operator =)
     (Number_literal 2) (Punctuation </) (Tag_name script) (Punctuation >))
    ()
    |}]
;;

let%expect_test "overlapping open script and string issues obey the issue cap" =
  let source = "<script language=chatml>\"open" in
  let limits =
    Highlight.Limits.create
      ~max_source_bytes:128
      ~max_identity_bytes:4096
      ~max_spans:32
      ~max_issues:1
      ~max_work_units:20_000
      ~max_nesting:8
    |> Or_error.ok_exn
  in
  let snapshot =
    Highlight.scan_chatmd
      (chatmd_input ~authored:source ~effective:source)
      ~view:Authored
      ~limits
      ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  print_s [%sexp (Highlight.status snapshot : Highlight.Status.t)];
  print_s
    [%sexp
      (Highlight.issues snapshot |> List.map ~f:Highlight.Issue.kind
       : Highlight.Issue.kind list)];
  [%expect
    {|
    (Truncated (next_byte 29) (reason Issue_limit))
    (Open_string)
    |}]
;;

let%expect_test "installed ChatML and ChatMD TextMate assets and aliases" =
  let registry = Terminal.Highlight_tm_loader.create_registry () in
  Terminal.Highlight_grammars.add_chatmd registry |> Or_error.ok_exn;
  let found lang =
    Terminal.Highlight_tm_loader.find_grammar_by_lang_tag registry lang |> Option.is_some
  in
  print_s [%sexp (List.map [ "chatml"; "chatmli"; "chatmd" ] ~f:found : bool list)];
  let engine =
    Terminal.Highlight_tm_engine.create ~theme:Terminal.Highlight_theme.github_dark
    |> Terminal.Highlight_tm_engine.with_registry ~registry
  in
  let lines, info =
    Terminal.Highlight_tm_engine.highlight_text_with_scopes_with_info
      engine
      ~lang:(Some "chatmd")
      ~text:"<script language=\"chatml\">let x = 1</script>"
  in
  let scopes =
    List.concat lines
    |> List.concat_map ~f:(fun span -> span.Terminal.Highlight_tm_engine.scopes)
    |> List.dedup_and_sort ~compare:String.compare
  in
  print_s [%sexp (Option.is_none info.fallback : bool)];
  print_s [%sexp (List.mem scopes "keyword.control.chatml" ~equal:String.equal : bool)];
  let following, _ =
    Terminal.Highlight_tm_engine.highlight_text_with_scopes_with_info
      engine
      ~lang:(Some "chatmd")
      ~text:"<script src=\"external.chatml\"/><developer>ok</developer>"
  in
  assert (
    List.concat following
    |> List.exists ~f:(fun span ->
      String.is_substring span.text ~substring:"developer"
      && List.mem span.scopes "entity.name.tag.chatmd" ~equal:String.equal));
  [%expect
    {|
    (true true true)
    true
    true
    |}]
;;

let%test_unit "per-language bundled grammar baseline retains unrelated custom selection" =
  let registry, baseline = Chat_tui.Highlight_registry.create_with_baseline () in
  let engine =
    Terminal.Highlight_tm_engine.create ~theme:Terminal.Highlight_theme.github_dark
    |> Terminal.Highlight_tm_engine.with_registry ~registry
  in
  let uses language =
    Chat_tui.Highlight_registry.Baseline.uses_bundled_grammar baseline engine ~language
  in
  assert (uses `Chatml && uses `Chatmd);
  let add name scope =
    let json =
      sprintf {|{"name":"%s","scopeName":"%s","patterns":[]}|} name scope
      |> Jsonaf.parse
      |> Or_error.ok_exn
    in
    Terminal.Highlight_tm_loader.add_grammar_jsonaf registry json |> Or_error.ok_exn
  in
  add "Python" "source.python";
  assert (uses `Chatml && uses `Chatmd);
  add "ChatML" "source.chatml";
  assert (not (uses `Chatml));
  assert (uses `Chatmd)
;;

let%expect_test "terminal source rendering is source-bound and rejects one long line" =
  let text = "let a = 1\r\n(* comment *)\nlet b = 2" in
  let snapshot =
    Highlight.scan_chatml (chatml_source text) ~limits:(limits ()) ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let render_limits =
    Terminal_source_highlighting.Limits.create ~max_lines:8 ~max_line_bytes:32
    |> Or_error.ok_exn
  in
  let rendered =
    Terminal_source_highlighting.render
      snapshot
      ~theme:Terminal.Highlight_theme.github_dark
      ~limits:render_limits
      ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let reconstructed =
    Terminal_source_highlighting.lines rendered
    |> List.map ~f:(fun spans -> spans |> List.map ~f:snd |> String.concat)
  in
  print_s [%sexp (reconstructed : string list)];
  [%expect {| ("let a = 1" "(* comment *)" "let b = 2") |}];
  let long_source = chatml_source (String.make 10_000 'x') in
  let long_snapshot =
    Highlight.scan_chatml long_source ~limits:(limits ()) ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let result =
    Terminal_source_highlighting.render
      long_snapshot
      ~theme:Terminal.Highlight_theme.github_dark
      ~limits:render_limits
      ~checkpoint:ignore
  in
  print_s [%sexp (Result.error result : Terminal_source_highlighting.Error.t option)];
  [%expect {| ((Line_too_long 0)) |}]
;;

exception Interrupted

let%test_unit "lexical and terminal output checkpoints preserve interruption" =
  let text = String.make 5_000 '+' in
  let scan_limits =
    Highlight.Limits.create
      ~max_source_bytes:10_000
      ~max_identity_bytes:4096
      ~max_spans:6_000
      ~max_issues:16
      ~max_work_units:1_000_000
      ~max_nesting:32
    |> Or_error.ok_exn
  in
  let scan_polls = ref 0 in
  let scan_checkpoint () =
    Int.incr scan_polls;
    if !scan_polls = 2 then raise Interrupted
  in
  assert (
    try
      ignore
        (Highlight.scan_chatml
           (chatml_source text)
           ~limits:scan_limits
           ~checkpoint:scan_checkpoint
         : (Highlight.t, Highlight.Error.t) Result.t);
      false
    with
    | Interrupted -> true
    | _ -> false);
  let snapshot =
    Highlight.scan_chatml (chatml_source text) ~limits:scan_limits ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let render_limits =
    Terminal_source_highlighting.Limits.create ~max_lines:1 ~max_line_bytes:5_000
    |> Or_error.ok_exn
  in
  let render_polls = ref 0 in
  let render_checkpoint () =
    Int.incr render_polls;
    if !render_polls = 3 then raise Interrupted
  in
  assert (
    try
      ignore
        (Terminal_source_highlighting.render
           snapshot
           ~theme:Terminal.Highlight_theme.github_dark
           ~limits:render_limits
           ~checkpoint:render_checkpoint
         : (Terminal_source_highlighting.t, Terminal_source_highlighting.Error.t) Result.t);
      false
    with
    | Interrupted -> true
    | _ -> false)
;;

let%expect_test "chat TUI source-fence frontdoor preserves multiline styling and limits" =
  let highlight lang text =
    Chat_tui.Highlight_source_code.highlight
      lang
      ~text
      ~theme:Terminal.Highlight_theme.github_dark
      ~is_cancelled:(fun () -> false)
  in
  let rendered =
    highlight
      Chat_tui.Highlight_source_code.Language.Chatml
      "(* open\n   close *)\nlet value = 2"
    |> Result.ok
    |> Option.value_exn
  in
  print_s
    [%sexp
      (List.map rendered ~f:(fun row -> row |> List.map ~f:snd |> String.concat)
       : string list)];
  let with_color =
    List.concat rendered
    |> List.exists ~f:(fun (attr, _) -> not (Notty.A.equal attr Notty.A.empty))
  in
  print_s [%sexp (with_color : bool)];
  let oversized =
    highlight Chat_tui.Highlight_source_code.Language.Chatmd (String.make 65_537 'x')
  in
  let long_line =
    highlight Chat_tui.Highlight_source_code.Language.Chatmd (String.make 4_097 'x')
  in
  print_s [%sexp ((Result.is_error oversized, Result.is_error long_line) : bool * bool)];
  let plain =
    Chat_tui.Highlight_source_code.plain_preview
      ~text:("let x = 1\000" ^ String.make 70_000 'z')
      ~is_cancelled:(fun () -> false)
  in
  assert (
    List.concat plain
    |> List.for_all ~f:(fun (_, text) -> not (String.contains text '\000')));
  let displayed = List.concat plain |> List.map ~f:snd |> String.concat in
  assert (String.is_prefix displayed ~prefix:"let x = 1?");
  assert (String.is_substring displayed ~substring:"[source preview truncated]");
  let colorless =
    Chat_tui.Highlight_source_code.highlight
      Chat_tui.Highlight_source_code.Language.Chatml
      ~text:"let x = 1"
      ~theme:Terminal.Highlight_theme.empty
      ~is_cancelled:(fun () -> false)
    |> Result.ok
    |> Option.value_exn
  in
  assert (
    List.concat colorless
    |> List.for_all ~f:(fun (attr, _) -> Notty.A.equal attr Notty.A.empty));
  let nested = "let x = 1 " ^ String.concat (List.init 129 ~f:(fun _ -> "(*")) in
  let bounded =
    highlight Chat_tui.Highlight_source_code.Language.Chatml nested
    |> Result.ok
    |> Option.value_exn
    |> List.concat
    |> List.map ~f:snd
    |> String.concat
  in
  assert (String.is_prefix bounded ~prefix:nested);
  assert (String.is_suffix bounded ~suffix:"[source preview truncated]");
  let polls = ref 0 in
  assert (
    try
      ignore
        (Chat_tui.Highlight_source_code.plain_preview
           ~text:(String.make 8_000 'x')
           ~is_cancelled:(fun () ->
             Int.incr polls;
             Int.equal !polls 3)
         : Terminal.Highlight_tm_engine.span list list);
      false
    with
    | Exit -> true
    | _ -> false);
  [%expect
    {|
    ("(* open" "   close *)" "let value = 2")
    true
    (true true)
    |}]
;;

let%test_unit "chat TUI display spans satisfy Notty control and UTF-8 admission" =
  let bytes = String.init 256 ~f:Char.of_int_exn in
  let c1 = Buffer.create 64 in
  for code = 0x80 to 0x9f do
    Uutf.Buffer.add_utf_8 c1 (Stdlib.Uchar.of_int code)
  done;
  let source = bytes ^ Buffer.contents c1 in
  let rows =
    [ Chat_tui.Highlight_source_code.plain_preview ~text:source ~is_cancelled:(fun () ->
        false)
    ; Chat_tui.Highlight_source_code.highlight
        Chat_tui.Highlight_source_code.Language.Chatml
        ~text:source
        ~theme:Terminal.Highlight_theme.github_dark
        ~is_cancelled:(fun () -> false)
      |> Result.ok
      |> Option.value_exn
    ]
  in
  List.iter rows ~f:(fun rows ->
    List.iter rows ~f:(fun row ->
      List.iter row ~f:(fun (attr, text) -> ignore (Notty.I.string attr text : Notty.I.t))))
;;

let reference_lines source =
  let len = String.length source in
  let rec loop start i acc =
    if i >= len
    then List.rev (String.sub source ~pos:start ~len:(len - start) :: acc)
    else (
      match source.[i] with
      | '\r' ->
        let next =
          if i + 1 < len && Char.equal source.[i + 1] '\n' then i + 2 else i + 1
        in
        loop next next (String.sub source ~pos:start ~len:(i - start) :: acc)
      | '\n' -> loop (i + 1) (i + 1) (String.sub source ~pos:start ~len:(i - start) :: acc)
      | _ -> loop start (i + 1) acc)
  in
  loop 0 0 []
;;

let%test_unit
    "generated lexical snapshots retain exact owned prefixes and reconstruct lines"
  =
  let open Quickcheck.Generator in
  let byte = of_list (List.init 256 ~f:(fun i -> String.of_char (Char.of_int_exn i))) in
  let syntax =
    of_list
      [ "<script language=chatml>"
      ; "</script >"
      ; "RAW|"
      ; "|RAW"
      ; "(*"
      ; "*)"
      ; "\""
      ; "\\"
      ; "\r\n"
      ; "let "
      ]
  in
  let source =
    list_with_length 18 (weighted_union [ 3., byte; 2., syntax ])
    |> map ~f:(String.concat ~sep:"")
  in
  let caps = of_list [ 1, 1, 1, 1; 50, 4, 2, 2; 500, 32, 4, 4; 20_000, 512, 32, 64 ] in
  Quickcheck.test
    ~trials:80
    (both source caps)
    ~f:(fun (source, (work, spans, issues, nesting)) ->
      let limits =
        Highlight.Limits.create
          ~max_source_bytes:1024
          ~max_identity_bytes:4096
          ~max_spans:spans
          ~max_issues:issues
          ~max_work_units:work
          ~max_nesting:nesting
        |> Or_error.ok_exn
      in
      let snapshots =
        [ Highlight.scan_chatml (chatml_source source) ~limits ~checkpoint:ignore
        ; Highlight.scan_chatmd
            (chatmd_input ~authored:source ~effective:source)
            ~view:Authored
            ~limits
            ~checkpoint:ignore
        ]
      in
      List.iter snapshots ~f:(fun result ->
        let snapshot = result |> Result.ok |> Option.value_exn in
        let prefix =
          match Highlight.status snapshot with
          | Highlight.Status.Reached_eof -> String.length source
          | Highlight.Status.Truncated { next_byte; _ } -> next_byte
        in
        assert (prefix >= 0 && prefix <= String.length source);
        assert (String.equal (Highlight.source_contents snapshot) source);
        let previous = ref 0 in
        List.iter (Highlight.spans snapshot) ~f:(fun span ->
          let range = Highlight.Span.range span in
          assert (Highlight.contains_span snapshot span);
          assert (
            Highlight.Source_owner.equal
              (Highlight.Range.owner range)
              (Highlight.source_owner snapshot));
          assert (Highlight.Range.start_byte range >= !previous);
          assert (Highlight.Range.end_byte range > Highlight.Range.start_byte range);
          assert (Highlight.Range.end_byte range <= prefix);
          previous := Highlight.Range.end_byte range);
        List.iter (Highlight.issues snapshot) ~f:(fun issue ->
          let range = Highlight.Issue.range issue in
          assert (
            Highlight.Source_owner.equal
              (Highlight.Range.owner range)
              (Highlight.source_owner snapshot));
          assert (Highlight.Range.start_byte range >= 0);
          assert (Highlight.Range.end_byte range > Highlight.Range.start_byte range);
          assert (Highlight.Range.end_byte range <= prefix));
        let render_limits =
          Terminal_source_highlighting.Limits.create ~max_lines:1024 ~max_line_bytes:1024
          |> Or_error.ok_exn
        in
        let rows =
          Terminal_source_highlighting.render
            snapshot
            ~theme:Terminal.Highlight_theme.github_dark
            ~limits:render_limits
            ~checkpoint:ignore
          |> Result.ok
          |> Option.value_exn
          |> Terminal_source_highlighting.lines
          |> List.map ~f:(fun row -> row |> List.map ~f:snd |> String.concat)
        in
        assert (List.equal String.equal rows (reference_lines source))))
;;

let cursor_view ?(work = 100_000) source cursor_byte =
  Highlight.scan_chatmd_cursor
    (chatmd_input ~authored:source ~effective:source)
    ~cursor_byte
    ~limits:(limits ~work ())
    ~checkpoint:ignore
  |> Result.ok
  |> Option.value_exn
;;

let cursor_summary = function
  | Highlight.Cursor.Unavailable unavailable ->
    Sexp.to_string_hum (Highlight.Cursor.Unavailable.sexp_of_t unavailable)
  | Available cursor ->
    let module C = Highlight.Cursor in
    let kind =
      match C.kind cursor with
      | Tag_name { closing } -> if closing then "closing-tag" else "opening-tag"
      | Attribute_name { tag } ->
        "attribute-name:" ^ Sexp.to_string (Chatmd_ast.sexp_of_tag tag)
      | Attribute_value { tag; attribute; value_state } ->
        sprintf
          "attribute-value:%s:%s:%s:%b:%b"
          (Sexp.to_string (Chatmd_ast.sexp_of_tag tag))
          attribute
          (Option.value_map
             (C.Value_state.quote value_state)
             ~default:"unquoted"
             ~f:(fun quote -> Sexp.to_string (Chatmd_attribute_lex.Quote.sexp_of_t quote)))
          (C.Value_state.trailing_matching_quote_escape value_state)
          (C.Value_state.open_entity value_state)
      | Text -> "text"
      | Script_body -> "script-body"
      | Opaque -> "opaque"
    in
    let attributes =
      C.completed_attributes cursor
      |> List.map ~f:(fun attribute -> C.Completed_attribute.decoded attribute)
    in
    sprintf
      "%s prefix=%S uncertain=%b attrs=%s"
      kind
      (C.Raw_slice.contents (C.raw_prefix cursor))
      (C.uncertain_discriminator cursor)
      (Sexp.to_string ([%sexp_of: Chatmd_ast.attribute list] attributes))
;;

let%expect_test "ChatMD cursor retains raw prefix and only completed strict attributes" =
  [ "<"
  ; "</"
  ; "<script "
  ; "<script kind"
  ; "<script kind "
  ; "<script kind="
  ; "<script kind=\"ext"
  ; "<script kind=\"ext\""
  ; "<script kind=\"ext\" foo"
  ; "<script kind=\"a\\\"b\" type=\"x\" kind='other' "
  ; "<script kind=\"a&amp;\" "
  ; "<script kind=\""
  ; "<script language=\"chatml\">"
  ; "<script>abc"
  ; "<script>abc</scr"
  ; "<script>abc</script "
  ; "<script>RAW|</scr"
  ; "<script>abc</script>"
  ; "<unknown><script "
  ; "<unknown></unknown><doc "
  ; "<developer></script><doc "
  ; "<shell_access/><command "
  ; "<developer><!--"
  ; "<script kind=\"x&am"
  ; "<script kind=\"x>y"
  ; "<script kind=\"x>y\">more<doc "
  ; "<script kind=\"x\\"
  ; "<script kind=\"x\\q\""
  ; "<script kind=unquoted&"
  ; "<msg x=a\" y=\">"
  ; "<msg x=a\" y=\"><tool na"
  ]
  |> List.iter ~f:(fun source ->
    printf
      "%S => %s\n"
      source
      (cursor_summary (cursor_view source (String.length source))));
  [%expect
    {|
    "<" => opening-tag prefix="" uncertain=true attrs=()
    "</" => closing-tag prefix="" uncertain=true attrs=()
    "<script " => attribute-name:Script prefix="" uncertain=false attrs=()
    "<script kind" => attribute-name:Script prefix="kind" uncertain=true attrs=()
    "<script kind " => attribute-name:Script prefix="" uncertain=true attrs=()
    "<script kind=" => attribute-value:Script:kind:unquoted:false:false prefix="" uncertain=true attrs=()
    "<script kind=\"ext" => attribute-value:Script:kind:Double:false:false prefix="ext" uncertain=true attrs=()
    "<script kind=\"ext\"" => attribute-name:Script prefix="" uncertain=false attrs=((kind(ext)))
    "<script kind=\"ext\" foo" => attribute-name:Script prefix="foo" uncertain=true attrs=((kind(ext)))
    "<script kind=\"a\\\"b\" type=\"x\" kind='other' " => attribute-name:Script prefix="" uncertain=false attrs=((kind("a\\\"b"))(type(x))(kind(other)))
    "<script kind=\"a&amp;\" " => attribute-name:Script prefix="" uncertain=false attrs=((kind(a&)))
    "<script kind=\"" => attribute-value:Script:kind:Double:false:false prefix="" uncertain=true attrs=()
    "<script language=\"chatml\">" => script-body prefix="" uncertain=false attrs=()
    "<script>abc" => script-body prefix="" uncertain=false attrs=()
    "<script>abc</scr" => closing-tag prefix="scr" uncertain=true attrs=()
    "<script>abc</script " => opaque prefix="" uncertain=false attrs=()
    "<script>RAW|</scr" => opaque prefix="" uncertain=false attrs=()
    "<script>abc</script>" => text prefix="" uncertain=false attrs=()
    "<unknown><script " => attribute-name:Script prefix="" uncertain=false attrs=()
    "<unknown></unknown><doc " => attribute-name:Doc prefix="" uncertain=false attrs=()
    "<developer></script><doc " => opaque prefix="" uncertain=true attrs=()
    "<shell_access/><command " => text prefix="" uncertain=false attrs=()
    "<developer><!--" => opaque prefix="" uncertain=false attrs=()
    "<script kind=\"x&am" => attribute-value:Script:kind:Double:false:true prefix="x&am" uncertain=true attrs=()
    "<script kind=\"x>y" => opaque prefix="" uncertain=false attrs=()
    "<script kind=\"x>y\">more<doc " => opaque prefix="" uncertain=true attrs=()
    "<script kind=\"x\\" => attribute-value:Script:kind:Double:true:false prefix="x\\" uncertain=true attrs=()
    "<script kind=\"x\\q\"" => attribute-name:Script prefix="" uncertain=false attrs=((kind("x\\q")))
    "<script kind=unquoted&" => attribute-value:Script:kind:unquoted:false:false prefix="unquoted&" uncertain=true attrs=()
    "<msg x=a\" y=\">" => opaque prefix="" uncertain=true attrs=()
    "<msg x=a\" y=\"><tool na" => opaque prefix="" uncertain=true attrs=()
    |}]
;;

let attribute_equal (left_name, left_value) (right_name, right_value) =
  String.equal left_name right_name && Option.equal String.equal left_value right_value
;;

let%expect_test "shared strict attribute spellings and every admitted byte prefix agree" =
  let fragments =
    [ {| a="x&amp;y" b='one\'two' flag|}
    ; {| a=left&amp;right b="&nosuch;&amp;"|}
    ; {| a="x\q" b="x\"y"|}
    ; {| ignored!name=ok a="&&&&&&&&"|}
    ]
  in
  List.iter fragments ~f:(fun source ->
    let full = Chatmd_attribute_lex.complete_exn source ~checkpoint:ignore in
    let with_ranges =
      Chatmd_attribute_lex.complete_with_ranges
        ~source
        ~start_byte:0
        ~end_byte:(String.length source)
        ~checkpoint:ignore
      |> Result.ok
      |> Option.value_exn
      |> List.map ~f:Chatmd_attribute_lex.Attribute.decoded
    in
    assert (List.equal attribute_equal full with_ranges);
    for end_byte = 0 to String.length source do
      let prefix =
        Chatmd_attribute_lex.prefix ~source ~start_byte:0 ~end_byte ~checkpoint:ignore
        |> Result.ok
        |> Option.value_exn
      in
      let completed =
        Chatmd_attribute_lex.Prefix.completed prefix
        |> List.map ~f:Chatmd_attribute_lex.Attribute.decoded
      in
      assert (
        List.equal attribute_equal completed (List.take full (List.length completed)))
    done);
  print_s
    [%sexp
      (Chatmd_attribute_lex.complete_exn
         {| a="x&amp;y" b='one\'two' flag|}
         ~checkpoint:ignore
       : Chatmd_ast.attribute list)];
  [%expect {| ((a (x&y)) (b ("one\\'two")) (flag ())) |}]
;;

let%expect_test "bounded malformed attribute prefixes remain tolerant and source-bound" =
  let alphabet = [| '='; ' '; '"'; '\\'; '&'; ';'; '<'; '>' |] in
  let examples = ref 0 in
  let check_slice source owner slice =
    let module C = Highlight.Cursor in
    let start_byte = C.Raw_slice.start_byte slice in
    let end_byte = C.Raw_slice.end_byte slice in
    assert (start_byte >= 0 && end_byte >= start_byte && end_byte <= String.length source);
    assert (Highlight.Source_owner.equal owner (C.Raw_slice.owner slice));
    assert (
      String.equal
        (C.Raw_slice.contents slice)
        (String.sub source ~pos:start_byte ~len:(end_byte - start_byte)))
  in
  let check source =
    let input = chatmd_input ~authored:source ~effective:source in
    let snapshot =
      Highlight.scan_chatmd input ~view:Authored ~limits:(limits ()) ~checkpoint:ignore
      |> Result.ok
      |> Option.value_exn
    in
    List.iter (Highlight.spans snapshot) ~f:(fun span ->
      let range = Highlight.Span.range span in
      assert (
        Highlight.Source_owner.equal
          (Highlight.source_owner snapshot)
          (Highlight.Range.owner range));
      assert (Highlight.Range.start_byte range >= 0);
      assert (Highlight.Range.end_byte range <= String.length source));
    for cursor_byte = 0 to String.length source do
      match
        Highlight.scan_chatmd_cursor
          input
          ~cursor_byte
          ~limits:(limits ())
          ~checkpoint:ignore
        |> Result.ok
        |> Option.value_exn
      with
      | Highlight.Cursor.Unavailable _ ->
        failwith "tiny admitted ChatMD prefix unexpectedly unavailable"
      | Available cursor ->
        let module C = Highlight.Cursor in
        let owner = C.owner cursor in
        assert (Highlight.Source_owner.equal owner (Highlight.source_owner snapshot));
        check_slice source owner (C.raw_prefix cursor);
        (match C.kind cursor with
         | Text | Script_body | Opaque ->
           assert (Int.equal (C.Raw_slice.start_byte (C.raw_prefix cursor)) cursor_byte);
           assert (List.is_empty (C.completed_attributes cursor))
         | Tag_name _ | Attribute_name _ | Attribute_value _ -> ());
        List.iter (C.completed_attributes cursor) ~f:(fun attribute ->
          check_slice source owner (C.Completed_attribute.raw_name attribute);
          Option.iter
            (C.Completed_attribute.raw_value attribute)
            ~f:(check_slice source owner))
    done;
    Int.incr examples
  in
  check "<msg x=a\" y=\">";
  for
    code = 0
    to (Array.length alphabet * Array.length alphabet * Array.length alphabet) - 1
  do
    let a = alphabet.(code mod Array.length alphabet) in
    let b = alphabet.(code / Array.length alphabet mod Array.length alphabet) in
    let c = alphabet.(code / (Array.length alphabet * Array.length alphabet)) in
    check (sprintf "<msg x=%c%c%c>" a b c)
  done;
  let callback = Failure "attribute-callback" in
  let caught =
    try
      ignore
        (Chatmd_attribute_lex.complete_with_ranges
           ~source:{| x="v"|}
           ~start_byte:0
           ~end_byte:6
           ~checkpoint:(fun () -> raise callback));
      false
    with
    | exn -> phys_equal exn callback
  in
  print_s [%sexp ((!examples, caught) : int * bool)];
  [%expect {| (513 true) |}]
;;

let%expect_test "cursor owner, invalid cursor, work bound, and callback identity" =
  let source = "<script kind=\"extension\" " in
  let input = chatmd_input ~authored:source ~effective:source in
  let snapshot =
    Highlight.scan_chatmd input ~view:Authored ~limits:(limits ()) ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let available =
    Highlight.scan_chatmd_cursor
      input
      ~cursor_byte:(String.length source)
      ~limits:(limits ())
      ~checkpoint:ignore
    |> Result.ok
    |> Option.value_exn
  in
  let owner_matches =
    match available with
    | Highlight.Cursor.Available cursor ->
      let module C = Highlight.Cursor in
      let exact_attribute =
        match C.completed_attributes cursor with
        | [ attribute ] ->
          let raw_name = C.Completed_attribute.raw_name attribute in
          let raw_value = C.Completed_attribute.raw_value attribute |> Option.value_exn in
          String.equal (C.Raw_slice.contents raw_name) "kind"
          && String.equal (C.Raw_slice.contents raw_value) "\"extension\""
          && List.for_all [ raw_name; raw_value ] ~f:(fun slice ->
            Highlight.Source_owner.equal (C.Raw_slice.owner slice) (C.owner cursor)
            && String.equal
                 (C.Raw_slice.contents slice)
                 (String.sub
                    source
                    ~pos:(C.Raw_slice.start_byte slice)
                    ~len:(C.Raw_slice.end_byte slice - C.Raw_slice.start_byte slice)))
        | _ -> false
      in
      exact_attribute
      && Highlight.Source_owner.equal (C.owner cursor) (Highlight.source_owner snapshot)
      && Highlight.Source_owner.equal
           (C.Raw_slice.owner (C.raw_prefix cursor))
           (C.owner cursor)
    | Unavailable _ -> false
  in
  let invalid = cursor_view source (String.length source + 1) in
  let small_work = cursor_view ~work:1 source (String.length source) in
  let cancellation = Failure "cursor-cancel" in
  let caught =
    try
      ignore
        (Highlight.scan_chatmd_cursor
           input
           ~cursor_byte:(String.length source)
           ~limits:(limits ())
           ~checkpoint:(fun () -> raise cancellation));
      false
    with
    | exn -> phys_equal exn cancellation
  in
  let malformed_bytes = "\255<doc " in
  let invalid_utf8_is_byte_exact =
    match cursor_view malformed_bytes (String.length malformed_bytes) with
    | Highlight.Cursor.Available cursor ->
      (match Highlight.Cursor.kind cursor with
       | Attribute_name { tag } -> Chatmd_ast.tag_equal tag Doc
       | Tag_name _ | Attribute_value _ | Text | Script_body | Opaque -> false)
    | Unavailable _ -> false
  in
  let long_source = String.make 5000 'x' in
  let callbacks = ref 0 in
  let midscan_caught =
    try
      ignore
        (Highlight.scan_chatmd_cursor
           (chatmd_input ~authored:long_source ~effective:long_source)
           ~cursor_byte:(String.length long_source)
           ~limits:(limits ())
           ~checkpoint:(fun () ->
             Int.incr callbacks;
             if Int.equal !callbacks 2 then raise cancellation));
      false
    with
    | exn -> phys_equal exn cancellation
  in
  print_s
    [%sexp
      ((owner_matches, caught, invalid_utf8_is_byte_exact, midscan_caught)
       : bool * bool * bool * bool)];
  print_s [%sexp ((cursor_summary invalid, cursor_summary small_work) : string * string)];
  [%expect
    {|
    (true true true true)
    (Invalid_cursor "(Beyond_scanned_prefix (next_byte 0) (reason Work_limit))")
    |}]
;;

let strict_lexer_recognizes_self source =
  let next = Chatmd_lexer.create () in
  let lexbuf = Lexing.from_string source in
  let rec loop remaining =
    if remaining <= 0
    then failwith "lexer token bound"
    else (
      match next lexbuf with
      | Chatmd_tokens.SELF _ -> true
      | EOF -> false
      | _ -> loop (remaining - 1))
  in
  loop 32
;;

let%expect_test "cursor official tag scope agrees with strict lexer tokens" =
  [ "", "doc"
  ; "", "command"
  ; "<shell_access>", "command"
  ; "<tool type=\"shell\">", "command"
  ; "<tool>", "command"
  ; "<tool type=\"moderator\">", "uses"
  ; "<tool>", "uses"
  ; "<developer>", "authoring_context"
  ; "", "authoring_context"
  ; "<unknown>", "doc"
  ]
  |> List.iter ~f:(fun (base, name) ->
    let strict = strict_lexer_recognizes_self (base ^ "<" ^ name ^ "/>") in
    let source = base ^ "<" ^ name ^ " " in
    let cursor = cursor_view source (String.length source) in
    let context =
      match cursor with
      | Highlight.Cursor.Available cursor ->
        (match Highlight.Cursor.kind cursor with
         | Attribute_name _ -> true
         | Tag_name _ | Attribute_value _ | Text | Script_body | Opaque -> false)
      | Unavailable _ -> false
    in
    assert (Bool.equal strict context);
    printf "%S %S => %b\n" base name context);
  [%expect
    {|
    "" "doc" => true
    "" "command" => false
    "<shell_access>" "command" => true
    "<tool type=\"shell\">" "command" => true
    "<tool>" "command" => false
    "<tool type=\"moderator\">" "uses" => true
    "<tool>" "uses" => false
    "<developer>" "authoring_context" => false
    "" "authoring_context" => true
    "<unknown>" "doc" => true
    |}]
;;
