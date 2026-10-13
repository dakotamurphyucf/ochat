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
