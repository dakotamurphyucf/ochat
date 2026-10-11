open! Core
module Input = Chatmd_parser_input
module CM = Prompt.Chat_markdown

let limits
      ?(bytes = 8 * 1024 * 1024)
      ?(calls = 1024)
      ?(tokens = 100_000)
      ?(depth = 128)
      ()
  =
  CM.Limits.create
    ~max_input_bytes:bytes
    ~max_parse_calls:calls
    ~max_tokens:tokens
    ~max_depth:depth
  |> Or_error.ok_exn
;;

let captured dir sources = Source_loader.captured_filesystem ~root:dir ~sources

let prepare ?(checkpoint = ignore) ?(preprocess = Fn.id) ?(bounds = limits ()) dir sources
  =
  let authored = List.Assoc.find_exn sources "root.chatmd" ~equal:String.equal in
  CM.prepare_chat_inputs
    ~source:"root.chatmd"
    ~source_loader:(captured dir sources)
    ~dir
    ~preprocess
    ~limits:bounds
    ~checkpoint
    authored
;;

let%test_unit
    "position index agrees with independent byte pieces across CRLF and block boundary"
  =
  let prefix = String.make 4095 'x' in
  let contents = prefix ^ "\r\né\rZ\n" in
  let index = Source.Position_index.create contents ~checkpoint:ignore in
  List.iter
    [ 0, 1, 0
    ; 4095, 1, 4095
    ; 4096, 1, 4096
    ; 4097, 2, 0
    ; 4098, 2, 1
    ; 4099, 2, 2
    ; 4100, 3, 0
    ; 4101, 3, 1
    ; 4102, 4, 0
    ]
    ~f:(fun (offset, line, column) ->
      let actual =
        Source.Position_index.position index ~byte_offset:offset ~checkpoint:ignore
        |> Or_error.ok_exn
      in
      assert (
        Int.equal actual.offset offset
        && Int.equal actual.line line
        && Int.equal actual.column column));
  assert (
    Result.is_error
      (Source.Position_index.position index ~byte_offset:(-1) ~checkpoint:ignore));
  assert (
    Result.is_error
      (Source.Position_index.position index ~byte_offset:4103 ~checkpoint:ignore))
;;

let%expect_test
    "tag ranges belong to exact root and imported inputs, including legacy moderator \
     conversion"
  =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let prefix = "<!-- é\r\n -->\r\n" in
    let moderator =
      {|<script id="m" language="chatml" kind="moderator">RAW|same|RAW</script>|}
    in
    let separator = "\r\n" in
    let tool = {|<script id="t" language="chatml" kind="shell_matcher">same</script>|} in
    let imported = {|<script id="u" language="chatml" kind="tool" src="code.chatml"/>|} in
    let root =
      prefix
      ^ moderator
      ^ separator
      ^ tool
      ^ {|<import src="import.chatmd" namespace="lib"/>|}
    in
    let parsed =
      prepare
        dir
        [ "root.chatmd", root; "import.chatmd", "\r\n" ^ imported; "code.chatml", "same" ]
    in
    let expected =
      [ ( "m"
        , "root.chatmd"
        , String.length prefix
        , String.length prefix + String.length moderator )
      ; ( "t"
        , "root.chatmd"
        , String.length prefix + String.length moderator + String.length separator
        , String.length prefix
          + String.length moderator
          + String.length separator
          + String.length tool )
      ; "lib:u", "import.chatmd", 2, 2 + String.length imported
      ]
    in
    List.iter2_exn
      (CM.Parsed.declarations parsed)
      expected
      ~f:(fun declaration (id, path, start_byte, end_byte) ->
        let range = CM.Declaration.range declaration in
        let input = Input.Range.input range in
        assert (String.equal (CM.Declaration.script_id declaration) id);
        assert (String.equal (Input.path input) path);
        assert (
          Int.equal (Input.Range.start_byte range) start_byte
          && Int.equal (Input.Range.end_byte range) end_byte);
        assert (String.equal (CM.Declaration.script_body declaration) "same");
        let reference = CM.Declaration.source_ref declaration in
        assert (
          Int.equal reference.start_pos.offset start_byte
          && Int.equal reference.end_pos.offset end_byte);
        assert (String.equal reference.source_sha256 (Input.effective_sha256 input));
        print_s
          [%sexp
            (id : string)
          , (Input.role input : Input.Role.t)
          , (path : string)
          , (start_byte : int)
          , (end_byte : int)]);
    match List.hd_exn (CM.Parsed.elements parsed) with
    | CM.Script { source_ref = Some reference; _ } ->
      assert (Int.equal reference.start_pos.offset (String.length prefix))
    | _ -> assert false);
  [%expect
    {|
    (m Root root.chatmd 15 86)
    (t Root root.chatmd 88 155)
    (lib:u Import import.chatmd 2 66)
    |}]
;;

let%test_unit
    "retained effective authored input replays without preprocessing or native fallback"
  =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let authored = "<user>raw</user>" in
    let effective = "<!-- META_REFINE --><user>éffective</user>" in
    let calls = ref 0 in
    let parsed =
      prepare
        ~preprocess:(fun text ->
          assert (String.equal text authored);
          incr calls;
          effective)
        dir
        [ "root.chatmd", authored ]
    in
    assert (Int.equal !calls 1);
    let input = CM.Parsed.root_input parsed in
    assert (
      String.equal (Input.authored input) authored
      && String.equal (Input.effective input) effective);
    let replay =
      CM.replay_chat_inputs
        input
        ~source_loader:(captured dir [ "root.chatmd", authored ])
        ~dir
        ~limits:(limits ())
        ~checkpoint:ignore
    in
    assert (Int.equal !calls 1);
    assert (
      List.equal
        (fun left right -> Int.equal (CM.compare_top_level_elements left right) 0)
        (CM.Parsed.elements parsed)
        (CM.Parsed.elements replay));
    List.iter
      [ captured dir []
      ; captured dir [ "root.chatmd", "different" ]
      ; Source_loader.filesystem ~root:dir
      ]
      ~f:(fun loader ->
        match
          CM.replay_chat_inputs
            input
            ~source_loader:loader
            ~dir
            ~limits:(limits ())
            ~checkpoint:ignore
        with
        | _ -> assert false
        | exception Failure _ -> ());
    (* Rejected generated input remains distinct from authored effective replay. *)
    let bundle =
      Chatmd_source_bundle.create
        ~root_file:"root.chatmd"
        ~sources:[ "root.chatmd", effective ]
        ()
      |> Result.ok_or_failwith
    in
    match CM.parse_source_bundle ~dir bundle with
    | _ -> assert false
    | exception Failure _ -> ())
;;

exception Interrupted of int

let%test_unit
    "every observed prepare and replay checkpoint preserves exception identity and \
     reusability"
  =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let sources =
      [ "root.chatmd", {|<import src="part.chatmd"/>|}; "part.chatmd", "<user>x</user>" ]
    in
    let count = ref 0 in
    let parsed = prepare ~checkpoint:(fun () -> incr count) dir sources in
    assert (!count > 0);
    for stop = 1 to !count do
      let calls = ref 0 in
      let error = Interrupted stop in
      match
        prepare
          ~checkpoint:(fun () ->
            incr calls;
            if Int.equal !calls stop then raise error)
          dir
          sources
      with
      | _ -> assert false
      | exception actual -> assert (phys_equal actual error)
    done;
    let input = CM.Parsed.root_input parsed in
    let replay checkpoint =
      CM.replay_chat_inputs
        input
        ~source_loader:(captured dir sources)
        ~dir
        ~limits:(limits ())
        ~checkpoint
    in
    count := 0;
    ignore (replay (fun () -> incr count) : CM.Parsed.t);
    assert (!count > 0);
    for stop = 1 to !count do
      let calls = ref 0 in
      let error = Interrupted stop in
      match
        replay (fun () ->
          incr calls;
          if Int.equal !calls stop then raise error)
      with
      | _ -> assert false
      | exception actual -> assert (phys_equal actual error)
    done;
    let returned = ref false in
    (match
       Eio.Cancel.sub (fun context ->
         let calls = ref 0 in
         ignore
           (replay (fun () ->
              incr calls;
              if Int.equal !calls !count
              then Eio.Cancel.cancel context (Interrupted !count))
            : CM.Parsed.t);
         returned := true)
     with
     | () -> assert false
     | exception Eio.Cancel.Cancelled _ -> ());
    assert (not !returned);
    assert (
      List.equal
        (fun left right -> Int.equal (CM.compare_top_level_elements left right) 0)
        (CM.Parsed.elements parsed)
        (CM.Parsed.elements (replay ignore))))
;;

let%test_unit "owned limits include imported parsing and referenced script reads" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let rejects bounds sources =
      match prepare ~bounds dir sources with
      | _ -> assert false
      | exception Failure _ -> ()
    in
    rejects
      (limits ~calls:1 ())
      [ "root.chatmd", {|<import src="part.chatmd"/>|}; "part.chatmd", "<user>x</user>" ];
    rejects (limits ~tokens:2 ()) [ "root.chatmd", "<user>x</user>" ];
    rejects (limits ~depth:1 ()) [ "root.chatmd", "<user><user>x</user></user>" ];
    let combined =
      [ "root.chatmd", {|<user><import src="part.chatmd"/></user>|}
      ; "part.chatmd", "<user>x</user>"
      ]
    in
    ignore (prepare ~bounds:(limits ~depth:2 ()) dir combined : CM.Parsed.t);
    rejects (limits ~depth:1 ()) combined;
    rejects
      (limits ~depth:2 ())
      [ "root.chatmd", {|<user><import src="part.chatmd"/></user>|}
      ; "part.chatmd", {|<msg role="assistant"><import src="unused.chatmd"/></msg>|}
      ];
    let repeated =
      prepare
        dir
        [ "root.chatmd", {|<import src="part.chatmd"/><import src="part.chatmd"/>|}
        ; "part.chatmd", "<user>x</user>"
        ]
    in
    (match CM.Parsed.inputs repeated with
     | [ _root; first; second ] -> assert (phys_equal first second)
     | _ -> assert false);
    let bounds =
      Chatmd_owned_parser.Limits.create
        ~max_input_bytes:1024
        ~max_parse_calls:2
        ~max_tokens:100
        ~max_depth:8
      |> Or_error.ok_exn
    in
    let owner = Chatmd_owned_parser.create bounds ~checkpoint:ignore in
    let input contents =
      Chatmd_owned_parser.input
        owner
        ~role:Import
        ~path:"part.chatmd"
        ~authored:contents
        ~effective:contents
    in
    let first = input "first" in
    assert (phys_equal first (input "first"));
    let changed = input "changed" in
    assert (not (phys_equal first changed));
    assert (String.equal (Input.effective changed) "changed");
    (match input "unused third revision" with
     | _ -> assert false
     | exception Failure _ -> ());
    rejects
      (limits ~bytes:100 ())
      [ "root.chatmd", {|<script language="chatml" kind="moderator" src="body.chatml"/>|}
      ; "body.chatml", String.make 101 'x'
      ];
    let loader =
      captured dir [ "root.chatmd", "abcd" ]
      |> Source_loader.with_read_limit ~max_bytes:(fun _ -> 3)
      |> Source_loader.with_read_limit ~max_bytes:(fun _ -> 100)
      |> Source_loader.with_observer ~f:(fun _ _ -> assert false)
      |> Source_loader.with_agent_observer ~f:ignore
    in
    let source = Source_loader.root loader ~file:"root.chatmd" |> Result.ok_or_failwith in
    assert (Result.is_error (Source_loader.read loader source));
    assert (Result.is_error (Source_loader.read_bounded ~max_bytes:100 loader source)))
;;
