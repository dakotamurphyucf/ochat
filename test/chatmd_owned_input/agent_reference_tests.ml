open Core
module CM = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module A = CM.Agent_reference

let parse ?(checkpoint = ignore) ?(observe = ignore) dir sources =
  let source_loader =
    Source_loader.captured_filesystem ~root:dir ~sources
    |> Source_loader.with_agent_observer ~f:observe
  in
  CM.prepare_chat_inputs
    ~source:"root.chatmd"
    ~source_loader
    ~dir
    ~preprocess:Fn.id
    ~limits:(Input_tests.limits ())
    ~checkpoint
    (List.Assoc.find_exn sources "root.chatmd" ~equal:String.equal)
;;

let kind reference =
  match A.invocation reference with
  | Tool tool -> "ordinary:" ^ tool.name
  | Persistent_tool (tool, Persistent) -> "persistent:" ^ tool.name
  | Persistent_tool (tool, Optional) -> "optional:" ^ tool.name
  | Content _ -> "content"
;;

let%expect_test
    "owned emitted agent references preserve actual imported declaration bases"
  =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let ordinary = {|<tool name="once" agent="../agents/a.chatmd" local/>|} in
    let persistent =
      {|<tool name="keep" agent="../agents/a.chatmd" local persistence="optional" source_mode="captured"/>|}
    in
    let child =
      {|<agent src="../agents/b.chatmd" local source_mode="file">child</agent>|}
    in
    let opening =
      {|<agent src="../agents/a.chatmd" local source_mode="captured">parent|}
    in
    let parent = opening ^ child ^ "</agent>" in
    let root =
      {|<import src="parts/tools.chatmd"/><user><import src="parts/content.chatmd"/></user>
<user><tool name="ignored" agent="agents/ignored.chatmd" local/></user>
<tool name="remote" agent="https://example.invalid/agent"/>
<tool name="external" agent="/explicit/external.chatmd" local/>|}
    in
    let sources =
      [ "root.chatmd", root
      ; "parts/tools.chatmd", ordinary ^ "\r\n" ^ persistent
      ; "parts/content.chatmd", "\r\n" ^ parent
      ]
    in
    let queued = ref [] in
    let parsed =
      parse dir sources ~observe:(fun source ->
        queued := Source_loader.relative_path source :: !queued)
    in
    let references = CM.Parsed.agent_references parsed in
    assert (List.length references = 6);
    (* Independent byte-piece oracle: no resolver or parser positions generate
       the expected declaration owners, offsets, extracted tags or target keys. *)
    let expected =
      [ "ordinary:once", "parts/tools.chatmd", 0, ordinary, Some "agents/a.chatmd"
      ; ( "optional:keep"
        , "parts/tools.chatmd"
        , String.length ordinary + 2
        , persistent
        , Some "agents/a.chatmd" )
      ; "content", "parts/content.chatmd", 2, parent, Some "agents/a.chatmd"
      ; ( "content"
        , "parts/content.chatmd"
        , 2 + String.length opening
        , child
        , Some "agents/b.chatmd" )
      ]
    in
    List.iter expected ~f:(fun (role, path, start_byte, tag, target) ->
      let reference =
        List.find_exn references ~f:(fun reference ->
          let range = A.range reference in
          String.equal (kind reference) role
          && String.equal (Input.path (Input.Range.input range)) path
          && Int.equal (Input.Range.start_byte range) start_byte)
      in
      let range = A.range reference in
      assert (Input.Range.end_byte range = start_byte + String.length tag);
      assert (Option.equal String.equal (A.queued_key reference) target);
      let expected_mode =
        if
          String.equal role "optional:keep"
          || (String.equal role "content" && Int.equal start_byte 2)
        then A.Source_mode.Captured
        else File
      in
      assert (A.Source_mode.equal expected_mode (A.source_mode reference));
      let exact =
        String.sub
          (Input.effective (Input.Range.input range))
          ~pos:start_byte
          ~len:(String.length tag)
      in
      assert (String.equal exact tag);
      print_s [%sexp (role : string), (path : string), (target : string option)]);
    List.iter [ "ordinary:remote"; "ordinary:external" ] ~f:(fun role ->
      let reference = List.find_exn references ~f:(fun r -> String.equal (kind r) role) in
      assert (Option.is_none (A.queued_key reference)));
    assert (
      not (List.exists references ~f:(fun r -> String.equal (kind r) "ordinary:ignored")));
    assert (List.count !queued ~f:(String.equal "agents/a.chatmd") = 3);
    assert (List.mem !queued "agents/b.chatmd" ~equal:String.equal);
    (* The old parser also resolves ignored markup. That queue fact is not an
       emitted executable invocation and must not become a requested role. *)
    assert (List.mem !queued "agents/ignored.chatmd" ~equal:String.equal);
    let replay =
      CM.replay_chat_inputs
        (CM.Parsed.root_input parsed)
        ~source_loader:(Source_loader.captured_filesystem ~root:dir ~sources)
        ~dir
        ~limits:(Input_tests.limits ())
        ~checkpoint:ignore
    in
    assert (
      List.equal
        (fun a b ->
           String.equal (kind a) (kind b)
           && Option.equal String.equal (A.queued_key a) (A.queued_key b)
           && A.Source_mode.equal (A.source_mode a) (A.source_mode b))
        references
        (CM.Parsed.agent_references replay)));
  [%expect
    {|
    (ordinary:once parts/tools.chatmd (agents/a.chatmd))
    (optional:keep parts/tools.chatmd (agents/a.chatmd))
    (content parts/content.chatmd (agents/a.chatmd))
    (content parts/content.chatmd (agents/b.chatmd)) |}]
;;

let%test_unit "agent member and observer exceptions preserve identity and source reuse" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let sources = [ "root.chatmd", {|<tool name="a" agent="child.chatmd" local/>|} ] in
    let loader = Source_loader.captured_filesystem ~root:dir ~sources in
    let base = Source_loader.root loader ~file:"root.chatmd" |> Result.ok_or_failwith in
    let sentinel = Failure "original agent reference callback" in
    (match
       Source_loader.agent_reference
         loader
         ~base
         ~reference:"child.chatmd"
         ~on_member:(fun _ -> raise sentinel)
     with
     | _ -> assert false
     | exception exn -> assert (phys_equal exn sentinel));
    (match parse dir sources ~observe:(fun _ -> raise sentinel) with
     | _ -> assert false
     | exception exn -> assert (phys_equal exn sentinel));
    let calls = ref 0 in
    ignore (parse dir sources ~checkpoint:(fun () -> incr calls) : CM.Parsed.t);
    assert (!calls > 0);
    List.iter
      (List.range 1 (!calls + 1))
      ~f:(fun interrupt ->
        let current = ref 0 in
        match
          parse dir sources ~checkpoint:(fun () ->
            incr current;
            if !current = interrupt then raise sentinel)
        with
        | _ -> assert false
        | exception exn -> assert (phys_equal exn sentinel));
    let reused = parse dir sources in
    assert (List.length (CM.Parsed.agent_references reused) = 1))
;;

let%test_unit "ordinary legacy AST agrees with owned conversion without metadata in wire" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let root =
      {|<tool name="a" agent="child.chatmd" local/>
<user><agent src="child.chatmd" local>same</agent></user>|}
    in
    let sources = [ "root.chatmd", root ] in
    let legacy =
      CM.parse_chat_inputs
        ~source:"root.chatmd"
        ~source_loader:(Source_loader.captured_filesystem ~root:dir ~sources)
        ~dir
        root
    in
    let owned = parse dir sources in
    assert (
      List.equal
        (fun a b -> CM.compare_top_level_elements a b = 0)
        legacy
        (CM.Parsed.elements owned));
    assert (
      Jsonaf.exactly_equal
        (`Array (List.map legacy ~f:CM.jsonaf_of_top_level_elements))
        (`Array (List.map (CM.Parsed.elements owned) ~f:CM.jsonaf_of_top_level_elements))))
;;

let%expect_test "projected nested content agents retain their exact original references" =
  Eio_main.run (fun env ->
    let opening = {|<agent src="../agents/child.chatmd" local>|} in
    let nested = {|<agent src="../agents/grandchild.chatmd" local>nested</agent>|} in
    let outer = opening ^ {|<doc src="note.txt" local capture/>|} ^ nested ^ "</agent>" in
    let repeated = opening ^ "other</agent>" in
    let parsed =
      parse
        env#cwd
        [ "root.chatmd", {|<user><import src="parts/content.chatmd"/></user>|}
        ; "parts/content.chatmd", outer ^ repeated
        ]
    in
    let project rendered =
      CM.materialize_documents
        parsed
        ~rendered
        ~max_occurrences:1
        ~max_output_bytes:64
        ~max_work_steps:10000
        ~checkpoint:ignore
      |> Or_error.ok_exn
    in
    let projection =
      project [ List.hd_exn (CM.Parsed.document_occurrences parsed), "CAPTURED" ]
    in
    let rec content_agents (elements : CM.content_item list) =
      List.concat_map elements ~f:(function
        | CM.Agent agent -> agent :: content_agents agent.items
        | Basic _ -> [])
    in
    let projected =
      match CM.Document_projection.elements projection with
      | [ CM.User { content = Some (Items items); _ } ] -> content_agents items
      | _ -> failwith "expected projected user content"
    in
    let original = CM.Parsed.agent_references parsed in
    [%test_eq: int] (List.length projected) 3;
    List.iter2_exn
      projected
      [ 0, "agents/child.chatmd"
      ; ( String.length opening + String.length {|<doc src="note.txt" local capture/>|}
        , "agents/grandchild.chatmd" )
      ; String.length outer, "agents/child.chatmd"
      ]
      ~f:(fun descriptor (offset, target) ->
        (* Parser metadata emission order is not projected-content traversal
           order. The handwritten byte-piece model identifies the original tag. *)
        let expected =
          List.find_exn original ~f:(fun reference ->
            Int.equal (Input.Range.start_byte (A.range reference)) offset)
        in
        let lookup ?(checkpoint = ignore) ?(max_work_steps = 100) () =
          CM.content_agent_reference
            parsed
            ~projection
            ~max_work_steps
            ~checkpoint
            descriptor
        in
        let actual = lookup () |> Or_error.ok_exn in
        assert (phys_equal actual expected);
        [%test_eq: int] (Input.Range.start_byte (A.range actual)) offset;
        [%test_eq: string option] (A.queued_key actual) (Some target);
        assert (
          Result.is_error
            (CM.content_agent_reference
               parsed
               ~max_work_steps:100
               ~checkpoint:ignore
               descriptor));
        assert (Result.is_error (lookup ~max_work_steps:1 ()));
        let calls = ref 0 in
        ignore (lookup ~checkpoint:(fun () -> Int.incr calls) () |> Or_error.ok_exn : A.t);
        assert (!calls > 0);
        let sentinel = Failure "projected content checkpoint" in
        List.iter
          (List.range 1 (!calls + 1))
          ~f:(fun interrupt ->
            let current = ref 0 in
            match
              lookup
                ~checkpoint:(fun () ->
                  Int.incr current;
                  if !current = interrupt then raise sentinel)
                ()
            with
            | _ -> assert false
            | exception exn -> assert (phys_equal exn sentinel));
        assert (phys_equal (lookup () |> Or_error.ok_exn) expected);
        print_s [%sexp (offset : int), (target : string)]);
    let foreign = parse env#cwd [ "root.chatmd", {|<user>foreign</user>|} ] in
    assert (
      Result.is_error
        (CM.content_agent_reference
           foreign
           ~projection
           ~max_work_steps:100
           ~checkpoint:ignore
           (List.hd_exn projected)));
    let original_reference = List.hd_exn original in
    let original_descriptor =
      match A.invocation original_reference with
      | Content value -> value
      | Tool _ | Persistent_tool _ -> assert false
    in
    assert (
      phys_equal
        (CM.content_agent_reference
           parsed
           ~max_work_steps:100
           ~checkpoint:ignore
           original_descriptor
         |> Or_error.ok_exn)
        original_reference));
  [%expect
    {|
    (0 agents/child.chatmd)
    (77 agents/grandchild.chatmd)
    (146 agents/child.chatmd)
    |}]
;;

let%test_unit "source mode admission is exact and captured never silently becomes legacy" =
  Eio_main.run (fun env ->
    let dir = env#cwd in
    List.iter
      [ {|<tool name="a" agent="child.chatmd" local source_mode/>|}
      ; {|<tool name="a" agent="child.chatmd" local source_mode="other"/>|}
      ; {|<tool name="a" agent="child.chatmd" local source_mode="file" source_mode="captured"/>|}
      ; {|<tool name="read_file" source_mode="file"/>|}
      ; {|<user><agent src="child.chatmd" local source_mode="FILE"/></user>|}
      ; {|<user><agent src="https://example.invalid/a" source_mode="captured"/></user>|}
      ]
      ~f:(fun root ->
        match parse dir [ "root.chatmd", root ] with
        | _ -> assert false
        | exception Failure _ -> ());
    List.iter
      [ {|<tool name="a" agent="child.chatmd" local source_mode="captured"/>|}
      ; {|<user><agent src="child.chatmd" local source_mode="captured"/></user>|}
      ]
      ~f:(fun root ->
        let parsed = parse dir [ "root.chatmd", root ] in
        assert (
          A.Source_mode.equal
            Captured
            (A.source_mode (List.hd_exn (CM.Parsed.agent_references parsed))));
        match
          CM.parse_chat_inputs
            ~source:"root.chatmd"
            ~source_loader:
              (Source_loader.captured_filesystem
                 ~root:dir
                 ~sources:[ "root.chatmd", root ])
            ~dir
            root
        with
        | _ -> assert false
        | exception Failure _ -> ()))
;;
