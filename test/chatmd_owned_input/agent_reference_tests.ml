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
      {|<tool name="keep" agent="../agents/a.chatmd" local persistence="optional"/>|}
    in
    let child = {|<agent src="../agents/b.chatmd" local>child</agent>|} in
    let opening = {|<agent src="../agents/a.chatmd" local>parent|} in
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
           && Option.equal String.equal (A.queued_key a) (A.queued_key b))
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
