open Core
module CM = Prompt.Chat_markdown
module X = Chatmd_shell_spec.Extension_spec

let get = function
  | Ok value -> value
  | Error errors -> raise_s [%sexp (errors : Chatmd_shell_spec.Diagnostic.t list)]
;;

let with_fixture f =
  Eio_main.run (fun env ->
    let root =
      Filename.temp_dir_name
      ^ "/ochat-extension-"
      ^ Int.to_string (Random.int 1_000_000_000)
    in
    let dir = Eio.Path.(Eio.Stdenv.fs env / root) in
    Eio.Path.mkdir ~perm:0o700 dir;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
      ~f:(fun () ->
        let write path text =
          Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(dir / path) text
        in
        write "schema.json" {|{"type":"object","additionalProperties":false}|};
        write "tool.chatml" "let run = fun ctx input -> Task.pure(`Complete(input))";
        f dir write))
;;

let parse ?source_loader dir text =
  CM.parse_chat_inputs ~source:"root.chatmd" ?source_loader ~dir text
;;

let rejected f =
  try
    ignore (f ());
    false
  with
  | _ -> true
;;

let standalone =
  {|<script id="worker" language="chatml" kind="tool" src="tool.chatml"/><tool name="do_work" type="chatml" script="worker" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="read_file"/></tool><tool name="read_file"/>|}
;;

let moderator api =
  sprintf
    {|<script id="owner" language="chatml" kind="moderator" %s>let initial_state = {}</script><tool name="watch" type="moderator" moderator="owner" input_schema="schema.json" output_schema="schema.json"/>|}
    api
;;

let%test_unit
    "extension declarations round-trip and omit source code from tool attributes"
  =
  with_fixture (fun dir _ ->
    let elements = parse dir standalone in
    let tool =
      List.find_map_exn elements ~f:(function
        | CM.Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    assert (List.equal String.equal tool.uses [ "read_file" ]);
    let script =
      List.find_map_exn elements ~f:(function
        | CM.Extension_script script -> Some script
        | _ -> None)
    in
    assert (X.equal_script_kind script.kind Tool_script);
    assert (
      String.equal
        script.source_sha256
        (Chatmd_shell_spec.Source_ref.digest (X.script_text script)));
    let encoded =
      String.concat
        (List.filter_map elements ~f:(function
           | CM.Tool (Extension tool) ->
             Some (Chatmd_extension_declaration.serialize_tool tool)
           | CM.Extension_script script ->
             Some (Chatmd_extension_declaration.serialize_script script)
           | Tool (Read_file _) -> Some {|<tool name="read_file"/>|}
           | _ -> None))
    in
    let restored = parse dir encoded in
    assert (List.length restored = 3);
    let restored =
      List.find_map_exn restored ~f:(function
        | CM.Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    assert (String.equal restored.input_schema.source_text tool.input_schema.source_text);
    assert (List.equal String.equal restored.uses tool.uses);
    assert (
      not
        (String.is_substring
           (Chatmd_extension_declaration.serialize_tool tool)
           ~substring:"Task.pure")))
;;

let%test_unit "moderator tools require their actual v1 handler and one owner" =
  with_fixture (fun dir _ ->
    ignore (parse dir (moderator {|api="extensibility-v1"|}) : CM.top_level_elements list);
    assert (rejected (fun () -> parse dir (moderator "")));
    assert (rejected (fun () -> parse dir (moderator {|api="future"|})));
    assert (
      rejected (fun () ->
        parse
          dir
          (moderator {|api="extensibility-v1"|}
           ^ {|<script id="another" language="chatml" kind="moderator">x</script>|})));
    assert (
      rejected (fun () ->
        parse
          dir
          {|<tool name="watch" type="moderator" moderator="missing" input_schema="schema.json" output_schema="schema.json"/>|})))
;;

let%test_unit "strict binding and capability errors are rejected before dependency reads" =
  with_fixture (fun dir _ ->
    let reads = ref 0 in
    let loader =
      Source_loader.filesystem ~root:dir
      |> Source_loader.with_observer ~f:(fun _ _ -> Int.incr reads)
    in
    List.iter
      [ {|<tool name="bad" type="chatml" script="x" entrypoint="run" command="touch /tmp/should-not-run" input_schema="schema.json" output_schema="schema.json"/>|}
      ; {|<tool name="bad" name="other" type="chatml" script="x" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|}
      ; {|<tool name="bad" type="chatml" script="x" entrypoint="run" moderator="x" input_schema="schema.json" output_schema="schema.json"/>|}
      ; {|<tool name="bad" type="chatml" script="x" entrypoint="wrong" input_schema="schema.json" output_schema="schema.json"/>|}
      ; {|<tool name="bad" type="chatml" script="x" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="read_file"/><uses tool="read_file"/></tool>|}
      ; {|<tool name="bad" type="moderator" moderator="x" input_schema="schema.json" output_schema="schema.json"><uses tool="read_file"/></tool>|}
      ]
      ~f:(fun text -> assert (rejected (fun () -> parse ~source_loader:loader dir text)));
    assert (!reads = 0))
;;

let%test_unit "duplicate tools, script IDs, and dependency cycles fail" =
  with_fixture (fun dir _ ->
    assert (rejected (fun () -> parse dir (standalone ^ {|<tool name="do_work"/>|})));
    assert (
      rejected (fun () ->
        parse
          dir
          (standalone
           ^ {|<script id="worker" language="chatml" kind="moderator">x</script>|})));
    let cyclic =
      String.substr_replace_all
        standalone
        ~pattern:{|uses tool="read_file"|}
        ~with_:{|uses tool="do_work"|}
    in
    assert (rejected (fun () -> parse dir cyclic));
    let without_uses =
      String.substr_replace_all standalone ~pattern:{|<uses tool="read_file"/>|} ~with_:""
    in
    let tools = parse dir without_uses in
    let tool =
      List.find_map_exn tools ~f:(function
        | CM.Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    assert (List.is_empty tool.uses))
;;

let%test_unit "authoring policy is explicit and old inline markup remains text" =
  with_fixture (fun dir _ ->
    List.iter
      [ {|<authoring_context policy="auto"/>|}
      ; {|<authoring_context policy="manual"/>|}
      ; {|<authoring_context policy="preload" topics="language.records runtime.tasks"/>|}
      ]
      ~f:(fun text ->
        match parse dir text with
        | [ CM.Authoring_context policy ] ->
          ignore
            (parse dir (Chatmd_extension_declaration.serialize_authoring policy)
             : CM.top_level_elements list)
        | _ -> failwith "missing policy");
    List.iter
      [ {|<authoring_context policy="auto" topics="x"/>|}
      ; {|<authoring_context policy="preload"/>|}
      ; {|<authoring_context policy="preload" topics="x x"/>|}
      ; {|<authoring_context policy="manual"/><authoring_context policy="auto"/>|}
      ]
      ~f:(fun text -> assert (rejected (fun () -> parse dir text)));
    let ast =
      Chatmd_parser.document
        (Chatmd_lexer.create ())
        (Lexing.from_string
           {|<developer><uses tool="read_file"/><authoring_context policy="manual"/></developer>|})
    in
    match ast with
    | [ Chatmd_ast.Element (Developer, _, children) ] ->
      assert (
        List.for_all children ~f:(function
          | Chatmd_ast.Text _ -> true
          | _ -> false))
    | _ -> failwith "unexpected markup")
;;

let%test_unit "custom authoring help is strict, captured, and uses exact callable names" =
  with_fixture (fun dir write ->
    let source =
      {|<authoring_help tool="author" package="one-off" tasks="one_off_script child_agent" topics="chatml/basics chatmd/children" required_helpers="ochat_validate"/>|}
    in
    write "help.chatmd" source;
    let parsed = parse dir {|<import src="help.chatmd" namespace="scope"/>|} in
    let declaration =
      match parsed with
      | [ CM.Authoring_help help ] -> help
      | _ -> failwith "missing authoring help"
    in
    assert (String.equal declaration.tool "author");
    assert (String.equal declaration.source_ref.file "help.chatmd");
    assert (Option.equal String.equal declaration.source_ref.namespace (Some "scope"));
    (match parse dir (Chatmd_extension_declaration.serialize_help declaration) with
     | [ CM.Authoring_help reparsed ] ->
       assert (
         Chatmd_shell_spec.Authoring_metadata.equal_help declaration.help reparsed.help)
     | _ -> failwith "help round-trip failed");
    List.iter
      [ String.substr_replace_all source ~pattern:"child_agent" ~with_:"unknown"
      ; String.substr_replace_all source ~pattern:"ochat_validate" ~with_:"shell"
      ; String.substr_replace_all
          source
          ~pattern:"chatml/basics chatmd/children"
          ~with_:"chatml/basics chatml/basics"
      ; String.substr_replace_first
          source
          ~pattern:"tool="
          ~with_:"helper=\"reference\" tool="
      ; String.substr_replace_first source ~pattern:"tool=" ~with_:"tool=\"other\" tool="
      ; String.substr_replace_first source ~pattern:"/>" ~with_:">body</authoring_help>"
      ; source ^ source
      ]
      ~f:(fun invalid -> assert (rejected (fun () -> parse dir invalid)));
    match parse dir ("<user>Example: " ^ source ^ "</user>") with
    | [ CM.User { content = Some (Text text); _ } ] ->
      assert (String.is_substring text ~substring:"<authoring_help")
    | _ -> failwith "inline help markup is not ordinary text")
;;

let%test_unit "bounded dependency reading observes only successful complete reads" =
  with_fixture (fun dir write ->
    write "large.txt" (String.make 8192 'x');
    let observed = ref [] in
    let loader =
      Source_loader.filesystem ~root:dir
      |> Source_loader.with_observer ~f:(fun source text ->
        observed := (Source_loader.relative_path source, text) :: !observed)
    in
    let source = Source_loader.root loader ~file:"large.txt" |> Result.ok_or_failwith in
    assert (Result.is_error (Source_loader.read_bounded ~max_bytes:4096 loader source));
    assert (List.is_empty !observed);
    assert (
      String.length
        (Source_loader.read_bounded ~max_bytes:8192 loader source |> Result.ok_or_failwith)
      = 8192);
    assert (List.length !observed = 1);
    List.iter
      [ "../outside"; "/absolute"; "https://example.com/schema" ]
      ~f:(fun reference ->
        assert (
          Result.is_error
            (Source_loader.resolve_within_root loader ~base:source ~reference))))
;;

let%test_unit
    "imported schema/script closure uses captured bytes and qualified handler IDs"
  =
  with_fixture (fun dir write ->
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(dir / "nested");
    let imported =
      String.substr_replace_all standalone ~pattern:{|<tool name="read_file"/>|} ~with_:""
    in
    write "nested/definitions.chatmd" imported;
    write "nested/schema.json" "true";
    write "nested/tool.chatml" "let run = fun ctx input -> Task.pure(`Complete(input))";
    let captured = Hashtbl.create (module String) in
    let loader =
      Source_loader.filesystem ~root:dir
      |> Source_loader.with_observer ~f:(fun source text ->
        Hashtbl.set captured ~key:(Source_loader.relative_path source) ~data:text)
    in
    let root =
      {|<import src="nested/definitions.chatmd" namespace="lab"/><tool name="read_file"/>|}
    in
    let elements = parse ~source_loader:loader dir root in
    let tool =
      List.find_map_exn elements ~f:(function
        | CM.Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    (match tool.implementation with
     | Standalone { script; _ } -> assert (String.equal script "lab:worker")
     | _ -> assert false);
    assert (String.equal tool.input_schema.source_ref.file "nested/schema.json");
    assert (Hashtbl.length captured = 3);
    write "nested/schema.json" "false";
    write "nested/tool.chatml" "corrupted live file";
    let pinned =
      Source_loader.captured_filesystem ~root:dir ~sources:(Hashtbl.to_alist captured)
    in
    let pinned_elements = parse ~source_loader:pinned dir root in
    let pinned_tool =
      List.find_map_exn pinned_elements ~f:(function
        | CM.Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    assert (String.equal pinned_tool.input_schema.source_text "true");
    let broken =
      Source_loader.captured_filesystem
        ~root:dir
        ~sources:
          (Hashtbl.to_alist captured
           |> List.filter ~f:(fun (path, _) ->
             not (String.equal path "nested/schema.json")))
    in
    assert (rejected (fun () -> parse ~source_loader:broken dir root));
    assert (
      Result.is_error
        (X.validate_schema { pinned_tool.input_schema with source_text = "false" })))
;;

let%test_unit "old moderator binary encoding keeps its original layout" =
  let script : CM.top_level_elements =
    Script { id = "main"; language = "chatml"; kind = "moderator"; source = Inline "x" }
  in
  let encoded =
    Bin_prot.Utils.bin_dump ~header:false CM.bin_writer_top_level_elements script
    |> Bigstring.to_string
  in
  assert (String.equal encoded "\012\004main\006chatml\009moderator\000\001x")
;;
