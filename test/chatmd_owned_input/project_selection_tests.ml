open Core
module CM = Prompt.Chat_markdown
module Selection = Chatmd_shell_spec.Script_project
module Input = Chatmd_parser_input

let selection declaration =
  CM.Declaration.project_selection declaration |> Option.value_exn
;;

let%expect_test "each script owns its project selection and declaring input" =
  Eio_main.run (fun env ->
    let parsed =
      Input_tests.prepare
        (Eio.Stdenv.cwd env)
        [ ( "root.chatmd"
          , {|<script id="main" language="chatml" kind="moderator" project="./orchestration/chatml.project.json" environment="main">let initial_state = 0</script>
<import src="parts/library.chatmd" namespace="lib"/>
<script id="gate" language="chatml" kind="shell_reviewer" project="./gates/chatml.project.json">let initial_state = 0</script>|}
          )
        ; ( "parts/library.chatmd"
          , {|<script id="lookup" language="chatml" kind="tool" src="helper.chatml" project="./tools/chatml.project.json" environment="lookup"/>|}
          )
        ; "parts/helper.chatml", "let run ctx input = Task.pure(`Complete(input))"
        ]
    in
    List.iter (CM.Parsed.declarations parsed) ~f:(fun declaration ->
      let selected = selection declaration in
      print_s
        [%sexp
          (CM.Declaration.script_id declaration : string)
        , (Input.path (Input.Range.input (CM.Declaration.range declaration)) : string)
        , (selected.project : string)
        , (selected.environment : string option)
        , (CM.Declaration.script_path declaration : string option)]));
  [%expect
    {|
    (main root.chatmd ./orchestration/chatml.project.json (main) ())
    (lib:lookup parts/library.chatmd ./tools/chatml.project.json (lookup)
     (helper.chatml))
    (gate root.chatmd ./gates/chatml.project.json () ())
    |}]
;;

let%test_unit "all script roles retain authored selection through their wire projection" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let roles =
      [ "moderator", ""
      ; "moderator", {| api="extensibility-v1"|}
      ; "tool", ""
      ; "shell_matcher", ""
      ; "shell_reviewer", ""
      ; "shell_before_interceptor", ""
      ; "shell_after_interceptor", ""
      ; "shell_effect_analyzer", ""
      ; "shell_audit_filter", ""
      ]
    in
    List.iter roles ~f:(fun (kind, api) ->
      List.iter [ false; true ] ~f:(fun external_source ->
        let ending =
          if external_source then {| src="code.chatml"/>|} else ">body</script>"
        in
        let text =
          sprintf
            "<script id=\"entry\" language=\"chatml\" kind=\"%s\"%s \
             project=\"./p&amp;q/chatml.project.json\" environment=\"test\"%s"
            kind
            api
            ending
        in
        let parse text =
          Input_tests.prepare dir [ "root.chatmd", text; "code.chatml", "body" ]
        in
        let parsed = parse text in
        let expected = selection (List.hd_exn (CM.Parsed.declarations parsed)) in
        [%test_eq: string] expected.project "./p&q/chatml.project.json";
        let element = List.hd_exn (CM.Parsed.elements parsed) in
        let decoded =
          CM.top_level_elements_of_jsonaf (CM.jsonaf_of_top_level_elements element)
        in
        let selected, encoded =
          match decoded with
          | CM.Script script -> script.project_selection, None
          | Shell_script script ->
            script.project_selection, Some (Chatmd_script_declaration.serialize script)
          | Extension_script script ->
            ( script.project_selection
            , Some (Chatmd_extension_declaration.serialize_script script) )
          | _ -> failwith "script projection"
        in
        assert (Option.equal Selection.equal selected (Some expected));
        Option.iter encoded ~f:(fun text ->
          let restored = parse text in
          assert (
            Selection.equal
              expected
              (selection (List.hd_exn (CM.Parsed.declarations restored))))))))
;;

let%test_unit "environment requires a project and ordinary scripts remain unselected" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let parse attributes =
      Input_tests.prepare
        dir
        [ ( "root.chatmd"
          , sprintf
              "<script id=\"entry\" language=\"chatml\" kind=\"tool\" %s>body</script>"
              attributes )
        ]
    in
    let ordinary = parse "" in
    assert (
      Option.is_none
        (CM.Declaration.project_selection (List.hd_exn (CM.Parsed.declarations ordinary))));
    List.iter
      [ {|environment="test"|}; {|project=""|}; {|project="p.json" environment=" "|} ]
      ~f:(fun attributes ->
        match parse attributes with
        | _ -> failwith "invalid project selection accepted"
        | exception Failure message ->
          assert (String.is_substring message ~substring:"chatmd.script_invalid_project")))
;;
