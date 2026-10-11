open Core
module CM = Prompt.Chat_markdown
module C = Chatml_prompt_programs
module P = Chatml_prepared_program
module S = Chatml_source
module Contract = Chatml.Chatml_compiler_contract
module EC = Chat_response.Extension_compiler
module L = Chatml.Chatml_lang

let ok = function
  | Ok value -> value
  | Error _ -> failwith "valid routing fixture rejected"
;;

let diagnostics = function
  | Ok value -> value
  | Error errors ->
    failwith (Sexp.to_string_hum ([%sexp_of: Chatmd_shell_spec.Diagnostic.t list] errors))
;;

let checkpoint = ignore
let body = "let run = fun ctx input -> Task.pure(`Complete(`String(Graph.value)))"

let limits =
  C.Limits.create
    ~max_programs:8
    ~max_parser_inputs:16
    ~max_logical_sources:32
    ~max_total_input_bytes:(8 * 1024 * 1024)
    ~project_work_steps:1_000_000
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let graph ?(moderator = false) ?contract dependency =
  let contract =
    Option.value_or_thunk contract ~default:(fun () ->
      Contract.for_target (if moderator then Moderator_v1 else Tool_v1) ~checkpoint)
  in
  let digest = Contract.fingerprint contract ~checkpoint in
  let body =
    if moderator
    then
      "let initial_state = Graph.value\n\
       let on_event = fun ctx state event -> Task.pure(state)"
    else body
  in
  let source path contents =
    S.create
      (S.Id.create ~package_namespace:"routing" ~relative_path:path |> Or_error.ok_exn)
      ~contents
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint
    |> Or_error.ok_exn
  in
  let entry = source "main.chatml" body in
  let implementation = source "graph.chatml" dependency in
  let location =
    S.locate entry ~start_byte:0 ~end_byte:0 ~checkpoint |> Or_error.ok_exn
  in
  let definition =
    P.Definition.create
      ~implementation:(S.id implementation)
      ~interface:None
      ~declared_at:location
    |> Or_error.ok_exn
  in
  let scopes =
    List.map [ entry; implementation ] ~f:(fun source ->
      P.Scope.create
        (S.id source)
        ~bindings:
          (if S.Id.equal (S.id source) (S.id entry)
           then
             [ P.Scope.
                 { name = "Graph"
                 ; module_id = P.Definition.id definition
                 ; declared_at = location
                 }
             ]
           else [])
        ~selected_at:None
        ~checkpoint
      |> Or_error.ok_exn)
  in
  P.create
    ~max_input_bytes:None
    ~compiler_contract:digest
    ~environment:(P.Environment.implicit ~required_contract:digest)
    ~provenance:Implicit
    ~entry:(S.id entry)
    ~sources:[ entry; implementation ]
    ~definitions:[ definition ]
    ~scopes
    ~checkpoint
    ()
  |> ok
;;

let fixture ?(moderator = false) ?legacy_surface env =
  let contract =
    match legacy_surface with
    | Some surface -> Chat_response.Moderator_program.contract surface ~checkpoint
    | None ->
      Contract.for_target (if moderator then Moderator_v1 else Tool_v1) ~checkpoint
  in
  let digest = Contract.fingerprint contract ~checkpoint in
  let dir = Eio.Stdenv.cwd env in
  let raw =
    if moderator
    then
      "<script id=\"handler\" language=\"chatml\" kind=\"moderator\" "
      ^ (if Option.is_some legacy_surface then "" else "api=\"extensibility-v1\"")
      ^ ">let initial_state = Graph.value\n\
         let on_event = fun ctx state event -> Task.pure(state)</script>"
    else
      "<script id=\"handler\" language=\"chatml\" kind=\"tool\">"
      ^ body
      ^ "</script><tool name=\"custom\" type=\"chatml\" script=\"handler\" \
         entrypoint=\"run\" input_schema=\"schema.json\" output_schema=\"schema.json\"/>"
  in
  let sources = [ "root.chatmd", raw; "schema.json", "true" ] in
  let authored =
    Chatmd_source_bundle.create ~root_file:"root.chatmd" ~sources ()
    |> Result.ok_or_failwith
  in
  let parsed =
    CM.prepare_chat_inputs
      ~source:"root.chatmd"
      ~source_loader:(Source_loader.captured_filesystem ~root:dir ~sources)
      ~dir
      ~preprocess:Fn.id
      ~limits:
        (CM.Limits.create
           ~max_input_bytes:65536
           ~max_parse_calls:16
           ~max_tokens:10000
           ~max_depth:32
         |> Or_error.ok_exn)
      ~checkpoint
      raw
  in
  let declaration = List.hd_exn (CM.Parsed.declarations parsed) in
  let binding =
    C.Binding.of_declaration parsed declaration ~checkpoint |> Or_error.ok_exn
  in
  let restore prepared =
    let custody =
      C.capture
        authored
        ~parsed_contexts:[ parsed ]
        ~root:(CM.Parsed.root_input parsed)
        ~programs:
          (Option.to_list
             (Option.map prepared ~f:(fun graph -> binding, C.Input.Provided graph)))
        ~active_programs:
          (Option.to_list (Option.map prepared ~f:(fun _ -> binding, digest)))
        ~limits
        ~checkpoint
      |> ok
    in
    C.restore
      custody
      ~parsed
      ~contracts:(Option.to_list (Option.map prepared ~f:(fun _ -> binding, contract)))
      ~limits
      ~checkpoint
    |> ok
  in
  let capabilities =
    Chat_response.Tool_capability.create
      ~owner:"routing"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "routing")
      []
    |> ok
  in
  CM.Parsed.elements parsed, capabilities, restore
;;

let compile env elements capabilities programs =
  EC.prepare_definition_in_domain ~env ~capabilities ~programs elements
;;

let execute definition =
  let prepared = List.hd_exn (EC.prepared_tools definition) in
  let config =
    Chatml_host_runtime.default_runtime_config
      ~surface:Chatml.Chatml_extension_surface.tool_v1
      ()
  in
  EC.Program.run_in_scope
    (EC.program prepared)
    ~control:None
    ~config
    ~entrypoint:"run"
    ~arguments:[ L.VUnit; L.VVariant ("Null", []) ]
    ()
  |> ok
  |> function
  | L.VVariant ("Complete", [ L.VVariant ("String", [ L.VString value ]) ]) -> value
  | _ -> failwith "unexpected standalone result"
;;

let%expect_test "captured dependencies determine execution and implementation identity" =
  Eio_main.run (fun env ->
    let elements, capabilities, restore = fixture env in
    let first =
      compile env elements capabilities (restore (Some (graph "let value = \"3\"")))
      |> diagnostics
    in
    let second =
      compile env elements capabilities (restore (Some (graph "let value = \"7\"")))
      |> diagnostics
    in
    printf
      "first=%s changed=%s retained=%s\n"
      (execute first)
      (execute second)
      (execute first);
    printf
      "same declaration, different implementation=%b\n"
      (not
         (String.equal
            (EC.definition_fingerprint first)
            (EC.definition_fingerprint second))));
  [%expect
    {| 
    first=3 changed=7 retained=3
    same declaration, different implementation=true |}]
;;

let%test_unit "strict custody and initializer-free admission" =
  Eio_main.run (fun env ->
    let elements, capabilities, restore = fixture env in
    let expect_code code = function
      | Error errors ->
        assert (
          List.exists errors ~f:(fun e ->
            String.equal e.Chatmd_shell_spec.Diagnostic.code code))
      | Ok _ -> failwith "invalid custody accepted"
    in
    compile env elements capabilities (restore None)
    |> expect_code "chatml.missing_program";
    let changed =
      List.map elements ~f:(function
        | CM.Extension_script script ->
          let source = "let run = fun ctx input -> Task.pure(`Complete(input))" in
          CM.Extension_script
            { script with
              source = Inline source
            ; source_sha256 = Chatmd_shell_spec.Source_ref.digest source
            }
        | element -> element)
    in
    compile env changed capabilities (restore (Some (graph "let value = \"3\"")))
    |> expect_code "chatml.source_mismatch";
    let poisoned =
      graph "let poison = fail(\"initializer reached\")\nlet value = \"3\""
    in
    ignore
      (compile env elements capabilities (restore (Some poisoned)) |> diagnostics
       : EC.definition);
    let configured =
      List.map elements ~f:(function
        | CM.Extension_script script ->
          CM.Extension_script
            { script with
              project_selection =
                Some { project = "./chatml.project.json"; environment = None }
            }
        | element -> element)
    in
    EC.prepare_definition_in_domain ~env ~capabilities configured
    |> expect_code "chatml.missing_program";
    compile env configured capabilities (restore (Some (graph "let value = \"3\"")))
    |> expect_code "chatml.project_mismatch")
;;

let%test_unit "moderator snapshots and deferred observers use the entire graph revision" =
  Eio_main.run (fun env ->
    let module M = Chat_response.Moderator_manager in
    let elements, capabilities, restore = fixture ~moderator:true env in
    let build dependency =
      compile
        env
        elements
        capabilities
        (restore (Some (graph ~moderator:true dependency)))
      |> diagnostics
    in
    let artifact definition =
      M.Registry.of_definition M.Registry.empty definition
      |> Result.ok_or_failwith
      |> snd
      |> Option.value_exn
    in
    let first = build "let value = \"first\"" in
    let manager =
      M.create
        ~artifact:(artifact first)
        ~capabilities:Chat_response.Moderation.Capabilities.default
        ~env
        ()
      |> Result.ok_or_failwith
    in
    let snapshot = M.snapshot manager |> Result.ok_or_failwith in
    let observer = M.invocation_observer manager |> Option.value_exn in
    assert (String.equal observer.source_sha256 snapshot.script_source_hash);
    let script, program = List.hd_exn (EC.compiled_scripts first) in
    assert (not (String.equal script.source_sha256 observer.source_sha256));
    assert (
      String.equal
        observer.source_sha256
        (EC.Program.source_revision program ~single_file_revision:script.source_sha256));
    ignore
      (M.create
         ~artifact:(artifact first)
         ~capabilities:Chat_response.Moderation.Capabilities.default
         ~env
         ~snapshot
         ()
       |> Result.ok_or_failwith
       : M.t);
    let changed =
      build "let poison = fail(\"initializer must not run\")\nlet value = \"second\""
    in
    match
      M.create
        ~artifact:(artifact changed)
        ~capabilities:Chat_response.Moderation.Capabilities.default
        ~env
        ~snapshot
        ()
    with
    | Ok _ -> failwith "changed module graph accepted old snapshot"
    | Error message ->
      assert (String.is_prefix message ~prefix:"Moderator snapshot source hash"))
;;

let%expect_test "legacy moderator projects retain their graph and host surface" =
  Eio_main.run (fun env ->
    let module M = Chat_response.Moderator_manager in
    let module Surface = Chatml.Chatml_builtin_surface in
    List.iter
      [ "headless", Surface.moderator_surface
      ; "interactive", Surface.ui_moderator_surface
      ]
      ~f:(fun (name, surface) ->
        let elements, capabilities, restore =
          fixture ~moderator:true ~legacy_surface:surface env
        in
        let script =
          List.find_map_exn elements ~f:(function
            | CM.Script script -> Some script
            | _ -> None)
        in
        let contract = Chat_response.Moderator_program.contract surface ~checkpoint in
        let prepare dependency =
          let programs = restore (Some (graph ~moderator:true ~contract dependency)) in
          M.Registry.prepare_script
            ~surface
            M.Registry.empty
            script
            ~programs
            ~capabilities
            ~env
            ~checkpoint
            ~limits:
              { Chatml_compilation.default_limits with
                max_source_bytes = 8 * 1024 * 1024
              }
            ~max_dependency_depth:128
          |> Result.ok_or_failwith
          |> snd
        in
        let artifact = prepare "let value = \"first\"" in
        let create ?snapshot artifact =
          M.create
            ~artifact
            ~capabilities:Chat_response.Moderation.Capabilities.default
            ~env
            ?snapshot
            ()
        in
        let manager = create artifact |> Result.ok_or_failwith in
        let snapshot = M.snapshot manager |> Result.ok_or_failwith in
        let resumed = create ~snapshot artifact |> Result.ok_or_failwith in
        let restored = M.snapshot resumed |> Result.ok_or_failwith in
        assert (String.equal snapshot.script_source_hash restored.script_source_hash);
        assert (Option.is_none (M.invocation_observer manager));
        let changed =
          prepare
            "let poison = fail(\"initializer must not run\")\nlet value = \"second\""
        in
        (match create ~snapshot changed with
         | Error message ->
           assert (String.is_prefix message ~prefix:"Moderator snapshot source hash")
         | Ok _ -> failwith "legacy changed graph accepted old state");
        let configured =
          { script with
            project_selection =
              Some { project = "./chatml.project.json"; environment = None }
          }
        in
        (match M.Registry.compile_script ~surface M.Registry.empty configured with
         | Error message ->
           assert (String.is_prefix message ~prefix:"chatml.missing_program:")
         | Ok _ -> failwith "configured legacy moderator silently ignored project");
        printf
          "%s: restored captured graph; changed graph rejected before initialization\n"
          name));
  [%expect
    {| 
    headless: restored captured graph; changed graph rejected before initialization
    interactive: restored captured graph; changed graph rejected before initialization |}]
;;
