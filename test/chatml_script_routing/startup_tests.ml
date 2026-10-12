open Core
module Fixture = Routing_tests
module CM = Prompt.Chat_markdown
module R = Chat_response.Agent_runtime
module C = Chatml_prompt_programs
module B = Chatml_project_build
module Source = Chatml_source
module Contract = Chatml.Chatml_compiler_contract
module Lang = Chatml.Chatml_lang

let checkpoint = ignore
let ok = Fixture.ok

let startup_ok = function
  | Ok value -> value
  | Error errors -> raise_s [%sexp (errors : R.diagnostic list)]
;;

let captured path contents =
  Source.create
    (Source.Id.create ~package_namespace:"routing" ~relative_path:path |> Or_error.ok_exn)
    ~contents
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint
  |> Or_error.ok_exn
;;

(* A real selected library receipt carries the hook's tool requirements. Its
   module inventory and entry are supplied as immutable captures, without native
   acquisition or a synthesized lock. *)
let selected_hook ~contract ~required_tool ~body dependency =
  let project =
    captured
      "chatml.project.json"
      (sprintf
         {|{"schema_version":1,"namespace":"routing","source_dirs":["."],"capabilities":["%s"]}|}
         required_tool)
    |> fun source ->
    Chatml_project.decode source ~limits:Document_schema.Limits.default ~checkpoint |> ok
  in
  let discovery =
    Chatml_catalog.Plan.Discovery.create
      project
      ~request:(Library { environment = None })
      ~checkpoint
    |> ok
  in
  let directory = List.hd_exn (Chatml_project.source_dirs project) in
  let observations =
    Chatml_catalog.Plan.Discovery.observe discovery ~directory ~name:"graph.chatml"
    |> ok
    |> Option.to_list
  in
  let plan = Chatml_catalog.Plan.create discovery ~observations ~checkpoint |> ok in
  let catalog =
    Chatml_catalog.create
      plan
      ~sources:[ Chatml_project.source project; captured "graph.chatml" dependency ]
      ~checkpoint
    |> ok
  in
  let limits =
    B.Limits.create
      ~max_work_steps:1_000_000
      ~max_input_bytes:None
      ~document:Document_schema.Limits.default
    |> Or_error.ok_exn
  in
  let selection_gate =
    B.Selection_gate.create Initial_prepare ~limits ~checkpoint |> ok |> fst
  in
  B.compose
    (B.Catalog_input.create
       catalog
       ~origin:Local
       ~acquisition_key:"startup-fixture"
       ~dependencies:[])
    ~catalogs:[]
    ~entry:(Captured (captured "main.chatml" body))
    ~selection_gate
    ~contract
    ~lock_id:
      (Source.Id.create ~package_namespace:"routing" ~relative_path:"selected.lock.json"
       |> Or_error.ok_exn)
    ~limits
    ~checkpoint
  |> ok
;;

let hook_body =
  "let initial_state = Graph.value\n\
   let review ctx state = Task.bind(Review.deny(\"fixture\"), fun ignored -> \
   Task.pure(state))"
;;

let raw =
  sprintf
    {|<script id="reviewer" language="chatml" kind="shell_reviewer" project="chatml.project.json">%s</script>
<shell_access id="checks" extends="builtin:yolo@1">
  <reviewers strategy="first_terminal">
    <reviewer id="review" kind="chatml" script="reviewer" lifecycle="session" failure="deny"/>
  </reviewers>
</shell_access>
<tool name="probe" type="shell" mode="fixed" runtime="checks" command="/bin/echo"/>
<script id="handler" language="chatml" kind="tool">%s</script>
<tool name="managed" type="chatml" script="handler" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|}
    hook_body
    Fixture.body
;;

let with_host env ~elements f =
  let root = Eio.Stdenv.cwd env in
  Eio.Switch.run (fun sw ->
    let fixture =
      Inference_fixture.create
        ~namespace:"captured-startup"
        ~default_model:"fixture-model"
        ~post_stream:(fun ~sw:_ ~inputs:_ ->
          failwith "startup unexpectedly dispatched inference")
    in
    let ctx =
      Inference_fixture.ctx
        fixture
        ~env
        ~dir:root
        ~tool_dir:root
        ~cache:(Chat_response.Cache.create ~max_size:1 ())
        ()
    in
    let host =
      R.host
        ~env
        ~workspace:root
        ~tool_dir:root
        ~prompt_dir:root
        ~session_dir:root
        ~cache_dir:root
        ~home:root
        ~session_id:"captured-startup"
        ~prompt_elements:elements
      |> ok
    in
    f ~sw ~ctx ~host)
;;

let with_startup ?compilation_limits env ~elements ~surface ~programs f =
  with_host env ~elements (fun ~sw ~ctx ~host ->
    R.prepare_extensions
      ?compilation_limits
      ~moderator_surface:surface
      ~programs
      ~sw
      ~ctx
      ~host
      ~platform:Macos
      ~prompt_elements:elements
      ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
      ~approval_provider:Shell_runtime.Approval_broker.None_available
      ~approval_store:(Shell_access.Approval.create_store ())
      ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
        failwith "startup unexpectedly delegated")
      ()
    |> f)
;;

let%expect_test
    "captured hook admission precedes initialization and retains managed graphs"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let elements, declarations, parsed, restore = Fixture.custody_fixture env raw in
    let declaration id =
      List.find_exn declarations ~f:(fun declaration ->
        String.equal (CM.Declaration.script_id declaration) id)
    in
    let binding id =
      C.Binding.of_declaration parsed (declaration id) ~checkpoint |> Or_error.ok_exn
    in
    let hook_contract =
      Shell_runtime.Chatml_extension.compiler_contract
        Shell_reviewer
        ~entrypoint:"review"
        ~checkpoint
      |> ok
    in
    let tool_contract = Contract.for_target Tool_v1 ~checkpoint in
    let run programs f =
      with_startup
        env
        ~elements
        ~surface:Chatml.Chatml_builtin_surface.moderator_surface
        ~programs
        f
    in
    let inventory
          ?(hook = true)
          ?(contract = hook_contract)
          ~required_tool
          dependency
          value
      =
      let managed = Fixture.graph (sprintf "let value = \"%s\"" value) in
      let selected = selected_hook ~contract ~required_tool ~body:hook_body dependency in
      restore
        ((binding "handler", C.Input.Provided managed, tool_contract)
         ::
         (if hook then [ binding "reviewer", C.Input.Selected selected, contract ] else [])
        )
    in
    let rejected code = function
      | Ok _ -> failwith "invalid startup admitted"
      | Error diagnostics ->
        assert (
          List.exists diagnostics ~f:(fun diagnostic ->
            String.equal diagnostic.R.code code))
    in
    let poison = "let value = 1 / 0" in
    run
      (inventory ~hook:false ~required_tool:"managed" poison "3")
      (rejected "chatml.missing_program");
    run
      (inventory ~contract:tool_contract ~required_tool:"managed" poison "3")
      (rejected "chatml.missing_program");
    print_endline "missing or different hook contract: rejected before initializer";
    run (inventory ~required_tool:"unavailable" poison "3") (fun result ->
      match result with
      | Error diagnostics ->
        assert (not (List.is_empty diagnostics));
        assert (
          List.for_all diagnostics ~f:(fun diagnostic ->
            String.is_prefix diagnostic.R.code ~prefix:"capability."))
      | Ok _ -> failwith "unavailable required tool admitted");
    print_endline "unavailable required tool: rejected before initializer";
    run (inventory ~required_tool:"managed" poison "3") (rejected "shell.chatml_activate");
    print_endline "required managed tool admitted: initializer is reached";
    List.iter [ "3"; "7" ] ~f:(fun value ->
      run (inventory ~required_tool:"managed" "let value = 0" value) (fun result ->
        let resources = startup_ok result in
        [%test_eq: string] (Fixture.execute resources.R.definition) value;
        printf "successful startup: captured managed dependency = %s\n" value)));
  [%expect
    {|
    missing or different hook contract: rejected before initializer
    unavailable required tool: rejected before initializer
    required managed tool admitted: initializer is reached
    successful startup: captured managed dependency = 3
    successful startup: captured managed dependency = 7
    |}]
;;

let%expect_test "legacy custody is compiled before activation and reused by its manager" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let module M = Chat_response.Moderator_manager in
    let module Surface = Chatml.Chatml_builtin_surface in
    let legacy_body =
      "let initial_state = Graph.value\n\
       let on_event ctx state event = Task.pure(Graph.advance(state))"
    in
    let raw =
      raw
      ^ "<script id=\"legacy\" language=\"chatml\" kind=\"moderator\" \
         project=\"chatml.project.json\">"
      ^ legacy_body
      ^ "</script>"
    in
    let elements, declarations, parsed, restore = Fixture.custody_fixture env raw in
    let binding id =
      let declaration =
        List.find_exn declarations ~f:(fun declaration ->
          String.equal (CM.Declaration.script_id declaration) id)
      in
      C.Binding.of_declaration parsed declaration ~checkpoint |> Or_error.ok_exn
    in
    let hook_contract =
      Shell_runtime.Chatml_extension.compiler_contract
        Shell_reviewer
        ~entrypoint:"review"
        ~checkpoint
      |> ok
    in
    let tool_contract = Contract.for_target Tool_v1 ~checkpoint in
    let legacy_contract =
      Chat_response.Moderator_program.contract Surface.moderator_surface ~checkpoint
    in
    let inventory
          ?(legacy = true)
          ?(contract = legacy_contract)
          ~hook_dependency
          dependency
      =
      let selected contract body dependency =
        C.Input.Selected
          (selected_hook ~contract ~required_tool:"managed" ~body dependency)
      in
      restore
        ([ ( binding "reviewer"
           , selected hook_contract hook_body hook_dependency
           , hook_contract )
         ; ( binding "handler"
           , C.Input.Provided (Fixture.graph "let value = \"3\"")
           , tool_contract )
         ]
         @
         if legacy
         then [ binding "legacy", selected contract legacy_body dependency, contract ]
         else [])
    in
    let run programs f =
      with_startup env ~elements ~surface:Surface.moderator_surface ~programs f
    in
    let rejected = function
      | Ok _ -> failwith "missing legacy contract accepted"
      | Error diagnostics ->
        assert (
          List.exists diagnostics ~f:(fun diagnostic ->
            String.equal diagnostic.R.code "chatml.missing_program"
            && Option.is_some diagnostic.source))
    in
    let dependency =
      "let value = 1 let rec burn n state = if n == 0 then state + 2 else burn(n - 1, \
       state) let advance state = burn(1000, state)"
    in
    let poison = "let value = 1 / 0" in
    run (inventory ~legacy:false ~hook_dependency:poison dependency) rejected;
    run
      (inventory
         ~contract:
           (Chat_response.Moderator_program.contract
              Surface.ui_moderator_surface
              ~checkpoint)
         ~hook_dependency:poison
         dependency)
      rejected;
    print_endline
      "missing or foreign host contract: source diagnostic before native initializer";
    let manager ?execution_policy ?snapshot artifact =
      M.create
        ~env
        ~artifact
        ~capabilities:Chat_response.Moderation.Capabilities.default
        ?execution_policy
        ?snapshot
        ()
    in
    run
      (inventory
         ~hook_dependency:"let value = 0"
         "let value = 1 / 0 let advance state = state")
      (fun result ->
         let artifact =
           (startup_ok result).R.native.legacy_moderator |> Option.value_exn
         in
         assert (Result.is_error (manager artifact));
         print_endline
           "startup compiled the legacy graph; only manager creation runs its initializer");
    run (inventory ~hook_dependency:"let value = 0" dependency) (fun result ->
      let native = (startup_ok result).R.native in
      let artifact = native.legacy_moderator |> Option.value_exn in
      assert (String.length (M.Registry.source_hash artifact) = 64);
      (match
         M.create ~artifact ~capabilities:Chat_response.Moderation.Capabilities.default ()
       with
       | Error message ->
         assert (String.is_prefix message ~prefix:"chatml.execution_host_required:")
       | Ok _ -> failwith "prepared legacy ran without host control");
      assert (
        Result.is_error
          (manager
             ~execution_policy:(Bounded { Chatml_execution.default_limits with fuel = 1 })
             artifact));
      let value snapshot =
        Chatml.Chatml_value_codec.Snapshot.to_value
          snapshot.Session.Moderator_snapshot.current_state
        |> Result.ok_or_failwith
      in
      let event manager =
        M.handle_event
          manager
          ~session_id:"captured-startup"
          ~now_ms:0
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Chat_response.Moderation.Event.Session_start
      in
      let limited =
        manager
          ~execution_policy:(Bounded { Chatml_execution.default_limits with fuel = 500 })
          artifact
        |> Result.ok_or_failwith
      in
      let limited_before = M.snapshot limited |> Result.ok_or_failwith in
      assert (Result.is_error (event limited));
      let limited_after = M.snapshot limited |> Result.ok_or_failwith in
      assert (Lang.equal_value (value limited_before) (value limited_after));
      let live =
        manager ~execution_policy:Unrestricted artifact |> Result.ok_or_failwith
      in
      let before = M.snapshot live |> Result.ok_or_failwith in
      ignore (event live |> Result.ok_or_failwith : Chat_response.Moderation.Outcome.t);
      let after = M.snapshot live |> Result.ok_or_failwith in
      assert (Lang.equal_value (value before) (Lang.VInt 1));
      assert (Lang.equal_value (value after) (Lang.VInt 3));
      let resumed = manager ~snapshot:after artifact |> Result.ok_or_failwith in
      let resumed = M.snapshot resumed |> Result.ok_or_failwith in
      assert (Lang.equal_value (value resumed) (Lang.VInt 3));
      assert (String.equal after.script_source_hash resumed.script_source_hash);
      let inherited =
        R.inherit_native
          ~parent:native
          ~capabilities:
            (Chat_response.Tool_capability.select
               (Lazy.force native.capabilities |> ok)
               ~names:[]
             |> ok)
          ()
        |> ok
      in
      assert (Option.is_none inherited.legacy_moderator);
      print_endline
        "carried graph: initialization and events bounded; unrestricted state 1 -> 3 \
         restores"));
  [%expect
    {|
    missing or foreign host contract: source diagnostic before native initializer
    startup compiled the legacy graph; only manager creation runs its initializer
    carried graph: initialization and events bounded; unrestricted state 1 -> 3 restores
    |}]
;;
