open Core
module Fixture = Routing_tests
module CM = Prompt.Chat_markdown
module R = Chat_response.Agent_runtime
module C = Chatml_prompt_programs
module B = Chatml_project_build
module Source = Chatml_source
module Contract = Chatml.Chatml_compiler_contract

let checkpoint = ignore
let ok = Fixture.ok

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
    let root = Eio.Stdenv.cwd env in
    let run programs f =
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
        R.prepare_extensions
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
        let resources = ok result in
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
