open Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module X = Chatml.Chatml_extension_surface
module C = Chatml.Chatml_compiler_contract
module Host = Chatml_host_runtime

let checkpoint () = ()

let source =
  {|let run ctx input = Task.bind(Model.spawn("agent_prompt_v1", input), fun id -> Task.pure(`Complete(`String(id))))|}
;;

let config calls =
  let handlers =
    { Host.default_handlers with
      on_model_spawn =
        (fun _ ~recipe ~payload:_ ->
          assert (String.equal recipe "agent_prompt_v1");
          incr calls;
          Ok "admitted-job")
    }
  in
  Host.default_runtime_config ~surface:X.developer_tool_v1 ~handlers ()
;;

let result_is_admitted = function
  | Ok (L.VVariant ("Complete", [ L.VVariant ("String", [ L.VString id ]) ])) ->
    String.equal id "admitted-job"
  | Ok _ | Error _ -> false
;;

let result_is_string expected = function
  | Ok (L.VVariant ("Complete", [ L.VVariant ("String", [ L.VString actual ]) ])) ->
    String.equal actual expected
  | Ok _ | Error _ -> false
;;

let%test_unit
    "distinct developer tool contract compiles Model.spawn with unchanged run ABI"
  =
  assert (
    Result.is_error
      (Host.compile_script
         ~surface:X.tool_v1
         ~required_bindings:X.tool_entrypoints
         ~source
         ()));
  let compiled =
    Host.compile_script
      ~surface:X.developer_tool_v1
      ~required_bindings:X.tool_entrypoints
      ~source
      ()
    |> Result.ok_or_failwith
  in
  let calls = ref 0 in
  let config = config calls in
  let run ?scoped_async_operation () =
    Host.run_entrypoint
      ?scoped_async_operation
      config
      compiled
      ~entrypoint:"run"
      ~arguments:[ L.VUnit; L.VVariant ("Null", []) ]
      ()
  in
  assert (Result.is_error (run ()));
  assert (Int.equal !calls 0);
  assert (result_is_admitted (run ~scoped_async_operation:Model_spawn ()));
  assert (Int.equal !calls 1)
;;

let%test_unit
    "linked selected contract needs its own graph identity and closed async mode"
  =
  let selected = C.for_target Developer_tool_v1 ~checkpoint in
  let old = C.for_target Tool_v1 ~checkpoint in
  assert (
    not
      (Chatml_source.Digest.equal
         (C.fingerprint selected ~checkpoint)
         (C.fingerprint old ~checkpoint)));
  let input = Support.input selected [ "entry.chatml", source, [] ] in
  assert (
    Result.is_error
      (Chatml_linked_program.compile
         input
         ~contract:old
         ~max_dependency_depth:16
         ~checkpoint));
  let program = Support.compiled selected input in
  let calls = ref 0 in
  let config = config calls in
  let run ?scoped_async_operation () =
    Host.run_prepared_entrypoint_detailed
      config
      program
      ~entrypoint:"run"
      ~arguments:[ L.VUnit; L.VVariant ("Null", []) ]
      ?scoped_async_operation
      ()
  in
  assert (Result.is_error (run ()));
  assert (Int.equal !calls 0);
  assert (result_is_admitted (run ~scoped_async_operation:Model_spawn ()));
  assert (Int.equal !calls 1)
;;

let%test_unit "selected Model.spawn mode does not admit another async operation" =
  let source =
    {|let run ctx input = Task.bind(Tool.spawn("selected", input), fun id -> Task.pure(`Complete(`String(id))))|}
  in
  let compiled =
    Host.compile_script
      ~surface:X.developer_tool_v1
      ~required_bindings:X.tool_entrypoints
      ~source
      ()
    |> Result.ok_or_failwith
  in
  let called = ref false in
  let extra : Host.op_def =
    { name = "Job.start_tool"
    ; kind = External_async
    ; phase_check = Host.allow_all_phases
    ; perform =
        (fun _ _ ->
          called := true;
          Ok (L.VString "wrong"))
    }
  in
  let base = config (ref 0) in
  let other_operations =
    List.filter base.operations ~f:(fun operation ->
      not (String.equal operation.name extra.name))
  in
  let synchronous =
    { base with operations = { extra with kind = External_sync } :: other_operations }
  in
  let synchronous_result =
    Host.run_entrypoint
      synchronous
      compiled
      ~entrypoint:"run"
      ~arguments:[ L.VUnit; L.VVariant ("Null", []) ]
      ()
  in
  (match synchronous_result with
   | Error error -> failwith error
   | Ok _ -> assert (result_is_string "wrong" synchronous_result));
  assert !called;
  called := false;
  let asynchronous = { base with operations = extra :: other_operations } in
  assert (
    Result.is_error
      (Host.run_entrypoint
         ~scoped_async_operation:Model_spawn
         asynchronous
         compiled
         ~entrypoint:"run"
         ~arguments:[ L.VUnit; L.VVariant ("Null", []) ]
         ()));
  assert (not !called)
;;
