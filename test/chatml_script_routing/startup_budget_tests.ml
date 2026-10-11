open Core
module Budget = Chat_response.Startup_compilation
module Compiler = Chatml_compilation
module Clock = Startup_budget_support.Clock
module Fixture = Routing_tests
module P = Chatml_prepared_program
module C = Chatml_prompt_programs
module Contract = Chatml.Chatml_compiler_contract
module CM = Prompt.Chat_markdown

let ok = Fixture.ok
let limits = Compiler.{ default_limits with wall_seconds = 2.5 }

let context ?max_distinct_text_bytes ?max_distinct_graph_input_bytes env ~checkpoint f =
  Budget.with_context
    ?max_distinct_text_bytes
    ?max_distinct_graph_input_bytes
    ~limits
    ~env
    ~checkpoint
    ~map_error:Fn.id
    ~f
    ()
;;

let error_code expected = function
  | Ok _ -> failwith "invalid startup budget admitted"
  | Error (actual : Compiler.error) -> [%test_eq: string] actual.code expected
;;

let%expect_test "one deadline covers successive compiler preparation phases" =
  Eio_main.run (fun env ->
    let clock = Clock.create () in
    let env =
      Startup_budget_support.environment
        clock
        env
        ~on_domain_start:ignore
        ~completed:(Atomic.make 0)
    in
    context env ~checkpoint:ignore (fun budget ->
      List.iter [ "shell"; "managed" ] ~f:(fun phase ->
        Budget.within budget ~map_error:Fn.id ~f:(fun () ->
          Clock.advance clock ~seconds:1.;
          Ok ())
        |> ok;
        let remaining = Budget.remaining_limits budget |> ok in
        printf "%s: %.1fs remaining\n" phase remaining.wall_seconds);
      Budget.within budget ~map_error:Fn.id ~f:(fun () ->
        Clock.advance clock ~seconds:1.;
        Ok ()))
    |> error_code "chatml.compile_timeout");
  [%expect
    {|
    shell: 1.5s remaining
    managed: 0.5s remaining
    |}]
;;

let%test_unit "exact-text and canonical graph ledgers stay independent" =
  Eio_main.run (fun env ->
    context ~max_distinct_text_bytes:4 env ~checkpoint:ignore (fun budget ->
      Budget.admit_text budget "ab" |> ok;
      Budget.admit_text budget (String.concat [ "a"; "b" ]) |> ok;
      Budget.admit_text budget "cd" |> ok;
      Budget.admit_text budget "e")
    |> error_code "chatml.definition_limit";
    let first = Fixture.graph "let value = \"first\"" in
    let changed = Fixture.graph "let value = \"other\"" in
    let bytes p = P.Accounting.input_bytes (P.accounting p) in
    context
      ~max_distinct_graph_input_bytes:(bytes first + bytes changed)
      env
      ~checkpoint:ignore
      (fun budget ->
         Budget.admit_prepared budget first |> ok;
         Budget.admit_prepared budget first |> ok;
         Budget.admit_prepared budget changed |> ok;
         Budget.admit_prepared budget (Fixture.graph "let value = \"third\""))
    |> error_code "chatml.definition_limit";
    context
      ~max_distinct_graph_input_bytes:(bytes first - 1)
      env
      ~checkpoint:ignore
      (fun budget -> Budget.admit_prepared budget first)
    |> error_code "chatml.definition_limit")
;;

exception Callback_failed of int ref

let%test_unit "callback origin, closed context and caller cancellation are preserved" =
  Eio_main.run (fun env ->
    let observed = ref 0 in
    let run checkpoint =
      context env ~checkpoint (fun budget ->
        Budget.within budget ~map_error:Fn.id ~f:(fun () ->
          Budget.admit_text budget (String.make 8193 'x')))
    in
    run (fun () -> Int.incr observed) |> ok;
    assert (!observed > 0);
    let identity = ref 42 in
    for interrupt = 1 to !observed do
      let count = ref 0 in
      match
        run (fun () ->
          Int.incr count;
          if !count = interrupt then raise (Callback_failed identity))
      with
      | _ -> failwith "checkpoint exception swallowed"
      | exception Callback_failed actual -> assert (phys_equal actual identity)
    done;
    (match run (fun () -> raise Eio.Time.Timeout) with
     | _ -> failwith "callback Timeout became compiler policy error"
     | exception Eio.Time.Timeout -> ());
    (match
       context env ~checkpoint:ignore (fun budget ->
         Budget.within budget ~map_error:Fn.id ~f:(fun () -> raise Eio.Time.Timeout))
     with
     | _ -> failwith "phase Timeout became compiler policy error"
     | exception Eio.Time.Timeout -> ());
    let escaped = ref None in
    context env ~checkpoint:ignore (fun budget ->
      escaped := Some budget;
      Ok ())
    |> ok;
    Budget.admit_text (Option.value_exn !escaped) "closed"
    |> error_code "chatml.invalid_context";
    match
      Eio.Cancel.sub (fun cancel ->
        Eio.Cancel.cancel cancel (Callback_failed identity);
        run ignore)
    with
    | _ -> failwith "cancelled caller admitted"
    | exception Eio.Cancel.Cancelled (Callback_failed actual) ->
      assert (phys_equal actual identity))
;;

let%test_unit "three real startup compilers share one deadline before activation" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let clock = Clock.create () in
    let entered = ref 0 in
    let completed = Atomic.make 0 in
    let env =
      Startup_budget_support.environment clock env ~completed ~on_domain_start:(fun () ->
        Int.incr entered;
        Clock.advance clock ~seconds:1.)
    in
    let legacy_body =
      "let initial_state = Graph.value\n\
       let on_event = fun ctx state event -> Task.pure(state)"
    in
    let raw =
      Startup_tests.raw
      ^ sprintf
          "<script id=\"legacy\" language=\"chatml\" kind=\"moderator\">%s</script>"
          legacy_body
    in
    let elements, declarations, parsed, restore = Fixture.custody_fixture env raw in
    let binding id =
      C.Binding.of_declaration
        parsed
        (List.find_exn declarations ~f:(fun d ->
           String.equal (CM.Declaration.script_id d) id))
        ~checkpoint:ignore
      |> Or_error.ok_exn
    in
    let hook_contract =
      Shell_runtime.Chatml_extension.compiler_contract
        Shell_reviewer
        ~entrypoint:"review"
        ~checkpoint:ignore
      |> ok
    in
    let tool_contract = Contract.for_target Tool_v1 ~checkpoint:ignore in
    let surface = Chatml.Chatml_builtin_surface.moderator_surface in
    let legacy_contract =
      Chat_response.Moderator_program.contract surface ~checkpoint:ignore
    in
    let hook =
      Startup_tests.selected_hook
        ~contract:hook_contract
        ~required_tool:"managed"
        ~body:Startup_tests.hook_body
        "let value = 1 / 0"
    in
    let programs =
      restore
        [ binding "reviewer", C.Input.Selected hook, hook_contract
        ; ( binding "handler"
          , C.Input.Provided (Fixture.graph "let value = \"checked\"")
          , tool_contract )
        ; ( binding "legacy"
          , C.Input.Provided
              (Fixture.graph ~moderator:true ~contract:legacy_contract "let value = 0")
          , legacy_contract )
        ]
    in
    Startup_tests.with_startup
      ~compilation_limits:limits
      env
      ~elements
      ~surface
      ~programs
      (function
      | Ok _ -> failwith "expired aggregate startup activated"
      | Error diagnostics ->
        if
          not
            (List.exists diagnostics ~f:(fun d ->
               String.equal d.Chat_response.Agent_runtime.code "chatml.compile_timeout"))
        then raise_s [%sexp (diagnostics : Chat_response.Agent_runtime.diagnostic list)];
        assert (
          List.for_all diagnostics ~f:(fun d ->
            not (String.equal d.Chat_response.Agent_runtime.code "shell.chatml_activate"))));
    [%test_eq: int] !entered 3;
    [%test_eq: int] (Atomic.get completed) 3)
;;

let%test_unit "non-extension captured legacy startup joins before caller cancellation" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let completed = Atomic.make 0 in
    let env =
      Startup_budget_support.environment
        (Clock.create ())
        env
        ~on_domain_start:ignore
        ~completed
    in
    let surface = Chatml.Chatml_builtin_surface.ui_moderator_surface in
    let elements, _, restore =
      Fixture.fixture ~moderator:true ~legacy_surface:surface env
    in
    let contract = Chat_response.Moderator_program.contract surface ~checkpoint:ignore in
    let programs =
      restore (Some (Fixture.graph ~moderator:true ~contract "let value = 7"))
    in
    Startup_tests.with_host env ~elements (fun ~sw ~ctx ~host ->
      let create checkpoint =
        Chat_response.Agent_runtime.create
          ~programs
          ~checkpoint
          ~moderator_surface:surface
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
      in
      let native = create ignore |> Startup_tests.startup_ok in
      [%test_eq: int] (Atomic.get completed) 1;
      let manager =
        Chat_response.Moderator_manager.create
          ~artifact:(Option.value_exn native.legacy_moderator)
          ~capabilities:Chat_response.Moderation.Capabilities.default
          ~env
          ()
        |> Result.ok_or_failwith
      in
      Chat_response.Moderator_manager.snapshot manager |> Result.ok_or_failwith |> ignore;
      let identity = ref 42 in
      match
        Eio.Cancel.sub (fun cancel ->
          create (fun () ->
            if Atomic.get completed = 2
            then Eio.Cancel.cancel cancel (Callback_failed identity)))
      with
      | _ -> failwith "post-worker caller cancellation swallowed"
      | exception Eio.Cancel.Cancelled (Callback_failed actual) ->
        assert (phys_equal actual identity);
        [%test_eq: int] (Atomic.get completed) 2))
;;

let%test_unit "runtime preparation retains time spent in the host's borrowed context" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    List.iter [ false; true ] ~f:(fun extensions ->
      let clock = Clock.create () in
      let completed = Atomic.make 0 in
      let env =
        Startup_budget_support.environment
          clock
          env
          ~completed
          ~on_domain_start:(fun () -> failwith "expired host started compiler")
      in
      let elements = [] in
      Startup_tests.with_host env ~elements (fun ~sw ~ctx ~host ->
        context env ~checkpoint:ignore (fun startup ->
          Clock.advance clock ~seconds:3.;
          let run_agent ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ =
            failwith "startup invoked child"
          in
          let result =
            if extensions
            then
              Chat_response.Agent_runtime.prepare_extensions
                ~startup
                ~compilation_limits:limits
                ~sw
                ~ctx
                ~host
                ~platform:Macos
                ~prompt_elements:elements
                ~manifest_authorizer:Shell_runtime.Manifest_authorizer.deny
                ~approval_provider:Shell_runtime.Approval_broker.None_available
                ~approval_store:(Shell_access.Approval.create_store ())
                ~run_agent
                ()
              |> Result.map ~f:ignore
            else
              Chat_response.Agent_runtime.create
                ~startup
                ~compilation_limits:limits
                ~sw
                ~ctx
                ~host
                ~platform:Macos
                ~prompt_elements:elements
                ~manifest_authorizer:Shell_runtime.Manifest_authorizer.deny
                ~approval_provider:Shell_runtime.Approval_broker.None_available
                ~approval_store:(Shell_access.Approval.create_store ())
                ~run_agent
                ()
              |> Result.map ~f:ignore
          in
          (match result with
           | Ok () -> failwith "runtime reset expired host deadline"
           | Error diagnostics ->
             assert (
               List.exists diagnostics ~f:(fun diagnostic ->
                 String.equal
                   diagnostic.Chat_response.Agent_runtime.code
                   "chatml.compile_timeout")));
          [%test_eq: int] (Atomic.get completed) 0;
          Ok ())
        |> ok)))
;;
