open Core
module X = Chatml_execution
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang

let%expect_test "persistent control proxies expire even for unrestricted execution" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let runner = X.create_runner ~env ~policy:Unrestricted () in
      let ready, release = Eio.Promise.create () in
      let result, finish = Eio.Promise.create () in
      X.run_scoped runner (fun () ->
        (X.runner_control runner).checkpoint ();
        Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.await ready;
          let outcome =
            Exn.does_raise (fun () -> (X.runner_control runner).checkpoint ())
          in
          Eio.Promise.resolve finish outcome))
      |> Result.map_error ~f:(fun error -> error.X.code)
      |> Result.ok_or_failwith;
      Eio.Promise.resolve release ();
      let outcome = Eio.Promise.await result in
      print_s [%sexp (outcome : bool)]));
  [%expect {| true |}]
;;

let compile env source =
  Chatml_compilation.compile ~env ~target:One_off_v1 ~source ()
  |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
  |> Result.ok_or_failwith
;;

let configuration on_tool_call : R.runtime_config =
  { surface = Chatml.Chatml_extension_surface.one_off_v1
  ; operations =
      R.default_operations ~handlers:{ R.default_handlers with on_tool_call } ()
  }
;;

let null = L.VVariant ("Null", [])
let tool_ok = L.VVariant ("Ok", [ null ])

let%expect_test "spawned-task exhaustion precedes effects and cannot be caught as success"
  =
  Eio_main.run (fun env ->
    let program =
      Chatml_compilation.compile
        ~env
        ~target:Moderator_v1
        ~source:
          {|let initial_state = `Null
let on_event ctx state event = Task.catch(
  Task.bind(Model.spawn("first", state), fun ignored ->
    Task.bind(Model.spawn("second", state), fun ignored -> Task.pure(state))),
  fun ignored -> Task.bind(Tool.call("after", state), fun result -> Task.pure(state)))|}
        ()
      |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
      |> Result.ok_or_failwith
    in
    List.iter [ 0; 1; 2 ] ~f:(fun max_tasks ->
      let spawned = ref []
      and after = ref 0 in
      let config : R.runtime_config =
        { surface = Chatml.Chatml_extension_surface.moderator_v1
        ; operations =
            R.default_operations
              ~handlers:
                { R.default_handlers with
                  on_model_spawn =
                    (fun _ ~recipe ~payload:_ ->
                      spawned := recipe :: !spawned;
                      Ok ("fixture-" ^ recipe))
                ; on_tool_call =
                    (fun _ ~name:_ ~args:_ ->
                      incr after;
                      Ok tool_ok)
                }
              ()
        }
      in
      let runner =
        X.create_runner ~env ~policy:(Bounded { X.default_limits with max_tasks }) ()
      in
      let session =
        X.run_scoped runner (fun () ->
          R.instantiate_session
            ~control:(X.runner_control runner)
            config
            program
            ~entrypoints:
              { initial_state_name = "initial_state"; on_event_name = "on_event" })
        |> Result.map_error ~f:(fun error -> error.X.message)
        |> Result.join
        |> Result.ok_or_failwith
      in
      let outcome =
        X.run_scoped runner (fun () ->
          R.handle_event
            session
            ~context:
              (L.VRecord (String.Map.singleton "phase" (L.VString "session_start")))
            ~event:(L.VVariant ("Session_start", [])))
      in
      let status =
        match outcome with
        | Ok (Ok ()) -> "ok"
        | Ok (Error message) -> failwith message
        | Error error -> error.X.code
      in
      print_s
        [%sexp
          (max_tasks : int)
        , (status : string)
        , (List.rev !spawned : string list)
        , (!after : int)]));
  [%expect
    {|
    (0 chatml.task_limit () 0)
    (1 chatml.task_limit (first) 0)
    (2 ok (first second) 0)
    |}]
;;

let%expect_test "nested execution shares ceilings and cannot hide ancestor exhaustion" =
  Eio_main.run (fun env ->
    let root =
      compile
        env
        {|let main input = Task.catch(
  Task.bind(Tool.call("child", input), fun ignored ->
    Task.bind(Tool.call("after", input), fun ignored -> Task.pure(`String("done")))),
  fun ignored -> Task.pure(`String("caught")))|}
    in
    List.iter
      [ `Fuel
      ; `Allocation
      ; `Value
      ; `Calls
      ; `Depth
      ; `Local_limit
      ; `Parallel
      ; `Effect_result
      ; `Domain
      ; `Captured_parent
      ]
      ~f:(fun mode ->
        let limits =
          match mode with
          | `Fuel | `Domain -> { X.default_limits with fuel = 1000 }
          | `Allocation | `Effect_result ->
            { X.default_limits with allocation_bytes = 32768 }
          | `Value -> { X.default_limits with max_value_bytes = 128 }
          | `Calls -> { X.default_limits with max_calls = 1 }
          | `Parallel -> { X.default_limits with max_calls = 3 }
          | `Depth -> { X.default_limits with max_invocation_depth = 2 }
          | `Local_limit | `Captured_parent -> X.default_limits
        in
        let child_source =
          match mode with
          | `Fuel | `Domain ->
            {|let rec sum n = if n == 0 then 0 else n + sum(n - 1)
let main input = let ignored = sum(1000) in Task.pure(input)|}
          | `Allocation ->
            "let main input = let ignored = Array.make(10000, 0) in Task.pure(input)"
          | `Value ->
            "let main input = Task.pure(`String(\"" ^ String.make 512 'x' ^ "\"))"
          | `Depth | `Captured_parent ->
            "let main input = Task.bind(Tool.call(\"child\", input), fun ignored -> \
             Task.pure(input))"
          | `Parallel ->
            "let main input = Task.bind(Tool.call(\"leaf\", input), fun ignored -> \
             Task.bind(Tool.call(\"leaf\", input), fun ignored -> Task.pure(input)))"
          | `Calls | `Local_limit | `Effect_result ->
            "let main input = Task.bind(Tool.call(\"leaf\", input), fun ignored -> \
             Task.pure(input))"
        in
        let child = compile env child_source in
        let after = ref 0
        and leaves = ref 0
        and failures = ref [] in
        let captured_root = ref None in
        let rec on_tool_call _ ~name ~args:_ =
          match name with
          | "after" ->
            Int.incr after;
            Ok tool_ok
          | "leaf" ->
            Int.incr leaves;
            Eio.Fiber.yield ();
            Ok tool_ok
          | "child" ->
            let run () =
              let context =
                match mode, !captured_root with
                | `Captured_parent, Some context -> context
                | _ ->
                  let context = X.capture_context () in
                  captured_root := Some context;
                  context
              in
              let execute () =
                X.run
                  ~context
                  ~policy:
                    (match mode with
                     | `Local_limit -> Bounded { X.default_limits with max_calls = 0 }
                     | `Captured_parent ->
                       Bounded { X.default_limits with max_invocation_depth = 1 }
                     | _ -> Unrestricted)
                  ~env
                  ~config:(configuration on_tool_call)
                  ~program:child
                  ~entrypoint:"main"
                  ~arguments:[ null ]
                  ()
              in
              let result =
                match mode with
                | `Domain -> Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) execute
                | _ -> execute ()
              in
              match result with
              | Error error -> failures := error.X.code :: !failures
              | Ok _ -> ()
            in
            (match mode with
             | `Parallel ->
               Eio.Fiber.both run run;
               Ok tool_ok
             | `Effect_result ->
               Ok
                 (L.VVariant
                    ( "Ok"
                    , [ L.VVariant ("String", [ L.VString (String.make 100_000 'x') ]) ]
                    ))
             | _ ->
               run ();
               Ok tool_ok)
          | _ -> assert false
        in
        let result =
          X.run
            ~policy:(Bounded limits)
            ~env
            ~config:(configuration on_tool_call)
            ~program:root
            ~entrypoint:"main"
            ~arguments:[ null ]
            ()
        in
        let actual =
          match result with
          | Error error -> error.X.code
          | Ok value ->
            Chatml.Chatml_value_codec.value_to_jsonaf_exn value |> Jsonaf.to_string
        in
        let expected =
          match mode with
          | `Fuel | `Domain -> "chatml.execution_limit"
          | `Allocation | `Effect_result -> "chatml.allocation_limit"
          | `Value -> "chatml.value_limit"
          | `Calls | `Parallel -> "chatml.call_limit"
          | `Depth -> "chatml.invocation_depth"
          | `Local_limit | `Captured_parent -> "\"done\""
        in
        [%test_eq: string] expected actual;
        (match mode with
         | `Local_limit | `Captured_parent -> [%test_eq: int] 1 !after
         | _ -> [%test_eq: int] 0 !after);
        print_s
          [%sexp
            (mode
             : [ `Fuel
               | `Allocation
               | `Value
               | `Calls
               | `Depth
               | `Local_limit
               | `Parallel
               | `Effect_result
               | `Domain
               | `Captured_parent
               ])
          , (actual : string)
          , (!leaves : int)
          , (!after : int)
          , (List.sort !failures ~compare:String.compare : string list)]));
  [%expect
    {|
    (Fuel chatml.execution_limit 0 0 (chatml.execution_limit))
    (Allocation chatml.allocation_limit 0 0 (chatml.allocation_limit))
    (Value chatml.value_limit 0 0 (chatml.value_limit))
    (Calls chatml.call_limit 0 0 (chatml.call_limit))
    (Depth chatml.invocation_depth 0 0
     (chatml.invocation_depth chatml.invocation_depth))
    (Local_limit "\"done\"" 0 1 (chatml.call_limit))
    (Parallel chatml.call_limit 2 0 (chatml.call_limit chatml.call_limit))
    (Effect_result chatml.allocation_limit 0 0 ())
    (Domain chatml.execution_limit 0 0 (chatml.execution_limit))
    (Captured_parent "\"done\"" 0 1
     (chatml.invocation_depth chatml.invocation_depth))
    |}]
;;

let%expect_test "independent roots keep separate budgets and captured scopes expire" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let program =
        compile
          env
          "let main input = Task.bind(Tool.call(\"leaf\", input), fun ignored -> \
           Task.pure(input))"
      in
      let later, later_u = Eio.Promise.create () in
      let finished, finished_u = Eio.Promise.create () in
      let calls = ref 0 in
      let config =
        configuration (fun _ ~name:_ ~args:_ ->
          Int.incr calls;
          Eio.Fiber.yield ();
          Ok tool_ok)
      in
      let run () =
        X.run
          ~policy:(Bounded { X.default_limits with max_calls = 1 })
          ~env
          ~config
          ~program
          ~entrypoint:"main"
          ~arguments:[ null ]
          ()
      in
      let a, b = Eio.Fiber.pair run run in
      assert (Result.is_ok a && Result.is_ok b);
      [%test_eq: int] 2 !calls;
      let capture_config =
        configuration (fun _ ~name:_ ~args:_ ->
          let context = X.capture_context () in
          Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.await later;
            let result =
              Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
                X.run
                  ~context
                  ~policy:Unrestricted
                  ~env
                  ~config
                  ~program
                  ~entrypoint:"main"
                  ~arguments:[ null ]
                  ())
            in
            match result with
            | Ok _ -> failwith "expired execution scope was reused"
            | Error error -> Eio.Promise.resolve finished_u error.X.code);
          Ok tool_ok)
      in
      X.run ~env ~config:capture_config ~program ~entrypoint:"main" ~arguments:[ null ] ()
      |> Result.map_error ~f:(fun error -> error.X.message)
      |> Result.ok_or_failwith
      |> ignore;
      Eio.Promise.resolve later_u ();
      print_s [%sexp (Eio.Promise.await finished : string), (!calls : int)]));
  [%expect {| (chatml.inactive_scope 2) |}]
;;
