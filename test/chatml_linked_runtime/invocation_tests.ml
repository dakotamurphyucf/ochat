open Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module Surface = Chatml.Chatml_builtin_surface
module C = Chatml.Chatml_compiler_contract
module Host = Chatml_host_runtime

let context_type = B.TRecord (TRow_extend ([ "phase", TString ], TRow_empty))

let%test_unit
    "Native invocation roles reject before marked initializers and allow coherent custom \
     labels"
  =
  let log = ref [] in
  let surface = Runtime_tests.marked_surface log Surface.core_surface in
  let config = Host.default_runtime_config ~surface () in
  let make state event body =
    let contract =
      Support.native_contract
        ~surface
        ~requirements:[ "seed", state; "dispatch", event ]
        ()
    in
    Support.input
      contract
      [ ( "entry.chatml"
        , "let seed = Counter.current " ^ body
        , [ "Counter", "counter.chatml" ] )
      ; "counter.chatml", "let initialize = mark(\"counter\") let current = 0", []
      ]
    |> Support.compiled contract
  in
  let session program initial_state_name on_event_name =
    Host.instantiate_prepared_session_detailed
      config
      program
      ~entrypoints:{ initial_state_name; on_event_name }
      ()
  in
  let reject program initial on_event =
    log := [];
    assert (Result.is_error (session program initial on_event));
    assert (List.is_empty !log)
  in
  let coherent =
    make
      B.TInt
      (B.TFun ([ context_type; TInt; TInt ], B.task_ty TInt))
      "let dispatch ctx state event = Task.pure(state + event)"
  in
  reject coherent "dispatch" "seed";
  reject coherent "seed" "missing";
  reject (make B.TInt B.TInt "let dispatch = 1") "seed" "dispatch";
  reject
    (make
       B.TInt
       (B.TFun ([ context_type; TInt ], B.task_ty TInt))
       "let dispatch ctx state = Task.pure(state)")
    "seed"
    "dispatch";
  reject
    (make
       B.TInt
       (B.TFun ([ context_type; TString; TInt ], B.task_ty TString))
       "let dispatch ctx state event = Task.pure(state)")
    "seed"
    "dispatch";
  let session = session coherent "seed" "dispatch" |> Runtime_tests.unwrap in
  [%test_eq: string list] (List.rev !log) [ "counter" ];
  Host.handle_event_detailed
    session
    ~context:(L.VRecord (String.Map.singleton "phase" (L.VString "turn_start")))
    ~event:(L.VInt 2)
  |> Runtime_tests.unwrap;
  assert (L.equal_value (Host.current_state session) (L.VInt 2))
;;

let%test_unit
    "standalone Native value-only and wrong argument contracts reject before effects"
  =
  let log = ref [] in
  let surface = Runtime_tests.marked_surface log Surface.core_surface in
  let config = Host.default_runtime_config ~surface () in
  let make scheme body =
    let contract =
      Support.native_contract ~surface ~requirements:[ "selected", scheme ] ()
    in
    Support.input
      contract
      [ "entry.chatml", "let initialize = mark(\"entry\") " ^ body, [] ]
    |> Support.compiled contract
  in
  let reject program arguments =
    log := [];
    assert (
      Result.is_error
        (Host.run_prepared_entrypoint_detailed
           config
           program
           ~entrypoint:"selected"
           ~arguments
           ()));
    assert (List.is_empty !log)
  in
  let value = make B.TInt "let selected = 7" in
  reject value [];
  (* Generic initialization intentionally remains available for selected values. *)
  Chatml_linked_runtime.instantiate value ~surface ~entrypoints:[ "selected" ] ()
  |> Runtime_tests.initialized
  |> ignore;
  [%test_eq: string list] (List.rev !log) [ "entry" ];
  reject (make (B.TFun ([], TInt)) "let selected () = 7") [];
  let task = make (B.TFun ([ TInt ], B.task_ty TInt)) "let selected x = Task.pure(x)" in
  reject task [];
  reject task [ L.VInt 1; L.VInt 2 ];
  let actual =
    Host.run_prepared_entrypoint_detailed
      config
      task
      ~entrypoint:"selected"
      ~arguments:[ L.VInt 8 ]
      ()
    |> Runtime_tests.unwrap
  in
  assert (L.equal_value actual (L.VInt 8))
;;

let%test_unit
    "registered target modes and canonical roles reject before authentic native \
     installers run"
  =
  let module X = Chatml.Chatml_extension_surface in
  List.iter
    [ C.Target.One_off_v1, X.one_off_v1, "let main input = Task.pure(input)"
    ; Tool_v1, X.tool_v1, "let run ctx input = Task.pure(`Complete(input))"
    ; ( Moderator_v1
      , X.moderator_v1
      , "let initial_state = 0 let on_event ctx state event = Task.pure(state)" )
    ; ( Delegated_moderator_v1
      , X.delegated_moderator_v1
      , "let initial_state = 0 let on_event ctx state event = Task.pure(state)" )
    ]
    ~f:(fun (target, surface, body) ->
      let calls = ref 0 in
      let surface =
        { surface with
          Surface.globals =
            List.map surface.globals ~f:(fun (builtin : B.builtin) ->
              if String.equal builtin.name "to_string"
              then
                { builtin with
                  impl =
                    (fun args ->
                      incr calls;
                      builtin.impl args)
                }
              else builtin)
        }
      in
      assert (
        List.exists surface.globals ~f:(fun (b : B.builtin) ->
          String.equal b.name "to_string"));
      let contract = C.for_target target ~checkpoint:(fun () -> ()) in
      let program =
        Support.input
          contract
          [ "entry.chatml", "let initialize = to_string(1) " ^ body, [] ]
        |> Support.compiled contract
      in
      let config = Host.default_runtime_config ~surface () in
      let invalid =
        match target with
        | One_off_v1 | Tool_v1 ->
          Host.instantiate_prepared_session_detailed
            config
            program
            ~entrypoints:{ initial_state_name = "main"; on_event_name = "main" }
            ()
          |> Result.map ~f:(fun _ -> ())
        | Moderator_v1 | Delegated_moderator_v1 ->
          Host.instantiate_prepared_session_detailed
            config
            program
            ~entrypoints:
              { initial_state_name = "on_event"; on_event_name = "initial_state" }
            ()
          |> Result.map ~f:(fun _ -> ())
      in
      assert (Result.is_error invalid);
      assert (Int.equal !calls 0);
      match target with
      | Moderator_v1 | Delegated_moderator_v1 ->
        assert (
          Result.is_error
            (Host.run_prepared_entrypoint_detailed
               config
               program
               ~entrypoint:"on_event"
               ~arguments:[]
               ()));
        assert (Int.equal !calls 0)
      | One_off_v1 ->
        assert (
          Result.is_error
            (Host.run_prepared_entrypoint_detailed
               config
               program
               ~entrypoint:"main"
               ~arguments:[]
               ()));
        assert (Int.equal !calls 0);
        let result =
          Host.run_prepared_entrypoint_detailed
            config
            program
            ~entrypoint:"main"
            ~arguments:[ L.VVariant ("Null", []) ]
            ()
          |> Runtime_tests.unwrap
        in
        assert (L.equal_value result (L.VVariant ("Null", [])));
        assert (Int.equal !calls 1)
      | Tool_v1 -> ())
;;
