open Core
module B = Chatml_project_build
module R = Chatml_prepared_program
module S = Chatml_source
module L = Chatml.Chatml_lang
module Linked = Chatml_linked_program

let counter_id path =
  S.Id.create ~package_namespace:"counter" ~relative_path:path |> Or_error.ok_exn
;;

let assert_result value expected =
  assert (L.equal_value value (L.VVariant ("String", [ L.VString expected ])))
;;

let retained_sources build =
  R.sources (B.prepared build)
  |> List.filter ~f:(fun source ->
    not (S.Id.equal (S.id source) (S.id (B.lock_source build))))
;;

let restore build =
  B.restore
    (B.lock_source build)
    ~sources:(retained_sources build)
    ~contract:(Support.contract ())
    ~limits:(Support.composition_limits ())
    ~checkpoint:ignore
  |> Support.ok
;;

let%expect_test
    "documented project compiles inertly and runs one shared diamond per instance"
  =
  Support.with_project (fun env _directory host ->
    let log = ref [] in
    let surface = Support.marked_surface log in
    let build =
      Support.prepare
        host
        ~environment:None
        ~operation:Initial_prepare
        ~clock:env#mono_clock
    in
    let input = B.prepared build in
    [%test_eq: int] (List.length (Chatml_project.Lock.catalogs (B.lock build))) 2;
    let main = Support.id "src/main.chatml" in
    assert (S.Id.equal (R.entry input) main);
    [%test_eq: int]
      (List.count (R.definitions input) ~f:(fun definition ->
         S.Id.equal (R.Definition.implementation definition) main))
      1;
    assert (Option.is_none (R.find_source input (Support.id "src/ignored.chatml")));
    let common =
      R.find_definition
        input
        (R.Module_id.of_implementation (counter_id "src/common.chatml"))
      |> Option.value_exn
    in
    assert (
      Option.equal
        S.Id.equal
        (R.Definition.interface common)
        (Some (counter_id "src/common.chatmli")));
    let program = Support.compiled env build in
    [%test_eq: string list] !log [];
    let ids = List.map (Linked.runtime_order program) ~f:R.Module_id.implementation in
    [%test_eq: int] (List.count ids ~f:(S.Id.equal main)) 1;
    [%test_eq: int] (List.count ids ~f:(S.Id.equal (counter_id "src/common.chatml"))) 1;
    printf "prepare + compile: no initializers\n";
    for invocation = 1 to 2 do
      log := [];
      assert_result (Support.run env surface program) "13";
      [%test_eq: string list] (List.rev !log) [ "common"; "left"; "right"; "main" ];
      printf
        "fresh run %d: shared counter result 13, each selected initializer once\n"
        invocation
    done;
    let alternate =
      Support.prepare
        host
        ~environment:(Some "alternate")
        ~operation:(Build { previous_lock = B.lock_source build })
        ~clock:env#mono_clock
    in
    let alternate_program = Support.compiled env alternate in
    log := [];
    assert_result (Support.run env surface alternate_program) "113";
    [%test_eq: string list] (List.rev !log) [ "common"; "left"; "alternate"; "main" ];
    printf "explicit alternate environment: result 113\n");
  [%expect
    {|
    prepare + compile: no initializers
    fresh run 1: shared counter result 13, each selected initializer once
    fresh run 2: shared counter result 13, each selected initializer once
    explicit alternate environment: result 113
    |}]
;;

let%expect_test
    "fresh capture sees local edits; retained graph runs after originals disappear"
  =
  Support.with_project (fun env directory host ->
    let original =
      Support.prepare
        host
        ~environment:None
        ~operation:Initial_prepare
        ~clock:env#mono_clock
    in
    let old_fingerprint = R.fingerprint (B.prepared original) in
    Support.save
      directory
      "vendor/counter/src/common.chatml"
      "let initialized = to_string(\"common\") let cell = ref(10) let next () = cell := \
       !cell + 1; !cell let reveal () = !cell";
    let fresh =
      Support.prepare
        host
        ~environment:None
        ~operation:(Build { previous_lock = B.lock_source original })
        ~clock:env#mono_clock
    in
    assert (not (S.Digest.equal old_fingerprint (R.fingerprint (B.prepared fresh))));
    let log = ref [] in
    let surface = Support.marked_surface log in
    assert_result (Support.run env surface (Support.compiled env original)) "13";
    assert_result (Support.run env surface (Support.compiled env fresh)) "53";
    printf "retained capture: 13; fresh local capture: 53\n";
    List.iter
      [ "chatml.project.json"; "src"; "branches"; "variants"; "vendor" ]
      ~f:(fun name -> Eio.Path.rmtree Eio.Path.(directory / name));
    let replay = restore original in
    assert (S.Digest.equal old_fingerprint (R.fingerprint (B.prepared replay)));
    assert_result (Support.run env surface (Support.compiled env replay)) "13";
    printf "capture-only restore + compile + run after source removal: 13\n");
  [%expect
    {|
    retained capture: 13; fresh local capture: 53
    capture-only restore + compile + run after source removal: 13
    |}]
;;

let%test_unit
    "submitted text retains the implicit adapter without ambient project discovery"
  =
  Support.with_project (fun env directory _host ->
    Support.save directory "chatml.project.json" "invalid project must not be inspected";
    let program =
      Chatml_compilation.compile_with_policy
        ~policy:(Bounded Chatml_compilation.default_limits)
        ~env
        ~target:One_off_v1
        ~source:"let main input = Task.pure(input)"
        ()
      |> function
      | Ok program -> program
      | Error error -> failwith error.Chatml_compilation.message
    in
    let input = L.VVariant ("Null", []) in
    let actual =
      Chatml_execution.run
        ~env
        ~config:
          (Chatml_host_runtime.default_runtime_config
             ~surface:Chatml.Chatml_extension_surface.one_off_v1
             ())
        ~program
        ~entrypoint:"main"
        ~arguments:[ input ]
        ()
      |> Support.ok
    in
    assert (L.equal_value input actual))
;;

let%expect_test
    "captured moderator code recomposes and initializes under the delegated host role"
  =
  Support.with_project (fun env directory host ->
    let old = Support.prepare_library host ~clock:env#mono_clock in
    let module Contract = Chatml.Chatml_compiler_contract in
    let contract = Contract.for_target Delegated_moderator_v1 ~checkpoint:ignore in
    let delegated =
      B.recompose
        old
        ~contract
        ~lock_id:(Support.id "delegated.lock.json")
        ~limits:(Support.composition_limits ())
        ~checkpoint:ignore
      |> Support.ok
    in
    List.iter
      [ "chatml.project.json"; "src"; "branches"; "variants"; "vendor" ]
      ~f:(fun name -> Eio.Path.rmtree Eio.Path.(directory / name));
    let program =
      match
        Chatml_compilation.compile_prepared_with_policy
          (B.prepared delegated)
          ~policy:(Bounded Chatml_compilation.default_limits)
          ~max_dependency_depth:32
          ~env
          ~target:Delegated_moderator_v1
          ()
      with
      | Ok program -> program
      | Error error -> failwith error.Chatml_compilation.message
    in
    let runner =
      Chatml_execution.create_runner
        ~env
        ~policy:(Bounded Chatml_execution.default_limits)
        ()
    in
    let session =
      Chatml_execution.run_scoped runner (fun () ->
        Chatml_host_runtime.instantiate_prepared_session_detailed
          (Chatml_host_runtime.default_runtime_config
             ~surface:Chatml.Chatml_extension_surface.delegated_moderator_v1
             ())
          program
          ~entrypoints:
            { initial_state_name = "initial_state"; on_event_name = "on_event" }
          ~control:(Chatml_execution.runner_control runner)
          ()
        |> function
        | Ok session -> session
        | Error diagnostic -> failwith diagnostic.Chatml_diagnostic.message)
      |> Support.ok
    in
    assert (L.equal_value (Chatml_host_runtime.current_state session) (L.VInt 1));
    printf "same captured counter; authentic delegated role; initialized state 1\n");
  [%expect {| same captured counter; authentic delegated role; initialized state 1 |}]
;;

let%expect_test "graph compilation leaves a failing initializer unevaluated" =
  Support.with_project (fun env directory host ->
    Support.save
      directory
      "src/main.chatml"
      "let initialize = 1 / 0 let main input = Task.pure(input)";
    let build =
      Support.prepare
        host
        ~environment:None
        ~operation:Initial_prepare
        ~clock:env#mono_clock
    in
    let program = Support.compiled env build in
    printf "graph compilation succeeded before division-by-zero initializer\n";
    match
      Chatml_execution.run_prepared
        program
        ~env
        ~config:
          (Chatml_host_runtime.default_runtime_config
             ~surface:Chatml.Chatml_extension_surface.one_off_v1
             ())
        ~entrypoint:"main"
        ~arguments:[ L.VVariant ("Null", []) ]
        ()
    with
    | Ok _ -> failwith "failing initializer did not run at execution"
    | Error error ->
      [%test_eq: string] error.code "chatml.execution_failed";
      assert (not (String.is_empty error.message));
      printf "bounded execution rejected the actual initializer\n");
  [%expect
    {|
    graph compilation succeeded before division-by-zero initializer
    bounded execution rejected the actual initializer
    |}]
;;

let%test_unit
    "configured companion and out-of-scope failures retain the actual entry revision"
  =
  Support.with_project (fun env directory host ->
    List.iter
      [ "let main input = Task.pure(Unknown.value)"
      ; "let main input = Task.pure(Alias.cell)"
      ]
      ~f:(fun contents ->
        Support.save directory "src/main.chatml" contents;
        let build =
          Support.prepare
            host
            ~environment:None
            ~operation:Initial_prepare
            ~clock:env#mono_clock
        in
        let source =
          R.find_source (B.prepared build) (Support.id "src/main.chatml")
          |> Option.value_exn
        in
        match
          Chatml_compilation.compile_prepared_with_policy
            (B.prepared build)
            ~policy:(Bounded Chatml_compilation.default_limits)
            ~max_dependency_depth:32
            ~env
            ~target:One_off_v1
            ()
        with
        | Ok _ -> failwith "unavailable configured value was accepted"
        | Error error ->
          let diagnostic = Option.value_exn error.diagnostic in
          let site = Option.value_exn diagnostic.diagnostic.Chatml_diagnostic.site in
          let location = Chatml_diagnostic.Site.location site in
          assert (S.Id.equal (S.Location.source location) (S.id source));
          assert (S.Digest.equal (S.Location.source_digest location) (S.digest source));
          let span = S.Location.span location in
          assert (span.left.offset >= 0 && span.right.offset <= String.length contents)))
;;
