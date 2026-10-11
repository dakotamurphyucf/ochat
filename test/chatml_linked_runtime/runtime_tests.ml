open Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module Surface = Chatml.Chatml_builtin_surface
module Runtime = Chatml_linked_runtime
module Host = Chatml_host_runtime
module Execution = Chatml_execution
module C = Chatml.Chatml_compiler_contract

let unwrap = function
  | Ok value -> value
  | Error diagnostic -> failwith diagnostic.Chatml_diagnostic.message
;;

let initialized result = Result.map_error result ~f:Runtime.Error.diagnostic |> unwrap
let value instance name = Runtime.binding instance ~name |> Option.value_exn

let marked_surface log base =
  Surface.merge
    { Surface.empty with
      globals =
        [ { B.name = "mark"
          ; scheme = TFun ([ TString ], TUnit)
          ; impl =
              (function
                | [ L.VString name ] ->
                  log := name :: !log;
                  L.VUnit
                | _ -> assert false)
          }
        ]
    }
    base
;;

let diamond surface =
  let contract =
    Support.native_contract
      ~surface
      ~requirements:[ "main", B.TFun ([], B.task_ty TInt) ]
      ()
  in
  let input =
    Support.input
      contract
      ~interfaces:
        [ "common.chatml", "val next : fun() -> int val reveal : fun() -> int", [] ]
      [ ( "entry.chatml"
        , {|open Alias
type unused = Types.t
let initialize = mark("entry")
let local = 100
let main () = Task.pure(Left.next() + Right.next() + next() + reveal())|}
        , [ "Alias", "common.chatml"
          ; "Left", "left.chatml"
          ; "Right", "right.chatml"
          ; "Types", "types.chatml"
          ] )
      ; ( "left.chatml"
        , "let initialize = mark(\"left\") let next () = Common.next()"
        , [ "Common", "common.chatml" ] )
      ; ( "right.chatml"
        , "let initialize = mark(\"right\") let next () = Common.next()"
        , [ "Common", "common.chatml" ] )
      ; ( "common.chatml"
        , "let initialize = mark(\"common\") let local = 7 let reveal () = local let \
           cell = ref(0) let next () = cell := !cell + 1; !cell"
        , [] )
      ; ( "types.chatml"
        , "type t = int let poison = Poison.value"
        , [ "Poison", "poison.chatml" ] )
      ; "poison.chatml", "let value = mark(\"type-only-poison\")", []
      ]
  in
  Support.compiled contract input
;;

let%expect_test
    "diamond aliases share state, type/open-only dependencies stay inert, fresh runs \
     reset"
  =
  let log = ref [] in
  let surface = marked_surface log Surface.core_surface in
  let program = diamond surface in
  let config = Host.default_runtime_config ~surface () in
  for _ = 1 to 2 do
    log := [];
    let result =
      Host.run_prepared_entrypoint_detailed
        config
        program
        ~entrypoint:"main"
        ~arguments:[]
        ()
      |> unwrap
    in
    assert (L.equal_value result (L.VInt 13));
    [%test_eq: string list] (List.rev !log) [ "common"; "left"; "right"; "entry" ]
  done;
  print_s [%sexp (List.rev !log : string list)];
  [%expect {| (common left right entry) |}]
;;

let%test_unit "completed maps preserve object identity and reject replacement" =
  let module S = Chatml_source in
  let module R = Chatml.Chatml_module_reference in
  let id =
    S.Id.create ~package_namespace:"runtime" ~relative_path:"dep.chatml"
    |> Or_error.ok_exn
  in
  let env = L.create_env () in
  L.define_var env "cell" (L.VRef (ref (L.VInt 0)));
  let public = L.VModule env in
  let map =
    L.Configured_modules.add
      L.Configured_modules.empty
      ~implementation:id
      ~public_module:public
    |> Result.ok
    |> Option.value_exn
  in
  let whole = R.create id ~member_path:[] |> Or_error.ok_exn in
  let first = L.Configured_modules.read map whole |> Result.ok |> Option.value_exn in
  let second = L.Configured_modules.read map whole |> Result.ok |> Option.value_exn in
  assert (L.equal_value first second);
  assert (
    Result.is_error
      (L.Configured_modules.add map ~implementation:id ~public_module:public));
  assert (
    Result.is_error
      (L.Configured_modules.add
         L.Configured_modules.empty
         ~implementation:id
         ~public_module:L.VUnit));
  assert (Result.is_error (L.Configured_modules.read L.Configured_modules.empty whole))
;;

let%test_unit
    "checked public views hide private cells and closures keep their own lexical \
     environment"
  =
  let log = ref [] in
  let surface = marked_surface log Surface.core_surface in
  let program = diamond surface in
  let instance =
    Runtime.instantiate program ~surface ~entrypoints:[ "main" ] () |> initialized
  in
  let env = Runtime.entry_env instance in
  assert (L.equal_value (L.find_var env "local" |> Option.value_exn) (L.VInt 100));
  let id =
    Chatml_source.Id.create
      ~package_namespace:"linked-test"
      ~relative_path:"common.chatml"
    |> Or_error.ok_exn
  in
  let reference path =
    Chatml.Chatml_module_reference.create id ~member_path:path |> Or_error.ok_exn
  in
  assert (
    Result.is_error
      (L.Configured_modules.read env.configured_modules (reference [ "cell" ])));
  assert (
    Result.is_error
      (L.Configured_modules.read env.configured_modules (reference [ "local" ])));
  let reveal =
    L.Configured_modules.read env.configured_modules (reference [ "reveal" ])
    |> Result.ok
    |> Option.value_exn
  in
  assert (
    L.equal_value (Chatml.Chatml_eval.apply_value_result reveal [] |> unwrap) (L.VInt 7))
;;

let%test_unit
    "selected private entry contracts and persistent events retain dependency state"
  =
  let surface = Surface.core_surface in
  let contract =
    Support.native_contract
      ~surface
      ~requirements:
        [ "initial_state", B.TInt
        ; ( "on_event"
          , B.TFun
              ( [ TRecord (TRow_extend ([ "phase", TString ], TRow_empty)); TInt; TInt ]
              , B.task_ty TInt ) )
        ]
      ()
  in
  let input =
    Support.input
      contract
      ~interfaces:
        [ "entry.chatml", "val public : int", []
        ; "counter.chatml", "val next : fun() -> int", []
        ]
      [ ( "entry.chatml"
        , "let public = 0 let initial_state = 0 let on_event ctx state event = \
           Task.pure(Counter.next())"
        , [ "Counter", "counter.chatml" ] )
      ; "counter.chatml", "let cell = ref(0) let next () = cell := !cell + 1; !cell", []
      ]
  in
  let program = Support.compiled contract input in
  Eio_main.run (fun env ->
    let runner =
      Execution.create_runner ~env ~policy:(Bounded Execution.default_limits) ()
    in
    let config = Host.default_runtime_config ~surface () in
    let session =
      Execution.run_scoped runner (fun () ->
        Host.instantiate_prepared_session_detailed
          config
          program
          ~entrypoints:
            { initial_state_name = "initial_state"; on_event_name = "on_event" }
          ~control:(Execution.runner_control runner)
          ()
        |> unwrap)
      |> Result.ok
      |> Option.value_exn
    in
    for expected = 1 to 2 do
      Execution.run_scoped runner (fun () ->
        Host.handle_event_detailed
          session
          ~context:(L.VRecord (String.Map.singleton "phase" (L.VString "turn_start")))
          ~event:(L.VInt 0)
        |> unwrap)
      |> Result.ok
      |> Option.value_exn;
      assert (L.equal_value (Host.current_state session) (L.VInt expected))
    done;
    match
      Host.handle_event_detailed
        session
        ~context:(L.VRecord (String.Map.singleton "phase" (L.VString "turn_start")))
        ~event:(L.VInt 0)
    with
    | _ -> assert false
    | exception Execution.Budget_exhausted error ->
      [%test_eq: string] error.code "chatml.inactive_scope")
;;

let%test_unit "installer metadata and unselected names reject before initializer effects" =
  let log = ref [] in
  let surface = marked_surface log Surface.core_surface in
  let program = diamond surface in
  let reject surface entrypoints =
    assert (Result.is_error (Runtime.instantiate program ~surface ~entrypoints ()));
    assert (List.is_empty !log)
  in
  reject surface [ "unknown" ];
  reject surface [];
  let globals =
    List.map surface.globals ~f:(fun (b : B.builtin) ->
      if String.equal b.name "mark" then { b with scheme = TFun ([ TInt ], TUnit) } else b)
  in
  reject { surface with globals } [ "main" ];
  let modules =
    List.map surface.modules ~f:(fun (m : B.builtin_module) ->
      if String.equal m.name "Task" then { m with exports = [] } else m)
  in
  reject { surface with modules } [ "main" ];
  reject { surface with type_aliases = [] } [ "main" ];
  List.iter
    [ C.Target.One_off_v1; Tool_v1; Moderator_v1; Delegated_moderator_v1 ]
    ~f:(fun target ->
      let contract = C.for_target target ~checkpoint:(fun () -> ()) in
      let module X = Chatml.Chatml_extension_surface in
      let actual =
        match target with
        | One_off_v1 -> X.one_off_v1
        | Tool_v1 -> X.tool_v1
        | Moderator_v1 -> X.moderator_v1
        | Delegated_moderator_v1 -> X.delegated_moderator_v1
      in
      assert (C.matches_runtime_surface contract actual ~checkpoint:(fun () -> ()) ()))
;;

let%test_unit "failed dependency stops later initializers and retains its own source" =
  let log = ref [] in
  let surface = marked_surface log Surface.core_surface in
  let contract = Support.native_contract ~surface ~requirements:[ "answer", B.TInt ] () in
  let program =
    Support.input
      contract
      [ ( "entry.chatml"
        , "let answer = Failed.value let initialize = mark(\"entry\")"
        , [ "Failed", "failed.chatml" ] )
      ; "failed.chatml", "let initialize = mark(\"failed\") let value = 1 / 0", []
      ]
    |> Support.compiled contract
  in
  (match Runtime.instantiate program ~surface ~entrypoints:[ "answer" ] () with
   | Ok _ -> assert false
   | Error error ->
     [%test_eq: string] (Support.owner (Runtime.Error.diagnostic error)) "failed.chatml");
  [%test_eq: string list] (List.rev !log) [ "failed" ]
;;

let%test_unit
    "selected imported entrypoints initialize their canonical origin despite an inert \
     open"
  =
  let log = ref [] in
  let surface = marked_surface log Surface.core_surface in
  let contract =
    Support.native_contract
      ~surface
      ~requirements:[ "main", B.TFun ([], B.task_ty TInt) ]
      ()
  in
  let program =
    Support.input
      contract
      [ "entry.chatml", "open Tools", [ "Tools", "tools.chatml" ]
      ; "tools.chatml", "let initialize = mark(\"tools\") let main () = Task.pure(42)", []
      ]
    |> Support.compiled contract
  in
  let result =
    Host.run_prepared_entrypoint_detailed
      (Host.default_runtime_config ~surface ())
      program
      ~entrypoint:"main"
      ~arguments:[]
      ()
    |> unwrap
  in
  assert (L.equal_value result (L.VInt 42));
  [%test_eq: string list] (List.rev !log) [ "tools" ]
;;

let%test_unit "deferred dependency tasks keep defining source and legacy catch semantics" =
  let surface = Surface.core_surface in
  let contract =
    Support.native_contract ~requirements:[ "main", B.TFun ([], B.task_ty TInt) ] ()
  in
  let program =
    Support.input
      contract
      [ ( "entry.chatml"
        , "let main () = Task.bind(Dependency.task(), fun x -> Task.pure(x))"
        , [ "Dependency", "dependency.chatml" ] )
      ; "dependency.chatml", "let task () = Task.map(Task.pure(0), fun x -> 1 / x)", []
      ]
    |> Support.compiled contract
  in
  match
    Host.run_prepared_entrypoint_detailed
      (Host.default_runtime_config ~surface ())
      program
      ~entrypoint:"main"
      ~arguments:[]
      ()
  with
  | Ok _ -> assert false
  | Error diagnostic -> [%test_eq: string] (Support.owner diagnostic) "dependency.chatml"
;;
