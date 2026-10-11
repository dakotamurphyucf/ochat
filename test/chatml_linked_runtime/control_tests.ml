open Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module Surface = Chatml.Chatml_builtin_surface
module Runtime = Chatml_linked_runtime
module Execution = Chatml_execution

let control ~checkpoint ~allocate : L.execution_control =
  { checkpoint
  ; diagnostic_checkpoint = checkpoint
  ; allocate
  ; before_builtin = (fun ~name:_ _ -> ())
  ; check_value = (fun _ -> ())
  ; before_json_import = (fun _ -> ())
  ; before_json_export = (fun _ -> ())
  ; before_effect = (fun ~name:_ ~spawned:_ -> ())
  ; after_effect = (fun _ -> ())
  }
;;

exception Interrupted of int ref

let%test_unit
    "every observed initialization checkpoint preserves interruption identity and no \
     partial result"
  =
  let log = ref [] in
  let surface = Runtime_tests.marked_surface log Surface.empty in
  let contract = Support.native_contract ~surface ~requirements:[ "answer", B.TInt ] () in
  let program =
    Support.input
      contract
      [ ( "entry.chatml"
        , "let answer = Dep.value let initialize = mark(\"entry\")"
        , [ "Dep", "dep.chatml" ] )
      ; "dep.chatml", "let value = mark(\"dep\"); 1", []
      ]
    |> Support.compiled contract
  in
  let polls = ref 0 in
  let allocated = ref 0 in
  let baseline =
    control
      ~checkpoint:(fun () -> incr polls)
      ~allocate:(fun bytes -> allocated := !allocated + bytes)
  in
  Runtime.instantiate program ~surface ~entrypoints:[ "answer" ] ~control:baseline ()
  |> Runtime_tests.initialized
  |> ignore;
  assert (!polls > 0);
  assert (!allocated > 0);
  let total = !polls in
  for stop = 1 to total do
    polls := 0;
    log := [];
    let identity = ref stop in
    let interrupted =
      control
        ~allocate:(fun _ -> ())
        ~checkpoint:(fun () ->
          incr polls;
          if Int.equal !polls stop then raise (Interrupted identity))
    in
    (match
       Runtime.instantiate
         program
         ~surface
         ~entrypoints:[ "answer" ]
         ~control:interrupted
         ()
     with
     | _ -> assert false
     | exception Interrupted actual -> assert (phys_equal identity actual));
    [%test_eq: string list]
      (List.rev !log)
      (List.take [ "dep"; "entry" ] (List.length !log))
  done;
  log := [];
  let fresh =
    Runtime.instantiate program ~surface ~entrypoints:[ "answer" ] ()
    |> Runtime_tests.initialized
  in
  assert (L.equal_value (Runtime_tests.value fresh "answer") (L.VInt 1));
  [%test_eq: string list] (List.rev !log) [ "dep"; "entry" ]
;;

let%test_unit "aggregate fuel and allocation apply across units without new budget scopes"
  =
  let surface = Surface.empty in
  let contract = Support.native_contract ~surface ~requirements:[ "answer", B.TInt ] () in
  let two =
    Support.input
      contract
      [ "entry.chatml", "let answer = Dep.value + 3", [ "Dep", "dep.chatml" ]
      ; "dep.chatml", "let value = 1 + 2", []
      ]
    |> Support.compiled contract
  in
  let single =
    Support.input contract [ "entry.chatml", "let answer = 1 + 2", [] ]
    |> Support.compiled contract
  in
  Eio_main.run (fun env ->
    let run program limits =
      Execution.with_control ~env ~policy:(Bounded limits) (fun control ->
        Runtime.instantiate program ~surface ~entrypoints:[ "answer" ] ?control ())
    in
    let limits = { Execution.default_limits with fuel = 5 } in
    assert (Result.is_ok (run single limits));
    (match run two limits with
     | Ok _ -> assert false
     | Error error -> [%test_eq: string] error.code "chatml.execution_limit");
    match run single { Execution.default_limits with allocation_bytes = 1 } with
    | Ok _ -> assert false
    | Error error -> [%test_eq: string] error.code "chatml.allocation_limit")
;;
