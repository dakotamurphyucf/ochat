open Core
module X = Chatml_execution
module L = Chatml.Chatml_lang
module Host = Chatml_host_runtime
module Surface = Chatml.Chatml_builtin_surface
module Helpers = Ownership_tests

let unwrap = Result.map_error ~f:(fun error -> error.X.message)

let%expect_test "diagnostic polling preserves fuel and obeys scope lifetime" =
  Eio_main.run (fun env ->
    List.iter [ 0; 1; 64; 4096 ] ~f:(fun polls ->
      let escaped = ref None in
      let result =
        X.with_control
          ~env
          ~policy:(Bounded { X.default_limits with fuel = 1 })
          (fun control ->
             let control = Option.value_exn control in
             escaped := Some control;
             for _ = 1 to polls do
               control.diagnostic_checkpoint ()
             done;
             control.checkpoint ();
             control.diagnostic_checkpoint ())
      in
      assert (Result.is_ok result);
      let control = Option.value_exn !escaped in
      match control.diagnostic_checkpoint () with
      | () -> assert false
      | exception X.Budget_exhausted error ->
        assert (String.equal error.code "chatml.inactive_scope"));
    let result =
      X.with_control
        ~env
        ~policy:(Bounded { X.default_limits with fuel = 1 })
        (fun control ->
           let control = Option.value_exn control in
           control.checkpoint ();
           control.diagnostic_checkpoint ();
           control.checkpoint ())
    in
    match result with
    | Ok _ -> assert false
    | Error error -> assert (String.equal error.code "chatml.execution_limit"));
  [%expect {| |}]
;;

let%expect_test "captured ownership does not change evaluator fuel thresholds" =
  Eio_main.run (fun env ->
    let text = "let main () = Task.map(Task.pure(1), fun value -> value + 1)" in
    let legacy =
      Host.compile_script ~surface:Surface.core_surface ~source:text ()
      |> Result.ok_or_failwith
    in
    let source = Helpers.capture "execution.chatml" text in
    let captured =
      Host.compile_captured_script source ~surface:Surface.core_surface ()
      |> Result.map_error ~f:(fun error -> error.Host.formatted)
      |> Result.ok_or_failwith
    in
    let config = Host.default_runtime_config ~surface:Surface.core_surface () in
    let run fuel program =
      X.run
        ~env
        ~policy:(Bounded { X.default_limits with fuel })
        ~config
        ~program
        ~entrypoint:"main"
        ~arguments:[]
        ()
    in
    for fuel = 1 to 128 do
      match run fuel legacy, run fuel captured with
      | Ok (L.VInt expected), Ok (L.VInt actual) -> assert (Int.equal expected actual)
      | Error expected, Error actual -> assert (String.equal expected.code actual.code)
      | _ -> failwith "source ownership changed evaluator fuel behavior"
    done;
    assert (Result.is_ok (run 128 captured)));
  [%expect {| |}]
;;
