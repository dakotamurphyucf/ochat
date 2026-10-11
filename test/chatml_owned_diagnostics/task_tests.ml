open Core
module Helpers = Ownership_tests
module L = Chatml.Chatml_lang
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Host = Chatml_host_runtime
module Surface = Chatml.Chatml_builtin_surface

let checkpoint () = ()

let site source =
  let location =
    Captured.locate
      source
      ~start_byte:0
      ~end_byte:(Captured.byte_length source)
      ~checkpoint
    |> Or_error.ok_exn
  in
  Diagnostic.Site.create source ~span:(Captured.Location.span location) ~checkpoint
  |> Or_error.ok_exn
;;

let config = Host.default_runtime_config ~surface:Surface.core_surface ()

let compile text =
  let source = Helpers.capture "tasks.chatml" text in
  let compiled =
    match Host.compile_captured_script source ~surface:Surface.core_surface () with
    | Ok compiled -> compiled
    | Error diagnostic -> failwith diagnostic.formatted
  in
  source, compiled
;;

let run ?limits compiled =
  Host.run_entrypoint_detailed ?limits config compiled ~entrypoint:"main" ~arguments:[] ()
;;

let%expect_test "task origin attachment preserves semantic task identity" =
  let source = Helpers.capture "task.chatml" "Task.pure(1)" in
  let origin = Some (site source) in
  let task value = L.Task.create (L.TPure value) in
  let original = task (L.VInt 1) in
  let owned = L.Task.with_fallback original origin in
  let distinct = task (L.VInt 1) in
  assert (L.equal_value (L.VTask original) (L.VTask owned));
  assert (not (L.equal_value (L.VTask original) (L.VTask distinct)));
  assert (Option.is_none (L.Task.site original));
  assert (Option.is_some (L.Task.site owned));
  [%expect {| |}]
;;

let%expect_test "origin metadata does not add task fuel steps" =
  let text = "let main () = Task.map(Task.pure(1), fun value -> value + 1)" in
  let _, captured = compile text in
  let legacy =
    Host.compile_script ~surface:Surface.core_surface ~source:text ()
    |> Result.ok_or_failwith
  in
  let classify = function
    | Ok (L.VInt value) -> `Value value
    | Ok _ -> assert false
    | Error diagnostic -> `Failure diagnostic.Diagnostic.message
  in
  List.iter [ 0; 1; 2; 3 ] ~f:(fun fuel ->
    let limits = { Host.fuel; max_tasks = 1 } in
    let expected = classify (run ~limits legacy) in
    let actual = classify (run ~limits captured) in
    match expected, actual with
    | `Value expected, `Value actual -> assert (Int.equal expected actual)
    | `Failure expected, `Failure actual -> assert (String.equal expected actual)
    | _ -> assert false);
  (match run ~limits:{ Host.fuel = 2; max_tasks = 1 } captured with
   | Ok (L.VInt 2) -> ()
   | Ok _ | Error _ -> assert false);
  [%expect {| |}]
;;

let%expect_test "Task.catch keeps plain failure strings and callback failure behavior" =
  let _, caught =
    compile
      "let main () = Task.catch(Task.fail(\"original failure\"), fun message -> \
       Task.pure(message))"
  in
  (match run caught with
   | Ok (L.VString message) -> assert (String.equal message "original failure")
   | Ok _ | Error _ -> assert false);
  List.iter [ 16_383; 16_384; 16_385; 20_000 ] ~f:(fun length ->
    let message = String.make length 'x' in
    let _, caught =
      compile
        (Printf.sprintf
           "let main () = Task.catch(Task.fail(%S), fun message -> Task.pure(message))"
           message)
    in
    match run caught with
    | Ok (L.VString caught) -> assert (String.equal message caught)
    | Ok _ | Error _ -> assert false);
  let source, callback =
    compile
      "let main () = Task.catch(Task.bind(Task.pure(0), fun value -> Task.pure(1 / \
       value)), fun message -> Task.pure(7))"
  in
  (match run callback with
   | Ok _ -> assert false
   | Error diagnostic ->
     let location = Helpers.require_owner source diagnostic in
     assert (String.equal (Captured.read source location |> Or_error.ok_exn) "1 / value"));
  [%expect {| |}]
;;

let%expect_test "delayed task failure reports its construction expression" =
  let source, compiled = compile "let main () = Task.fail(\"delayed failure\")" in
  (match run compiled with
   | Ok _ -> assert false
   | Error diagnostic ->
     let location = Helpers.require_owner source diagnostic in
     printf
       "%s\n%s\n"
       diagnostic.message
       (Captured.read source location |> Or_error.ok_exn));
  [%expect
    {|
    delayed failure
    Task.fail("delayed failure")
    |}]
;;
