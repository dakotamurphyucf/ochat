open Core
module Helpers = Ownership_tests
module Parse = Chatml.Chatml_parse
module Checker = Chatml_typechecker
module Host = Chatml_host_runtime
module Surface = Chatml.Chatml_builtin_surface

(* Raising at every observed callback position checks the public boundary, not
   the checkpoint wrapper's internal implementation. Physical exception identity
   is intentional: cancellation must not be reconstructed or converted. *)
let require_unchanged exceptions run =
  let calls = ref 0 in
  run (fun () -> Int.incr calls);
  assert (!calls > 0);
  List.iter exceptions ~f:(fun original ->
    for position = 1 to !calls do
      let seen = ref 0 in
      match
        run (fun () ->
          Int.incr seen;
          if Int.equal !seen position then raise original)
      with
      | () -> failwith "compiler swallowed a checkpoint exception"
      | exception actual -> assert (phys_equal original actual)
    done)
;;

let%test_unit "compiler-domain exceptions from checkpoints retain their original identity"
  =
  let source = Helpers.capture "checkpoints.chatml" "let result = 1" in
  let parsed = Parse.parse_captured source ~checkpoint:(fun () -> ()) |> Helpers.unwrap in
  let span =
    Chatml_source.locate source ~start_byte:0 ~end_byte:1 ~checkpoint:(fun () -> ())
    |> Or_error.ok_exn
    |> Chatml_source.Location.span
  in
  let exceptions =
    [ Chatml.Chatml_parser.Error
    ; Checker.Type_error "checkpoint type error"
    ; Checker.Type_error_with_loc ("checkpoint located error", span)
    ; Failure "checkpoint failure"
    ]
  in
  require_unchanged exceptions (fun checkpoint ->
    ignore (Parse.parse_captured source ~checkpoint));
  require_unchanged exceptions (fun checkpoint ->
    ignore
      (Checker.check_captured_program_with_surface
         ~checkpoint
         Surface.core_surface
         parsed));
  require_unchanged exceptions (fun checkpoint ->
    ignore
      (Host.compile_captured_script source ~checkpoint ~surface:Surface.core_surface ()))
;;
