open Core
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module L = Chatml.Chatml_lang
module Parse = Chatml.Chatml_parse
module Eval = Chatml.Chatml_eval
module Builtin = Chatml.Chatml_builtin_spec
module Surface = Chatml.Chatml_builtin_surface
module Checker = Chatml_typechecker
module Resolver = Chatml_resolver

let checkpoint () = ()

let capture path contents =
  let id =
    Captured.Id.create ~package_namespace:"ownership" ~relative_path:path
    |> Or_error.ok_exn
  in
  Captured.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let unwrap = function
  | Ok value -> value
  | Error diagnostic ->
    failwith (Diagnostic.render diagnostic ~label:"Unexpected error" ~checkpoint)
;;

let compile ?(surface = Surface.core_surface) source =
  let parsed = Parse.parse_captured source ~checkpoint |> unwrap in
  let checked =
    Checker.check_captured_program_with_surface ~checkpoint surface parsed |> unwrap
  in
  parsed, checked, Resolver.resolve_captured_program checked parsed |> unwrap
;;

let runtime_env () =
  let env = L.create_env () in
  Chatml_builtin_modules.add_global_builtins env;
  env
;;

let require_owner source diagnostic =
  let site = Option.value_exn diagnostic.Diagnostic.site in
  let location = Diagnostic.Site.location site in
  assert (Captured.Id.equal (Captured.id source) (Captured.Location.source location));
  assert (
    Captured.Digest.equal
      (Captured.digest source)
      (Captured.Location.source_digest location));
  location
;;

let%expect_test
    "annotation lookups and resolution distinguish equal offsets in different sources"
  =
  let integer = capture "integer.chatml" "let result = 1" in
  let string = capture "string.chatml" "let result = \"text\"" in
  let revision = capture "integer.chatml" "let result = 2" in
  let parsed, checked, _ = compile integer in
  let other, _, _ = compile string in
  let changed, _, _ = compile revision in
  let location source end_byte =
    Captured.locate source ~start_byte:13 ~end_byte ~checkpoint |> Or_error.ok_exn
  in
  let own = location integer 14 in
  assert (Option.is_some (Checker.checked_lookup_location_type checked own));
  assert (
    Option.is_none (Checker.checked_lookup_location_type checked (location string 14)));
  assert (
    Option.is_none (Checker.checked_lookup_location_type checked (location revision 14)));
  assert (Result.is_ok (Resolver.resolve_captured_program checked parsed));
  assert (Result.is_error (Resolver.resolve_captured_program checked other));
  assert (Result.is_error (Resolver.resolve_captured_program checked changed));
  assert (
    Result.is_error
      (Checker.check_program_with_surface
         ~source:string
         Surface.core_surface
         (Parse.Unit.program parsed)));
  let owned_env = L.create_env ~source:string () in
  let _, _, resolved = compile integer in
  (match Eval.eval_program owned_env resolved with
   | () -> assert false
   | exception L.Runtime_error diagnostic -> assert (Option.is_none diagnostic.site));
  assert (Result.is_error (Eval.eval_captured_program owned_env ~source:string resolved));
  [%expect {| |}]
;;

let%expect_test
    "per-unit annotation tables preserve shared inference identities and weak cells"
  =
  let first = capture "first.chatml" "1" in
  let second = capture "second.chatml" "1" in
  let left = Checker.create_state ~source:first () in
  let right = Checker.for_source left second in
  let names =
    List.init 20 ~f:(fun i -> Checker.gensym (if i mod 2 = 0 then left else right))
  in
  assert (List.length (List.dedup_and_sort names ~compare:String.compare) = 20);
  let weak = Checker.Array (Checker.new_var left 0) in
  let span =
    Captured.locate first ~start_byte:0 ~end_byte:1 ~checkpoint
    |> Or_error.ok_exn
    |> Captured.Location.span
  in
  Checker.record_span_type left span weak;
  Checker.record_span_type right span Checker.Boolean;
  let lookup (state : Checker.infer_state) = Hashtbl.find_exn state.span_types (0, 1) in
  Checker.unify right (lookup left) (Checker.Array Checker.TInt);
  Checker.unify left (lookup right) Checker.Boolean;
  (match Checker.unify right weak (Checker.Array Checker.Boolean) with
   | () -> assert false
   | exception Checker.Type_error _ -> ());
  [%expect {| |}]
;;

let%expect_test "captured parse and type errors preserve exact owners" =
  let malformed = capture "broken.chatml" "let result = )" in
  (match Parse.parse_captured malformed ~checkpoint with
   | Ok _ -> assert false
   | Error diagnostic -> ignore (require_owner malformed diagnostic : Captured.Location.t));
  let ill_typed = capture "wrong.chatml" "let result = 1 + true" in
  let parsed = Parse.parse_captured ill_typed ~checkpoint |> unwrap in
  (match Checker.check_captured_program_with_surface Surface.core_surface parsed with
   | Ok _ -> assert false
   | Error diagnostic -> ignore (require_owner ill_typed diagnostic : Captured.Location.t));
  [%expect {| |}]
;;

let%expect_test
    "deferred direct tail and builtin callback failures retain the dependency expression"
  =
  let dependency =
    capture "dependency.chatml" "let dependency_divide value = 1 / value"
  in
  let _, _, resolved = compile dependency in
  let dependency_env =
    Eval.eval_captured_program (runtime_env ()) ~source:dependency resolved |> unwrap
  in
  let callback = L.find_var dependency_env "dependency_divide" |> Option.value_exn in
  let signature =
    { Builtin.name = "dependency_divide"
    ; scheme = Builtin.TFun ([ Builtin.TInt ], Builtin.TInt)
    ; impl = (fun _ -> assert false)
    }
  in
  let surface =
    { Surface.core_surface with globals = signature :: Surface.core_surface.globals }
  in
  List.iter
    [ "direct", "let result = dependency_divide(0)"
    ; "tail", "let tail n = dependency_divide(n)\nlet result = tail(0)"
    ; "callback", "let result = Array.map([0], dependency_divide)"
    ]
    ~f:(fun (label, text) ->
      let caller = capture (label ^ ".chatml") text in
      let _, _, resolved = compile ~surface caller in
      let env = runtime_env () in
      L.define_var env "dependency_divide" callback;
      match Eval.eval_captured_program env ~source:caller resolved with
      | Ok _ -> assert false
      | Error diagnostic ->
        let location = require_owner dependency diagnostic in
        let selected = Captured.read dependency location |> Or_error.ok_exn in
        assert (String.equal selected "1 / value");
        printf "%s: dependency.chatml: %s\n" label diagnostic.message);
  [%expect
    {|
    direct: dependency.chatml: Division by zero
    tail: dependency.chatml: Division by zero
    callback: dependency.chatml: Division by zero
    |}]
;;
