open Core
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Parse = Chatml.Chatml_parse
module Checker = Chatml_typechecker
module Lang = Chatml.Chatml_lang
module Surface = Chatml.Chatml_builtin_surface

let checkpoint () = ()

let capture path contents =
  let id =
    Captured.Id.create ~package_namespace:"owned-signatures" ~relative_path:path
    |> Or_error.ok_exn
  in
  Captured.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let unwrap = function
  | Ok value -> value
  | Error diagnostic -> failwith diagnostic.Diagnostic.message
;;

let require_owner source diagnostic =
  let site = Option.value_exn diagnostic.Diagnostic.site in
  let owner = Diagnostic.Site.source site in
  assert (Captured.Id.equal (Captured.id owner) (Captured.id source));
  assert (Captured.Digest.equal (Captured.digest owner) (Captured.digest source))
;;

let implementation ?(checkpoint = checkpoint) body =
  let source =
    capture "implementation.chatml" ("module Implementation = struct " ^ body ^ " end")
  in
  let parsed = Parse.parse_captured source ~checkpoint |> unwrap in
  let state = Checker.create_state ~source ~checkpoint () in
  let stmt = List.hd_exn (Parse.Unit.program parsed).stmts in
  let env, types =
    Checker.infer_stmt state (Checker.init_env ()) Checker.Type_alias.empty stmt.value
  in
  let values =
    match Checker.Env.find_exn env "Implementation" with
    | Module (Row (fields, Empty_row)) -> fields
    | _ -> failwith "expected module exports"
  in
  let types =
    Checker.Type_alias.declared_module types ~name:"Implementation" |> Option.value_exn
  in
  source, state, values, types
;;

let check (_, state, values, types) interface =
  Checker.check_captured_module_signature
    state
    interface
    ~values
    ~types
    ~outer_types:Checker.Type_alias.empty
;;

let%test_unit
    "companion claims own interface bytes and alias mismatches retain definitions"
  =
  let actual =
    implementation
      "type private_int = int type public_int = private_int let value = 1 let never_run \
       = 1 / 0"
  in
  List.iter
    [ "val value : bool"; "val value : missing"; "val value : fun('a) -> 'a" ]
    ~f:(fun text ->
      let source = capture "implementation.chatmli" text in
      let interface = Parse.parse_captured_interface source ~checkpoint |> unwrap in
      match check actual interface with
      | Ok _ -> failwith "invalid companion admitted"
      | Error diagnostic -> require_owner source diagnostic);
  let source = capture "implementation.chatmli" "type public_int = string" in
  let interface = Parse.parse_captured_interface source ~checkpoint |> unwrap in
  (match check actual interface with
   | Ok _ -> failwith "different alias admitted"
   | Error diagnostic ->
     require_owner source diagnostic;
     let implementation_source, _, _, _ = actual in
     let related = List.hd_exn diagnostic.related |> Diagnostic.Related.site in
     assert (
       Captured.Id.equal
         (Captured.id (Diagnostic.Site.source related))
         (Captured.id implementation_source));
     assert (
       Captured.Digest.equal
         (Captured.digest (Diagnostic.Site.source related))
         (Captured.digest implementation_source)));
  let allowed =
    capture "implementation.chatmli" "type public_int = int val value : public_int"
  in
  assert (
    Result.is_ok
      (check actual (Parse.parse_captured_interface allowed ~checkpoint |> unwrap)))
;;

let%test_unit
    "interface parsing preserves every checkpoint exception including parser collisions"
  =
  let source = capture "long.chatmli" "type 'a id = 'a val identity : fun('a) -> 'a" in
  let count = ref 0 in
  ignore (Parse.parse_captured_interface source ~checkpoint:(fun () -> Int.incr count));
  assert (!count > 0);
  for stop = 1 to !count do
    List.iter [ Failure "callback"; Chatml.Chatml_parser.Error ] ~f:(fun original ->
      let visited = ref 0 in
      match
        Parse.parse_captured_interface source ~checkpoint:(fun () ->
          Int.incr visited;
          if Int.equal !visited stop then raise original)
      with
      | exception exn -> assert (phys_equal original exn)
      | Ok _ | Error _ -> failwith "interface parser swallowed checkpoint exception")
  done;
  let malformed = capture "broken.chatmli" "val broken : )" in
  match Parse.parse_captured_interface malformed ~checkpoint with
  | Ok _ -> failwith "malformed interface parsed"
  | Error diagnostic -> require_owner malformed diagnostic
;;

let%test_unit "private parameterized aliases retain their captured defining revision" =
  let actual = implementation "type 'a hidden = 'a -> int type 'a exposed = 'a hidden" in
  let source = capture "implementation.chatmli" "type 'a exposed = fun('a) -> int" in
  let interface = Parse.parse_captured_interface source ~checkpoint |> unwrap in
  (match check actual interface with
   | Ok _ -> failwith "parameter-dependent alias admitted as fixed explicit function"
   | Error diagnostic ->
     require_owner source diagnostic;
     let definition = List.hd_exn diagnostic.related |> Diagnostic.Related.site in
     let implementation_source, _, _, _ = actual in
     assert (
       Captured.Digest.equal
         (Captured.digest (Diagnostic.Site.source definition))
         (Captured.digest implementation_source)));
  let caller = capture "caller.chatmli" "val callback : exposed" in
  let signature =
    Parse.parse_captured_interface caller ~checkpoint
    |> unwrap
    |> Parse.Interface.signature
  in
  let annotation =
    match (List.hd_exn signature).desc with
    | Value { annotation; _ } -> annotation
    | Type _ | Module _ -> failwith "expected value specification"
  in
  let _, _, _, types = actual in
  match Checker.Type_alias.elaborate ~source:caller types annotation ~checkpoint with
  | Ok _ -> failwith "missing alias argument admitted"
  | Error diagnostic -> require_owner caller diagnostic
;;

let compile source =
  let parsed = Parse.parse_captured source ~checkpoint |> unwrap in
  let checked =
    Checker.check_captured_program_with_surface ~checkpoint Surface.core_surface parsed
    |> unwrap
  in
  parsed, checked
;;

let%test_unit
    "visibility tables are per unit and lookup rejects foreign identities or revisions"
  =
  let text = "module M : sig val value : int end = struct let value = 1 end" in
  let source = capture "first.chatml" text in
  let other = capture "second.chatml" text in
  let revision = capture "first.chatml" (text ^ " ") in
  let parsed, checked = compile source in
  let span =
    match (List.hd_exn (Parse.Unit.program parsed).stmts).value with
    | SModule { signature = Some signature; _ } -> signature.span
    | _ -> failwith "expected inline signature"
  in
  let location source = Captured.of_span source span ~checkpoint |> Or_error.ok_exn in
  assert (
    Option.is_some (Checker.checked_lookup_location_visibility checked (location source)));
  List.iter [ other; revision ] ~f:(fun foreign ->
    assert (
      Option.is_none
        (Checker.checked_lookup_location_visibility checked (location foreign)));
    let other, _ = compile foreign in
    assert (Result.is_error (Chatml_resolver.resolve_captured_program checked other)));
  let state = Checker.create_state ~source () in
  let next = Checker.for_source state other in
  Hashtbl.set state.module_visibility ~key:(Checker.span_to_key span) ~data:[];
  assert (Hashtbl.is_empty next.module_visibility);
  assert (phys_equal state.shared next.shared)
;;

let%test_unit "nested selected modules retain captured owners and private closure state" =
  let source =
    capture
      "dependency.chatml"
      "module Public : sig module Inner : sig val divide : fun(int) -> int end end = \
       struct module Inner = struct let private_value = 1 let divide value = \
       private_value / value end end"
  in
  let parsed, checked = compile source in
  let resolved = Chatml_resolver.resolve_captured_program checked parsed |> unwrap in
  let env = Chatml_builtin_modules.create_default_env () in
  let env = Chatml.Chatml_eval.eval_captured_program env ~source resolved |> unwrap in
  let module_env env name =
    match Lang.find_var env name with
    | Some (VModule env) -> env
    | _ -> failwith "expected selected module"
  in
  let inner = module_env (module_env env "Public") "Inner" in
  assert (Option.is_none (Lang.find_var inner "private_value"));
  assert (
    Captured.Id.equal (Captured.id (Option.value_exn inner.source)) (Captured.id source));
  let divide = Lang.find_var inner "divide" |> Option.value_exn in
  match Chatml.Chatml_eval.apply_value_result divide [ VInt 0 ] with
  | Ok _ -> failwith "division by zero succeeded"
  | Error diagnostic -> require_owner source diagnostic
;;

let%test_unit "signature callback exceptions preserve identity and shared weak exports" =
  let interface_source =
    capture
      "implementation.chatmli"
      "val first : int array module Nested : sig val second : int array end"
  in
  let interface = Parse.parse_captured_interface interface_source ~checkpoint |> unwrap in
  let alternate =
    capture
      "alternate.chatmli"
      "val first : bool array module Nested : sig val second : bool array end"
    |> Parse.parse_captured_interface ~checkpoint
    |> unwrap
  in
  List.iter [ false; true ] ~f:(fun captured ->
    let run (_, state, values, types) =
      if captured
      then
        Checker.check_captured_module_signature
          state
          interface
          ~values
          ~types
          ~outer_types:Checker.Type_alias.empty
      else (
        let state = Checker.for_source state interface_source in
        let signature = Parse.Interface.signature interface in
        let span = (List.hd_exn signature).span in
        Checker.check_module_signature
          state
          ~values
          ~types
          ~outer_types:Checker.Type_alias.empty
          ~signature
          ~span)
    in
    let prepare original =
      let calls = ref 0 in
      let enabled = ref false in
      let stop = ref None in
      let checkpoint () =
        if !enabled
        then (
          Int.incr calls;
          if Option.exists !stop ~f:(Int.equal !calls) then raise original)
      in
      let actual =
        implementation
          ~checkpoint
          "let storage = [] let first = storage module Nested = struct let second = \
           storage end"
      in
      enabled := true;
      actual, calls, enabled, stop
    in
    let exception Interrupted in
    List.iter
      [ Interrupted; Checker.Type_error "checkpoint callback" ]
      ~f:(fun original ->
        let baseline, calls, _, _ = prepare original in
        assert (Result.is_ok (run baseline));
        assert (!calls > 0);
        for interruption = 1 to !calls do
          let actual, _, enabled, stop = prepare original in
          stop := Some interruption;
          (match run actual with
           | exception exn -> assert (phys_equal original exn)
           | Ok _ | Error _ -> failwith "signature helper swallowed checkpoint exception");
          enabled := false;
          assert (Result.is_ok (check actual alternate))
        done))
;;
