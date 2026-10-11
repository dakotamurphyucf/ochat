open! Core
module L = Chatml.Chatml_lang
module Source = Chatml_source
module Diagnostic = Chatml_diagnostic

let capture path contents =
  let id =
    Source.Id.create ~package_namespace:"binders" ~relative_path:path |> Or_error.ok_exn
  in
  Source.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint:(fun () ->
    ())
  |> Or_error.ok_exn
;;

let unwrap = function
  | Ok value -> value
  | Error diagnostic -> raise_s [%sexp (diagnostic : Diagnostic.t)]
;;

let control checkpoint : L.execution_control =
  { checkpoint
  ; diagnostic_checkpoint = checkpoint
  ; allocate = (fun _ -> checkpoint ())
  ; before_builtin = (fun ~name:_ _ -> checkpoint ())
  ; check_value = (fun _ -> checkpoint ())
  ; before_json_import = (fun _ -> checkpoint ())
  ; before_json_export = (fun _ -> checkpoint ())
  ; before_effect = (fun ~name:_ ~spawned:_ -> checkpoint ())
  ; after_effect = (fun _ -> checkpoint ())
  }
;;

let%test_unit
    "every observed compiler checkpoint propagates and captured binders are reusable"
  =
  let source =
    capture
      "compile.chatml"
      {|module P : sig val number : int val label : string end = struct
        let (|number, label|) = (|7, "x"|)
      end
      let consume (|n, (|_, text|)|) = text ++ to_string(n)
      let result = consume((|P.number, (|false, P.label|)|))|}
  in
  Support.assert_checkpoints ~f:(fun checkpoint ->
    assert (
      Result.is_ok (Chatml_host_runtime.compile_captured_script source ~checkpoint ())));
  let parsed =
    Chatml.Chatml_parse.parse_captured source ~checkpoint:(fun () -> ()) |> unwrap
  in
  let program = Chatml.Chatml_parse.Unit.program parsed in
  let checked =
    Chatml_typechecker.check_captured_program_with_surface
      Chatml.Chatml_builtin_surface.core_surface
      parsed
    |> unwrap
  in
  let other = capture "other.chatml" program.source_text in
  let other_unit =
    Chatml.Chatml_parse.parse_captured other ~checkpoint:(fun () -> ()) |> unwrap
  in
  assert (Result.is_error (Chatml_resolver.resolve_captured_program checked other_unit))
;;

let%test_unit "cancellation cannot publish a proper subset of a destructuring declaration"
  =
  let program = Support.resolved "let (|left, (|_, right|)|) = (|7, (|false, \"x\"|)|)" in
  Support.assert_checkpoints ~f:(fun checkpoint ->
    let env = L.create_env ~control:(control checkpoint) () in
    L.define_var env "left" (L.VInt 99);
    L.define_var env "right" (L.VString "old");
    match Chatml.Chatml_eval.eval_program env program with
    | () ->
      assert (L.equal_value (Option.value_exn (L.find_var env "left")) (L.VInt 7));
      assert (L.equal_value (Option.value_exn (L.find_var env "right")) (L.VString "x"))
    | exception original ->
      assert (L.equal_value (Option.value_exn (L.find_var env "left")) (L.VInt 99));
      assert (L.equal_value (Option.value_exn (L.find_var env "right")) (L.VString "old"));
      raise original)
;;

let%test_unit
    "host tuple argument mismatches retain the callee binder source and remain reusable"
  =
  let text = "let f : fun(int * bool) -> int = fun (|n, _|) -> n" in
  let source = capture "callee.chatml" text in
  let env = L.create_env ~source () in
  Chatml.Chatml_eval.eval_program env (Support.resolved text);
  let fn = Option.value_exn (L.find_var env "f") in
  let wrong = Chatml.Chatml_eval.apply_value_result fn [ L.VInt 7 ] in
  (match wrong with
   | Error { site = Some site; _ } ->
     assert (Source.Id.equal (Source.id (Diagnostic.Site.source site)) (Source.id source));
     assert (
       Source.Digest.equal
         (Source.digest (Diagnostic.Site.source site))
         (Source.digest source));
     let span = Source.Location.span (Diagnostic.Site.location site) in
     [%test_eq: string]
       (String.sub text ~pos:span.left.offset ~len:(span.right.offset - span.left.offset))
       "(|n, _|)"
   | _ -> failwith "Missing owned tuple binder mismatch");
  let correct =
    L.VTuple (Chatml.Chatml_tuple.create (L.VInt 7) (L.VBool true) ~rest:[])
  in
  match Chatml.Chatml_eval.apply_value_result fn [ correct ] with
  | Ok (L.VInt n) -> [%test_eq: int] n 7
  | _ -> failwith "Tuple closure was not reusable after rejected host input"
;;

let%test_unit
    "tuple parameter staging propagates every observed callback and remains reusable"
  =
  let program =
    Support.resolved "let f : fun(int * bool, string) -> int = fun (|n, _|) text -> n"
  in
  Support.assert_checkpoints ~f:(fun checkpoint ->
    let env = L.create_env ~control:(control checkpoint) () in
    Chatml.Chatml_eval.eval_program env program;
    let fn = Option.value_exn (L.find_var env "f") in
    let tuple =
      L.VTuple (Chatml.Chatml_tuple.create (L.VInt 7) (L.VBool true) ~rest:[])
    in
    match Chatml.Chatml_eval.apply_value_result fn [ tuple; L.VString "x" ] with
    | Ok (L.VInt n) -> [%test_eq: int] n 7
    | _ -> failwith "Unexpected tuple closure application result")
;;

let%test_unit "module tuple exports become visible only after complete checked admission" =
  let program =
    Support.resolved
      {|module M : sig val left : int val right : string end = struct
        let (|left, right|) = (|7, "x"|)
      end|}
  in
  Support.assert_checkpoints ~f:(fun checkpoint ->
    let env = L.create_env ~control:(control checkpoint) () in
    match Chatml.Chatml_eval.eval_program env program with
    | () ->
      (match L.find_var env "M" with
       | Some (L.VModule module_env) ->
         [%test_eq: int] (Hashtbl.length module_env.bindings) 2;
         assert (
           L.equal_value (Option.value_exn (L.find_var module_env "left")) (L.VInt 7));
         assert (
           L.equal_value
             (Option.value_exn (L.find_var module_env "right"))
             (L.VString "x"))
       | _ -> failwith "Missing complete checked tuple exports")
    | exception original ->
      assert (Option.is_none (L.find_var env "M"));
      raise original)
;;
