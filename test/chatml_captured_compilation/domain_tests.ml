open Core
module Compiler = Chatml_compilation
module Host = Chatml_host_runtime
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Lang = Chatml.Chatml_lang
module Surface = Chatml.Chatml_builtin_surface
module Builtin = Chatml.Chatml_builtin_spec

let capture contents =
  let id =
    Captured.Id.create ~package_namespace:"domain-test" ~relative_path:"handler.chatml"
    |> Or_error.ok_exn
  in
  Captured.create
    id
    ~contents
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint:(fun () -> ())
  |> Or_error.ok_exn
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

let compiled = function
  | Ok program -> program
  | Error error -> failwith error.Compiler.message
;;

let surface_contract (surface : Surface.surface) =
  let bindings entries =
    List.map entries ~f:(fun (entry : Builtin.builtin) -> entry.name, entry.scheme)
  in
  [%sexp
    (bindings surface.globals : (string * Builtin.ty) list)
  , (List.map surface.modules ~f:(fun (entry : Builtin.builtin_module) ->
       entry.name, bindings entry.exports)
     : (string * (string * Builtin.ty) list) list)
  , (List.map surface.type_aliases ~f:(fun (entry : Surface.builtin_type_alias) ->
       entry.name, entry.body)
     : (string * Builtin.ty) list)]
;;

let%expect_test "domain policy preserves legacy validation and captured error ownership" =
  Eio_main.run (fun env ->
    let cases =
      [ Compiler.One_off_v1, "let main input = Task.pure(input)"
      ; Compiler.Tool_v1, "let run ctx input = Task.pure(`Complete(input))"
      ; ( Compiler.Moderator_v1
        , "let initial_state = 0\nlet on_event ctx state event = Task.pure(state)" )
      ; ( Compiler.Delegated_moderator_v1
        , "let initial_state = 0\nlet on_event ctx state event = Task.pure(state)" )
      ; Compiler.One_off_v1, "let main input ="
      ; Compiler.Tool_v1, "let run ctx input = 1 + true"
      ]
    in
    List.iter
      [ Compiler.Bounded Compiler.default_limits; Compiler.Unrestricted ]
      ~f:(fun policy ->
        List.iter cases ~f:(fun (target, text) ->
          let source = capture text in
          let legacy =
            Compiler.compile_with_policy ~policy ~env ~target ~source:text ()
          in
          let owned =
            Compiler.compile_captured_with_policy source ~policy ~env ~target ()
          in
          match legacy, owned with
          | Ok legacy, Ok owned ->
            assert (
              Sexp.equal
                (surface_contract (Host.compiled_surface legacy))
                (surface_contract (Host.compiled_surface owned)))
          | Error legacy, Error owned ->
            [%test_eq: string] legacy.code owned.code;
            let legacy = Option.value_exn legacy.diagnostic in
            let owned = Option.value_exn owned.diagnostic in
            assert (Host.equal_compilation_stage legacy.stage owned.stage);
            [%test_eq: string] legacy.message owned.message;
            assert (Option.is_none legacy.diagnostic.site);
            let location = require_owner source owned.diagnostic in
            let span = Captured.Location.span location in
            let legacy_span = Option.value_exn legacy.span in
            [%test_eq: int * int]
              (span.left.offset, span.right.offset)
              (legacy_span.left.offset, legacy_span.right.offset)
          | Ok _, Error _ | Error _, Ok _ ->
            failwith "captured compilation changed validation")));
  [%expect {| |}]
;;

let%expect_test "joined domain artifacts retain eager and deferred source failures" =
  Eio_main.run (fun env ->
    List.iter
      [ "let poison = 1 / 0\nlet main input = Task.pure(input)", "1 / 0"
      ; ( "let main input = Task.fail(\"deferred failure\")"
        , "Task.fail(\"deferred failure\")" )
      ]
      ~f:(fun (text, failing_expression) ->
        let source = capture text in
        (* The initializer deliberately fails. Admission here proves domain
           compilation does not initialize the program. Only execution below
           may fail, after the compilation-owned domain has joined. *)
        let program =
          Compiler.compile_captured source ~env ~target:Compiler.One_off_v1 () |> compiled
        in
        let changed = capture (text ^ "\n(* newer revision *)") in
        let config =
          Host.default_runtime_config ~surface:(Host.compiled_surface program) ()
        in
        match
          Host.run_entrypoint_detailed
            config
            program
            ~entrypoint:"main"
            ~arguments:[ Lang.VVariant ("Null", []) ]
            ()
        with
        | Ok _ -> failwith "expected source-owned runtime failure"
        | Error diagnostic ->
          let location = require_owner source diagnostic in
          [%test_eq: string]
            (Captured.read source location |> Or_error.ok_exn)
            failing_expression;
          assert (Result.is_error (Captured.read changed location))));
  [%expect {| |}]
;;

let%expect_test "captured byte limits reject before compiler validation" =
  Eio_main.run (fun env ->
    let source = capture "let main input =" in
    let check code result =
      match result with
      | Ok _ -> failwith "expected pre-domain source/policy rejection"
      | Error error ->
        [%test_eq: string] error.Compiler.code code;
        assert (Option.is_none error.diagnostic)
    in
    check
      "chatml.source_limit"
      (Compiler.compile_captured
         source
         ~limits:{ Compiler.default_limits with max_source_bytes = 1 }
         ~env
         ~target:Compiler.One_off_v1
         ());
    check
      "chatml.invalid_limits"
      (Compiler.compile_captured
         source
         ~limits:{ Compiler.default_limits with wall_seconds = 0. }
         ~env
         ~target:Compiler.One_off_v1
         ());
    [%test_eq: string] (Captured.contents source) "let main input =");
  [%expect {| |}]
;;
