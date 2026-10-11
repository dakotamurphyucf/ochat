open Core
module B = Chatml.Chatml_builtin_spec
module L = Chatml.Chatml_lang
module Surface = Chatml.Chatml_builtin_surface
module C = Chatml.Chatml_compiler_contract
module Compiler = Chatml_compilation
module Host = Chatml_host_runtime
module S = Chatml_source

let contract surface state hook =
  C.for_static_surface
    (Chatml.Chatml_static_surface.of_runtime_surface
       surface
       ~checkpoint:Support.checkpoint)
    ~required_bindings:[ "initial_state", state; "hook", hook ]
;;

let compile env contract input =
  Compiler.compile_prepared_with_contract
    input
    ~contract
    ~limits:Support.compiler_limits
    ~max_dependency_depth:16
    ~env
    ()
  |> Support.ok
;;

let native_session surface program =
  Host.instantiate_prepared_event_state_session_detailed
    (Host.default_runtime_config ~surface ())
    program
    ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "hook" }
    ()
;;

let owner diagnostic =
  let site = Option.value_exn diagnostic.Chatml_diagnostic.site in
  S.Location.source (Chatml_diagnostic.Site.location site) |> S.Id.relative_path
;;

let%expect_test
    "native graph compiler defers initializer and keeps exact dependency error owner"
  =
  Eio_main.run (fun env ->
    let surface = Surface.core_surface in
    let c = contract surface B.TInt (B.TFun ([ TInt; TInt ], B.task_ty TInt)) in
    let poison =
      "let initial_state = 1 / 0 let hook event state = Task.pure(event+state)"
    in
    let input = Support.input c [ "entry.chatml", poison, [] ] in
    let program = compile env c input in
    let diagnostic =
      match native_session surface program with
      | Error diagnostic -> diagnostic
      | Ok _ -> failwith "initializer poison executed successfully"
    in
    print_s [%sexp (owner diagnostic : string)];
    let broken =
      Support.input
        c
        [ ( "entry.chatml"
          , "let initial_state = Dep.value let hook event state = Task.pure(event+state)"
          , [ "Dep", "dep.chatml" ] )
        ; "dep.chatml", "let value = 1 + \"bad\"", []
        ]
    in
    match
      Compiler.compile_prepared_with_contract
        broken
        ~contract:c
        ~limits:Support.compiler_limits
        ~max_dependency_depth:16
        ~env
        ()
    with
    | Ok _ -> assert false
    | Error failure ->
      let diagnostic = Option.value_exn failure.diagnostic in
      let site = Option.value_exn diagnostic.diagnostic.site in
      [%test_eq: string]
        (S.Digest.to_hex
           (S.Location.source_digest (Chatml_diagnostic.Site.location site)))
        (S.Digest.of_contents "let value = 1 + \"bad\"" ~checkpoint:Support.checkpoint
         |> S.Digest.to_hex);
      print_s
        [%sexp
          (S.Location.source (Chatml_diagnostic.Site.location site) |> S.Id.relative_path
           : string)]);
  [%expect
    {|
    entry.chatml
    dep.chatml
  |}]
;;

let%test_unit
    "native event/state preflight rejects incoherent roles before marked initializer"
  =
  Eio_main.run (fun env ->
    let calls = ref 0 in
    let marker =
      B.
        { name = "mark"
        ; scheme = TFun ([], TUnit)
        ; impl =
            (fun _ ->
              incr calls;
              L.VUnit)
        }
    in
    let surface =
      Surface.merge { Surface.empty with globals = [ marker ] } Surface.core_surface
    in
    let cases =
      [ B.TFun ([ TInt; TInt ], TInt), "let hook event state = event+state"
      ; ( B.TFun ([ TInt; TInt; TInt ], B.task_ty TInt)
        , "let hook ctx state event = Task.pure(state)" )
      ; ( B.TFun ([ TInt; TBool ], B.task_ty TBool)
        , "let hook event state = Task.pure(state)" )
      ]
    in
    List.iter cases ~f:(fun (scheme, body) ->
      let c = contract surface B.TInt scheme in
      let program =
        compile
          env
          c
          (Support.input
             c
             [ "entry.chatml", "let marked = mark() let initial_state = 10 " ^ body, [] ])
      in
      calls := 0;
      assert (Result.is_error (native_session surface program));
      [%test_eq: int] !calls 0);
    let c = contract surface B.TInt (B.TFun ([ TInt; TInt ], B.task_ty TInt)) in
    let program =
      compile
        env
        c
        (Support.input
           c
           [ ( "entry.chatml"
             , "let marked = mark() let initial_state = 10 let hook event state = \
                Task.pure(state-event)"
             , [] )
           ])
    in
    let session = native_session surface program |> Support.ok in
    [%test_eq: int] !calls 1;
    Host.handle_event_detailed session ~context:Support.context ~event:(L.VInt 3)
    |> Support.ok;
    assert (L.equal_value (Host.current_state session) (L.VInt 7));
    let standard = C.for_target Moderator_v1 ~checkpoint:Support.checkpoint in
    assert (
      Result.is_error
        (C.Invocation.event_state_session
           standard
           ~initial_state:"initial_state"
           ~on_event:"on_event"
           ~checkpoint:Support.checkpoint
           ())))
;;

let%test_unit "native metadata and expected original body share bounded worker admission" =
  Eio_main.run (fun env ->
    let c =
      contract Surface.core_surface B.TInt (B.TFun ([ TInt; TInt ], B.task_ty TInt))
    in
    let body = "let initial_state = 0 let hook event state = Task.pure(state)" in
    let input = Support.input c [ "entry.chatml", body, [] ] in
    let input_bytes =
      Chatml_prepared_program.Accounting.input_bytes
        (Chatml_prepared_program.accounting input)
    in
    let run ?expected_entry limit =
      Compiler.compile_prepared_with_contract
        input
        ~contract:c
        ?expected_entry
        ~limits:{ Support.compiler_limits with max_source_bytes = limit }
        ~max_dependency_depth:16
        ~env
        ()
    in
    let rejected code (result : (_, Compiler.error) Result.t) =
      match result with
      | Ok _ -> assert false
      | Error error ->
        [%test_eq: string] error.code code;
        assert (Option.is_none error.diagnostic)
    in
    rejected "chatml.metadata_allocation_limit" (run (input_bytes + 1));
    let digest = S.Digest.of_contents body ~checkpoint:Support.checkpoint in
    rejected
      "chatml.source_limit"
      (run
         ~expected_entry:{ contents = String.make 4096 'x'; digest }
         (input_bytes + 4095));
    let wrong = S.Digest.of_contents (body ^ " ") ~checkpoint:Support.checkpoint in
    (match
       run
         ~expected_entry:{ contents = body; digest = wrong }
         Support.compiler_limits.max_source_bytes
     with
     | Ok _ -> assert false
     | Error error -> [%test_eq: string] error.code "chatml.entry_mismatch");
    ignore
      (run
         ~expected_entry:{ contents = body; digest }
         Support.compiler_limits.max_source_bytes
       |> Support.ok
       : Chatml_linked_program.t))
;;

exception Interrupted of int ref

let%test_unit
    "canonical contract measurement is independently sized and interruption preserves \
     identity"
  =
  let c = contract Surface.empty B.TInt (B.TFun ([ TInt; TInt ], B.task_ty TInt)) in
  let polls = ref 0 in
  let allocated = ref 0 in
  let bytes =
    C.input_bytes
      c
      ~checkpoint:(fun () -> incr polls)
      ~allocate:(fun n -> allocated := !allocated + n)
  in
  [%test_eq: int]
    bytes
    (C.identity c ~checkpoint:Support.checkpoint |> Sexp.to_string_mach |> String.length);
  assert (!polls > 0);
  assert (!allocated >= bytes);
  let count = !polls in
  for stop = 1 to count do
    polls := 0;
    let identity = ref stop in
    match
      C.input_bytes
        c
        ~checkpoint:(fun () ->
          incr polls;
          if Int.equal !polls stop then raise (Interrupted identity))
        ~allocate:ignore
    with
    | _ -> assert false
    | exception Interrupted actual -> assert (phys_equal identity actual)
  done;
  [%test_eq: int] (C.input_bytes c ~checkpoint:Support.checkpoint ~allocate:ignore) bytes
;;
