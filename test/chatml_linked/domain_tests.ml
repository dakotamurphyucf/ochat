open Core
module Compiler = Chatml_compilation
module C = Chatml.Chatml_compiler_contract
module Linked = Chatml_linked_program
module Host = Chatml_host_runtime

let%test_unit
    "prepared domain compilation preserves selected standard contracts and never \
     initializes"
  =
  Eio_main.run (fun env ->
    let cases =
      [ ( Compiler.One_off_v1
        , C.Target.One_off_v1
        , "let poison = 1 / 0 let main input = Task.pure(input)" )
      ; Tool_v1, Tool_v1, "let run ctx input = Task.pure(`Complete(input))"
      ; ( Moderator_v1
        , Moderator_v1
        , "let initial_state = 0 let on_event ctx state event = Task.pure(state)" )
      ; ( Delegated_moderator_v1
        , Delegated_moderator_v1
        , "let initial_state = 0 let on_event ctx state event = Task.pure(state)" )
      ]
    in
    List.iter [ Compiler.Bounded Compiler.default_limits; Unrestricted ] ~f:(fun policy ->
      List.iter cases ~f:(fun (target, kind, source) ->
        let contract = C.for_target kind ~checkpoint:(fun () -> ()) in
        let prepared = Support.input contract [ "entry.chatml", source, [] ] in
        let legacy = Compiler.compile_with_policy ~policy ~env ~target ~source () in
        let linked =
          Compiler.compile_prepared_with_policy
            prepared
            ~policy
            ~env
            ~target
            ~max_dependency_depth:16
            ()
        in
        match legacy, linked with
        | Ok _, Ok linked ->
          [%test_eq: int]
            (List.length (Linked.entrypoints linked))
            (List.length (C.required_bindings contract));
          assert (
            Sexp.equal
              (C.identity (Linked.static_contract linked) ~checkpoint:(fun () -> ()))
              (Compiler.contract target))
        | Error error, _ | _, Error error -> failwith error.message)))
;;

let%test_unit
    "aggregate admission precedes domain compilation and actual target mismatch precedes \
     parse"
  =
  Eio_main.run (fun env ->
    let contract = C.for_target C.Target.One_off_v1 ~checkpoint:(fun () -> ()) in
    let input =
      Support.input
        contract
        [ "entry.chatml", "let main input = Task.pure(input)", []
        ; "unused.chatml", String.make 10000 ' ', []
        ]
    in
    (match
       Compiler.compile_prepared
         input
         ~limits:{ Compiler.wall_seconds = 5.; max_source_bytes = 1000 }
         ~env
         ~target:One_off_v1
         ~max_dependency_depth:16
         ()
     with
     | Error error ->
       [%test_eq: string] error.code "chatml.source_limit";
       assert (Option.is_none error.diagnostic)
     | Ok _ -> failwith "Unused inventory escaped aggregate admission");
    let invalid = Support.input contract [ "entry.chatml", "let main =", [] ] in
    match
      Compiler.compile_prepared invalid ~env ~target:Tool_v1 ~max_dependency_depth:16 ()
    with
    | Error error ->
      let diagnostic = Option.value_exn error.diagnostic in
      assert (Host.equal_compilation_stage diagnostic.stage Host.Typecheck);
      assert (
        String.is_substring
          diagnostic.message
          ~substring:"actual selected static host contract")
    | Ok _ -> failwith "Claimed Prepared contract substituted for actual target")
;;
