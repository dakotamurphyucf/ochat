open Core
open Fixtures

let%expect_test "one-off preparation binds source and selected tools without execution" =
  let module P = Chat_response.One_off_script in
  let module C = Chat_response.Tool_capability in
  Eio_main.run (fun env ->
    let calls = ref 0 in
    let registry = native_registry calls ~raises:true in
    let source =
      {|let poison = fail("initializers must not execute during validation")
let main input = Task.bind(Tool.call("read_file", input), fun result ->
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(message) -> Task.pure(`String(message)))|}
    in
    let prepare ?(tools = [ "read_file" ]) source =
      P.prepare_in_domain ~env ~capabilities:registry ~tools ~source ()
    in
    let admitted = function
      | Ok prepared -> prepared
      | Error diagnostics ->
        raise_s [%sexp (diagnostics : Chatmd_shell_spec.Diagnostic.t list)]
    in
    let prepared = prepare source |> admitted in
    let repeated = prepare source |> admitted in
    let changed = prepare (source ^ "\n") |> admitted in
    let no_tools = prepare ~tools:[] source |> admitted in
    assert (String.equal (P.source prepared) source);
    assert (
      String.equal
        (P.source_ref prepared).source_sha256
        (Chatmd_shell_spec.Source_ref.digest source));
    assert (String.equal (P.fingerprint prepared) (P.fingerprint repeated));
    assert (not (String.equal (P.fingerprint prepared) (P.fingerprint changed)));
    assert (not (String.equal (P.fingerprint prepared) (P.fingerprint no_tools)));
    P.revalidate prepared ~capabilities:registry
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith;
    let replacement = native_registry calls ~raises:true in
    let stale = P.revalidate prepared ~capabilities:replacement in
    assert (Result.is_error stale);
    P.revalidate no_tools ~capabilities:replacement
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith;
    print_s
      [%sexp
        { selected =
            (List.map
               (C.references (P.capabilities prepared))
               ~f:(fun reference -> reference.name)
             : string list)
        ; empty_selection = (List.length (C.references (P.capabilities no_tools)) : int)
        ; native_calls = (!calls : int)
        ; replaced_binding_rejected = (Result.is_error stale : bool)
        }];
    List.iter
      [ "unknown capability before syntax", [ "missing" ], "let main ="
      ; "duplicate selection", [ "read_file"; "read_file" ], source
      ; "syntax", [], "let main input =\n  Task.pure("
      ; "result type", [], "let main input =\n  Task.pure(1)"
      ; "forbidden model", [], "let main input = Model.call(\"agent\", input)"
      ; "wrong entrypoint", [], "let run ctx input = Task.pure(input)"
      ]
      ~f:(fun (name, tools, source) ->
        match prepare ~tools source with
        | Ok _ -> failwith (name ^ " unexpectedly compiled")
        | Error diagnostics ->
          List.iter diagnostics ~f:(fun diagnostic ->
            let source_ref = Option.value_exn diagnostic.source in
            assert (
              String.equal
                source_ref.source_sha256
                (Chatmd_shell_spec.Source_ref.digest source));
            print_s
              [%sexp
                (name : string)
              , (diagnostic.code : string)
              , (diagnostic.path : string list)
              , (source_ref.start_pos.line : int)
              , (source_ref.start_pos.column : int)])));
  [%expect
    {|
    ((selected (read_file)) (empty_selection 0) (native_calls 0)
     (replaced_binding_rejected true))
    ("unknown capability before syntax" capability.not_selected (tools) 1 0)
    ("duplicate selection" capability.duplicate_selection (tools) 1 0)
    (syntax chatml.parse_error (source) 2 12)
    ("result type" chatml.type_error (source) 1 0)
    ("forbidden model" chatml.type_error (source) 1 17)
    ("wrong entrypoint" chatml.type_error (source) 1 0)
    |}]
;;
