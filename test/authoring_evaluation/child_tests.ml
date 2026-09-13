open Core
open Authoring_evaluation
open Runner

let candidate = Authoring_evaluation_fixtures.Solutions.child

let%expect_test
    "generated child authoring retains evidence across follow-ups and reads incrementally"
  =
  Eio_main.run (fun env ->
    Execution_host.with_capabilities
      ~env
      ~declarations:Execution_cases.read_declaration
      ~files:[]
      (fun capabilities ->
         Capability_tests.evaluate
           ~env
           ~capabilities
           ~id:"child-evidence-review"
           ~target:Generated_chatmd
           ~candidate
           ~validate:(Child_cases.validate ~env)
           ~execute:(Child_cases.execute ~env)));
  [%expect
    {|
    (Minimal 1 1)
    (Automatic_retrieval 1 1)
    (Selected_preload 1 1)
    |}]
;;

let%expect_test
    "child oracle rejects follow-ups sent to another ID and reads without their cursor"
  =
  Eio_main.run (fun env ->
    let wrong_id =
      Execution_tests.replace
        candidate
        "send"
        (`Object
            [ "session_id", `String "ses_foreignchild"
            ; "message", `String "$message"
            ; "idempotency_key", `String "$key"
            ])
    in
    let no_cursor =
      Execution_tests.replace
        candidate
        "read"
        (`Object [ "session_id", `String "$session_id" ])
    in
    List.iter
      [ "wrong child", wrong_id; "missing cursor", no_cursor ]
      ~f:(fun (name, candidate) ->
        match Child_cases.execute ~env candidate with
        | Failed (Semantics, message) ->
          let expected =
            match name with
            | "wrong child" -> "denied"
            | _ -> "read repeated old output"
          in
          (match String.is_substring message ~substring:expected with
           | true -> ()
           | false -> raise_s [%sexp (name : string), (message : string)]);
          print_endline (name ^ ": rejected")
        | result -> raise_s [%sexp (name : string), (result : execution)]));
  [%expect
    {|
    wrong child: rejected
    missing cursor: rejected
    |}]
;;

let%expect_test
    "generated child validation rejects uncaptured instructions and new native authority"
  =
  Eio_main.run (fun env ->
    let module V = Chat_response.Authoring_validation in
    let host =
      V.create_host
        ~runtime_identity:"evaluation-child-validation"
        ~targets:[ Generated_chatmd ]
        ~moderator_surface:Ordinary
        ~compilation:Chatml_compilation.default_limits
      |> Result.ok_or_failwith
    in
    Execution_host.with_capabilities
      ~env
      ~declarations:Execution_cases.read_declaration
      ~files:[]
      (fun capabilities ->
         let create = Jsonaf.member_exn "create" candidate in
         let sources root companion =
           `Array
             ([ `Object [ "path", `String "reviewer.chatmd"; "text", `String root ] ]
              @
              match companion with
              | None -> []
              | Some text ->
                [ `Object [ "path", `String "instructions.chatmd"; "text", `String text ]
                ])
         in
         let variants =
           [ ( "missing companion"
             , Semantics
             , sources [%blob "fixtures/reviewer.chatmd"] None )
           ; ( "unused companion"
             , Semantics
             , sources
                 {|<config model="o3" reasoning_effort="high"/><developer>Inline only.</developer><tool type="inherited" name="read_file"/>|}
                 (Some [%blob "fixtures/reviewer-instructions.chatmd"]) )
           ; ( "new reader root"
             , Capability
             , sources
                 ([%blob "fixtures/reviewer.chatmd"]
                  ^ {|<tool name="read_file"><read id="outside" path="/"/></tool>|})
                 (Some [%blob "fixtures/reviewer-instructions.chatmd"]) )
           ]
         in
         List.iter variants ~f:(fun (name, expected, sources) ->
           let changed =
             Execution_tests.replace
               candidate
               "create"
               (Execution_tests.replace create "sources" sources)
           in
           match Child_cases.validate ~env ~host ~capabilities changed with
           | Invalid (kind, _) when equal_failure kind expected ->
             print_endline (name ^ ": rejected")
           | result -> raise_s [%sexp (name : string), (result : validation)])));
  [%expect
    {|
    missing companion: rejected
    unused companion: rejected
    new reader root: rejected
    |}]
;;
