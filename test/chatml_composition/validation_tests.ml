open Core
open Fixtures
module V = Chat_response.Authoring_validation

let agent =
  {|<tool name="ochat_validate"/>
<tool name="read_file"><read id="reports" path="${workspace}/reports"/></tool>|}
;;

let request source =
  `Object
    [ "version", `Number "1"
    ; "target", `String "one_off_script"
    ; "source", `String source
    ; "tools", `Array [ `String "read_file" ]
    ]
;;

let%expect_test
    "readonly validation uses the daemon native borrow without evaluating candidates"
  =
  let validation_host =
    V.create_host
      ~runtime_identity:"qualified-validation-runtime-1"
      ~targets:[ One_off_script; Standalone_tool; Moderator ]
      ~moderator_surface:Ordinary
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  let good =
    "let poison = fail(\"initializer ran\")\n\
     let main input = Task.bind(Tool.call(\"read_file\", input), fun result -> \
     Task.pure(input))"
  in
  with_daemon
    ~validation_host
    ~sources:[ "agent.chatmd", agent ]
    ~calls:
      [ "good", "ochat_validate", request good
      ; "bad", "ochat_validate", request "let main input = Task.pure(input + 1)"
      ]
    (fun state ->
       [%test_eq: int] 2 (List.length state.invocations);
       [%test_eq: int] 0 (List.length (native_reads state));
       List.iter
         [ "good", true; "bad", false ]
         ~f:(fun (id, expected) ->
           let report =
             match result state id with
             | Complete (`String text) -> Jsonaf.of_string text
             | other -> raise_s [%sexp (other : I.outcome)]
           in
           let valid = Jsonaf.member_exn "valid" report |> Jsonaf.bool_exn in
           [%test_eq: bool] expected valid;
           [%test_eq: string]
             "qualified-validation-runtime-1"
             (Jsonaf.member_exn "runtime_identity" report |> Jsonaf.string_exn);
           assert (
             Option.is_some
               (Jsonaf.member "validation_id" report |> Option.bind ~f:Jsonaf.string));
           print_s [%sexp (id : string), (valid : bool)]));
  with_daemon
    ~sources:[ "agent.chatmd", agent ]
    ~calls:[ "default", "ochat_validate", request good ]
    (fun state ->
       assert (List.is_empty (native_reads state));
       let report =
         match result state "default" with
         | Complete (`String text) -> Jsonaf.of_string text
         | other -> raise_s [%sexp (other : I.outcome)]
       in
       assert (Jsonaf.member_exn "valid" report |> Jsonaf.bool_exn);
       assert (
         String.equal
           (Jsonaf.member_exn "runtime_identity" report |> Jsonaf.string_exn)
           "ochat.extensibility.v1"));
  print_endline "qualified host supplies default readonly validation";
  [%expect
    {|
    (good true)
    (bad false)
    qualified host supplies default readonly validation
    |}]
;;
