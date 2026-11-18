open! Core
module P = Agent_protocol
module Package = Request_package_support
module Sources = Capability_sources

let library_namespace = "ochat.capabilities.goal-requests"
let namespace = "ochat.examples.goal-requests-test"

let source_ok result =
  Result.map_error result ~f:Sources.Error.message |> Result.ok_or_failwith
;;

let library =
  let inventory =
    Sources.installed ~limits:Sources.Limits.default ~checkpoint:ignore |> source_ok
  in
  Sources.find inventory (Sources.Id.of_string "goal-requests" |> source_ok) |> source_ok
;;

let installed_member relative =
  let path =
    Source.Portable_path.create ("examples/libraries/goal-requests/" ^ relative)
    |> Or_error.ok_exn
  in
  Sources.Resource.member library path |> source_ok |> Sources.Member.contents
;;

let library_source = installed_member "chatml.project.json"

let members =
  List.map [ "src/goal_requests.chatml"; "src/goal_requests.chatmli" ] ~f:(fun path ->
    path, installed_member path)
;;

let compile ~namespace ~project ~entry_path ~target entry =
  Package.compile
    ~library_namespace
    ~library_source
    ~members
    ~namespace
    ~project
    ~entry_path
    ~target
    entry
;;

let compile_fixture =
  compile
    ~namespace
    ~project:(Package.test_project ~namespace ~module_name:"Goal_requests")
    ~entry_path:"main.chatml"
    ~target:One_off_v1
;;

let same_json expected actual =
  if not (Document_schema.Json.equal expected actual)
  then
    failwithf
      "expected %s, got %s"
      (Jsonaf.to_string expected)
      (Jsonaf.to_string actual)
      ()
;;

let roundtrip decode encode actual =
  let typed =
    decode actual
    |> Result.map_error ~f:(fun (error : P.Error.t) -> error.message)
    |> Result.ok_or_failwith
  in
  same_json (encode typed) actual
;;

let expected_wires =
  let string text = `String text in
  let generation = "9007199254740991" in
  let key = string "case:/1" in
  let goal_id = "goal_id", string "goal_fixture" in
  let create =
    [ "generation", string generation; goal_id; "expected_catalog_revision", string "6" ]
  in
  let source =
    ( "source"
    , `Object
        [ "script_id", string "fixture"
        ; "source_sha256", string (String.make 64 'a')
        ; "generation", `Number generation
        ; "installation_epoch", string "1"
        ] )
  in
  let goal = ("expected_goal_revision", string "2") :: create in
  let budget =
    `Object
      [ "admitted_turns", string "3"
      ; "admitted_attempts", `Null
      ; ( "observed_tokens"
        , `Object [ "component", string "reasoning_output"; "ceiling", string "300" ] )
      ; "deadline", `Null
      ]
  in
  let start input =
    `Object
      (("key", key)
       :: ("kind", string "goal_start_episode_v1")
       :: ("input", input)
       :: source
       :: goal)
  in
  let edit kind value =
    `Object
      (("key", key) :: ("kind", string kind) :: ("value", string value) :: source :: goal)
  in
  let control kind reason =
    `Object (("key", key) :: ("kind", string kind) :: ("reason", string reason) :: goal)
  in
  [ `Object [ "goal_id", `Null ]
  ; `Object [ goal_id ]
  ; `Object
      (("key", key)
       :: ("objective", string "Review")
       :: ("budget", budget)
       :: source
       :: create)
  ; start
      (`Object
          [ "kind", string "user_submission"
          ; ( "content"
            , `Object
                [ "kind", string "chatmd"
                ; "text", string "<user>Proceed.</user>"
                ; "attachments", `Array []
                ] )
          ])
  ; start (`Object [ "kind", string "authored_start" ])
  ; edit "objective" "Review again"
  ; edit "progress" ""
  ; control "hold" "approval"
  ; control "abandon" "superseded"
  ; `Object
      ([ "kind", string "goal_resume_episode_v1"
       ; "key", string "resume"
       ; "session_id", string "ses_fixture"
       ; "run_id", string "run_fixture"
       ; "expected_run_revision", string "9223372036854775807"
       ; source
       ]
       @ goal)
  ]
;;

let%expect_test "checked Goal package emits exact selected request data" =
  let program = compile_fixture [%blob "goal_request_wire.chatml"] |> Package.ok in
  let values = Support.run program `Null |> Jsonaf.list_exn in
  let wires, rejected = List.split_n values (List.length expected_wires) in
  List.iter2_exn expected_wires wires ~f:same_json;
  (match wires with
   | [ _list
     ; _get
     ; create
     ; submit
     ; authored_start
     ; objective
     ; progress
     ; hold
     ; abandon
     ; resume
     ] ->
     roundtrip
       P.Goal_native_create_request.of_json
       P.Goal_native_create_request.to_json
       create;
     List.iter
       [ submit; authored_start ]
       ~f:
         (roundtrip
            P.Goal_native_start_request.of_json
            P.Goal_native_start_request.to_json);
     List.iter
       [ objective; progress ]
       ~f:(roundtrip P.Goal_edit.of_json P.Goal_edit.to_json);
     List.iter
       [ hold; abandon ]
       ~f:
         (roundtrip
            P.Goal_native_control_request.of_json
            P.Goal_native_control_request.to_json);
     roundtrip P.Goal_resume_request.of_json P.Goal_resume_request.to_json resume
   | _ -> failwith "missing Goal request");
  same_json (`Array [ `Array (List.init 9 ~f:(fun _ -> `True)); `True ]) (`Array rejected);
  print_endline
    "exact wires, production mutation codecs, invalid inputs and protocol-only Resume";
  [%expect
    {| exact wires, production mutation codecs, invalid inputs and protocol-only Resume |}]
;;

let%expect_test "checked Goal package enforces the imported call arity" =
  (match compile_fixture {|let main input = Task.pure(Requests.start(input))|} with
   | Ok _ -> failwith "compiled a Start call with missing arguments"
   | Error failure ->
     let module F = Chatml_linked_program.Failure in
     assert (Chatml_linked_program.Stage.equal Typecheck (F.stage failure));
     assert (String.is_substring (F.diagnostic failure).message ~substring:"arity"));
  print_endline "wrong Start arity rejected during type checking";
  [%expect {| wrong Start arity rejected during type checking |}]
;;

let%expect_test "authored Goal startup example compiles in the moderator contract" =
  compile
    ~namespace:"ochat.examples.goal-requests"
    ~project:[%blob "../../docs-src/examples/learning/goal-requests/chatml.project.json"]
    ~entry_path:"moderator.chatml"
    ~target:Moderator_v1
    [%blob "../../docs-src/examples/learning/goal-requests/moderator.chatml"]
  |> Package.ok
  |> ignore;
  print_endline "authored Get, Create, Get, Start moderator compiled";
  [%expect {| authored Get, Create, Get, Start moderator compiled |}]
;;
