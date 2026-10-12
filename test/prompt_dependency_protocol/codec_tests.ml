open! Core
open Support

let set_field json name value =
  match json with
  | `Object fields ->
    `Object
      ((name, value)
       :: List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
  | _ -> assert false
;;

let remove_field json name =
  match json with
  | `Object fields ->
    `Object (List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
  | _ -> assert false
;;

let%expect_test
    "operator updates are sets; publication reports each actual target outcome"
  =
  let request = request () in
  let result = result () in
  print_endline (Jsonaf.to_string (D.Update_request.to_json request));
  print_endline (Jsonaf.to_string (D.Update_result.to_json result));
  let status =
    D.Status.of_json
      (json {|{"history":{"state":"tracked","generation":8},"publication":"uncertain"}|})
    |> checked
  in
  print_s [%sexp (status : D.Status.t)];
  assert (D.Update_result.matches_request result request);
  let command =
    P.Command.of_method_and_params
      ~method_:"prompt.dependencies.update"
      ~params:request_json
    |> checked
  in
  assert (String.equal (P.Command.method_name command) "prompt.dependencies.update");
  let receipt = P.Command_receipt.Committed (Prompt_dependencies_updated result) in
  let decoded =
    P.Command_receipt.of_json (P.Command_receipt.to_json receipt) |> checked
  in
  (match decoded with
   | Committed (Prompt_dependencies_updated restored) ->
     assert (D.Update_result.equal result restored)
   | _ -> assert false);
  [%expect
    {|
    {"definitions":["prd_a","prd_b"],"expected_generation":7,"idempotency_key":"update:original"}
    {"previous_generation":7,"generation":8,"targets":[{"definition_id":"prd_a","outcome":{"state":"ready","revision":"prv_a"}},{"definition_id":"prd_b","outcome":{"state":"unavailable","reason":"source unavailable"}}]}
    ((history (Tracked 8)) (publication Uncertain)) |}]
;;

let%test_unit "independent wire transforms cannot erase bounds or request intent" =
  let malformed =
    [ remove_field request_json "definitions"
    ; set_field request_json "definitions" `Null
    ; set_field request_json "definitions" (`Array [])
    ; set_field request_json "definitions" (`Array [ `String "prd_a"; `String "prd_a" ])
    ; set_field request_json "definitions" (`Array [ `String "prv_a" ])
    ; remove_field request_json "expected_generation"
    ; set_field request_json "expected_generation" `Null
    ; set_field request_json "expected_generation" (`Number "-1")
    ; set_field request_json "expected_generation" (`Number "1.5")
    ; set_field request_json "expected_generation" (`Number "0x7")
    ; set_field request_json "expected_generation" (`Number "01")
    ; set_field request_json "expected_generation" (`Number (String.make 100000 '9'))
    ; set_field request_json "idempotency_key" (`String "")
    ; set_field request_json "pin" (`String "unrequested")
    ; `Object [ "definitions", `Array []; "definitions", `Array [] ]
    ]
  in
  List.iter malformed ~f:(fun fixture ->
    assert (Result.is_error (D.Update_request.of_json fixture)));
  let ids = List.init 1024 ~f:(fun i -> definition ("prd_" ^ Int.to_string i)) in
  let create definitions =
    D.Update_request.create
      ~definitions
      ~expected_generation:(generation 7)
      ~idempotency_key:(D.Update_request.idempotency_key (request ()))
  in
  assert (Result.is_ok (create ids));
  assert (Result.is_error (create (definition "prd_extra" :: ids)));
  let canonical = D.Update_request.to_json (request ()) in
  assert (
    Jsonaf.exactly_equal
      canonical
      (json
         {|{"definitions":["prd_a","prd_b"],"expected_generation":7,"idempotency_key":"update:original"}|}));
  List.iter
    [ `Null; `Array []; `Object [ "history", `Null ] ]
    ~f:(fun fixture -> assert (Result.is_error (D.Status.of_json fixture)));
  List.iter [ "missing"; "invalid" ] ~f:(fun state ->
    let fixture =
      `Object
        [ "history", `Object [ "state", `String state ]
        ; "publication", `String "available"
        ]
    in
    assert (Result.is_ok (D.Status.of_json fixture));
    assert (
      Result.is_error
        (D.Status.of_json
           (set_field
              fixture
              "history"
              (`Object [ "state", `String state; "generation", `Null ])))));
  assert (Result.is_ok (D.Status_request.of_json (`Object [])));
  assert (Result.is_error (D.Status_request.of_json (`Object [ "unused", `Null ])))
;;

let%test_unit
    "result admission validates independent generation, target and diagnostic constraints"
  =
  let targets = D.Update_result.targets (result ()) in
  let create ~previous ~next targets =
    D.Update_result.create
      ~previous_generation:(generation previous)
      ~generation:(generation next)
      ~targets
  in
  assert (Result.is_error (create ~previous:7 ~next:7 targets));
  assert (Result.is_error (create ~previous:7 ~next:9 targets));
  assert (Result.is_error (create ~previous:Int.max_value ~next:Int.max_value targets));
  assert (Result.is_error (create ~previous:7 ~next:8 []));
  assert (Result.is_error (create ~previous:7 ~next:8 (List.hd_exn targets :: targets)));
  List.iter
    [ ""; String.make 4097 'x'; "nul\000byte"; "\255" ]
    ~f:(fun reason ->
      assert (
        Result.is_error
          (create
             ~previous:7
             ~next:8
             [ D.Update_result.Target.
                 { definition_id = definition "prd_a"; outcome = Unavailable { reason } }
             ])));
  assert (
    Result.is_ok
      (create
         ~previous:7
         ~next:8
         [ D.Update_result.Target.
             { definition_id = definition "prd_a"
             ; outcome = Unavailable { reason = String.make 4096 'x' }
             }
         ]));
  let wrong = create ~previous:6 ~next:7 targets |> checked in
  assert (not (D.Update_result.matches_request wrong (request ())));
  let strict =
    set_field
      result_json
      "targets"
      (`Array
          [ json
              {|{"definition_id":"prd_a","outcome":{"state":"ready","revision":"prv_a","reason":null}}|}
          ])
  in
  assert (Result.is_error (D.Update_result.of_json strict));
  assert (
    Result.is_error (D.Update_result.of_json (set_field result_json "unexpected" `Null)))
;;

let%test_unit "dependency scope is administrative rather than organization CRUD authority"
  =
  let principal scopes =
    P.Principal.create
      ~id:(P.Id.Principal.of_string "pri_scope" |> checked)
      ~authentication_kind:"test"
      ~scopes:(P.Scope.Set.of_list scopes)
      ~attributes:[]
    |> checked
  in
  List.iter
    [ P.Command.Prompt_dependencies_status; Prompt_dependencies_update (request ()) ]
    ~f:(fun command ->
      assert (
        Result.is_error
          (Agent_server.Authorization.authorize
             (principal [ Manage_organization; List_prompts ])
             command));
      assert (
        Result.is_ok
          (Agent_server.Authorization.authorize
             (principal [ Administer_configuration ])
             command)))
;;

let%test_unit "sexp readers preserve constructor admission" =
  let reject decode text =
    match decode (Sexp.of_string text) with
    | _ -> assert false
    | exception Sexplib.Conv.Of_sexp_error _ -> ()
  in
  reject D.Generation.t_of_sexp "-1";
  List.iter
    [ "((definitions (prd_a prd_a)) (expected_generation 7) (idempotency_key \
       update:original))"
    ; "((definitions (prv_a)) (expected_generation 7) (idempotency_key update:original))"
    ]
    ~f:(reject D.Update_request.t_of_sexp);
  List.iter
    [ "((previous_generation 7) (generation 9) (targets (((definition_id prd_a) (outcome \
       (Ready prv_a))))))"
    ; "((previous_generation 7) (generation 8) (targets (((definition_id prv_a) (outcome \
       (Ready prv_a))))))"
    ]
    ~f:(reject D.Update_result.t_of_sexp)
;;
