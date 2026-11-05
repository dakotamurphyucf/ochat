open! Core
module P = Agent_protocol
module C = Agent_server.Recurrence_execution_configuration
module Fixture = Host_tests

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let config_ok = function
  | Ok value -> value
  | Error diagnostics ->
    raise_s [%sexp (diagnostics : Agent_server.Config.Diagnostic.t list)]
;;

let session n = P.Id.Session.of_string (sprintf "ses_execution_%d" n) |> ok
let principal = P.Id.Principal.of_string "pri_execution_host" |> ok

let grant ?(captured_children = false) controller targets =
  C.Controller.create
    ~session_id:(session controller)
    ~existing_targets:(List.map targets ~f:session)
    ~captured_children
;;

let configuration controllers =
  C.create ~principal_id:principal ~controllers ~expires_at:(Some Fixture.expiry)
;;

let rejects = function
  | Error _ -> ()
  | Ok _ -> failwith "invalid execution configuration was admitted"
;;

let%expect_test "execution configuration is separate, exact and bounded on every decode" =
  Fixture.with_workspace (fun env _ root ->
    let text =
      {|(recurrence_automation ((principal_id pri_planning) (controllers (ses_planning))))
(recurrence_execution ((principal_id pri_execution_host)
 (controllers (((session_id ses_execution_0) (existing_targets (ses_execution_0 ses_execution_1)) (captured_children false))
               ((session_id ses_execution_1) (existing_targets ()) (captured_children true))))
 (expires_at 2050-01-02T00:00:00Z)))|}
    in
    let actual = Fixture.admitted_config env root text |> config_ok in
    let configured = Option.value_exn actual.server.recurrence_execution in
    let expected =
      configuration [ grant 0 [ 0; 1 ] |> ok; grant ~captured_children:true 1 [] |> ok ]
      |> ok
    in
    assert (C.equal configured expected);
    assert (C.equal configured (C.t_of_sexp (C.sexp_of_t configured)));
    let planning_only =
      Fixture.admitted_config env root (Fixture.automation "") |> config_ok
    in
    assert (Option.is_some planning_only.server.recurrence_automation);
    assert (Option.is_none planning_only.server.recurrence_execution);
    assert (
      Option.is_none
        (Fixture.admitted_config env root "" |> config_ok).server.recurrence_execution);
    rejects (grant 0 []);
    rejects (grant 0 [ 1; 1 ]);
    configuration [] |> ok |> C.validate |> ok;
    let single = grant 0 [ 0 ] |> ok in
    rejects (configuration [ single; single ]);
    let targets = List.init 256 ~f:Fn.id in
    grant 0 targets |> ok |> ignore;
    rejects (grant 0 (256 :: targets));
    let controllers =
      List.init 256 ~f:(fun n -> grant ~captured_children:true n [] |> ok)
    in
    configuration controllers |> ok |> C.validate |> ok;
    rejects (configuration ((grant ~captured_children:true 256 [] |> ok) :: controllers));
    let edges = List.init 4 ~f:(fun n -> grant n targets |> ok) in
    configuration edges |> ok |> C.validate |> ok;
    rejects (configuration ((grant 4 [ 0 ] |> ok) :: edges));
    let decoded_rejects sexp =
      assert (Result.is_error (Or_error.try_with (fun () -> C.t_of_sexp sexp)))
    in
    let wire controllers =
      Sexp.List
        [ List [ Atom "principal_id"; Atom "pri_execution_host" ]
        ; List [ Atom "controllers"; List controllers ]
        ; List [ Atom "expires_at"; List [] ]
        ]
    in
    decoded_rejects
      (wire [ C.Controller.sexp_of_t single; C.Controller.sexp_of_t single ]);
    decoded_rejects
      (wire (List.map ((grant 4 [ 0 ] |> ok) :: edges) ~f:C.Controller.sexp_of_t));
    let long_targets =
      List.init 256 ~f:(fun n ->
        P.Id.Session.of_string (sprintf "ses_%04d_%s" n (String.make 87 'x')) |> ok)
    in
    let large_rows =
      List.init 3 ~f:(fun n ->
        C.Controller.create
          ~session_id:(session n)
          ~existing_targets:long_targets
          ~captured_children:false
        |> ok)
    in
    rejects (configuration large_rows);
    decoded_rejects (wire (List.map large_rows ~f:C.Controller.sexp_of_t));
    decoded_rejects
      (Sexp.List
         [ List [ Atom "principal_id"; Atom "ses_wrong_kind" ]
         ; List [ Atom "controllers"; List [ C.Controller.sexp_of_t single ] ]
         ]);
    decoded_rejects
      (wire
         [ List
             [ List [ Atom "session_id"; Atom "ses_execution_0" ]
             ; List [ Atom "existing_targets"; List [ Atom "*" ] ]
             ; List [ Atom "captured_children"; Atom "false" ]
             ]
         ]);
    print_endline
      "planning never enables execution; exact self/distinct and separate captured \
       grants round-trip";
    print_endline
      "empty/duplicate/ill-typed grants and controller/target/aggregate bounds reject \
       before use");
  [%expect
    {|
    planning never enables execution; exact self/distinct and separate captured grants round-trip
    empty/duplicate/ill-typed grants and controller/target/aggregate bounds reject before use
    |}]
;;
