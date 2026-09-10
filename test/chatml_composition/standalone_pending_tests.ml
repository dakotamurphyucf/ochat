open Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol

let sources =
  [ "agent.chatmd", [%blob "../chatml_extensibility_fixtures/x04-standalone/async.chatmd"]
  ; "async.chatml", [%blob "../chatml_extensibility_fixtures/x04-standalone/async.chatml"]
  ; ( "accepted.json"
    , [%blob "../chatml_extensibility_fixtures/x04-standalone/accepted.json"] )
  ]
  @ List.filter (Standalone_tests.sources ~bad_output:false) ~f:(fun (name, _) ->
    not (String.equal name "agent.chatmd"))
;;

let%expect_test
    "X04 asynchronous standalone bundle retains owned jobs, two reads and fresh state"
  =
  with_daemon
    ~sources
    ~settle:Job_launch_tests.settle
    ~calls:
      [ ( "first"
        , "compare_reports_async"
        , Standalone_tests.input "report-a.json" "report-b.json" )
      ; ( "second"
        , "compare_reports_async"
        , Standalone_tests.input "report-b.json" "report-a.json" )
      ]
    (fun state ->
       [%test_eq: int] 2 (List.length state.jobs);
       List.iter [ "first"; "second" ] ~f:(fun name ->
         let invocation = model_invocation state name in
         let id, acknowledgement =
           match outcome invocation with
           | Pending (Job id, acknowledgement) -> id, acknowledgement
           | other -> raise_s [%sexp (other : I.outcome)]
         in
         [%test_eq: string]
           (P.Id.Job.to_string id)
           (Jsonaf.member_exn "job_id" acknowledgement |> Jsonaf.string_exn);
         [%test_eq: string]
           "accepted"
           (Jsonaf.member_exn "status" acknowledgement |> Jsonaf.string_exn);
         let job = List.find_exn state.jobs ~f:(fun job -> P.Id.Job.equal job.id id) in
         [%test_eq: int] 1 job.attempt;
         assert (
           P.Job.equal_launch_owner
             (Option.value_exn job.launch).owner
             (Invocation invocation.context.id));
         let value =
           match P.Job.terminal_completion job |> protocol_ok with
           | Some (Succeeded value) -> value
           | other -> raise_s [%sexp (other : P.Completion.t option)]
         in
         [%test_eq: float]
           1.
           (Jsonaf.member_exn "invocation_count" value |> Jsonaf.float_exn);
         let left, right =
           match name with
           | "first" -> report_a, report_b
           | _ -> report_b, report_a
         in
         List.iter
           [ "left", left; "right", right ]
           ~f:(fun (field, expected) ->
             assert (
               String.is_substring
                 (Jsonaf.member_exn field value |> Jsonaf.string_exn)
                 ~substring:(String.strip expected))));
       [%test_eq: int] 4 (List.length (native_reads state));
       print_endline
         "two acknowledgements reference their owned jobs; each performs two reads with \
          fresh state";
       print_endline "one session, no moderator, no background model request");
  [%expect
    {|
    two acknowledgements reference their owned jobs; each performs two reads with fresh state
    one session, no moderator, no background model request
    |}]
;;
