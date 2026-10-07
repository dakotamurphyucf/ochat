open! Core
module R = Inference.Request
module E = Inference.Event
module O = Inference.Observation
module Runtime = Inference_runtime
module P = History_entry.Payload

let ok result =
  Result.ok_or_failwith (Result.map_error result ~f:(fun _ -> "fixture admission"))
;;

let limits = Document_schema.Limits.default

let target =
  R.Target.create
    ~adapter:"synthetic"
    ~profile:"selected"
    ~profile_revision:None
    ~account:None
    ~endpoint:"local"
    ~model:"test"
    ~settings:[]
    ~limits
  |> ok
;;

let request target = R.create ~target ~history:[] ~tools:[] ~assets:[] ~limits |> ok

let scope =
  Transcript.Scope.create
    ~source:(Transcript.Source_id.of_string "host" |> ok)
    ~attempt:(Transcript.Attempt_id.of_string "1" |> ok)
    ~relation:Root
  |> ok
;;

let accounting_id = O.Observation_id.of_string "usage" |> ok

let usage ?(revision = 0L) ?(tokens = 3L) ~scope ~accounting_id () =
  let actual = O.Count.create (Actual tokens) |> ok in
  let unknown = O.Count.create (Unknown Not_reported) |> ok in
  let usage =
    O.Usage.create
      ~counts:
        { input = actual
        ; output = unknown
        ; reported_total = unknown
        ; cached_input = unknown
        ; cache_write_input = unknown
        ; reasoning_output = unknown
        }
      ~inclusions:[]
    |> ok
  in
  O.create
    ~scope
    ~id:accounting_id
    ~revision
    ~payload:(Usage usage)
    ~limits:O.Admission.observation
  |> ok
;;

let candidate ?(text = "exact") id =
  let semantic =
    P.Semantic.create
      (Call
         { kind = Function
         ; name = "inspect"
         ; namespace = Absent
         ; input_bytes = text
         ; async = Absent
         })
      ~metadata:P.Metadata.empty
    |> ok
  in
  let item =
    Transcript.Item.create
      ~scope
      ~id:(Transcript.Item_id.of_string id |> ok)
      ~entry_id:None
      ~header:(Some (Call Function))
      ~call_name:(Some "inspect")
    |> ok
  in
  E.create
    (Candidate_ready
       { item; payload = P.authored semantic; local_execution = Tool_candidate })
    ~limits
  |> ok
;;

let receipt ?(output = []) ?(revision = 0L) ?(tokens = 3L) ~scope ~accounting_id () =
  let terminal =
    E.Terminal.create ~scope ~delivery:Response_started ~outcome:Completed |> ok
  in
  Runtime.Receipt.create
    ~terminal
    ~usage:(usage ~revision ~tokens ~scope ~accounting_id ())
    ~output
    ~output_coverage:Response_output
    ~limits:Runtime.Limits.default
  |> ok
;;

let context ?(prepared_request = Fn.id) run =
  Runtime.Adapter.create
    ~id:"synthetic"
    ~limits:Runtime.Limits.default
    ~bind:(fun _ -> Ok ())
    ~prepare:(fun ~preparation_id input ->
      let request = prepared_request input in
      let configuration =
        O.Configuration.of_target
          (R.target request)
          ~preparation_id
          ~transport:In_process
          ~capabilities:[]
          ~limits:O.Admission.observation
        |> ok
      in
      Runtime.Plan.create ~request ~configuration ~fingerprint:"private fingerprint" ~run)
  |> ok
  |> Runtime.Context.create ~target
  |> ok
;;

let attempt run =
  Runtime.Context.prepare
    (context run)
    ~preparation_id:"opaque preparation"
    (request target)
  |> ok
  |> Runtime.Prepared.start ~scope ~accounting_id
  |> ok
;;

let execute attempt ~on_event ~on_observation =
  Eio_main.run (fun _ ->
    Eio.Switch.run (fun sw -> Runtime.Attempt.run attempt ~sw ~on_event ~on_observation))
;;

let quiet attempt = execute attempt ~on_event:ignore ~on_observation:ignore

let no_effect ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event:_ ~on_observation:_ =
  receipt ~scope ~accounting_id ()
;;

let%expect_test "one attempt, candidate reconciliation and one authoritative final usage" =
  let calls = ref 0 in
  let first = candidate "first"
  and second = candidate "second" in
  let attempt =
    attempt (fun ~sw:_ ~scope ~accounting_id ~note_delivery ~on_event ~on_observation ->
      incr calls;
      note_delivery Response_started;
      on_event first;
      on_event first;
      on_observation (usage ~revision:1L ~tokens:10L ~scope ~accounting_id ());
      receipt ~output:[ first; second ] ~revision:2L ~tokens:4L ~scope ~accounting_id ())
  in
  let events = ref []
  and counts = ref [] in
  let on_event event =
    events
    := (match E.view event with
        | Candidate_ready { item; _ } -> Transcript.Item_id.to_string item.id
        | Terminal _ -> "terminal"
        | Live _ -> "live")
       :: !events
  in
  let on_observation observation = counts := O.revision observation :: !counts in
  ignore (execute attempt ~on_event ~on_observation |> ok);
  print_s
    [%sexp
      (List.rev !events : string list), (List.rev !counts : int64 list), (!calls : int)];
  print_s
    [%sexp
      (quiet attempt
       : ((Runtime.Receipt.t, Runtime.Attempt.run_error) Result.t[@sexp.opaque]))];
  assert (Result.is_error (quiet attempt));
  assert (!calls = 1);
  [%expect
    {|
    ((first second terminal) (1 2) 1)
    <opaque>
    |}]
;;

let%expect_test
    "conflicting or missing final candidates reject before new tool publication"
  =
  List.iter [ "conflict"; "missing" ] ~f:(fun mode ->
    let seen = ref 0 in
    let attempt =
      attempt
        (fun ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event ~on_observation:_ ->
           on_event (candidate "first");
           let output =
             if String.equal mode "conflict"
             then [ candidate "new"; candidate ~text:"changed" "first" ]
             else [ candidate "new" ]
           in
           receipt ~output ~scope ~accounting_id ())
    in
    (try
       ignore (execute attempt ~on_event:(fun _ -> incr seen) ~on_observation:ignore)
     with
     | Runtime.Contract_violation error ->
       print_s [%sexp (error : Runtime.Contract_error.t)]);
    assert (!seen = 1));
  [%expect
    {|
    Conflicting_candidate
    Missing_candidate
    |}]
;;

exception Observer_failure

let%expect_test "observer failure propagates and attempt cannot be resumed or retried" =
  let attempt =
    attempt
      (fun ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event ~on_observation:_ ->
         on_event (candidate "first");
         receipt ~scope ~accounting_id ())
  in
  (try
     ignore
       (execute
          attempt
          ~on_event:(fun _ -> raise Observer_failure)
          ~on_observation:ignore)
   with
   | Observer_failure -> print_endline "original observer failure");
  assert (Result.is_error (quiet attempt));
  print_s [%sexp (Runtime.Attempt.delivery attempt : E.Terminal.delivery)];
  [%expect
    {|
    original observer failure
    Response_started
    |}]
;;

let%expect_test "cancellation propagates without inventing a terminal" =
  let events = ref 0 in
  let attempt =
    attempt
      (fun ~sw ~scope:_ ~accounting_id:_ ~note_delivery:_ ~on_event:_ ~on_observation:_ ->
         Eio.Switch.fail sw Exit;
         Eio.Fiber.yield ();
         failwith "unreachable")
  in
  (try
     ignore (execute attempt ~on_event:(fun _ -> incr events) ~on_observation:ignore)
   with
   | Exit | Eio.Cancel.Cancelled _ -> print_endline "cancelled");
  assert (!events = 0);
  assert (Result.is_error (quiet attempt));
  print_s [%sexp (Runtime.Attempt.delivery attempt : E.Terminal.delivery)];
  [%expect
    {|
    cancelled
    Possibly_submitted
    |}]
;;

let%expect_test "usage revisions replace lower counts but stale final receipt rejects" =
  let attempt =
    attempt
      (fun ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event:_ ~on_observation ->
         on_observation (usage ~revision:2L ~scope ~accounting_id ());
         receipt ~revision:1L ~scope ~accounting_id ())
  in
  (try ignore (quiet attempt) with
   | Runtime.Contract_violation error ->
     print_s [%sexp (error : Runtime.Contract_error.t)]);
  [%expect {| Conflicting_usage |}]
;;

let%expect_test "authentication failure refines conservative delivery without submission" =
  let attempt =
    attempt
      (fun ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event:_ ~on_observation:_ ->
         let terminal =
           E.Terminal.create
             ~scope
             ~delivery:Definitely_not_submitted
             ~outcome:(Failed (Authentication Missing))
           |> ok
         in
         Runtime.Receipt.create
           ~terminal
           ~usage:(usage ~scope ~accounting_id ())
           ~output:[]
           ~output_coverage:Observed_prefix
           ~limits:Runtime.Limits.default
         |> ok)
  in
  ignore (quiet attempt |> ok);
  print_s [%sexp (Runtime.Attempt.delivery attempt : E.Terminal.delivery)];
  [%expect {| Definitely_not_submitted |}]
;;

let%expect_test "selected target and complete prepared request cannot be substituted" =
  let selected = context no_effect in
  let other_target = R.Target.with_model target ~model:"other" ~limits |> ok in
  let wrong =
    Runtime.Context.prepare selected ~preparation_id:"id" (request other_target)
  in
  assert (Result.is_error wrong);
  let tampered = context ~prepared_request:(fun _ -> request other_target) no_effect in
  let wrong = Runtime.Context.prepare tampered ~preparation_id:"id" (request target) in
  assert (Result.is_error wrong);
  let changed_endpoint =
    match R.Target.to_json target with
    | `Object fields ->
      `Object (List.Assoc.add fields ~equal:String.equal "endpoint" (`String "elsewhere"))
    | _ -> assert false
  in
  let changed_endpoint = R.Target.of_json changed_endpoint ~limits |> ok in
  assert (Result.is_error (Runtime.Context.derive selected ~target:changed_endpoint));
  assert (Result.is_ok (Runtime.Context.derive selected ~target:other_target));
  print_endline
    "request substitution and endpoint override rejected; explicit model override \
     admitted";
  [%expect
    {| request substitution and endpoint override rejected; explicit model override admitted |}]
;;
