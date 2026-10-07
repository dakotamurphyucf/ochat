open! Core
open Runtime_tests
module C = Inference_client

let identity =
  C.Identity.
    { new_preparation_id = (fun () -> "client-preparation")
    ; new_attempt = (fun _ ~relation:_ -> scope, accounting_id)
    }
;;

let run context ~on_attempt ~on_completion ~on_observation ~on_event =
  Eio_main.run (fun _ ->
    Eio.Switch.run (fun sw ->
      C.run
        context
        ~sw
        ~identity
        ~relation:Root
        ~request:(request target)
        ~before_dispatch:ignore
        ~on_attempt
        ~on_completion
        ~on_observation
        ~on_event))
;;

let%expect_test
    "actual completion is acknowledged after evidence before returning to caller"
  =
  let events = ref [] in
  let record text = events := text :: !events in
  let context =
    context
      (fun ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event:_ ~on_observation:_ ->
         record "dispatch";
         receipt ~scope ~accounting_id ())
  in
  ignore
    (run
       context
       ~on_attempt:(fun _ -> record "ack")
       ~on_event:(fun event ->
         match E.view event with
         | Terminal _ -> record "terminal"
         | _ -> ())
       ~on_observation:(fun _ -> record "usage")
       ~on_completion:(fun completion ->
         assert (
           Transcript.Scope.equal
             (Runtime.Attempt.scope (C.Completion.attempt completion))
             scope);
         match C.Completion.outcome completion with
         | Returned terminal ->
           assert (E.Terminal.equal_outcome (E.Terminal.outcome terminal) Completed);
           record "completion"
         | Interrupted _ -> assert false)
     |> ok
     : Runtime.Receipt.t);
  print_s [%sexp (List.rev !events : string list)];
  [%expect {| (ack dispatch usage terminal completion) |}]
;;

exception Original_observer_failed
exception Cleanup_failed

let%expect_test
    "failed acknowledgement reports actual unsubmitted interruption and keeps original \
     exception"
  =
  let dispatched = ref false in
  let completion_count = ref 0 in
  let context =
    context
      (fun ~sw:_ ~scope ~accounting_id ~note_delivery:_ ~on_event:_ ~on_observation:_ ->
         dispatched := true;
         receipt ~scope ~accounting_id ())
  in
  let original =
    try
      ignore
        (run
           context
           ~on_attempt:(fun _ -> raise Original_observer_failed)
           ~on_event:ignore
           ~on_observation:ignore
           ~on_completion:(fun completion ->
             incr completion_count;
             (match C.Completion.outcome completion with
              | Interrupted
                  { reason = Host_interrupted; delivery = Definitely_not_submitted } -> ()
              | _ -> assert false);
             raise Cleanup_failed)
         : (Runtime.Receipt.t, C.Error.t) Result.t);
      false
    with
    | Original_observer_failed -> true
  in
  printf
    "original=%b dispatched=%b completion_count=%d\n"
    original
    !dispatched
    !completion_count;
  [%expect {| original=true dispatched=false completion_count=1 |}]
;;

let%expect_test "cancellation reports observed delivery without inventing a terminal" =
  let interrupted = ref false in
  let cancelled =
    try
      ignore
        (run
           (context no_effect)
           ~on_attempt:ignore
           ~on_event:ignore
           ~on_observation:(fun _ -> raise (Eio.Cancel.Cancelled (Failure "cancelled")))
           ~on_completion:(fun completion ->
             match C.Completion.outcome completion with
             | Interrupted { reason = Cancelled; delivery = Response_started } ->
               interrupted := true
             | _ -> assert false)
         : (Runtime.Receipt.t, C.Error.t) Result.t);
      false
    with
    | Eio.Cancel.Cancelled _ -> true
  in
  printf "cancelled=%b interrupted=%b\n" cancelled !interrupted;
  [%expect {| cancelled=true interrupted=true |}]
;;

let%expect_test "terminal acknowledgement failure is not reclassified or delivered twice" =
  let count = ref 0 in
  let raised =
    try
      ignore
        (run
           (context no_effect)
           ~on_attempt:ignore
           ~on_event:ignore
           ~on_observation:ignore
           ~on_completion:(fun completion ->
             incr count;
             (match C.Completion.outcome completion with
              | Returned _ -> ()
              | Interrupted _ -> assert false);
             raise Original_observer_failed)
         : (Runtime.Receipt.t, C.Error.t) Result.t);
      false
    with
    | Original_observer_failed -> true
  in
  printf "raised=%b completion_count=%d\n" raised !count;
  [%expect {| raised=true completion_count=1 |}]
;;
