open! Core
open Creation_support
module Actor = A.Session_actor
module Worker = A.Operation_worker
module Input = A.Runtime_input_preparation
module Owner = Agent_server.Runtime_owner
module Held = Actor.Held_run
module Activation = Actor.Recurrence_activation
module Intent = A.Recurrence_execution_intent
module Attempt = A.Recurrence_execution_attempt

module Cut = struct
  type t =
    | Lexical_exit
    | Controller_revoked
    | Writer_rejected
  [@@deriving sexp_of]
end

let protect_cleanup ~finally f =
  match f () with
  | value ->
    finally ();
    value
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try finally () with
     | cleanup -> eprintf !"secondary handoff cleanup: %{Exn}\n" cleanup);
    Exn.raise_with_original_backtrace exn backtrace
;;

let plain_input ~scope:_ (execution : Input.Execution.t) =
  let open Result.Let_syntax in
  let%bind () = execution.checkpoint () in
  match execution.request.input with
  | User_submission content ->
    Input.Prepared_input.create
      ~request:execution.request
      ~id:execution.id
      ~entry:
        (A.History_codec.user_text ~id:execution.id content.text
         |> A.History_codec.to_canonical)
  | Authored_start -> Error (P.Error.invalid_request "plain fixture requires user input")
;;

let selected_run (state : A.Session_state.t) admission =
  A.Run_state.find (Option.value_exn state.run_state) admission.P.Run_receipt.run_id
  |> Option.value_exn
;;

let require_root run operation_id =
  assert (
    List.exists run.P.Run.owned_work ~f:(fun work ->
      P.Run_work.Key.equal work.key (Operation operation_id)))
;;

let run (cut : Cut.t) =
  let selected = ref None in
  let armed = ref false in
  let ready = ref false in
  let paused = ref false in
  let controller_live = ref true in
  let prefix = ref 0 in
  let borrowed_current = ref false in
  let submitted = ref None in
  let rejected = ref false in
  let retried = ref false in
  let admission = ref None in
  let output = ref None in
  let gate, gate_u = Eio.Promise.create () in
  let refresh_release, refresh_release_u = Eio.Promise.create () in
  let entered, entered_u = Eio.Promise.create () in
  let worker_release, worker_release_u = Eio.Promise.create () in
  let release promise resolver =
    if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
  in
  let release_all () =
    release refresh_release refresh_release_u;
    release worker_release worker_release_u
  in
  let published (state : A.Session_state.t) =
    if !armed
    then (
      match state.active_operation with
      | Some operation ->
        (match !selected, operation.state with
         | None, Starting -> selected := Some operation.id
         | Some expected, Running when P.Id.Operation.equal expected operation.id ->
           ready := true
         | _ -> ())
      | None -> ())
  in
  let before_observe () =
    (* This is the actual controller Store adapter. Earlier Actor-held reads are
       never suspended: only the worker's refresh after its real Running ACK is
       paused. The external observer can now join all lexical producer cleanup. *)
    if !ready && not !paused
    then (
      paused := true;
      Eio.Promise.resolve gate_u ();
      Eio.Promise.await refresh_release)
  in
  Own_ack_tests.with_controller
    ~published
    ~before_observe
    (fun fixture actor source owner intent attempt ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 30. (fun () ->
         let producer_join = ref None in
         protect_cleanup
           ~finally:(fun () ->
             fixture.reject := false;
             release_all ();
             Option.iter !producer_join ~f:(fun join -> Eio.Cancel.protect join))
           (fun () ->
              let original =
                Operator_authorization.guarded ~principal ~is_current:(fun () -> true)
              in
              let controller =
                Operator_authorization.guarded ~principal ~is_current:(fun () ->
                  !controller_live)
              in
              let worker =
                Worker.create ~run:(fun ~sw:_ ~input capabilities ->
                  (* A synchronous effect prefix: the readiness guard must run
                     before entering this function, rather than at its first I/O. *)
                  Int.incr prefix;
                  let borrowed = capabilities.original_agent_actor () |> protocol_ok in
                  borrowed_current
                  := Option.exists borrowed ~f:(fun proof ->
                       Operator_authorization.is_current proof);
                  Eio.Promise.resolve entered_u ();
                  Eio.Promise.await worker_release;
                  Worker.Completed
                    { final_history = input.history
                    ; runtime_requests = []
                    ; moderator_snapshot = (state actor).moderator
                    })
              in
              Actor.set_operation_worker actor (Some worker) |> protocol_ok;
              let producer =
                Eio.Fiber.fork_promise ~sw:fixture.sw (fun () ->
                  Own_ack_tests.with_prepared
                    actor
                    owner
                    source
                    ~target_proof:original
                    intent
                    attempt
                    (fun preparation prepared ->
                       let binding = Owner.Prepared.binding prepared in
                       Own_ack_tests.record actor binding ~proof:controller intent attempt
                       |> protocol_ok
                       |> ignore;
                       let held =
                         match
                           Held_tests.admit actor binding intent attempt session
                           |> protocol_ok
                         with
                         | Held held -> held
                         | Retained _ ->
                           failwith "self-target admission has no live owner"
                         | Rejected error | Uncertain error ->
                           raise_s [%sexp (error : P.Error.t)]
                       in
                       admission := Some (Held.admission held);
                       Own_ack_tests.import
                         actor
                         held
                         ~proof:controller
                         (Import_tests.observe actor intent attempt)
                       |> protocol_ok
                       |> ignore;
                       let observation = Import_tests.observe actor intent attempt in
                       let permit =
                         Activation_tests.acquire
                           actor
                           ~actor_proof:controller
                           observation
                         |> protocol_ok
                       in
                       protect_cleanup
                         ~finally:(fun () -> Activation_tests.release actor permit)
                         (fun () ->
                            let checked =
                              Activation_tests.refresh actor permit observation
                              |> protocol_ok
                            in
                            let admitted = Activation.Checked.admitted checked in
                            let duplicate = Activation.Checked.admitted checked in
                            Actor.Admitted_activation.Checked.check_basis admitted
                            |> protocol_ok;
                            let continuation =
                              Actor.begin_held_activation
                                actor
                                ~binding
                                ~held
                                ~checked:admitted
                              |> protocol_ok
                            in
                            protect_cleanup
                              ~finally:(fun () ->
                                Actor.end_held_activation
                                  actor
                                  ~activation:
                                    (Activation.Continuation.target continuation)
                                |> protocol_ok)
                              (fun () ->
                                 (* Both public views retain the same consumed
                                    owner. A second projection cannot open a
                                    second activation or poison the first one. *)
                                 assert (
                                   Result.is_error
                                     (Activation.Checked.check_basis checked));
                                 assert (
                                   Result.is_error
                                     (Actor.Admitted_activation.Checked.check_basis
                                        duplicate));
                                 assert (
                                   Result.is_error
                                     (Actor.begin_held_activation
                                        actor
                                        ~binding
                                        ~held
                                        ~checked:duplicate));
                                 [%test_eq: int] 0 !prefix;
                                 Activation.Continuation.refresh continuation
                                 |> protocol_ok;
                                 Actor.with_held_input
                                   actor
                                   ~continuation
                                   ~prepare:plain_input
                                   (fun ~scope ~input ->
                                      output := Some input;
                                      Own_ack_tests.assert_original preparation attempt;
                                      armed := true;
                                      match cut with
                                      | Lexical_exit | Controller_revoked ->
                                        Result.map
                                          (Actor.submit_held_run_input
                                             actor
                                             ~scope
                                             ~input)
                                          ~f:(fun result -> submitted := Some result)
                                      | Writer_rejected ->
                                        let before = state actor in
                                        let frames = Held_tests.frames fixture in
                                        fixture.reject := true;
                                        protect_cleanup
                                          ~finally:(fun () -> fixture.reject := false)
                                          (fun () ->
                                             match
                                               Actor.submit_held_run_input
                                                 actor
                                                 ~scope
                                                 ~input
                                             with
                                             | Error error
                                               when P.Error.equal_code
                                                      error.code
                                                      Persistence_error ->
                                               rejected := true
                                             | Error error ->
                                               raise_s [%sexp (error : P.Error.t)]
                                             | Ok _ ->
                                               failwith "rejected writer ACKed root");
                                        Held_tests.assert_same_state before (state actor);
                                        [%test_eq: string list]
                                          frames
                                          (Held_tests.frames fixture);
                                        (match
                                           Actor.submit_held_run_input actor ~scope ~input
                                         with
                                         | Error error
                                           when P.Error.equal_code error.code Conflict ->
                                           retried := true
                                         | Error error ->
                                           raise_s [%sexp (error : P.Error.t)]
                                         | Ok _ -> failwith "unconfirmed root was retried");
                                        Ok ())
                                 |> protocol_ok))))
              in
              let join () = Eio.Promise.await_exn producer in
              producer_join := Some join;
              (match cut with
               | Writer_rejected ->
                 join ();
                 assert !rejected;
                 assert !retried;
                 assert (not !ready);
                 assert (Option.is_none !submitted);
                 assert (Option.is_none (state actor).active_operation);
                 [%test_eq: int] !prefix 0;
                 assert (Option.is_none !selected)
               | Lexical_exit | Controller_revoked ->
                 let rec await_gate () =
                   match Eio.Promise.peek gate with
                   | Some () -> ()
                   | None ->
                     (match Eio.Promise.peek producer with
                      | Some _ ->
                        join ();
                        if Option.is_none (state actor).active_operation
                        then failwith "root ended before its actual readiness refresh";
                        Eio.Fiber.yield ();
                        await_gate ()
                      | None ->
                        Eio.Fiber.yield ();
                        await_gate ())
                 in
                 await_gate ();
                 assert !ready;
                 [%test_eq: int] !prefix 0;
                 (* Join source lease, input scope, preparation, and the old
                    controller permit BEFORE the real worker refresh resumes. *)
                 join ();
                 [%test_eq: int] !prefix 0;
                 let submission = Option.value_exn !submitted in
                 let operation_id = Option.value_exn submission.operation_id in
                 assert (Option.equal P.Id.Operation.equal !selected (Some operation_id));
                 let receipt = Option.value_exn !admission in
                 let current = state actor in
                 let run = selected_run current receipt in
                 assert (P.Run_source.equal run.source receipt.source);
                 let origin =
                   A.Run_state.recurrence_origin
                     (Option.value_exn current.run_state)
                     run.id
                   |> Option.value_exn
                 in
                 A.Recurrence_run_origin.validate origin ~intent ~attempt |> protocol_ok;
                 A.Recurrence_run_origin.validate_run origin ~run ~admission:receipt
                 |> protocol_ok;
                 require_root run operation_id;
                 (match run.lifecycle with
                  | Admitted | Active | Waiting _ -> ()
                  | Terminal _ -> failwith "lexical exit settled the handed-off Run");
                 let entry = Input.Prepared_input.entry (Option.value_exn !output) in
                 [%test_eq: int]
                   (List.count current.conversation.canonical_history ~f:(fun saved ->
                      P.History.Id.equal saved.id submission.history_id))
                   1;
                 assert (
                   List.exists
                     current.conversation.canonical_history
                     ~f:(P.History.equal_entry entry));
                 (match cut with
                  | Controller_revoked -> controller_live := false
                  | Lexical_exit -> ()
                  | Writer_rejected -> assert false);
                 release refresh_release refresh_release_u;
                 (match cut with
                  | Lexical_exit ->
                    Eio.Promise.await entered;
                    [%test_eq: int] !prefix 1;
                    assert !borrowed_current;
                    release worker_release worker_release_u
                  | Controller_revoked -> ()
                  | Writer_rejected -> assert false);
                 let rec await_terminal () =
                   match (state actor).active_operation with
                   | None -> ()
                   | Some _ ->
                     Eio.Fiber.yield ();
                     await_terminal ()
                 in
                 await_terminal ();
                 let final = state actor in
                 let run = selected_run final receipt in
                 let journal, _, _ = restored_journal fixture in
                 let operations =
                   (Agent_store.Journal.scan journal |> store_ok).entries
                   |> List.concat_map ~f:(fun entry ->
                     if
                       Int.equal
                         (Agent_store.Frame.flags entry.Agent_store.Journal.frame)
                         0
                     then (
                       let transaction =
                         Agent_store.Transaction.decode
                           (Agent_store.Frame.payload entry.frame)
                         |> store_ok
                       in
                       A.Session_persistence.durable_events
                         ~limits:document_limits
                         transaction
                       |> store_ok)
                     else [])
                   |> List.filter_map ~f:(fun event ->
                     match
                       P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                       |> protocol_ok
                     with
                     | Operation_completed operation
                     | Operation_failed operation
                     | Operation_cancelled operation
                     | Operation_interrupted operation
                       when P.Id.Operation.equal operation.id operation_id ->
                       Some operation
                     | _ -> None)
                 in
                 let operation =
                   match operations with
                   | [ operation ] -> operation
                   | _ -> failwith "actual root must have one durable terminal event"
                 in
                 (match cut, operation.state, run.lifecycle with
                  | Lexical_exit, Completed, Terminal (Completed _) ->
                    [%test_eq: int] !prefix 1;
                    print_endline "self-target root survives full lexical release"
                  | Controller_revoked, Failed error, Terminal (Failed _)
                    when P.Error.equal_code error.code Permission_denied ->
                    [%test_eq: int] !prefix 0;
                    print_endline "controller revocation denies the synchronous prefix"
                  | _ ->
                    raise_s
                      [%sexp (cut : Cut.t), (operation : P.Operation.t), (run : P.Run.t)]);
                 let outcome =
                   match cut with
                   | Lexical_exit -> P.Run_work.Terminal.Succeeded
                   | Controller_revoked -> P.Run_work.Terminal.Failed
                   | Writer_rejected -> assert false
                 in
                 assert (
                   List.exists run.terminal_work ~f:(fun terminal ->
                     P.Run_work.Key.equal terminal.work.key (Operation operation_id)
                     && P.Run_work.Terminal.equal_outcome terminal.outcome outcome));
                 let index = Option.value_exn final.run_state in
                 assert (
                   List.exists
                     (A.Run_state.receipts index)
                     ~f:(P.Run_receipt.equal receipt));
                 [%test_eq: int]
                   (List.count (A.Run_state.receipts index) ~f:(fun saved ->
                      P.Id.Run.equal saved.run_id receipt.run_id
                      && P.Run_receipt.Kind.equal saved.kind Admission))
                   1;
                 [%test_eq: int]
                   (List.count (A.Run_state.receipts index) ~f:(fun saved ->
                      P.Id.Run.equal saved.run_id receipt.run_id
                      && P.Run_receipt.Kind.equal saved.kind Terminal))
                   1);
              Held_tests.assert_same_state (state actor) (replayed fixture);
              match cut with
              | Writer_rejected ->
                print_endline "attempted root writer cannot resubmit the same Ready DATA"
              | Lexical_exit | Controller_revoked -> ())))
;;

let%expect_test "held input root transfer fences lexical exit, controller and writer" =
  List.iter [ Cut.Lexical_exit; Controller_revoked; Writer_rejected ] ~f:run;
  [%expect
    {|
    self-target root survives full lexical release
    controller revocation denies the synchronous prefix
    attempted root writer cannot resubmit the same Ready DATA
    |}]
;;
