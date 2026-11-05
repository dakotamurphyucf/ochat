open! Core
open Creation_support
module Actor = A.Session_actor
module G = P.Goal
module Catalog = A.Goal_catalog
module Ledger = A.Inference_ledger
module Runtime = Inference_runtime
module Owner = Agent_server.Runtime_owner

(* Exercise the real Store/controller publication boundary with an ordinary
   Operation allocation, after the held activation has handed off. *)
let goal_id = P.Id.Goal.of_string "goal_publication_fence" |> protocol_ok

let goal current =
  Catalog.find (Option.value_exn current.A.Session_state.goal_catalog) goal_id
  |> Option.value_exn
;;

let command actor ~key mutation =
  let current = state actor in
  let catalog = Option.value current.goal_catalog ~default:Catalog.empty in
  let source =
    if A.Goal_command.Mutation.needs_source mutation
    then Some (A.Goal_catalog_source.capture current |> protocol_ok |> snd)
    else None
  in
  let request =
    A.Goal_command.create
      ~session_id:current.identity.session_id
      ~generation:current.identity.generation
      ~source
      ~expected_catalog_revision:(Catalog.revision catalog)
      ~expected_goal_revision:(Option.map (Catalog.find catalog goal_id) ~f:G.revision)
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
      ~mutation
    |> protocol_ok
  in
  Actor.goal_command actor ~actor:proof ~authorize ~session ~request |> protocol_ok
;;

let configuration () =
  let fixture =
    Inference_fixture.create
      ~namespace:"goal-publication-registration"
      ~default_model:"fixture"
      ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "unexpected provider")
  in
  let inference_ok result =
    Result.map_error result ~f:(fun error ->
      Sexp.to_string_hum (Runtime.Preparation_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let target =
    Inference_fixture.capture_config fixture Chat_response.Config.default |> inference_ok
  in
  let context = Inference_fixture.resolve fixture target |> inference_ok in
  let request =
    Inference.Request.create
      ~target
      ~history:[]
      ~tools:[]
      ~assets:[]
      ~limits:Transcript.Admission.default
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  Runtime.Context.prepare context ~preparation_id:"goal-publication-configuration" request
  |> inference_ok
  |> Runtime.Prepared.configuration
;;

let%expect_test "same-value explicit Goal Hold revokes before publication yields" =
  let refreshed, refreshed_u = Eio.Promise.create () in
  let continue_check, continue_check_u = Eio.Promise.create () in
  let published, published_u = Eio.Promise.create () in
  let finish_publish, finish_publish_u = Eio.Promise.create () in
  let checked, checked_u = Eio.Promise.create () in
  let prefix = ref 0 in
  let expected_receipt = ref None in
  let selected_handle = ref None in
  let callback_state = ref None in
  let release promise resolver =
    if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
  in
  let release_all () =
    release continue_check continue_check_u;
    release finish_publish finish_publish_u
  in
  let on_published (current : A.Session_state.t) =
    match !expected_receipt with
    | None -> ()
    | Some key ->
      let catalog = Option.value_exn current.goal_catalog in
      if
        List.exists (Catalog.receipts catalog) ~f:(fun receipt ->
          P.Idempotency_key.equal receipt.A.Goal_command_receipt.key key)
      then (
        expected_receipt := None;
        callback_state := Some current;
        Eio.Promise.resolve published_u ();
        Eio.Promise.await finish_publish)
  in
  Own_ack_tests.with_controller
    ~published:on_published
    (fun fixture actor source owner _intent _attempt ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 30. (fun () ->
         let joins = ref [] in
         Handoff_tests.protect_cleanup
           ~finally:(fun () ->
             release_all ();
             List.iter !joins ~f:(fun join -> Eio.Cancel.protect join))
           (fun () ->
              let worker =
                A.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
                  let graph =
                    Actor.open_inference_owner
                      actor
                      ~source:
                        (Transcript.Source_id.of_string "goal-publication-operation"
                         |> Result.ok_or_failwith)
                    |> protocol_ok
                  in
                  let allocated = ref None in
                  Handoff_tests.protect_cleanup
                    ~finally:(fun () ->
                      (* The publisher is deliberately still blocked when the result
                       is observed. All factual mailbox cleanup waits until release. *)
                      Eio.Promise.await finish_publish;
                      Option.iter !allocated ~f:(fun allocation ->
                        Actor.release_inference
                          actor
                          ~owner:graph
                          ~handle:(A.Inference_allocation.handle allocation)
                        |> protocol_ok);
                      Actor.seal_inference_owner actor ~owner:graph |> protocol_ok;
                      Actor.finish_inference_owner actor ~owner:graph |> protocol_ok)
                    (fun () ->
                       let configuration = configuration () in
                       let first =
                         caps.allocate_inference ~graph ~relation:Root ~configuration
                         |> protocol_ok
                       in
                       allocated := Some first;
                       selected_handle := Some (A.Inference_allocation.handle first);
                       (* Reach the real automatic budget hold through a second fresh
                       allocation. It charges nothing and retains the first handle. *)
                       (match
                          caps.allocate_inference ~graph ~relation:Root ~configuration
                        with
                        | Error error ->
                          [%test_eq: P.Error.code] Permission_denied error.code
                        | Ok _ -> failwith "attempt budget admitted a second allocation");
                       let extra =
                         Runtime.Submission_guard.create ~refresh:(fun () ->
                           Eio.Promise.resolve refreshed_u ();
                           Eio.Promise.await continue_check;
                           Fn.id)
                       in
                       let guard =
                         Runtime.Submission_guard.compose
                           (A.Inference_allocation.submission_guard first)
                           extra
                       in
                       let result =
                         try
                           Runtime.Submission_guard.check guard;
                           Int.incr prefix;
                           Ok ()
                         with
                         | A.Inference_allocation.Rejected error -> Error error
                       in
                       Eio.Promise.resolve checked_u result);
                  A.Operation_worker.Completed
                    { final_history = input.history
                    ; moderator_snapshot = (state actor).moderator
                    ; runtime_requests = []
                    })
              in
              Actor.set_operation_worker actor (Some worker) |> protocol_ok;
              let budget =
                G.Budget.create
                  ~admitted_turns:None
                  ~admitted_attempts:(Some 1L)
                  ~observed_tokens:None
                  ~deadline:None
                |> protocol_ok
              in
              command
                actor
                ~key:"publication-create"
                (Create
                   { id = goal_id
                   ; objective =
                       G.Objective.of_string "Fence one admitted inference" |> protocol_ok
                   ; budget
                   })
              |> ignore;
              let before = state actor in
              let request =
                A.Run_admission_request.Headless.create
                  ~session_id:before.identity.session_id
                  ~generation:before.identity.generation
                  ~expected_revision:before.counters.revision
                  ~mode:Workflow
                  ~input:
                    (User_submission
                       { kind = Plain_text
                       ; text = "actual Goal control publication"
                       ; attachments = []
                       })
                  ~key:(P.Idempotency_key.of_string "publication-episode" |> protocol_ok)
                |> protocol_ok
              in
              let intent =
                A.Goal_episode_admission.intent
                  ~goal_id
                  ~expected_goal_revision:(G.revision (goal before))
                  ~expected_catalog_revision:
                    (Catalog.revision (Option.value_exn before.goal_catalog))
                |> protocol_ok
              in
              let preparation =
                match
                  Actor.begin_headless_run_preparation
                    actor
                    ~actor:proof
                    ~authorize
                    ~request
                  |> protocol_ok
                with
                | Prepare preparation -> preparation
                | Retained _ -> failwith "fresh Goal publication fixture retained a Run"
              in
              let submitted =
                Handoff_tests.protect_cleanup
                  ~finally:(fun () ->
                    Actor.end_run_preparation actor preparation |> protocol_ok)
                  (fun () ->
                     Owner.with_prepared_source
                       owner
                       ~preparation
                       ~source
                       (fun prepared ->
                          let binding = Owner.Prepared.binding prepared in
                          let activation =
                            match
                              Actor.admit_held_goal_run actor ~binding ~intent ~session
                              |> protocol_ok
                            with
                            | Admitted activation -> activation
                            | Rejected error
                            | Uncertain error
                            | Activation_unavailable { error; _ } ->
                              raise_s [%sexp (error : P.Error.t)]
                            | Retained _ ->
                              failwith "actual Goal admission unexpectedly retained"
                          in
                          let continuation =
                            Actor.begin_held_activation
                              actor
                              ~binding
                              ~held:(Actor.Goal_activation.held activation)
                              ~checked:(Actor.Goal_activation.checked activation)
                            |> protocol_ok
                          in
                          Handoff_tests.protect_cleanup
                            ~finally:(fun () ->
                              Actor.end_held_activation
                                actor
                                ~activation:
                                  (Actor.Admitted_activation.Continuation.target
                                     continuation)
                              |> protocol_ok)
                            (fun () ->
                               Actor.Admitted_activation.Continuation.refresh continuation
                               |> protocol_ok;
                               Actor.with_held_input
                                 actor
                                 ~continuation
                                 ~prepare:Handoff_tests.plain_input
                                 (fun ~scope ~input ->
                                    Actor.submit_held_run_input actor ~scope ~input)))
                     |> protocol_ok)
              in
              let operation_id = Option.value_exn submitted.operation_id in
              Eio.Promise.await refreshed;
              let held = state actor in
              assert (
                G.State.equal
                  (Open (Held (Budget_gate Admitted_attempts)))
                  (G.state (goal held)));
              let key =
                P.Idempotency_key.of_string "publication-explicit-hold" |> protocol_ok
              in
              expected_receipt := Some key;
              let holding =
                Eio.Fiber.fork_promise ~sw:fixture.sw (fun () ->
                  command
                    actor
                    ~key:"publication-explicit-hold"
                    (Hold (goal_id, Budget_gate Admitted_attempts)))
              in
              joins := [ (fun () -> Eio.Promise.await_exn holding |> ignore) ];
              Eio.Promise.await published;
              let acknowledged = Option.value_exn !callback_state in
              assert (G.State.equal (G.state (goal held)) (G.state (goal acknowledged)));
              let handle = Option.value_exn !selected_handle in
              let row =
                Ledger.find
                  acknowledged.inference_ledger
                  ~ordinal:(Ledger.Handle.ordinal handle)
                |> Option.value_exn
              in
              assert (Ledger.Handle.equal handle (Ledger.Row.handle row));
              [%test_eq: P.Id.Operation.t option]
                (Some operation_id)
                (Ledger.Handle.operation_id handle);
              [%test_eq: int] 0 !prefix;
              release continue_check continue_check_u;
              (match Eio.Promise.await checked with
               | Error error ->
                 [%test_eq: P.Error.code] Permission_denied error.code;
                 [%test_eq: bool] false error.retryable
               | Ok () ->
                 failwith
                   "explicit Hold ACK left the admitted final closure live during \
                    publication");
              [%test_eq: int] 0 !prefix;
              release finish_publish finish_publish_u;
              Eio.Promise.await_exn holding |> ignore;
              joins := [];
              let rec settled () =
                let current = state actor in
                match current.active_operation with
                | Some _ ->
                  Eio.Fiber.yield ();
                  settled ()
                | None -> current
              in
              let final = settled () in
              let run =
                A.Run_state.find
                  (Option.value_exn final.run_state)
                  (P.Goal_episode.admission (G.episode (goal final) |> Option.value_exn))
                    .run_id
                |> Option.value_exn
              in
              assert (
                List.exists run.terminal_work ~f:(fun terminal ->
                  P.Run_work.Key.equal
                    terminal.P.Run_work.Terminal.work.key
                    (Operation operation_id)
                  && P.Run_work.Terminal.equal_outcome Succeeded terminal.outcome));
              [%test_eq: int] 0 !prefix)));
  print_endline
    "real explicit Hold ACK fences the same admitted handle before publisher release";
  [%expect
    {| real explicit Hold ACK fences the same admitted handle before publisher release |}]
;;
