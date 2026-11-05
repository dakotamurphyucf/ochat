open! Core
open Creation_support
module Actor = A.Session_actor
module G = P.Goal
module Catalog = A.Goal_catalog
module Accounting = A.Goal_accounting
module Ledger = A.Inference_ledger
module Owner = Agent_server.Runtime_owner

(* Shared real Create/source/admission/root producer. Neither decoded Goal data
   nor a graph identity supplies permission: the actual worker allocator does. *)
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

let accounting current =
  A.Goal_accounts.find (Option.value_exn current.A.Session_state.goal_accounts) goal_id
  |> Option.value_exn
  |> fun document ->
  A.Goal_accounting_document.resolve
    document
    ~goal:(goal current)
    ~ledger:current.inference_ledger
  |> protocol_ok
;;

let start actor source owner ~budget ~worker =
  Actor.set_operation_worker actor (Some worker) |> protocol_ok;
  command
    actor
    ~key:"publication-create"
    (Create
       { id = goal_id
       ; objective = G.Objective.of_string "Fence one admitted inference" |> protocol_ok
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
      ~expected_catalog_revision:(Catalog.revision (Option.value_exn before.goal_catalog))
    |> protocol_ok
  in
  let preparation =
    match
      Actor.begin_headless_run_preparation actor ~actor:proof ~authorize ~request
      |> protocol_ok
    with
    | Prepare preparation -> preparation
    | Retained _ -> failwith "fresh Goal publication fixture retained a Run"
  in
  let submitted =
    Handoff_tests.protect_cleanup
      ~finally:(fun () -> Actor.end_run_preparation actor preparation |> protocol_ok)
      (fun () ->
         Owner.with_prepared_source owner ~preparation ~source (fun prepared ->
           let binding = Owner.Prepared.binding prepared in
           let activation =
             match
               Actor.admit_held_goal_run actor ~binding ~intent ~session |> protocol_ok
             with
             | Admitted activation -> activation
             | Rejected error | Uncertain error | Activation_unavailable { error; _ } ->
               raise_s [%sexp (error : P.Error.t)]
             | Retained _ -> failwith "actual Goal admission unexpectedly retained"
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
                 ~activation:(Actor.Admitted_activation.Continuation.target continuation)
               |> protocol_ok)
             (fun () ->
                Actor.Admitted_activation.Continuation.refresh continuation |> protocol_ok;
                Actor.with_held_input
                  actor
                  ~continuation
                  ~prepare:Handoff_tests.plain_input
                  (fun ~scope ~input -> Actor.submit_held_run_input actor ~scope ~input)))
         |> protocol_ok)
  in
  submitted
;;

let settled actor operation_id =
  let rec loop () =
    let current = state actor in
    match current.active_operation with
    | Some _ ->
      Eio.Fiber.yield ();
      loop ()
    | None -> current
  in
  let current = loop () in
  let admission =
    G.episode (goal current) |> Option.value_exn |> P.Goal_episode.admission
  in
  let run =
    A.Run_state.find (Option.value_exn current.run_state) admission.run_id
    |> Option.value_exn
  in
  assert (
    List.exists run.terminal_work ~f:(fun terminal ->
      P.Run_work.Key.equal terminal.P.Run_work.Terminal.work.key (Operation operation_id)
      && P.Run_work.Terminal.equal_outcome Succeeded terminal.outcome));
  current
;;

(* One bounded worker joins through its actual terminal ACK. Exceptions retain
   their backtrace and are observed outside worker conversion; no assertion can
   turn into a successful operation. Cancellation reaches the surrounding scope. *)
let with_worker ~budget ~run =
  Own_ack_tests.with_controller (fun fixture actor source owner _intent _attempt ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock fixture.env) 30. (fun () ->
      let completed, completed_u = Eio.Promise.create () in
      let source_closed, source_closed_u = Eio.Promise.create () in
      let release () =
        if Option.is_none (Eio.Promise.peek source_closed)
        then Eio.Promise.resolve source_closed_u ()
      in
      Handoff_tests.protect_cleanup ~finally:release (fun () ->
        let worker =
          A.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let result =
              match
                Eio.Promise.await source_closed;
                run fixture actor ~sw ~input caps
              with
              | () -> Ok ()
              | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
            in
            Eio.Promise.resolve completed_u result;
            match result with
            | Ok () ->
              A.Operation_worker.Completed
                { final_history = input.history
                ; moderator_snapshot = (state actor).moderator
                ; runtime_requests = []
                }
            | Error ((Eio.Cancel.Cancelled _ as exn), backtrace) ->
              Exn.raise_with_original_backtrace exn backtrace
            | Error _ ->
              A.Operation_worker.Failed
                (P.Error.create
                   Internal_error
                   ~message:"Goal allocation test failed"
                   ~retryable:false
                   ()))
        in
        let submitted = start actor source owner ~budget:(budget fixture) ~worker in
        (* Real root ACK and the complete source/preparation CPS precede dispatch. *)
        release ();
        (match Eio.Promise.await completed with
         | Ok () -> ()
         | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
        settled actor (Option.value_exn submitted.operation_id) |> ignore)))
;;
