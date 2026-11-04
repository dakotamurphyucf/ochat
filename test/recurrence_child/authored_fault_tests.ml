open! Core
open Support
module Fixtures = Activation_tests
module Owner = Agent_server.Runtime_owner
module Held = Actor.Held_run
module Res = Openai.Responses

module Ack_cut = struct
  type t =
    | First_lifecycle
    | Final_aggregate
  [@@deriving sexp_of]
end

let rec atoms = function
  | A.Session_delta.Batch changes -> List.concat_map changes ~f:atoms
  | delta -> [ delta ]
;;

let selected_candidate
      (cut : Ack_cut.t)
      ~session
      ~(admission : P.Run_receipt.t)
      (transaction : Agent_store.Transaction.t)
  =
  P.Id.Session.equal session transaction.session_id
  &&
  let delta =
    A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
    |> document_ok
    |> A.Session_delta_document.value
  in
  List.exists (atoms delta) ~f:(fun delta ->
    match cut, delta with
    | First_lifecycle, Lifecycle_changed { desired = Running; observed = Starting } ->
      true
    | Final_aggregate, Run_state_changed index ->
      (match A.Run_state.find index admission.run_id with
       | Some run ->
         P.Id.Session.equal (P.Session_ref.session_id run.session) session
         && P.Id.Principal.equal run.principal_id admission.principal_id
         && P.Run_source.equal run.source admission.source
         && (match run.lifecycle with
             | Terminal (Completed _) -> true
             | Admitted | Active | Waiting _ | Terminal _ -> false)
         && List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal admission)
         && List.exists (A.Run_state.receipts index) ~f:(fun receipt ->
           P.Run_receipt.Kind.equal receipt.kind Terminal
           && P.Id.Run.equal receipt.run_id run.id
           && P.Id.Principal.equal receipt.principal_id run.principal_id
           && P.Run_source.equal receipt.source run.source
           && Int64.equal receipt.run_revision run.revision
           && Int64.equal receipt.session_revision transaction.session_revision)
       | None -> false)
    | _ -> false)
;;

module Fault = struct
  type t =
    { cut : Ack_cut.t
    ; mutable armed : (P.Id.Session.t * P.Run_receipt.t) option
    ; mutable candidate : Agent_store.Transaction.t option
    ; mutable hits : int
    }

  let create cut = { cut; armed = None; candidate = None; hits = 0 }
  let arm t session admission = t.armed <- Some (session, admission)

  let reached t env path =
    match t.armed with
    | None -> ()
    | Some (session, admission) ->
      (* Read through the original env, never reenter the Actor/Store from its
         actual writer. Bytes are visible here but have NOT been synced. *)
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> store_ok
      in
      (match List.last scan.entries with
       | Some entry when Int.equal (Agent_store.Frame.flags entry.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
           |> store_ok
         in
         if selected_candidate t.cut ~session ~admission transaction
         then (
           t.armed <- None;
           t.candidate <- Some transaction;
           t.hits <- t.hits + 1;
           raise (Core_unix.Unix_error (EIO, "536 authored ACK before sync", path)))
       | Some _ | None -> ())
  ;;
end

let state_json current = T.state_document current |> store_ok |> D.Document.json

let assert_unconfirmed_run child held ~intent ~attempt =
  let current = state child in
  let admission = Held.admission held in
  let index = Option.value_exn current.run_state in
  let run = A.Run_state.find index admission.run_id |> Option.value_exn in
  assert (P.Run_source.equal run.source admission.source);
  (match run.lifecycle with
   | Admitted | Active | Waiting _ -> ()
   | Terminal _ -> raise_s [%sexp "unconfirmed terminal was installed", (run : P.Run.t)]);
  let receipts =
    A.Run_state.receipts index
    |> List.filter ~f:(fun receipt -> P.Id.Run.equal receipt.run_id run.id)
  in
  [%test_eq: int]
    1
    (List.count receipts ~f:(fun receipt ->
       P.Run_receipt.Kind.equal receipt.kind Admission));
  assert (List.exists receipts ~f:(P.Run_receipt.equal admission));
  assert (
    not
      (List.exists receipts ~f:(fun receipt ->
         P.Run_receipt.Kind.equal receipt.kind Terminal)));
  let origin = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
  A.Recurrence_run_origin.validate origin ~intent ~attempt |> protocol_ok;
  A.Recurrence_run_origin.validate_run origin ~run ~admission |> protocol_ok;
  assert (Option.is_none current.active_operation);
  assert (
    not
      (List.exists run.owned_work ~f:(fun work ->
         match work.P.Run_work.key with
         | Operation _ -> true
         | Retained _ -> false)));
  current, run
;;

let assert_completed_children child held (current : A.Session_state.t) (run : P.Run.t) =
  [%test_eq: int] 2 (List.length current.jobs);
  List.iter current.jobs ~f:(fun job ->
    assert (P.Job.equal_kind job.kind Model_call);
    [%test_eq: int] 1 job.attempt;
    assert (Option.equal P.Inference_origin.equal job.inference_origin (Some (Run run.id)));
    (match job.status with
     | Succeeded -> ()
     | _ -> raise_s [%sexp (job : P.Job.t)]);
    assert (
      Option.equal
        Jsonaf.exactly_equal
        (Option.bind job.result ~f:(Jsonaf.member "final_text"))
        (Some (`String "520-constructor-model-result")));
    let work = A.Run_job_occurrence.work job |> protocol_ok in
    assert (List.mem run.owned_work work ~equal:P.Run_work.equal);
    match
      List.filter run.terminal_work ~f:(fun proof -> P.Run_work.equal proof.work work)
    with
    | [ proof ] -> assert (P.Run_work.Terminal.equal_outcome proof.outcome Succeeded)
    | proofs -> raise_s [%sexp (proofs : P.Run_work.Terminal.t list)]);
  let startup =
    match
      List.filter current.moderator_executions ~f:(fun receipt ->
        P.Moderator_execution.equal_phase receipt.context.phase Session_start)
    with
    | [ receipt ] -> receipt
    | receipts -> raise_s [%sexp (receipts : P.Moderator_execution.t list)]
  in
  (match startup.status with
   | Completed _ -> ()
   | _ -> raise_s [%sexp (startup : P.Moderator_execution.t)]);
  assert (P.Invocation.equal_observer startup.context.source run.source.observer);
  [%test_eq: int] run.source.generation startup.context.generation;
  [%test_eq: int] 2 (List.length current.invocations);
  List.iter current.invocations ~f:(fun invocation ->
    assert (
      Option.equal
        P.Id.Moderator_execution.equal
        invocation.parent_event
        (Some startup.context.id));
    assert (
      Option.equal
        P.Inference_origin.equal
        invocation.inference_origin
        (Some (Run run.id)));
    match invocation.status with
    | Resolved (Complete result) | Published (Complete result) ->
      let expected =
        match invocation.context.tool_name with
        | "read_file" -> "520 actual allowed native result"
        | "recipe_source" -> "536-native-child-result"
        | name -> raise_s [%sexp "unexpected authored native", (name : string)]
      in
      assert (String.is_substring (Jsonaf.to_string result) ~substring:expected)
    | _ -> raise_s [%sexp (invocation : P.Invocation.t)]);
  Fixtures.assert_nested_authored_call child held
;;

let run cut =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
      Mirage_crypto_rng_unix.use_default ();
      let root = T.temporary_root env in
      protect_cleanup
        ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           let prompt = Filename.concat root "parent.chatmd" in
           let child_prompt = Filename.concat root "authored-child.chatmd" in
           List.iter
             [ ( prompt
               , {|<developer>Actual recurrence controller.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
               )
             ; ( child_prompt
               , Fixtures.Authored_case.prompt Fixtures.Authored_case.V1_children )
             ; ( Filename.concat root "activation-recipe.chatmd"
               , "<developer>520_CONSTRUCTOR_RECIPE. Return the offline fixture \
                  response.</developer>" )
             ; ( Filename.concat root "activation-native.txt"
               , "520 actual allowed native result" )
             ]
             ~f:(fun (path, contents) ->
               Eio.Path.save
                 ~create:(`Exclusive 0o600)
                 Eio.Path.(Eio.Stdenv.fs env / path)
                 contents);
           let counters : Fixtures.counters =
             { constructions = ref 0; providers = ref 0 }
           in
           let policy =
             T.inference_policy
               ~default_model:"fixture-model"
               ~post_stream:(fun ~sw:_ ~inputs ->
                 Int.incr counters.providers;
                 let transcript =
                   `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
                 in
                 Fixtures.completed_response
                   (if String.is_substring transcript ~substring:"536_NATIVE_CHILD"
                    then "536-native-child-result"
                    else "520-constructor-model-result"))
           in
           (* These are ACK faults, not Ask-flow tests. Keep actual default Allow. *)
           let configuration = T.config ~profile:T.permission_profile root root prompt in
           let configured_prompt = List.hd_exn configuration.prompts in
           let configuration =
             { configuration with
               prompts =
                 configuration.prompts
                 @ [ { configured_prompt with
                       id = "cold-activation-target"
                     ; path = child_prompt
                     }
                   ]
             }
           in
           let fault = Fault.create cut in
           let daemon_env =
             Crash_fault_io.wrap
               env
               ~boundary:Before_sync
               ~matches:(fun path ->
                 String.is_substring path ~substring:"/journal/"
                 && String.is_suffix path ~suffix:".log")
               ~reached:(Fault.reached fault env)
           in
           with_daemon
             ~policy
             ~on_runtime_construction:(fun () -> Int.incr counters.constructions)
             daemon_env
             root
             configuration
             ~provider_calls:counters.providers
             (fun sw daemon ->
                let connection = T.connection daemon principal in
                protect_cleanup
                  ~finally:(fun () -> Agent_client.Connection.close connection)
                  (fun () ->
                     T.initialize connection;
                     let created, _ =
                       T.create_session ~start_immediately:true connection
                     in
                     let parent = loaded daemon created.id in
                     start_parent_settled env sw connection parent;
                     let original =
                       Operator_authorization.guarded ~principal ~is_current:(fun () ->
                         true)
                     in
                     let confirmed = ref None in
                     let child =
                       with_planner env sw daemon parent (fun () ->
                         Fixtures.with_cold_held
                           ~catalog_target:true
                           env
                           daemon
                           parent
                           ~root_file:child_prompt
                           ~counters
                           ~original
                           (fun child prepared held checked intent attempt ->
                              let admission = Held.admission held in
                              let before_counts =
                                !(counters.constructions), !(counters.providers)
                              in
                              Fault.arm fault (state child).identity.session_id admission;
                              (match
                                 Owner.start_held_authored_runtime
                                   child.runtime
                                   ~prepared
                                   ~held
                                   ~checked
                               with
                               | Error failure ->
                                 [%test_eq: P.Error.code] Persistence_error failure.code
                               | Ok receipt ->
                                 raise_s
                                   [%sexp
                                     "unconfirmed authored ACK returned Admission"
                                   , (receipt : P.Run_receipt.t)]);
                              [%test_eq: int] 1 fault.hits;
                              let candidate = Option.value_exn fault.candidate in
                              assert (
                                selected_candidate
                                  cut
                                  ~session:(state child).identity.session_id
                                  ~admission
                                  candidate);
                              let current, run =
                                assert_unconfirmed_run child held ~intent ~attempt
                              in
                              [%test_eq: int64]
                                (Int64.succ current.counters.revision)
                                candidate.session_revision;
                              (match cut with
                               | Ack_cut.First_lifecycle ->
                                 assert (
                                   P.Session.equal_desired_state
                                     current.lifecycle.desired
                                     Stopped);
                                 (match current.lifecycle.observed with
                                  | Stopped -> ()
                                  | observed ->
                                    raise_s [%sexp (observed : P.Session.observed_state)]);
                                 [%test_eq: int] (snd before_counts) !(counters.providers);
                                 assert (List.is_empty current.jobs);
                                 assert (List.is_empty current.invocations);
                                 assert (List.is_empty current.moderator_executions);
                                 assert (List.is_empty run.terminal_work)
                               | Final_aggregate ->
                                 assert (
                                   P.Session.equal_desired_state
                                     current.lifecycle.desired
                                     Running);
                                 (match current.lifecycle.observed with
                                  | Idle -> ()
                                  | observed ->
                                    raise_s [%sexp (observed : P.Session.observed_state)]);
                                 [%test_eq: int]
                                   (snd before_counts + 3)
                                   !(counters.providers);
                                 assert_completed_children child held current run);
                              let stable_counts =
                                !(counters.constructions), !(counters.providers)
                              in
                              (match
                                 Owner.start_held_authored_runtime
                                   child.runtime
                                   ~prepared
                                   ~held
                                   ~checked
                               with
                               | Error failure ->
                                 [%test_eq: P.Error.code] Conflict failure.code
                               | Ok _ -> failwith "failed authored ACK replayed startup");
                              [%test_eq: int * int]
                                stable_counts
                                (!(counters.constructions), !(counters.providers));
                              assert (
                                Jsonaf.exactly_equal
                                  (state_json current)
                                  (state_json (state child)));
                              confirmed := Some current;
                              Ok child))
                     in
                     (* Failed sync poisons the writer. Later interruption/observer
                        cleanup cannot ACK; do not claim absent bytes, rollback,
                        terminal interruption or observed native delivery. *)
                     assert (not (Owner.is_loaded child.runtime));
                     assert (
                       Jsonaf.exactly_equal
                         (state_json (Option.value_exn !confirmed))
                         (state_json (state child)));
                     Fixtures.assert_saved_child env child)))))
;;

let%test_unit "held authored first and final ACK failures retain only confirmed truth" =
  List.iter
    Ack_cut.[ First_lifecycle; Final_aggregate ]
    ~f:(fun cut ->
      try run cut with
      | exn ->
        Exn.reraise
          exn
          (Sexp.to_string_hum [%sexp "held authored writer fault", (cut : Ack_cut.t)]))
;;
