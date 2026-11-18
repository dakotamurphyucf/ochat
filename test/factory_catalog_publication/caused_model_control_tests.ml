open! Core
open Agent_server_test_support
open Retry_support
module S = Caused_async_support
module Model = S.Model
module Ledger = S.Ledger
module PC = S.PC
module Cause = S.Cause
module Book = S.Book
module Graph = S.Graph
module Finish = Caused_model_finish_tests
module Initial = Caused_initial_drain_tests
module Read = Initial.Read
module Goal = Goal_service_tests
module G = P.Goal
module O = Inference.Observation

type cut =
  | Inline_before_h
  | After_h_before_confirmation

let assert_rejection cut error =
  [%test_eq: P.Error.code] Conflict error.P.Error.code;
  [%test_eq: bool] false error.retryable;
  [%test_eq: string]
    (match cut with
     | Inline_before_h -> "child accounting Actor/controller/State observation changed"
     | After_h_before_confirmation ->
       "causal factual Actor/controller/State observation changed")
    error.message
;;

let actual_stream subject cause =
  assert (
    P.Session_ref.equal
      (Cause.caller cause)
      (G.session (Goal.goal (state subject.S.child))));
  assert (
    P.Session_ref.equal
      (Cause.callee cause)
      (G.session (Goal.goal (state subject.parent))));
  let target =
    Cause.targets cause
    |> List.filter ~f:(fun target ->
      P.Session_ref.equal
        (Cause.Goal_target.owner target)
        (G.session (Goal.goal (state subject.child))))
    |> Model.only "genuine caller-local Goal target"
  in
  PC.Stream.for_target cause ~target |> protocol_ok
;;

(* A first reservation has a genuine Pending and no confirmed projection yet.
   Observe that public carrier directly; the post-finish oracle below still
   requires the actual imported projection through Initial.slot. *)
let pending_slot current stream =
  let payload =
    Option.value_exn current.A.Session_state.goal_policy_contributions
    |> Book.to_document
    |> protocol_ok
    |> Document_schema.Document.payload
  in
  let book =
    Jsonaf.member_exn "books" payload
    |> Jsonaf.list_exn
    |> List.filter ~f:(fun book ->
      let id = Jsonaf.member_exn "goal_id" book |> P.Id.Goal.of_json |> protocol_ok in
      P.Id.Goal.equal id Goal.goal_id)
    |> Model.only "actual control-cut Goal book"
  in
  let pending =
    Jsonaf.member_exn "slots" book
    |> Jsonaf.list_exn
    |> List.filter ~f:(fun slot ->
      let actual = Jsonaf.member_exn "stream" slot |> PC.Stream.of_json |> protocol_ok in
      PC.Stream.equal stream actual)
    |> Model.only "actual control-cut remote stream"
    |> Jsonaf.member_exn "pending"
    |> Book.Pending.of_json
    |> protocol_ok
  in
  assert (
    List.equal
      A.Goal_descendant_occurrence.equal
      (Book.Subtotal.pending (Model.subtotal current))
      [ Book.Pending.occurrence pending ]);
  pending
;;

let assert_pending subject job cause stream pending =
  [%test_eq: string] (PC.Stream.id stream) (Book.Pending.stream_id pending);
  assert (
    P.Session_ref.equal
      (Book.Pending.owner pending)
      (G.session (Goal.goal (state subject.S.child))));
  [%test_eq: P.Id.Goal.t] Goal.goal_id (Book.Pending.goal_id pending);
  assert (Cause.Key.equal (Cause.key cause) (Book.Pending.cause pending));
  (match Book.Pending.work pending with
   | Model_job_attempt { id; attempt } ->
     [%test_eq: P.Id.Job.t] job.P.Job.id id;
     [%test_eq: int] job.attempt attempt
   | Event _ | Invocation _ | Async_job_attempt _ ->
     failwith "pending lost actual Model Job work");
  let occurrence = Book.Pending.occurrence pending in
  assert (
    P.Session_ref.equal
      (A.Goal_descendant_occurrence.Frame.session
         (A.Goal_descendant_occurrence.frame occurrence))
      (G.session (Goal.goal (state subject.parent))));
  match A.Goal_descendant_occurrence.kind occurrence with
  | Attempt { ordinal; _ } -> ordinal
  | Host_turn _ -> failwith "Model pending became a Host Turn"
;;

let exercise cut =
  let read = Read.create () in
  S.with_claimed_async
    ~wrap_env:(Read.wrap read)
    ~at_direct:(fun _ -> failwith "controlled Model dispatched a provider")
    ~scenario:S.Model_finish_cleanup
    ~parent_prompt_text:Finish.parent_prompt
    ~sources:
      [ S.original_file, "actual control-cut child read"
      ; Finish.effect_file, "actual control-cut native finish"
      ; Finish.recipe, "<developer>Actual selected control-cut source.</developer>"
      ]
    ~model_marker:Finish.marker
    ~at_model:(fun _ _ -> failwith "control cut unexpectedly scheduled a Model recipe")
    (fun env sw daemon fixture ->
       let subject = fixture.probe in
       let within phase f = Model.within env phase f in
       let born =
         within "actual control Model birth" (fun () -> Finish.model_job fixture)
       in
       Caused_async_retry_tests.direct_parent subject born;
       let job =
         within "actual control Model claim" (fun () ->
           Actor.claim_job
             subject.parent.actor
             ~job_id:born.id
             ~generation:born.generation
           |> protocol_ok
           |> Option.value_exn)
       in
       let inference =
         Factory.model_job_inference (Daemon.factory daemon) subject.parent job
         |> protocol_ok
       in
       let target = Inference_runtime.Context.target inference.context in
       inference.capture_recipe_target target |> protocol_ok;
       Finish.assert_target subject job target;
       let before_source = state subject.parent in
       let before_remote = state subject.child in
       (* The creator Goal Run ended before this policy Job: its own callee Goal
          is an inline causal overlay, rather than an ordinary native charge. *)
       assert (
         Option.is_none
           (A.Goal_work_attribution.run_of_job before_source job |> protocol_ok));
       assert (Option.equal P.Inference_origin.equal (Some Unbound) job.inference_origin);
       let member = PC.Source.Native_prefix.member_of_job job |> protocol_ok in
       let cause = S.source_cause before_source member in
       let stream = actual_stream subject cause in
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
       let graph =
         Graph.create
           subject.parent.actor
           ~source:
             (Transcript.Source_id.of_string "597-real-control-cut"
              |> Result.ok_or_failwith)
           ~upstream:
             { new_preparation_id = (fun () -> "597-real-control-preparation")
             ; on_admitted =
                 (fun ~scope:_ ~accounting_id:_ ->
                   failwith "control cut returned an executable allocation")
             ; on_attempt = ignore
             ; on_observation = ignore
             ; on_completion = ignore
             }
         |> protocol_ok
       in
       let owner = ref None in
       let selected = ref None in
       let selections = ref 0 in
       read.path
       <- Some
            (Filename.concat
               (Agent_store.Session_store.Handle.directory
                  (Option.value_exn subject.child.store_handle))
               "ARCHIVED");
       protect_cleanup
         ~finally:(fun () ->
           Read.clear read;
           within "join actual controlled Graph" (fun () ->
             Graph.seal graph |> protocol_ok;
             Graph.finish graph |> protocol_ok))
         (fun () ->
            Actor.with_job_execution
              subject.parent.actor
              ~job_id:job.id
              ~generation:job.generation
              ~attempt:job.attempt
              ~deadline:None
              (fun execution ->
                 let allocate ~graph ~relation ~configuration =
                   assert (Option.is_none !owner);
                   owner := Some graph;
                   execution.allocate_inference ~graph ~relation ~configuration
                 in
                 Read.arm read (fun () ->
                   let source = state subject.parent in
                   let remote = state subject.child in
                   if not (List.is_empty (Book.Subtotal.pending (Model.subtotal remote)))
                   then (
                     let pending = pending_slot remote stream in
                     let ordinal = assert_pending subject job cause stream pending in
                     let rows = Model.new_rows before_source source in
                     let is_cut =
                       match cut, rows with
                       | Inline_before_h, [] -> true
                       | After_h_before_confirmation, [ row ] ->
                         [%test_eq: int64]
                           ordinal
                           (Ledger.Handle.ordinal (Ledger.Row.handle row));
                         let accounting =
                           Option.value_exn source.policy_canonical_usage
                         in
                         let binding =
                           PC.bindings accounting
                           |> List.filter ~f:(fun binding ->
                             Int64.equal ordinal (PC.Binding.ordinal binding))
                           |> Model.only "same actual canonical Model binding"
                         in
                         assert (Cause.equal cause (Model.cause_for accounting binding));
                         let inline =
                           PC.Binding.choices binding
                           |> List.filter ~f:(fun choice ->
                             P.Session_ref.equal
                               (Cause.Goal_target.owner (PC.Choice.target choice))
                               (G.session (Goal.goal source)))
                           |> Model.only "actual inline callee Goal choice"
                         in
                         (match PC.Choice.coverage inline with
                          | Causal_overlay -> ()
                          | Covered_local | Covered_physical ->
                            failwith "control fixture bypassed the inline overlay");
                         true
                       | Inline_before_h, _ :: _ ->
                         failwith "inline control missed pre-H reservation"
                       | After_h_before_confirmation, [] -> false
                       | After_h_before_confirmation, _ :: _ ->
                         failwith "control cut allocated multiple canonical Handles"
                     in
                     if is_cut
                     then (
                       Read.clear read;
                       Int.incr selections;
                       selected := Some (pending, source, remote);
                       let entry =
                         match cut with
                         | Inline_before_h -> subject.parent
                         | After_h_before_confirmation -> subject.child
                       in
                       let before = state entry in
                       let other =
                         match cut with
                         | Inline_before_h -> subject.child
                         | After_h_before_confirmation -> subject.parent
                       in
                       let other_goal = Goal.goal (state other) in
                       Goal_admitted_guard_tests.command
                         daemon
                         entry
                         ~key:"597-real-control-hold"
                         (Hold (Goal.goal_id, Operator_pause))
                       |> protocol_ok
                       |> ignore;
                       let after = state entry in
                       [%test_eq: int64]
                         Int64.(G.revision (Goal.goal before) + 1L)
                         (G.revision (Goal.goal after));
                       assert (
                         G.State.equal
                           (Open (Held Operator_pause))
                           (G.state (Goal.goal after)));
                       assert (G.equal other_goal (Goal.goal (state other))))));
                 let failure =
                   Read.with_caller read (fun () ->
                     within "actual controlled Model denial" (fun () ->
                       match
                         Inference_client.run
                           inference.context
                           ~sw
                           ~identity:(Graph.bind_identity graph ~allocate)
                           ~relation:Root
                           ~request
                           ~before_dispatch:ignore
                           ~on_attempt:(Graph.on_attempt graph)
                           ~on_observation:(Graph.on_observation graph)
                           ~on_completion:(Graph.on_completion graph)
                           ~on_event:ignore
                       with
                       | exception Graph.Rejected error -> error
                       | exception A.Inference_allocation.Rejected error -> error
                       | Error error -> raise_s [%sexp (error : Inference_client.Error.t)]
                       | Ok _ -> failwith "controlled Model unexpectedly completed"))
                 in
                 S.Failure.raise_if_present read.failure;
                 S.Failure.raise_if_present fixture.failure;
                 assert_rejection cut failure;
                 [%test_eq: int] 1 !selections;
                 [%test_eq: int] 0 !(fixture.direct_providers);
                 let pending, acknowledged, _ = Option.value_exn !selected in
                 let owner = Option.value_exn !owner in
                 within "settle same controlled factual Graph" (fun () ->
                   Graph.seal graph |> protocol_ok;
                   Graph.finish graph |> protocol_ok);
                 Finish.assert_finished owner;
                 let source = state subject.parent in
                 let remote = state subject.child in
                 let rows = Model.new_rows before_source source in
                 let expected =
                   match cut with
                   | Inline_before_h ->
                     [%test_eq: int] 0 (List.length rows);
                     assert (
                       Jsonaf.exactly_equal
                         (Finish.ledger_json before_source)
                         (Finish.ledger_json source));
                     0L
                   | After_h_before_confirmation ->
                     let row = Model.only "retained actual denied Model H" rows in
                     let acknowledged_row =
                       Model.new_rows before_source acknowledged
                       |> Model.only "same actual H at the last-wait barrier"
                     in
                     assert (
                       Ledger.Handle.equal
                         (Ledger.Row.handle acknowledged_row)
                         (Ledger.Row.handle row));
                     (match O.Attempt_record.state (Ledger.Row.record row) with
                      | Interrupted { reason = Host_interrupted; _ } -> ()
                      | Prepared | Running | Terminal _ | Interrupted _ ->
                        failwith "denied actual H lost native interruption");
                     let current = Initial.slot remote stream in
                     [%test_eq: int64]
                       (Book.Pending.next_prefix pending)
                       (PC.Projection.admitted_attempts current.projection);
                     let head =
                       PC.Projection.head current.projection |> Option.value_exn
                     in
                     (match PC.Head.work head with
                      | Model_job_attempt { id; attempt } ->
                        [%test_eq: P.Id.Job.t] job.id id;
                        [%test_eq: int] job.attempt attempt
                      | Event _ | Invocation _ | Async_job_attempt _ ->
                        failwith "repair changed actual Model work");
                     1L
                 in
                 [%test_eq: int64]
                   Int64.(Model.committed before_source + expected)
                   (Model.committed source);
                 [%test_eq: int64]
                   Int64.(Model.committed before_remote + expected)
                   (Model.committed remote);
                 assert (List.is_empty (Book.Subtotal.pending (Model.subtotal remote)));
                 let stable_source = Finish.ledger_json source in
                 let stable_canonical =
                   Option.value_exn source.policy_canonical_usage
                   |> PC.to_document
                   |> protocol_ok
                   |> Document_schema.Document.json
                 in
                 let stable_book = Finish.book_json remote in
                 within "repeat factual finish without provider" (fun () ->
                   Graph.finish graph |> protocol_ok);
                 assert (
                   Jsonaf.exactly_equal
                     stable_source
                     (Finish.ledger_json (state subject.parent)));
                 assert (
                   Jsonaf.exactly_equal
                     stable_book
                     (Finish.book_json (state subject.child)));
                 let canonical =
                   Option.value_exn (state subject.parent).policy_canonical_usage
                   |> PC.to_document
                   |> protocol_ok
                   |> Document_schema.Document.json
                 in
                 assert (Jsonaf.exactly_equal stable_canonical canonical);
                 [%test_eq: int] 0 !(fixture.direct_providers);
                 Ok failure)
            |> protocol_ok
            |> fun failure ->
            let terminal =
              Actor.complete_job
                subject.parent.actor
                ~job_id:job.id
                ~generation:job.generation
                ~attempt:job.attempt
                (A.Runtime_builder.Model_failed failure.P.Error.message)
              |> protocol_ok
            in
            (match terminal.status with
             | Failed _ -> ()
             | _ -> failwith "denied actual Model Job did not retain failure");
            Finish.native_terminal (state subject.parent) terminal);
       S.execute_async daemon fixture ~check_completion:(function
         | Succeeded (`String text) ->
           assert (String.is_substring text ~substring:"actual control-cut native finish")
         | completion -> raise_s [%sexp (completion : P.Completion.t)]);
       fixture.release_child ();
       within "join actual child after control denial" (fun () ->
         let rec await () =
           S.Failure.raise_if_present fixture.failure;
           if Option.is_some (Model.checkpoint subject.child).active_operation
           then (
             Eio.Fiber.yield ();
             await ())
         in
         await ());
       [%test_eq: int] 1 !(fixture.child_providers);
       [%test_eq: int] 0 !(fixture.direct_providers));
  print_endline "actual control cut denied provider; exact canonical facts settled once"
;;

let%expect_test "inline Hold after real remote reservation prevents canonical H" =
  exercise Inline_before_h;
  [%expect {| actual control cut denied provider; exact canonical facts settled once |}]
;;

let%expect_test "explicit Hold after canonical Model ACK denies final provider dispatch" =
  exercise After_h_before_confirmation;
  [%expect {| actual control cut denied provider; exact canonical facts settled once |}]
;;
