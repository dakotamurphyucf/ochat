open! Core
open Agent_server_test_support
open Retry_support
open Goal_native_test_support
module Start_request = P.Goal_native_start_request
module Start_intent = P.Goal_native_start_intent
module Resume_intent = P.Goal_native_resume_intent

let unstarted_input = "unselected native Start must never become input"
let unstarted_goal_id = P.Id.Goal.of_string "goal_native_unstarted" |> protocol_ok

let unstarted_goal current =
  A.Goal_catalog.find
    (Option.value_exn current.A.Session_state.goal_catalog)
    unstarted_goal_id
  |> Option.value_exn
;;

(* Real native metadata Create, on the SAME actual Manager/ordinary callback
   owner. This creates a second genuine Goal rather than forging a Run/episode
   so mutually incompatible Start/Resume preconditions appear to coexist. *)
let create_unstarted_goal entry runtime operation =
  let current = state entry in
  let request =
    P.Goal_native_create_request.create
      ~generation:current.identity.generation
      ~source:(G.source (Goal.goal current))
      ~goal_id:unstarted_goal_id
      ~expected_catalog_revision:
        (A.Goal_catalog.revision (Option.value_exn current.goal_catalog))
      ~objective:(G.Objective.of_string "A genuine unstarted native Goal" |> protocol_ok)
      ~budget:G.Budget.unlimited
      ~key:(P.Idempotency_key.of_string "native-unstarted-create" |> protocol_ok)
    |> protocol_ok
  in
  let result, invocations =
    callback entry runtime ~operation ~mode:"create" ~run:(fun admission ->
      Actor.stage_native_goal_create entry.actor ~admission ~authorize ~request)
  in
  assert (result |> protocol_ok);
  [%test_eq: int] 1 (List.length invocations);
  let goal = unstarted_goal (state entry) in
  assert (Option.is_none (G.episode goal));
  assert (G.State.equal (Open (Held Source_review)) (G.state goal))
;;

let start_request entry ~key =
  let current = state entry in
  let goal = unstarted_goal current in
  Start_request.create
    ~generation:current.identity.generation
    ~source:(G.source goal)
    ~goal_id:unstarted_goal_id
    ~expected_goal_revision:(G.revision goal)
    ~expected_catalog_revision:
      (A.Goal_catalog.revision (Option.value_exn current.goal_catalog))
    ~input:
      (User_submission { kind = Plain_text; text = unstarted_input; attachments = [] })
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let stored_start entry invocation_id =
  let invocation =
    List.find_exn (state entry).invocations ~f:(fun actual ->
      P.Id.Invocation.equal actual.P.Invocation.context.id invocation_id)
  in
  match invocation.native_goal_action with
  | Some (Start intent) -> intent
  | Some (Resume _) | None -> failwith "expected the actual stored native Start"
;;

let no_selected_actions current =
  List.iter current.A.Session_state.invocations ~f:(fun invocation ->
    Option.iter invocation.P.Invocation.native_goal_action ~f:(function
      | Start intent ->
        (match Start_intent.phase intent with
         | Staged | Discarded _ -> ()
         | Accepted _ | Dispatch_claimed _ | Admitted _ | Rejected _ ->
           failwith "unselected Start retained an executable command phase")
      | Resume intent ->
        (match Resume_intent.phase intent with
         | Staged | Discarded _ -> ()
         | Accepted _ | Dispatch_claimed _ | Rejected _ ->
           failwith "unselected Resume retained an executable command phase")))
;;

let%expect_test "speculative Start and selected mixed Start Resume cannot dispatch" =
  List.iter [ "start-catch"; "start-resume" ] ~f:(fun mode ->
    with_callback
      ~v1:true
      (fun
          _env
           _daemon
           entry
           runtime
           counters
           _session
           admission
           _old_live
           _held
           _request
           operation
           ~release_ordinary
           ~resumed_entered
           ~release_resumed:_
           ~reopen:_
         ->
         create_unstarted_goal entry runtime operation;
         let before = state entry in
         let request = start_request entry ~key:mode in
         (* Creating B advanced the catalog; Resume(A) must use the actual new
            frontier, not with_callback's earlier immutable request. *)
         let resume = Staging.exact_request before ~key:"mixed-native-resume" in
         let additional =
           if String.equal mode "start-resume"
           then
             [ ( Admission.Effect.Resume
               , fun admission ->
                   Actor.stage_native_goal_resume
                     entry.actor
                     ~admission
                     ~authorize
                     ~request:resume )
             ]
           else []
         in
         let result, invocations =
           callback entry runtime ~operation ~mode ~additional ~run:(fun admission ->
             Actor.stage_native_goal_start entry.actor ~admission ~authorize ~request)
         in
         if String.equal mode "start-catch"
         then assert (result |> protocol_ok)
         else assert (Result.is_error result);
         [%test_eq: int]
           (if String.equal mode "start-resume" then 2 else 1)
           (List.length invocations);
         List.iter invocations ~f:(fun invocation ->
           match invocation.P.Invocation.status with
           | Resolved (Complete _) | Published (Complete _) -> ()
           | Admitted | Dispatching
           | Resolved (Pending _ | Fail _ | Cancelled _)
           | Published (Pending _ | Fail _ | Cancelled _) ->
             failwith "selection test did not complete both genuine native calls");
         let actions =
           List.map invocations ~f:(fun invocation ->
             List.find_exn (state entry).invocations ~f:(fun actual ->
               P.Id.Invocation.equal
                 actual.P.Invocation.context.id
                 invocation.P.Invocation.context.id)
             |> fun actual -> actual.P.Invocation.native_goal_action)
         in
         (match actions with
          | [ Some (Start _) ] when String.equal mode "start-catch" -> ()
          | [ Some (Start _); Some (Resume _) ] when String.equal mode "start-resume" ->
            ()
          | _ -> failwith "callback did not stage its exact Start/Resume branches");
         let current = state entry in
         no_selected_actions current;
         assert (G.equal (unstarted_goal before) (unstarted_goal current));
         assert (G.equal (Goal.goal before) (Goal.goal current));
         assert (
           A.Goal_accounting.equal (Goal.accounting before) (Goal.accounting current));
         let before_index = Option.value_exn before.run_state in
         let index = Option.value_exn current.run_state in
         assert (
           P.Run.equal
             (A.Run_state.find before_index admission.P.Run_receipt.run_id
              |> Option.value_exn)
             (A.Run_state.find index admission.run_id |> Option.value_exn));
         [%test_eq: int]
           (List.length (A.Run_state.runs before_index))
           (List.length (A.Run_state.runs index));
         let account current =
           A.Goal_accounts.find
             (Option.value_exn current.A.Session_state.goal_accounts)
             unstarted_goal_id
           |> Option.value_exn
           |> A.Goal_accounting_document.resolve
                ~goal:(unstarted_goal current)
                ~ledger:current.inference_ledger
           |> protocol_ok
         in
         assert (A.Goal_accounting.equal (account before) (account current));
         release_ordinary ();
         ignore (await_quiet entry : A.Session_state.t);
         assert (
           Option.is_none (Actor.select_native_goal_action entry.actor |> protocol_ok));
         assert (Option.is_none (Eio.Promise.peek resumed_entered));
         [%test_eq: int] 2 !(counters.providers)));
  print_endline
    "caught Start rollback retains no selection; valid Start(B) plus Resume(A) is \
     rejected atomically without dispatch";
  [%expect
    {| caught Start rollback retains no selection; valid Start(B) plus Resume(A) is rejected atomically without dispatch |}]
;;
