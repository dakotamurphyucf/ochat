open! Core
module P = Agent_protocol

module Execution_id = struct
  include P.Id.Moderator_execution
  include Comparator.Make (P.Id.Moderator_execution)
end

type t =
  { run : P.Run.t
  ; delta : Session_delta.t
  }

let run t = t.run
let delta t = t.delta
let payloads _ = []
let conflict message = P.Error.create Conflict ~message ~retryable:false ()

let rebuild
      (previous : P.Run.t)
      ~lifecycle
      ~owned_work
      ~relinquished_work
      ~terminal_work
      ~now
  =
  if Int64.equal previous.revision Int64.max_value
  then Error (conflict "run revision exhausted")
  else
    P.Run.create
      ~id:previous.id
      ~session:previous.session
      ~principal_id:previous.principal_id
      ~source:previous.source
      ~mode:previous.mode
      ~lifecycle
      ~revision:(Int64.succ previous.revision)
      ~owned_work
      ~relinquished_work
      ~terminal_work
      ~created_at:previous.created_at
      ~updated_at:now
;;

let make_receipt (state : Session_state.t) (run : P.Run.t) ~kind ~key ~digest ~now =
  let open Result.Let_syntax in
  let%bind key = P.Idempotency_key.of_string key in
  if Int64.equal state.counters.revision Int64.max_value
  then Error (conflict "session revision exhausted")
  else
    P.Run_receipt.create
      ~run_id:run.id
      ~principal_id:run.principal_id
      ~source:run.source
      ~key
      ~request_sha256:digest
      ~kind
      ~run_revision:run.revision
      ~session_revision:(Int64.succ state.counters.revision)
      ~committed_at:now
;;

let digest json =
  Jsonaf.to_string json |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
;;

let prepare
      (state : Session_state.t)
      ~scope
      ~(executing : P.Moderator_execution.t)
      ~work_state
      ~owned_work
      ~action
      ~now
  =
  let open Result.Let_syntax in
  let%bind () = Session_state.validate work_state in
  let%bind () =
    if
      P.Id.Session.equal
        state.identity.session_id
        work_state.Session_state.identity.session_id
      && Int.equal state.identity.generation work_state.identity.generation
    then Ok ()
    else Error (conflict "staged work belongs to another session or generation")
  in
  let%bind index =
    Result.of_option state.run_state ~error:(conflict "run index absent")
  in
  let%bind previous =
    Result.of_option
      (Run_state.find index (Run_scope.run_id scope))
      ~error:(conflict "run absent")
  in
  let%bind () =
    if
      Run_scope.is_open scope
      && P.Id.Moderator_execution.equal
           executing.context.id
           (Run_scope.execution_id scope)
      && P.Id.Principal.equal previous.principal_id (Run_scope.principal_id scope)
      && P.Run_source.equal previous.source (Run_scope.source scope)
      && Int64.equal previous.revision (Run_scope.revision scope)
      && Int.equal state.identity.generation previous.source.generation
    then Ok ()
    else Error (conflict "run callback scope changed before commit")
  in
  let%bind selected = Run_event_scope.select state ~executing in
  let%bind () =
    if Option.exists selected ~f:(fun run -> P.Id.Run.equal run.id previous.id)
    then Ok ()
    else Error (conflict "execution no longer owns this run")
  in
  let%bind () = Option.value_map action ~default:(Ok ()) ~f:P.Run_action.validate in
  let%bind () =
    Option.value_map action ~default:(Ok ()) ~f:(fun action ->
      Run_state.check_action index ~run_id:previous.id ~action)
  in
  let%bind () =
    P.Run_limits.check_count (List.length previous.owned_work + List.length owned_work)
  in
  let owned = Set.of_list (module P.Run_work) (previous.owned_work @ owned_work) in
  let%bind terminal_work =
    match
      List.find work_state.Session_state.moderator_executions ~f:(fun completed ->
        P.Id.Moderator_execution.equal completed.context.id executing.context.id)
    with
    | None -> Ok previous.terminal_work
    | Some completed -> Run_callback_evidence.capture previous ~executing ~completed
  in
  let evidence =
    Set.of_list
      (module P.Run_work)
      (List.map terminal_work ~f:(fun proof -> proof.P.Run_work.Terminal.work))
  in
  let%bind lifecycle, relinquished, owned =
    match action with
    | None ->
      let lifecycle =
        match previous.lifecycle with
        | Admitted -> P.Run.Lifecycle.Active
        | Active | Waiting _ | Terminal _ -> previous.lifecycle
      in
      Ok (lifecycle, previous.relinquished_work, Set.to_list owned)
    | Some P.Run_action.Continue ->
      Ok (P.Run.Lifecycle.Active, previous.relinquished_work, Set.to_list owned)
    | Some (Wait wake) ->
      let%bind projected =
        P.Run.create
          ~id:previous.id
          ~session:previous.session
          ~principal_id:previous.principal_id
          ~source:previous.source
          ~mode:previous.mode
          ~lifecycle:previous.lifecycle
          ~revision:previous.revision
          ~owned_work:(Set.to_list owned)
          ~relinquished_work:previous.relinquished_work
          ~terminal_work:previous.terminal_work
          ~created_at:previous.created_at
          ~updated_at:previous.updated_at
      in
      let%map () = Run_wake_owner.validate work_state ~run:projected ~wake in
      P.Run.Lifecycle.Waiting wake, previous.relinquished_work, Set.to_list owned
    | Some (Finish { terminal; relinquish }) ->
      let%bind () =
        List.fold_result relinquish ~init:() ~f:(fun () work ->
          if (not (Set.mem owned work)) || Set.mem evidence work
          then Error (conflict "finish cannot relinquish absent or terminal work")
          else (
            match work.P.Run_work.key with
            | Retained (Job { id; attempt = _ }) ->
              if
                List.exists work_state.Session_state.jobs ~f:(fun (job : P.Job.t) ->
                  P.Id.Job.equal job.id id
                  && Run_job_occurrence.matches job ~work
                  &&
                  match job.status with
                  | Queued -> true
                  | Running
                  | Waiting_permission _
                  | Waiting_completion _
                  | Succeeded
                  | Failed _
                  | Cancelled
                  | Interrupted _ -> false)
              then Ok ()
              else
                Error
                  (conflict
                     "finish requires settled work or explicit independent queued-job \
                      custody")
            | Operation _
            | Retained
                ( Schedule _
                | Invocation _
                | Subscription _
                | Delivery _
                | Moderator_execution _ ) ->
              Error (conflict "finish cannot relinquish callback or action obligations")))
      in
      let remaining = Set.diff owned (Set.of_list (module P.Run_work) relinquish) in
      let unsettled = Set.diff remaining evidence in
      let%bind lifecycle =
        if Set.is_empty unsettled
        then Ok (P.Run.Lifecycle.Terminal terminal)
        else (
          match
            executing.context.phase, executing.context.operation_id, Set.to_list unsettled
          with
          | Turn_end, Some operation_id, [ work ] ->
            (match work.P.Run_work.key with
             | Operation id when P.Id.Operation.equal id operation_id ->
               Ok P.Run.Lifecycle.Active
             | Operation _ | Retained _ ->
               Error (conflict "finish has unrelated unfinished owned work"))
          | ( ( Session_start
              | Session_resume
              | Turn_start
              | Message_appended
              | Pre_tool_call
              | Post_tool_response
              | Turn_end
              | Internal_event )
            , _
            , _ ) ->
            Error
              (conflict "finish requires settled owned work before leaving its callback"))
      in
      Ok (lifecycle, previous.relinquished_work @ relinquish, Set.to_list remaining)
  in
  let%bind run =
    rebuild
      previous
      ~lifecycle
      ~owned_work:owned
      ~relinquished_work:relinquished
      ~terminal_work
      ~now
  in
  let%map index =
    match action with
    | None -> Run_state.advance index ~run ~intents:[]
    | Some action ->
      let%bind receipt =
        make_receipt
          state
          run
          ~kind:Action
          ~key:("run-action:" ^ P.Id.Moderator_execution.to_string executing.context.id)
          ~digest:(digest (P.Run_action.to_json action))
          ~now
      in
      let%bind intent =
        Run_intent.create ~receipt ~execution_id:executing.context.id ~action
      in
      let%bind intent =
        match run.lifecycle with
        | Terminal _ -> Run_intent.consume intent ~operation_id:None
        | Admitted | Active | Waiting _ -> Ok intent
      in
      let%bind index = Run_state.commit index ~run ~receipt ~intent:(Some intent) in
      let%bind index =
        match run.lifecycle with
        | Admitted | Active | Waiting _ -> Ok index
        | Terminal terminal ->
          let%bind terminal_receipt =
            make_receipt
              state
              run
              ~kind:Terminal
              ~key:
                ("run-terminal:"
                 ^ P.Id.Run.to_string run.id
                 ^ ":"
                 ^ Int64.to_string run.revision)
              ~digest:(digest (P.Run.Terminal.to_json terminal))
              ~now
          in
          Run_state.commit index ~run ~receipt:terminal_receipt ~intent:None
      in
      Ok index
  in
  { run; delta = Session_delta.Run_state_changed index }
;;

let settle_operation (state : Session_state.t) ~operation_id ~outcome ~now =
  let open Result.Let_syntax in
  match state.run_state with
  | None -> Ok []
  | Some original ->
    let%map index =
      List.fold_result (Run_state.runs original) ~init:original ~f:(fun index previous ->
        let work =
          List.find previous.P.Run.owned_work ~f:(fun work ->
            match work.P.Run_work.key with
            | Operation id -> P.Id.Operation.equal id operation_id
            | Retained _ -> false)
        in
        match previous.lifecycle, work with
        | Terminal _, _ | (Admitted | Active | Waiting _), None -> Ok index
        | (Admitted | Active | Waiting _), Some work ->
          if
            List.exists previous.terminal_work ~f:(fun proof ->
              P.Run_work.equal proof.work work)
          then Ok index
          else (
            let%bind proof =
              P.Run_work.Terminal.create
                ~work
                ~outcome
                ~revision:(Int64.succ previous.revision)
            in
            let terminal_work = proof :: previous.terminal_work in
            let pending =
              List.filter (Run_state.intents index) ~f:(fun intent ->
                P.Id.Run.equal intent.Run_intent.receipt.run_id previous.id
                && Run_intent.Disposition.equal intent.disposition Pending)
            in
            let requested =
              List.find_map pending ~f:(fun intent ->
                match intent.action with
                | Finish { terminal; _ } -> Some terminal
                | Continue | Wait _ -> None)
            in
            let settled = List.length terminal_work = List.length previous.owned_work in
            let terminal_outcome =
              List.fold terminal_work ~init:outcome ~f:(fun accumulated proof ->
                match proof.P.Run_work.Terminal.outcome, accumulated with
                | Failed, _ | _, Failed -> P.Run_work.Terminal.Failed
                | Cancelled, _ | _, Cancelled -> Cancelled
                | Limited, _ | _, Limited -> Limited
                | Interrupted, _ | _, Interrupted -> Interrupted
                | Unconfirmed, _ | _, Unconfirmed -> Unconfirmed
                | Succeeded, Succeeded -> Succeeded)
            in
            let awaiting_callback =
              match previous.lifecycle with
              | Waiting _ -> true
              | Admitted | Active | Terminal _ -> false
            in
            let terminal =
              if (not settled) || awaiting_callback
              then None
              else (
                match terminal_outcome with
                | Failed -> Some (P.Run.Terminal.Failed None)
                | Cancelled -> Some P.Run.Terminal.Cancelled
                | Limited -> Some P.Run.Terminal.Limited
                | Interrupted | Unconfirmed -> Some P.Run.Terminal.Interrupted
                | Succeeded ->
                  (match requested, previous.mode with
                   | Some terminal, _ -> Some terminal
                   | None, Single_turn -> Some (P.Run.Terminal.Completed None)
                   | None, Workflow -> None))
            in
            let lifecycle =
              Option.value_map terminal ~default:previous.lifecycle ~f:(fun terminal ->
                P.Run.Lifecycle.Terminal terminal)
            in
            let%bind run =
              rebuild
                previous
                ~lifecycle
                ~owned_work:previous.owned_work
                ~relinquished_work:previous.relinquished_work
                ~terminal_work
                ~now
            in
            let%bind intents =
              match terminal with
              | None -> Ok []
              | Some _ ->
                List.map pending ~f:(fun intent ->
                  Run_intent.consume intent ~operation_id:(Some operation_id))
                |> Result.all
            in
            let%bind index = Run_state.advance index ~run ~intents in
            match terminal with
            | None -> Ok index
            | Some terminal ->
              let%bind receipt =
                make_receipt
                  state
                  run
                  ~kind:Terminal
                  ~key:
                    ("run-terminal:"
                     ^ P.Id.Run.to_string run.id
                     ^ ":"
                     ^ Int64.to_string run.revision)
                  ~digest:(digest (P.Run.Terminal.to_json terminal))
                  ~now
              in
              Run_state.commit index ~run ~receipt ~intent:None))
    in
    [ Session_delta.Run_state_changed index ]
;;

let admit_turn
      (state : Session_state.t)
      ~(operation : P.Operation.t)
      ~events
      ~deliveries
      ~now
  =
  let open Result.Let_syntax in
  match state.run_state with
  | None -> Ok ([], [])
  | Some original ->
    let execution_ids =
      List.filter_map events ~f:(fun (event : P.Moderator_execution.t) ->
        match event.intent with
        | Some Applied -> Some event.context.id
        | Some (Pending | Waiting_compaction _ | Discarded _) | None -> None)
      |> Set.of_list (module Execution_id)
    in
    let%map index, resumed =
      List.fold_result
        (Run_state.intents original)
        ~init:(original, [])
        ~f:(fun (index, resumed) intent ->
          match intent.Run_intent.disposition with
          | Consumed _ | Retired -> Ok (index, resumed)
          | Pending ->
            let%bind run =
              Result.of_option
                (Run_state.find index intent.receipt.run_id)
                ~error:(conflict "turn intent references absent run")
            in
            let selected =
              match intent.action, run.lifecycle with
              | Continue, (Admitted | Active) -> Set.mem execution_ids intent.execution_id
              | Wait wake, Waiting retained when P.Run_wake.equal wake retained ->
                (match wake.occurrence with
                 | Subscription_delivery { delivery_id; _ } ->
                   List.exists deliveries ~f:(fun (delivery : P.Delivery.t) ->
                     P.Id.Delivery.equal delivery.context.id delivery_id)
                 | Job_completion _ | Delivered_timer _ -> false)
              | Finish _, _
              | Continue, (Waiting _ | Terminal _)
              | Wait _, (Admitted | Active | Waiting _ | Terminal _) -> false
            in
            if not selected
            then Ok (index, resumed)
            else (
              let%bind () =
                match intent.action with
                | Wait wake -> Run_wake_owner.validate state ~run ~wake
                | Continue -> Ok ()
                | Finish _ -> Error (conflict "finish cannot admit another turn")
              in
              let%bind work =
                P.Run_work.create
                  ~key:(Operation operation.id)
                  ~generation:operation.generation
              in
              let owned_work =
                Set.add (Set.of_list (module P.Run_work) run.owned_work) work
                |> Set.to_list
              in
              let%bind next =
                rebuild
                  run
                  ~lifecycle:Active
                  ~owned_work
                  ~relinquished_work:run.relinquished_work
                  ~terminal_work:run.terminal_work
                  ~now
              in
              let%bind intent =
                Run_intent.consume intent ~operation_id:(Some operation.id)
              in
              let%map index = Run_state.advance index ~run:next ~intents:[ intent ] in
              index, run.id :: resumed))
    in
    ( resumed
    , if List.is_empty resumed then [] else [ Session_delta.Run_state_changed index ] )
;;

let claim_wake (state : Session_state.t) ~(executing : P.Moderator_execution.t) ~now =
  let open Result.Let_syntax in
  match state.run_state with
  | None -> Ok ([], [])
  | Some original ->
    let%bind index, resumed =
      List.fold_result
        (Run_state.intents original)
        ~init:(original, [])
        ~f:(fun (index, resumed) intent ->
          match intent.Run_intent.disposition, intent.action with
          | Pending, Wait wake ->
            let%bind run =
              Result.of_option
                (Run_state.find index intent.receipt.run_id)
                ~error:(conflict "wake intent references absent run")
            in
            (match run.lifecycle with
             | Waiting retained when P.Run_wake.equal wake retained ->
               let%bind matches =
                 Run_wake_owner.claim_matches state ~run ~wake ~executing
               in
               if not matches
               then Ok (index, resumed)
               else (
                 let%bind work =
                   P.Run_work.create
                     ~key:(Retained (Moderator_execution executing.context.id))
                     ~generation:executing.context.generation
                 in
                 let owned_work =
                   Set.add (Set.of_list (module P.Run_work) run.owned_work) work
                   |> Set.to_list
                 in
                 let%bind run =
                   rebuild
                     run
                     ~lifecycle:Active
                     ~owned_work
                     ~relinquished_work:run.relinquished_work
                     ~terminal_work:run.terminal_work
                     ~now
                 in
                 let%bind intent = Run_intent.consume intent ~operation_id:None in
                 let%bind job_delivery =
                   match Run_job_delivery.Key.of_wake wake with
                   | None -> Ok None
                   | Some key ->
                     (match Run_state.find_job_delivery index key with
                      | None -> Ok None
                      | Some delivery ->
                        let%map delivery =
                          Run_job_delivery.claim
                            delivery
                            ~execution_id:executing.context.id
                        in
                        Some delivery)
                 in
                 let%map index =
                   match job_delivery with
                   | None -> Run_state.advance index ~run ~intents:[ intent ]
                   | Some job_delivery ->
                     Run_state.advance_claim index ~job_delivery ~run ~intents:[ intent ]
                 in
                 index, run.id :: resumed)
             | Admitted | Active | Waiting _ | Terminal _ -> Ok (index, resumed))
          | Pending, (Continue | Finish _) | (Consumed _ | Retired), _ ->
            Ok (index, resumed))
    in
    (match resumed with
     | [] -> Ok ([], [])
     | [ run_id ] -> Ok ([ run_id ], [ Session_delta.Run_state_changed index ])
     | _ :: _ :: _ -> Error (conflict "wake belongs to more than one run"))
;;
