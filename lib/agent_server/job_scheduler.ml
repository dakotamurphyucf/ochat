open! Core
module P = Agent_protocol
module Developer = Job_scheduler_developer

module Fairness = struct
  type 'candidate group =
    { owner : P.Execution_owner.t
    ; candidates : 'candidate list
    }

  let has_queued_claim jobs =
    List.exists jobs ~f:(fun (job : P.Job.t) ->
      match job.status, job.delivery with
      | Queued, (Pending | Delivered _) -> true
      | Queued, (Not_required | Discarded _)
      | ( ( Running
          | Waiting_permission _
          | Waiting_completion _
          | Succeeded
          | Failed _
          | Cancelled
          | Interrupted _ )
        , _ ) -> false)
  ;;

  let conversation_group ~owner ~jobs ~candidate =
    if has_queued_claim jobs
    then Option.map owner ~f:(fun owner -> { owner; candidates = [ candidate ] })
    else None
  ;;

  let order ~after groups =
    let groups =
      List.filter groups ~f:(fun group -> not (List.is_empty group.candidates))
      |> List.sort ~compare:(fun left right ->
        P.Execution_owner.compare left.owner right.owner)
    in
    let rec check_unique = function
      | left :: (right :: _ as rest) ->
        if P.Execution_owner.equal left.owner right.owner
        then invalid_arg "duplicate scheduler owner group";
        check_unique rest
      | [] | [ _ ] -> ()
    in
    check_unique groups;
    let groups =
      match after with
      | None -> groups
      | Some anchor ->
        let before, after =
          List.split_while groups ~f:(fun group ->
            P.Execution_owner.compare group.owner anchor <= 0)
        in
        after @ before
    in
    groups, Option.map (List.hd groups) ~f:(fun group -> group.owner)
  ;;
end

module Worker_key = struct
  module T = struct
    type t =
      { identity : P.Host_job.Identity.t
      ; attempt : int
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let of_conversation (job : P.Job.t) =
    let open Result.Let_syntax in
    let%map host = P.Host_job.of_conversation job in
    { identity = P.Host_job.identity host; attempt = job.attempt }
  ;;
end

type running_job =
  { key : Worker_key.t
  ; session_id : Agent_protocol.Id.Session.t
  ; actor : Agent_session.Session_actor.t
  ; sw : Eio.Switch.t
  ; rejected : bool
  }

type t =
  { domain_id : int
  ; closed : bool Atomic.t
  ; loop_wait_deadline : float option Atomic.t
  ; loop_done : unit Eio.Promise.t
  ; active_children : int ref
  ; children_idle : Eio.Condition.t
  ; capacity : Job_capacity.t
  ; developer : Developer.t option
  ; developer_registry : Developer_root_owner.Registry.t option
  ; mutex : Eio.Mutex.t
  ; mutable running : running_job Map.M(Worker_key).t
  ; mutable after : P.Execution_owner.t option
  ; mutable delivering : Session_registry.entry list
  ; mutable delivery_cancels : Eio.Cancel.t list
  ; deliveries_idle : Eio.Condition.t
  ; model_job_inference :
      Session_registry.entry
      -> Agent_protocol.Job.t
      -> (Session_factory.model_job_inference, Agent_protocol.Error.t) Result.t
  ; sleep : float -> unit
  }

let scheduler_fiber_key : t Eio.Fiber.key = Eio.Fiber.create_key ()
let on_domain t = Int.equal t.domain_id (Stdlib.Domain.self () :> int)

let check_domain_exn t =
  if not (on_domain t) then invalid_arg "developer scheduler used on a foreign domain"
;;

let child_finished t =
  t.active_children := !(t.active_children) - 1;
  if !(t.active_children) = 0 then Eio.Condition.broadcast t.children_idle
;;

let fork_owned t ~sw ~on_refused f =
  let counted = ref false in
  match
    check_domain_exn t;
    Eio.Switch.check sw;
    t.active_children := !(t.active_children) + 1;
    counted := true;
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~f:(fun () -> Eio.Fiber.with_binding scheduler_fiber_key t f)
        ~finally:(fun () -> Eio.Cancel.protect (fun () -> child_finished t)))
  with
  | () -> ()
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
      if !counted then child_finished t;
      (* Both callbacks are local nonyielding cleanup for pre-fork custody.
         Preserve the original switch/domain failure if cleanup itself fails. *)
      try on_refused () with
      | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

module For_testing = struct
  let fair_order ~after groups =
    let groups =
      List.map groups ~f:(fun (owner, candidates) ->
        if
          not
            (List.for_all candidates ~f:(fun candidate ->
               P.Execution_owner.equal owner (P.Host_job.owner candidate)))
        then invalid_arg "scheduler candidate owner differs from group";
        Fairness.{ owner; candidates })
    in
    let ordered, after = Fairness.order ~after groups in
    List.map ordered ~f:(fun group -> group.owner, group.candidates), after
  ;;

  let fair_conversation_order ~after rows =
    let groups =
      List.filter_map rows ~f:(fun (owner, jobs) ->
        Fairness.conversation_group ~owner:(Some owner) ~jobs ~candidate:jobs)
    in
    let ordered, after = Fairness.order ~after groups in
    List.map ordered ~f:(fun group -> group.owner), after
  ;;

  let developer_retry_pending t ~root =
    Option.value_map t.developer ~default:false ~f:(fun developer ->
      Developer.retry_pending developer ~root)
  ;;

  let loop_wait_deadline t =
    if (not (on_domain t)) || Atomic.get t.closed
    then None
    else Atomic.get t.loop_wait_deadline
  ;;
end

exception Job_cancelled

let interrupted_reason = "daemon restarted while the job was running"

let reconcile_job entry (job : Agent_protocol.Job.t) =
  match job.status with
  | Agent_protocol.Job.Running ->
    Agent_session.Session_actor.interrupt_job
      entry.Session_registry.actor
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      ~reason:interrupted_reason
    |> Result.map ~f:(fun (_ : Agent_protocol.Job.t) -> ())
  | Queued
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> Ok ()
;;

let reconcile_entry entry ~max_count ~max_total_bytes =
  let open Result.Let_syntax in
  let%bind () =
    Agent_session.Session_actor.recover_background_results
      entry.Session_registry.actor
      ~max_count
      ~max_total_bytes
  in
  let%bind state = Agent_session.Session_actor.state entry.actor in
  List.fold_result state.jobs ~init:() ~f:(fun () job -> reconcile_job entry job)
;;

let reconcile_recovered ~registry ~max_count ~max_total_bytes =
  Session_registry.entries registry
  |> List.fold_result ~init:() ~f:(fun () entry ->
    reconcile_entry entry ~max_count ~max_total_bytes)
;;

let payload_fields = function
  | `Object fields -> Ok fields
  | _ ->
    Error (Agent_protocol.Error.invalid_request "model job payload must be an object")
;;

let model_job_input (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let%bind fields = payload_fields job.payload in
  let%bind recipe =
    match List.Assoc.find fields "recipe" ~equal:String.equal with
    | Some (`String value) -> Ok value
    | Some _ | None ->
      Error (Agent_protocol.Error.invalid_request "model job recipe is missing")
  in
  let%map payload =
    List.Assoc.find fields "payload" ~equal:String.equal
    |> Result.of_option
         ~error:(Agent_protocol.Error.invalid_request "model job input is missing")
  in
  recipe, payload
;;

let complete entry job outcome =
  ignore
    (Agent_session.Session_actor.complete_job
       entry.Session_registry.actor
       ~job_id:job.Agent_protocol.Job.id
       ~generation:job.generation
       ~attempt:job.attempt
       outcome
     : (Agent_protocol.Job.t, Agent_protocol.Error.t) result)
;;

let run_model_job t entry job =
  match model_job_input job with
  | Error error ->
    complete entry job (Agent_session.Runtime_builder.Model_failed error.message)
  | Ok (recipe, payload) ->
    let outcome =
      match
        let open Result.Let_syntax in
        let%bind inference = t.model_job_inference entry job in
        Runtime_owner.execute_model_job
          entry.Session_registry.runtime
          ~inference_context:inference.context
          ~capture_recipe_target:inference.capture_recipe_target
          ~recipe
          ~payload
      with
      | Ok outcome -> outcome
      | Error error -> Agent_session.Runtime_builder.Model_failed error.message
    in
    complete entry job outcome
;;

let failure_completion (failure : Agent_protocol.Error.t) =
  let outcome =
    Agent_protocol.Completion.Failed
      { code = "background." ^ Agent_protocol.Error.code_to_string failure.code
      ; message = failure.message
      ; retryable = failure.retryable
      ; details = failure.data
      }
  in
  match Agent_protocol.Completion.validate outcome with
  | Ok () -> outcome
  | Error _ ->
    Agent_protocol.Completion.Failed
      { code = "background.host_failed"
      ; message = "Background execution failed."
      ; retryable = false
      ; details = `Null
      }
;;

let completion_still_owned entry (job : Agent_protocol.Job.t) =
  match Agent_session.Session_actor.state entry.Session_registry.actor with
  | Error _ -> true
  | Ok state ->
    Int.equal state.identity.generation job.generation
    && List.exists state.jobs ~f:(fun current ->
      Agent_protocol.Id.Job.equal current.id job.id
      && Int.equal current.generation job.generation
      && Int.equal current.attempt job.attempt
      &&
      match current.status with
      | Running -> true
      | _ -> false)
;;

let save_background_result t entry (job : Agent_protocol.Job.t) persist =
  let rec save delay =
    match Atomic.get t.closed with
    | true -> ()
    | false ->
      (match persist () with
       | Ok _ -> ()
       | Error _ ->
         (match completion_still_owned entry job with
          | false -> ()
          | true ->
            t.sleep delay;
            save (Float.min 1. (delay *. 2.))))
  in
  save 0.05
;;

let complete_background t entry (job : Agent_protocol.Job.t) outcome =
  save_background_result t entry job (fun () ->
    Agent_session.Session_actor.complete_background_job
      entry.Session_registry.actor
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      outcome)
;;

let run_background_job t entry job =
  let outcome =
    match Runtime_owner.execute_background_job entry.Session_registry.runtime job with
    | Ok outcome -> outcome
    | Error error -> Runtime_owner.Completed (failure_completion error)
  in
  match outcome with
  | Completed outcome -> complete_background t entry job outcome
  | Pending dependency ->
    save_background_result t entry job (fun () ->
      Agent_session.Session_actor.defer_background_job
        entry.Session_registry.actor
        ~job_id:job.id
        ~generation:job.generation
        ~attempt:job.attempt
        dependency)
;;

let register_running t key (job : Agent_protocol.Job.t) job_sw ~actor ~rejected =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Atomic.get t.closed
    then Eio.Switch.fail job_sw Job_cancelled
    else if Map.mem t.running key
    then failwith "duplicate conversation job worker"
    else
      t.running
      <- Map.set
           t.running
           ~key
           ~data:{ key; session_id = job.session_id; actor; sw = job_sw; rejected })
;;

let unregister_running t key ~actor =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Map.find t.running key with
    | Some running when phys_equal running.actor actor ->
      t.running <- Map.remove t.running key
    | None | Some _ -> ())
;;

let run_claimed_job t entry job =
  try
    match job.Agent_protocol.Job.kind with
    | Model_call -> run_model_job t entry job
    | Async_tool -> run_background_job t entry job
    | Nested_agent | Scheduled_event | Shell_process | Compaction ->
      complete
        entry
        job
        (Agent_session.Runtime_builder.Model_failed "job kind has no installed executor")
  with
  | Eio.Cancel.Cancelled _ -> ()
  | exn ->
    (match job.kind with
     | Async_tool ->
       complete_background
         t
         entry
         job
         (failure_completion
            (Agent_protocol.Error.create
               Internal_error
               ~message:"Background worker failed unexpectedly."
               ~retryable:false
               ()))
     | _ ->
       complete entry job (Agent_session.Runtime_builder.Model_failed (Exn.to_string exn)))
;;

let dispatch t sw entry job lease rejection =
  let key =
    match Worker_key.of_conversation job with
    | Ok key -> key
    | Error _ -> failwith "claimed conversation job has invalid identity"
  in
  fork_owned
    t
    ~sw
    ~on_refused:(fun () -> Option.iter lease ~f:Job_capacity.release)
    (fun () ->
       Exn.protect
         ~f:(fun () ->
           try
             Eio.Switch.run (fun job_sw ->
               register_running
                 t
                 key
                 job
                 job_sw
                 ~actor:entry.Session_registry.actor
                 ~rejected:(Option.is_some rejection);
               Eio.Switch.check job_sw;
               match rejection with
               | None -> run_claimed_job t entry job
               | Some error ->
                 (match job.kind with
                  | Async_tool ->
                    complete_background t entry job (failure_completion error)
                  | _ ->
                    complete
                      entry
                      job
                      (Agent_session.Runtime_builder.Model_failed error.message)))
           with
           | Job_cancelled | Eio.Cancel.Cancelled _ -> ())
         ~finally:(fun () ->
           Eio.Cancel.protect (fun () ->
             unregister_running t key ~actor:entry.Session_registry.actor;
             Option.iter lease ~f:Job_capacity.release)))
;;

let nested_depth (job : Agent_protocol.Job.t) =
  match job.launch, job.payload with
  | Some launch, _ -> Ok launch.nested_depth
  | None, `Object fields ->
    (match List.Assoc.find fields "nested_depth" ~equal:String.equal with
     | None -> Ok 0
     | Some (`Number encoded) ->
       (match Int.of_string_opt encoded with
        | Some value when value >= 0 -> Ok value
        | Some _ | None ->
          Error (Agent_protocol.Error.invalid_request "job nested_depth is invalid"))
     | Some _ ->
       Error (Agent_protocol.Error.invalid_request "job nested_depth is invalid"))
  | None, _ -> Ok 0
;;

let capacity_key entry job =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state entry.Session_registry.actor in
  let%map nested_depth = nested_depth job in
  let prompt =
    Option.value_map
      state.spec.prompt_definition_id
      ~default:"<local>"
      ~f:Agent_protocol.Id.Prompt_definition.to_string
  in
  Job_capacity.Key.create
    ~principal_id:state.identity.creating_principal
    ~prompt
    ~workspace_conflict_domain:state.spec.workspace_instance.conflict_domain
    ~session_id:state.identity.session_id
    ~kind:job.Agent_protocol.Job.kind
    ~nested_depth
;;

let reject_job t sw entry (job : Agent_protocol.Job.t) (error : Agent_protocol.Error.t) =
  match
    Agent_session.Session_actor.claim_job
      entry.Session_registry.actor
      ~job_id:job.id
      ~generation:job.generation
  with
  | Ok (Some claimed) ->
    dispatch t sw entry claimed None (Some error);
    true
  | Ok None | Error _ -> false
;;

let claim_with_lease t sw entry (job : Agent_protocol.Job.t) lease =
  match
    Agent_session.Session_actor.claim_job
      entry.Session_registry.actor
      ~job_id:job.id
      ~generation:job.generation
  with
  | Ok (Some claimed) ->
    dispatch t sw entry claimed (Some lease) None;
    true
  | Ok None | Error _ ->
    Job_capacity.release lease;
    false
;;

let claim t sw entry (job : Agent_protocol.Job.t) =
  let key = Worker_key.of_conversation job in
  let already_running =
    Eio.Mutex.use_ro t.mutex (fun () ->
      match key with
      | Error _ -> false
      | Ok key ->
        Map.exists t.running ~f:(fun running ->
          P.Host_job.Identity.equal running.key.identity key.identity
          || (running.rejected && P.Id.Session.equal running.session_id job.session_id)))
  in
  match key, already_running with
  | Error error, _ -> reject_job t sw entry job error
  | Ok _, true -> false
  | Ok _, false ->
    (match capacity_key entry job with
     | Error error -> reject_job t sw entry job error
     | Ok key ->
       (match Job_capacity.try_acquire_job t.capacity key ~job with
        | Error error -> reject_job t sw entry job error
        | Ok None -> false
        | Ok (Some lease) -> claim_with_lease t sw entry job lease))
;;

let delivery_pending (job : Agent_protocol.Job.t) =
  match job.kind, job.status, job.delivery, job.launch with
  | ( Model_call
    , (Succeeded | Failed _ | Cancelled | Interrupted _)
    , Agent_protocol.Job.Pending
    , _ )
  | ( Async_tool
    , (Succeeded | Failed _ | Cancelled | Interrupted _)
    , Agent_protocol.Job.Pending
    , Some _ ) -> true
  | _ -> false
;;

let deliver entry (job : Agent_protocol.Job.t) =
  let delivery =
    match job.kind with
    | Async_tool ->
      Runtime_owner.deliver_background_job_completion entry.Session_registry.runtime job
    | _ -> Runtime_owner.deliver_model_job_completion entry.Session_registry.runtime job
  in
  match delivery with
  | Error _ -> ()
  | Ok () ->
    ignore
      (Runtime_owner.drain_idle_moderator entry.runtime
       : (bool, Agent_protocol.Error.t) result)
;;

let pending_run_deliveries entry =
  match Agent_session.Session_actor.state entry.Session_registry.actor with
  | Error _ -> []
  | Ok state ->
    Option.value_map state.run_state ~default:[] ~f:(fun index ->
      List.filter (Agent_session.Run_state.job_deliveries index) ~f:(fun delivery ->
        match Agent_session.Run_job_delivery.disposition delivery with
        | Pending -> true
        | Enqueued _ | Claimed _ | Retired _ -> false))
;;

let deliver_pending entry =
  List.iter (pending_run_deliveries entry) ~f:(fun delivery ->
    match
      Runtime_owner.deliver_run_job_completion entry.Session_registry.runtime delivery
    with
    | Error _ -> ()
    | Ok () ->
      ignore
        (Runtime_owner.drain_idle_moderator entry.runtime
         : (bool, Agent_protocol.Error.t) result));
  match Agent_session.Session_actor.state entry.actor with
  | Error _ -> ()
  | Ok state -> List.filter state.jobs ~f:delivery_pending |> List.iter ~f:(deliver entry)
;;

let dispatch_delivery t sw entry jobs =
  if
    (not (Atomic.get t.closed))
    && (List.exists jobs ~f:delivery_pending
        || not (List.is_empty (pending_run_deliveries entry)))
    && not (List.mem t.delivering entry ~equal:phys_equal)
  then (
    t.delivering <- entry :: t.delivering;
    fork_owned
      t
      ~sw
      ~on_refused:(fun () ->
        t.delivering
        <- List.filter t.delivering ~f:(fun active -> not (phys_equal active entry));
        if List.is_empty t.delivering then Eio.Condition.broadcast t.deliveries_idle)
      (fun () ->
         Exn.protect
           ~f:(fun () ->
             try
               Eio.Cancel.sub (fun cancel ->
                 t.delivery_cancels <- cancel :: t.delivery_cancels;
                 Exn.protect
                   ~f:(fun () -> if not (Atomic.get t.closed) then deliver_pending entry)
                   ~finally:(fun () ->
                     t.delivery_cancels
                     <- List.filter t.delivery_cancels ~f:(fun current ->
                          not (phys_equal current cancel))))
             with
             | Job_cancelled | Eio.Cancel.Cancelled _ -> ())
           ~finally:(fun () ->
             t.delivering
             <- List.filter t.delivering ~f:(fun active -> not (phys_equal active entry));
             if List.is_empty t.delivering then Eio.Condition.broadcast t.deliveries_idle)))
;;

let claim_one t sw entry jobs =
  let rec loop = function
    | [] -> ()
    | (job : Agent_protocol.Job.t) :: rest ->
      (match job.status, job.delivery with
       | Agent_protocol.Job.Queued, (Agent_protocol.Job.Not_required | Discarded _) ->
         loop rest
       | Queued, (Pending | Delivered _) -> if not (claim t sw entry job) then loop rest
       | ( ( Running
           | Waiting_permission _
           | Waiting_completion _
           | Succeeded
           | Failed _
           | Cancelled
           | Interrupted _ )
         , _ ) -> loop rest)
  in
  loop jobs
;;

let claim_developer developer batch =
  let rec loop = function
    | [] ->
      Developer.clear_retry developer batch;
      Developer.Skipped
    | candidate :: rest ->
      (match Developer.Batch.availability batch candidate with
       | Error _ ->
         Developer.clear_retry developer batch;
         Developer.Attempted
       | Ok (Developer.Availability.Runnable | Developer.Availability.Malformed_supported)
         ->
         (match Developer.offer developer batch candidate with
          | Ok Developer.Attempted -> Developer.Attempted
          | Ok Developer.Unavailable -> Developer.Unavailable
          | Ok Developer.Skipped -> loop rest
          | Error _ ->
            Developer.clear_retry developer batch;
            Developer.Attempted)
       | Ok
           ( Developer.Availability.Executor_unavailable
           | Developer.Availability.Artifact_installer_unavailable ) -> loop rest)
  in
  loop (Developer.Batch.candidates batch)
;;

let cancel_terminal_worker t ~actor (job : Agent_protocol.Job.t) =
  match job.status with
  | Cancelled | Interrupted _ ->
    (match Worker_key.of_conversation job with
     | Error _ -> ()
     | Ok key ->
       Eio.Mutex.use_ro t.mutex (fun () ->
         Map.find t.running key
         |> Option.iter ~f:(fun running ->
           if
             phys_equal running.actor actor
             && P.Host_job.Identity.equal running.key.identity key.identity
             && Int.equal running.key.attempt job.attempt
           then Eio.Switch.fail running.sw Job_cancelled)))
  | _ -> ()
;;

let state_jobs t entry =
  match Agent_session.Session_actor.state entry.Session_registry.actor with
  | Error _ -> None, []
  | Ok state ->
    let owner =
      match
        P.Execution_owner.conversation
          ~session_id:state.identity.session_id
          ~generation:state.identity.generation
      with
      | Ok owner -> owner
      | Error _ -> failwith "validated session has invalid scheduler owner"
    in
    Job_capacity.retire_previous_generations
      t.capacity
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation;
    List.iter state.jobs ~f:(Job_capacity.retire_job t.capacity);
    List.iter state.jobs ~f:(cancel_terminal_worker t ~actor:entry.actor);
    if
      Agent_protocol.Session.equal_desired_state state.lifecycle.desired Stopped
      || (match state.runtime_initialization with
          | Pending _ -> true
          | Ready -> false)
      ||
      match Inference.Selection.view state.spec.inference_target with
      | Unresolved -> true
      | Captured _ -> false
    then Some owner, []
    else
      ( Some owner
      , List.filter state.jobs ~f:(fun job ->
          (not state.halted)
          || not
               (match job.status with
                | Queued -> true
                | _ -> false)) )
;;

let process t sw registry =
  let entries = Session_registry.entries registry in
  let actors = List.map entries ~f:(fun entry -> entry.Session_registry.actor) in
  Eio.Mutex.use_ro t.mutex (fun () ->
    Map.iter t.running ~f:(fun running ->
      match List.mem actors running.actor ~equal:phys_equal with
      | true -> ()
      | false -> Eio.Switch.fail running.sw Job_cancelled));
  let states =
    List.map entries ~f:(fun entry ->
      let owner, jobs = state_jobs t entry in
      entry, owner, jobs)
  in
  List.iter states ~f:(fun (entry, _, jobs) ->
    List.iter jobs ~f:(fun job ->
      match job.Agent_protocol.Job.status with
      | Waiting_completion _ ->
        ignore
          (Agent_session.Session_actor.refresh_background_job
             entry.Session_registry.actor
             ~job_id:job.id
             ~generation:job.generation
             ~attempt:job.attempt
           : (Agent_protocol.Job.t, Agent_protocol.Error.t) result)
      | _ -> ()));
  List.iter states ~f:(fun (entry, _, jobs) -> dispatch_delivery t sw entry jobs);
  let conversation_groups =
    List.filter_map states ~f:(fun (entry, owner, jobs) ->
      Fairness.conversation_group ~owner ~jobs ~candidate:(`Conversation (entry, jobs)))
  in
  let developer_groups =
    match t.developer with
    | None -> []
    | Some developer ->
      let admitted batch =
        match Developer.current developer batch with
        | Ok true -> Some batch
        | Ok false | Error _ ->
          Developer.clear_retry developer batch;
          None
      in
      let retry = Option.bind (Developer.retry_batch developer) ~f:admitted in
      let fresh = Option.bind (Developer.take_batch developer) ~f:admitted in
      let batches =
        match retry, fresh with
        | Some prior, Some latest
          when P.Execution_owner.equal
                 (Developer.Batch.owner prior)
                 (Developer.Batch.owner latest) ->
          Developer.clear_retry developer prior;
          [ latest ]
        | Some prior, Some latest -> [ prior; latest ]
        | Some batch, None | None, Some batch -> [ batch ]
        | None, None -> []
      in
      List.map batches ~f:(fun batch ->
        Fairness.
          { owner = Developer.Batch.owner batch
          ; candidates = [ `Developer (developer, batch) ]
          })
  in
  let groups = conversation_groups @ developer_groups in
  let ordered, _ = Fairness.order ~after:t.after groups in
  let after = ref t.after in
  let considered = ref false in
  List.iter ordered ~f:(fun group ->
    List.iter group.candidates ~f:(function
      | `Conversation (entry, jobs) ->
        if not !considered
        then (
          considered := true;
          after := Some group.owner);
        claim_one t sw entry jobs
      | `Developer (developer, batch) ->
        let outcome = claim_developer developer batch in
        if
          (not !considered)
          &&
          match outcome with
          | Developer.Attempted -> true
          | Developer.Unavailable | Developer.Skipped -> false
        then (
          considered := true;
          after := Some group.owner)));
  t.after <- !after
;;

let rec run t sw clock registry =
  if not (Atomic.get t.closed)
  then (
    Option.iter t.developer ~f:Developer.poll;
    process t sw registry;
    let deadline = Eio.Time.now clock +. 0.05 in
    Atomic.set t.loop_wait_deadline (Some deadline);
    Exn.protect
      ~finally:(fun () -> Atomic.set t.loop_wait_deadline None)
      ~f:(fun () -> Eio.Time.sleep_until clock deadline);
    run t sw clock registry)
;;

let start_controlled
      ~enabled
      ~sw
      ~clock
      ~registry
      ~developer_registry
      ~capacity
      ~model_job_inference
  =
  let developer =
    Option.map developer_registry ~f:(fun registry ->
      Developer.create ~sw ~clock ~registry ~capacity)
  in
  let loop_done, loop_done_u = Eio.Promise.create () in
  let t =
    { domain_id = (Stdlib.Domain.self () :> int)
    ; closed = Atomic.make (not enabled)
    ; loop_wait_deadline = Atomic.make None
    ; loop_done
    ; active_children = ref 0
    ; children_idle = Eio.Condition.create ()
    ; capacity
    ; developer
    ; developer_registry
    ; model_job_inference
    ; mutex = Eio.Mutex.create ()
    ; running = Map.empty (module Worker_key)
    ; after = None
    ; delivering = []
    ; delivery_cancels = []
    ; deliveries_idle = Eio.Condition.create ()
    ; sleep = Eio.Time.sleep clock
    }
  in
  if not enabled then Option.iter developer ~f:Developer.close;
  if enabled
  then (
    Eio.Switch.check sw;
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~f:(fun () ->
          Eio.Fiber.with_binding scheduler_fiber_key t (fun () -> run t sw clock registry))
        ~finally:(fun () -> Eio.Promise.resolve loop_done_u ())))
  else Eio.Promise.resolve loop_done_u ();
  t
;;

let uses_developer_registry t registry =
  (not (Atomic.get t.closed))
  && Option.exists t.developer_registry ~f:(fun installed ->
    phys_equal installed registry)
;;

let start ~sw ~clock ~registry ~developer_registry ~capacity ~model_job_inference =
  start_controlled
    ~enabled:true
    ~sw
    ~clock
    ~registry
    ~developer_registry
    ~capacity
    ~model_job_inference
;;

let cancel_conversation t ~actor ~job =
  match Worker_key.of_conversation job with
  | Error _ -> ()
  | Ok key ->
    Eio.Mutex.use_ro t.mutex (fun () ->
      Map.find t.running key
      |> Option.iter ~f:(fun running ->
        if phys_equal running.actor actor then Eio.Switch.fail running.sw Job_cancelled))
;;

let close t =
  check_domain_exn t;
  Atomic.set t.closed true;
  Option.iter t.developer ~f:Developer.close;
  List.iter t.delivery_cancels ~f:(fun cancel -> Eio.Cancel.cancel cancel Job_cancelled);
  Eio.Mutex.use_ro t.mutex (fun () ->
    Map.iter t.running ~f:(fun running -> Eio.Switch.fail running.sw Job_cancelled))
;;

let is_running t = not (Atomic.get t.closed)

let attach_developer t ~root ~runner =
  if Atomic.get t.closed
  then
    Error
      (P.Error.create
         P.Error.Server_shutting_down
         ~message:"developer scheduler is stopped"
         ~retryable:true
         ())
  else (
    match t.developer with
    | None -> Error (P.Error.invalid_request "developer scheduler has no root registry")
    | Some developer -> Developer.attach developer ~root ~runner)
;;

let attach_developer_model t ~root ~runner =
  if Atomic.get t.closed
  then
    Error
      (P.Error.create
         P.Error.Server_shutting_down
         ~message:"developer scheduler is stopped"
         ~retryable:true
         ())
  else (
    match t.developer with
    | None -> Error (P.Error.invalid_request "developer scheduler has no root registry")
    | Some developer -> Developer.attach_model developer ~root ~runner)
;;

let wake_developer t ~root =
  Option.iter t.developer ~f:(fun developer -> Developer.wake developer ~root)
;;

let retire_developer t ~root =
  match t.developer with
  | None -> Ok ()
  | Some developer -> Developer.retire developer ~root
;;

let await_developer_scans_idle t =
  match t.developer with
  | None -> Ok ()
  | Some developer -> Developer.await_idle developer
;;

let close_and_wait t =
  if not (on_domain t)
  then
    Error
      (P.Error.create
         P.Error.Invalid_state
         ~message:"developer scheduler cannot join from a foreign domain"
         ~retryable:false
         ())
  else if
    Option.value_map (Eio.Fiber.get scheduler_fiber_key) ~default:false ~f:(fun current ->
      phys_equal current t)
  then
    Error
      (P.Error.create
         P.Error.Invalid_state
         ~message:"developer scheduler cannot join its own fiber"
         ~retryable:false
         ())
  else
    Eio.Cancel.protect (fun () ->
      close t;
      Eio.Promise.await t.loop_done;
      let rec await_children () =
        if !(t.active_children) > 0
        then (
          Eio.Condition.await_no_mutex t.children_idle;
          await_children ())
      in
      await_children ();
      await_developer_scans_idle t)
;;

let rec await_deliveries_idle t =
  match t.delivering with
  | [] -> ()
  | _ ->
    Eio.Condition.await_no_mutex t.deliveries_idle;
    await_deliveries_idle t
;;

let running_count t = Eio.Mutex.use_ro t.mutex (fun () -> Map.length t.running)
