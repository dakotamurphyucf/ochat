open! Core

type running_job =
  { generation : int
  ; attempt : int
  ; sw : Eio.Switch.t
  }

type t =
  { closed : bool Atomic.t
  ; capacity : Job_capacity.t
  ; mutex : Eio.Mutex.t
  ; mutable running : (Agent_protocol.Id.Job.t, running_job) Map.Poly.t
  ; mutable cursor : int
  ; mutable delivering : Session_registry.entry list
  }

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
  | Queued | Waiting_permission _ | Succeeded | Failed _ | Cancelled | Interrupted _ ->
    Ok ()
;;

let reconcile_entry entry =
  Result.bind
    (Agent_session.Session_actor.state entry.Session_registry.actor)
    ~f:(fun state ->
      List.fold_result state.jobs ~init:() ~f:(fun () job -> reconcile_job entry job))
;;

let reconcile_recovered ~registry =
  Session_registry.entries registry
  |> List.fold_result ~init:() ~f:(fun () entry -> reconcile_entry entry)
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

let run_model_job entry job =
  match model_job_input job with
  | Error error ->
    complete entry job (Agent_session.Runtime_builder.Model_failed error.message)
  | Ok (recipe, payload) ->
    let outcome =
      match
        Runtime_owner.execute_model_job entry.Session_registry.runtime ~recipe ~payload
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

let complete_background entry (job : Agent_protocol.Job.t) outcome =
  ignore
    (Agent_session.Session_actor.complete_background_job
       entry.Session_registry.actor
       ~job_id:job.id
       ~generation:job.generation
       ~attempt:job.attempt
       outcome
     : (Agent_protocol.Job.t, Agent_protocol.Error.t) result)
;;

let run_background_job entry job =
  let outcome =
    match Runtime_owner.execute_background_job entry.Session_registry.runtime job with
    | Ok outcome -> outcome
    | Error error -> failure_completion error
  in
  complete_background entry job outcome
;;

let register_running t (job : Agent_protocol.Job.t) job_sw =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Atomic.get t.closed
    then Eio.Switch.fail job_sw Job_cancelled
    else
      t.running
      <- Map.set
           t.running
           ~key:job.id
           ~data:{ generation = job.generation; attempt = job.attempt; sw = job_sw })
;;

let unregister_running t (job : Agent_protocol.Job.t) =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Map.find t.running job.id with
    | Some running
      when Int.equal running.generation job.generation
           && Int.equal running.attempt job.attempt ->
      t.running <- Map.remove t.running job.id
    | None | Some _ -> ())
;;

let run_claimed_job entry job =
  try
    match job.Agent_protocol.Job.kind with
    | Model_call -> run_model_job entry job
    | Async_tool -> run_background_job entry job
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

let dispatch t sw entry job lease =
  Eio.Fiber.fork ~sw (fun () ->
    Exn.protect
      ~f:(fun () ->
        try
          Eio.Switch.run (fun job_sw ->
            register_running t job job_sw;
            run_claimed_job entry job)
        with
        | Job_cancelled | Eio.Cancel.Cancelled _ -> ())
      ~finally:(fun () ->
        unregister_running t job;
        Job_capacity.release lease))
;;

let nested_depth (job : Agent_protocol.Job.t) =
  match job.payload with
  | `Object fields ->
    (match List.Assoc.find fields "nested_depth" ~equal:String.equal with
     | None -> Ok 0
     | Some (`Number encoded) ->
       (match Int.of_string_opt encoded with
        | Some value when value >= 0 -> Ok value
        | Some _ | None ->
          Error (Agent_protocol.Error.invalid_request "job nested_depth is invalid"))
     | Some _ ->
       Error (Agent_protocol.Error.invalid_request "job nested_depth is invalid"))
  | _ -> Ok 0
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

let reject_job entry (job : Agent_protocol.Job.t) (error : Agent_protocol.Error.t) =
  match
    Agent_session.Session_actor.claim_job
      entry.Session_registry.actor
      ~job_id:job.id
      ~generation:job.generation
  with
  | Ok (Some claimed) ->
    (match claimed.kind with
     | Async_tool -> complete_background entry claimed (failure_completion error)
     | _ ->
       complete entry claimed (Agent_session.Runtime_builder.Model_failed error.message));
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
    dispatch t sw entry claimed lease;
    true
  | Ok None | Error _ ->
    Job_capacity.release lease;
    false
;;

let claim t sw entry (job : Agent_protocol.Job.t) =
  let already_running = Eio.Mutex.use_ro t.mutex (fun () -> Map.mem t.running job.id) in
  match already_running with
  | true -> false
  | false ->
    (match capacity_key entry job with
     | Error error -> reject_job entry job error
     | Ok key ->
       (match Job_capacity.try_acquire t.capacity key with
        | Error error -> reject_job entry job error
        | Ok None -> false
        | Ok (Some lease) -> claim_with_lease t sw entry job lease))
;;

let delivery_pending (job : Agent_protocol.Job.t) =
  match job.kind, job.status, job.delivery with
  | ( Model_call
    , (Succeeded | Failed _ | Cancelled | Interrupted _)
    , Agent_protocol.Job.Pending ) -> true
  | _ -> false
;;

let deliver entry (job : Agent_protocol.Job.t) =
  match Runtime_owner.deliver_model_job_completion entry.Session_registry.runtime job with
  | Error _ -> ()
  | Ok () ->
    ignore
      (Runtime_owner.drain_idle_moderator entry.runtime
       : (bool, Agent_protocol.Error.t) result)
;;

let deliver_pending entry jobs =
  List.filter jobs ~f:delivery_pending |> List.iter ~f:(deliver entry)
;;

let dispatch_delivery t sw entry jobs =
  if
    List.exists jobs ~f:delivery_pending
    && not (List.mem t.delivering entry ~equal:phys_equal)
  then (
    t.delivering <- entry :: t.delivering;
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~f:(fun () -> if not (Atomic.get t.closed) then deliver_pending entry jobs)
        ~finally:(fun () ->
          t.delivering
          <- List.filter t.delivering ~f:(fun active -> not (phys_equal active entry)))))
;;

let claim_one t sw entry jobs =
  let rec loop = function
    | [] -> ()
    | (job : Agent_protocol.Job.t) :: rest ->
      (match job.status, job.delivery with
       | Agent_protocol.Job.Queued, Agent_protocol.Job.Not_required -> loop rest
       | Queued, (Pending | Delivered _) -> if not (claim t sw entry job) then loop rest
       | ( ( Running
           | Waiting_permission _
           | Succeeded
           | Failed _
           | Cancelled
           | Interrupted _ )
         , _ ) -> loop rest)
  in
  loop jobs
;;

let cancel_terminal_worker t (job : Agent_protocol.Job.t) =
  match job.status with
  | Cancelled | Interrupted _ ->
    Eio.Mutex.use_ro t.mutex (fun () ->
      Map.find t.running job.id
      |> Option.iter ~f:(fun running ->
        if
          Int.equal running.generation job.generation
          && Int.equal running.attempt job.attempt
        then Eio.Switch.fail running.sw Job_cancelled))
  | _ -> ()
;;

let state_jobs t entry =
  match Agent_session.Session_actor.state entry.Session_registry.actor with
  | Error _ -> []
  | Ok state ->
    List.iter state.jobs ~f:(cancel_terminal_worker t);
    if Agent_protocol.Session.equal_desired_state state.lifecycle.desired Stopped
    then []
    else
      List.filter state.jobs ~f:(fun job ->
        (not state.halted)
        || not
             (match job.status with
              | Queued -> true
              | _ -> false))
;;

let rotate entries cursor =
  match entries with
  | [] -> [], 0
  | _ ->
    let length = List.length entries in
    let offset = cursor mod length in
    let before, after = List.split_n entries offset in
    after @ before, (offset + 1) mod length
;;

let process t sw registry =
  let entries, cursor = rotate (Session_registry.entries registry) t.cursor in
  t.cursor <- cursor;
  let states = List.map entries ~f:(fun entry -> entry, state_jobs t entry) in
  List.iter states ~f:(fun (entry, jobs) -> dispatch_delivery t sw entry jobs);
  List.iter states ~f:(fun (entry, jobs) -> claim_one t sw entry jobs)
;;

let rec run t sw clock registry =
  if not (Atomic.get t.closed)
  then (
    process t sw registry;
    Eio.Time.sleep clock 0.05;
    run t sw clock registry)
;;

let start ~sw ~clock ~registry ~capacity =
  let t =
    { closed = Atomic.make false
    ; capacity
    ; mutex = Eio.Mutex.create ()
    ; running = Map.Poly.empty
    ; cursor = 0
    ; delivering = []
    }
  in
  Eio.Fiber.fork ~sw (fun () -> run t sw clock registry);
  t
;;

let cancel t job_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Map.find t.running job_id
    |> Option.iter ~f:(fun running -> Eio.Switch.fail running.sw Job_cancelled))
;;

let close t =
  Atomic.set t.closed true;
  Eio.Mutex.use_ro t.mutex (fun () ->
    Map.iter t.running ~f:(fun running -> Eio.Switch.fail running.sw Job_cancelled))
;;

let is_running t = not (Atomic.get t.closed)
let running_count t = Eio.Mutex.use_ro t.mutex (fun () -> Map.length t.running)
