open! Core
module P = Agent_protocol
module Staging = Developer_job_staging
module Store = Agent_store.Developer_run_store
module Capability = Chat_response.Tool_capability
module Request = Chat_response.Background_request
module Terminal_queue = Developer_terminal_queue
module H = P.Host_invocation
module Job_scope = Agent_session.Developer_job_scope
module Model_transaction = Developer_external_model_transaction
module Mutation_gate = Developer_root_mutation_gate
module Publisher = Agent_store.Developer_result_publisher

module Limits = struct
  type t =
    { max_active_runs : int
    ; max_staged_jobs : int
    ; max_native_occupancy : int
    ; max_active_workers : int
    ; max_quarantined_workers : int
    ; max_active_inference_attempts : int
    ; max_admitted_call_bindings : int
    }

  let create
        ~max_active_runs
        ~max_staged_jobs
        ~max_native_occupancy
        ~max_active_workers
        ~max_quarantined_workers
        ~max_active_inference_attempts
        ~max_admitted_call_bindings
    =
    if
      max_active_runs <= 0
      || max_staged_jobs <= 0
      || max_native_occupancy <= 0
      || max_active_workers <= 0
      || max_quarantined_workers < max_active_workers
      || max_active_inference_attempts <= 0
      || max_admitted_call_bindings <= 0
      || max_active_runs > 4096
      || max_staged_jobs > 16384
      || max_native_occupancy > 16384
      || max_active_workers > 16384
      || max_quarantined_workers > 16384
      || max_active_inference_attempts > 256
      || max_admitted_call_bindings > 16384
    then Or_error.error_string "invalid developer root owner limits"
    else
      Ok
        { max_active_runs
        ; max_staged_jobs
        ; max_native_occupancy
        ; max_active_workers
        ; max_quarantined_workers
        ; max_active_inference_attempts
        ; max_admitted_call_bindings
        }
  ;;
end

type state =
  | Live
  | Closing
  | Quarantined
  | Closed

type call =
  { mutable cancel : Eio.Cancel.t option
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  }

type final_admission =
  actor:Operator_authorization.t
  -> capture:Chatml_developer_capture.t
  -> checkpoint:(unit -> unit)
  -> (unit -> (unit, P.Error.t) result, P.Error.t) result

type admission =
  { mutable call : call
  ; mutable claim : Store.Run_claim.t option
  ; mutable worker : Developer_job_worker.t option
  ; mutable recovery : Store.Recovery.t option
  ; mutable tracking : Store.Tracking.t option
  ; actor : Operator_authorization.t
  ; capture : Chatml_developer_capture.t
  ; registry : Chat_response.Tool_capability.t
  ; selected : Chat_response.Tool_capability.t
  ; selected_model : Provider_operator_port.Model_selection.t option
  ; job_policy : Chat_response.One_off_request.policy
  ; handle : Store.handle
  ; workspace_conflict_domain : string
  ; final_admission : final_admission option
  ; mutable running : bool
  }

type registry =
  { store : Store.t
  ; capacity : Job_capacity.t
  ; now : unit -> P.Timestamp.t
  ; limits : Limits.t
  ; switch : Eio.Switch.t
  ; domain_id : int
  ; owners : (P.Id.Developer_run.t, slot) Hashtbl.t
  ; mutable closed : bool
  }

and slot =
  | Admitting of admission
  | Owned of t

and t =
  { registry : registry
  ; actor : Operator_authorization.t
  ; capture : Chatml_developer_capture.t
  ; owner : P.Execution_owner.developer
  ; handle : Store.handle
  ; run_claim : Store.Run_claim.t
  ; tracking : Store.Tracking.t
  ; mutable tracking_finished : bool
  ; worker : Developer_job_worker.t
  ; project_source : Chatml_source.Id.t
  ; workspace_conflict_domain : string
  ; job_policy : Chat_response.One_off_request.policy
  ; native_ceiling : Capability.t
  ; mutable selected : Capability.t
  ; selected_model : Provider_operator_port.Model_selection.t option
  ; mutable selection_revision : int
  ; mutable state : state
  ; mutable root_call : call option
  ; mutable active : action option
  ; native_calls : (P.Id.Invocation.t, native_registration) Hashtbl.t
  ; mutable unresolved_native : native_registration list
  ; terminals : terminal_intent Terminal_queue.t
  ; mutation_gate : Mutation_gate.t
  ; mutable pending : pending option
  ; mutable control_pending : control_pending option
  ; mutable control_call : call option
  ; mutable dispatch_owner : native_scope option
  ; mutable external_model : external_model_slot option
  ; mutable effect_revision : int
  ; mutable reconciling : bool
  ; mutable inference_scopes : inference_scope list
  ; mutable inference_routes : inference_route list
  ; mutable inference_active : int
  ; mutable inference_bindings : int
  ; mutable inference_serial : int
  ; mutable result_binding : job_result_binding
  }

and job_result_binding =
  | Result_unbound
  | Result_binding
  | Result_bound of job_result_owner

and job_result_owner =
  { result_issuer : t
  ; result_publisher : Publisher.t
  ; result_storage : Publisher.Storage.t
  }

and inference_scope =
  { inference_issuer : t
  ; inference_origin : inference_origin
  ; inference_active_worker : Job_scope.active
  ; inference_parent : Job_scope.Parent.t
  ; inference_context : Inference_runtime.Context.t
  ; inference_selected_model : Provider_operator_port.Model_selection.t
  ; inference_candidate_limit : int
  ; mutable inference_live : bool
  ; mutable inference_exit_reason :
      Inference.Observation.Attempt_record.interruption option
  ; mutable inference_switch : Eio.Switch.t option
  ; mutable inference_calls : inference_call list
  ; mutable inference_issued : inference_route list
  }

and inference_origin =
  | Claimed_model_job
  | Claimed_saved_agent of
      { selected : Agent_session.Captured_agent_runner.Claimed_developer.Selected.t
      ; terminal : native_scope
      }

and inference_call =
  { inference_call_parent : Transcript.Scope.parent
  ; inference_call_registration : Store.Tracking.Registration.t
  }

and inference_route =
  { inference_scope : inference_scope
  ; inference_claim : Store.Claim.t
  ; inference_configuration : Inference.Observation.Configuration.t
  ; mutable inference_registration : Store.Tracking.Registration.t option
  ; mutable inference_attempt : Inference_runtime.Attempt.t option
  ; mutable inference_started : bool
  ; mutable inference_pending : Store.Tracking.Pending.t option
  ; mutable inference_pending_change : Store.Tracking.change option
  ; mutable inference_observation : Inference.Observation.t option
  ; mutable inference_completion : Store.Tracking.change option
  ; mutable inference_terminal : bool
  }

and action =
  { issuer : t
  ; snapshot : current
  ; staged : Staging.t
  ; mutable stage_in_flight : int
  ; mutable sealed : selection option
  ; mutable live : bool
  ; mutable callback_switch : Eio.Switch.t option
  ; mutable queued : bool
  ; call : call
  ; terminal_slot : terminal_intent Terminal_queue.callback
  }

and selection =
  { issuer : action
  ; jobs : P.Developer_job.t list
  ; selected_ids : P.Id.Job.t list
  ; staged_selection : Staging.Selection.t
  }

and current =
  { issuer : t
  ; selected : Capability.t
  ; selection_revision : int
  ; effect_revision : int
  ; terminal_witness : Terminal_queue.witness
  }

and pending =
  { prepared : Store.Prepared.t
  ; handoff : Store.Handoff.t
  ; drain : terminal_intent Terminal_queue.drain
  }

and control_pending =
  { prepared : Store.Prepared.t
  ; job : P.Developer_job.t
  ; witness : current
  }

and external_model_source =
  | Root_model of action
  | Native_model of native_scope
  | Claimed_model of inference_scope

and external_model_slot =
  | Model_preparing of
      { token : unit ref
      ; source : external_model_source
      }
  | Model_reserved of
      { token : unit ref
      ; source : external_model_source
      ; job : P.Developer_job.t
      ; reservation : Job_capacity.reservation
      }
  | Model_pending of
      { token : unit ref
      ; source : external_model_source
      ; transaction : Model_transaction.t
      }

and native_registration =
  { call : call
  ; invocation : H.t
  ; job_parent : Job_scope.Parent.t option
  ; terminal_slot : terminal_intent Terminal_queue.callback
  ; mutable scope : native_scope option
  }

and native_scope =
  { issuer : t
  ; registration : native_registration
  ; action : action
  ; mutable dispatched : H.t option
  ; mutable callback_switch : Eio.Switch.t option
  ; mutable callback_complete : bool
  ; mutable bracket_exited : bool
  ; mutable sealed : native_selection option
  ; mutable submitted : bool
  ; mutable dispatch_state : dispatch_state
  ; mutable remembered_terminal : H.t option
  }

and dispatch_state =
  | No_attempt
  | Dispatch_pending of
      { prepared : Store.Prepared.t
      ; handoff : Store.Handoff.t
      ; dispatched : H.t
      }
  | Dispatch_installed of H.t * int

and native_selection =
  { scope : native_scope
  ; selection : selection
  ; outcome : P.Invocation.outcome
  }

and terminal_intent =
  { plan : terminal_plan
  ; mutable settlement : Store.Settlement.t option
  }

and terminal_plan =
  | Root_terminal of
      { action : action
      ; selection : selection
      ; phase : Store.Checkpoint.phase
      }
  | Native_terminal of
      { scope : native_scope
      ; terminal : H.t
      ; selected : native_selection option
      }

type run = t

type claim_preflight =
  { preflight_root : run
  ; preflight_selected : Capability.t
  ; preflight_selection_revision : int
  }

let action_key : action Eio.Fiber.key = Eio.Fiber.create_key ()
let root_call_key : t Eio.Fiber.key = Eio.Fiber.create_key ()
let native_call_key : t Eio.Fiber.key = Eio.Fiber.create_key ()
let control_call_key : t Eio.Fiber.key = Eio.Fiber.create_key ()
let admission_key : registry Eio.Fiber.key = Eio.Fiber.create_key ()
let inference_route_key : inference_route Eio.Fiber.key = Eio.Fiber.create_key ()
let inference_scope_key : inference_scope Eio.Fiber.key = Eio.Fiber.create_key ()
let invalid message = Error (P.Error.invalid_request message)
let ( let* ) result f = Result.bind result ~f

module Action_cleanup = struct
  type t =
    | Aborted
    | Physically_owned
  [@@deriving sexp_of]
end

let same_domain registry = Int.equal registry.domain_id (Stdlib.Domain.self () :> int)

let error_of_store error =
  let code, message, retryable =
    match error with
    | Store.Error.Conflict | Prewrite_conflict ->
      P.Error.Conflict, "developer run changed before admission", true
    | Foreign_or_stale -> P.Error.Lease_stale, "developer run owner is stale", false
    | Limit_exceeded -> P.Error.Resource_limit, "developer run limit was reached", false
    | Closed -> P.Error.Invalid_state, "developer run store is closed", false
    | Store _ -> P.Error.Persistence_error, "developer run store is unavailable", true
  in
  P.Error.create code ~message ~retryable ()
;;

let error_of_terminal_queue = function
  | Terminal_queue.Error.Closed | Stale ->
    P.Error.invalid_request "developer terminal scope is closed or stale"
  | Busy ->
    P.Error.create
      P.Error.Conflict
      ~message:"developer terminal settlement is active"
      ~retryable:true
      ()
  | Limit_exceeded ->
    P.Error.create
      P.Error.Resource_limit
      ~message:"developer native invocation occupancy is full"
      ~retryable:false
      ()
;;

let selected_within ~registry selected =
  List.fold_result (Capability.references selected) ~init:() ~f:(fun () reference ->
    Capability.resolve registry ~id:reference.id ~fingerprint:reference.fingerprint
    |> Result.map ~f:ignore
    |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message))
;;

let project_source capture =
  let prepared = Chatml_developer_capture.prepared capture in
  match Chatml_prepared_program.provenance prepared with
  | Project { declaration; lock = _ } -> declaration
  | Implicit -> Chatml_prepared_program.entry prepared
;;

let owner t = t.owner
let capture t = t.capture
let native_ceiling t = t.native_ceiling
let native_selected t = t.selected
let selected_model t = t.selected_model
let original_actor t = t.actor
let job_policy t = t.job_policy
let issued_by_store t store = Store.issued_by store t.handle

let result_root t =
  if not (same_domain t.registry)
  then invalid "developer result root belongs to another domain"
  else Store.Result_root.of_handle t.handle |> Result.map_error ~f:error_of_store
;;

let document_limits t =
  if not (same_domain t.registry)
  then invalid "developer document limits belong to another domain"
  else Store.document_limits t.handle |> Result.map_error ~f:error_of_store
;;

module Job_result = struct
  type nonrec t = job_result_owner

  let issued_by_root t root = phys_equal t.result_issuer root

  let bind issuer ~blobs ~limits ~storage =
    if not (same_domain issuer.registry)
    then invalid "developer result owner belongs to another domain"
    else if issuer.registry.closed
    then invalid "developer result owner registry is closed"
    else (
      match issuer.state, issuer.result_binding with
      | Live, Result_unbound ->
        issuer.result_binding <- Result_binding;
        let installed = ref false in
        Exn.protect
          ~finally:(fun () ->
            if not !installed
            then (
              match issuer.result_binding with
              | Result_binding -> issuer.result_binding <- Result_unbound
              | Result_unbound | Result_bound _ -> ()))
          ~f:(fun () ->
            let* root = result_root issuer in
            let* publisher =
              Publisher.create
                ~sw:issuer.registry.switch
                ~runs:issuer.registry.store
                ~root
                ~blobs
                ~limits
            in
            match issuer.state, issuer.result_binding with
            | Live, Result_binding when not issuer.registry.closed ->
              let owner =
                { result_issuer = issuer
                ; result_publisher = publisher
                ; result_storage = storage
                }
              in
              issuer.result_binding <- Result_bound owner;
              installed := true;
              Ok owner
            | Live, Result_binding
            | Live, Result_unbound
            | Live, Result_bound _
            | Closing, _
            | Quarantined, _
            | Closed, _ -> invalid "developer result owner admission ended")
      | Live, Result_binding | Live, Result_bound _ ->
        Error
          (P.Error.create
             P.Error.Conflict
             ~message:"developer result owner is already bound"
             ~retryable:true
             ())
      | Closing, _ | Quarantined, _ | Closed, _ ->
        invalid "developer result owner is unavailable")
  ;;

  let select t ~claim ~proposed =
    let issuer = t.result_issuer in
    if not (same_domain issuer.registry)
    then invalid "developer result selection belongs to another domain"
    else (
      let job = Store.Claim.job claim in
      if
        (not (Store.Claim.issued_by claim issuer.registry.store))
        || (not
              (P.Execution_owner.equal_developer (P.Developer_job.owner job) issuer.owner))
        || not
             (Chatml_source.Digest.equal
                (P.Developer_job.capture_fingerprint job)
                (Chatml_developer_capture.fingerprint issuer.capture))
      then invalid "developer result claim belongs to another root"
      else (
        match proposed.P.Developer_job.State.result with
        | None -> Ok None
        | Some (P.Developer_job.Result_value.Artifact _) ->
          invalid "developer result was already selected as an artifact"
        | Some (P.Developer_job.Result_value.Inline _) ->
          let* selection =
            Publisher.select
              t.result_publisher
              ~storage:t.result_storage
              ~claim
              ~proposed
              ~selected_at:(issuer.registry.now ())
          in
          let* wrapped =
            Developer_job_worker.Selected.create
              ~claim
              ~proposed
              ~after:(Publisher.Selected.after selection)
              ~ensure_attempted:(fun () -> Publisher.Selected.ensure_attempted selection)
              ~acknowledge_installed:(fun installed ->
                Publisher.Selected.acknowledge_installed selection installed)
          in
          Ok (Some wrapped)))
  ;;
end

let native_now t = t.registry.now ()

let external_model_token = function
  | Model_preparing { token; _ }
  | Model_reserved { token; _ }
  | Model_pending { token; _ } -> token
;;

let owns_external_model t token =
  Option.value_map t.external_model ~default:false ~f:(fun slot ->
    phys_equal (external_model_token slot) token)
;;

let check_current_local
      ?control
      ?external_model
      ?(allow_tracking = false)
      t
      (witness : current)
      ~allow_pending
  =
  if
    (not (phys_equal witness.issuer t))
    || (not (same_domain t.registry))
    || t.registry.closed
    || (not (Int.equal witness.selection_revision t.selection_revision))
    || (not (Int.equal witness.effect_revision t.effect_revision))
    || (not (phys_equal witness.selected t.selected))
    || (not (Terminal_queue.check_effect_witness t.terminals witness.terminal_witness))
    || ((not allow_pending) && Option.is_some t.pending)
    || ((not allow_tracking) && Option.is_some (Store.Tracking.pending t.tracking))
    || (match t.control_pending, control with
        | None, None -> false
        | Some pending, Some allowed when phys_equal pending allowed -> false
        | None, Some _ | Some _, None | Some _, Some _ -> true)
    || (match t.external_model, external_model with
        | None, None -> false
        | Some slot, Some allowed -> not (phys_equal (external_model_token slot) allowed)
        | None, Some _ | Some _, None -> true)
    || Option.is_some t.dispatch_owner
    || t.reconciling
    || (not (Operator_authorization.is_current t.actor))
    ||
    match t.state with
    | Live -> false
    | Closing | Quarantined | Closed -> true
  then invalid "developer run authority changed"
  else Ok ()
;;

let check_effect_now t witness = check_current_local t witness ~allow_pending:false

let restore_live_after_retention t =
  if
    (match t.state with
     | Quarantined -> true
     | Live | Closing | Closed -> false)
    && Option.is_none t.pending
    && Option.is_none t.control_pending
    && Option.is_none t.dispatch_owner
    && Option.is_none t.external_model
    && Option.is_none (Store.Tracking.pending t.tracking)
    && List.is_empty t.unresolved_native
    && Option.is_some (Terminal_queue.effect_witness t.terminals)
  then t.state <- Live
;;

let current_local ?external_model ?(allow_tracking = false) t =
  if not (same_domain t.registry)
  then invalid "developer run owner belongs to another domain"
  else (
    match Terminal_queue.effect_witness t.terminals with
    | None -> invalid "developer terminal settlement excludes effects"
    | Some terminal_witness ->
      let witness =
        { issuer = t
        ; selected = t.selected
        ; selection_revision = t.selection_revision
        ; effect_revision = t.effect_revision
        ; terminal_witness
        }
      in
      Result.map
        (check_current_local
           ?external_model
           ~allow_tracking
           t
           witness
           ~allow_pending:false)
        ~f:(fun () -> witness))
;;

let current_with ?external_model t =
  if not (same_domain t.registry)
  then invalid "developer run owner belongs to another domain"
  else
    let* active = Store.is_latest_active t.handle |> Result.map_error ~f:error_of_store in
    if not active
    then invalid "developer run epoch is not active"
    else current_local ?external_model t
;;

let current t = current_with t

let check_inference_scope_local (scope : inference_scope) =
  let issuer = scope.inference_issuer in
  let callback_live =
    match scope.inference_switch with
    | None -> true
    | Some sw ->
      Option.is_none (Eio.Switch.get_error sw)
      && Option.value_map
           (Eio.Fiber.get inference_scope_key)
           ~default:false
           ~f:(phys_equal scope)
  in
  if
    (not (same_domain issuer.registry)) || (not scope.inference_live) || not callback_live
  then invalid "developer inference callback has exited"
  else
    let* selection =
      match issuer.selected_model with
      | Some selection when phys_equal selection scope.inference_selected_model ->
        Ok selection
      | None | Some _ -> invalid "developer inference selection changed"
    in
    let* () =
      Job_scope.Parent.check_now
        scope.inference_parent
        ~owner:issuer.owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint issuer.capture)
    in
    let* () = Provider_operator_port.Model_selection.check_current selection in
    match scope.inference_origin with
    | Claimed_model_job -> Ok ()
    | Claimed_saved_agent { selected; terminal } ->
      let* () =
        Agent_session.Captured_agent_runner.Claimed_developer.Selected.check_now selected
      in
      let context = H.context terminal.registration.invocation in
      if
        phys_equal terminal.issuer issuer
        && Option.value_map
             (Eio.Fiber.get native_call_key)
             ~default:false
             ~f:(phys_equal issuer)
        && Option.value_map
             (Hashtbl.find issuer.native_calls context.id)
             ~default:false
             ~f:(phys_equal terminal.registration)
        && Option.value_map
             terminal.registration.scope
             ~default:false
             ~f:(phys_equal terminal)
      then Ok ()
      else invalid "saved Agent native callback is no longer owned"
;;

module Current = struct
  type nonrec t = current

  let selected (t : t) = t.selected
end

module Claim_preflight = struct
  type nonrec t = claim_preflight

  let selected (t : t) = t.preflight_selected
end

let job_parent t active =
  let issuer = Developer_job_worker.issuer t.worker in
  let* parent = Job_scope.parent issuer active in
  let* witness = current t in
  let capture_fingerprint = Chatml_developer_capture.fingerprint t.capture in
  let* () = Job_scope.Parent.check parent ~owner:t.owner ~capture_fingerprint in
  let* () = check_effect_now t witness in
  let* () = Job_scope.Parent.check_now parent ~owner:t.owner ~capture_fingerprint in
  Ok parent
;;

let claim_preflight t =
  Result.map (current_local t) ~f:(fun (witness : current) ->
    { preflight_root = t
    ; preflight_selected = witness.selected
    ; preflight_selection_revision = witness.selection_revision
    })
;;

let check_claim_selection t (original : claim_preflight) =
  let* latest = current t in
  if
    phys_equal original.preflight_root t
    && phys_equal original.preflight_selected latest.selected
    && Int.equal original.preflight_selection_revision latest.selection_revision
  then Ok ()
  else invalid "developer worker selection changed before claim"
;;

let with_claimed_job t ~expected ~lookup ?on_capacity_decision ?select f =
  let* original = claim_preflight t in
  Developer_job_worker.with_claimed_attempt
    t.worker
    ~expected
    ~lookup
    ?on_capacity_decision
    ~before_claim:(fun () -> check_claim_selection t original)
    ?select
    f
;;

module Worker_action = struct
  type t =
    { issuer : run
    ; children : Developer_job_worker.Children.t
    ; parent : Job_scope.Parent.t
    ; snapshot : current
    ; mutable live : bool
    }

  module Selection = struct
    type t = Developer_job_worker.Children.Selection.t
  end

  let create issuer ~active ~children =
    let* snapshot = current issuer in
    let* parent = job_parent issuer active in
    let* () = check_effect_now issuer snapshot in
    Ok { issuer; children; parent; snapshot; live = true }
  ;;

  let check_now t =
    if not t.live
    then invalid "developer worker action is no longer live"
    else
      let* latest = current_local t.issuer in
      let* () =
        if
          phys_equal latest.selected t.snapshot.selected
          && Int.equal latest.selection_revision t.snapshot.selection_revision
        then Ok ()
        else invalid "developer worker selected registry changed"
      in
      let* () = check_effect_now t.issuer latest in
      Job_scope.Parent.check_now
        t.parent
        ~owner:t.issuer.owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint t.issuer.capture)
  ;;

  let stage_background t ~request ~checkpoint =
    let issuer = t.issuer in
    let* () = check_now t in
    let* parent_id =
      let depth = Job_scope.Parent.nested_depth t.parent in
      if depth >= Int.max_value
      then invalid "developer job nesting depth overflows"
      else
        Ok (Job_scope.Parent.job_id t.parent, Job_scope.Parent.attempt t.parent, depth + 1)
    in
    let id, attempt, nested_depth = parent_id in
    let* launch =
      P.Developer_job.Launch.create
        ~parent:P.Developer_job.Launch.Run_root
        ~parent_job:(Some (id, attempt))
        ~nested_depth
    in
    let* staged =
      Developer_job_worker.Children.stage t.children ~checkpoint ~prepare:(fun () ->
        let* prepared =
          Agent_session.Developer_root_job_admission.background
            ~owner:issuer.owner
            ~capture_fingerprint:(Chatml_developer_capture.fingerprint issuer.capture)
            ~launch
            ~selected:t.snapshot.selected
            ~policy:issuer.job_policy
            ~request
            ~created_at:(issuer.registry.now ())
            ~checkpoint
        in
        let* () = check_now t in
        Ok prepared)
    in
    match check_now t with
    | Ok () -> Ok staged
    | Error error ->
      ignore
        (Developer_job_worker.Children.rollback_staged t.children staged
         : (unit, P.Error.t) result);
      Error error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      ignore
        (Developer_job_worker.Children.rollback_staged t.children staged
         : (unit, P.Error.t) result);
      Stdlib.Printexc.raise_with_backtrace exn backtrace
  ;;

  let rollback_staged t id =
    let* () = check_now t in
    Developer_job_worker.Children.rollback_staged t.children id
  ;;

  let cleanup_staged t id =
    Result.map (Developer_job_worker.Children.cleanup_staged t.children id) ~f:(function
      | Developer_job_worker.Children.Aborted -> Action_cleanup.Aborted
      | Physically_owned -> Action_cleanup.Physically_owned)
  ;;

  let seal t ~selected =
    let* () = check_now t in
    Developer_job_worker.Children.seal t.children ~selected
  ;;
end

module Job_candidate = struct
  type t =
    { issuer : run
    ; stored : Store.Scheduler_candidate.t
    ; host_job : P.Host_job.t
    }

  let job t = Store.Scheduler_candidate.job t.stored
  let host_job t = t.host_job

  let read_current issuer ~checkpoint =
    let* witness = current_local issuer in
    let* stored =
      Store.scheduler_candidates issuer.handle ~checkpoint
      |> Result.map_error ~f:error_of_store
    in
    let* () = check_effect_now issuer witness in
    let* candidates =
      List.map stored ~f:(fun stored ->
        checkpoint ();
        let job = Store.Scheduler_candidate.job stored in
        let* host_job = P.Host_job.of_developer job in
        Ok { issuer; stored; host_job })
      |> Result.all
    in
    let* () = check_effect_now issuer witness in
    Ok candidates
  ;;

  let with_claimed issuer t ?on_capacity_decision ?select f =
    if not (phys_equal issuer t.issuer)
    then invalid "developer scheduler candidate belongs to another run"
    else
      with_claimed_job
        issuer
        ~expected:(Store.Scheduler_candidate.observation t.stored)
        ~lookup:(Store.Scheduler_candidate.lookup t.stored)
        ?on_capacity_decision
        ?select
        f
  ;;

  let with_claimed_selected issuer t ?on_capacity_decision ?selection_witness ?select f =
    if not (phys_equal issuer t.issuer)
    then invalid "developer scheduler candidate belongs to another run"
    else
      let* original =
        match selection_witness with
        | None -> claim_preflight issuer
        | Some (witness : claim_preflight) ->
          if phys_equal witness.preflight_root issuer
          then Ok witness
          else invalid "developer worker selection came from another root"
      in
      Developer_job_worker.with_claimed_selected_attempt
        issuer.worker
        ~expected:(Store.Scheduler_candidate.observation t.stored)
        ~lookup:(Store.Scheduler_candidate.lookup t.stored)
        ~max_staged_jobs:issuer.registry.limits.max_staged_jobs
        ?on_capacity_decision
        ~before_claim:(fun () -> check_claim_selection issuer original)
        ?select
        (fun ~active ~sw ~children ->
           let* action = Worker_action.create issuer ~active ~children in
           Exn.protect
             ~finally:(fun () -> action.live <- false)
             ~f:(fun () -> f ~active ~action ~sw))
  ;;
end

module Job_control = struct
  type snapshot =
    { issuer : run
    ; job : P.Developer_job.t
    }

  type cancellation =
    | Already_terminal
    | Durable_cancelled
    | Claimed_cancel_requested

  type control_resolution =
    | Installed
    | Absent

  let job snapshot = snapshot.job

  let same_attempt left right =
    P.Developer_job.same_immutable left right
    && Int.equal
         (P.Developer_job.state left).attempt
         (P.Developer_job.state right).attempt
  ;;

  let owned_job t job =
    P.Execution_owner.equal_developer (P.Developer_job.owner job) t.owner
    && Chatml_source.Digest.equal
         (P.Developer_job.capture_fingerprint job)
         (Chatml_developer_capture.fingerprint t.capture)
  ;;

  let find_current t id =
    let* witness = current t in
    let* observed =
      Store.observe_latest_active t.handle |> Result.map_error ~f:error_of_store
    in
    let* () = check_effect_now t witness in
    match Store.find_job observed id with
    | None -> Ok None
    | Some job when owned_job t job -> Ok (Some { issuer = t; job })
    | Some _ -> Ok None
  ;;

  let finish_installed t (pending : control_pending) installed =
    let id = P.Developer_job.id pending.job in
    let observed = Store.Installed.observation installed in
    match Store.find_job observed id with
    | Some job
      when same_attempt pending.job job
           &&
           match (P.Developer_job.state job).status with
           | P.Job.Cancelled -> true
           | Queued
           | Running
           | Waiting_permission _
           | Waiting_completion _
           | Succeeded
           | Failed _
           | Interrupted _ -> false ->
      let* host_job = P.Host_job.of_developer job in
      Eio.Cancel.protect (fun () ->
        Job_capacity.retire_host_job t.registry.capacity host_job;
        t.control_pending <- None);
      Ok Installed
    | None | Some _ ->
      t.state <- Quarantined;
      invalid "installed developer job cancellation differs from its exact attempt"
  ;;

  let reconcile_control t (pending : control_pending) =
    match t.control_pending with
    | Some current when phys_equal current pending ->
      (match Store.reconcile_inactive_cancel t.handle pending.prepared with
       | Ok (Store.Installed installed) -> finish_installed t pending installed
       | Ok Store.Absent | Error Store.Error.Prewrite_conflict ->
         t.control_pending <- None;
         Ok Absent
       | Ok Store.Diverged ->
         t.state <- Quarantined;
         invalid "developer job cancellation diverged after an attempted write"
       | Error error -> Error (error_of_store error))
    | None | Some _ -> invalid "developer job cancellation lost its physical owner"
  ;;

  let reconcile_pending t =
    match t.control_pending with
    | None -> Ok ()
    | Some pending -> Result.map (reconcile_control t pending) ~f:(fun _ -> ())
  ;;

  let matching_job t snapshot observed =
    let id = P.Developer_job.id snapshot.job in
    match Store.find_job observed id with
    | Some job when owned_job t job && same_attempt snapshot.job job -> Ok job
    | None | Some _ -> invalid "developer job attempt changed since lookup"
  ;;

  let rec cancel_attempt t snapshot ~remaining =
    let* witness = current t in
    let* observed =
      Store.observe_latest_active t.handle |> Result.map_error ~f:error_of_store
    in
    let* job = matching_job t snapshot observed in
    let* () = check_effect_now t witness in
    match (P.Developer_job.state job).status with
    | P.Job.Succeeded | Failed _ | Cancelled | Interrupted _ -> Ok Already_terminal
    | Running | Waiting_permission _ ->
      let* outcome = Developer_job_worker.cancel_claimed t.worker ~job in
      (match outcome with
       | Developer_job_worker.Cancellation.Joined | Requested_self ->
         Ok Claimed_cancel_requested
       | No_live_claim -> invalid "claimed developer job has no live physical worker")
    | Queued | Waiting_completion _ ->
      let* () =
        if
          (P.Developer_job.state job).attempt > 0
          ||
          match (P.Developer_job.state job).status with
          | Waiting_completion _ -> true
          | Queued -> false
          | Running
          | Waiting_permission _
          | Succeeded
          | Failed _
          | Cancelled
          | Interrupted _ -> false
        then Developer_job_worker.join_ended_attempt t.worker ~job
        else Ok ()
      in
      let step =
        Mutation_gate.with_owner t.mutation_gate ~f:(fun () ->
          let* current_witness = current t in
          let* fresh =
            Store.observe_latest_active t.handle |> Result.map_error ~f:error_of_store
          in
          let* fresh_job = matching_job t snapshot fresh in
          let* () = check_effect_now t current_witness in
          match (P.Developer_job.state fresh_job).status with
          | P.Job.Succeeded | Failed _ | Cancelled | Interrupted _ ->
            Ok (`Finished Already_terminal)
          | Running | Waiting_permission _ ->
            invalid "developer job became claimed before cancellation"
          | Queued | Waiting_completion _ ->
            let lookup = Store.find_job_exact fresh (P.Developer_job.id fresh_job) in
            let* prepared =
              Store.prepare_inactive_cancel
                t.handle
                ~expected:fresh
                ~lookup
                ~now:(t.registry.now ())
              |> Result.map_error ~f:error_of_store
            in
            let pending = { prepared; job = fresh_job; witness = current_witness } in
            t.control_pending <- Some pending;
            let admission () =
              Result.is_ok
                (check_current_local
                   ~control:pending
                   t
                   current_witness
                   ~allow_pending:false)
            in
            let committed =
              match Store.commit_inactive_cancel t.handle prepared ~admit:admission with
              | value -> `Returned value
              | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
            in
            (match committed with
             | `Returned (Ok installed) ->
               Result.map (finish_installed t pending installed) ~f:(fun _ ->
                 `Finished Durable_cancelled)
             | `Returned (Error Store.Error.Prewrite_conflict) ->
               t.control_pending <- None;
               Ok `Retry
             | `Returned (Error error) ->
               (match Eio.Cancel.protect (fun () -> reconcile_control t pending) with
                | Ok Installed -> Ok (`Finished Durable_cancelled)
                | Ok Absent -> Ok `Retry
                | Error _ -> Error (error_of_store error))
             | `Raised (exn, backtrace) ->
               (try
                  ignore
                    (Eio.Cancel.protect (fun () -> reconcile_control t pending)
                     : (control_resolution, P.Error.t) result)
                with
                | _ -> ());
               Exn.raise_with_original_backtrace exn backtrace))
      in
      let* step = step in
      (match step with
       | `Finished outcome -> Ok outcome
       | `Retry ->
         if remaining <= 0
         then invalid "developer job cancellation changed before its write"
         else cancel_attempt t snapshot ~remaining:(remaining - 1))
  ;;

  let cancel_current t snapshot =
    if not (phys_equal snapshot.issuer t)
    then invalid "developer job snapshot belongs to another physical run owner"
    else if not (same_domain t.registry)
    then invalid "developer job cancellation belongs to another domain"
    else if Option.is_some t.control_call || Option.is_some t.control_pending
    then
      Error
        (P.Error.create
           P.Error.Conflict
           ~message:"developer job cancellation is already in flight or unresolved"
           ~retryable:true
           ())
    else (
      let finished, finish = Eio.Promise.create () in
      let call = { cancel = None; finished; finish } in
      t.control_call <- Some call;
      Exn.protect
        ~finally:(fun () ->
          (match t.control_call with
           | Some current when phys_equal current call -> t.control_call <- None
           | None | Some _ -> ());
          call.cancel <- None;
          Eio.Promise.resolve finish ())
        ~f:(fun () ->
          Eio.Cancel.sub (fun cancel ->
            call.cancel <- Some cancel;
            Eio.Fiber.with_binding control_call_key t (fun () ->
              cancel_attempt t snapshot ~remaining:3))))
  ;;
end

module Selection = struct
  type nonrec t = selection

  let jobs (t : t) = t.jobs
end

let capacity_key t job =
  Job_capacity.Key.developer
    ~owner:t.owner
    ~project_source:t.project_source
    ~workspace_conflict_domain:t.workspace_conflict_domain
    ~kind:(P.Developer_job.kind job)
    ~nested_depth:(P.Developer_job.Launch.nested_depth (P.Developer_job.launch job))
;;

let same_action_fiber action =
  Option.value_map (Eio.Fiber.get action_key) ~default:false ~f:(fun current ->
    phys_equal current action)
;;

let native_effect_parent_live_now (scope : native_scope) =
  same_domain scope.issuer.registry
  && (not scope.bracket_exited)
  && (not scope.callback_complete)
  && scope.action.live
  && Option.value_map scope.registration.call.cancel ~default:false ~f:(fun cancel ->
    Option.is_none (Eio.Cancel.get_error cancel))
  && Option.value_map scope.callback_switch ~default:false ~f:(fun sw ->
    Option.is_none (Eio.Switch.get_error sw))
;;

let model_source_issuer = function
  | Root_model action -> action.issuer
  | Native_model scope -> scope.issuer
  | Claimed_model scope -> scope.inference_issuer
;;

let model_source_now source ~check_parent_now =
  let issuer = model_source_issuer source in
  let capture_fingerprint = Chatml_developer_capture.fingerprint issuer.capture in
  let check_action action =
    if
      (not (same_domain issuer.registry))
      || (not (same_action_fiber action))
      || (not action.live)
      || Option.is_some action.sealed
    then invalid "developer model call is outside its physical callback"
    else Ok ()
  in
  match source with
  | Claimed_model scope ->
    let* () = check_inference_scope_local scope in
    check_parent_now ()
  | Root_model action ->
    let* () = check_action action in
    if
      Option.value_map issuer.active ~default:false ~f:(phys_equal action)
      && Option.value_map action.call.cancel ~default:false ~f:(fun cancel ->
        Option.is_none (Eio.Cancel.get_error cancel))
      && Option.value_map action.callback_switch ~default:false ~f:(fun sw ->
        Option.is_none (Eio.Switch.get_error sw))
    then Ok ()
    else invalid "developer model root action is no longer current"
  | Native_model scope ->
    let* () = check_action scope.action in
    let* () =
      if
        (not scope.callback_complete)
        && Option.is_some scope.dispatched
        && Option.is_some scope.callback_switch
        && Option.value_map
             (Hashtbl.find
                issuer.native_calls
                (H.context scope.registration.invocation).id)
             ~default:false
             ~f:(fun registration -> phys_equal registration scope.registration)
      then Ok ()
      else invalid "developer model native callback is no longer registered"
    in
    let* () =
      if native_effect_parent_live_now scope
      then Ok ()
      else invalid "developer model native parent was cancelled"
    in
    let* () =
      match scope.registration.job_parent with
      | None -> Ok ()
      | Some parent ->
        Job_scope.Parent.check_now parent ~owner:issuer.owner ~capture_fingerprint
    in
    check_parent_now ()
;;

let reserve_external_model source ~check_parent_now =
  let issuer = model_source_issuer source in
  let* () = model_source_now source ~check_parent_now in
  let* fresh = current_with issuer in
  let* () = model_source_now source ~check_parent_now in
  let* () =
    match source with
    | Claimed_model _ -> Ok ()
    | Root_model action | Native_model { action; _ } ->
      if
        phys_equal action.snapshot.selected fresh.selected
        && Int.equal action.snapshot.selection_revision fresh.selection_revision
      then Ok ()
      else invalid "developer model selection changed before admission"
  in
  if
    Option.is_some issuer.external_model
    || Option.is_some issuer.pending
    || Option.is_some issuer.control_pending
    || Option.is_some issuer.dispatch_owner
    || issuer.reconciling
  then invalid "developer run already owns a mutation"
  else if Int.equal issuer.effect_revision Int.max_value
  then invalid "developer run effect revision is exhausted"
  else (
    let token = ref () in
    issuer.external_model <- Some (Model_preparing { token; source });
    issuer.effect_revision <- issuer.effect_revision + 1;
    Ok token)
;;

let model_source_parent_check source =
  match source with
  | Root_model _ -> Ok ()
  | Claimed_model scope ->
    Job_scope.Parent.check
      scope.inference_parent
      ~owner:scope.inference_issuer.owner
      ~capture_fingerprint:
        (Chatml_developer_capture.fingerprint scope.inference_issuer.capture)
  | Native_model scope ->
    (match scope.registration.job_parent with
     | None -> Ok ()
     | Some parent ->
       Job_scope.Parent.check
         parent
         ~owner:scope.issuer.owner
         ~capture_fingerprint:(Chatml_developer_capture.fingerprint scope.issuer.capture))
;;

let model_source_launch source =
  match source with
  | Root_model _ ->
    P.Developer_job.Launch.create ~parent:Run_root ~parent_job:None ~nested_depth:0
  | Claimed_model scope ->
    let parent = scope.inference_parent in
    let depth = Job_scope.Parent.nested_depth parent in
    if Int.equal depth Int.max_value
    then invalid "developer model nesting depth overflows"
    else
      P.Developer_job.Launch.create
        ~parent:Run_root
        ~parent_job:
          (Some (Job_scope.Parent.job_id parent, Job_scope.Parent.attempt parent))
        ~nested_depth:(depth + 1)
  | Native_model scope ->
    let context = H.context scope.registration.invocation in
    let* parent_job, nested_depth =
      match scope.registration.job_parent with
      | None -> Ok (None, 0)
      | Some parent ->
        let depth = Job_scope.Parent.nested_depth parent in
        if Int.equal depth Int.max_value
        then invalid "developer model nesting depth overflows"
        else
          Ok
            ( Some (Job_scope.Parent.job_id parent, Job_scope.Parent.attempt parent)
            , depth + 1 )
    in
    P.Developer_job.Launch.create
      ~parent:(Invocation context.id)
      ~parent_job
      ~nested_depth
;;

let model_parent_in_observation source observed =
  match source with
  | Root_model _ -> Ok Store.External_model_parent.Run_root
  | Claimed_model scope ->
    let claim = Job_scope.claim scope.inference_active_worker in
    let lookup =
      Store.find_job_exact observed (Store.Claim.job claim |> P.Developer_job.id)
    in
    (match Store.Job_lookup.value lookup with
     | Some parent
       when P.Developer_job.same_immutable parent (Store.Claim.job claim)
            && Int.equal
                 (P.Developer_job.state parent).attempt
                 (Store.Claim.attempt claim)
            &&
            match (P.Developer_job.state parent).status with
            | Running -> true
            | Queued
            | Waiting_permission _
            | Waiting_completion _
            | Succeeded
            | Failed _
            | Cancelled
            | Interrupted _ -> false ->
       Ok (Store.External_model_parent.Claimed_model (claim, lookup))
     | None | Some _ -> invalid "developer model claimed parent is not Running")
  | Native_model scope ->
    let id = (H.context scope.registration.invocation).id in
    let lookup = Store.find_invocation observed id in
    (match Store.Invocation_lookup.prior lookup, scope.dispatched with
     | Some prior, Some dispatched
       when H.equal_context
              (H.context (Store.Invocation_observation.value prior))
              (H.context dispatched)
            && P.Invocation.equal_status (H.status dispatched) Dispatching
            && P.Invocation.equal_status
                 (H.status (Store.Invocation_observation.value prior))
                 Dispatching -> Ok (Store.External_model_parent.Dispatching lookup)
     | None, _ | Some _, None | Some _, Some _ ->
       invalid "developer model parent is not durably dispatching")
;;

let clear_external_model issuer token =
  if owns_external_model issuer token
  then (
    issuer.external_model <- None;
    restore_live_after_retention issuer)
;;

let abandon_external_model issuer token =
  match issuer.external_model with
  | Some (Model_preparing current) when phys_equal current.token token ->
    clear_external_model issuer token
  | Some (Model_reserved current) when phys_equal current.token token ->
    Job_capacity.abort current.reservation;
    clear_external_model issuer token
  | Some (Model_pending current) when phys_equal current.token token ->
    issuer.state <- Quarantined
  | None | Some _ -> ()
;;

let reconcile_external_model_once issuer token =
  match issuer.external_model with
  | Some (Model_pending current) when phys_equal current.token token ->
    let* settlement =
      Model_transaction.reconcile current.transaction issuer.handle
      |> Result.map_error ~f:error_of_store
    in
    (match settlement with
     | Store.Settlement.Installed _ ->
       clear_external_model issuer token;
       Ok settlement
     | Absent | Prewrite_conflict ->
       Model_transaction.abort_proven_absent current.transaction;
       clear_external_model issuer token;
       Ok settlement
     | Diverged ->
       issuer.state <- Quarantined;
       Ok settlement)
  | None | Some _ -> invalid "developer model transaction is not pending here"
;;

let admit_external_model_from_scope source ~request ~checkpoint ~check_parent_now =
  let issuer = model_source_issuer source in
  Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () ->
    let* token = reserve_external_model source ~check_parent_now in
    let model_selection = issuer.selected_model in
    let check_now witness selection =
      let* () =
        check_current_local ~external_model:token issuer witness ~allow_pending:false
      in
      let* () = Provider_operator_port.Model_selection.check_current selection in
      model_source_now source ~check_parent_now
    in
    let perform () =
      let* selection =
        match model_selection with
        | Some value -> Ok value
        | None -> invalid "developer Model.spawn has no admitted provider selection"
      in
      checkpoint ();
      let* limits =
        Store.document_limits issuer.handle |> Result.map_error ~f:error_of_store
      in
      let* _ =
        P.Model_spawn_request.admit (P.Model_spawn_request.original request) ~limits
      in
      let* launch = model_source_launch source in
      let* () = Provider_operator_port.Model_selection.check_current selection in
      let* job =
        Developer_external_model_job.create
          ~owner:issuer.owner
          ~capture_fingerprint:(Chatml_developer_capture.fingerprint issuer.capture)
          ~launch
          ~request
          ~target:(Provider_operator_port.Model_selection.target selection)
          ~limits
          ~created_at:(issuer.registry.now ())
      in
      let* witness = current_with ~external_model:token issuer in
      let* () = model_source_parent_check source in
      let* () = check_now witness selection in
      let* key = capacity_key issuer job in
      let* host_job = P.Host_job.of_developer job in
      let* reservation =
        Job_capacity.reserve_host_job issuer.registry.capacity key ~job:host_job
        |> Result.bind ~f:(function
          | Some reservation -> Ok reservation
          | None ->
            Error
              (P.Error.create
                 P.Error.Resource_limit
                 ~message:"shared job capacity is full"
                 ~retryable:true
                 ()))
      in
      issuer.external_model <- Some (Model_reserved { token; source; job; reservation });
      let rec save witness remaining =
        if remaining <= 0
        then invalid "developer model document changed repeatedly"
        else (
          checkpoint ();
          let* observed =
            Store.observe_latest_active issuer.handle
            |> Result.map_error ~f:error_of_store
          in
          let* parent = model_parent_in_observation source observed in
          let* () = model_source_parent_check source in
          let* () = check_now witness selection in
          let* prepared =
            Store.prepare_external_model issuer.handle ~expected:observed ~parent ~job
            |> Result.map_error ~f:error_of_store
          in
          let transaction = Model_transaction.create ~reservation ~prepared in
          issuer.external_model <- Some (Model_pending { token; source; transaction });
          let result =
            Model_transaction.commit transaction issuer.handle ~admit:(fun () ->
              owns_external_model issuer token
              && Result.is_ok (check_now witness selection))
          in
          let settled =
            match result with
            | Ok settlement -> Ok settlement
            | Error original ->
              (match
                 Eio.Cancel.protect (fun () ->
                   Model_transaction.reconcile transaction issuer.handle)
               with
               | Ok settlement -> Ok settlement
               | Error _ -> Error (error_of_store original))
          in
          match settled with
          | Error error ->
            issuer.state <- Quarantined;
            Error error
          | Ok (Store.Settlement.Installed _) ->
            clear_external_model issuer token;
            Ok (P.Developer_job.id job)
          | Ok (Absent | Prewrite_conflict) ->
            issuer.external_model
            <- Some (Model_reserved { token; source; job; reservation });
            let* fresh = current_with ~external_model:token issuer in
            let* () = model_source_parent_check source in
            let* () = check_now fresh selection in
            save fresh (remaining - 1)
          | Ok Diverged ->
            issuer.state <- Quarantined;
            invalid "developer model write remains unresolved")
      in
      save witness 3
    in
    match perform () with
    | Ok _ as value -> value
    | Error _ as error ->
      abandon_external_model issuer token;
      error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      Eio.Cancel.protect (fun () ->
        (match issuer.external_model with
         | Some (Model_pending current) when phys_equal current.token token ->
           (match reconcile_external_model_once issuer token with
            | Ok _ | Error _ -> ()
            | exception _ -> ())
         | None | Some _ -> ());
        abandon_external_model issuer token);
      Exn.raise_with_original_backtrace exn backtrace)
;;

module Action = struct
  type nonrec t = action

  type cleanup = Action_cleanup.t =
    | Aborted
    | Physically_owned
  [@@deriving sexp_of]

  let stage_background_with_launch t ~launch ~request ~checkpoint ~check_parent_now =
    let issuer = t.issuer in
    if (not (same_domain issuer.registry)) || (not (same_action_fiber t)) || not t.live
    then invalid "developer root action is not live in this scope"
    else if Option.is_some t.sealed
    then invalid "developer root action selection is sealed"
    else if
      Staging.count t.staged + t.stage_in_flight >= issuer.registry.limits.max_staged_jobs
    then
      Error
        (P.Error.create
           P.Error.Resource_limit
           ~message:"developer root staged job limit was reached"
           ~retryable:false
           ())
    else (
      t.stage_in_flight <- t.stage_in_flight + 1;
      Exn.protect
        ~finally:(fun () -> t.stage_in_flight <- t.stage_in_flight - 1)
        ~f:(fun () ->
          let* current_witness = current issuer in
          let* () =
            if phys_equal current_witness.selected t.snapshot.selected
            then Ok ()
            else invalid "developer action selection changed"
          in
          let* prepared =
            Agent_session.Developer_root_job_admission.background
              ~owner:issuer.owner
              ~capture_fingerprint:(Chatml_developer_capture.fingerprint issuer.capture)
              ~launch
              ~selected:t.snapshot.selected
              ~policy:issuer.job_policy
              ~request
              ~created_at:(issuer.registry.now ())
              ~checkpoint
          in
          let* () = check_effect_now issuer current_witness in
          let* () = check_parent_now () in
          if (not t.live) || Option.is_some t.sealed
          then invalid "developer root action changed during job preparation"
          else (
            let job = Agent_session.Developer_root_job_admission.Prepared.job prepared in
            let* key = capacity_key issuer job in
            Staging.reserve t.staged ~capacity:issuer.registry.capacity ~key ~prepared)))
  ;;

  let stage_background (t : t) ~request ~checkpoint =
    let* launch =
      P.Developer_job.Launch.create
        ~parent:P.Developer_job.Launch.Run_root
        ~parent_job:None
        ~nested_depth:0
    in
    stage_background_with_launch
      t
      ~launch
      ~request
      ~checkpoint
      ~check_parent_now:(fun () -> Ok ())
  ;;

  let admit_external_model (t : t) ~request ~checkpoint =
    admit_external_model_from_scope
      (Root_model t)
      ~request
      ~checkpoint
      ~check_parent_now:(fun () -> Ok ())
  ;;

  let rollback_staged t id =
    if not (same_domain t.issuer.registry)
    then invalid "developer staged job belongs to another domain"
    else if (not t.live) || not (same_action_fiber t)
    then invalid "developer staged job rollback is outside its action"
    else Staging.rollback t.staged id
  ;;

  let cleanup_staged t id =
    if not (same_domain t.issuer.registry)
    then invalid "developer staged job belongs to another domain"
    else (
      match Staging.status t.staged id with
      | None -> invalid "developer staged job is not owned by this action"
      | Some Staging.Aborted -> Ok Aborted
      | Some Staging.Published -> Ok Physically_owned
      | Some Staging.Provisional when t.queued || Option.is_some t.sealed ->
        Ok Physically_owned
      | Some Staging.Provisional when t.live ->
        Result.map (rollback_staged t id) ~f:(fun () -> Aborted)
      | Some Staging.Provisional ->
        invalid "unsealed developer action leaked provisional capacity")
  ;;

  let seal t ~selected =
    if
      (not (same_domain t.issuer.registry))
      || (not (same_action_fiber t))
      || (not t.live)
      || Option.is_some t.sealed
    then invalid "developer root action cannot be sealed"
    else if t.stage_in_flight > 0
    then invalid "developer root job preparation is still active"
    else
      let* staged_selection = Staging.seal t.staged ~selected in
      let value =
        { issuer = t
        ; jobs = Staging.Selection.jobs staged_selection
        ; selected_ids = selected
        ; staged_selection
        }
      in
      t.sealed <- Some value;
      Ok value
  ;;
end

let narrow_selection t ~names =
  if not (same_domain t.registry)
  then invalid "developer root owner belongs to another domain"
  else (
    match t.state with
    | Closing | Quarantined | Closed -> invalid "developer root owner is closed"
    | Live ->
      if not (Operator_authorization.is_current t.actor)
      then invalid "developer root actor expired"
      else
        let* selected =
          Capability.select t.selected ~names
          |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
        in
        if Int.equal t.selection_revision Int.max_value
        then invalid "developer root selection revision exhausted"
        else (
          t.selection_revision <- t.selection_revision + 1;
          t.selected <- selected;
          Ok ()))
;;

let abort_staged action =
  ignore (Staging.abort_pre_handoff action.staged : (unit, P.Error.t) result)
;;

let settle_staged action selection = function
  | Store.Settlement.Installed _ ->
    (match Staging.settle action.staged selection.staged_selection Selected_installed with
     | Ok () -> ()
     | Error _ -> failwith "developer staged settlement invariant failed")
  | Absent ->
    (match Staging.settle action.staged selection.staged_selection Absent with
     | Ok () -> ()
     | Error _ -> failwith "developer staged settlement invariant failed")
  | Prewrite_conflict | Diverged -> ()
;;

let intent_action intent =
  match intent.plan with
  | Root_terminal { action; _ } -> action
  | Native_terminal { scope; _ } -> scope.action
;;

let intent_jobs intent =
  match intent.plan with
  | Root_terminal { selection; _ } -> selection.jobs
  | Native_terminal { selected = None; _ } -> []
  | Native_terminal { selected = Some selected; _ } -> selected.selection.jobs
;;

let settle_intent intent settlement =
  match intent.plan with
  | Root_terminal { action; selection; _ } ->
    (match settlement with
     | Store.Settlement.Absent when not (List.is_empty selection.jobs) -> ()
     | Installed _ | Absent | Prewrite_conflict | Diverged ->
       settle_staged action selection settlement)
  | Native_terminal { scope; selected; _ } ->
    (match selected, settlement with
     | Some selected, Store.Settlement.Installed _ ->
       settle_staged scope.action selected.selection settlement
     | None, Installed _ -> abort_staged scope.action
     | None, Absent -> ()
     | Some _, Absent | _, (Prewrite_conflict | Diverged) -> ())
;;

type absent_disposition =
  | Retire_empty_root
  | Retain_selected_root
  | Reprepare_native

let absent_disposition intent =
  match intent.plan with
  | Root_terminal { selection; _ } ->
    if List.is_empty selection.jobs then Retire_empty_root else Retain_selected_root
  | Native_terminal _ -> Reprepare_native
;;

let same_pending t pending =
  Option.value_map t.pending ~default:false ~f:(fun current ->
    phys_equal current.prepared pending.prepared
    && phys_equal current.handoff pending.handoff
    && Terminal_queue.same_head current.drain pending.drain)
;;

let publication_allowed t intent =
  let action = intent_action intent in
  let snapshot = action.snapshot in
  let terminal_only =
    match intent.plan with
    | Native_terminal _ -> List.is_empty (intent_jobs intent)
    | Root_terminal _ -> false
  in
  same_domain t.registry
  && Option.is_none t.external_model
  && phys_equal snapshot.issuer t
  && (terminal_only
      || (Int.equal snapshot.selection_revision t.selection_revision
          && phys_equal snapshot.selected t.selected
          && Operator_authorization.is_current t.actor))
  (* Quarantine excludes fresh effects, not exact frozen terminal bookkeeping.
     The Store guard still checks this Prepared/Handoff and live authority. *)
  && (match t.state with
      | Live | Closing | Quarantined -> true
      | Closed -> false)
  &&
  match intent.plan with
  | Root_terminal { phase = Store.Checkpoint.Active; _ } -> true
  | Root_terminal { phase = Interrupted | Closed; _ } ->
    not (Terminal_queue.has_native_occupancy t.terminals)
  | Native_terminal _ -> true
;;

let prepare_intent t intent expected =
  match intent.plan with
  | Root_terminal { phase; selection; _ } ->
    Store.prepare_root_selected t.handle ~expected ~phase ~selected:selection.jobs
  | Native_terminal { terminal; _ } ->
    let id = (H.context terminal).id in
    let lookup = Store.find_invocation expected id in
    Store.prepare_terminal_selected
      t.handle
      ~expected
      ~lookup
      ~terminal
      ~selected:(intent_jobs intent)
;;

let settled_revision intent =
  match intent.settlement with
  | Some (Store.Settlement.Installed installed) ->
    (match intent.plan with
     | Root_terminal _ -> Ok 0
     | Native_terminal { terminal; _ } ->
       let observation = Store.Installed.observation installed in
       let lookup = Store.find_invocation observation (H.context terminal).id in
       (match Store.Invocation_lookup.prior lookup with
        | Some entry -> Ok (Store.Invocation_observation.revision entry)
        | None -> invalid "installed native terminal is missing"))
  | Some Absent | Some Prewrite_conflict | Some Diverged | None ->
    invalid "developer terminal intent has no durable result"
;;

let retain_drain t drain =
  Terminal_queue.retain t.terminals drain |> Result.map_error ~f:error_of_terminal_queue
;;

let settle_drain t drain =
  let* () =
    Terminal_queue.settle t.terminals drain |> Result.map_error ~f:error_of_terminal_queue
  in
  restore_live_after_retention t;
  Ok ()
;;

let with_retained_head t drain f =
  let retain () = ignore (retain_drain t drain : (unit, P.Error.t) result) in
  match f () with
  | Ok _ as result -> result
  | Error _ as result ->
    Eio.Cancel.protect retain;
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect retain;
    Exn.raise_with_original_backtrace exn backtrace
;;

let rec process_head t drain remaining =
  let intent = Terminal_queue.payload drain in
  if Option.is_some t.external_model
  then
    let* () = retain_drain t drain in
    invalid "developer model admission blocks terminal preparation"
  else if Option.is_some t.dispatch_owner
  then
    let* () = retain_drain t drain in
    invalid "developer dispatch blocks terminal preparation"
  else if remaining <= 0
  then
    let* () = retain_drain t drain in
    invalid "developer terminal transaction changed repeatedly"
  else
    let* expected = Store.observe t.handle |> Result.map_error ~f:error_of_store in
    let* prepared =
      prepare_intent t intent expected |> Result.map_error ~f:error_of_store
    in
    let handoff = Store.Handoff.create ~settle:(settle_intent intent) in
    let pending = { prepared; handoff; drain } in
    t.pending <- Some pending;
    let admit () = same_pending t pending && publication_allowed t intent in
    let result =
      match Store.commit_and_settle t.handle prepared handoff ~admit with
      | value -> `Returned value
      | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let finish settlement =
      match settlement with
      | Store.Settlement.Installed _ ->
        t.pending <- None;
        intent.settlement <- Some settlement;
        settle_drain t drain
      | Absent ->
        t.pending <- None;
        (match absent_disposition intent with
         | Retire_empty_root ->
           intent.settlement <- Some settlement;
           settle_drain t drain
         | Retain_selected_root ->
           let* () = retain_drain t drain in
           invalid "selected root checkpoint was not admitted"
         | Reprepare_native -> process_head t drain (remaining - 1))
      | Prewrite_conflict ->
        t.pending <- None;
        process_head t drain (remaining - 1)
      | Diverged ->
        t.state <- Quarantined;
        let* () = retain_drain t drain in
        invalid "developer terminal transaction remains quarantined"
    in
    match result with
    | `Returned (Ok settlement) -> finish settlement
    | `Returned (Error original) ->
      (match Store.reconcile_and_settle t.handle prepared handoff with
       | Ok settlement ->
         (match finish settlement with
          | Ok () -> Ok ()
          | Error _ -> Error (error_of_store original))
       | Error _ ->
         let* () = retain_drain t drain in
         Error (error_of_store original))
    | `Raised (exn, backtrace) ->
      Eio.Cancel.protect (fun () ->
        ignore (retain_drain t drain : (unit, P.Error.t) result));
      Exn.raise_with_original_backtrace exn backtrace
;;

let drain_terminals_unlocked t =
  let rec loop remaining =
    if Option.is_some t.external_model
    then invalid "developer model admission blocks terminal drain"
    else if Option.is_some t.dispatch_owner
    then invalid "developer dispatch blocks terminal drain"
    else if remaining <= 0
    then invalid "developer terminal drain exceeded its occupancy bound"
    else (
      match Terminal_queue.take_head t.terminals with
      | Error error -> Error (error_of_terminal_queue error)
      | Ok None -> Ok ()
      | Ok (Some drain) ->
        let* () =
          with_retained_head t drain (fun () ->
            match t.pending with
            | None -> process_head t drain 3
            | Some pending when Terminal_queue.same_head pending.drain drain ->
              (match
                 Store.reconcile_and_settle t.handle pending.prepared pending.handoff
               with
               | Error error ->
                 let* () = retain_drain t drain in
                 Error (error_of_store error)
               | Ok settlement ->
                 (* Exact retained reconciliation has the same classification as
                  a fresh commit; there is no callback or application replay. *)
                 (match settlement with
                  | Installed _ ->
                    t.pending <- None;
                    let intent = Terminal_queue.payload drain in
                    intent.settlement <- Some settlement;
                    settle_drain t drain
                  | Absent ->
                    t.pending <- None;
                    let intent = Terminal_queue.payload drain in
                    (match absent_disposition intent with
                     | Retire_empty_root ->
                       intent.settlement <- Some settlement;
                       settle_drain t drain
                     | Retain_selected_root ->
                       let* () = retain_drain t drain in
                       invalid "selected root checkpoint was not admitted"
                     | Reprepare_native -> process_head t drain 3)
                  | Prewrite_conflict ->
                    t.pending <- None;
                    process_head t drain 3
                  | Diverged ->
                    t.state <- Quarantined;
                    let* () = retain_drain t drain in
                    invalid "developer terminal transaction remains quarantined"))
            | Some _ ->
              let* () = retain_drain t drain in
              invalid "developer terminal pending owner is mismatched")
        in
        loop (remaining - 1))
  in
  loop (t.registry.limits.max_native_occupancy + 2)
;;

let drain_terminals t =
  (* Completed callbacks may arrive while another fiber is yielding in a Store
     transaction. The queue retains exact intents; one physical drainer owns
     their ordered settlement until it releases this gate. *)
  Mutation_gate.with_owner t.mutation_gate ~f:(fun () -> drain_terminals_unlocked t)
;;

let drain_and_settle_unlocked t intent =
  let drained = drain_terminals_unlocked t in
  match intent.settlement with
  | Some _ -> settled_revision intent
  | None ->
    let* () = drained in
    settled_revision intent
;;

let with_root_action t ~phase f =
  if not (same_domain t.registry)
  then invalid "developer root owner belongs to another domain"
  else if
    Option.is_some t.root_call
    || Option.is_some t.pending
    || Option.is_some t.control_pending
    || Option.is_some t.external_model
    || Option.is_some t.dispatch_owner
    || t.reconciling
  then invalid "developer root action is already active or unresolved"
  else
    let* terminal_slot =
      Terminal_queue.reserve t.terminals ~kind:Root
      |> Result.map_error ~f:error_of_terminal_queue
    in
    let finished, finish = Eio.Promise.create () in
    let call = { cancel = None; finished; finish } in
    t.root_call <- Some call;
    Exn.protect
      ~finally:(fun () ->
        Option.iter t.active ~f:(fun action ->
          if phys_equal action.call call
          then (
            action.live <- false;
            if not action.queued then abort_staged action));
        ignore (Terminal_queue.abandon t.terminals terminal_slot : (unit, _) result);
        call.cancel <- None;
        t.active <- None;
        t.root_call <- None;
        Eio.Promise.resolve finish ())
      ~f:(fun () ->
        Eio.Cancel.sub (fun cancel ->
          call.cancel <- Some cancel;
          Eio.Fiber.with_binding root_call_key t (fun () ->
            let* snapshot = current t in
            let* staged =
              Staging.create ~max_staged_jobs:t.registry.limits.max_staged_jobs
            in
            let action =
              { issuer = t
              ; snapshot
              ; staged
              ; stage_in_flight = 0
              ; sealed = None
              ; live = true
              ; callback_switch = None
              ; queued = false
              ; call
              ; terminal_slot
              }
            in
            t.active <- Some action;
            let* value, (selection : selection) =
              Exn.protect
                ~finally:(fun () -> action.callback_switch <- None)
                ~f:(fun () ->
                  Eio.Switch.run (fun sw ->
                    action.callback_switch <- Some sw;
                    Eio.Fiber.with_binding action_key action (fun () -> f ~action ~sw)))
            in
            if
              (not (phys_equal selection.issuer action))
              || (not
                    (Option.value_map
                       action.sealed
                       ~default:false
                       ~f:(phys_equal selection)))
              || action.stage_in_flight > 0
            then invalid "developer root action returned a foreign selection"
            else
              Mutation_gate.with_owner t.mutation_gate ~f:(fun () ->
                let* () = drain_terminals_unlocked t in
                let* latest = current t in
                let* () =
                  if phys_equal latest.selected snapshot.selected
                  then Ok ()
                  else invalid "developer root selection changed during callback"
                in
                let* () =
                  match phase, selection.jobs with
                  | Store.Checkpoint.Active, _ | (Interrupted | Closed), [] -> Ok ()
                  | (Interrupted | Closed), _ :: _ ->
                    invalid "closed developer epoch cannot select queued jobs"
                in
                let* () =
                  match phase with
                  | Store.Checkpoint.Active -> Ok ()
                  | Interrupted | Closed ->
                    if Terminal_queue.has_native_occupancy t.terminals
                    then invalid "native invocations must finish before the epoch closes"
                    else Ok ()
                in
                let intent =
                  { plan = Root_terminal { action; selection; phase }; settlement = None }
                in
                let* () =
                  Terminal_queue.complete t.terminals terminal_slot ~intent
                  |> Result.map_error ~f:error_of_terminal_queue
                in
                action.queued <- true;
                let* () = Staging.mark_handoff action.staged selection.staged_selection in
                let* _ = drain_and_settle_unlocked t intent in
                Ok value))))
;;

module Native_terminal = struct
  type nonrec t = native_scope

  let issued_by_root (t : t) ~root = phys_equal t.issuer root
  let invocation t = t.registration.invocation

  type dispatch_reconciliation =
    | Installed of H.t * int
    | Absent
    | Uncertain

  module Selection = struct
    type nonrec t = native_selection

    let jobs t = t.selection.jobs
  end

  let callback_complete (t : t) = t.callback_complete
  let effect_parent_live_now = native_effect_parent_live_now

  let check_completion_now (scope : t) =
    let issuer = scope.issuer in
    let context = H.context scope.registration.invocation in
    let dispatched =
      match scope.dispatched, scope.dispatch_state with
      | Some active, Dispatch_installed (installed, _)
        when H.equal_context (H.context active) context
             && H.equal_context (H.context installed) context
             && P.Invocation.equal_status (H.status active) Dispatching -> true
      | None, _ | Some _, (No_attempt | Dispatch_pending _ | Dispatch_installed _) ->
        false
    in
    if
      (not (same_domain issuer.registry))
      || issuer.registry.closed
      || (match issuer.state with
          | Live -> false
          | Closing | Quarantined | Closed -> true)
      || scope.bracket_exited
      || scope.callback_complete
      || scope.submitted
      || Option.is_some scope.sealed
      || (not dispatched)
      || (not (native_effect_parent_live_now scope))
      || (not (same_action_fiber scope.action))
      || (not (phys_equal scope.action.snapshot.issuer issuer))
      || (not (phys_equal scope.action.snapshot.selected issuer.selected))
      || (not
            (Int.equal scope.action.snapshot.selection_revision issuer.selection_revision))
      || (not (Operator_authorization.is_current issuer.actor))
      || (not
            (Option.value_map
               (Hashtbl.find issuer.native_calls context.id)
               ~default:false
               ~f:(phys_equal scope.registration)))
      || not
           (Option.value_map
              scope.registration.scope
              ~default:false
              ~f:(phys_equal scope))
    then invalid "native callback completion is no longer current"
    else Ok ()
  ;;

  let sealed (t : t) =
    Option.map t.sealed ~f:(fun (selected : native_selection) ->
      selected.outcome, selected)
  ;;

  let dispatch_revision observed id =
    match Store.Invocation_lookup.prior (Store.find_invocation observed id) with
    | Some prior -> Ok (Store.Invocation_observation.revision prior)
    | None -> invalid "installed developer dispatch is missing"
  ;;

  let read_dispatch (t : t) id =
    let issuer = t.issuer in
    let expected = H.context t.registration.invocation in
    if
      (not (same_domain issuer.registry))
      || t.bracket_exited
      || t.submitted
      || not (P.Id.Invocation.equal id expected.id)
    then invalid "native dispatch read is outside its physical scope"
    else
      let* observed = Store.observe issuer.handle |> Result.map_error ~f:error_of_store in
      Ok
        (Option.map
           (Store.Invocation_lookup.prior (Store.find_invocation observed id))
           ~f:(fun prior ->
             ( Store.Invocation_observation.revision prior
             , Store.Invocation_observation.value prior )))
  ;;

  let dispatch_allowed (t : t) (witness : current) =
    let issuer = t.issuer in
    let snapshot = t.action.snapshot in
    Option.value_map issuer.dispatch_owner ~default:false ~f:(phys_equal t)
    && Option.is_none issuer.pending
    && Option.is_none issuer.control_pending
    && Option.is_none issuer.external_model
    && (match issuer.state with
        | Live -> true
        | Closing | Quarantined | Closed -> false)
    && (not issuer.registry.closed)
    && Operator_authorization.is_current issuer.actor
    && Int.equal snapshot.selection_revision issuer.selection_revision
    && phys_equal snapshot.selected issuer.selected
    && phys_equal witness.selected issuer.selected
    && Int.equal witness.selection_revision issuer.selection_revision
    && Terminal_queue.check_effect_witness issuer.terminals witness.terminal_witness
  ;;

  let mark_installed (t : t) dispatched installed =
    let issuer = t.issuer in
    match issuer.dispatch_owner with
    | Some current when phys_equal current t ->
      let observed = Store.Installed.observation installed in
      let* revision = dispatch_revision observed (H.context dispatched).id in
      t.dispatch_state <- Dispatch_installed (dispatched, revision);
      issuer.dispatch_owner <- None;
      Ok revision
    | None | Some _ ->
      issuer.state <- Quarantined;
      invalid "installed native dispatch lost its physical writer ownership"
  ;;

  let absent (t : t) =
    t.dispatch_state <- No_attempt;
    (match t.issuer.dispatch_owner with
     | Some current when phys_equal current t -> t.issuer.dispatch_owner <- None
     | None | Some _ -> ());
    Ok Absent
  ;;

  let reconcile_failed_save (t : t) ~prepared ~handoff ~dispatched =
    let issuer = t.issuer in
    (* The exact pending pair remains protected by the Root mutation gate.
       Settle a decidable write before another terminal observes this writer;
       an unknown result keeps its physical owner for later recovery. *)
    let settle () =
      match Store.reconcile_and_settle issuer.handle prepared handoff with
      | Ok (Store.Settlement.Installed installed) ->
        (match mark_installed t dispatched installed with
         | Ok _ -> ()
         | Error _ -> issuer.state <- Quarantined)
      | Ok (Absent | Prewrite_conflict) ->
        ignore (absent t : (dispatch_reconciliation, _) result)
      | Ok Diverged -> issuer.state <- Quarantined
      | Error _ -> ()
    in
    (* Cleanup may itself fail, but it must never replace the original Store
       failure or cancellation. Its pending pair remains available to the
       ordinary exited-bracket reconciliation path. *)
    try Eio.Cancel.protect settle with
    | _ -> ()
  ;;

  let save_dispatch (t : t) ~expected_revision ~dispatched =
    let issuer = t.issuer in
    let original = t.registration.invocation in
    Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () ->
      if
        (not (same_domain issuer.registry))
        || t.bracket_exited
        || t.submitted
        || Option.is_some issuer.dispatch_owner
        || Option.is_some issuer.pending
        || Option.is_some issuer.control_pending
        || Option.is_some issuer.external_model
        || issuer.reconciling
        || (not (phys_equal t.action.snapshot.issuer issuer))
        || (not (H.equal_context (H.context original) (H.context dispatched)))
        || not (P.Invocation.equal_status (H.status dispatched) Dispatching)
      then invalid "native dispatch cannot enter this physical scope"
      else
        let* () = drain_terminals_unlocked issuer in
        let* fresh = current issuer in
        let* () =
          if
            phys_equal fresh.selected t.action.snapshot.selected
            && Int.equal fresh.selection_revision t.action.snapshot.selection_revision
          then Ok ()
          else invalid "native dispatch selection changed before its Store write"
        in
        if Int.equal issuer.effect_revision Int.max_value
        then invalid "developer effect generation is exhausted"
        else
          let* observed =
            Store.observe issuer.handle |> Result.map_error ~f:error_of_store
          in
          let lookup = Store.find_invocation observed (H.context dispatched).id in
          let actual_revision =
            Option.map
              (Store.Invocation_lookup.prior lookup)
              ~f:Store.Invocation_observation.revision
          in
          let* () =
            if Option.equal Int.equal actual_revision expected_revision
            then Ok ()
            else invalid "developer dispatch prior revision changed"
          in
          let* prepared =
            Store.prepare_dispatch issuer.handle ~expected:observed ~lookup ~dispatched
            |> Result.map_error ~f:error_of_store
          in
          let handoff = Store.Handoff.create ~settle:(fun _ -> ()) in
          issuer.dispatch_owner <- Some t;
          issuer.effect_revision <- issuer.effect_revision + 1;
          t.dispatch_state <- Dispatch_pending { prepared; handoff; dispatched };
          match
            Store.commit_and_settle issuer.handle prepared handoff ~admit:(fun () ->
              dispatch_allowed t fresh)
          with
          | Ok (Store.Settlement.Installed installed) ->
            mark_installed t dispatched installed
          | Ok (Absent | Prewrite_conflict) ->
            let* _ = absent t in
            invalid "developer dispatch was not installed"
          | Ok Diverged ->
            issuer.state <- Quarantined;
            invalid "developer dispatch is divergent"
          | Error error ->
            reconcile_failed_save t ~prepared ~handoff ~dispatched;
            Error (error_of_store error)
          | exception exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            reconcile_failed_save t ~prepared ~handoff ~dispatched;
            Exn.raise_with_original_backtrace exn backtrace)
  ;;

  let reconcile_dispatch_unlocked (t : t) =
    let issuer = t.issuer in
    if
      (not (same_domain issuer.registry)) || (not t.bracket_exited) || t.callback_complete
    then invalid "native dispatch recovery requires an exited unentered bracket"
    else (
      match t.dispatch_state with
      | Dispatch_installed (dispatched, revision) -> Ok (Installed (dispatched, revision))
      | No_attempt ->
        let* observed =
          Store.observe issuer.handle |> Result.map_error ~f:error_of_store
        in
        (match
           Store.Invocation_lookup.prior
             (Store.find_invocation observed (H.context t.registration.invocation).id)
         with
         | None -> absent t
         | Some prior ->
           (match H.status (Store.Invocation_observation.value prior) with
            | Admitted -> absent t
            | Dispatching | Resolved _ | Published _ ->
              invalid "unowned durable dispatch cannot be adopted"))
      | Dispatch_pending { prepared; handoff; dispatched } ->
        (match Store.reconcile_and_settle issuer.handle prepared handoff with
         | Error error -> Error (error_of_store error)
         | Ok (Store.Settlement.Installed installed_value) ->
           let* revision = mark_installed t dispatched installed_value in
           Ok (Installed (dispatched, revision))
         | Ok (Absent | Prewrite_conflict) -> absent t
         | Ok Diverged ->
           issuer.state <- Quarantined;
           Ok Uncertain))
  ;;

  let reconcile_dispatch (t : t) =
    Mutation_gate.with_owner t.issuer.mutation_gate ~f:(fun () ->
      reconcile_dispatch_unlocked t)
  ;;

  let retained (t : t) =
    let issuer = t.issuer in
    if (not (same_domain issuer.registry)) || not t.bracket_exited
    then invalid "native retention requires an exited same-domain bracket"
    else
      Ok
        (List.exists issuer.unresolved_native ~f:(fun current ->
           phys_equal current t.registration))
  ;;

  let remember_terminal (t : t) terminal =
    let context = H.context t.registration.invocation in
    let* () = H.validate terminal in
    if not (H.equal_context context (H.context terminal))
    then invalid "native terminal belongs to another physical scope"
    else (
      match t.remembered_terminal with
      | None ->
        t.remembered_terminal <- Some terminal;
        Ok ()
      | Some prior
        when H.equal_context (H.context prior) (H.context terminal)
             && P.Invocation.equal_status (H.status prior) (H.status terminal)
             && Option.equal
                  P.Invocation.equal_outcome
                  (H.outcome prior)
                  (H.outcome terminal) -> Ok ()
      | Some _ -> invalid "native terminal changed after physical retention")
  ;;

  let remember_failure (t : t) ~terminal =
    if Option.is_some t.sealed
    then invalid "pre-entry failure cannot replace a sealed native outcome"
    else (
      match H.outcome terminal, H.status terminal with
      | Some (Fail _ | Cancelled _), Resolved _ -> remember_terminal t terminal
      | _ -> invalid "pre-entry native terminal is not a failure")
  ;;

  let stage_background (t : t) ~request ~checkpoint ~check_parent_now =
    let context = H.context t.registration.invocation in
    let capture_fingerprint = Chatml_developer_capture.fingerprint t.issuer.capture in
    let* () =
      match t.registration.job_parent with
      | None -> Ok ()
      | Some parent ->
        Job_scope.Parent.check parent ~owner:t.issuer.owner ~capture_fingerprint
    in
    let* parent_job, nested_depth =
      match t.registration.job_parent with
      | None -> Ok (None, 0)
      | Some parent ->
        let depth = Job_scope.Parent.nested_depth parent in
        if depth >= Int.max_value
        then invalid "developer job nesting depth overflows"
        else
          Ok
            ( Some (Job_scope.Parent.job_id parent, Job_scope.Parent.attempt parent)
            , depth + 1 )
    in
    let* launch =
      P.Developer_job.Launch.create
        ~parent:(P.Developer_job.Launch.Invocation context.id)
        ~parent_job
        ~nested_depth
    in
    Action.stage_background_with_launch
      t.action
      ~launch
      ~request
      ~checkpoint
      ~check_parent_now:(fun () ->
        let* () = check_parent_now () in
        match t.registration.job_parent with
        | None -> Ok ()
        | Some parent ->
          Job_scope.Parent.check_now parent ~owner:t.issuer.owner ~capture_fingerprint)
  ;;

  let admit_external_model (t : t) ~request ~checkpoint ~check_parent_now =
    admit_external_model_from_scope
      (Native_model t)
      ~request
      ~checkpoint
      ~check_parent_now
  ;;

  let rollback_staged (t : t) id = Action.rollback_staged t.action id
  let cleanup_staged (t : t) id = Action.cleanup_staged t.action id

  let seal (t : t) ~outcome ~selected =
    let* () =
      match outcome with
      | P.Invocation.Pending (P.Invocation.Job id, _) ->
        if List.exists selected ~f:(P.Id.Job.equal id)
        then Ok ()
        else invalid "pending native job is not selected"
      | Pending (P.Invocation.Subscription _, _) ->
        invalid "developer native subscription work is unsupported"
      | (Fail _ | Cancelled _) when not (List.is_empty selected) ->
        invalid "failed native invocation cannot select new jobs"
      | Complete _ | Fail _ | Cancelled _ -> Ok ()
    in
    let* selection = Action.seal t.action ~selected in
    let frozen = { scope = t; selection; outcome } in
    t.sealed <- Some frozen;
    Ok frozen
  ;;

  let validate_selection (t : t) outcome selected ~retained =
    match t.sealed, selected with
    | Some frozen, Some selected
      when phys_equal frozen selected && P.Invocation.equal_outcome outcome frozen.outcome
      -> Ok ()
    | None, None ->
      (match outcome with
       | P.Invocation.Pending _ -> invalid "unowned pending work cannot be resolved"
       | Complete _ when retained ->
         invalid "unsealed interrupted native callback cannot report success"
       | Complete _ | Fail _ | Cancelled _ -> Ok ())
    | Some _, None | None, Some _ | Some _, Some _ ->
      invalid "native terminal selection is foreign or changed"
  ;;

  let submit (t : t) ~terminal ~(selected : Selection.t option) =
    let issuer = t.issuer in
    let context = H.context t.registration.invocation in
    let terminal_context = H.context terminal in
    let dispatched = t.dispatched in
    let* () =
      if
        (not (same_domain issuer.registry))
        || t.submitted
        || (not t.callback_complete)
        || (not (H.equal_context context terminal_context))
        || not
             (Option.value_map dispatched ~default:false ~f:(fun dispatched ->
                H.equal_context (H.context dispatched) terminal_context
                && P.Invocation.equal_status (H.status dispatched) Dispatching))
      then invalid "native terminal is not this scope's dispatched invocation"
      else Ok ()
    in
    let* outcome =
      match H.outcome terminal, H.status terminal with
      | Some outcome, P.Invocation.Resolved _ -> Ok outcome
      | _ -> invalid "native terminal must be resolved"
    in
    let* () = validate_selection t outcome selected ~retained:false in
    let* () = remember_terminal t terminal in
    let intent =
      { plan = Native_terminal { scope = t; terminal; selected }; settlement = None }
    in
    Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () ->
      let* () =
        if
          same_domain issuer.registry
          && (not t.submitted)
          && t.callback_complete
          && Option.value_map t.dispatched ~default:false ~f:(fun current ->
            H.equal_context (H.context current) terminal_context)
          && Option.value_map
               (Hashtbl.find issuer.native_calls context.id)
               ~default:false
               ~f:(fun registration -> phys_equal registration t.registration)
        then validate_selection t outcome selected ~retained:false
        else invalid "native terminal lost its physical submission owner"
      in
      let* () =
        match selected with
        | None -> Staging.abort_pre_handoff t.action.staged
        | Some _ -> Ok ()
      in
      let* () =
        Terminal_queue.complete issuer.terminals t.registration.terminal_slot ~intent
        |> Result.map_error ~f:error_of_terminal_queue
      in
      t.submitted <- true;
      t.action.queued <- true;
      let* () =
        match selected with
        | None -> Ok ()
        | Some selected ->
          Staging.mark_handoff t.action.staged selected.selection.staged_selection
      in
      drain_and_settle_unlocked issuer intent)
  ;;

  let with_recovery_slot issuer ~owned f =
    if owned
    then
      if issuer.reconciling then f () else invalid "native recovery lost its owner slot"
    else if issuer.reconciling
    then invalid "native recovery is already active"
    else (
      issuer.reconciling <- true;
      Exn.protect ~finally:(fun () -> issuer.reconciling <- false) ~f)
  ;;

  let resolve_retained_internal (t : t) ~terminal ~owned =
    let issuer = t.issuer in
    let registration = t.registration in
    if
      (not (same_domain issuer.registry))
      || t.submitted
      || (not t.bracket_exited)
      || (issuer.reconciling && not owned)
      || not
           (List.exists issuer.unresolved_native ~f:(fun current ->
              phys_equal current registration))
    then invalid "native terminal recovery has no retained physical scope"
    else
      with_recovery_slot issuer ~owned (fun () ->
        let* () = H.validate terminal in
        let context = H.context registration.invocation in
        let* outcome =
          if H.equal_context context (H.context terminal)
          then (
            match H.outcome terminal, H.status terminal with
            | Some outcome, P.Invocation.Resolved _ -> Ok outcome
            | _ -> invalid "retained native terminal must be resolved")
          else invalid "retained native terminal belongs to another invocation"
        in
        let selected = t.sealed in
        let* () = validate_selection t outcome selected ~retained:true in
        let* observed =
          Store.observe issuer.handle |> Result.map_error ~f:error_of_store
        in
        let prior =
          Store.find_invocation observed context.id |> Store.Invocation_lookup.prior
        in
        let* () =
          match prior with
          | Some prior ->
            let actual = Store.Invocation_observation.value prior in
            if
              H.equal_context (H.context actual) context
              && P.Invocation.equal_status (H.status actual) Dispatching
            then Ok ()
            else invalid "retained native dispatch changed durably"
          | None -> invalid "retained native dispatch is not durable"
        in
        let intent =
          { plan = Native_terminal { scope = t; terminal; selected }; settlement = None }
        in
        Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () ->
          let* () =
            if
              same_domain issuer.registry
              && (not t.submitted)
              && t.bracket_exited
              && List.exists issuer.unresolved_native ~f:(fun current ->
                phys_equal current registration)
            then validate_selection t outcome selected ~retained:true
            else invalid "retained native terminal lost its physical owner"
          in
          let* () =
            match selected with
            | None -> Staging.abort_pre_handoff t.action.staged
            | Some _ -> Ok ()
          in
          let* () =
            Terminal_queue.complete issuer.terminals registration.terminal_slot ~intent
            |> Result.map_error ~f:error_of_terminal_queue
          in
          t.submitted <- true;
          t.action.queued <- true;
          let* () =
            match selected with
            | None -> Ok ()
            | Some selected ->
              Staging.mark_handoff t.action.staged selected.selection.staged_selection
          in
          issuer.unresolved_native
          <- List.filter issuer.unresolved_native ~f:(fun current ->
               not (phys_equal current registration));
          drain_and_settle_unlocked issuer intent))
  ;;

  let resolve_retained t ~terminal = resolve_retained_internal t ~terminal ~owned:false

  let resolve_remembered (t : t) =
    match t.remembered_terminal with
    | None -> invalid "native scope has no remembered terminal"
    | Some terminal -> resolve_retained t ~terminal
  ;;

  let abandon_retained_without_dispatch_internal (t : t) ~owned =
    let issuer = t.issuer in
    let registration = t.registration in
    if
      (not (same_domain issuer.registry))
      || t.submitted
      || (not t.bracket_exited)
      || Option.is_some t.dispatched
      || t.callback_complete
      || Option.is_some t.sealed
      || (not (Staging.is_empty t.action.staged))
      || (issuer.reconciling && not owned)
      || not
           (List.exists issuer.unresolved_native ~f:(fun current ->
              phys_equal current registration))
    then invalid "native scope has no unentered retained dispatch proof"
    else
      with_recovery_slot issuer ~owned (fun () ->
        let context = H.context registration.invocation in
        let* observed =
          Store.observe issuer.handle |> Result.map_error ~f:error_of_store
        in
        let prior =
          Store.find_invocation observed context.id |> Store.Invocation_lookup.prior
        in
        let* () =
          match prior with
          | None -> Ok ()
          | Some prior ->
            let actual = Store.Invocation_observation.value prior in
            if
              H.equal_context (H.context actual) context
              && P.Invocation.equal_status (H.status actual) Admitted
            then Ok ()
            else invalid "native scope has a durable dispatch or terminal"
        in
        let* () =
          Terminal_queue.abandon issuer.terminals registration.terminal_slot
          |> Result.map_error ~f:error_of_terminal_queue
        in
        issuer.unresolved_native
        <- List.filter issuer.unresolved_native ~f:(fun current ->
             not (phys_equal current registration));
        restore_live_after_retention issuer;
        Ok ())
  ;;

  let abandon_retained_without_dispatch t =
    abandon_retained_without_dispatch_internal t ~owned:false
  ;;
end

module Model_inference = struct
  type nonrec t = inference_scope

  exception Tracking_failure of P.Error.t

  let require = function
    | Ok value -> value
    | Error error -> raise (Tracking_failure error)
  ;;

  let require_store result = require (Result.map_error result ~f:error_of_store)

  let resource_limit message =
    P.Error.create P.Error.Resource_limit ~message ~retryable:false ()
  ;;

  let same_parent (left : Transcript.Scope.parent) (right : Transcript.Scope.parent) =
    Transcript.Scope.Key.equal left.scope right.scope
    && Option.equal History_entry.Id.equal left.call_entry_id right.call_entry_id
    && Option.equal String.equal left.call_alias right.call_alias
  ;;

  let model_selection t =
    match t.inference_issuer.selected_model with
    | Some selection when phys_equal selection t.inference_selected_model -> Ok selection
    | None | Some _ -> invalid "developer inference selection changed"
  ;;

  let check_now ?(allow_tracking = false) t =
    let issuer = t.inference_issuer in
    let* _ = current_local ~allow_tracking issuer in
    let* () = check_inference_scope_local t in
    match t.inference_origin with
    | Claimed_model_job -> Ok ()
    | Claimed_saved_agent { terminal; _ } -> Native_terminal.check_completion_now terminal
  ;;

  let check t =
    let issuer = t.inference_issuer in
    let* witness = current issuer in
    let* () =
      Job_scope.Parent.check
        t.inference_parent
        ~owner:issuer.owner
        ~capture_fingerprint:(Chatml_developer_capture.fingerprint issuer.capture)
    in
    let* () =
      match t.inference_origin with
      | Claimed_model_job -> Ok ()
      | Claimed_saved_agent { selected; _ } ->
        Agent_session.Captured_agent_runner.Claimed_developer.Selected.check selected
    in
    let* () = check_effect_now issuer witness in
    check_now t
  ;;

  let check_current t = require (check t)

  let route t =
    match Eio.Fiber.get inference_route_key with
    | Some route when phys_equal route.inference_scope t -> route
    | None | Some _ ->
      raise
        (Tracking_failure
           (P.Error.invalid_request "developer inference route is not owned"))
  ;;

  let registration route =
    match route.inference_registration with
    | Some registration -> registration
    | None ->
      raise
        (Tracking_failure
           (P.Error.invalid_request "developer inference admission is unsettled"))
  ;;

  let release_slot route =
    if not route.inference_terminal
    then (
      route.inference_terminal <- true;
      let issuer = route.inference_scope.inference_issuer in
      issuer.inference_active <- issuer.inference_active - 1;
      if
        Option.is_none route.inference_pending
        && Option.is_none route.inference_observation
        && Option.is_none route.inference_completion
      then (
        route.inference_attempt <- None;
        issuer.inference_routes
        <- List.filter issuer.inference_routes ~f:(fun current ->
             not (phys_equal current route));
        route.inference_scope.inference_issued
        <- List.filter route.inference_scope.inference_issued ~f:(fun current ->
             not (phys_equal current route))))
  ;;

  let acknowledge route = function
    | Store.Tracking.Admitted registration ->
      route.inference_registration <- Some registration;
      route.inference_pending <- None;
      route.inference_pending_change <- None
    | Store.Tracking.Updated _ ->
      route.inference_pending <- None;
      route.inference_pending_change <- None
  ;;

  let change route change =
    let issuer = route.inference_scope.inference_issuer in
    let tracking = issuer.tracking in
    let registration = registration route in
    let previous = require_store (Store.Tracking.observe tracking registration) in
    let prepared =
      require_store
        (Store.Tracking.prepare_change
           tracking
           ~claim:route.inference_claim
           ~registration
           ~previous
           ~change)
    in
    match prepared with
    | Store.Tracking.Unchanged _ -> ()
    | Prepared pending ->
      route.inference_pending <- Some pending;
      route.inference_pending_change <- Some change;
      let admit () =
        match change with
        | Store.Tracking.Started ->
          Result.is_ok (check_now ~allow_tracking:true route.inference_scope)
        | Observed _ | Finished _ | Interrupted _ -> same_domain issuer.registry
      in
      (match Store.Tracking.commit tracking pending ~admit with
       | Ok receipt -> acknowledge route receipt
       | Error error -> raise (Tracking_failure (error_of_store error)))
  ;;

  let record_completion route change =
    match route.inference_completion with
    | None -> route.inference_completion <- Some change
    | Some _ ->
      raise
        (Tracking_failure
           (P.Error.invalid_request "developer inference completion changed"))
  ;;

  let completion_change = function
    | Inference_client.Completion.Returned terminal -> Store.Tracking.Finished terminal
    | Interrupted { reason; delivery } -> Interrupted { reason; delivery }
  ;;

  let current_route t =
    let route = route t in
    if route.inference_terminal
    then
      raise
        (Tracking_failure
           (P.Error.invalid_request "developer inference attempt already settled"));
    route
  ;;

  let with_claimed_base issuer ~active ~origin ~f =
    try
      let result =
        let* () =
          if same_domain issuer.registry
          then Ok ()
          else invalid "developer inference owner belongs to another domain"
        in
        let* selection =
          match issuer.selected_model with
          | Some selection -> Ok selection
          | None -> invalid "developer inference provider is unavailable"
        in
        let* context = Provider_operator_port.Model_selection.context selection in
        let* parent =
          Job_scope.parent (Developer_job_worker.issuer issuer.worker) active
        in
        let claim = Job_scope.claim active in
        let* () =
          match origin with
          | Claimed_model_job ->
            if
              P.Job.equal_kind
                (P.Developer_job.kind (Store.Claim.job claim))
                P.Job.Model_call
            then Ok ()
            else invalid "developer inference requires a physical Model_call claim"
          | Claimed_saved_agent { selected; terminal } ->
            let* () =
              if
                P.Job.equal_kind
                  (P.Developer_job.kind (Store.Claim.job claim))
                  P.Job.Async_tool
                && phys_equal
                     active
                     (Agent_session.Captured_agent_runner.Claimed_developer.Selected
                      .active
                        selected)
              then Ok ()
              else invalid "saved Agent inference requires its physical Async_tool claim"
            in
            let* () = Native_terminal.check_completion_now terminal in
            (match terminal.registration.job_parent with
             | Some native_parent when Job_scope.Parent.same_claim parent native_parent ->
               Ok ()
             | Some _ | None ->
               invalid "saved Agent inference has a foreign native parent")
        in
        let scope =
          { inference_issuer = issuer
          ; inference_origin = origin
          ; inference_active_worker = active
          ; inference_parent = parent
          ; inference_context = context
          ; inference_selected_model = selection
          ; inference_candidate_limit =
              Inference_runtime.Limits.max_candidates
                (Inference_runtime.Context.limits context)
          ; inference_live = true
          ; inference_exit_reason = None
          ; inference_switch = None
          ; inference_calls = []
          ; inference_issued = []
          }
        in
        let* () = check scope in
        issuer.inference_scopes <- scope :: issuer.inference_scopes;
        let finally ~reason =
          scope.inference_live <- false;
          scope.inference_exit_reason <- Some reason;
          scope.inference_switch <- None;
          List.iter scope.inference_issued ~f:(fun route ->
            if
              (not route.inference_terminal)
              && Option.is_some route.inference_registration
              && Option.is_none route.inference_completion
            then (
              let delivery =
                match route.inference_attempt with
                | Some attempt -> Inference_runtime.Attempt.delivery attempt
                | None -> Inference.Event.Terminal.Definitely_not_submitted
              in
              route.inference_completion
              <- Some (Store.Tracking.Interrupted { reason; delivery })));
          issuer.inference_bindings
          <- issuer.inference_bindings - List.length scope.inference_calls;
          scope.inference_calls <- [];
          issuer.inference_scopes
          <- List.filter issuer.inference_scopes ~f:(fun current ->
               not (phys_equal current scope));
          Ok ()
        in
        let outcome =
          try
            `Returned
              (Eio.Switch.run (fun sw ->
                 scope.inference_switch <- Some sw;
                 Eio.Fiber.with_binding inference_scope_key scope (fun () -> f scope ~sw)))
          with
          | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        match outcome with
        | `Returned value ->
          let* () =
            finally ~reason:Inference.Observation.Attempt_record.Host_interrupted
          in
          Ok value
        | `Raised (exn, backtrace) ->
          let reason =
            match exn with
            | Eio.Cancel.Cancelled _ -> Inference.Observation.Attempt_record.Cancelled
            | _ -> Host_interrupted
          in
          (try ignore (finally ~reason : (unit, P.Error.t) result) with
           | _ -> ());
          Exn.raise_with_original_backtrace exn backtrace
      in
      result
    with
    | Tracking_failure error -> Error error
  ;;

  let context t = t.inference_context

  let admit_external_model t ~request ~checkpoint =
    match t.inference_origin with
    | Claimed_model_job ->
      admit_external_model_from_scope
        (Claimed_model t)
        ~request
        ~checkpoint
        ~check_parent_now:(fun () -> Ok ())
    | Claimed_saved_agent { selected; terminal } ->
      let* () = check t in
      admit_external_model_from_scope
        (Native_model terminal)
        ~request
        ~checkpoint
        ~check_parent_now:(fun () ->
          Agent_session.Captured_agent_runner.Claimed_developer.Selected.check_now
            selected)
  ;;

  let identity t : Inference_client.Identity.t =
    { new_preparation_id =
        (fun () ->
          let issuer = t.inference_issuer in
          if issuer.inference_serial = Int.max_value
          then raise (Tracking_failure (resource_limit "inference identity exhausted"));
          issuer.inference_serial <- issuer.inference_serial + 1;
          "developer-inference-" ^ Int.to_string issuer.inference_serial)
    ; with_attempt =
        (fun prepared ~relation ~f ->
          let issuer = t.inference_issuer in
          require (check t);
          let relation =
            match relation with
            | Transcript.Scope.Root -> Store.Tracking.Root
            | Nested parent ->
              let binding =
                List.find t.inference_calls ~f:(fun call ->
                  same_parent call.inference_call_parent parent)
              in
              (match binding with
               | Some binding ->
                 Store.Tracking.Nested
                   { parent = binding.inference_call_registration
                   ; call_entry_id = parent.call_entry_id
                   ; call_alias = parent.call_alias
                   }
               | None ->
                 raise
                   (Tracking_failure
                      (P.Error.invalid_request
                         "nested inference has no admitted physical call")))
          in
          if
            issuer.inference_active
            >= issuer.registry.limits.max_active_inference_attempts
          then
            raise
              (Tracking_failure
                 (resource_limit "active inference attempt limit was reached"));
          issuer.inference_active <- issuer.inference_active + 1;
          let route =
            { inference_scope = t
            ; inference_claim = Job_scope.claim t.inference_active_worker
            ; inference_configuration = Inference_runtime.Prepared.configuration prepared
            ; inference_registration = None
            ; inference_attempt = None
            ; inference_started = false
            ; inference_pending = None
            ; inference_pending_change = None
            ; inference_observation = None
            ; inference_completion = None
            ; inference_terminal = false
            }
          in
          issuer.inference_routes <- route :: issuer.inference_routes;
          t.inference_issued <- route :: t.inference_issued;
          let discard_unissued () =
            if
              Option.is_none route.inference_pending
              && Option.is_none route.inference_registration
            then (
              release_slot route;
              issuer.inference_routes
              <- List.filter issuer.inference_routes ~f:(fun current ->
                   not (phys_equal current route)))
          in
          let outcome =
            try
              let pending =
                Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () ->
                  require_store
                    ((match t.inference_origin with
                      | Claimed_model_job -> Store.Tracking.prepare_admit
                      | Claimed_saved_agent _ -> Store.Tracking.prepare_admit_saved_agent)
                       issuer.tracking
                       ~claim:route.inference_claim
                       ~relation
                       ~configuration:(Inference_runtime.Prepared.configuration prepared)))
              in
              route.inference_pending <- Some pending;
              let receipt =
                Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () ->
                  require_store
                    (Store.Tracking.commit issuer.tracking pending ~admit:(fun () ->
                       Result.is_ok (check_now ~allow_tracking:true t))))
              in
              acknowledge route receipt;
              let registration = registration route in
              `Returned
                (Eio.Fiber.with_binding inference_route_key route (fun () ->
                   f
                     ~scope:(Store.Tracking.Registration.scope registration)
                     ~accounting_id:
                       (Store.Tracking.Registration.accounting_id registration)))
            with
            | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
          in
          discard_unissued ();
          match outcome with
          | `Returned value -> value
          | `Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
    }
  ;;

  let before_dispatch t prepared =
    require (check t);
    require
      (if
         Inference.Request.Target.equal
           (Inference_runtime.Prepared.target prepared)
           (Inference_runtime.Context.target t.inference_context)
       then Ok ()
       else invalid "prepared inference target changed")
  ;;

  let on_attempt t attempt =
    let route = current_route t in
    let registration = registration route in
    if Option.is_some route.inference_attempt
    then
      raise
        (Tracking_failure
           (P.Error.invalid_request "inference attempt was already acknowledged"));
    if
      not
        (Transcript.Scope.equal
           (Inference_runtime.Attempt.scope attempt)
           (Store.Tracking.Registration.scope registration))
    then
      raise (Tracking_failure (P.Error.invalid_request "inference attempt scope changed"));
    if
      (not
         (Inference.Observation.Observation_id.equal
            (Inference_runtime.Attempt.accounting_id attempt)
            (Store.Tracking.Registration.accounting_id registration)))
      || not
           (Inference.Observation.Configuration.equal
              (Inference_runtime.Attempt.configuration attempt)
              route.inference_configuration)
    then
      raise
        (Tracking_failure
           (P.Error.invalid_request
              "inference attempt accounting or prepared configuration changed"));
    route.inference_attempt <- Some attempt;
    Mutation_gate.with_owner t.inference_issuer.mutation_gate ~f:(fun () ->
      change route Store.Tracking.Started);
    route.inference_started <- true
  ;;

  let on_observation t observation =
    let route = current_route t in
    if Option.is_some route.inference_observation
    then
      raise
        (Tracking_failure
           (P.Error.invalid_request "inference observation already retained"));
    try
      Mutation_gate.with_owner t.inference_issuer.mutation_gate ~f:(fun () ->
        change route (Store.Tracking.Observed observation))
    with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      if Option.is_some (Store.Tracking.pending t.inference_issuer.tracking)
      then route.inference_observation <- Some observation;
      Exn.raise_with_original_backtrace exn backtrace
  ;;

  let on_completion t completion =
    let route = current_route t in
    let attempt = Inference_client.Completion.attempt completion in
    if
      not
        (Option.value_map route.inference_attempt ~default:false ~f:(fun actual ->
           phys_equal actual attempt))
    then
      raise
        (Tracking_failure
           (P.Error.invalid_request "inference completion has another attempt"));
    let change_plan =
      completion_change (Inference_client.Completion.outcome completion)
    in
    record_completion route change_plan;
    Mutation_gate.with_owner t.inference_issuer.mutation_gate ~f:(fun () ->
      change route change_plan);
    route.inference_completion <- None;
    release_slot route
  ;;

  let on_admitted_call t token =
    require (check_now t);
    let issuer = t.inference_issuer in
    let route = current_route t in
    let registration =
      match route.inference_attempt, route.inference_registration with
      | Some attempt, Some registration
        when route.inference_started
             && phys_equal
                  attempt
                  (Chat_response.In_memory_stream.Admitted_call.attempt token) ->
        registration
      | None, _ | Some _, None | Some _, Some _ ->
        raise
          (Tracking_failure
             (P.Error.invalid_request "admitted call belongs to another attempt"))
    in
    let parent = Chat_response.In_memory_stream.Admitted_call.parent token in
    if
      (not
         (Transcript.Scope.Key.equal
            parent.scope
            (Transcript.Scope.key (Store.Tracking.Registration.scope registration))))
      || List.exists t.inference_calls ~f:(fun call ->
        same_parent call.inference_call_parent parent)
    then
      raise
        (Tracking_failure
           (P.Error.invalid_request "admitted call parent is duplicate or foreign"));
    if
      List.count t.inference_calls ~f:(fun call ->
        phys_equal call.inference_call_registration registration)
      >= t.inference_candidate_limit
      || issuer.inference_bindings >= issuer.registry.limits.max_admitted_call_bindings
    then
      raise (Tracking_failure (resource_limit "admitted call binding limit was reached"));
    t.inference_calls
    <- { inference_call_parent = parent; inference_call_registration = registration }
       :: t.inference_calls;
    issuer.inference_bindings <- issuer.inference_bindings + 1
  ;;

  let submission_guard t =
    Inference_runtime.Submission_guard.create ~refresh:(fun () ->
      require (check t);
      fun () -> require (check_now t))
  ;;

  let interrupted_change route ~reason =
    let delivery =
      match route.inference_attempt with
      | Some attempt -> Inference_runtime.Attempt.delivery attempt
      | None -> Inference.Event.Terminal.Definitely_not_submitted
    in
    Store.Tracking.Interrupted { reason; delivery }
  ;;

  let retain_unfinished route ~reason =
    if
      (not route.inference_terminal)
      && Option.is_some route.inference_registration
      && Option.is_none route.inference_completion
    then route.inference_completion <- Some (interrupted_change route ~reason)
  ;;

  let reconcile_one_pending route =
    let issuer = route.inference_scope.inference_issuer in
    match route.inference_pending with
    | None -> ()
    | Some pending ->
      let settlement = Store.Tracking.reconcile issuer.tracking pending in
      (match settlement with
       | Ok (Store.Tracking.Tracking_installed receipt) -> acknowledge route receipt
       | Ok Store.Tracking.Tracking_diverged ->
         raise
           (Tracking_failure (P.Error.invalid_request "inference persistence diverged"))
       | Ok Store.Tracking.Tracking_absent ->
         if
           (not route.inference_scope.inference_live)
           && Option.is_none route.inference_registration
         then (
           require_store (Store.Tracking.retire_absent_admission issuer.tracking pending);
           route.inference_pending <- None;
           release_slot route)
         else if
           (not route.inference_scope.inference_live)
           && (match route.inference_pending_change with
               | Some Store.Tracking.Started -> true
               | None | Some (Observed _ | Finished _ | Interrupted _) -> false)
           && Option.is_some route.inference_registration
         then (
           require_store (Store.Tracking.retire_absent_start issuer.tracking pending);
           route.inference_pending <- None;
           route.inference_pending_change <- None)
         else (
           let next =
             require_store (Store.Tracking.reprepare_same issuer.tracking pending)
           in
           route.inference_pending <- Some next;
           let admit () =
             if
               Option.is_none route.inference_registration
               ||
               match route.inference_pending_change with
               | Some Store.Tracking.Started -> true
               | None | Some (Observed _ | Finished _ | Interrupted _) -> false
             then Result.is_ok (check_now ~allow_tracking:true route.inference_scope)
             else same_domain issuer.registry
           in
           acknowledge
             route
             (require_store (Store.Tracking.commit issuer.tracking next ~admit)))
       | Error Store.Error.Prewrite_conflict
         when (not route.inference_scope.inference_live)
              && Option.is_none route.inference_registration ->
         require_store
           (Store.Tracking.abort_unattempted_admission issuer.tracking pending);
         route.inference_pending <- None;
         release_slot route
       | Error error -> raise (Tracking_failure (error_of_store error)))
  ;;

  let drain_route route =
    reconcile_one_pending route;
    if Option.is_none route.inference_pending
    then (
      (match route.inference_scope.inference_exit_reason with
       | Some reason -> retain_unfinished route ~reason
       | None -> ());
      (match route.inference_observation with
       | None -> ()
       | Some observation ->
         change route (Store.Tracking.Observed observation);
         route.inference_observation <- None);
      match route.inference_completion with
      | None -> ()
      | Some completion ->
        change route completion;
        route.inference_completion <- None;
        release_slot route)
  ;;

  let reconcile issuer =
    try
      Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () ->
        match Store.Tracking.pending issuer.tracking with
        | None -> ()
        | Some pending ->
          (match
             List.find issuer.inference_routes ~f:(fun route ->
               Option.value_map route.inference_pending ~default:false ~f:(fun owned ->
                 phys_equal owned pending))
           with
           | Some route -> reconcile_one_pending route
           | None ->
             raise
               (Tracking_failure
                  (P.Error.invalid_request
                     "inference pending has no physical route owner"))));
      List.iter (List.rev issuer.inference_routes) ~f:(fun route ->
        Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () -> drain_route route));
      if
        Option.is_some (Store.Tracking.pending issuer.tracking)
        || List.exists issuer.inference_routes ~f:(fun route ->
          Option.is_some route.inference_observation
          || Option.is_some route.inference_completion)
      then invalid "inference bookkeeping remains unsettled"
      else Ok ()
    with
    | Tracking_failure error -> Error error
  ;;

  let with_claimed_origin issuer ~active ~origin ~f =
    match with_claimed_base issuer ~active ~origin ~f with
    | Ok value ->
      let* () = reconcile issuer in
      Ok value
    | Error error ->
      (try
         ignore (Eio.Cancel.protect (fun () -> reconcile issuer) : (unit, _) result)
       with
       | _ -> ());
      Error error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      (try
         ignore (Eio.Cancel.protect (fun () -> reconcile issuer) : (unit, _) result)
       with
       | _ -> ());
      Exn.raise_with_original_backtrace exn backtrace
  ;;

  let with_claimed issuer ~active ~f =
    with_claimed_origin issuer ~active ~origin:Claimed_model_job ~f
  ;;

  let with_claimed_agent issuer ~selected ~f =
    let module Selected = Agent_session.Captured_agent_runner.Claimed_developer.Selected
    in
    let* () = Selected.check selected in
    let invocation = Selected.invocation selected in
    let context = H.context invocation in
    let* terminal =
      match Hashtbl.find issuer.native_calls context.id with
      | Some registration when phys_equal registration.invocation invocation ->
        (match registration.scope with
         | Some terminal
           when Option.value_map
                  (Eio.Fiber.get native_call_key)
                  ~default:false
                  ~f:(phys_equal issuer) -> Ok terminal
         | Some _ | None -> invalid "saved Agent native callback is not entered")
      | Some _ | None -> invalid "saved Agent native invocation is not registered"
    in
    let* () = Native_terminal.check_completion_now terminal in
    with_claimed_origin
      issuer
      ~active:(Selected.active selected)
      ~origin:(Claimed_saved_agent { selected; terminal })
      ~f
  ;;

  let live issuer =
    List.exists issuer.inference_scopes ~f:(fun scope -> scope.inference_live)
  ;;
end

let reconcile_pending t =
  if not (same_domain t.registry)
  then invalid "developer root owner belongs to another domain"
  else if Option.is_some t.root_call || not (Hashtbl.is_empty t.native_calls)
  then invalid "developer callback is still active"
  else if Option.is_some t.control_call
  then invalid "developer job cancellation is still active"
  else if Model_inference.live t
  then invalid "developer inference callback is still active"
  else if t.reconciling
  then
    Error
      (P.Error.create
         P.Error.Conflict
         ~message:"developer terminal reconciliation is already active"
         ~retryable:true
         ())
  else (
    t.reconciling <- true;
    Exn.protect
      ~finally:(fun () -> t.reconciling <- false)
      ~f:(fun () ->
        let* () = Model_inference.reconcile t in
        let* () =
          match t.external_model with
          | None -> Ok ()
          | Some (Model_preparing _ | Model_reserved _) ->
            invalid "developer model admission has not joined"
          | Some (Model_pending { token; _ }) ->
            (match
               Mutation_gate.with_owner t.mutation_gate ~f:(fun () ->
                 reconcile_external_model_once t token)
             with
             | Ok (Store.Settlement.Installed _ | Absent | Prewrite_conflict) -> Ok ()
             | Ok Diverged -> invalid "developer model write remains quarantined"
             | Error error -> Error error)
        in
        let* () =
          Mutation_gate.with_owner t.mutation_gate ~f:(fun () ->
            Job_control.reconcile_pending t)
        in
        let* () =
          List.fold_result t.unresolved_native ~init:() ~f:(fun () scope_registration ->
            match scope_registration.scope with
            | None -> invalid "retained native registration has no physical scope"
            | Some scope ->
              if (not scope.bracket_exited) || scope.submitted
              then invalid "retained native callback has not joined"
              else if scope.callback_complete
              then (
                match scope.remembered_terminal with
                | Some terminal ->
                  Native_terminal.resolve_retained_internal scope ~terminal ~owned:true
                  |> Result.map ~f:ignore
                | None ->
                  invalid "completed native callback has no canonical terminal plan")
              else
                let* dispatch = Native_terminal.reconcile_dispatch scope in
                (match dispatch with
                 | Native_terminal.Absent ->
                   Native_terminal.abandon_retained_without_dispatch_internal
                     scope
                     ~owned:true
                 | Installed _ ->
                   (match scope.remembered_terminal with
                    | Some terminal ->
                      Native_terminal.resolve_retained_internal
                        scope
                        ~terminal
                        ~owned:true
                      |> Result.map ~f:ignore
                    | None ->
                      invalid "retained native dispatch has no truthful terminal plan")
                 | Uncertain -> invalid "native dispatch remains uncertain"))
        in
        let* () =
          if Option.is_none t.dispatch_owner
          then Ok ()
          else invalid "native dispatch still owns the Store writer"
        in
        let* () = drain_terminals t in
        if List.is_empty t.unresolved_native && Option.is_none t.external_model
        then Ok ()
        else invalid "native terminal remains physically unresolved"))
;;

let with_native_registration t ?job_parent ~invocation f =
  if not (same_domain t.registry)
  then invalid "developer native owner belongs to another domain"
  else if
    Option.is_some t.pending
    || Option.is_some t.control_pending
    || Option.is_some t.external_model
    || Option.is_some t.dispatch_owner
    || t.reconciling
  then invalid "developer terminal settlement is unresolved"
  else (
    let context = H.context invocation in
    let* () = H.validate invocation in
    let* () =
      if
        P.Invocation.equal_status (H.status invocation) Admitted
        && P.Execution_owner.equal context.owner (P.Execution_owner.Developer t.owner)
        && not (Hashtbl.mem t.native_calls context.id)
      then Ok ()
      else invalid "native terminal registration requires a fresh root invocation"
    in
    let* () =
      match job_parent, context.parent_job with
      | None, None -> Ok ()
      | Some parent, Some id
        when Job_scope.owns_parent (Developer_job_worker.issuer t.worker) parent
             && P.Id.Job.equal id (Job_scope.Parent.job_id parent) ->
        Job_scope.Parent.check_now
          parent
          ~owner:t.owner
          ~capture_fingerprint:(Chatml_developer_capture.fingerprint t.capture)
      | None, Some _ | Some _, None | Some _, Some _ ->
        invalid "native terminal job parent lacks this root's physical claim"
    in
    let* terminal_slot =
      Terminal_queue.reserve t.terminals ~kind:Native
      |> Result.map_error ~f:error_of_terminal_queue
    in
    let finished, finish = Eio.Promise.create () in
    let call = { cancel = None; finished; finish } in
    let registration = { call; invocation; job_parent; terminal_slot; scope = None } in
    Hashtbl.set t.native_calls ~key:context.id ~data:registration;
    let outcome =
      match
        Eio.Cancel.sub (fun cancel ->
          call.cancel <- Some cancel;
          Eio.Fiber.with_binding native_call_key t (fun () ->
            let* () =
              match job_parent with
              | None -> Ok ()
              | Some parent ->
                Job_scope.Parent.check
                  parent
                  ~owner:t.owner
                  ~capture_fingerprint:(Chatml_developer_capture.fingerprint t.capture)
            in
            let* snapshot = current t in
            let* () =
              match job_parent with
              | None -> Ok ()
              | Some parent ->
                Job_scope.Parent.check_now
                  parent
                  ~owner:t.owner
                  ~capture_fingerprint:(Chatml_developer_capture.fingerprint t.capture)
            in
            let* staged =
              Staging.create ~max_staged_jobs:t.registry.limits.max_staged_jobs
            in
            let action =
              { issuer = t
              ; snapshot
              ; staged
              ; stage_in_flight = 0
              ; sealed = None
              ; live = false
              ; callback_switch = None
              ; queued = false
              ; call
              ; terminal_slot
              }
            in
            let scope =
              { issuer = t
              ; registration
              ; action
              ; dispatched = None
              ; callback_switch = None
              ; callback_complete = false
              ; bracket_exited = false
              ; sealed = None
              ; submitted = false
              ; dispatch_state = No_attempt
              ; remembered_terminal = None
              }
            in
            registration.scope <- Some scope;
            f ~scope))
      with
      | value -> `Returned value
      | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    Eio.Cancel.protect (fun () ->
      Option.iter registration.scope ~f:(fun scope -> scope.action.live <- false);
      let submitted =
        Option.value_map registration.scope ~default:false ~f:(fun scope ->
          scope.submitted)
      in
      if not submitted
      then (
        let durable_or_unknown =
          match registration.scope with
          | Some scope
            when Option.value_map t.dispatch_owner ~default:false ~f:(phys_equal scope) ->
            true
          | Some scope when Option.is_some scope.remembered_terminal -> true
          | Some scope when Option.is_some scope.dispatched -> true
          | None ->
            (* The scope was never constructed or handed to the callback, so
               this registration could not have initiated dispatch. There is
               no escaped token requiring durable recovery. *)
            false
          | Some _ ->
            (match Store.observe t.handle with
             | Error _ | (exception _) -> true
             | Ok observed ->
               (match
                  Store.Invocation_lookup.prior
                    (Store.find_invocation observed context.id)
                with
                | None -> false
                | Some prior ->
                  (match H.status (Store.Invocation_observation.value prior) with
                   | P.Invocation.Admitted -> false
                   | Dispatching | Resolved _ | Published _ -> true)))
        in
        if durable_or_unknown
        then (
          t.state <- Quarantined;
          t.unresolved_native <- registration :: t.unresolved_native)
        else (
          Option.iter registration.scope ~f:(fun scope -> abort_staged scope.action);
          ignore (Terminal_queue.abandon t.terminals terminal_slot : (unit, _) result)));
      (match Hashtbl.find t.native_calls context.id with
       | Some current when phys_equal current registration ->
         Hashtbl.remove t.native_calls context.id
       | None | Some _ -> ());
      Option.iter registration.scope ~f:(fun scope -> scope.bracket_exited <- true);
      call.cancel <- None;
      Eio.Promise.resolve finish ());
    match outcome with
    | `Returned value -> value
    | `Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
;;

let with_native_callback (scope : native_scope) ~dispatched f =
  let issuer = scope.issuer in
  let admitted =
    Mutation_gate.with_owner issuer.mutation_gate ~f:(fun () ->
      if not (same_domain issuer.registry)
      then invalid "native callback belongs to another domain"
      else if scope.submitted || Option.is_some scope.dispatched
      then invalid "native callback already entered"
      else (
        let context = H.context scope.registration.invocation in
        let* () =
          if
            H.equal_context context (H.context dispatched)
            && P.Invocation.equal_status (H.status dispatched) Dispatching
          then Ok ()
          else invalid "native callback requires its exact dispatched record"
        in
        let* () = drain_terminals_unlocked issuer in
        let* observed =
          Store.observe issuer.handle |> Result.map_error ~f:error_of_store
        in
        let lookup = Store.find_invocation observed context.id in
        let* () =
          match Store.Invocation_lookup.prior lookup with
          | Some prior
            when P.Invocation.equal_status
                   (H.status (Store.Invocation_observation.value prior))
                   Dispatching
                 && H.equal_context
                      (H.context (Store.Invocation_observation.value prior))
                      context -> Ok ()
          | None | Some _ -> invalid "native callback dispatch is not durable"
        in
        let* latest = current issuer in
        let* () =
          if
            phys_equal latest.selected scope.action.snapshot.selected
            && Int.equal
                 latest.selection_revision
                 scope.action.snapshot.selection_revision
          then Ok ()
          else invalid "native callback selection changed"
        in
        let* () =
          match scope.registration.job_parent with
          | None -> Ok ()
          | Some parent ->
            Job_scope.Parent.check
              parent
              ~owner:issuer.owner
              ~capture_fingerprint:(Chatml_developer_capture.fingerprint issuer.capture)
        in
        let* () = check_effect_now issuer latest in
        let* () =
          match scope.registration.job_parent with
          | None -> Ok ()
          | Some parent ->
            Job_scope.Parent.check_now
              parent
              ~owner:issuer.owner
              ~capture_fingerprint:(Chatml_developer_capture.fingerprint issuer.capture)
        in
        scope.dispatched <- Some dispatched;
        scope.action.live <- true;
        Ok ()))
  in
  let* () = admitted in
  let outcome =
    match
      Eio.Switch.run (fun sw ->
        scope.callback_switch <- Some sw;
        Eio.Fiber.with_binding action_key scope.action (fun () -> f ~sw))
    with
    | value -> `Returned value
    | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  scope.action.live <- false;
  scope.callback_switch <- None;
  scope.callback_complete <- true;
  match outcome with
  | `Returned value -> value
  | `Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let close_owner t =
  if not (same_domain t.registry)
  then invalid "developer root owner belongs to another domain"
  else if
    Option.value_map (Eio.Fiber.get root_call_key) ~default:false ~f:(fun current ->
      phys_equal current t)
    || Option.value_map (Eio.Fiber.get native_call_key) ~default:false ~f:(fun current ->
      phys_equal current t)
    || Option.value_map (Eio.Fiber.get control_call_key) ~default:false ~f:(fun current ->
      phys_equal current t)
    || Option.value_map
         (Eio.Fiber.get inference_scope_key)
         ~default:false
         ~f:(fun scope -> phys_equal scope.inference_issuer t)
  then invalid "developer root action cannot close its own owner"
  else (
    match t.state with
    | Closed -> Ok ()
    | Live | Closing | Quarantined ->
      t.state <- Closing;
      let* () =
        Terminal_queue.begin_close t.terminals
        |> Result.map_error ~f:error_of_terminal_queue
      in
      let root_call = t.root_call in
      let control_call = t.control_call in
      let native_calls = Hashtbl.data t.native_calls |> List.map ~f:(fun r -> r.call) in
      Eio.Cancel.protect (fun () ->
        Option.iter root_call ~f:(fun call ->
          Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit));
        Option.iter control_call ~f:(fun call ->
          Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit));
        List.iter native_calls ~f:(fun call ->
          Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit));
        Option.iter root_call ~f:(fun call -> Eio.Promise.await call.finished);
        Option.iter control_call ~f:(fun call -> Eio.Promise.await call.finished);
        List.iter native_calls ~f:(fun call -> Eio.Promise.await call.finished));
      let worker_result =
        match
          Eio.Cancel.protect (fun () -> Developer_job_worker.close_and_wait t.worker)
        with
        | value -> `Returned value
        | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      let pending_result =
        match reconcile_pending t with
        | value -> `Returned value
        | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      let worker_result =
        match worker_result, pending_result with
        | `Returned (Error _), `Returned (Ok ()) ->
          (match
             Eio.Cancel.protect (fun () -> Developer_job_worker.close_and_wait t.worker)
           with
           | value -> `Returned value
           | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
        | (`Raised _ | `Returned (Ok ())), _
        | `Returned (Error _), (`Raised _ | `Returned (Error _)) -> worker_result
      in
      let* () =
        match worker_result, pending_result with
        | `Returned (Ok ()), `Returned (Ok ()) -> Ok ()
        | `Raised (exn, backtrace), _ | _, `Raised (exn, backtrace) ->
          Exn.raise_with_original_backtrace exn backtrace
        | `Returned (Error error), _ | _, `Returned (Error error) -> Error error
      in
      let* () =
        if
          Terminal_queue.is_empty t.terminals
          && List.is_empty t.unresolved_native
          && Option.is_none t.external_model
        then Ok ()
        else invalid "developer terminal custody remains unresolved"
      in
      let* () = Model_inference.reconcile t in
      let* () =
        if t.tracking_finished
        then Ok ()
        else
          let* () =
            Store.Tracking.finish t.tracking |> Result.map_error ~f:error_of_store
          in
          t.tracking_finished <- true;
          Ok ()
      in
      Job_capacity.close_developer_run t.registry.capacity ~owner:t.owner;
      let* () =
        Store.Run_claim.release t.run_claim |> Result.map_error ~f:error_of_store
      in
      t.state <- Closed;
      (match Hashtbl.find t.registry.owners t.owner.run_id with
       | Some (Owned current) when phys_equal current t ->
         Hashtbl.remove t.registry.owners t.owner.run_id
       | None | Some (Admitting _) | Some (Owned _) -> ());
      Ok ())
;;

let close_and_wait = close_owner

module Registry = struct
  type nonrec final_admission = final_admission
  type nonrec t = registry

  let uses_capacity t capacity = phys_equal t.capacity capacity
  let max_active_workers t = t.limits.max_active_workers

  let active_for_scheduler t =
    if t.closed || not (same_domain t)
    then invalid "developer root registry is closed or foreign"
    else
      Hashtbl.data t.owners
      |> List.filter_map ~f:(function
        | Owned run ->
          (match run.state with
           | Live -> Some run
           | Closing | Quarantined | Closed -> None)
        | Admitting _ -> None)
      |> List.sort ~compare:(fun left right ->
        P.Id.Developer_run.compare left.owner.run_id right.owner.run_id)
      |> Result.return
  ;;

  let cleanup_admission t run_id (admission : admission) =
    let* () =
      match admission.worker with
      | None -> Ok ()
      | Some worker ->
        let* () = Developer_job_worker.close_and_wait worker in
        admission.worker <- None;
        Ok ()
    in
    let* () =
      match admission.recovery with
      | None -> Ok ()
      | Some recovery when Store.Recovery.is_finished recovery ->
        admission.recovery <- None;
        Ok ()
      | Some recovery ->
        let* () =
          Store.Recovery.abort_clean recovery |> Result.map_error ~f:error_of_store
        in
        admission.recovery <- None;
        Ok ()
    in
    let* () =
      match admission.tracking with
      | None -> Ok ()
      | Some tracking ->
        let* () = Store.Tracking.finish tracking |> Result.map_error ~f:error_of_store in
        admission.tracking <- None;
        Ok ()
    in
    let* () =
      match admission.claim with
      | None -> Ok ()
      | Some claim ->
        let* () = Store.Run_claim.release claim |> Result.map_error ~f:error_of_store in
        admission.claim <- None;
        Ok ()
    in
    (match Hashtbl.find t.owners run_id with
     | Some (Admitting current) when phys_equal current admission ->
       Hashtbl.remove t.owners run_id
     | None | Some (Admitting _) | Some (Owned _) -> ());
    Ok ()
  ;;

  let close_and_wait t =
    if not (same_domain t)
    then invalid "developer root registry belongs to another domain"
    else if
      Option.value_map (Eio.Fiber.get admission_key) ~default:false ~f:(fun current ->
        phys_equal current t)
      || Option.value_map (Eio.Fiber.get root_call_key) ~default:false ~f:(fun current ->
        phys_equal current.registry t)
      || Option.value_map
           (Eio.Fiber.get native_call_key)
           ~default:false
           ~f:(fun current -> phys_equal current.registry t)
      || Option.value_map
           (Eio.Fiber.get control_call_key)
           ~default:false
           ~f:(fun current -> phys_equal current.registry t)
      || Option.value_map
           (Eio.Fiber.get inference_scope_key)
           ~default:false
           ~f:(fun scope -> phys_equal scope.inference_issuer.registry t)
    then invalid "developer root admission cannot close its own registry"
    else (
      t.closed <- true;
      let admissions =
        Hashtbl.data t.owners
        |> List.filter_map ~f:(function
          | Admitting admission -> Some admission.call
          | Owned _ -> None)
      in
      Eio.Cancel.protect (fun () ->
        List.iter admissions ~f:(fun call ->
          Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
          Eio.Promise.await call.finished));
      let owners =
        Hashtbl.data t.owners
        |> List.filter_map ~f:(function
          | Admitting _ -> None
          | Owned owner -> Some owner)
      in
      let abandoned_admissions =
        Hashtbl.to_alist t.owners
        |> List.filter_map ~f:(fun (run_id, slot) ->
          match slot with
          | Admitting admission -> Some (run_id, admission)
          | Owned _ -> None)
      in
      let first_error = ref None in
      let first_exception = ref None in
      Eio.Cancel.protect (fun () ->
        List.iter abandoned_admissions ~f:(fun (run_id, admission) ->
          match cleanup_admission t run_id admission with
          | Ok () -> ()
          | Error error -> if Option.is_none !first_error then first_error := Some error
          | exception exn ->
            if Option.is_none !first_exception
            then first_exception := Some (exn, Stdlib.Printexc.get_raw_backtrace ()));
        List.iter owners ~f:(fun owner ->
          match close_owner owner with
          | Ok () -> ()
          | Error error -> if Option.is_none !first_error then first_error := Some error
          | exception exn ->
            if Option.is_none !first_exception
            then first_exception := Some (exn, Stdlib.Printexc.get_raw_backtrace ())));
      match !first_exception, !first_error with
      | Some (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
      | None, Some error -> Error error
      | None, None -> Ok ())
  ;;

  let create ~sw ~store ~capacity ~now ~limits =
    let t =
      { store
      ; capacity
      ; now
      ; limits
      ; switch = sw
      ; domain_id = (Stdlib.Domain.self () :> int)
      ; owners = Hashtbl.create (module P.Id.Developer_run)
      ; closed = false
      }
    in
    Eio.Switch.check sw;
    (* The Store hook was registered when the caller opened it. Eio runs
       release hooks in reverse registration order, so this joins root actions
       and releases run claims before the Store hook. Explicit host shutdown
       still reports any unresolved quarantine to its caller. *)
    Eio.Switch.on_release sw (fun () ->
      match close_and_wait t with
      | Ok () -> ()
      | Error _ -> failwith "developer root registry shutdown is incomplete");
    t
  ;;

  let verify_admission t (admission : admission) =
    let owner = Store.owner admission.handle in
    let principal = Operator_authorization.principal admission.actor in
    if
      P.Id.Principal.equal principal.id owner.principal_id
      && Operator_authorization.is_current admission.actor
      && Chatml_source.Digest.equal
           (Chatml_developer_capture.fingerprint admission.capture)
           (Store.capture_fingerprint admission.handle)
      && (not (String.is_empty admission.workspace_conflict_domain))
      && not t.closed
    then
      let* () =
        match admission.selected_model with
        | None -> Ok ()
        | Some selection ->
          if
            phys_equal
              (Provider_operator_port.Model_selection.actor selection)
              admission.actor
          then Provider_operator_port.Model_selection.check_current selection
          else invalid "developer model selection belongs to another actor"
      in
      selected_within ~registry:admission.registry admission.selected
    else invalid "developer root actor or capture does not match"
  ;;

  let publish_admission t (admission : admission) ~checkpoint =
    let handle = admission.handle in
    let owner = Store.owner handle in
    let* token =
      match admission.claim with
      | Some token -> Ok token
      | None -> invalid "developer root physical claim is missing"
    in
    let* () =
      match admission.recovery with
      | Some recovery when Store.Recovery.is_finished recovery -> Ok ()
      | None | Some _ -> invalid "developer terminal recovery is unfinished"
    in
    let* tracking =
      match admission.tracking with
      | Some tracking -> Ok tracking
      | None ->
        let* tracking =
          Store.Tracking.open_ ~claim:token ~handle |> Result.map_error ~f:error_of_store
        in
        admission.tracking <- Some tracking;
        Ok tracking
    in
    let* active = Store.is_latest_active handle |> Result.map_error ~f:error_of_store in
    let* () = verify_admission t admission in
    let* () = if active then Ok () else invalid "developer root admission expired" in
    let* terminals =
      Terminal_queue.create ~max_native_occupancy:t.limits.max_native_occupancy
      |> Result.map_error ~f:error_of_terminal_queue
    in
    let project_source = project_source admission.capture in
    let* worker =
      Developer_job_worker.create
        ~close_policy:Retain_unclaimed
        ~sw:t.switch
        ~store:t.store
        ~initial:handle
        ~capacity:t.capacity
        ~project_source
        ~workspace_conflict_domain:admission.workspace_conflict_domain
        ~now:t.now
        ~max_active:t.limits.max_active_workers
        ~max_quarantined:t.limits.max_quarantined_workers
        ()
    in
    admission.worker <- Some worker;
    let* () = verify_admission t admission in
    let* final_check =
      match admission.final_admission with
      | None -> Ok (fun () -> Ok ())
      | Some prepare ->
        let* check =
          prepare ~actor:admission.actor ~capture:admission.capture ~checkpoint
        in
        let* () = verify_admission t admission in
        Ok check
    in
    let run =
      { registry = t
      ; actor = admission.actor
      ; capture = admission.capture
      ; owner
      ; handle
      ; run_claim = token
      ; tracking
      ; tracking_finished = false
      ; worker
      ; project_source
      ; workspace_conflict_domain = admission.workspace_conflict_domain
      ; job_policy = admission.job_policy
      ; native_ceiling = admission.selected
      ; selected = admission.selected
      ; selected_model = admission.selected_model
      ; selection_revision = 0
      ; state = Live
      ; root_call = None
      ; active = None
      ; native_calls = Hashtbl.create (module P.Id.Invocation)
      ; unresolved_native = []
      ; terminals
      ; mutation_gate = Mutation_gate.create ()
      ; pending = None
      ; control_pending = None
      ; control_call = None
      ; dispatch_owner = None
      ; external_model = None
      ; effect_revision = 0
      ; reconciling = false
      ; inference_scopes = []
      ; inference_routes = []
      ; inference_active = 0
      ; inference_bindings = 0
      ; inference_serial = 0
      ; result_binding = Result_unbound
      }
    in
    let* () = final_check () in
    Hashtbl.set t.owners ~key:owner.run_id ~data:(Owned run);
    Ok run
  ;;

  let drive_recovery t (admission : admission) ~recover ~checkpoint =
    let run_id = (Store.owner admission.handle).run_id in
    let call = admission.call in
    let outcome =
      match
        Eio.Cancel.sub (fun cancel ->
          call.cancel <- Some cancel;
          Eio.Fiber.with_binding admission_key t (fun () ->
            let* () = verify_admission t admission in
            let* recovery =
              match admission.recovery with
              | Some recovery -> Ok recovery
              | None ->
                let* claim =
                  match admission.claim with
                  | Some claim -> Ok claim
                  | None ->
                    let* claim =
                      Store.Run_claim.acquire ~store:t.store ~handle:admission.handle
                      |> Result.map_error ~f:error_of_store
                    in
                    admission.claim <- Some claim;
                    Ok claim
                in
                let* recovery =
                  Store.Recovery.open_ ~claim ~handle:admission.handle
                  |> Result.map_error ~f:error_of_store
                in
                admission.recovery <- Some recovery;
                Ok recovery
            in
            let* () = recover recovery in
            publish_admission t admission ~checkpoint))
      with
      | value -> `Returned value
      | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let cleanup =
      match outcome with
      | `Returned (Ok _) -> `Returned (Ok ())
      | `Returned (Error _) | `Raised _ ->
        (match Eio.Cancel.protect (fun () -> cleanup_admission t run_id admission) with
         | value -> `Returned value
         | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
    in
    call.cancel <- None;
    admission.running <- false;
    Eio.Promise.resolve call.finish ();
    match outcome, cleanup with
    | `Returned (Ok run), _ -> Ok run
    | `Returned (Error original), `Returned (Ok ()) -> Error original
    | `Returned (Error _), `Returned (Error cleanup) -> Error cleanup
    | `Returned (Error _), `Raised (exn, backtrace) ->
      Exn.raise_with_original_backtrace exn backtrace
    | `Raised (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
  ;;

  let admit_with_recovery
        ?final_admission
        ?checkpoint
        t
        ~actor
        ~capture
        ~registry
        ~selected
        ~selected_model
        ~job_policy
        ~handle
        ~workspace_conflict_domain
        ~recover
    =
    if Option.is_some final_admission && Option.is_none checkpoint
    then invalid "developer final admission needs a fresh checkpoint"
    else if t.closed || not (same_domain t)
    then invalid "developer root registry is closed or foreign"
    else (
      let run_id = (Store.owner handle).run_id in
      if
        Hashtbl.mem t.owners run_id || Hashtbl.length t.owners >= t.limits.max_active_runs
      then invalid "developer run owner slot is unavailable"
      else (
        let finished, finish = Eio.Promise.create () in
        let call = { cancel = None; finished; finish } in
        let admission =
          { call
          ; claim = None
          ; worker = None
          ; tracking = None
          ; recovery = None
          ; actor
          ; capture
          ; registry
          ; selected
          ; selected_model
          ; job_policy
          ; handle
          ; workspace_conflict_domain
          ; final_admission
          ; running = true
          }
        in
        Hashtbl.set t.owners ~key:run_id ~data:(Admitting admission);
        drive_recovery
          t
          admission
          ~recover
          ~checkpoint:(Option.value checkpoint ~default:Fn.id)))
  ;;

  let admit
        ?final_admission
        ?checkpoint
        t
        ~actor
        ~capture
        ~registry
        ~selected
        ~selected_model
        ~job_policy
        ~handle
        ~workspace_conflict_domain
    =
    admit_with_recovery
      ?final_admission
      ?checkpoint
      t
      ~actor
      ~capture
      ~registry
      ~selected
      ~selected_model
      ~job_policy
      ~handle
      ~workspace_conflict_domain
      ~recover:(fun recovery ->
        let* view =
          Store.Recovery.observe recovery |> Result.map_error ~f:error_of_store
        in
        if List.is_empty (Store.Recovery.View.candidates view)
        then Store.Recovery.finish recovery |> Result.map_error ~f:error_of_store
        else invalid "abandoned developer job requires terminal recovery")
  ;;

  let resume_recovery ?checkpoint t ~actor ~handle ~recover =
    if t.closed || not (same_domain t)
    then invalid "developer root registry is closed or foreign"
    else (
      let run_id = (Store.owner handle).run_id in
      match Hashtbl.find t.owners run_id with
      | Some (Admitting admission)
        when (not admission.running)
             && phys_equal admission.actor actor
             && phys_equal admission.handle handle
             && Option.is_some admission.recovery ->
        if Option.is_some admission.final_admission && Option.is_none checkpoint
        then invalid "developer resumed final admission needs a fresh checkpoint"
        else (
          let finished, finish = Eio.Promise.create () in
          admission.call <- { cancel = None; finished; finish };
          admission.running <- true;
          drive_recovery
            t
            admission
            ~recover
            ~checkpoint:(Option.value checkpoint ~default:Fn.id))
      | None | Some (Owned _) | Some (Admitting _) ->
        invalid "developer terminal recovery does not belong to this actor")
  ;;
end
