open! Core
module Activation_custody = Recurrence_activation_custody

module Recurrence_activation = struct
  type t = Activation_custody.t

  let origin = Activation_custody.origin

  module Checked = struct
    type t =
      { check_current : unit -> (unit, Agent_protocol.Error.t) result
      ; check_basis : unit -> (unit, Agent_protocol.Error.t) result
      }

    let check_current t = t.check_current ()
    let check_basis t = t.check_basis ()
  end
end

module Recurrence_planning_read = struct
  type t =
    { entries : Recurrence_view.t list
    ; planning : Recurrence_planning_status.t
    }
  [@@deriving sexp_of]
end

module Recurrence_creation = struct
  module Checked = struct
    type t =
      { definition : Recurrence_definition.t
      ; principal : Agent_protocol.Principal.t
      ; check_current : unit -> (unit, Agent_protocol.Error.t) result
      }

    let definition t = t.definition
    let principal t = t.principal
    let check_current t = t.check_current ()
  end

  type t =
    { owner : unit ref
    ; actor : Operator_authorization.t
    ; authorize :
        Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result
    ; reservation : Recurrence_child_reservation.t
    }

  let reservation t = t.reservation
end

module Recurrence_child_link = struct
  type t =
    { association : Recurrence_child_association.t
    ; controller : Recurrence_controller.t
    ; check_current : unit -> (unit, Agent_protocol.Error.t) result
    }

  let association t = t.association
  let check_current t = t.check_current ()
end

module Recurrence_run_observation = struct
  type t =
    { origin : Recurrence_run_origin.t
    ; terminal : Recurrence_run_reference.t option
    ; controller : Recurrence_controller.t
    ; check_current : unit -> (unit, Agent_protocol.Error.t) result
    ; check_basis : unit -> (unit, Agent_protocol.Error.t) result
    ; validate_reference :
        Recurrence_run_reference.t -> (unit, Agent_protocol.Error.t) result
    ; completion : Recurrence_runtime.Completion.t option
    ; check_admitted : unit -> (unit, Agent_protocol.Error.t) result
    }

  let origin t = t.origin
  let terminal t = t.terminal
  let check_current t = t.check_current ()
end

module Recurrence_import = struct
  module Timing = struct
    type t =
      | No_successor
      | Live_phase
      | Recovery_adoption
      | Held of Agent_protocol.Error.t
  end

  type t =
    { settlement : Recurrence_settlement.t
    ; timing : Timing.t
    }
end

module Recurrence_import_candidate = struct
  type t =
    { admission : Recurrence_settlement_admission.t
    ; transition : Session_transition.t
    ; phase : Recurrence_runtime.Completion_phase.t option
    ; timing : Recurrence_import.Timing.t
    }
end

module Runtime_retirement = struct
  type outcome =
    ((unit, Agent_protocol.Error.t) result, exn * Stdlib.Printexc.raw_backtrace) result

  type t = outcome Eio.Promise.t

  let is_finished t = Option.is_some (Eio.Promise.peek t)

  let await t =
    match Eio.Promise.await t with
    | Ok result -> result
    | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  ;;
end

type runtime_retirement =
  { finished : Runtime_retirement.t
  ; finish : Runtime_retirement.outcome Eio.Promise.u
  ; mutable pending_operation : Agent_protocol.Id.Operation.t option
  ; mutable closing : bool
  }

module Inference_owner = Inference_graph_owner
module Graph_owner = Inference_graph_owner.Host

let equal_agent_entry = Agent_protocol.History.equal_entry

type agent_custody = Execution_custody.t
type agent_admission = Execution_custody.Admission.t

let create_agent_custody = Execution_custody.create
let transfer_agent_custody = Execution_custody.transfer
let retire_agent_custody = Execution_custody.retire
let create_agent_admission = Execution_custody.Admission.create
let retire_agent_admission = Execution_custody.Admission.retire
let check_agent_admission = Execution_custody.Admission.check
let transfer_agent_admission = Execution_custody.Admission.transfer

type lifecycle_continuation =
  | Pending of agent_admission
  | Consumed

type agent_intent_key =
  | Event_intent of Agent_protocol.Id.Moderator_execution.t
  | Invocation_intent of Agent_protocol.Id.Invocation.t
[@@deriving equal]

type agent_intent_custody =
  { key : agent_intent_key
  ; generation : int
  ; source : Agent_protocol.Invocation.observer option
  ; custody : agent_custody option
  ; observation : Observation_continuation.t option
  }

module Follow_up_authority = struct
  type t =
    { custody : agent_custody option
    ; observations : Observation_continuation.Selection.t option
    }
end

type lifecycle_agent_custody =
  | Unadmitted
  | Admitted of
      { generation : int
      ; source : Agent_protocol.Invocation.observer option
      ; pending : lifecycle_continuation
      }

(* Private transfer plan for the FIRST authored-start lifecycle ACK. Durable
   receipt retries never construct it; installed custody belongs to normal Actor
   retirement, not to this speculative plan's cleanup. *)
module Authored_run_acknowledgement : sig
  type t

  val require_unadmitted
    :  lifecycle_agent_custody
    -> (unit, Agent_protocol.Error.t) result

  val prepare
    :  previous:Session_state.t
    -> authorities:Run_authorities.t
    -> lifecycle:lifecycle_agent_custody
    -> scope:Run_admission.Scope.t
    -> request:Run_admission_request.t
    -> run:Agent_protocol.Run.t
    -> receipt:Agent_protocol.Run_receipt.t
    -> (t, Agent_protocol.Error.t) result

  val scope : t -> Run_admission.Scope.t

  (** Callback-free exact old ownership and new first-ACK candidate facts. *)
  val check_basis
    :  t
    -> previous:Session_state.t
    -> authorities:Run_authorities.t
    -> lifecycle:lifecycle_agent_custody
    -> candidate:Session_transition.t
    -> (unit, Agent_protocol.Error.t) result

  val install : t -> Run_authorities.t * lifecycle_agent_custody option
  val discard : t -> unit
end = struct
  module P = Agent_protocol

  module Phase = struct
    type t =
      | Staged
      | Installed
      | Discarded
  end

  type t =
    { previous : Session_state.t
    ; previous_authorities : Run_authorities.t
    ; scope : Run_admission.Scope.t
    ; run : P.Run.t
    ; receipt : P.Run_receipt.t
    ; installed : Run_authorities.t * lifecycle_agent_custody option
    ; mutable phase : Phase.t
    }

  let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

  let require_unadmitted = function
    | Unadmitted -> Ok ()
    | Admitted _ -> conflict "authored startup already has lifecycle custody"
  ;;

  let prepare ~previous ~authorities ~lifecycle ~scope ~request ~run ~receipt =
    let open Result.Let_syntax in
    let%bind () = require_unadmitted lifecycle in
    let%bind () =
      match
        ( request.Run_admission_request.input
        , previous.Session_state.lifecycle.desired
        , previous.lifecycle.observed
        , previous.active_operation )
      with
      | Authored_start, Stopped, Stopped, None -> Ok ()
      | _ -> conflict "authored startup handoff requires its unstarted owner"
    in
    let%map installed_authorities =
      Run_authorities.add_for_caller authorities ~run ~scope ~caller:request.caller
    in
    let pending =
      Option.map (Run_admission.Scope.original_actor scope) ~f:(fun original ->
        Admitted
          { generation = run.P.Run.source.generation
          ; source = Some run.source.observer
          ; pending =
              Pending (create_agent_admission (Some (create_agent_custody original)))
          })
    in
    { previous
    ; previous_authorities = authorities
    ; scope
    ; run
    ; receipt
    ; installed = installed_authorities, pending
    ; phase = Staged
    }
  ;;

  let scope t = t.scope

  let check_basis t ~previous ~authorities ~lifecycle ~candidate =
    let open Result.Let_syntax in
    let%bind () = require_unadmitted lifecycle in
    let%bind () =
      match t.phase with
      | Staged
        when phys_equal t.previous previous
             && phys_equal t.previous_authorities authorities -> Ok ()
      | Staged | Installed | Discarded ->
        conflict "authored startup handoff owner changed"
    in
    match candidate.Session_transition.state.run_state with
    | None -> conflict "authored startup handoff lost its Run"
    | Some index ->
      let%bind receipt =
        Run_state.receipt
          index
          ~principal_id:t.receipt.principal_id
          ~key:t.receipt.key
          ~request_sha256:t.receipt.request_sha256
      in
      if
        Option.exists (Run_state.find index t.run.id) ~f:(P.Run.equal t.run)
        && Option.exists receipt ~f:(P.Run_receipt.equal t.receipt)
        && Int64.equal candidate.state.counters.revision t.receipt.session_revision
        && P.Session.equal_desired_state candidate.state.lifecycle.desired Running
        && (match candidate.state.lifecycle.observed with
            | Starting -> true
            | _ -> false)
        && Option.is_none candidate.state.active_operation
      then Ok ()
      else conflict "authored startup handoff differs from its first ACK"
  ;;

  let install t =
    t.phase <- Installed;
    t.installed
  ;;

  let discard t =
    match t.phase with
    | Installed | Discarded -> ()
    | Staged ->
      t.phase <- Discarded;
      Option.iter (snd t.installed) ~f:(function
        | Admitted { pending = Pending admission; _ } -> retire_agent_admission admission
        | Unadmitted | Admitted { pending = Consumed; _ } -> ())
  ;;
end

module Initialization_scope = struct
  module Purpose = struct
    type t =
      | Pending_initialization
      | Runtime_construction
    [@@deriving equal, sexp_of]
  end

  module Acknowledged = struct
    module Permission_basis = struct
      type t =
        { permissions : Agent_protocol.Permission.t list
        ; grants : Agent_protocol.Grant.t list
        ; lifecycle : Session_state.Lifecycle.t
        }

      let capture (state : Session_state.t) =
        { permissions = state.permissions
        ; grants = state.grants
        ; lifecycle = state.lifecycle
        }
      ;;
    end

    type t =
      { moderator : Jsonaf.t option
      ; run_state : Run_state.t option
      ; moderator_executions : Agent_protocol.Moderator_execution.t list
      ; invocations : Agent_protocol.Invocation.t list
      ; permission_basis : Permission_basis.t
      }

    let capture (state : Session_state.t) =
      { moderator = state.moderator
      ; run_state = state.run_state
      ; moderator_executions = state.moderator_executions
      ; invocations = state.invocations
      ; permission_basis = Permission_basis.capture state
      }
    ;;

    let apply t (expected : Session_state.t) =
      { expected with
        moderator = t.moderator
      ; run_state = t.run_state
      ; moderator_executions = t.moderator_executions
      ; invocations = t.invocations
      ; permissions = t.permission_basis.permissions
      ; grants = t.permission_basis.grants
      ; lifecycle = t.permission_basis.lifecycle
      }
    ;;
  end

  module Callback = struct
    type t =
      | Unclaimed
      | Claimed of Agent_protocol.Id.Moderator_execution.t
      | Settled of Agent_protocol.Id.Moderator_execution.t
    [@@deriving equal]
  end

  module Resource = struct
    type t =
      { manager : Chat_response.Moderator_manager.t
      ; source : Agent_protocol.Run_source.t
      ; ordinals : int64 list
      ; schedules : Agent_protocol.Id.Schedule.t list
      ; handed_off : bool
      }

    let select previous (state : Session_state.t) ~manager ~source ~ordinals ~schedules =
      let open Result.Let_syntax in
      let%bind previous =
        match previous with
        | None ->
          Ok ({ manager; source; ordinals = []; schedules = []; handed_off = false } : t)
        | Some previous ->
          if
            phys_equal previous.manager manager
            && Agent_protocol.Run_source.equal previous.source source
            && not previous.handed_off
          then Ok previous
          else
            Error
              (Agent_protocol.Error.create
                 Conflict
                 ~message:"constructor resource changed before its checkpoint"
                 ~retryable:false
                 ())
      in
      let%map metadata = Moderator_checkpoint.queue state.moderator in
      let retained_ordinals =
        Option.value_map metadata ~default:[] ~f:(fun metadata ->
          List.map (Moderator_queue.entries metadata) ~f:Moderator_queue.Entry.ordinal)
      in
      { previous with
        ordinals =
          List.filter (previous.ordinals @ ordinals) ~f:(fun ordinal ->
            List.mem retained_ordinals ordinal ~equal:Int64.equal)
          |> List.dedup_and_sort ~compare:Int64.compare
      ; schedules =
          List.filter (previous.schedules @ schedules) ~f:(fun id ->
            List.exists state.schedules ~f:(fun schedule ->
              Agent_protocol.Id.Schedule.equal id schedule.id))
          |> List.dedup_and_sort ~compare:Agent_protocol.Id.Schedule.compare
      }
    ;;
  end

  type t =
    { owner : unit ref
    ; purpose : Purpose.t
    ; expected : Session_state.t
    ; mutable acknowledged : Acknowledged.t
    ; mutable jobs : Agent_protocol.Id.Job.t list
    ; mutable callback : Callback.t
    ; mutable resource : Resource.t option
    ; run_preparation : Run_preparation.t option
    ; agent_admission : agent_admission
    }

  module Acknowledgement = struct
    type scope = t

    (* Only the actual constructor checkpoint/claim planners prepare this private
       candidate. It changes the currentness projection, never the immutable
       control/Selection basis or original Admission. *)
    type t =
      { scope : scope
      ; previous : Acknowledged.t
      ; previous_callback : Callback.t
      ; previous_resource : Resource.t option
      ; previous_jobs : Agent_protocol.Id.Job.t list
      ; callback : Callback.t
      ; resource : Resource.t option
      ; jobs : Agent_protocol.Id.Job.t list
      ; incoming : Acknowledged.t
      }

    let prepare
          ?callback
          ?resource
          ?jobs
          ?permission_checkpoint
          scope
          (state : Session_state.t)
      =
      { scope
      ; previous = scope.acknowledged
      ; previous_callback = scope.callback
      ; previous_resource = scope.resource
      ; previous_jobs = scope.jobs
      ; callback = Option.value callback ~default:scope.callback
      ; resource =
          (match resource with
           | Some _ -> resource
           | None -> scope.resource)
      ; jobs = Option.value jobs ~default:scope.jobs
      ; incoming =
          (* An ordinary constructor ACK must not absorb unrelated permissions,
             grants or lifecycle changes. Only a selected validated DATA plan
             advances this subprojection. *)
          { (Acknowledged.capture state) with
            permission_basis =
              Option.value_map
                permission_checkpoint
                ~default:scope.acknowledged.permission_basis
                ~f:(fun checkpoint ->
                  Acknowledged.Permission_basis.capture
                    (Constructor_permission_checkpoint.apply checkpoint scope.expected))
          }
      }
    ;;

    let check_basis t =
      if
        phys_equal t.scope.acknowledged t.previous
        && Callback.equal t.scope.callback t.previous_callback
        && Option.equal phys_equal t.scope.resource t.previous_resource
        && phys_equal t.scope.jobs t.previous_jobs
      then Ok ()
      else
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"constructor acknowledged checkpoint changed before commit"
             ~retryable:false
             ())
    ;;

    let expected t = Acknowledged.apply t.incoming t.scope.expected

    (* Caller has checked the exact scope and prepared candidate before ACK.
       This assignment is nonyielding and cannot fail after durable acceptance. *)
    let install t =
      t.scope.acknowledged <- t.incoming;
      t.scope.callback <- t.callback;
      t.scope.resource <- t.resource;
      t.scope.jobs <- t.jobs
    ;;
  end
end

type persistence =
  { commit :
      command_audit:Document_schema.Document.t option
      -> previous:Session_state.t
      -> Session_transition.t
      -> (unit, Agent_protocol.Error.t) result
  ; archive_reference :
      previous:Session_state.t
      -> kind:Session_state.Compaction_archive.kind
      -> Agent_protocol.Id.Operation.t
      -> (Session_state.Compaction_archive.t, Agent_protocol.Error.t) result
  }

type services =
  { now : unit -> Agent_protocol.Timestamp.t
  ; monotonic_now : unit -> Mtime.t
  ; create_attachment_id : unit -> Agent_protocol.Id.Attachment.t
  ; create_reclaim_token : unit -> string
  ; state_committed : Session_state.t -> Agent_protocol.Event.Durable.t list -> unit
  ; job_results : Agent_store.Job_result_store.Publisher.t option
  ; subscription_limits : Staged_subscriptions.limits
  ; schedule_limits : Staged_schedules.limits
  ; notification_limits : Staged_notifications.limits
  ; ingress_limits : Staged_ingress.limits
  }

module Compaction_inference = struct
  type t =
    { with_execution :
        'a.
        operation:Agent_protocol.Operation.t
        -> selection:Inference.Selection.t
        -> borrow_agent_actor:
             (unit -> (Operator_authorization.t option, Agent_protocol.Error.t) result)
        -> (Inference_client.Execution.t
            -> guidance:Inference.Request.Guidance.t option
            -> reserved_input_tokens:int
            -> ('a, Agent_protocol.Error.t) Result.t)
        -> ('a, Agent_protocol.Error.t) Result.t
    }
end

type submission =
  { session : Agent_protocol.Session.t
  ; history_id : Agent_protocol.History.Id.t
  ; disposition : Agent_protocol.Method_result.Send_message.disposition
  ; operation_id : Agent_protocol.Id.Operation.t option
  }

type reset_options = Administration.reset_options =
  { keep_history : bool
  ; keep_tasks : bool
  ; keep_grants : bool
  ; keep_labels : bool
  ; workspace_instance : Workspace_instance.t option
  }

module Extension_change = struct
  type t =
    | Invocation of Agent_protocol.Invocation.t
    | Subscription of Agent_protocol.Subscription.t
    | Delivery of Agent_protocol.Delivery.t
    | Publish of Agent_protocol.Delivery.t * Agent_protocol.History.entry
    | Start_job of Agent_protocol.Job.t
    | Schedule of Agent_protocol.Schedule.t
    | Moderator_state of Jsonaf.t option
end

type job_scope =
  { job : Agent_protocol.Job.t
  ; progress : Job_progress_buffer.t
  ; deadline : Agent_protocol.Timestamp.t option
  ; cancelled : unit Eio.Promise.t
  ; cancel_signal : unit Eio.Promise.u
  ; mutable active : bool
  ; mutable cancel_requested : bool
  ; mutable unfinished_on_return : bool
  ; authority_live : bool Atomic.t
  ; mutable cancel : (unit -> unit) option
  }

type moderator_borrow_kind =
  | Invocation
  | Observation

type moderator_borrow =
  { operation_id : Agent_protocol.Id.Operation.t option
  ; job_scope : job_scope option
  ; kind : moderator_borrow_kind
  ; observation : Observation_continuation.t option
  ; invocation : Agent_protocol.Invocation.t
  ; mutable committed : bool
  ; mutable accepts_children : bool
  ; mutable callback_finished : bool
  ; authority_live : bool Atomic.t
  ; mutable cancel : (unit -> unit) option
  ; mutable cancel_requested : bool
  }

type event_borrow_kind =
  | Queued
  | Ordinary

module Queued_agent_admission = struct
  type t =
    | Contextual of agent_admission option
    | Resumed_run of agent_admission option

  let admission = function
    | Contextual admission | Resumed_run admission -> admission
  ;;
end

type queued_event_borrow =
  { kind : event_borrow_kind
  ; job_scope : job_scope option
  ; agent_admission : Queued_agent_admission.t
  ; receipt : Agent_protocol.Moderator_execution.t
  ; before : Session.Moderator_state.Identity_snapshot.t
  ; event : Session.Snapshot.t
  ; retirement_reason : string option
  ; mutable callback_active : bool
  ; authority_live : bool Atomic.t
  ; mutable run_actions : Run_action_service.t option
  ; mutable committed : bool
  ; mutable cancel : (unit -> unit) option
  ; mutable cancel_requested : bool
  }

(* The queued claim and its exact resumed-Run loan share one durable ACK.
   Staged cleanup cannot retire a loan already installed before publication. *)
module Queued_claim_acknowledgement : sig
  type t

  val prepare
    :  previous:Session_state.t
    -> authorities:Run_authorities.t
    -> resumed:Agent_protocol.Id.Run.t list
    -> candidate:Session_transition.t
    -> borrow:queued_event_borrow
    -> (t, Agent_protocol.Error.t) result

  val scope : t -> Run_admission.Scope.t option

  (** No policy, original-proof or clock callbacks. *)
  val check_basis
    :  t
    -> previous:Session_state.t
    -> authorities:Run_authorities.t
    -> current_borrow:queued_event_borrow option
    -> candidate:Session_transition.t
    -> (unit, Agent_protocol.Error.t) result

  val install : t -> queued_event_borrow
  val discard : t -> unit
end = struct
  module P = Agent_protocol

  module Phase = struct
    type t =
      | Staged
      | Installed
      | Discarded
  end

  type t =
    { previous : Session_state.t
    ; candidate : Session_transition.t
    ; binding : (P.Id.Run.t * Run_authorities.binding) option
    ; borrow : queued_event_borrow
    ; mutable phase : Phase.t
    }

  let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

  let prepare
        ~(previous : Session_state.t)
        ~authorities
        ~resumed
        ~(candidate : Session_transition.t)
        ~(borrow : queued_event_borrow)
    =
    let open Result.Let_syntax in
    let%bind binding =
      match resumed with
      | [] -> Ok None
      | [ run_id ] ->
        let%bind binding =
          Result.of_option
            (Run_authorities.find authorities run_id)
            ~error:
              (P.Error.create
                 Permission_denied
                 ~message:"run no longer has live host authority"
                 ~retryable:false
                 ())
        in
        let%bind run =
          Result.of_option
            (Option.bind candidate.Session_transition.state.run_state ~f:(fun index ->
               Run_state.find index run_id))
            ~error:(P.Error.invalid_request "queued claim lost its resumed Run")
        in
        if
          Int.equal run.source.generation borrow.receipt.context.generation
          && P.Invocation.equal_observer run.source.observer borrow.receipt.context.source
          && P.Invocation.equal_observer
               run.source.observer
               (Run_admission.Scope.observer (Run_authorities.scope binding))
          && List.exists run.owned_work ~f:(fun work ->
            match work.P.Run_work.key with
            | Retained (Moderator_execution id) ->
              P.Id.Moderator_execution.equal id borrow.receipt.context.id
            | Operation _ | Retained _ -> false)
        then Ok (Some (run_id, binding))
        else conflict "queued claim differs from its actual resumed Run"
      | _ :: _ :: _ -> conflict "queued claim has more than one resumed Run"
    in
    let admission =
      match binding with
      | None -> Queued_agent_admission.Contextual None
      | Some (_, binding) ->
        Resumed_run
          (Option.map
             (Run_admission.Scope.original_actor (Run_authorities.scope binding))
             ~f:(fun original ->
               create_agent_admission (Some (create_agent_custody original))))
    in
    Ok
      { previous
      ; candidate
      ; binding
      ; borrow = { borrow with agent_admission = admission }
      ; phase = Staged
      }
  ;;

  let scope t =
    Option.map t.binding ~f:(fun (_, binding) -> Run_authorities.scope binding)
  ;;

  let check_basis t ~previous ~authorities ~current_borrow ~candidate =
    match t.phase with
    | Staged
      when phys_equal previous t.previous
           && phys_equal candidate t.candidate
           && Option.is_none current_borrow
           && Option.for_all t.binding ~f:(fun (run_id, binding) ->
             Run_authorities.is_current authorities binding ~run_id) -> Ok ()
    | Staged | Installed | Discarded -> conflict "queued claim custody changed before ACK"
  ;;

  let install t =
    t.phase <- Installed;
    t.borrow
  ;;

  let discard t =
    match t.phase with
    | Installed | Discarded -> ()
    | Staged ->
      t.phase <- Discarded;
      Atomic.set t.borrow.authority_live false;
      Option.iter
        (Queued_agent_admission.admission t.borrow.agent_admission)
        ~f:retire_agent_admission
  ;;
end

module Callback_scope = struct
  (* Source bootstrap alone never creates a continuation. Only an actual live
     parent admission lends custody; historical absence is not legacy None. *)
  type custody =
    | Unavailable
    | Admitted of agent_admission

  type t =
    { manager : Chat_response.Moderator_manager.t
    ; constructor_scope : Initialization_scope.t option
    ; callback : Chat_response.Moderator_manager.Callback.t
    ; borrow : queued_event_borrow
    ; source : Agent_protocol.Run_source.t
    ; origin : Agent_protocol.Inference_origin.t option
    ; custody : custody
    ; mutable committed_checkpoint : Jsonaf.t option option
    }

  let executing t = t.borrow.receipt
  let source t = t.source
  let origin t = t.origin
  let committed_checkpoint t = t.committed_checkpoint
end

type legacy_callback_handler =
  scope:Callback_scope.t
  -> commit:
       (snapshot:Session.Moderator_state.Identity_snapshot.t
        -> requests:Agent_protocol.Invocation.follow_up
        -> notifications:string list
        -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result

type legacy_callback_claim =
  callback:Chat_response.Moderator_manager.Callback.t
  -> legacy_callback_handler
  -> (bool, Agent_protocol.Error.t) result

type legacy_callback_parent =
  | Callback_operation of Agent_protocol.Id.Operation.t option
  | Callback_job of job_scope

type invocation_owner =
  | Foreground of Agent_protocol.Id.Operation.t
  | Invocation_moderator of moderator_borrow
  | Event_moderator of queued_event_borrow
  | Background_job of job_scope

type delegated_event_claim =
  | Delegated_unavailable
  | Delegated_borrow of queued_event_borrow
  | Delegated_replay of Agent_protocol.Moderator_execution.t

type invocation_execution =
  { owner : invocation_owner
  ; dispatched : Agent_protocol.Invocation.t
  ; mutable accepts_children : bool
  ; authority_live : bool Atomic.t
  }

(* References to the EXISTING physical execution owners. No second owner index
   or caller-supplied Goal identity is introduced. *)
type inference_execution =
  | Inference_operation of Agent_protocol.Operation.t * bool Atomic.t
  | Inference_invocation of invocation_execution
  | Inference_moderator of moderator_borrow
  | Inference_event of queued_event_borrow
  | Inference_job of job_scope
  | Inference_initialization of Initialization_scope.t
  | Inference_initialization_job of Initialization_scope.t * Agent_protocol.Job.t

module History_edit_result = struct
  type t =
    { session : Agent_protocol.Session.t
    ; history_id : Agent_protocol.History.Id.t
    ; content_revision : Agent_protocol.History.Content_revision.t
    ; archived_revision : int64
    ; continuation : Agent_protocol.History_edit.Continuation.t
    }
end

module History_continuation_result = struct
  type t =
    { session : Agent_protocol.Session.t
    ; continuation : Agent_protocol.History_edit.Continuation.t
    }
end

module Lifecycle_fence = struct
  type phase =
    | Reserved
    | Retiring

  type t =
    { owner : unit ref
    ; mutable phase : phase
    }
end

type _ request =
  | Open_inference_owner : Transcript.Source_id.t -> Inference_owner.t request
  | Admit_inference :
      Run_preparation.t option
      * Inference_owner.t
      * Transcript.Scope.relation
      * Agent_protocol.Id.Operation.t option
      * Agent_protocol.Id.Invocation.t option
      * Inference.Observation.Configuration.t
      -> Inference_ledger.Handle.t request
  | Allocate_inference :
      inference_execution
      * Inference_owner.t
      * Transcript.Scope.relation
      * Inference.Observation.Configuration.t
      -> Inference_allocation.t request
  | Admit_model_job :
      inference_execution * Agent_protocol.Job.t
      -> Agent_protocol.Job.t request
  | Refresh_inference :
      inference_execution * Inference_owner.t * Inference_ledger.Handle.t
      -> (unit -> unit) request
  | Acknowledge_inference :
      Run_preparation.t option
      * Inference_owner.t
      * Inference_ledger.Handle.t
      * Inference_runtime.Attempt.t
      -> unit request
  | Observe_inference :
      Run_preparation.t option * Inference_ledger.Handle.t * Inference.Observation.t
      -> unit request
  | Observe_owned_inference :
      Run_preparation.t option * Inference_owner.t * Inference.Observation.t
      -> unit request
  | Complete_inference :
      Run_preparation.t option
      * Inference_owner.t
      * Inference_ledger.Handle.t
      * Inference_client.Completion.t
      -> unit request
  | Release_inference :
      Run_preparation.t option * Inference_owner.t * Inference_ledger.Handle.t
      -> unit request
  | Seal_inference_owner : Inference_owner.t -> unit request
  | Finish_inference_owner : Run_preparation.t option * Inference_owner.t -> unit request
  | Reconcile_inference_recovery : unit request
  | Claim_delegated_event :
      Agent_protocol.Id.Moderator_execution.t
      * Agent_protocol.Moderator_execution.delegation
      * Session.Moderator_state.Identity_snapshot.t
      * Chat_response.Moderation.Event.t
      -> delegated_event_claim request
  | Claim_job_moderator :
      job_scope * Agent_protocol.Invocation.t
      -> moderator_borrow request
  | Claim_job_event :
      job_scope
      * Agent_protocol.Id.Moderator_execution.t
      * Session.Moderator_state.Identity_snapshot.t
      * Chat_response.Moderation.Event.t
      -> queued_event_borrow option request
  | Manage_moderator_follow_up :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.observer
      -> unit request
  | Admit_moderator_turn : Agent_protocol.Id.Operation.t -> unit request
  | Admit_notification_turn : Agent_protocol.Id.Operation.t -> unit request
  | Claim_ordinary_event :
      Agent_protocol.Id.Moderator_execution.t
      * Agent_protocol.Id.Operation.t option
      * Session.Moderator_state.Identity_snapshot.t
      * Chat_response.Moderation.Event.t
      -> queued_event_borrow option request
  | Claim_queued_event :
      Agent_protocol.Id.Moderator_execution.t
      * Agent_protocol.Id.Operation.t option
      * Session.Moderator_state.Identity_snapshot.t
      -> queued_event_borrow option request
  | Claim_legacy_callback :
      Chat_response.Moderator_manager.Callback.t * legacy_callback_parent
      -> Callback_scope.t option request
  | Claim_initialization_callback :
      Initialization_scope.t
      * Chat_response.Moderator_manager.t
      * Chat_response.Moderator_manager.Callback.t
      -> Callback_scope.t option request
  | Retire_initialization_resource :
      Initialization_scope.t * Chat_response.Moderator_manager.t
      -> unit request
  | Claim_queued_retirement :
      Agent_protocol.Id.Moderator_execution.t
      * Session.Moderator_state.Identity_snapshot.t
      * string
      -> queued_event_borrow option request
  | Commit_queued_event :
      queued_event_borrow
      * Session.Moderator_state.Identity_snapshot.t
      * Agent_protocol.Invocation.follow_up
      * Agent_protocol.Moderator_execution.Decision.t option
      * string list
      -> unit request
  | Reconcile_run_recovery : unit request
  | Run_actions :
      Agent_protocol.Moderator_execution.t
      -> Run_action_service.t option request
  | Finish_queued_event : queued_event_borrow * bool -> unit request
  | Set_queued_event_cancel : queued_event_borrow * (unit -> unit) -> unit request
  | Commit_invocation_call :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.t * History_entry.t
      -> unit request
  | Claim_invocation :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.t
      -> invocation_execution request
  | Claim_idle_invocation :
      moderator_borrow * Agent_protocol.Invocation.t
      -> invocation_execution request
  | Claim_event_invocation :
      queued_event_borrow * Agent_protocol.Invocation.t
      -> invocation_execution request
  | Claim_job_scope :
      Agent_protocol.Id.Job.t * int * int * Agent_protocol.Timestamp.t option
      -> job_scope request
  | Set_job_scope_cancel : job_scope * (unit -> unit) -> unit request
  | Seal_job_scope : job_scope -> unit request
  | Finish_job_scope : job_scope -> unit request
  | Prepare_background_job :
      Agent_protocol.Job.launch_owner * Chat_response.Background_request.t
      -> Agent_protocol.Job.t request
  | Stage_background_job : Agent_protocol.Job.t * Staged_jobs.capacity -> unit request
  | Stage_subscription_mutation :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Subscription.t option
      * Agent_protocol.Subscription.t
      -> int request
  | Create_script_subscription :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * string
      * int
      * Agent_protocol.Completion.wake
      * Jsonaf.t option
      -> (int * Agent_protocol.Subscription.t) request
  | Finish_script_subscription :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Subscription.t
      * int
      * Agent_protocol.Completion.t
      -> (int * Agent_protocol.Subscription.t) request
  | Select_subscription_mutations :
      Agent_protocol.Job.launch_owner * Agent_protocol.Invocation.observer * int list
      -> unit request
  | Abort_subscription_mutation : Agent_protocol.Job.launch_owner * int -> unit request
  | Read_script_subscription :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Subscription.t
      -> Agent_protocol.Subscription.t request
  | Expire_subscriptions : int request
  | Create_script_ingress :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Subscription.t
      * int
      * string
      * Jsonaf.t
      -> (int * External_ingress.t) request
  | Revoke_script_ingress :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Capability.t
      * string
      -> (int * External_ingress.t) request
  | Read_script_ingress :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Capability.t
      -> External_ingress.t request
  | Select_ingress_mutations :
      Agent_protocol.Job.launch_owner * Agent_protocol.Invocation.observer * int list
      -> unit request
  | Abort_ingress_mutation : Agent_protocol.Job.launch_owner * int -> unit request
  | Stage_schedule_mutation :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Schedule.t option
      * Agent_protocol.Schedule.t
      -> int request
  | Create_script_schedule :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * int
      * Jsonaf.t
      * Agent_protocol.Schedule.misfire
      -> (int * Agent_protocol.Schedule.t) request
  | Select_schedule_mutations :
      Agent_protocol.Job.launch_owner * Agent_protocol.Invocation.observer * int list
      -> unit request
  | Abort_schedule_mutation : Agent_protocol.Job.launch_owner * int -> unit request
  | Read_script_schedule :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Schedule.t
      -> Agent_protocol.Schedule.t request
  | Select_background_jobs :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t list
      -> unit request
  | Create_script_notification :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Chat_response.Notification_operations.correlation
      * Agent_protocol.Completion.t
      * Agent_protocol.Completion.wake
      * (string * string) list option
      -> (int * Agent_protocol.Delivery.t) request
  | Read_script_notification :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Delivery.t
      -> Agent_protocol.Delivery.t request
  | Select_notification_mutations :
      Agent_protocol.Job.launch_owner * Agent_protocol.Invocation.observer * int list
      -> unit request
  | Abort_notification_mutation : Agent_protocol.Job.launch_owner * int -> unit request
  | Abort_background_job :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t
      -> unit request
  | Has_staged_background_job :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t
      -> bool request
  | Read_script_job :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t
      -> Agent_protocol.Job.t request
  | Read_script_job_result :
      Agent_protocol.Job.launch_owner * Agent_protocol.Job.t
      -> Agent_protocol.Completion.t request
  | Cancel_script_job :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t
      -> unit request
  | Claim_job_invocation :
      job_scope * Agent_protocol.Invocation.t
      -> invocation_execution request
  | Finish_invocation :
      invocation_execution
      * Agent_protocol.Invocation.outcome
      * Agent_protocol.Invocation.follow_up option
      * bool
      * Agent_protocol.Authoring_reference.t option
      -> Agent_protocol.Invocation.t request
  | Claim_moderator_invocation :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.t
      -> moderator_borrow request
  | Claim_moderator_observation :
      Agent_protocol.Id.Operation.t * Agent_protocol.Id.Invocation.t
      -> moderator_borrow request
  | Claim_next_moderator_observation :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.observer
      -> moderator_borrow option request
  | Claim_idle_moderator_observation :
      Agent_protocol.Invocation.observer * bool
      -> moderator_borrow option request
  | Commit_moderator_invocation :
      moderator_borrow
      * Agent_protocol.Invocation.t
      * Session.Moderator_state.Identity_snapshot.t
      -> unit request
  | Finish_moderator_invocation :
      moderator_borrow * Agent_protocol.Invocation.outcome option
      -> unit request
  | Set_idle_moderator_cancel : moderator_borrow * (unit -> unit) -> unit request
  | Commit_extensions :
      int * int64 * Extension_change.t list
      -> Agent_protocol.Session.t request
  | Configuration : Agent_protocol.Session_configuration.t request
  | Set_organization_admission : Session_organization_admission.t -> unit request
  | Set_configuration_policy : Configuration_policy.t -> unit request
  | Prepare_configuration_update :
      Agent_protocol.Session_configuration.Update_request.t
      -> Configuration_update.t request
  | Commit_configuration_update :
      Configuration_update.Validated.t
      -> Agent_protocol.Session_configuration.t request
  | Begin_configuration_capture :
      Agent_protocol.Id.Operation.t
      -> Configuration_capture.Token.t request
  | Mark_configuration_capture :
      Configuration_capture.Token.t * Inference.Observation.Configuration.t
      -> unit request
  | Finish_configuration_capture : Configuration_capture.Token.t * bool -> unit request
  | Begin_lifecycle :
      Agent_protocol.Id.Attachment.t option * int * int64
      -> Lifecycle_fence.t request
  | Lifecycle_state : Lifecycle_fence.t -> Session_state.t request
  | Retire_lifecycle : Lifecycle_fence.t -> unit request
  | Abort_lifecycle : Lifecycle_fence.t -> unit request
  | State : Session_state.t request
  | Snapshot : Agent_protocol.Snapshot.t request
  | Observe : (Session_state.t * Agent_protocol.Snapshot.t) request
  | Authorize_writer : Agent_protocol.Id.Attachment.t -> unit request
  | Set_operation_worker : Operation_worker.t option -> unit request
  | Set_runtime_worker :
      Operator_authorization.t option
      * Operation_worker.t option
      * Inference_client.Execution.t option
      * Chat_response.Moderator_manager.t option
      -> unit request
  | Check_runtime_admission : unit request
  | Retire_runtime_worker : bool -> Runtime_retirement.t request
  | Set_compaction_inference : Compaction_inference.t option -> unit request
  | Enable_automatic_turn_budget :
      Operator_authorization.t option * Chat_response.Runtime_semantics.policy
      -> unit request
  | Set_automatic_turn_pauses :
      Chat_response.Runtime_semantics.pause_condition list
      -> unit request
  | Change_moderator :
      Operator_authorization.t option * Jsonaf.t option
      -> Agent_protocol.Session.t request
  | Change_workspace : Workspace_instance.t -> Agent_protocol.Session.t request
  | Shell_approval_grants : Session.Shell_state.Approval_grant.persisted list request
  | Replace_shell_approval_grants :
      Run_preparation.t option * Session.Shell_state.Approval_grant.persisted list
      -> unit request
  | Replace_shell_extension_snapshots :
      Run_preparation.t option * Session.Shell_state.Extension_snapshot.t list
      -> unit request
  | Shell_manifest_grants : Session.Shell_state.Manifest_grant.persisted list request
  | Add_shell_manifest_grant :
      Run_preparation.t option * Session.Shell_state.Manifest_grant.persisted
      -> unit request
  | Reset :
      Agent_protocol.Id.Attachment.t * int64 * reset_options
      -> Agent_protocol.Session.t request
  | Upgrade_prompt :
      Agent_protocol.Id.Attachment.t * int64 * Agent_protocol.Id.Prompt_revision.t
      -> Agent_protocol.Session.t request
  | Commit_administration :
      Agent_protocol.Id.Attachment.t
      * int64
      * Session_state.Compaction_archive.kind
      * Session_state.t
      -> Agent_protocol.Session.t request
  | Validate_administration_basis :
      Agent_protocol.Id.Attachment.t * Session_state.t
      -> unit request
  | Commit_reconciled_administration :
      Agent_protocol.Id.Attachment.t
      * Session_state.t
      * Session_state.Compaction_archive.kind
      * Session_state.t
      -> Agent_protocol.Session.t request
  | Begin_initialization :
      Initialization_scope.Purpose.t * Operator_authorization.t option * Session_state.t
      -> Initialization_scope.t request
  | Borrow_initialization_agent_actor :
      Initialization_scope.t
      -> Operator_authorization.t option request
  | Borrow_operation_agent_actor :
      Agent_protocol.Id.Operation.t
      -> Operator_authorization.t option request
  | Borrow_compaction_agent_actor :
      Agent_protocol.Operation.t
      -> Operator_authorization.t option request
  | Borrow_invocation_agent_actor :
      invocation_execution * Agent_protocol.Invocation.t
      -> Operator_authorization.t option request
  | Borrow_moderator_agent_actor :
      moderator_borrow * Agent_protocol.Invocation.t
      -> Operator_authorization.t option request
  | Begin_run_initialization :
      Initialization_scope.Purpose.t * Run_preparation.t * Session_state.t
      -> Initialization_scope.t request
  | End_initialization : Initialization_scope.t -> unit request
  | Complete_initialization :
      Initialization_scope.t * Session_state.t * Chat_response.Moderator_manager.t option
      -> Agent_protocol.Session.t request
  | Checkpoint_runtime_construction :
      Initialization_scope.t * Jsonaf.t option * Chat_response.Moderator_manager.t option
      -> Agent_protocol.Session.t request
  | Fail_initialization :
      Initialization_scope.t * Agent_protocol.Error.t
      -> Agent_protocol.Session.t request
  | Start :
      Operator_authorization.t option * Agent_protocol.Id.Attachment.t * int64 option
      -> Agent_protocol.Session.t request
  | Start_initial_delegated :
      Agent_store.Delegation_store.Reference.t * int64 option
      -> Agent_protocol.Session.t request
  | Fail_initial_delegated :
      Agent_store.Delegation_store.Reference.t * Agent_protocol.Error.t
      -> Agent_protocol.Session.t request
  | Queue_start : Agent_protocol.Id.Attachment.t -> Agent_protocol.Session.t request
  | Activate_queued_start : Agent_protocol.Session.t request
  | Update_organization :
      Agent_protocol.Principal.t * Agent_protocol.Session_organization.Request.t
      -> Agent_protocol.Session.t request
  | Set_recurrence_controller : Recurrence_controller.t -> unit request
  | Set_recurrence_planning_host : Recurrence_planning_host.t -> unit request
  | Recurrence_due_ids : Agent_protocol.Id.Recurrence.t list request
  | Begin_recurrence_due :
      Agent_protocol.Id.Recurrence.t
      -> Recurrence_runtime.preparation option request
  | Finish_recurrence_due : Recurrence_runtime.prepared -> unit request
  | Release_recurrence_due : Recurrence_runtime.preparation -> unit request
  | Apply_recurrence_command :
      Operator_authorization.t
      * (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      * Recurrence_command.t
      -> Recurrence_command_receipt.t request
  | Reserve_recurrence_child :
      Operator_authorization.t
      * (Session_state.t
         -> Recurrence_definition.t
         -> (unit, Agent_protocol.Error.t) result)
      * Recurrence_identity.Occurrence.t
      * int64
      -> Recurrence_creation.t request
  | Check_recurrence_creation :
      Recurrence_creation.t
      -> Recurrence_creation.Checked.t request
  | Record_recurrence_execution :
      Operator_authorization.t
      * (Session_state.t
         -> Recurrence_definition.t
         -> (unit, Agent_protocol.Error.t) result)
      * Recurrence_execution_intent.t
      * Recurrence_execution_attempt.t
      -> Recurrence_execution.t request
  | Observe_recurrence_run :
      Operator_authorization.t
      * (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      * Recurrence_execution_intent.t
      * Recurrence_execution_attempt.t
      -> Recurrence_run_observation.t option request
  | Import_recurrence_execution :
      Operator_authorization.t
      * (Session_state.t
         -> Recurrence_definition.t
         -> (unit, Agent_protocol.Error.t) result)
      * (Session_state.t
         -> Recurrence_definition.t
         -> (bool, Agent_protocol.Error.t) result)
      * Recurrence_run_observation.t
      -> Recurrence_import.t request
  | Acquire_recurrence_activation :
      Operator_authorization.t
      * (Session_state.t
         -> Recurrence_definition.t
         -> (unit, Agent_protocol.Error.t) result)
      * Recurrence_run_observation.t
      -> Recurrence_activation.t request
  | Check_recurrence_activation :
      Recurrence_activation.t * Recurrence_run_observation.t
      -> Recurrence_activation.Checked.t request
  | Release_recurrence_activation : Recurrence_activation.t -> unit request
  | Verify_recurrence_child :
      Recurrence_child_reservation.t
      -> Recurrence_child_link.t request
  | Link_recurrence_child :
      Recurrence_creation.t * Recurrence_child_link.t
      -> Recurrence_child_association.t request
  | Read_recurrences :
      Operator_authorization.t
      * (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      -> Recurrence_view.t list request
  | Read_recurrence_planning :
      Operator_authorization.t
      * (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      -> Recurrence_planning_read.t request
  | Apply_goal_command :
      Operator_authorization.t
      * (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      * Agent_protocol.Session_ref.t
      * Goal_command.t
      -> Goal_command_receipt.t request
  | Read_goals :
      Operator_authorization.t
      * (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      -> Agent_protocol.Goal.t list request
  | Apply_instruction_command :
      Instruction_native_admission.t
      * Instruction_command_admission.Source.t option
      * Instruction_command.t
      -> Instruction_command.Receipt.t request
  | Update_metadata :
      Agent_protocol.Id.Attachment.t * int64 * Agent_protocol.Session_metadata.Patch.t
      -> Agent_protocol.Session.t request
  | Stop :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Session.stop_mode
      -> Agent_protocol.Session.t request
  | Stop_delegated :
      Agent_store.Delegation_store.Reference.t * Agent_protocol.Session.stop_mode
      -> Agent_protocol.Session.t request
  | Stop_managed :
      Agent_store.Delegation_store.Reference.t
      * Agent_protocol.Idempotency_key.t
      * Agent_protocol.Session.stop_mode
      * int
      * int option
      -> Managed_stop.t request
  | Stop_delegated_at_epoch :
      Agent_store.Delegation_store.Reference.t * int64 * bool
      -> Agent_protocol.Session.t request
  | Append_history :
      Agent_protocol.Id.Attachment.t * Agent_protocol.History.entry list
      -> Agent_protocol.Session.t request
  | Defer_history :
      Agent_protocol.Id.Attachment.t * Agent_protocol.History.entry list
      -> Agent_protocol.Session.t request
  | Submit_message :
      Operator_authorization.t option
      * Agent_protocol.Id.Attachment.t
      * Agent_protocol.Id.Principal.t
      * Agent_protocol.Pending_input.Timing.t
      * Agent_protocol.History.entry
      -> submission request
  | Control_pending :
      Agent_protocol.Id.Principal.t
      * Pending_inspection.projection
      * Agent_protocol.Pending_control.Cancel_request.t
      * string option
      -> Agent_protocol.Pending_control.Result.t request
  | Capture_run_constructor_model_source :
      Run_preparation.t
      * Agent_protocol.Id.Job.t
      * int
      * Inference.Request.Target.t
      * Document_schema.Limits.t
      -> unit request
  | Capture_run_constructor_recipe_target :
      Run_preparation.t
      * Agent_protocol.Id.Job.t
      * int
      * int
      * Inference.Request.Target.t
      * Document_schema.Limits.t
      -> unit request
  | Enable_run_constructor_turn_budget :
      Run_preparation.t * Chat_response.Runtime_semantics.policy
      -> unit request
  | Start_run_constructor_model_job :
      Run_preparation.t * Agent_protocol.Job.t
      -> Agent_protocol.Job.t request
  | Complete_run_constructor_model_job :
      Run_preparation.t
      * Agent_protocol.Id.Job.t
      * int
      * int
      * Runtime_builder.model_job_outcome
      -> Agent_protocol.Job.t request
  | Interrupt_run_constructor_job :
      Run_preparation.t * Agent_protocol.Id.Job.t * int * int * string
      -> Agent_protocol.Job.t request
  | Add_run_constructor_job :
      Run_preparation.t * Agent_protocol.Job.t
      -> Agent_protocol.Job.t request
  | Add_run_constructor_schedule :
      Run_preparation.t * Agent_protocol.Schedule.t
      -> Agent_protocol.Schedule.t request
  | Cancel_run_constructor_schedule :
      Run_preparation.t * Agent_protocol.Id.Schedule.t
      -> Agent_protocol.Schedule.t request
  | Reserve_run_history_block :
      Run_preparation.t * int
      -> History_id_source.reservation request
  | Checkpoint_run_moderator :
      Run_preparation.t * Jsonaf.t option * Chat_response.Moderator_manager.t option
      -> Agent_protocol.Session.t request
  | Admit_prepared_run :
      Run_preparation.t
      * Run_admission.Scope.t
      * Agent_protocol.Session_ref.t
      * Agent_protocol.History.entry option
      -> Run_admission_outcome.t request
  | Begin_run_preparation :
      Operator_authorization.t option
      * (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      * Agent_protocol.Id.Principal.t
      * Agent_protocol.Run_start.t
      * string
      -> Run_preparation.Decision.t request
  | Begin_headless_run_preparation :
      Operator_authorization.t
      * (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      * Run_admission_request.Headless.t
      -> Run_preparation.Decision.t request
  | Close_run_preparation : Run_preparation.t -> unit request
  | Run_start_receipt :
      (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      * Agent_protocol.Id.Principal.t
      * Agent_protocol.Run_start.t
      * string
      -> Agent_protocol.Run_receipt.t option request
  | Admit_run :
      Run_admission.Scope.t
      * Agent_protocol.Run_start.t
      * Agent_protocol.Session_ref.t
      * string
      * Agent_protocol.History.entry option
      -> Agent_protocol.Run_receipt.t request
  | Submit_managed_message :
      Agent_store.Delegation_store.Reference.t
      * Agent_protocol.Idempotency_key.t
      * string
      * int
      * int option
      * Agent_protocol.History.entry
      -> Managed_submission.t request
  | Compact :
      Operator_authorization.t option * Agent_protocol.Id.Attachment.t * int64 option
      -> Agent_protocol.Session.t request
  | Delete_history :
      Agent_protocol.Id.Attachment.t * int64 * Agent_protocol.History.Id.t
      -> Agent_protocol.Session.t request
  | Edit_history :
      Agent_protocol.History_edit.Edit_request.t
      -> History_edit_result.t request
  | Continue_history :
      Agent_protocol.History_edit.Continue_request.t
      -> History_continuation_result.t request
  | Adopt_deferred : Agent_protocol.Session.t request
  | Reserve_history_block : int -> History_id_source.reservation request
  | Commit_worker_entry : Agent_protocol.Id.Operation.t * History_entry.t -> unit request
  | Prepare_authoring_input :
      Agent_protocol.Id.Operation.t
      * Chat_response.Authoring_materialization.t
      * History_entry.t list
      * Chat_response.Moderation.Effective_entry.t list
      -> History_entry.t list request
  | Publish_invocation_output :
      Agent_protocol.Id.Operation.t * Agent_protocol.Id.Invocation.t * History_entry.t
      -> unit request
  | Commit_worker_moderator :
      Agent_protocol.Id.Operation.t * Jsonaf.t option
      -> unit request
  | Consume_deferred : Agent_protocol.Id.Operation.t -> History_entry.t list request
  | Consume_notifications :
      Agent_protocol.Id.Operation.t * Notification_delivery.t
      -> Chat_response.In_memory_stream.Safe_point_input.batch request
  | Deliver_idle_notifications : Notification_delivery.idle -> bool request
  | Admit_standalone_delivery : Standalone_delivery.t -> unit request
  | Retire_obsolete_moderator_delivery : int64 * Agent_protocol.Job.t -> bool request
  | Deliver_standalone_completion :
      int64
      * Agent_protocol.Job.t
      * Chat_response.Tool_capability.t
      * Chat_response.One_off_request.policy
      -> unit request
  | Consume_initial_notifications :
      Agent_protocol.Id.Operation.t * Notification_delivery.idle
      -> Chat_response.In_memory_stream.Safe_point_input.batch request
  | Has_writer_attachment : bool request
  | Invocation_granted : string * string -> bool request
  | Worker_ready : Agent_protocol.Id.Operation.t * (unit -> unit) -> unit request
  | Worker_terminal :
      Agent_protocol.Id.Operation.t * Operation_worker.outcome * agent_custody option
      -> unit request
  | Compaction_terminal :
      Agent_protocol.Id.Operation.t * compaction_outcome
      -> unit request
  | Cancel_operation :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Id.Operation.t
      -> Agent_protocol.Session.t request
  | Open_permission :
      Agent_protocol.Permission.t * float option * Agent_protocol.Permission.choice
      -> Agent_protocol.Permission.resolution Eio.Promise.t request
  | Respond_permission :
      Agent_protocol.Id.Attachment.t
      * Agent_protocol.Id.Principal.t option
      * Agent_protocol.Id.Permission.t
      * int
      * Agent_protocol.Permission.choice
      * string option
      -> Agent_protocol.Permission.t request
  | Resolve_permission_system :
      Agent_protocol.Id.Permission.t
      * int
      * Agent_protocol.Permission.choice
      * string option
      -> Agent_protocol.Permission.t request
  | Revoke_grant :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Id.Grant.t * string
      -> (Agent_protocol.Grant.t * Agent_protocol.Session.t) request
  | Change_job :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Job.t
      -> Agent_protocol.Session.t request
  | Capture_inference_target :
      Inference.Request.Target.t * Document_schema.Limits.t
      -> unit request
  | Capture_model_job_source :
      Agent_protocol.Id.Job.t
      * int
      * Inference.Request.Target.t
      * Document_schema.Limits.t
      -> unit request
  | Capture_recipe_target :
      Agent_protocol.Id.Job.t
      * int
      * int
      * Inference.Request.Target.t
      * Document_schema.Limits.t
      -> unit request
  | Add_initialization_model_job :
      Initialization_scope.t * Agent_protocol.Job.t
      -> Agent_protocol.Job.t request
  | Start_initialization_model_job :
      Initialization_scope.t * Agent_protocol.Job.t
      -> Agent_protocol.Job.t request
  | Claim_initialization_model_job :
      Initialization_scope.t * Agent_protocol.Id.Job.t * int
      -> Agent_protocol.Job.t request
  | Initialization_model_job_is_current :
      Initialization_scope.t * Agent_protocol.Id.Job.t * int * int
      -> bool request
  | Capture_initialization_recipe_target :
      Initialization_scope.t
      * Agent_protocol.Id.Job.t
      * int
      * int
      * Inference.Request.Target.t
      * Document_schema.Limits.t
      -> unit request
  | Complete_initialization_model_job :
      Initialization_scope.t
      * Agent_protocol.Id.Job.t
      * int
      * int
      * Runtime_builder.model_job_outcome
      -> Agent_protocol.Job.t request
  | Add_job : Agent_protocol.Job.t -> Agent_protocol.Job.t request
  | Read_job : Agent_protocol.Id.Job.t -> Agent_protocol.Job.t request
  | Publish_job_progress :
      Agent_protocol.Id.Invocation.t * Ochat_function.Progress.t
      -> unit request
  | Claim_job : Agent_protocol.Id.Job.t * int -> Agent_protocol.Job.t option request
  | Complete_job :
      Agent_protocol.Id.Job.t * int * int * Runtime_builder.model_job_outcome
      -> Agent_protocol.Job.t request
  | Complete_background_job :
      Agent_protocol.Id.Job.t * int * int * Agent_protocol.Completion.t
      -> Agent_protocol.Job.t request
  | Defer_background_job :
      Agent_protocol.Id.Job.t * int * int * Agent_protocol.Job.dependency
      -> Agent_protocol.Job.t request
  | Refresh_background_job :
      Agent_protocol.Id.Job.t * int * int
      -> Agent_protocol.Job.t request
  | Recover_background_results : int * int -> unit request
  | Deliver_job :
      Agent_protocol.Id.Job.t
      * int
      * Session.Moderator_state.Identity_snapshot.t option
      * Agent_protocol.Job.t option
      * Jsonaf.t option
      * Chat_response.Moderator_manager.t option
      -> Agent_protocol.Job.t request
  | Enqueue_run_job_delivery :
      Run_job_delivery.t * Session.Moderator_state.Identity_snapshot.t * Jsonaf.t
      -> unit request
  | Cancel_job_internal : Agent_protocol.Id.Job.t -> Agent_protocol.Job.t request
  | Cancel_job :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Id.Job.t * int option * int option
      -> Agent_protocol.Job.t request
  | Interrupt_job :
      Agent_protocol.Id.Job.t * int * int * string
      -> Agent_protocol.Job.t request
  | Change_schedule :
      Agent_protocol.Id.Attachment.t
      * [ `Created | `Cancelled ]
      * Agent_protocol.Schedule.t
      -> Agent_protocol.Session.t request
  | Add_schedule : Agent_protocol.Schedule.t -> Agent_protocol.Schedule.t request
  | Callback_schedule_after_ms :
      Callback_scope.t * int * Jsonaf.t
      -> Agent_protocol.Schedule.t request
  | Callback_schedule_cancel :
      Callback_scope.t * Agent_protocol.Id.Schedule.t
      -> Agent_protocol.Schedule.t request
  | Cancel_schedule :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Id.Schedule.t * int option
      -> Agent_protocol.Schedule.t request
  | Cancel_schedule_internal :
      Agent_protocol.Id.Schedule.t
      -> Agent_protocol.Schedule.t request
  | Claim_schedule :
      Agent_protocol.Id.Schedule.t * int
      -> Agent_protocol.Schedule.t option request
  | Due_schedules :
      (Agent_protocol.Session.observed_state * Agent_protocol.Schedule.t list) request
  | Retry_schedule :
      Agent_protocol.Id.Schedule.t * int
      -> Agent_protocol.Schedule.t request
  | Complete_schedule :
      Agent_protocol.Id.Schedule.t
      * int
      * Session.Moderator_state.Identity_snapshot.t option
      * Agent_protocol.Schedule.t option
      * Jsonaf.t option
      * Chat_response.Moderator_manager.t option
      -> Agent_protocol.Schedule.t request
  | Prepare_ingress_submission :
      Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Principal.t
      * Agent_protocol.Id.Capability.t
      * string
      * Agent_protocol.Idempotency_key.t
      * Jsonaf.t
      -> Ingress_submission.decision request
  | Commit_ingress_submission :
      Ingress_submission.t
      * Session.Moderator_state.Identity_snapshot.t
      * Session.Moderator_state.Identity_snapshot.t
      -> External_ingress.receipt request
  | Fail_schedule :
      Agent_protocol.Id.Schedule.t * int * Agent_protocol.Error.t
      -> Agent_protocol.Schedule.t request
  | Skip_schedule :
      Agent_protocol.Id.Schedule.t * int
      -> Agent_protocol.Schedule.t request
  | Claim_idle_moderator : History_entry.t list option request
  | Apply_observation_follow_up : bool request
  | Complete_idle_moderator : Runtime_builder.moderator_drain -> unit request
  | Fail_idle_moderator : Agent_protocol.Error.t -> unit request
  | Expire_permission :
      Agent_protocol.Id.Permission.t * int * Agent_protocol.Permission.choice
      -> unit request
  | Attach :
      (Agent_protocol.Session.attachment_mode
      * bool
      * Agent_protocol.Id.Principal.t option
      * string option)
      -> (Agent_protocol.Session.Attachment.t
         * Subscriber.t option
         * Agent_protocol.Snapshot.t
         * string option)
           request
  | Detach : Agent_protocol.Id.Attachment.t -> unit request
  | Renew_owner :
      Agent_protocol.Id.Attachment.t * int64
      -> (Agent_protocol.Session.Owner_lease.t * Agent_protocol.Session.t) request
  | Owner_expired : int64 -> unit request
  | Checkpoint :
      (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      -> unit request
  | Quiescent_checkpoint :
      (Session_state.t -> ('a, Agent_protocol.Error.t) result)
      -> 'a option request
  | Shutdown : unit request

and compaction_outcome =
  | Compacted of History_entry.t list
  | Compaction_cancelled of string
  | Compaction_failed of Agent_protocol.Error.t

type packed =
  | Pack :
      Document_schema.Document.t option * 'a request * 'a Actor_reply.t Eio.Promise.u
      -> packed

type permission_waiter =
  { resolver : Agent_protocol.Permission.resolution Eio.Promise.u
  ; resume_observed : Agent_protocol.Session.observed_state
  }

type t =
  { sw : Eio.Switch.t
  ; sleep : float -> unit
  ; mailbox : packed Mailbox.t
  ; finished : unit Eio.Promise.t
  ; persistence : persistence
  ; services : services
  ; subscriber_mutex : Eio.Mutex.t
  ; active_calls : Active_calls.t
  ; subscribers : (Agent_protocol.Id.Attachment.t, Subscriber.t) Map.Poly.t ref
  ; permission_waiters :
      (Agent_protocol.Id.Permission.t, permission_waiter) Map.Poly.t ref
  ; lifecycle_owner : unit ref
  ; mutable lifecycle_fence : Lifecycle_fence.t option
  ; mutable organization_admission : Session_organization_admission.t option
  ; mutable configuration_policy : Configuration_policy.t option
  ; mutable recurrence_controller : Recurrence_controller.t option
  ; recurrence_runtime : Recurrence_runtime.t
  ; recurrence_creation_owner : unit ref
  ; recurrence_activation_owner : Activation_custody.Owner.t
  ; mutable recurrence_activations : Activation_custody.t list
  ; configuration_update_owner : Configuration_update.Owner.t
  ; configuration_capture : Configuration_capture.t
  ; mutable operation_worker : Operation_worker.t option
  ; mutable compaction_inference : Compaction_inference.t option
  ; mutable inference_execution : Inference_client.Execution.t option
  ; mutable moderator_manager : Chat_response.Moderator_manager.t option
  ; compaction_env : Eio_unix.Stdenv.base option
  ; owner_lease_duration_ms : int
  ; max_attachments : int
  ; subscriber_capacity : int
  ; schedule_permission_timeouts : bool
  ; mutable owner_timer_cancel : unit Eio.Promise.u option
  ; mutable active_cancel : (unit -> unit) option
  ; mutable runtime_retirement : runtime_retirement option
  ; mutable idle_moderator_borrowed : bool
  ; mutable moderator_borrow : moderator_borrow option
  ; mutable queued_event_borrow : queued_event_borrow option
  ; mutable legacy_callback_scope : Callback_scope.t option
  ; mutable legacy_queue_custody : Legacy_queue_custody.t
  ; mutable legacy_schedule_custody : Legacy_schedule_custody.t
  ; mutable operation_agent_custody :
      (Agent_protocol.Id.Operation.t * agent_admission) option
  ; mutable lifecycle_agent_custody : lifecycle_agent_custody
  ; mutable intent_agent_custody : agent_intent_custody list
  ; mutable pending_agent_custody : (Agent_protocol.History.entry * agent_custody) list
  ; mutable job_agent_custody : Job_custody.t
  ; mutable run_authorities : Run_authorities.t
  ; run_preparation_owner : Run_preparation.Owner.t
  ; mutable run_preparation : Run_preparation.t option
  ; mutable foreground_moderator :
      (Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.observer) option
  ; mutable invocation_executions : invocation_execution list
  ; inference_owner_identity : unit ref
  ; mutable inference_owners : Inference_owner.t list
  ; initialization_owner : unit ref
  ; mutable initialization_scope : Initialization_scope.t option
  ; mutable job_scopes : job_scope list
  ; staged_jobs : Job_custody.Fresh.t option Staged_jobs.t
  ; staged_subscriptions : Staged_subscriptions.t
  ; staged_ingress : Staged_ingress.t
  ; staged_schedules : Staged_schedules.t
  ; staged_notifications : Staged_notifications.t
  ; mutable notification_inputs :
      (Agent_protocol.Id.Operation.t * Agent_protocol.Id.Delivery.t list) option
  ; extension_clock : Extension_clock.t
  ; invocation_gate : Chat_response.Execution_gate.t
  ; event_sequence : int64 Atomic.t
  ; mutable state : Session_state.t
  ; mutable stopped : bool
  ; mutable command_audit : Document_schema.Document.t option
  }

let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()

let agent_source_current t generation source =
  Int.equal generation t.state.identity.generation
  && (not t.stopped)
  && Agent_protocol.Session.equal_desired_state t.state.lifecycle.desired Running
  &&
  match Runtime_builder.moderator_snapshot_observer t.state.moderator with
  | Error _ -> false
  | Ok current -> Option.equal Agent_protocol.Invocation.equal_observer source current
;;

let observation_source t =
  match t.state.run_state with
  | None -> Ok None
  | Some index ->
    let installation = Run_state.installation index in
    (match installation.Run_source_installation.source with
     | None -> Ok None
     | Some _ ->
       Run_source_installation.captured
         installation
         ~generation:t.state.identity.generation
       |> Result.map ~f:Option.some)
;;

let observation_pending (invocation : Agent_protocol.Invocation.t) =
  match invocation.observation with
  | Some { status = Awaiting | Observing; _ } -> true
  | Some
      { status = Observed
      ; follow_up = Some (Pending_follow_up _ | Compaction_accepted_follow_up _)
      ; _
      } -> true
  | None
  | Some
      { status = Observed
      ; follow_up = None | Some (Applied_follow_up _ | Discarded_follow_up _)
      ; _
      }
  | Some { status = Observation_failed _; _ } -> false
;;

let retire_instruction_continuations t =
  (match t.lifecycle_agent_custody with
   | Unadmitted -> ()
   | Admitted { pending = Pending admission; _ } -> retire_agent_admission admission
   | Admitted { pending = Consumed; _ } -> ());
  t.lifecycle_agent_custody <- Unadmitted;
  List.iter t.intent_agent_custody ~f:(fun entry ->
    Option.iter entry.custody ~f:retire_agent_custody;
    Option.iter entry.observation ~f:Observation_continuation.retire);
  t.intent_agent_custody <- []
;;

let retain_instruction_continuations t =
  (match t.lifecycle_agent_custody with
   | Admitted { generation; source; _ }
     when not (agent_source_current t generation source) ->
     retire_instruction_continuations t
   | Unadmitted | Admitted _ -> ());
  let current_source = observation_source t in
  let current_observer = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  t.intent_agent_custody
  <- List.filter_map t.intent_agent_custody ~f:(fun entry ->
       let invocation =
         match entry.key with
         | Event_intent _ -> None
         | Invocation_intent id ->
           List.find t.state.invocations ~f:(fun invocation ->
             Agent_protocol.Id.Invocation.equal invocation.context.id id)
       in
       let follow_up =
         agent_source_current t entry.generation entry.source
         &&
         match entry.key with
         | Event_intent id ->
           List.exists t.state.moderator_executions ~f:(fun receipt ->
             Agent_protocol.Id.Moderator_execution.equal receipt.context.id id
             && Observation_follow_up.pending_event receipt)
         | Invocation_intent _ ->
           Option.exists invocation ~f:Observation_follow_up.pending
       in
       let observation =
         Option.bind entry.observation ~f:(fun observation ->
           let retained =
             (not t.stopped)
             && Int.equal entry.generation t.state.identity.generation
             &&
             match current_source, current_observer, invocation with
             | Ok source, Ok observer, Some invocation ->
               Option.equal Agent_protocol.Invocation.equal_observer entry.source observer
               && observation_pending invocation
               && Observation_continuation.matches observation ~source invocation
             | (Error _ | Ok _), (Error _ | Ok _), (None | Some _) -> false
           in
           if retained
           then Some observation
           else (
             Observation_continuation.retire observation;
             None))
       in
       if not follow_up then Option.iter entry.custody ~f:retire_agent_custody;
       if follow_up || Option.is_some observation
       then
         Some
           { entry with
             custody = (if follow_up then entry.custody else None)
           ; observation
           }
       else None)
;;

(* Retention uses actual installation facts, independently of whether this
   resource is currently eligible to execute. It cannot capture a new source. *)
let retained_moderator_source (state : Session_state.t) =
  match state.run_state with
  | None -> None
  | Some index ->
    let source =
      let open Result.Let_syntax in
      let%bind observer = Moderator_checkpoint.observer state.moderator in
      let%bind source =
        Run_source_installation.captured
          (Run_state.installation index)
          ~generation:state.identity.generation
      in
      if
        Option.equal
          Agent_protocol.Invocation.equal_observer
          observer
          (Some source.observer)
      then Ok source
      else Error (error Conflict "retained moderator source differs from installation")
    in
    (match source with
     | Ok source -> Some source
     | Error _ -> None)
;;

let retained_agent_custody t =
  retain_instruction_continuations t;
  let metadata =
    match Moderator_checkpoint.queue t.state.moderator with
    | Ok metadata -> metadata
    | Error _ -> None
  in
  let source = retained_moderator_source t.state in
  t.legacy_queue_custody
  <- Legacy_queue_custody.install
       t.legacy_queue_custody
       ~incoming:
         (Legacy_queue_custody.retain_facts t.legacy_queue_custody ~source ~metadata);
  t.legacy_schedule_custody
  <- Legacy_schedule_custody.install
       t.legacy_schedule_custody
       ~incoming:
         (Legacy_schedule_custody.retain_facts
            t.legacy_schedule_custody
            ~source
            ~schedules:t.state.schedules);
  let operation_current id =
    (not t.stopped)
    && Option.exists t.state.active_operation ~f:(fun operation ->
      Agent_protocol.Id.Operation.equal operation.id id
      &&
      match operation.state with
      | Starting | Running -> true
      | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ -> false)
  in
  (match t.operation_agent_custody with
   | Some (id, admission) when not (operation_current id) ->
     retire_agent_admission admission;
     t.operation_agent_custody <- None
   | Some _ | None -> ());
  t.job_agent_custody
  <- Job_custody.install_retained
       t.job_agent_custody
       ~incoming:
         (if t.stopped
          then Job_custody.empty
          else Job_custody.retain t.job_agent_custody ~jobs:t.state.jobs)
;;

let operation_agent_actor t id =
  match t.operation_agent_custody with
  | Some (owner, admission) when Agent_protocol.Id.Operation.equal owner id ->
    (match Execution_custody.Admission.custody admission with
     | Some custody -> Result.map (Execution_custody.actor custody) ~f:Option.some
     | None -> Ok None)
  | Some _ | None -> Ok None
;;

let install_operation_agent_custody t operation custody =
  Option.iter t.operation_agent_custody ~f:(fun (_, admission) ->
    retire_agent_admission admission);
  t.operation_agent_custody
  <- Some (operation.Agent_protocol.Operation.id, create_agent_admission custody)
;;

let job_scope_current t scope =
  match
    ( List.mem t.job_scopes scope ~equal:phys_equal
    , List.find t.state.jobs ~f:(fun job ->
        Agent_protocol.Id.Job.equal job.id scope.job.id) )
  with
  | true, Some job
    when Int.equal job.generation scope.job.generation
         && Int.equal job.generation t.state.identity.generation
         && Int.equal job.attempt scope.job.attempt
         && Agent_protocol.Job.equal_kind job.kind scope.job.kind
         && Jsonaf.exactly_equal job.payload scope.job.payload -> Ok job
  | _ -> Error (error Conflict "background callback no longer owns this job attempt")
;;

let job_scope_can_execute t scope =
  let open Result.Let_syntax in
  let%bind job = job_scope_current t scope in
  match job.status, t.state.lifecycle.desired, t.state.halted, t.state.failure with
  | (Running | Waiting_permission _), Running, false, None
    when scope.active
         && (not scope.cancel_requested)
         && Option.for_all scope.deadline ~f:(fun deadline ->
           Agent_protocol.Timestamp.compare (t.services.now ()) deadline < 0) -> Ok ()
  | _ -> Error (error Conflict "background job cannot admit another invocation")
;;

let job_execution_owned_by scope execution =
  match execution.owner with
  | Background_job owner -> phys_equal owner scope
  | Event_moderator borrow -> Option.exists borrow.job_scope ~f:(phys_equal scope)
  | Invocation_moderator borrow -> Option.exists borrow.job_scope ~f:(phys_equal scope)
  | _ -> false
;;

let signal_job_scope scope =
  match Eio.Promise.peek scope.cancelled with
  | Some () -> ()
  | None -> Eio.Promise.resolve scope.cancel_signal ()
;;

let cancel_job_scope (scope : job_scope) =
  scope.cancel_requested <- true;
  Atomic.set scope.authority_live false;
  signal_job_scope scope;
  Option.iter scope.cancel ~f:(fun cancel -> cancel ())
;;

let cancel_job_moderators t scope =
  Option.iter t.moderator_borrow ~f:(fun borrow ->
    match Option.exists borrow.job_scope ~f:(phys_equal scope) with
    | false -> ()
    | true ->
      borrow.cancel_requested <- true;
      borrow.accepts_children <- false;
      Atomic.set borrow.authority_live false;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()));
  Option.iter t.queued_event_borrow ~f:(fun borrow ->
    match Option.exists borrow.job_scope ~f:(phys_equal scope) with
    | false -> ()
    | true ->
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()))
;;

let job_has_moderator t scope =
  Option.exists t.moderator_borrow ~f:(fun borrow ->
    Option.exists borrow.job_scope ~f:(phys_equal scope))
  || Option.exists t.queued_event_borrow ~f:(fun borrow ->
    Option.exists borrow.job_scope ~f:(phys_equal scope))
;;

let cancel_job_scopes t jobs =
  List.iter t.job_scopes ~f:(fun scope ->
    match
      List.exists jobs ~f:(fun (job : Agent_protocol.Job.t) ->
        Agent_protocol.Id.Job.equal job.id scope.job.id
        && Int.equal job.generation scope.job.generation
        && Int.equal job.attempt scope.job.attempt)
    with
    | true ->
      cancel_job_scope scope;
      cancel_job_moderators t scope
    | false -> ())
;;

let moderator_is_borrowed t =
  Option.is_some t.moderator_borrow || Option.is_some t.queued_event_borrow
;;

let call t ?(priority = Mailbox.Normal) ?command_audit request =
  let promise, resolver = Eio.Promise.create () in
  let open Result.Let_syntax in
  let%bind () =
    Mailbox.push t.mailbox ~priority (Pack (command_audit, request, resolver))
  in
  Actor_reply.await promise
;;

let publish_durable t events =
  Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
    List.iter events ~f:(Active_calls.finish t.active_calls);
    Map.iter !(t.subscribers) ~f:(fun subscriber ->
      List.iter events ~f:(Subscriber.publish_durable subscriber)))
;;

let broadcast_recoverable t event =
  Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
    Active_calls.observe t.active_calls event;
    Map.iter !(t.subscribers) ~f:(fun subscriber ->
      Subscriber.publish_recoverable subscriber event))
;;

let sync_extension_clock t =
  let timers =
    t.state.schedules @ Staged_schedules.values t.staged_schedules
    |> List.filter_map ~f:(fun timer ->
      match timer.Agent_protocol.Schedule.ownership, timer.status with
      | Some _, (Scheduled | Delivering) ->
        Some (Extension_clock.Key.Schedule timer.id, timer.next_due_at)
      | _ -> None)
  in
  let subscriptions =
    t.state.subscriptions @ Staged_subscriptions.values t.staged_subscriptions
    |> List.filter_map ~f:(fun subscription ->
      match
        subscription.Agent_protocol.Subscription.context.source, subscription.result
      with
      | Some _, None ->
        Some
          ( Extension_clock.Key.Subscription subscription.context.id
          , subscription.context.deadline )
      | _ -> None)
  in
  Extension_clock.reconcile
    t.extension_clock
    ~retained:(timers @ subscriptions)
    ~wall_now:(t.services.now ())
    ~monotonic_now:(t.services.monotonic_now ())
;;

let inference_port t execution ~graph ~relation ~configuration =
  call t (Allocate_inference (execution, graph, relation, configuration))
;;

let model_job_port t execution =
  Model_job_admission.Host.create ~admit:(fun job ->
    (* Model.spawn is an immediate effect. Once enqueued, receive its exact ACK
       even if the surrounding callback is cancelled; never replay it blindly. *)
    Eio.Cancel.protect (fun () -> call t (Admit_model_job (execution, job))))
;;

let close_run_preparation t =
  Option.iter t.run_preparation ~f:Run_preparation.close;
  t.run_preparation <- None
;;

let check_run_preparation t preparation =
  match t.run_preparation with
  | Some active when phys_equal active preparation ->
    Run_preparation.check preparation ~owner:t.run_preparation_owner ~state:t.state
  | Some _ | None ->
    Error (error Conflict "run preparation is not this actor's live admission")
;;

let install_live_run_agent_custody t =
  match t.operation_agent_custody, t.state.active_operation, t.state.run_state with
  | None, Some operation, Some index
    when match operation.state with
         | Starting | Running -> true
         | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ -> false ->
    let owners =
      List.filter (Run_state.runs index) ~f:(fun run ->
        List.exists run.owned_work ~f:(fun work ->
          Int.equal work.Agent_protocol.Run_work.generation operation.generation
          &&
          match work.key with
          | Operation id -> Agent_protocol.Id.Operation.equal operation.id id
          | Retained _ -> false))
    in
    (match owners with
     | [ run ] ->
       Option.iter (Run_authorities.find t.run_authorities run.id) ~f:(fun binding ->
         Option.iter
           (Run_admission.Scope.original_actor (Run_authorities.scope binding))
           ~f:(fun actor ->
             t.operation_agent_custody
             <- Some
                  ( operation.id
                  , create_agent_admission (Some (create_agent_custody actor)) )))
     | [] | _ :: _ :: _ -> ())
  | Some _, _, _ | None, None, _ | None, Some _, (Some _ | None) -> ()
;;

let close_recurrence_activations t =
  List.iter t.recurrence_activations ~f:Activation_custody.close;
  t.recurrence_activations <- []
;;

let retain_recurrence_activations t state =
  t.recurrence_activations
  <- Activation_custody.retain_candidates t.recurrence_activations state
;;

let invalidate_goal_inference_allocations t goal_id =
  List.iter t.inference_owners ~f:(fun owner ->
    Graph_owner.iter_registrations owner ~f:(fun registration ->
      if
        Option.exists
          (Inference_owner.Registration.goal_id registration)
          ~f:(Agent_protocol.Id.Goal.equal goal_id)
      then Inference_owner.Registration.invalidate_policy registration))
;;

let invalidate_goal_inference_policy ?admission_hold t (current : Session_state.t) =
  let goal state id =
    Option.bind state.Session_state.goal_catalog ~f:(fun catalog ->
      Goal_catalog.find catalog id)
  in
  List.iter t.inference_owners ~f:(fun owner ->
    Graph_owner.iter_registrations owner ~f:(fun registration ->
      Option.iter (Inference_owner.Registration.goal_id registration) ~f:(fun id ->
        let changed =
          match goal t.state id, goal current id with
          | Some before, Some after ->
            (not (Goal_admission_policy.same_allocation_policy before after))
            || ((not (Option.exists admission_hold ~f:(Agent_protocol.Id.Goal.equal id)))
                && not
                     (Agent_protocol.Goal.State.equal
                        (Agent_protocol.Goal.state before)
                        (Agent_protocol.Goal.state after)))
          | None, _ | _, None -> true
        in
        if changed then Inference_owner.Registration.invalidate_policy registration)))
;;

let install_committed_transition
      ?queued_claim_acknowledgement
      ?authored_run_acknowledgement
      ?legacy_queue_custody
      ?legacy_schedule_custody
      ?retire_legacy_custody
      ?run_preparation
      ?job_custody
      ?initialization_acknowledgement
      ?admission_hold
      t
      (transition : Session_transition.t)
  =
  let stopping_jobs =
    match t.state.lifecycle.desired, transition.state.lifecycle.desired with
    | Running, Stopped -> true
    | Running, Running | Stopped, Running | Stopped, Stopped -> false
  in
  (match t.run_preparation, run_preparation with
   | Some active, Some supplied when phys_equal active supplied ->
     (match
        Run_preparation.advance
          active
          ~owner:t.run_preparation_owner
          ~previous:t.state
          ~current:transition.state
      with
      | Ok () -> ()
      | Error _ -> close_run_preparation t)
   | Some _, (Some _ | None) -> close_run_preparation t
   | None, (Some _ | None) -> ());
  t.command_audit <- None;
  invalidate_goal_inference_policy ?admission_hold t transition.state;
  t.state <- transition.state;
  Option.iter authored_run_acknowledgement ~f:(fun acknowledgement ->
    let authorities, lifecycle = Authored_run_acknowledgement.install acknowledgement in
    t.run_authorities <- authorities;
    Option.iter lifecycle ~f:(fun lifecycle -> t.lifecycle_agent_custody <- lifecycle));
  Option.iter
    initialization_acknowledgement
    ~f:Initialization_scope.Acknowledgement.install;
  Option.iter job_custody ~f:(fun update ->
    t.job_agent_custody <- Job_custody.Update.install update);
  Option.iter legacy_queue_custody ~f:(fun incoming ->
    t.legacy_queue_custody
    <- Legacy_queue_custody.install t.legacy_queue_custody ~incoming);
  Option.iter legacy_schedule_custody ~f:(fun incoming ->
    t.legacy_schedule_custody
    <- Legacy_schedule_custody.install t.legacy_schedule_custody ~incoming);
  if stopping_jobs
  then (
    Job_custody.retire t.job_agent_custody;
    t.job_agent_custody <- Job_custody.empty);
  (* Explicit Stop also retires loans when already Stopped. An ordinary
     constructor ACK while initially Stopped is not a Stop disposition. *)
  let stopping_legacy_custody =
    match retire_legacy_custody with
    | Some () -> true
    | None -> stopping_jobs
  in
  if stopping_legacy_custody
  then (
    Legacy_queue_custody.retire t.legacy_queue_custody;
    t.legacy_queue_custody <- Legacy_queue_custody.empty;
    Legacy_schedule_custody.retire t.legacy_schedule_custody;
    t.legacy_schedule_custody <- Legacy_schedule_custody.empty);
  Recurrence_runtime.retain_committed t.recurrence_runtime t.state;
  t.run_authorities
  <- Run_authorities.retain_current
       t.run_authorities
       ~index:t.state.run_state
       ~generation:t.state.identity.generation;
  retained_agent_custody t;
  install_live_run_agent_custody t;
  Option.iter queued_claim_acknowledgement ~f:(fun acknowledgement ->
    t.queued_event_borrow <- Some (Queued_claim_acknowledgement.install acknowledgement);
    t.idle_moderator_borrowed <- true);
  Atomic.set t.event_sequence t.state.counters.event_sequence
;;

let publish_committed_transition t (transition : Session_transition.t) =
  sync_extension_clock t;
  publish_durable t transition.events;
  t.services.state_committed t.state transition.events
;;

(* Every actual durable write passes this seam. Only real Turn starts sample
   fresh policy time; ledger/outcome/cleanup writes add no clock callback. *)
let persist_transition ?check_before_commit t (candidate : Session_transition.t) =
  let open Result.Let_syntax in
  let revision = t.state.counters.revision in
  let%bind payloads =
    List.filter_map candidate.events ~f:(fun event ->
      match event.Agent_protocol.Event.Durable.kind with
      | Operation_started ->
        Some (Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload)
      | _ -> None)
    |> Result.all
  in
  let starts_turn =
    List.exists payloads ~f:(function
      | Agent_protocol.Event.Durable.Payload.Operation_started { kind = Turn _; _ } ->
        true
      | _ -> false)
  in
  let policy_now = if starts_turn then Some (t.services.now ()) else None in
  let%bind admission =
    if starts_turn
    then (
      match check_before_commit with
      | None -> Error (error Invalid_state "host turn has no captured final owner check")
      | Some _ ->
        Goal_host_turn_admission.prepare
          ~previous:t.state
          ~candidate:candidate.state
          ~payloads
          ~now:(Option.value_exn policy_now))
    else Ok Goal_host_turn_admission.Allow
  in
  let%bind () =
    Option.value_map check_before_commit ~default:(Ok ()) ~f:(fun check -> check ())
  in
  let%bind () =
    if Int64.equal revision t.state.counters.revision
    then Ok ()
    else Error (error Conflict "candidate basis changed during final commit policy")
  in
  match admission with
  | Allow ->
    retain_recurrence_activations t candidate.state;
    t.persistence.commit ~command_audit:t.command_audit ~previous:t.state candidate
  | Held { goal_id; delta } ->
    let%bind () =
      match delta with
      | None -> Ok ()
      | Some delta ->
        (* Only the actual preexisting Goal may be held. Candidate operation,
           inputs, episode and charges are discarded. Reuse sampled policy time. *)
        let%bind held =
          Session_transition.apply
            ~now:(Option.value_exn policy_now)
            t.state
            ~delta
            ~payloads:[]
        in
        retain_recurrence_activations t held.state;
        let%map () = t.persistence.commit ~command_audit:None ~previous:t.state held in
        install_committed_transition ~admission_hold:goal_id t held;
        publish_committed_transition t held
    in
    Error (error Permission_denied "goal policy holds fresh host-turn admission")
;;

let prepare_completion_plan t (transition : Session_transition.t) completion_sample =
  match completion_sample with
  | None -> Ok None
  | Some sample ->
    Recurrence_runtime.prepare_completions
      t.recurrence_runtime
      ~sample
      ~previous:t.state
      ~current:transition.state
    |> Result.map ~f:Option.some
;;

let install
      ?queued_claim_acknowledgement
      ?authored_run_acknowledgement
      ?initialization_acknowledgement
      ?check_before_commit
      ?admission_hold
      ?retire_legacy_custody
      ?run_preparation
      ?commit_attempted
      ?job_custody
      ?completion_sample
      t
      transition
  =
  let open Result.Let_syntax in
  let%bind () =
    Option.value_map job_custody ~default:(Ok ()) ~f:(fun update ->
      Job_custody.Update.check_before_commit update ~current:t.job_agent_custody)
  in
  let%bind completion_plan = prepare_completion_plan t transition completion_sample in
  Option.iter commit_attempted ~f:(fun attempted -> attempted := true);
  let%bind () = persist_transition ?check_before_commit t transition in
  install_committed_transition
    ?queued_claim_acknowledgement
    ?authored_run_acknowledgement
    ?initialization_acknowledgement
    ?admission_hold
    ?retire_legacy_custody
    ?run_preparation
    ?job_custody
    t
    transition;
  Option.iter
    completion_plan
    ~f:(Recurrence_runtime.install_completions t.recurrence_runtime);
  publish_committed_transition t transition;
  Ok ()
;;

let current_snapshot t =
  Eio.Mutex.use_ro t.subscriber_mutex (fun () ->
    let active_tool_calls, active_agent_calls = Active_calls.snapshot t.active_calls in
    { (Session_state.snapshot ~now:(t.services.now ()) t.state) with
      active_tool_calls
    ; active_agent_calls
    })
;;

(* Every actor candidate, including audited commits and speculative staged work,
   selects source retirement before validation. Persistence and installation
   consume this exact prepared candidate without rewriting it. *)
let prepare_transition ?run_preparation t ~delta ~payloads =
  let open Result.Let_syntax in
  let%bind () =
    match run_preparation with
    | None -> Ok ()
    | Some preparation -> check_run_preparation t preparation
  in
  let now = t.services.now () in
  let%bind delta = Run_source_change.prepare t.state ~delta ~now in
  let%bind delta = Goal_catalog_source.prepare_change t.state ~delta in
  let%bind delta = Goal_run_settlement.prepare t.state ~delta in
  let%bind delta = Recurrence_source.prepare_change t.state ~delta in
  let%bind prepared = Session_transition.apply ~now t.state ~delta ~payloads in
  let%map () =
    match prepared.state.run_state with
    | None -> Ok ()
    | Some index ->
      let%bind additional_bytes =
        Run_job_capacity.bookkeeping_reserve index ~jobs:prepared.state.jobs
      in
      Run_state.validate_encoded_capacity_with_reserve
        index
        ~additional_bytes
        (Run_state.to_jsonaf index)
  in
  prepared
;;

let prepare_constructor_job_handoff ?run_preparation t ~delta ~payloads ~ids ~manager =
  let open Result.Let_syntax in
  let revision = t.state.counters.revision in
  let%bind prepared = prepare_transition ?run_preparation t ~delta ~payloads in
  let%bind index, binding = Constructor_job_handoff.prepare prepared.state ~manager in
  let source_changed =
    not (Option.equal Run_state.equal_observation prepared.state.run_state index)
  in
  let%bind prepared =
    if not source_changed
    then Ok prepared
    else (
      match index with
      | None -> Error (error Invalid_state "constructor removed its planned source index")
      | Some index ->
        let delta =
          match Session_replacement_delta.classify delta with
          | Some replacement ->
            let candidate = Session_replacement_delta.state replacement in
            Session_replacement_delta.with_state
              replacement
              { candidate with run_state = Some index }
          | None -> Session_delta.Batch [ delta; Run_state_changed index ]
        in
        prepare_transition ?run_preparation t ~delta ~payloads)
  in
  let jobs =
    List.filter_map ids ~f:(fun id ->
      List.find prepared.state.jobs ~f:(fun job -> Agent_protocol.Id.Job.equal id job.id))
  in
  let%map job_custody =
    Job_custody.prepare_constructor_handoff t.job_agent_custody ~jobs ~binding
  in
  prepared, job_custody, source_changed, revision
;;

let check_constructor_job_handoff
      t
      ~(prepared : Session_transition.t)
      ~job_custody
      ~manager
      ~revision
  =
  let open Result.Let_syntax in
  let%bind () =
    if Int64.equal revision t.state.counters.revision
    then Ok ()
    else Error (error Conflict "constructor checkpoint basis changed before commit")
  in
  let%bind _ = Constructor_job_handoff.prepare prepared.state ~manager in
  Option.value_map job_custody ~default:(Ok ()) ~f:(fun update ->
    Job_custody.Update.check_before_commit update ~current:t.job_agent_custody)
;;

let transition
      ?check_before_commit
      ?admission_hold
      ?retire_legacy_custody
      ?run_preparation
      ?commit_attempted
      ?job_custody
      ?completion_sample
      t
      ~delta
      ~payloads
  =
  Exn.protect
    ~finally:(fun () -> Option.iter job_custody ~f:Job_custody.Update.discard)
    ~f:(fun () ->
      let open Result.Let_syntax in
      let%bind transition = prepare_transition ?run_preparation t ~delta ~payloads in
      let%map () =
        install
          ?check_before_commit
          ?admission_hold
          ?retire_legacy_custody
          ?run_preparation
          ?commit_attempted
          ?job_custody
          ?completion_sample
          t
          transition
      in
      Agent_protocol.Session.(Session_state.summary t.state))
;;

let require_recurrence_controller t =
  Result.of_option
    t.recurrence_controller
    ~error:(error Invalid_state "recurrence controller lifecycle owner is not installed")
;;

let reconcile_recurrences_internal t =
  match t.state.recurrence_catalog with
  | None -> Ok ()
  | Some _ ->
    let open Result.Let_syntax in
    let%bind controller = require_recurrence_controller t in
    let%bind prepared = Recurrence_reconciliation.prepare t.state ~controller in
    let%bind () = Recurrence_reconciliation.check_current prepared in
    (match prepared.delta with
     | None -> Ok ()
     | Some delta ->
       let%bind candidate = prepare_transition t ~delta ~payloads:[] in
       let%bind () = Recurrence_reconciliation.check_current prepared in
       install t candidate)
;;

let inference_error _ = error Invalid_state "inference tracking admission failed"

let commit_inference_ledger ?run_preparation ?admission t ledger =
  let open Result.Let_syntax in
  let%bind () =
    match run_preparation with
    | None -> Ok ()
    | Some preparation -> check_run_preparation t preparation
  in
  if
    Int64.equal
      (Inference_ledger.revision ledger)
      (Inference_ledger.revision t.state.inference_ledger)
  then Ok ()
  else (
    let%bind accounting =
      Goal_ledger_reconciliation.prepare
        ~accounts:t.state.goal_accounts
        ~catalog:t.state.goal_catalog
        ~before:t.state.inference_ledger
        ~after:ledger
        ~admission
    in
    let accounting_delta =
      if Goal_ledger_reconciliation.changed accounting
      then
        [ Session_delta.Goal_accounts_changed
            (Option.value_exn (Goal_ledger_reconciliation.accounts accounting))
        ]
      else []
    in
    transition
      ?run_preparation
      t
      ~delta:(Session_delta.Batch (Inference_ledger_changed ledger :: accounting_delta))
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_updated
            (Session_state.summary { t.state with inference_ledger = ledger })
        ]
    |> Result.map ~f:ignore)
;;

let validate_inference_owner t (owner : Inference_owner.t) =
  if not (phys_equal t.inference_owner_identity (Graph_owner.issuing_actor owner))
  then Error (error Conflict "inference owner belongs to another actor")
  else if not (List.mem t.inference_owners owner ~equal:phys_equal)
  then Error (error Conflict "inference graph owner is no longer active")
  else Ok ()
;;

let open_inference_owner t source =
  if
    List.exists t.inference_owners ~f:(fun owner ->
      Transcript.Source_id.equal (Graph_owner.source owner) source)
  then Error (error Conflict "inference graph source is already owned")
  else
    let open Result.Let_syntax in
    let%bind qualified_source =
      Inference_ledger.qualify_source t.state.inference_ledger source
      |> Result.map_error ~f:inference_error
    in
    let owner =
      Graph_owner.create
        ~issuing_actor:t.inference_owner_identity
        ~source
        ~generation:t.state.identity.generation
        ~qualified_source
    in
    t.inference_owners <- owner :: t.inference_owners;
    Ok owner
;;

let inference_registration t owner handle =
  let open Result.Let_syntax in
  let%bind () = validate_inference_owner t owner in
  match
    Graph_owner.registration owner ~ordinal:(Inference_ledger.Handle.ordinal handle)
  with
  | Some registration
    when Inference_ledger.Handle.equal
           (Inference_owner.Registration.handle registration)
           handle -> Ok registration
  | Some _ | None -> Error (error Conflict "inference handle is not owned by this graph")
;;

let admit_inference
      ?run_preparation
      t
      owner
      relation
      operation_id
      invocation_id
      configuration
  =
  let open Result.Let_syntax in
  let%bind () = validate_inference_owner t owner in
  let%bind () =
    if
      Int.equal (Graph_owner.generation owner) t.state.identity.generation
      &&
      match Graph_owner.phase owner with
      | Open -> true
      | Sealed | Finished -> false
    then Ok ()
    else Error (error Conflict "inference graph excludes new attempts")
  in
  let%bind protected_ordinals =
    Goal_ledger_reconciliation.protected_ordinals
      t.state.goal_accounts
      ~ledger:t.state.inference_ledger
  in
  let%bind ledger, handle, _tracking =
    Inference_ledger.admit
      ~protected_ordinals
      t.state.inference_ledger
      ~source:(Graph_owner.source owner)
      ~relation
      ~operation_id
      ~invocation_id
      ~configuration
    |> Result.map_error ~f:inference_error
  in
  let%bind () = commit_inference_ledger ?run_preparation t ledger in
  Graph_owner.register owner ~handle ~configuration ~goal_id:None;
  Ok handle
;;

let validate_inference_attempt registration attempt =
  if
    Transcript.Scope.equal
      (Inference_ledger.Handle.scope (Inference_owner.Registration.handle registration))
      (Inference_runtime.Attempt.scope attempt)
    && Inference.Observation.Observation_id.equal
         (Inference_ledger.Handle.accounting_id
            (Inference_owner.Registration.handle registration))
         (Inference_runtime.Attempt.accounting_id attempt)
    && Inference.Observation.Configuration.equal
         (Inference_owner.Registration.configuration registration)
         (Inference_runtime.Attempt.configuration attempt)
  then Ok ()
  else Error (error Conflict "inference attempt differs from its admitted identity")
;;

let acknowledge_inference ?run_preparation t owner handle attempt =
  let open Result.Let_syntax in
  let%bind registration = inference_registration t owner handle in
  let%bind () = validate_inference_attempt registration attempt in
  Inference_owner.Registration.acknowledge registration attempt;
  let%bind ledger =
    Inference_ledger.set_state t.state.inference_ledger handle Running
    |> Result.map_error ~f:inference_error
  in
  commit_inference_ledger ?run_preparation t ledger
;;

let observe_inference ?run_preparation t handle incoming =
  let open Result.Let_syntax in
  let%bind ledger, _disposition =
    Inference_ledger.observe t.state.inference_ledger handle incoming
    |> Result.map_error ~f:inference_error
  in
  commit_inference_ledger ?run_preparation t ledger
;;

let observe_owned_inference ?run_preparation t owner incoming =
  let open Result.Let_syntax in
  let%bind () = validate_inference_owner t owner in
  let scope = Inference.Observation.scope incoming in
  let owner_matches =
    match Graph_owner.phase owner, Graph_owner.qualified_source owner with
    | (Open | Sealed), Some source ->
      Transcript.Source_id.equal source (Transcript.Scope.key scope).source
    | Finished, _ | _, None -> false
  in
  let%bind () =
    if owner_matches
    then Ok ()
    else Error (error Conflict "observation does not belong to a live inference graph")
  in
  match
    List.find (Inference_ledger.rows t.state.inference_ledger) ~f:(fun row ->
      let handle = Inference_ledger.Row.handle row in
      Int.equal (Graph_owner.generation owner) (Inference_ledger.Handle.generation handle)
      && Transcript.Scope.equal scope (Inference_ledger.Handle.scope handle))
  with
  | None -> Error (error Conflict "observation has no retained admitted scope")
  | Some row ->
    observe_inference ?run_preparation t (Inference_ledger.Row.handle row) incoming
;;

let complete_inference ?run_preparation t owner handle completion =
  let open Result.Let_syntax in
  let%bind registration = inference_registration t owner handle in
  let%bind () =
    validate_inference_attempt
      registration
      (Inference_client.Completion.attempt completion)
  in
  let state =
    match Inference_client.Completion.outcome completion with
    | Returned terminal -> Inference.Observation.Attempt_record.Terminal terminal
    | Interrupted { reason; delivery } -> Interrupted { reason; delivery }
  in
  let%bind ledger =
    Inference_ledger.set_state t.state.inference_ledger handle state
    |> Result.map_error ~f:inference_error
  in
  commit_inference_ledger ?run_preparation t ledger
;;

let interrupt_inference_row ledger handle ~delivery =
  let interrupt fallback =
    Inference_ledger.set_state
      ledger
      handle
      (Interrupted
         { reason = Host_interrupted; delivery = Option.value delivery ~default:fallback })
    |> Result.map_error ~f:inference_error
  in
  match
    Inference_ledger.find ledger ~ordinal:(Inference_ledger.Handle.ordinal handle)
  with
  | None -> Ok ledger
  | Some row ->
    (match
       Inference.Observation.Attempt_record.state (Inference_ledger.Row.record row)
     with
     | Terminal _ | Interrupted _ -> Ok ledger
     | Prepared -> interrupt Inference.Event.Terminal.Definitely_not_submitted
     | Running -> interrupt Inference.Event.Terminal.Possibly_submitted)
;;

let release_inference ?run_preparation t owner handle =
  let open Result.Let_syntax in
  let%bind registration = inference_registration t owner handle in
  Exn.protect
    ~f:(fun () ->
      let%bind ledger =
        interrupt_inference_row
          t.state.inference_ledger
          handle
          ~delivery:
            (Option.map
               (Inference_owner.Registration.attempt registration)
               ~f:Inference_runtime.Attempt.delivery)
      in
      commit_inference_ledger ?run_preparation t ledger)
    ~finally:(fun () ->
      Graph_owner.release owner ~ordinal:(Inference_ledger.Handle.ordinal handle))
;;

let seal_inference_owner t owner =
  let open Result.Let_syntax in
  let%map () = validate_inference_owner t owner in
  Graph_owner.seal owner
;;

let finish_inference_owner ?run_preparation t owner =
  let open Result.Let_syntax in
  let%bind () = validate_inference_owner t owner in
  let%bind () =
    match Graph_owner.phase owner with
    | Open -> Error (error Conflict "inference graph must exclude calls before joining")
    | Sealed -> Ok ()
    | Finished -> Error (error Conflict "inference graph already finished")
  in
  let rows = Inference_ledger.rows t.state.inference_ledger in
  let%bind ledger =
    List.fold_result rows ~init:t.state.inference_ledger ~f:(fun ledger row ->
      let handle = Inference_ledger.Row.handle row in
      match Graph_owner.qualified_source owner with
      | Some source
        when Transcript.Source_id.equal
               source
               (Transcript.Scope.key (Inference_ledger.Handle.scope handle)).source ->
        interrupt_inference_row
          ledger
          handle
          ~delivery:
            (Graph_owner.registration
               owner
               ~ordinal:(Inference_ledger.Handle.ordinal handle)
             |> Option.bind ~f:(fun registration ->
               Inference_owner.Registration.attempt registration)
             |> Option.map ~f:Inference_runtime.Attempt.delivery)
      | Some _ | None -> Ok ledger)
  in
  let%map () = commit_inference_ledger ?run_preparation t ledger in
  Graph_owner.finish owner;
  t.inference_owners
  <- List.filter t.inference_owners ~f:(fun current -> not (phys_equal current owner))
;;

let reconcile_inference_recovery t =
  let open Result.Let_syntax in
  let%bind () =
    if List.is_empty t.inference_owners
    then Ok ()
    else Error (error Conflict "live inference graphs must join before recovery")
  in
  let%bind ledger =
    List.fold_result
      (Inference_ledger.rows t.state.inference_ledger)
      ~init:t.state.inference_ledger
      ~f:(fun ledger row ->
        interrupt_inference_row ledger (Inference_ledger.Row.handle row) ~delivery:None)
  in
  commit_inference_ledger t ledger
;;

let abort_staged_work t ~owner =
  Staged_jobs.abort_owner t.staged_jobs ~owner;
  Staged_subscriptions.release_owner t.staged_subscriptions ~owner;
  Staged_ingress.release_owner t.staged_ingress ~owner;
  Staged_schedules.release_owner t.staged_schedules ~owner;
  Staged_notifications.release_owner t.staged_notifications ~owner;
  sync_extension_clock t
;;

let abort_all_staged_work t =
  Staged_jobs.abort_all t.staged_jobs;
  Staged_subscriptions.abort_all t.staged_subscriptions;
  Staged_ingress.abort_all t.staged_ingress;
  Staged_schedules.abort_all t.staged_schedules;
  Staged_notifications.abort_all t.staged_notifications;
  sync_extension_clock t
;;

let with_staged_transaction t owner f =
  match f () with
  | Ok _ as result ->
    Staged_jobs.commit t.staged_jobs ~owner;
    Staged_subscriptions.release_owner t.staged_subscriptions ~owner;
    Staged_ingress.release_owner t.staged_ingress ~owner;
    Staged_schedules.release_owner t.staged_schedules ~owner;
    Staged_notifications.release_owner t.staged_notifications ~owner;
    sync_extension_clock t;
    result
  | Error _ as result ->
    abort_staged_work t ~owner;
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    abort_staged_work t ~owner;
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let lookup_subscription t id =
  List.find t.state.subscriptions ~f:(fun subscription ->
    Agent_protocol.Id.Subscription.equal subscription.context.id id)
;;

let lookup_notification t id =
  List.find t.state.deliveries ~f:(fun value ->
    Agent_protocol.Id.Delivery.equal value.context.id id)
;;

let lookup_ingress t id =
  List.find t.state.ingress_registrations ~f:(fun value ->
    Agent_protocol.Id.Capability.equal value.External_ingress.context.id id)
;;

let ingress_subscription_lookup t subscriptions id epoch =
  let matches value =
    Agent_protocol.Id.Subscription.equal value.Agent_protocol.Subscription.context.id id
  in
  match
    List.find (subscriptions @ t.state.subscriptions) ~f:(fun value ->
      matches value && value.epoch = epoch)
  with
  | Some _ as result -> result
  | None -> List.find (List.rev subscriptions @ t.state.subscriptions) ~f:matches
;;

let selected_ingress t owner subscriptions =
  Staged_ingress.selected
    t.staged_ingress
    ~owner
    ~lookup:(lookup_ingress t)
    ~subscription:(ingress_subscription_lookup t subscriptions)
;;

let staged_work_changes t owner =
  let open Result.Let_syntax in
  let%bind subscriptions =
    Staged_subscriptions.selected
      t.staged_subscriptions
      ~owner
      ~lookup:(lookup_subscription t)
  in
  let%bind registrations = selected_ingress t owner subscriptions in
  let%bind () =
    List.fold_result registrations ~init:() ~f:(fun () value ->
      match lookup_ingress t value.External_ingress.context.id with
      | Some _ -> Ok ()
      | None ->
        let%bind due =
          Extension_clock.is_due
            t.extension_clock
            (Subscription value.context.subscription_id)
            ~now:(t.services.monotonic_now ())
        in
        (match
           (not due)
           && Agent_protocol.Timestamp.compare
                (t.services.now ())
                value.context.expires_at
              < 0
         with
         | true -> Ok ()
         | false ->
           Error
             (error Conflict "ingress subscription expired before registration commit")))
  in
  let%bind () =
    match
      List.exists registrations ~f:(fun value ->
        Option.is_none (lookup_ingress t value.External_ingress.context.id))
    with
    | false -> Ok ()
    | true ->
      Staged_ingress.check_capacity
        ~limits:t.services.ingress_limits
        ~generation:t.state.identity.generation
        ~now:(t.services.now ())
        ~subscriptions:(t.state.subscriptions @ subscriptions)
        ~values:(t.state.ingress_registrations @ registrations)
  in
  let%bind schedules =
    Staged_schedules.selected t.staged_schedules ~owner ~lookup:(fun id ->
      List.find t.state.schedules ~f:(fun value ->
        Agent_protocol.Id.Schedule.equal value.id id))
  in
  let%map notifications =
    Staged_notifications.selected
      t.staged_notifications
      ~owner
      ~lookup:(lookup_notification t)
  in
  let jobs = Staged_jobs.selected t.staged_jobs ~owner in
  ( List.map jobs ~f:(fun job -> Session_delta.Job_changed job)
    @ Staged_ingress.ordered_changes ~subscriptions ~registrations
    @ List.map schedules ~f:(fun schedule -> Session_delta.Schedule_changed schedule)
    @ List.map notifications ~f:(fun delivery -> Session_delta.Delivery_changed delivery)
  , List.map jobs ~f:(fun job ->
      Agent_protocol.Event.Durable.Payload.Job_state_changed job)
    @ List.map schedules ~f:(fun schedule ->
      Agent_protocol.Event.Durable.Payload.Schedule_state_changed schedule) )
;;

let prepare_background_job_launch t ~owner request =
  call t (Prepare_background_job (owner, request))
;;

let create_script_notification
      ?disclosure_pins
      t
      ~owner
      ~source
      ~correlation
      ~completion
      ~wake
  =
  call
    t
    (Create_script_notification
       (owner, source, correlation, completion, wake, disclosure_pins))
;;

let read_script_notification t ~owner ~source ~id =
  call t (Read_script_notification (owner, source, id))
;;

let select_notification_mutations t ~owner ~source ~receipts =
  call t (Select_notification_mutations (owner, source, receipts))
;;

let abort_notification_mutation t ~owner ~receipt =
  call t (Abort_notification_mutation (owner, receipt))
;;

let stage_background_job t ~job ~capacity =
  Eio.Cancel.protect (fun () ->
    match call t (Stage_background_job (job, capacity)) with
    | Ok () -> Ok ()
    | Error _ as result ->
      capacity.Staged_jobs.abort ();
      result)
;;

let select_background_jobs t ~owner ~ids = call t (Select_background_jobs (owner, ids))

let stage_subscription_mutation t ~owner ~source ~previous ~next =
  call t (Stage_subscription_mutation (owner, source, previous, next))
;;

let create_script_subscription
      t
      ~owner
      ~source
      ~kind
      ~lifetime_ms
      ~wake
      ~completion_schema
  =
  call
    t
    (Create_script_subscription (owner, source, kind, lifetime_ms, wake, completion_schema))
;;

let select_subscription_mutations t ~owner ~source ~receipts =
  call t (Select_subscription_mutations (owner, source, receipts))
;;

let finish_script_subscription t ~owner ~source ~id ~expected_epoch completion =
  call t (Finish_script_subscription (owner, source, id, expected_epoch, completion))
;;

let abort_subscription_mutation t ~owner ~receipt =
  Eio.Cancel.protect (fun () -> call t (Abort_subscription_mutation (owner, receipt)))
;;

let read_script_subscription t ~owner ~source ~id =
  call t (Read_script_subscription (owner, source, id))
;;

let expire_subscriptions t = call t Expire_subscriptions

let create_script_ingress
      t
      ~owner
      ~source
      ~subscription_id
      ~expected_epoch
      ~namespace
      ~schema
  =
  call
    t
    (Create_script_ingress
       (owner, source, subscription_id, expected_epoch, namespace, schema))
;;

let revoke_script_ingress t ~owner ~source ~id ~reason =
  call t (Revoke_script_ingress (owner, source, id, reason))
;;

let read_script_ingress t ~owner ~source ~id =
  call t (Read_script_ingress (owner, source, id))
;;

let select_ingress_mutations t ~owner ~source ~receipts =
  call t (Select_ingress_mutations (owner, source, receipts))
;;

let abort_ingress_mutation t ~owner ~receipt =
  Eio.Cancel.protect (fun () -> call t (Abort_ingress_mutation (owner, receipt)))
;;

let stage_schedule_mutation t ~owner ~source ~previous ~next =
  call t (Stage_schedule_mutation (owner, source, previous, next))
;;

let create_script_schedule t ~owner ~source ~delay_ms ~payload ~misfire =
  call t (Create_script_schedule (owner, source, delay_ms, payload, misfire))
;;

let select_schedule_mutations t ~owner ~source ~receipts =
  call t (Select_schedule_mutations (owner, source, receipts))
;;

let abort_schedule_mutation t ~owner ~receipt =
  Eio.Cancel.protect (fun () -> call t (Abort_schedule_mutation (owner, receipt)))
;;

let read_script_schedule t ~owner ~source ~id =
  call t (Read_script_schedule (owner, source, id))
;;

let subscription_is_due_at
      t
      (subscription : Agent_protocol.Subscription.t)
      ~now
      ~monotonic_now
  =
  match subscription.context.source with
  | None -> Ok (Agent_protocol.Timestamp.compare subscription.context.deadline now <= 0)
  | Some _ ->
    Extension_clock.is_due
      t.extension_clock
      (Subscription subscription.context.id)
      ~now:monotonic_now
;;

let subscription_is_due t subscription =
  subscription_is_due_at
    t
    subscription
    ~now:(t.services.now ())
    ~monotonic_now:(t.services.monotonic_now ())
;;

let subscription_terminal_time_at
      (subscription : Agent_protocol.Subscription.t)
      completion
      ~now
  =
  let lower_bound =
    match completion with
    | Agent_protocol.Completion.Expired -> subscription.context.deadline
    | _ -> subscription.context.created_at
  in
  match Agent_protocol.Timestamp.compare now lower_bound < 0 with
  | true -> lower_bound
  | false -> now
;;

let subscription_terminal_time t subscription completion =
  subscription_terminal_time_at subscription completion ~now:(t.services.now ())
;;

let expire_subscriptions_internal t =
  let module P = Agent_protocol in
  let open Result.Let_syntax in
  let%bind due =
    List.filter t.state.subscriptions ~f:(fun subscription ->
      Option.is_none subscription.result)
    |> List.map ~f:(fun subscription ->
      subscription_is_due t subscription
      |> Result.map ~f:(fun due -> Option.some_if due subscription))
    |> Result.all
    |> Result.map ~f:List.filter_opt
  in
  match due with
  | [] -> Ok 0
  | _ ->
    let open Result.Let_syntax in
    let%bind terminal =
      List.map due ~f:(fun subscription ->
        P.Subscription.finish
          subscription
          ~expected_epoch:subscription.epoch
          ~now:(subscription_terminal_time t subscription Expired)
          Expired
        |> Result.map ~f:fst)
      |> Result.all
    in
    let timer_ids =
      List.filter_map due ~f:(fun subscription -> subscription.timer_id)
      |> Hash_set.of_list (module P.Id.Schedule)
    in
    let cancelled =
      List.filter_map t.state.schedules ~f:(fun schedule ->
        match Hash_set.mem timer_ids schedule.id, schedule.status with
        | true, (P.Schedule.Scheduled | Delivering) ->
          Some { schedule with status = P.Schedule.Cancelled }
        | _ -> None)
    in
    let%map _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             (List.map terminal ~f:(fun subscription ->
                Session_delta.Subscription_expired subscription)
              @ List.map cancelled ~f:(fun schedule ->
                Session_delta.Schedule_changed schedule)))
        ~payloads:
          (List.map cancelled ~f:(fun schedule ->
             P.Event.Durable.Payload.Schedule_cancelled schedule))
    in
    List.length terminal
;;

let abort_background_job t ~owner ~id =
  Eio.Cancel.protect (fun () -> call t (Abort_background_job (owner, id)))
;;

let has_staged_background_job t ~owner ~id =
  call t (Has_staged_background_job (owner, id))
;;

let read_script_job t ~owner ~id = call t (Read_script_job (owner, id))

let read_script_job_result t ~owner ~expected =
  call t (Read_script_job_result (owner, expected))
;;

let cancel_script_job t ~owner ~id = call t (Cancel_script_job (owner, id))

let commit_extensions_internal t generation expected_revision changes =
  let open Result.Let_syntax in
  if
    generation <> t.state.identity.generation
    || not (Int64.equal expected_revision t.state.counters.revision)
  then
    Error
      (error Conflict "extension transaction uses a stale session revision or generation")
  else if
    (moderator_is_borrowed t || not (List.is_empty t.invocation_executions))
    && List.exists changes ~f:(function
      | Extension_change.Moderator_state _ -> moderator_is_borrowed t
      | Invocation value ->
        List.exists t.invocation_executions ~f:(fun execution ->
          Agent_protocol.Id.Invocation.compare
            value.context.id
            execution.dispatched.context.id
          = 0)
        || Option.exists t.moderator_borrow ~f:(fun borrow ->
          Agent_protocol.Id.Invocation.compare
            value.context.id
            borrow.invocation.context.id
          = 0)
      | _ -> false)
  then Error (error Conflict "invocation callback owns this state transaction")
  else if List.is_empty changes || List.length changes > 256
  then
    Error
      (error
         Resource_limit
         "extension transaction must contain between 1 and 256 changes")
  else if
    List.exists changes ~f:(function
      | Extension_change.Publish _ -> true
      | _ -> false)
    && (Option.is_some t.state.active_operation
        || t.idle_moderator_borrowed
        || t.state.halted
        || (not
              (Agent_protocol.Session.equal_desired_state
                 t.state.lifecycle.desired
                 Running))
        ||
        match t.state.lifecycle.observed with
        | Agent_protocol.Session.Idle -> false
        | _ -> true)
  then
    Error
      (error
         Invalid_state
         "notification publication requires an idle running session safe point")
  else (
    let new_jobs = Hash_set.create (module Agent_protocol.Id.Job) in
    let new_schedules = Hash_set.create (module Agent_protocol.Id.Schedule) in
    let%bind deltas =
      List.map changes ~f:(function
        | Extension_change.Invocation value -> Ok (Session_delta.Invocation_changed value)
        | Subscription value -> Ok (Session_delta.Subscription_changed value)
        | Delivery value ->
          (match value.completion_projection with
           | Some _
             when not
                    (List.exists t.state.deliveries ~f:(fun old ->
                       Agent_protocol.Id.Delivery.equal old.context.id value.context.id))
             ->
             Error
               (error
                  Permission_denied
                  "standalone delivery requires checked adapter admission")
           | _ -> Ok (Session_delta.Delivery_changed value))
        | Publish (value, entry) ->
          let%bind () =
            match value.completion_projection with
            | Some _
              when not
                     (List.exists t.state.deliveries ~f:(fun old ->
                        Agent_protocol.Id.Delivery.equal old.context.id value.context.id))
              ->
              Error
                (error
                   Permission_denied
                   "standalone delivery requires checked adapter admission")
            | _ -> Ok ()
          in
          let%map () = Notification_history.validate ~delivery:value entry in
          Session_delta.Delivery_committed (value, entry)
        | Moderator_state value -> Ok (Session_delta.Moderator_changed value)
        | Start_job job ->
          let%bind () =
            Extension_invariants.owner
              ~session_id:t.state.identity.session_id
              ~generation
              job.session_id
              job.generation
          in
          if
            Hash_set.mem new_jobs job.id
            || List.exists t.state.jobs ~f:(fun old ->
              Agent_protocol.Id.Job.compare old.id job.id = 0)
          then Error (error Conflict "extension job identity is already admitted")
          else (
            match job.status with
            | Queued ->
              Hash_set.add new_jobs job.id;
              Ok (Session_delta.Job_changed job)
            | _ -> Error (error Invalid_state "extension job must start queued"))
        | Schedule schedule ->
          let%bind () =
            Extension_invariants.owner
              ~session_id:t.state.identity.session_id
              ~generation
              schedule.session_id
              schedule.generation
          in
          if
            Hash_set.mem new_schedules schedule.id
            || List.exists t.state.schedules ~f:(fun old ->
              Agent_protocol.Id.Schedule.compare old.id schedule.id = 0)
          then Error (error Conflict "extension schedule identity is already admitted")
          else (
            match schedule.status with
            | Scheduled ->
              Hash_set.add new_schedules schedule.id;
              Ok (Session_delta.Schedule_changed schedule)
            | _ -> Error (error Invalid_state "extension schedule must start scheduled")))
      |> Result.all
    in
    let delta = Session_delta.Batch deltas in
    let%bind candidate = Session_delta.apply t.state delta in
    let old_ids = Hash_set.create (module History_entry.Id) in
    List.iter t.state.conversation.canonical_history ~f:(fun entry ->
      Hash_set.add old_ids entry.id);
    let appended =
      List.filter candidate.conversation.canonical_history ~f:(fun entry ->
        not (Hash_set.mem old_ids entry.id))
    in
    let history_payloads =
      if List.is_empty appended
      then []
      else [ Agent_protocol.Event.Durable.Payload.History_appended appended ]
    in
    let work_payloads =
      List.filter_map changes ~f:(function
        | Extension_change.Start_job job ->
          Some (Agent_protocol.Event.Durable.Payload.Job_state_changed job)
        | Schedule schedule ->
          Some (Agent_protocol.Event.Durable.Payload.Schedule_created schedule)
        | _ -> None)
    in
    transition t ~delta ~payloads:(history_payloads @ work_payloads))
;;

let replace_operation_worker t worker ~intended_manager =
  if
    Option.exists t.runtime_retirement ~f:(fun retirement ->
      match Eio.Promise.peek retirement.finished with
      | Some (Ok (Ok ())) ->
        Option.is_some worker
        && (retirement.closing || Option.is_some t.state.active_operation)
      | Some (Ok (Error _) | Error _) | None -> true)
  then Error (error Conflict "cannot replace a retiring runtime")
  else if moderator_is_borrowed t
  then Error (error Conflict "cannot replace a borrowed moderator runtime")
  else (
    match worker, t.state.active_operation, t.idle_moderator_borrowed with
    | None, Some _, _ -> Error (error Conflict "cannot unload an active runtime")
    | None, None, true -> Error (error Conflict "cannot unload a borrowed moderator")
    | None, None, false | Some _, _, _ ->
      if Option.is_some t.operation_worker then close_recurrence_activations t;
      let job_custody =
        Job_custody.retain_runtime t.job_agent_custody ~manager:intended_manager
      in
      let queue_custody =
        Legacy_queue_custody.retain_runtime
          t.legacy_queue_custody
          ~manager:intended_manager
      in
      let schedule_custody =
        Legacy_schedule_custody.retain_runtime
          t.legacy_schedule_custody
          ~manager:intended_manager
      in
      t.operation_worker <- worker;
      t.inference_execution <- None;
      t.job_agent_custody
      <- Job_custody.install_retained t.job_agent_custody ~incoming:job_custody;
      t.legacy_queue_custody
      <- Legacy_queue_custody.install t.legacy_queue_custody ~incoming:queue_custody;
      t.legacy_schedule_custody
      <- Legacy_schedule_custody.install
           t.legacy_schedule_custody
           ~incoming:schedule_custody;
      t.moderator_manager <- None;
      (match worker with
       | Some _ -> t.runtime_retirement <- None
       | None ->
         (match t.runtime_retirement with
          | Some retirement when not retirement.closing -> t.runtime_retirement <- None
          | Some _ | None -> ()));
      Ok ())
;;

let set_operation_worker t worker =
  replace_operation_worker t worker ~intended_manager:None
;;

let runtime_admission_open t =
  Option.is_none t.runtime_retirement && Option.is_none t.lifecycle_fence
;;

let require_runtime_admission t =
  if runtime_admission_open t
  then Ok ()
  else Error (error Conflict "session runtime is retiring")
;;

let retire_runtime_worker t ~closing =
  match t.runtime_retirement with
  | Some retirement ->
    retirement.closing <- retirement.closing || closing;
    Ok retirement.finished
  | None ->
    let finished, finish = Eio.Promise.create () in
    let pending_operation =
      Option.map t.state.active_operation ~f:(fun operation -> operation.id)
    in
    close_recurrence_activations t;
    t.runtime_retirement <- Some { finished; finish; pending_operation; closing };
    Legacy_queue_custody.retire t.legacy_queue_custody;
    t.legacy_queue_custody <- Legacy_queue_custody.empty;
    Legacy_schedule_custody.retire t.legacy_schedule_custody;
    t.legacy_schedule_custody <- Legacy_schedule_custody.empty;
    t.moderator_manager <- None;
    t.job_agent_custody
    <- Job_custody.install_retained
         t.job_agent_custody
         ~incoming:(Job_custody.retain_runtime t.job_agent_custody ~manager:None);
    retire_instruction_continuations t;
    Option.iter t.operation_agent_custody ~f:(fun (_, admission) ->
      retire_agent_admission admission);
    Option.iter t.active_cancel ~f:(fun cancel -> cancel ());
    if Option.is_none pending_operation then Eio.Promise.resolve finish (Ok (Ok ()));
    Ok finished
;;

let finish_runtime_retirement t operation_id result =
  Option.iter t.runtime_retirement ~f:(fun retirement ->
    match retirement.pending_operation with
    | Some pending when Agent_protocol.Id.Operation.equal pending operation_id ->
      retirement.pending_operation <- None;
      Eio.Promise.resolve retirement.finish result
    | Some _ | None -> ())
;;

let with_runtime_terminal t operation_id f =
  match f () with
  | result ->
    finish_runtime_retirement t operation_id (Ok result);
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    finish_runtime_retirement t operation_id (Error (exn, backtrace));
    Exn.raise_with_original_backtrace exn backtrace
;;

let set_runtime_worker t worker inference moderator_manager =
  let open Result.Let_syntax in
  let%bind () =
    match worker, inference with
    | None, None -> Ok ()
    | Some _, Some _ ->
      (match t.state.runtime_initialization with
       | Ready -> Ok ()
       | Pending _ -> Error (error Invalid_state "runtime initialization is not complete"))
    | None, Some _ | Some _, None ->
      Error
        (error Invalid_request "worker and selected inference must be installed together")
  in
  let%bind () =
    match worker, moderator_manager with
    | None, Some _ ->
      Error (error Invalid_request "moderator resource requires an installed runtime")
    | Some _, _ | None, None -> Ok ()
  in
  let%bind () =
    match moderator_manager with
    | None -> Ok ()
    | Some _ ->
      Constructor_job_handoff.prepare t.state ~manager:moderator_manager
      |> Result.map ~f:ignore
  in
  let%map () = replace_operation_worker t worker ~intended_manager:moderator_manager in
  t.inference_execution <- inference;
  t.moderator_manager <- moderator_manager
;;

let change_moderator t moderator =
  if moderator_is_borrowed t
  then Error (error Conflict "moderator invocation owns the moderator checkpoint")
  else (
    let state = { t.state with moderator } in
    transition
      t
      ~delta:(Session_delta.Moderator_changed moderator)
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_updated
            (Session_state.summary state)
        ])
;;

let change_workspace t workspace =
  match t.state.lifecycle.observed, t.state.active_operation with
  | Agent_protocol.Session.Stopped, None when List.is_empty t.job_scopes ->
    transition t ~delta:(Session_delta.Workspace_changed workspace) ~payloads:[]
  | ( ( Queued_for_slot
      | Starting
      | Recovering
      | Idle
      | Running_turn _
      | Compacting _
      | Waiting_for_permission _
      | Stopping
      | Failed _
      | Stopped )
    , _ ) ->
    Error (error Invalid_state "workspace replacement requires a stopped session")
;;

let lifecycle
      ?authored_run_acknowledgement
      ?run_preparation
      ?commit_attempted
      ?(extra_deltas = [])
      t
      ~desired
      ~observed
  =
  let open Result.Let_syntax in
  let%bind transition =
    let delta = Session_delta.Lifecycle_changed { desired; observed } in
    prepare_transition
      t
      ~delta:
        (Session_delta.Batch
           (extra_deltas
            @
            if t.state.pending_initial_start
            then [ Session_delta.Initial_start_consumed; delta ]
            else [ delta ]))
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_state_changed
            { desired_state = desired; observed_state = observed }
        ]
  in
  let check_before_commit =
    Option.map authored_run_acknowledgement ~f:(fun acknowledgement ->
      let basis () =
        let%bind () = require_runtime_admission t in
        let%bind () =
          match run_preparation with
          | None -> Ok ()
          | Some preparation ->
            (match t.run_preparation with
             | Some current when phys_equal current preparation ->
               Run_preparation.check_basis
                 preparation
                 ~owner:t.run_preparation_owner
                 ~state:t.state
             | Some _ | None ->
               Error (error Conflict "authored startup preparation changed"))
        in
        Authored_run_acknowledgement.check_basis
          acknowledgement
          ~previous:t.state
          ~authorities:t.run_authorities
          ~lifecycle:t.lifecycle_agent_custody
          ~candidate:transition
      in
      fun () ->
        let%bind () = basis () in
        let%bind () =
          Option.value_map run_preparation ~default:(Ok ()) ~f:(check_run_preparation t)
        in
        let scope = Authored_run_acknowledgement.scope acknowledgement in
        let%bind () = Run_admission.Scope.authorize scope t.state in
        let%bind () =
          if Run_admission.Scope.startup_pending scope
          then Ok ()
          else Error (error Conflict "authored startup callback is no longer pending")
        in
        (* Candidate preparation already sampled clocks. All policy/startup
           callbacks precede final originals, then the callback-free joint basis. *)
        let%bind () =
          Option.value_map run_preparation ~default:(Ok ()) ~f:(fun preparation ->
            Run_preparation.check_current
              preparation
              ~owner:t.run_preparation_owner
              ~state:t.state)
        in
        let%bind () = Run_admission.Scope.check_current scope in
        let%map () = basis () in
        Option.iter commit_attempted ~f:(fun attempted -> attempted := true))
  in
  let commit_attempted =
    match authored_run_acknowledgement with
    | None -> commit_attempted
    | Some _ -> None
  in
  let%map () =
    install
      ?authored_run_acknowledgement
      ?check_before_commit
      ?run_preparation
      ?commit_attempted
      t
      transition
  in
  Session_state.summary t.state
;;

let start_internal
      ?authored_run_acknowledgement
      ?run_preparation
      ?commit_attempted
      ?expected_parent_stop_epoch
      ?(extra_deltas = [])
      t
  =
  if not (runtime_admission_open t)
  then Error (error Conflict "session runtime is retiring")
  else if
    match t.state.runtime_initialization with
    | Pending _ -> true
    | Ready -> false
  then Error (error Invalid_state "runtime initialization must complete before start")
  else if
    Option.exists expected_parent_stop_epoch ~f:(fun epoch ->
      not (Option.equal Int64.equal t.state.parent_stop_epoch (Some epoch)))
  then Error (error Conflict "parent stopped while child start was being prepared")
  else if t.idle_moderator_borrowed
  then Error (error Conflict "cannot restart while the idle moderator is borrowed")
  else (
    match t.state.lifecycle.desired, t.state.lifecycle.observed with
    | Running, (Idle | Running_turn _ | Starting | Queued_for_slot) ->
      Ok (Session_state.summary t.state)
    | _, _ ->
      let open Result.Let_syntax in
      let%bind _ =
        lifecycle
          ?authored_run_acknowledgement
          ?run_preparation
          ?commit_attempted
          ~extra_deltas
          t
          ~desired:Running
          ~observed:Starting
      in
      lifecycle ?run_preparation ?commit_attempted t ~desired:Running ~observed:Idle)
;;

let queue_start_internal t =
  if not (runtime_admission_open t)
  then Error (error Conflict "session runtime is retiring")
  else if t.idle_moderator_borrowed
  then Error (error Conflict "cannot restart while the idle moderator is borrowed")
  else (
    match t.state.lifecycle.desired, t.state.lifecycle.observed with
    | Running, (Queued_for_slot | Starting | Idle | Running_turn _) ->
      Ok (Session_state.summary t.state)
    | _, _ -> lifecycle t ~desired:Running ~observed:Queued_for_slot)
;;

let activate_queued_start t =
  if not (runtime_admission_open t)
  then Error (error Conflict "session runtime is retiring")
  else if
    match t.state.runtime_initialization with
    | Pending _ -> true
    | Ready -> false
  then
    Error (error Invalid_state "runtime initialization must complete before activation")
  else (
    match t.state.lifecycle.desired, t.state.lifecycle.observed with
    | Running, Queued_for_slot ->
      let open Result.Let_syntax in
      let%bind _ = lifecycle t ~desired:Running ~observed:Starting in
      lifecycle t ~desired:Running ~observed:Idle
    | Running, (Starting | Idle | Running_turn _) -> Ok (Session_state.summary t.state)
    | _, _ -> Error (error Invalid_state "session has no queued start intent"))
;;

let background_terminal_result (job : Agent_protocol.Job.t) completion =
  match job.kind with
  | Async_tool -> Some (Agent_protocol.Completion.to_json completion)
  | _ -> job.result
;;

let stopped_jobs t mode =
  if Agent_protocol.Session.equal_stop_mode mode Graceful
  then []
  else
    List.filter_map t.state.jobs ~f:(fun (job : Agent_protocol.Job.t) ->
      match job.status with
      | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
        Some
          { job with
            status = Cancelled
          ; result = background_terminal_result job (Cancelled "session stopped")
          ; completed_at = Some (t.services.now ())
          ; delivery =
              (match job.delivery with
               | Not_required -> Not_required
               | _ -> Pending)
          }
      | Succeeded | Failed _ | Cancelled | Interrupted _ -> None)
;;

let cancel_permission t reason (permission : Agent_protocol.Permission.t) =
  { permission with
    state = Cancelled
  ; resolution =
      Some
        { choice = Deny
        ; principal_id = None
        ; resolved_at = t.services.now ()
        ; reason = Some reason
        }
  }
;;

let resolve_cleaned_permission_waiters t permissions =
  List.iter permissions ~f:(fun permission ->
    Option.iter permission.Agent_protocol.Permission.resolution ~f:(fun resolution ->
      Option.iter (Map.find !(t.permission_waiters) permission.id) ~f:(fun waiter ->
        Eio.Promise.resolve waiter.resolver resolution));
    t.permission_waiters := Map.remove !(t.permission_waiters) permission.id)
;;

let pending_invocation_permissions t ~matches =
  List.filter t.state.permissions ~f:(fun permission ->
    Agent_protocol.Permission.equal_state permission.state Pending
    &&
    match permission.owner with
    | Invocation id -> matches id
    | Operation _ -> false)
;;

let permission_resume_observed
      t
      ~resolved
      ~(fallback : Agent_protocol.Session.observed_state)
  =
  match t.state.lifecycle.desired, t.state.active_operation with
  | Stopped, None -> Agent_protocol.Session.Stopped
  | _ ->
    (match
       List.find t.state.permissions ~f:(fun permission ->
         Agent_protocol.Permission.equal_state permission.state Pending
         && not
              (List.exists resolved ~f:(Agent_protocol.Id.Permission.equal permission.id)))
     with
     | Some permission -> Waiting_for_permission permission.id
     | None ->
       (match fallback with
        | (Running_turn id | Compacting id)
          when not
                 (Option.exists t.state.active_operation ~f:(fun operation ->
                    Agent_protocol.Id.Operation.equal operation.id id)) ->
          (match t.state.lifecycle.desired with
           | Running -> Idle
           | Stopped -> Stopped)
        | _ -> fallback))
;;

let cleanup_invocation_permissions t ids =
  let permissions =
    pending_invocation_permissions t ~matches:(fun id ->
      List.mem ids id ~equal:Agent_protocol.Id.Invocation.equal)
    |> List.map
         ~f:(cancel_permission t "invocation finished before permission resolution")
  in
  let lifecycle =
    match t.state.lifecycle.observed with
    | Waiting_for_permission id
      when List.exists permissions ~f:(fun p ->
             Agent_protocol.Id.Permission.equal p.id id) ->
      Option.map (Map.find !(t.permission_waiters) id) ~f:(fun waiter ->
        { t.state.lifecycle with
          observed =
            permission_resume_observed
              t
              ~resolved:
                (List.map permissions ~f:(fun p -> p.Agent_protocol.Permission.id))
              ~fallback:waiter.resume_observed
        })
    | _ -> None
  in
  ( permissions
  , List.map permissions ~f:(fun permission ->
      Session_delta.Permission_changed permission)
    @ Option.to_list
        (Option.map lifecycle ~f:(fun value -> Session_delta.Lifecycle_changed value))
  , List.map permissions ~f:(fun permission ->
      Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
    @ Option.to_list
        (Option.map lifecycle ~f:(fun value ->
           Agent_protocol.Event.Durable.Payload.Session_state_changed
             { desired_state = value.desired; observed_state = value.observed })) )
;;

let stop_transition
      ?parent_stop_epoch
      ?managed_receipt
      t
      mode
      ~extension_work
      lifecycle
      deltas
      payloads
  =
  let open Result.Let_syntax in
  let%bind discarded =
    Observation_follow_up.discard t.state.invocations ~reason:"session stopped"
  in
  let%bind events =
    Observation_follow_up.discard_events
      t.state.moderator_executions
      ~reason:"session stopped"
  in
  let jobs = stopped_jobs t mode in
  let job_custody = Job_custody.prepare_retirement t.job_agent_custody in
  let permissions =
    pending_invocation_permissions t ~matches:(fun _ -> true)
    |> List.map ~f:(cancel_permission t "session stopped")
  in
  let%map result =
    transition
      ~retire_legacy_custody:()
      ~job_custody
      t
      ~delta:
        (Session_delta.Batch
           ((if t.state.pending_initial_start
             then [ Session_delta.Initial_start_consumed ]
             else [])
            @ (Session_delta.Lifecycle_changed lifecycle :: deltas)
            @ Option.to_list
                (Option.map managed_receipt ~f:(fun receipt ->
                   Session_delta.Managed_stop_admitted receipt))
            @ Option.to_list
                (Option.map parent_stop_epoch ~f:(fun epoch ->
                   Session_delta.Parent_stop_epoch_changed epoch))
            @ List.map discarded ~f:Observation_follow_up.delta
            @ List.map events ~f:Observation_follow_up.event_delta
            @ List.map permissions ~f:(fun permission ->
              Session_delta.Permission_changed permission)
            @ List.map jobs ~f:(fun job -> Session_delta.Job_changed job)
            @ Extension_stop.deltas extension_work))
      ~payloads:
        ((Agent_protocol.Event.Durable.Payload.Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
          :: payloads)
         @ List.map jobs ~f:(fun job ->
           Agent_protocol.Event.Durable.Payload.Job_state_changed job)
         @ List.map permissions ~f:(fun permission ->
           Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
         @ Extension_stop.payloads extension_work)
  in
  cancel_job_scopes t jobs;
  resolve_cleaned_permission_waiters t permissions;
  result
;;

let cancel_event_for_operation t operation_id =
  Option.iter t.queued_event_borrow ~f:(fun borrow ->
    if
      Option.exists
        borrow.receipt.context.operation_id
        ~f:(Agent_protocol.Id.Operation.equal operation_id)
    then (
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ())))
;;

let cancel_independent_moderator t =
  Option.iter t.moderator_borrow ~f:(fun borrow ->
    match borrow.operation_id with
    | Some _ -> ()
    | None ->
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()));
  Option.iter t.queued_event_borrow ~f:(fun borrow ->
    match borrow.receipt.context.operation_id with
    | Some _ -> ()
    | None ->
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()))
;;

(* This safety disposition is durable before Stop effects. Its commit must not
   accept the original Stop audit: a later Stop failure leaves recurrences paused
   without claiming that stopping or external cancellation completed. The same
   serialized command restores the original audit for its final Stop commit. *)
let pause_recurrences_before_stop t =
  let open Result.Let_syntax in
  let%bind paused =
    match t.state.recurrence_catalog with
    | None -> Ok None
    | Some catalog -> Recurrence_catalog.pause_for_stop catalog
  in
  match paused with
  | None -> Ok false
  | Some catalog ->
    let audit = t.command_audit in
    Exn.protect
      ~finally:(fun () -> t.command_audit <- audit)
      ~f:(fun () ->
        t.command_audit <- None;
        let%map _ =
          transition
            t
            ~delta:(Session_delta.Recurrence_catalog_changed catalog)
            ~payloads:[]
        in
        true)
;;

let stop_internal ?parent_stop_epoch ?managed_receipt t mode =
  let open Result.Let_syntax in
  let%bind recurrence_paused = pause_recurrences_before_stop t in
  let%bind extension_work =
    Extension_stop.prepare ~state:t.state ~mode ~now:(t.services.now ())
  in
  match t.state.active_operation with
  | None ->
    let open Result.Let_syntax in
    let%map session =
      match t.state.lifecycle.desired, t.state.lifecycle.observed with
      | Stopped, Stopped
        when (not recurrence_paused)
             && (not t.state.pending_initial_start)
             && Option.is_none parent_stop_epoch
             && Option.is_none managed_receipt
             && List.is_empty (stopped_jobs t mode)
             && Job_custody.is_empty t.job_agent_custody
             && Legacy_queue_custody.is_empty t.legacy_queue_custody
             && Legacy_schedule_custody.is_empty t.legacy_schedule_custody
             && Extension_stop.is_empty extension_work
             && (not
                   (Option.exists t.state.recurrence_catalog ~f:(fun catalog ->
                      List.exists (Recurrence_catalog.entries catalog) ~f:(fun entry ->
                        match Recurrence_entry.control entry with
                        | Enabled -> true
                        | Paused _ | Retired _ -> false))))
             && not
                  (List.exists t.state.invocations ~f:Observation_follow_up.pending
                   || List.exists
                        t.state.moderator_executions
                        ~f:Observation_follow_up.pending_event) ->
        Ok (Session_state.summary t.state)
      | _, _ ->
        stop_transition
          ?parent_stop_epoch
          ?managed_receipt
          t
          mode
          ~extension_work
          { desired = Stopped; observed = Stopped }
          []
          []
    in
    t.initialization_scope <- None;
    (match mode with
     | Cancel ->
       cancel_independent_moderator t;
       abort_all_staged_work t
     | Graceful -> ());
    session
  | Some operation ->
    let lifecycle =
      Session_state.Lifecycle.{ desired = Stopped; observed = t.state.lifecycle.observed }
    in
    let operation =
      match mode with
      | Agent_protocol.Session.Graceful -> operation
      | Cancel ->
        { operation with
          state = Agent_protocol.Operation.Cancelling
        ; updated_at = t.services.now ()
        }
    in
    let open Result.Let_syntax in
    let%bind session =
      stop_transition
        ?parent_stop_epoch
        ?managed_receipt
        t
        mode
        ~extension_work
        lifecycle
        [ Session_delta.Active_operation_changed (Some operation) ]
        []
    in
    t.initialization_scope <- None;
    if Agent_protocol.Session.equal_stop_mode mode Cancel
    then (
      abort_all_staged_work t;
      cancel_event_for_operation t operation.id;
      cancel_independent_moderator t;
      Option.iter t.active_cancel ~f:(fun cancel -> cancel ()));
    Ok session
;;

let stop_managed_internal t reference key mode generation max_receipts =
  let module M = Managed_stop in
  let module P = Agent_protocol in
  let module D = Agent_store.Delegation_store in
  let open Result.Let_syntax in
  let%bind () =
    match t.state.spec.delegation with
    | Some current
      when D.Reference.equal current reference
           && P.Id.Session.equal reference.child_session_id t.state.identity.session_id ->
      Ok ()
    | _ ->
      Error (error Permission_denied "delegation.stop: child relationship does not match")
  in
  match
    List.find t.state.managed_stops ~f:(fun receipt ->
      D.Reference.equal receipt.M.reference reference
      && P.Idempotency_key.equal receipt.key key)
  with
  | Some receipt ->
    (match P.Session.equal_stop_mode receipt.mode mode with
     | true -> Ok receipt
     | false -> Error (error Conflict "managed stop key was used for a different mode"))
  | None ->
    let%bind () =
      match Int.equal generation t.state.identity.generation with
      | true -> Ok ()
      | false -> Error (error Conflict "child generation changed before managed stop")
    in
    let%bind () =
      match max_receipts with
      | Some maximum when maximum < 1 || List.length t.state.managed_stops >= maximum ->
        Error (error Invalid_state "managed stop receipt capacity reached")
      | None | Some _ -> Ok ()
    in
    let%bind stop_epoch =
      match t.state.lifecycle.desired with
      | Stopped -> Ok t.state.stop_epoch
      | Running when Int64.equal t.state.stop_epoch Int64.max_value ->
        Error (error Invalid_state "stop epoch overflow")
      | Running -> Ok Int64.(t.state.stop_epoch + 1L)
    in
    let%bind receipt =
      M.create ~reference ~key ~mode ~generation ~stop_epoch ~now:(t.services.now ())
    in
    let%map _ = stop_internal ~managed_receipt:receipt t mode in
    receipt
;;

let append_history t entries =
  transition
    t
    ~delta:(Session_delta.Canonical_entries_appended entries)
    ~payloads:[ Agent_protocol.Event.Durable.Payload.History_appended entries ]
;;

let prepare_pending t ?(archive = None) change =
  let retention = Pending_disposition.Retention.default in
  Pending_transition.prepare
    t.state
    ~change
    ~retention
    ~archive
    ~limits:Session_delta.native_limits
;;

let eligible_pending t boundary =
  let open Result.Let_syntax in
  let%bind eligibility =
    Pending_eligibility.create
      t.state
      ~boundary
      ~runtime_admission_open:(runtime_admission_open t)
  in
  Pending_eligibility.eligible_prefix
    eligibility
    t.state.conversation.deferred_user_entries
;;

let prepare_pending_inputs t ~owner timing entries =
  let open Result.Let_syntax in
  let%bind binding =
    Agent_protocol.Pending_input.Binding.create
      timing
      ~generation:t.state.identity.generation
      ~operation:t.state.active_operation
  in
  let%bind documents =
    List.map entries ~f:(fun entry ->
      let%bind value =
        Agent_protocol.Pending_input.create
          ~entry
          ~generation:t.state.identity.generation
          ~binding
      in
      Pending_input_document.authored ~owner value ~limits:Session_delta.native_limits
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request
          (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))))
    |> Result.all
  in
  prepare_pending t (Enqueue documents)
;;

let defer_history
      ?(extra_deltas = [])
      ?(owner = Pending_input_document.Owner.Host_internal)
      ?(timing = Agent_protocol.Pending_input.Timing.Safe_boundary)
      t
      entries
  =
  let open Result.Let_syntax in
  let%bind prepared = prepare_pending_inputs t ~owner timing entries in
  let payloads =
    List.map entries ~f:(fun entry ->
      Agent_protocol.Event.Durable.Payload.History_message_deferred entry)
  in
  transition
    t
    ~delta:(Session_delta.Batch (extra_deltas @ [ Pending_transition.delta prepared ]))
    ~payloads
;;

let write_attachment t attachment_id =
  match
    List.find t.state.attachments ~f:(fun attachment ->
      Agent_protocol.Id.Attachment.compare attachment.id attachment_id = 0)
  with
  | None -> Error (error Invalid_request "attachment is not active")
  | Some { mode = Read_only; _ } ->
    Error (error Permission_denied "read-only attachment cannot mutate the session")
  | Some ({ mode = Read_write; _ } as attachment) -> Ok attachment
  | Some ({ mode = Owner_read_write; owner_lease = None; _ } as attachment) ->
    Ok attachment
  | Some ({ mode = Owner_read_write; owner_lease = Some lease; _ } as attachment) ->
    if Option.is_some lease.disconnect_grace_until
    then Error (error Lease_stale "owner attachment is disconnected")
    else if Agent_protocol.Timestamp.compare lease.expires_at (t.services.now ()) <= 0
    then Error (error Lease_stale "owner lease has expired")
    else Ok attachment
;;

let has_writer_attachment t =
  List.exists t.state.attachments ~f:(fun attachment ->
    match attachment.Agent_protocol.Session.Attachment.mode with
    | Read_write -> true
    | Owner_read_write ->
      Option.value_map attachment.owner_lease ~default:true ~f:(fun lease ->
        Option.is_none lease.disconnect_grace_until
        && Agent_protocol.Timestamp.compare lease.expires_at (t.services.now ()) > 0)
    | Read_only -> false)
;;

let grant_is_unexpired t grant =
  match grant.Agent_protocol.Grant.expires_at with
  | None -> true
  | Some expires_at -> Agent_protocol.Timestamp.compare expires_at (t.services.now ()) > 0
;;

let grant_identity_matches grant ~tool_name ~identity_digest =
  match grant.Agent_protocol.Grant.scope with
  | Exact_session | Durable_exact -> String.equal grant.identity_digest identity_digest
  | Prefix_session ->
    String.equal
      grant.identity_digest
      (Permission_policy.prefix_identity_digest ~tool_name)
;;

let invocation_granted t ~tool_name ~identity_digest =
  List.exists t.state.grants ~f:(fun grant ->
    Agent_protocol.Grant.equal_state grant.state Active
    && String.equal grant.tool_name tool_name
    && grant_is_unexpired t grant
    && grant_identity_matches grant ~tool_name ~identity_digest)
;;

let with_writer t attachment_id f =
  Result.bind (write_attachment t attachment_id) ~f:(fun _ -> f ())
;;

let validate_administrative_state t attachment_id expected_revision =
  let open Result.Let_syntax in
  let%bind _ = write_attachment t attachment_id in
  if not (Int64.equal expected_revision t.state.counters.revision)
  then Error (error Conflict "session revision does not match")
  else if Option.is_some t.state.active_operation
  then Error (error Conflict "session has an active foreground operation")
  else if t.idle_moderator_borrowed
  then Error (error Conflict "session moderator is processing background work")
  else if not (List.is_empty t.job_scopes)
  then Error (error Conflict "session has an active background execution scope")
  else (
    match t.state.lifecycle.observed with
    | Agent_protocol.Session.Stopped -> Ok ()
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping
    | Failed _ ->
      Error (error Invalid_state "administrative mutation requires a stopped session"))
;;

let commit_administration t attachment_id expected_revision kind candidate =
  let open Result.Let_syntax in
  let%bind () = validate_administrative_state t attachment_id expected_revision in
  let ingress_matches =
    match kind with
    | Session_state.Compaction_archive.Reset | Rebuild ->
      t.state.identity.generation < Int.max_value
      && Int.equal
           candidate.Session_state.identity.generation
           (t.state.identity.generation + 1)
      && List.is_empty candidate.ingress_registrations
    | Upgrade | Compaction | Edit | Delete | Pending_input ->
      List.equal
        External_ingress.equal
        candidate.ingress_registrations
        t.state.ingress_registrations
  in
  let%bind () =
    if
      Agent_protocol.Id.Session.compare
        candidate.Session_state.identity.session_id
        t.state.identity.session_id
      <> 0
      || (not (Int64.equal candidate.counters.revision expected_revision))
      || (not
            (Option.equal
               Automatic_turn_budget.equal
               candidate.automatic_turn_budget
               t.state.automatic_turn_budget))
      || (not ingress_matches)
      || Int64.(
           candidate.conversation.next_history_sequence
           < t.state.conversation.next_history_sequence)
    then
      Error (error Conflict "administrative candidate does not match the captured state")
    else Session_state.validate candidate
  in
  let%bind state =
    Administration.archive
      ~archive_reference:t.persistence.archive_reference
      ~previous:t.state
      candidate
      kind
  in
  let%bind pending =
    match kind with
    | Reset | Rebuild ->
      if List.is_empty t.state.conversation.deferred_user_entries
      then Ok None
      else (
        let%bind archive =
          List.hd state.conversation.compaction_archives
          |> Result.of_option
               ~error:(error Internal_error "administrative archive missing")
        in
        let reason =
          match kind with
          | Reset -> Pending_disposition.Retirement_reason.Source_reset
          | Rebuild -> Source_replaced
          | Compaction | Upgrade | Edit | Delete | Pending_input ->
            Canonical_history_retired
        in
        prepare_pending t ~archive:(Some archive) (Retire reason)
        |> Result.map ~f:Option.some)
    | Compaction | Upgrade | Edit | Delete | Pending_input -> Ok None
  in
  let state =
    Option.value_map pending ~default:state ~f:(fun prepared ->
      let plan = Pending_transition.plan prepared in
      { state with
        conversation =
          { state.conversation with
            deferred_user_entries = Pending_plan.pending plan
          ; pending_revision = Pending_plan.revision plan
          ; pending_dispositions = Pending_plan.dispositions plan
          }
      })
  in
  let delta =
    match pending with
    | None -> Session_delta.Created state
    | Some prepared ->
      Session_delta.Batch
        [ Pending_transition.delta prepared; Session_delta.Created state ]
  in
  transition t ~delta ~payloads:(Administration.payloads ~previous:t.state state)
;;

let administration_basis (expected : Session_state.t) (state : Session_state.t) =
  { state with
    identity = { state.identity with updated_at = expected.identity.updated_at }
  ; counters = expected.counters
  ; inference_ledger = expected.inference_ledger
  }
;;

let same_administration_basis ~reconciled expected current =
  let open Result.Let_syntax in
  let document_error _ = error Invalid_state "administration basis admission failed" in
  let%bind limits =
    Persistence_codec.limits ~max_bytes:(64 * 1024 * 1024)
    |> Result.map_error ~f:document_error
  in
  let admit state =
    Session_state_document.encode (Session_state_document.authored state) ~limits
    |> Result.map_error ~f:document_error
  in
  let%bind expected_document = admit expected in
  let%map current_document =
    admit (if reconciled then administration_basis expected current else current)
  in
  String.equal
    (Jsonaf.to_string (Document_schema.Document.json expected_document))
    (Jsonaf.to_string (Document_schema.Document.json current_document))
;;

let validate_administration_basis t attachment_id expected =
  let open Result.Let_syntax in
  let%bind () =
    validate_administrative_state t attachment_id expected.Session_state.counters.revision
  in
  let%bind unchanged = same_administration_basis ~reconciled:false expected t.state in
  if unchanged
  then Ok ()
  else Error (error Conflict "administration basis changed before resource retirement")
;;

let commit_reconciled_administration t attachment_id expected kind candidate =
  let open Result.Let_syntax in
  let%bind () =
    Session_state.validate_administration_candidate candidate ~previous:expected
  in
  let%bind unchanged = same_administration_basis ~reconciled:true expected t.state in
  let%bind () =
    if unchanged
    then Ok ()
    else Error (error Conflict "administration basis changed during resource retirement")
  in
  let%bind inference_ledger =
    Inference_ledger.with_generation
      t.state.inference_ledger
      ~generation:candidate.Session_state.identity.generation
    |> Result.map_error ~f:inference_error
  in
  let candidate = { candidate with inference_ledger; counters = t.state.counters } in
  commit_administration t attachment_id t.state.counters.revision kind candidate
;;

(* Initialization may durably admit jobs and tracking before its final local
   runtime state is available. Those records are never replaced by an earlier
   constructor snapshot. History reservations alone may also advance. *)
let initialization_basis (basis : Session_state.t) (state : Session_state.t) =
  { state with
    identity = { state.identity with updated_at = basis.identity.updated_at }
  ; counters = basis.counters
  ; inference_ledger = basis.inference_ledger
  ; goal_accounts = basis.goal_accounts
  ; jobs = basis.jobs
  ; model_job_targets = basis.model_job_targets
  ; schedules = basis.schedules
  ; shell =
      { state.shell with
        manifest_grants = basis.shell.manifest_grants
      ; approval_grants = basis.shell.approval_grants
      }
  ; conversation =
      { state.conversation with
        next_history_sequence = basis.conversation.next_history_sequence
      ; reserved_history_through = basis.conversation.reserved_history_through
      }
  }
;;

let validate_initialization_projection (expected : Session_state.t) state =
  if
    Sexp.equal
      (Session_state.sexp_of_t expected)
      (Session_state.sexp_of_t (initialization_basis expected state))
    && Document_schema.Json.equal
         (Inference.Selection.to_json expected.spec.inference_target)
         (Inference.Selection.to_json state.Session_state.spec.inference_target)
  then Ok ()
  else Error (error Conflict "initialization basis changed while resources were prepared")
;;

(* The explicit constructor scope must own the very borrow already held by this
   actor. This predicate does not call the effect/currentness guard, so Init and
   callback validation cannot recurse through each other. Existing installed
   callback claims never set [constructor_scope]. *)
let initialization_callback_is_owned t (scope : Initialization_scope.t) =
  match t.initialization_scope, t.legacy_callback_scope, t.queued_event_borrow with
  | Some active, Some callback, Some borrow ->
    phys_equal active scope
    && Option.exists callback.constructor_scope ~f:(phys_equal scope)
    && phys_equal callback.borrow borrow
    && (match scope.callback with
        | Claimed id ->
          Agent_protocol.Id.Moderator_execution.equal id borrow.receipt.context.id
        | Unclaimed | Settled _ -> false)
    && t.idle_moderator_borrowed
    && borrow.callback_active
    && (not (borrow.committed || borrow.cancel_requested))
    && Atomic.get borrow.authority_live
    && Chat_response.Moderator_manager.callback_is_current
         callback.manager
         callback.callback
  | None, _, _ | _, None, _ | _, _, None -> false
;;

let validate_initialization_basis ?scope t ~purpose (expected : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () =
    match purpose, expected.runtime_initialization, t.state.runtime_initialization with
    | Initialization_scope.Purpose.Pending_initialization, Pending _, Pending _
    | Runtime_construction, Ready, Ready -> Ok ()
    | Pending_initialization, Ready, _
    | Pending_initialization, _, Ready
    | Runtime_construction, Pending _, _
    | Runtime_construction, _, Pending _ ->
      Error (error Conflict "constructor purpose differs from its actual lifecycle basis")
  in
  if
    Option.is_some t.state.active_operation
    || (t.idle_moderator_borrowed
        && not (Option.exists scope ~f:(initialization_callback_is_owned t)))
    || not (List.is_empty t.job_scopes)
  then Error (error Conflict "initialization basis changed while resources were prepared")
  else validate_initialization_projection expected t.state
;;

let validate_initialization_owner t (scope : Initialization_scope.t) =
  (* Physical identity is deliberate capability ownership, never domain equality. *)
  if phys_equal t.initialization_owner scope.owner
  then Ok ()
  else Error (error Conflict "initialization scope belongs to another actor")
;;

let validate_initialization_scope_current t scope =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_owner t scope in
  let%bind () =
    match t.initialization_scope with
    | Some active when phys_equal active scope -> Ok ()
    | Some _ | None -> Error (error Conflict "initialization scope is no longer active")
  in
  let expected =
    Initialization_scope.Acknowledged.apply scope.acknowledged scope.expected
  in
  let%bind () = validate_initialization_basis ~scope t ~purpose:scope.purpose expected in
  match scope.Initialization_scope.run_preparation with
  | None -> Ok ()
  | Some preparation ->
    (match t.run_preparation with
     | Some active when phys_equal active preparation ->
       Run_preparation.check_current
         preparation
         ~owner:t.run_preparation_owner
         ~state:t.state
     | Some _ | None ->
       Error (error Conflict "initializer Run preparation is no longer active"))
;;

let validate_initialization_scope t scope =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_scope_current t scope in
  match scope.Initialization_scope.run_preparation with
  | None -> Ok ()
  | Some preparation -> check_run_preparation t preparation
;;

let begin_initialization
      ?run_preparation
      ?original_actor
      ?(purpose = Initialization_scope.Purpose.Pending_initialization)
      t
      (expected : Session_state.t)
  =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_basis t ~purpose expected in
  let%bind () =
    match purpose with
    | Pending_initialization -> Ok ()
    | Runtime_construction ->
      if
        Int64.equal expected.counters.revision t.state.counters.revision
        && (Option.is_some original_actor || Option.is_some run_preparation)
      then Ok ()
      else
        Error
          (error
             Permission_denied
             "Ready construction requires current original custody or its exact Run \
              preparation and CAS")
  in
  let%bind () =
    match Inference.Selection.view t.state.spec.inference_target with
    | Captured _ -> Ok ()
    | Unresolved ->
      Error (error Migration_required "initialization requires a captured target")
  in
  match t.initialization_scope with
  | Some _ -> Error (error Conflict "another initialization scope is active")
  | None ->
    let scope =
      Initialization_scope.
        { owner = t.initialization_owner
        ; purpose
        ; expected
        ; acknowledged = Acknowledged.capture expected
        ; jobs = []
        ; callback = Callback.Unclaimed
        ; resource = None
        ; run_preparation
        ; agent_admission =
            create_agent_admission (Option.map original_actor ~f:create_agent_custody)
        }
    in
    t.initialization_scope <- Some scope;
    Ok scope
;;

(* Selected keys identify only this constructor's already acknowledged rows.
   They contain no loan and cannot reconstruct an absent process-local row. *)
let retire_constructor_rows t (resource : Initialization_scope.Resource.t) =
  t.legacy_queue_custody
  <- Legacy_queue_custody.install
       t.legacy_queue_custody
       ~incoming:
         (Legacy_queue_custody.without_constructor
            t.legacy_queue_custody
            ~manager:resource.manager
            ~source:resource.source
            ~ordinals:resource.ordinals);
  t.legacy_schedule_custody
  <- Legacy_schedule_custody.install
       t.legacy_schedule_custody
       ~incoming:
         (Legacy_schedule_custody.without_constructor
            t.legacy_schedule_custody
            ~manager:resource.manager
            ~source:resource.source
            ~ids:resource.schedules)
;;

let retire_initialization_resource t (scope : Initialization_scope.t) manager =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_owner t scope in
  match scope.resource with
  | None -> Ok ()
  | Some resource when phys_equal manager resource.manager ->
    retire_constructor_rows t resource;
    Ok ()
  | Some _ -> Error (error Conflict "constructor cleanup names a different Manager")
;;

let check_constructor_callback_settled (scope : Initialization_scope.t) =
  match scope.callback with
  | Unclaimed | Settled _ -> Ok ()
  | Claimed _ -> Error (error Conflict "constructor callback has not settled")
;;

let prepare_constructor_resource_handoff
      t
      (scope : Initialization_scope.t)
      (state : Session_state.t)
      manager
  =
  let open Result.Let_syntax in
  let%bind () = check_constructor_callback_settled scope in
  match scope.resource, manager with
  | None, _ -> Ok None
  | Some resource, Some manager ->
    let%bind () =
      if
        phys_equal resource.manager manager
        && Option.equal
             Agent_protocol.Run_source.equal
             (retained_moderator_source state)
             (Some resource.source)
      then Ok ()
      else Error (error Conflict "constructor handoff resource or source changed")
    in
    let%bind () =
      Legacy_queue_custody.validate_constructor_handoff
        t.legacy_queue_custody
        ~manager
        ~source:resource.source
        ~ordinals:resource.ordinals
    in
    let%map () =
      Legacy_schedule_custody.validate_constructor_handoff
        t.legacy_schedule_custody
        ~manager
        ~source:resource.source
        ~ids:resource.schedules
    in
    Some { resource with handed_off = true }
  | Some _, None ->
    Error (error Conflict "constructor handoff requires its actual Manager")
;;

let end_initialization t scope =
  let open Result.Let_syntax in
  let%map () = validate_initialization_owner t scope in
  match t.initialization_scope with
  | Some active when phys_equal active scope ->
    Option.iter active.resource ~f:(fun resource ->
      if not resource.handed_off then retire_constructor_rows t resource);
    retire_agent_admission active.agent_admission;
    t.initialization_scope <- None
  | Some _ | None -> ()
;;

let check_initialization_agent_actor scope =
  check_agent_admission scope.Initialization_scope.agent_admission
;;

let prepare_initialization_job_selection t (scope : Initialization_scope.t) =
  let open Result.Let_syntax in
  match scope.run_preparation with
  | None -> Ok (scope.jobs, None)
  | Some preparation ->
    let%map ids, selection =
      Run_preparation.Constructor_jobs.prepare_handoff
        preparation
        ~owner:t.run_preparation_owner
        ~state:t.state
    in
    ( List.dedup_and_sort (scope.jobs @ ids) ~compare:Agent_protocol.Id.Job.compare
    , Some selection )
;;

let complete_initialization t scope (candidate : Session_state.t) moderator_manager =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_scope t scope in
  (* Reject before any Manager snapshot can reacquire the active callback gate. *)
  let%bind () = check_constructor_callback_settled scope in
  let%bind () =
    match scope.Initialization_scope.purpose with
    | Pending_initialization -> Ok ()
    | Runtime_construction ->
      Error
        (error
           Conflict
           "Ready construction cannot complete or fail Pending initialization")
  in
  let%bind () = check_initialization_agent_actor scope in
  let%bind moderator =
    Moderator_checkpoint.reuse_unchanged_snapshot
      ~previous:t.state.moderator
      ~incoming:candidate.moderator
  in
  let candidate = { candidate with moderator } in
  let expected = scope.Initialization_scope.expected in
  let projection =
    { candidate with
      conversation = expected.conversation
    ; moderator = expected.moderator
    ; shell = expected.shell
    }
    |> initialization_basis expected
  in
  let%bind () =
    if
      (not
         (Sexp.equal
            (Session_state.sexp_of_t expected)
            (Session_state.sexp_of_t projection)))
      || not
           (Document_schema.Json.equal
              (Inference.Selection.to_json expected.spec.inference_target)
              (Inference.Selection.to_json candidate.spec.inference_target))
    then Error (error Conflict "initialization candidate changed non-initializer fields")
    else Ok ()
  in
  let current = t.state in
  let state =
    { current with
      runtime_initialization = Ready
    ; failure = None
    ; lifecycle =
        { current.lifecycle with
          observed =
            (match current.lifecycle.observed with
             | Failed _ -> Agent_protocol.Session.Stopped
             | ( Stopped
               | Queued_for_slot
               | Starting
               | Recovering
               | Idle
               | Running_turn _
               | Compacting _
               | Waiting_for_permission _
               | Stopping ) as observed -> observed)
        }
    ; conversation =
        { current.conversation with
          canonical_history = candidate.conversation.canonical_history
        ; initial_prompt_entry_count = candidate.conversation.initial_prompt_entry_count
        ; next_history_sequence =
            Int64.max
              current.conversation.next_history_sequence
              candidate.conversation.next_history_sequence
        ; reserved_history_through =
            Int64.max
              current.conversation.reserved_history_through
              candidate.conversation.reserved_history_through
        }
    ; moderator = candidate.moderator
    ; shell =
        { current.shell with extension_snapshots = candidate.shell.extension_snapshots }
    }
  in
  let%bind () = Session_state.validate state in
  let run_preparation = scope.Initialization_scope.run_preparation in
  let%bind ids, selection = prepare_initialization_job_selection t scope in
  let%bind prepared, job_custody, _, revision =
    prepare_constructor_job_handoff
      ?run_preparation
      t
      ~delta:(Session_delta.Created state)
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_updated
            (Session_state.summary state)
        ]
      ~ids
      ~manager:moderator_manager
  in
  Exn.protect
    ~finally:(fun () -> Option.iter job_custody ~f:Job_custody.Update.discard)
    ~f:(fun () ->
      let%bind resource =
        prepare_constructor_resource_handoff t scope prepared.state moderator_manager
      in
      let acknowledgement =
        Initialization_scope.Acknowledgement.prepare ?resource scope prepared.state
      in
      Eio.Cancel.protect (fun () ->
        let%bind () = validate_initialization_scope t scope in
        let%bind () = check_initialization_agent_actor scope in
        let%bind () =
          Option.value_map selection ~default:(Ok ()) ~f:(fun selection ->
            Run_preparation.Constructor_jobs.check_before_commit
              selection
              ~owner:t.run_preparation_owner
              ~state:t.state)
        in
        let%bind () =
          check_constructor_job_handoff
            t
            ~prepared
            ~job_custody
            ~manager:moderator_manager
            ~revision
        in
        let%bind _ =
          prepare_constructor_resource_handoff t scope prepared.state moderator_manager
        in
        let%bind () = Initialization_scope.Acknowledgement.check_basis acknowledgement in
        let%bind () = persist_transition t prepared in
        install_committed_transition
          ?run_preparation
          ?job_custody
          ~initialization_acknowledgement:acknowledgement
          t
          prepared;
        Option.iter selection ~f:Run_preparation.Constructor_jobs.acknowledge;
        publish_committed_transition t prepared;
        Ok (Session_state.summary t.state)))
;;

let checkpoint_runtime_construction t scope moderator moderator_manager =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_scope t scope in
  (* Reject before any Manager snapshot can reacquire the active callback gate. *)
  let%bind () = check_constructor_callback_settled scope in
  let%bind () =
    match scope.Initialization_scope.purpose with
    | Runtime_construction -> Ok ()
    | Pending_initialization ->
      Error (error Conflict "Pending construction requires initialization completion")
  in
  let%bind () = check_initialization_agent_actor scope in
  let%bind moderator =
    Moderator_checkpoint.reuse_unchanged_snapshot
      ~previous:t.state.moderator
      ~incoming:moderator
  in
  let%bind ids, selection = prepare_initialization_job_selection t scope in
  let run_preparation = scope.Initialization_scope.run_preparation in
  let%bind prepared, job_custody, source_changed, revision =
    prepare_constructor_job_handoff
      ?run_preparation
      t
      ~delta:(Session_delta.Moderator_changed moderator)
      ~payloads:[]
      ~ids
      ~manager:moderator_manager
  in
  Exn.protect
    ~finally:(fun () -> Option.iter job_custody ~f:Job_custody.Update.discard)
    ~f:(fun () ->
      let%bind resource =
        prepare_constructor_resource_handoff t scope prepared.state moderator_manager
      in
      let needs_handoff =
        Option.exists scope.resource ~f:(fun resource -> not resource.handed_off)
      in
      if
        (not needs_handoff)
        && Option.equal Jsonaf.exactly_equal t.state.moderator moderator
        && (not source_changed)
        && Option.is_none job_custody
      then Ok (Session_state.summary t.state)
      else (
        let acknowledgement =
          Initialization_scope.Acknowledgement.prepare ?resource scope prepared.state
        in
        let%bind () =
          validate_initialization_projection
            (Initialization_scope.Acknowledgement.expected acknowledgement)
            prepared.state
        in
        Eio.Cancel.protect (fun () ->
          let%bind () = validate_initialization_scope t scope in
          let%bind () = check_initialization_agent_actor scope in
          let%bind () =
            Option.value_map selection ~default:(Ok ()) ~f:(fun selection ->
              Run_preparation.Constructor_jobs.check_before_commit
                selection
                ~owner:t.run_preparation_owner
                ~state:t.state)
          in
          let%bind () =
            check_constructor_job_handoff
              t
              ~prepared
              ~job_custody
              ~manager:moderator_manager
              ~revision
          in
          let%bind () =
            Initialization_scope.Acknowledgement.check_basis acknowledgement
          in
          let%bind _ =
            prepare_constructor_resource_handoff t scope prepared.state moderator_manager
          in
          let%bind () = persist_transition t prepared in
          install_committed_transition
            ?run_preparation
            ?job_custody
            ~initialization_acknowledgement:acknowledgement
            t
            prepared;
          Option.iter selection ~f:Run_preparation.Constructor_jobs.acknowledge;
          publish_committed_transition t prepared;
          Ok (Session_state.summary t.state))))
;;

let checkpoint_run_moderator t preparation moderator moderator_manager =
  let open Result.Let_syntax in
  let%bind () = check_run_preparation t preparation in
  let%bind moderator =
    Moderator_checkpoint.reuse_unchanged_snapshot
      ~previous:t.state.moderator
      ~incoming:moderator
  in
  let%bind ids, selection =
    Run_preparation.Constructor_jobs.prepare_handoff
      preparation
      ~owner:t.run_preparation_owner
      ~state:t.state
  in
  let%bind prepared, job_custody, source_changed, revision =
    prepare_constructor_job_handoff
      ~run_preparation:preparation
      t
      ~delta:(Session_delta.Moderator_changed moderator)
      ~payloads:[]
      ~ids
      ~manager:moderator_manager
  in
  if
    Option.equal Jsonaf.exactly_equal t.state.moderator moderator
    && (not source_changed)
    && Option.is_none job_custody
  then Ok (Session_state.summary t.state)
  else
    Exn.protect
      ~finally:(fun () -> Option.iter job_custody ~f:Job_custody.Update.discard)
      ~f:(fun () ->
        Eio.Cancel.protect (fun () ->
          let%bind () = check_run_preparation t preparation in
          let%bind () =
            Run_preparation.Constructor_jobs.check_before_commit
              selection
              ~owner:t.run_preparation_owner
              ~state:t.state
          in
          let%bind () =
            check_constructor_job_handoff
              t
              ~prepared
              ~job_custody
              ~manager:moderator_manager
              ~revision
          in
          let%bind () = persist_transition t prepared in
          install_committed_transition
            ~run_preparation:preparation
            ?job_custody
            t
            prepared;
          Run_preparation.Constructor_jobs.acknowledge selection;
          publish_committed_transition t prepared;
          Ok (Session_state.summary t.state)))
;;

let fail_initialization t scope failure =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_scope t scope in
  let%bind () =
    match scope.Initialization_scope.purpose with
    | Pending_initialization -> Ok ()
    | Runtime_construction ->
      Error
        (error
           Conflict
           "Ready construction cannot complete or fail Pending initialization")
  in
  let lifecycle =
    { t.state.lifecycle with Session_state.Lifecycle.observed = Failed failure }
  in
  transition
    ?run_preparation:scope.Initialization_scope.run_preparation
    t
    ~delta:
      (Session_delta.Batch [ Failure_changed (Some failure); Lifecycle_changed lifecycle ])
    ~payloads:
      [ Agent_protocol.Event.Durable.Payload.Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ]
;;

let reset_internal t attachment_id expected_revision options =
  let open Result.Let_syntax in
  let%bind () = validate_administrative_state t attachment_id expected_revision in
  let%bind state = Administration.reset t.state options in
  commit_administration t attachment_id expected_revision Reset state
;;

let upgrade_prompt_internal t attachment_id expected_revision target_revision =
  let open Result.Let_syntax in
  let%bind () = validate_administrative_state t attachment_id expected_revision in
  let previous_revision = t.state.spec.prompt_revision_id in
  if Agent_protocol.Id.Prompt_revision.compare previous_revision target_revision = 0
  then Ok (Session_state.summary t.state)
  else (
    let state =
      { t.state with
        spec = { t.state.spec with prompt_revision_id = target_revision }
      ; moderator = None
      ; shell = Session.Shell_state.empty
      ; failure = None
      }
    in
    transition
      t
      ~delta:(Session_delta.Created state)
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Prompt_upgraded
            { prompt_id = Option.value_exn state.spec.prompt_definition_id
            ; previous_revision
            ; current_revision = target_revision
            }
        ; Session_updated (Session_state.summary state)
        ])
;;

let adoption_boundary t =
  match t.state.active_operation with
  | Some { Agent_protocol.Operation.kind = Turn _; id; _ } ->
    Pending_eligibility.Boundary.Worker id
  | Some { kind = Compaction; _ } | None -> Idle_start
;;

let adopt_deferred t =
  let open Result.Let_syntax in
  let%bind () = require_runtime_admission t in
  let%bind prepared =
    prepare_pending
      t
      (Adopt
         { boundary = adoption_boundary t
         ; runtime_admission_open = runtime_admission_open t
         })
  in
  let entries = Pending_plan.adopted_entries (Pending_transition.plan prepared) in
  if List.is_empty entries
  then Ok (Session_state.summary t.state)
  else
    transition
      t
      ~delta:(Pending_transition.delta prepared)
      ~payloads:[ Agent_protocol.Event.Durable.Payload.History_appended entries ]
;;

let reserve_history_block ?run_preparation t count =
  if count <= 0
  then Error (error Invalid_request "history reservation count must be positive")
  else if Int64.(t.state.conversation.next_history_sequence > max_value - of_int count)
  then Error (error Invalid_state "history sequence overflow")
  else (
    let first_sequence = t.state.conversation.next_history_sequence in
    let reserved_through = Int64.(first_sequence + of_int count) in
    let open Result.Let_syntax in
    let%map _ =
      transition
        ?run_preparation
        t
        ~delta:(Session_delta.History_block_reserved reserved_through)
        ~payloads:[]
    in
    History_id_source.{ first_sequence; reserved_through })
;;

let current_operation t operation_id =
  match t.state.active_operation with
  | Some operation when Agent_protocol.Id.Operation.compare operation.id operation_id = 0
    -> Ok operation
  | Some _ -> Error (error Conflict "a different foreground operation is active")
  | None -> Error (error Operation_not_found "foreground operation is not active")
;;

let running_operation
      ?(allow_stopping = false)
      ?(allow_waiting_permission = false)
      t
      operation_id
  =
  let open Result.Let_syntax in
  let%bind operation = current_operation t operation_id in
  match operation.state, t.state.lifecycle.desired, t.state.lifecycle.observed with
  | Agent_protocol.Operation.Running, desired, Running_turn id
    when (allow_stopping || Agent_protocol.Session.equal_desired_state desired Running)
         && Agent_protocol.Id.Operation.compare id operation_id = 0
         && not t.state.halted -> Ok operation
  | Running, desired, Waiting_for_permission _
    when allow_waiting_permission
         && (allow_stopping || Agent_protocol.Session.equal_desired_state desired Running)
         && not t.state.halted -> Ok operation
  | _ ->
    Error (error Invalid_state "foreground operation is not running at a tool safe point")
;;

(* Compaction is an inference-only owner, not a tool safe point. This private
   borrow belongs to the actual captured compaction worker; ordinary/native
   invocation admission deliberately continues to require [running_operation]. *)
let running_compaction t (expected : Agent_protocol.Operation.t) =
  let open Result.Let_syntax in
  let%bind () = require_runtime_admission t in
  let%bind current = current_operation t expected.id in
  match current.kind, current.state, t.state.lifecycle.observed with
  | Compaction, Running, Compacting id
    when Agent_protocol.Operation.equal_kind expected.kind Compaction
         && Int.equal current.generation expected.generation
         && Agent_protocol.Id.Operation.equal id expected.id
         && not t.state.halted -> Ok current
  | _ -> Error (error Conflict "compaction execution owner is no longer current")
;;

let invocation_origin t (invocation : Agent_protocol.Invocation.t) ~fallback =
  match invocation.context.parent_invocation with
  | None -> fallback ()
  | Some id ->
    (match
       List.find t.state.invocations ~f:(fun parent ->
         Agent_protocol.Id.Invocation.equal parent.context.id id)
     with
     | Some parent -> Ok parent.inference_origin
     | None -> Error (error Conflict "original parent invocation is unavailable"))
;;

let operation_invocation_origin t operation_id invocation =
  invocation_origin t invocation ~fallback:(fun () ->
    let open Result.Let_syntax in
    let%bind operation = current_operation t operation_id in
    Invocation_inference_provenance.of_operation ~runs:t.state.run_state operation
    |> Result.map ~f:Option.some)
;;

let event_inference_origin t (receipt : Agent_protocol.Moderator_execution.t) =
  match receipt.context.job with
  | Some _ ->
    Invocation_inference_provenance.of_event
      ~runs:t.state.run_state
      ~jobs:t.state.jobs
      receipt
  | None ->
    let open Result.Let_syntax in
    let%bind selected = Run_event_scope.select t.state ~executing:receipt in
    (match selected with
     | Some run -> Ok (Some (Agent_protocol.Inference_origin.Run run.id))
     | None ->
       Invocation_inference_provenance.of_event
         ~runs:t.state.run_state
         ~jobs:t.state.jobs
         receipt)
;;

let event_invocation_origin t receipt invocation =
  invocation_origin t invocation ~fallback:(fun () -> event_inference_origin t receipt)
;;

let invocation_admission_deltas t (invocation : Agent_protocol.Invocation.t) ~origin =
  let open Result.Let_syntax in
  let%bind () =
    if Option.is_none invocation.inference_origin
    then Ok ()
    else Error (error Invalid_request "invocation inference origin is actor captured")
  in
  let%bind () = Agent_protocol.Invocation.validate invocation in
  let%bind captured =
    match origin with
    | None -> Ok invocation
    | Some origin -> Agent_protocol.Invocation.capture_inference_origin invocation origin
  in
  match
    List.find t.state.invocations ~f:(fun current ->
      Agent_protocol.Id.Invocation.equal current.context.id invocation.context.id)
  with
  | None -> Ok (captured, [ Session_delta.Invocation_changed captured ])
  | Some current
    when Agent_protocol.Invocation.equal_origin invocation.context.origin Model
         && Option.is_some invocation.context.call_entry_id
         && Agent_protocol.Invocation.equal current captured -> Ok (current, [])
  | Some _ -> Error (error Conflict "invocation identity is already admitted")
;;

let commit_invocation_call t operation_id (invocation : Agent_protocol.Invocation.t) entry
  =
  let open Result.Let_syntax in
  let%bind operation = current_operation t operation_id in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () = Agent_protocol.Invocation.validate invocation in
  let%bind () =
    match
      ( invocation.status
      , invocation.context.origin
      , invocation.context.parent_job
      , invocation.context.parent_invocation
      , invocation.context.call_entry_id )
    with
    | Admitted, Model, None, None, Some id
      when History_entry.Id.equal id (History_entry.id entry) -> Ok ()
    | _ ->
      Error
        (error Invalid_request "call intent requires an admitted root model invocation")
  in
  let%bind () =
    if Option.is_none invocation.inference_origin
    then Ok ()
    else Error (error Invalid_request "call intent inference origin is actor captured")
  in
  let%bind origin =
    Invocation_inference_provenance.of_operation ~runs:t.state.run_state operation
  in
  let%bind invocation =
    Agent_protocol.Invocation.capture_inference_origin invocation origin
  in
  let encoded =
    History_codec.canonical_encoder ~previous:t.state.conversation.canonical_history entry
  in
  let%bind entries =
    match
      List.find t.state.conversation.canonical_history ~f:(fun item ->
        History_entry.Id.equal item.id encoded.id)
    with
    | None -> Ok [ encoded ]
    | Some existing when Agent_protocol.History.equal_entry existing encoded -> Ok []
    | Some _ -> Error (error Conflict "history call already has different content")
  in
  let%bind admission =
    match
      List.find t.state.invocations ~f:(fun current ->
        Agent_protocol.Id.Invocation.compare current.context.id invocation.context.id = 0)
    with
    | None -> Ok [ Session_delta.Invocation_changed invocation ]
    | Some current
      when List.is_empty entries
           && Option.equal
                Agent_protocol.Inference_origin.equal
                current.inference_origin
                invocation.inference_origin
           && Agent_protocol.Invocation.equal_context current.context invocation.context
           && Option.equal
                Agent_protocol.Invocation.equal_routing
                current.routing
                invocation.routing -> Ok []
    | Some _ -> Error (error Conflict "call intent identity has different content")
  in
  if List.is_empty entries && List.is_empty admission
  then Ok ()
  else
    transition
      t
      ~delta:
        (Session_delta.Batch
           ((if List.is_empty entries
             then []
             else [ Session_delta.Canonical_entries_appended entries ])
            @ admission))
      ~payloads:
        (if List.is_empty entries
         then []
         else [ Agent_protocol.Event.Durable.Payload.History_appended entries ])
    |> Result.map ~f:(fun _ -> ())
;;

let custody_of_admission = function
  | None -> Ok None
  | Some admission ->
    let open Result.Let_syntax in
    let%map () = check_agent_admission admission in
    Execution_custody.Admission.custody admission
;;

let job_agent_admission t scope =
  let open Result.Let_syntax in
  let%bind job = job_scope_current t scope in
  let%bind row = Job_custody.find t.job_agent_custody ~job in
  match row with
  | None -> Ok None
  | Some row -> Result.map (Job_custody.Row.admission row) ~f:Option.some
;;

let job_callback_admission t scope ~manager ~source =
  let open Result.Let_syntax in
  let%bind job = job_scope_current t scope in
  let%bind row = Job_custody.find t.job_agent_custody ~job in
  match row with
  | None -> Ok None
  | Some row ->
    let%bind () =
      match Job_custody.Row.binding row with
      | Captured { source = captured; manager = Some resource }
        when Agent_protocol.Run_source.equal captured source
             && phys_equal resource manager -> Ok ()
      | No_captured_moderator | Captured _ ->
        Error
          (error Conflict "Job callback has no loan for this exact Manager and source")
    in
    Result.map (Job_custody.Row.admission row) ~f:Option.some
;;

let operation_agent_admission t id =
  match t.operation_agent_custody with
  | None -> Ok None
  | Some (owner, admission) ->
    if Agent_protocol.Id.Operation.equal owner id
    then Ok (Some admission)
    else Error (error Conflict "operation admission belongs to another owner")
;;

let parent_of_admission = function
  | None -> Observation_continuation.Parent.Unavailable
  | Some admission -> Observation_continuation.Parent.Available admission
;;

let moderator_settlement_parent t (borrow : moderator_borrow) =
  match borrow.kind with
  | Observation ->
    Ok
      (Option.value_map
         borrow.observation
         ~default:Observation_continuation.Parent.Unavailable
         ~f:Observation_continuation.parent)
  | Invocation ->
    let admission =
      match borrow.job_scope, borrow.operation_id with
      | Some scope, _ -> job_agent_admission t scope
      | None, Some id -> operation_agent_admission t id
      | None, None -> Ok None
    in
    Result.map admission ~f:parent_of_admission
;;

let moderator_agent_admission t (borrow : moderator_borrow) =
  match borrow.kind with
  | Observation ->
    (match borrow.observation with
     | Some observation ->
       Observation_continuation.admission observation |> Result.map ~f:Option.some
     | None -> Ok None)
  | Invocation ->
    (match borrow.job_scope, borrow.operation_id with
     | Some scope, _ -> job_agent_admission t scope
     | None, Some id -> operation_agent_admission t id
     | None, None -> Ok None)
;;

let moderator_agent_custody t borrow =
  Result.bind (moderator_agent_admission t borrow) ~f:custody_of_admission
;;

let event_agent_admission t (borrow : queued_event_borrow) =
  match borrow.agent_admission with
  | Resumed_run admission -> Ok admission
  | Contextual admission ->
    (match t.legacy_callback_scope with
     | Some scope when phys_equal scope.borrow borrow ->
       (match scope.custody with
        | Unavailable -> Ok None
        | Admitted admission -> Ok (Some admission))
     | None | Some _ ->
       (match borrow.job_scope, borrow.receipt.context.operation_id with
        | Some scope, _ -> job_agent_admission t scope
        | None, Some id -> operation_agent_admission t id
        | None, None -> Ok admission))
;;

let execution_agent_admission t (execution : invocation_execution) =
  match execution.owner with
  | Foreground id -> operation_agent_admission t id
  | Invocation_moderator borrow -> moderator_agent_admission t borrow
  | Event_moderator borrow -> event_agent_admission t borrow
  | Background_job scope -> job_agent_admission t scope
;;

let execution_settlement_parent t (execution : invocation_execution) =
  match execution.owner with
  | Invocation_moderator borrow -> moderator_settlement_parent t borrow
  | Foreground _ | Event_moderator _ | Background_job _ ->
    Result.map (execution_agent_admission t execution) ~f:parent_of_admission
;;

let settlement_custody = function
  | Observation_continuation.Parent.Available admission ->
    Execution_custody.Admission.custody admission
  | Expired | Unavailable -> None
;;

let execution_agent_custody t execution =
  Result.bind (execution_agent_admission t execution) ~f:custody_of_admission
;;

let job_moderator_binding t =
  let open Result.Let_syntax in
  let%bind observer = Moderator_checkpoint.observer t.state.moderator in
  match observer with
  | None -> Ok Job_custody.Moderator_binding.No_captured_moderator
  | Some _ ->
    let%map _, source = Moderator_source_installation.capture t.state in
    Job_custody.Moderator_binding.Captured { source; manager = t.moderator_manager }
;;

let check_job_moderator_binding t = function
  | Job_custody.Moderator_binding.No_captured_moderator ->
    let open Result.Let_syntax in
    let%bind observer = Moderator_checkpoint.observer t.state.moderator in
    (match observer with
     | None -> Ok ()
     | Some _ -> Error (error Conflict "Job admission acquired a different source"))
  | Captured { source; manager } ->
    let open Result.Let_syntax in
    let%bind _, current = Moderator_source_installation.capture t.state in
    if
      Agent_protocol.Run_source.equal source current
      && Option.for_all manager ~f:(fun manager ->
        Option.exists t.moderator_manager ~f:(phys_equal manager))
    then Ok ()
    else Error (error Conflict "Job admission source or resource changed")
;;

let prepare_selected_job_custody t owner =
  let open Result.Let_syntax in
  let selected =
    Staged_jobs.selected_payloads t.staged_jobs ~owner
    |> List.filter_map ~f:(fun ((job : Agent_protocol.Job.t), fresh) ->
      match job.status, fresh with
      | Queued, Some fresh -> Some (job, fresh)
      | Queued, None -> None
      | ( ( Running
          | Waiting_permission _
          | Waiting_completion _
          | Succeeded
          | Failed _
          | Cancelled
          | Interrupted _ )
        , _ ) -> None)
  in
  let%map update = Job_custody.prepare_selected t.job_agent_custody ~selected in
  update, selected
;;

let check_selected_job_custody t update selected =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result selected ~init:() ~f:(fun () (_, fresh) ->
      let binding = Job_custody.Fresh.binding fresh in
      let%bind () = check_job_moderator_binding t binding in
      match binding with
      | No_captured_moderator -> Ok ()
      | Captured { source = _; manager } ->
        if Option.equal phys_equal manager t.moderator_manager
        then Ok ()
        else Error (error Conflict "staged Job admission resource topology changed"))
  in
  Job_custody.Update.check_before_commit update ~current:t.job_agent_custody
;;

let with_selected_job_custody t owner f =
  let open Result.Let_syntax in
  let%bind update, selected = prepare_selected_job_custody t owner in
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () -> if not !acknowledged then Job_custody.Update.discard update)
    ~f:(fun () ->
      f
        ~job_custody:update
        ~check_jobs:(fun () -> check_selected_job_custody t update selected)
        ~acknowledge_jobs:(fun () ->
          acknowledged := true;
          Staged_jobs.commit t.staged_jobs ~owner))
;;

let claim_invocation t operation_id (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () =
    if
      Option.is_some invocation.context.parent_job
      || Option.is_some invocation.parent_event
    then
      Error
        (error
           Invalid_state
           "foreground invocation cannot borrow background-job ownership")
    else Ok ()
  in
  let%bind owner =
    match invocation.context.parent_invocation with
    | None -> Ok (Foreground operation_id)
    | Some parent ->
      let owned id op =
        Agent_protocol.Id.Invocation.compare id parent = 0
        && Agent_protocol.Id.Operation.compare op operation_id = 0
      in
      (match
         List.find t.invocation_executions ~f:(fun execution ->
           execution.accepts_children
           &&
           match execution.owner with
           | Foreground op -> owned execution.dispatched.context.id op
           | Invocation_moderator borrow ->
             Option.exists borrow.operation_id ~f:(owned execution.dispatched.context.id)
             && (not (borrow.committed || borrow.cancel_requested))
             && Option.exists t.moderator_borrow ~f:(phys_equal borrow)
           | Event_moderator _ | Background_job _ -> false)
       with
       | Some execution -> Ok execution.owner
       | None ->
         (match
            Option.find t.moderator_borrow ~f:(fun borrow ->
              borrow.accepts_children
              && (not borrow.committed)
              && Option.exists borrow.operation_id ~f:(owned borrow.invocation.context.id))
          with
          | Some borrow -> Ok (Invocation_moderator borrow)
          | None ->
            Error (error Conflict "parent invocation is not executing in this operation")))
  in
  let%bind origin = operation_invocation_origin t operation_id invocation in
  let%bind invocation, admission = invocation_admission_deltas t invocation ~origin in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let execution =
    { owner; dispatched; accepts_children = true; authority_live = Atomic.make true }
  in
  t.invocation_executions <- execution :: t.invocation_executions;
  Ok execution
;;

(* At most one combined process-local slot per retained Invocation or
   Moderator_execution key. The three private producers update that actual
   durable owner; only its ACK installs/replaces this slot, after committed
   retention and before publication. The complete durable owner inventories
   bound this list. An independent count quota here could reject a truthful
   native result after its effect has already run. No codec or principal lookup
   exists. Missing or mixed original proofs mint no authentication. *)
let with_intent_agent_custody ?observation t key ~pending custody f =
  if (not pending) && Option.is_none observation
  then f ~acknowledge:(fun () -> ())
  else
    let open Result.Let_syntax in
    let existing =
      List.find t.intent_agent_custody ~f:(fun entry ->
        equal_agent_intent_key entry.key key)
    in
    let%bind source = Runtime_builder.moderator_snapshot_observer t.state.moderator in
    let custody =
      if not pending
      then None
      else (
        match existing, custody with
        | None, custody -> custody
        | Some { custody = Some previous; _ }, Some current
          when Execution_custody.same_origin previous current -> Some previous
        | Some _, (Some _ | None) -> None)
    in
    let staged, owned =
      match custody with
      | None -> None, false
      | Some custody ->
        (match transfer_agent_custody custody with
         | Ok child -> Some child, true
         | Error _ -> Some custody, false)
    in
    let entry : agent_intent_custody =
      { key
      ; generation = t.state.identity.generation
      ; source
      ; custody = staged
      ; observation =
          Option.first_some
            observation
            (Option.bind existing ~f:(fun entry -> entry.observation))
      }
    in
    let acknowledged = ref false in
    (* This callback is actor-private. Every caller invokes it immediately after
       the actual committed-state install, before publication can yield. Keep
       other entries from that install's retention pass, not a stale old list. *)
    let acknowledge () =
      if not !acknowledged
      then (
        Option.iter existing ~f:(fun previous ->
          Option.iter previous.custody ~f:(fun previous ->
            if not (Option.exists staged ~f:(phys_equal previous))
            then retire_agent_custody previous));
        Option.iter existing ~f:(fun previous ->
          Option.iter previous.observation ~f:(fun previous ->
            if not (Option.exists entry.observation ~f:(phys_equal previous))
            then Observation_continuation.retire previous));
        t.intent_agent_custody
        <- entry
           :: List.filter t.intent_agent_custody ~f:(fun current ->
             not (equal_agent_intent_key current.key key));
        acknowledged := true)
    in
    Exn.protect
      ~f:(fun () -> f ~acknowledge)
      ~finally:(fun () ->
        if owned && not !acknowledged then Option.iter staged ~f:retire_agent_custody)
;;

(* The selected Job candidate is prepared against the current inventory, not
   the inventory present when each independent staged loan was captured. *)
let with_selected_intent_agent_custody ?observation t owner key ~pending custody f =
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () ->
      if not !acknowledged then Option.iter observation ~f:Observation_continuation.retire)
    ~f:(fun () ->
      with_selected_job_custody t owner (fun ~job_custody ~check_jobs ~acknowledge_jobs ->
        with_intent_agent_custody ?observation t key ~pending custody (fun ~acknowledge ->
          f ~job_custody ~check_jobs ~acknowledge:(fun () ->
            acknowledge ();
            acknowledged := true;
            acknowledge_jobs ()))))
;;

let plan_agent_custody t (plan : Observation_follow_up.t) =
  let open Result.Let_syntax in
  let accepted = function
    | Agent_protocol.Invocation.Applied_follow_up _ | Compaction_accepted_follow_up _ ->
      true
    | Pending_follow_up _ | Discarded_follow_up _ -> false
  in
  let observation_contributes (invocation : Agent_protocol.Invocation.t) =
    match invocation.observation with
    | Some { status = Observed; follow_up = Some status; _ } -> accepted status
    | None
    | Some { status = Awaiting | Observing | Observation_failed _; _ }
    | Some { status = Observed; follow_up = None; _ } -> false
  in
  let handler_contributes (invocation : Agent_protocol.Invocation.t) =
    Option.exists invocation.handler_intent ~f:(fun intent -> accepted intent.follow_up)
  in
  let keys =
    List.filter_map plan.events ~f:(fun receipt ->
      match receipt.Agent_protocol.Moderator_execution.intent with
      | Some (Applied | Waiting_compaction _) -> Some (Event_intent receipt.context.id)
      | None | Some (Pending | Discarded _) -> None)
    @ List.filter_map plan.invocations ~f:(fun invocation ->
      Option.some_if
        (handler_contributes invocation || observation_contributes invocation)
        (Invocation_intent invocation.context.id))
  in
  let find key =
    List.find t.intent_agent_custody ~f:(fun entry ->
      equal_agent_intent_key entry.key key)
  in
  let entries = List.map keys ~f:find in
  let ordinary =
    match entries with
    | [] -> None
    | Some { custody = Some first; _ } :: rest
      when List.for_all rest ~f:(function
             | Some { custody = Some other; _ } ->
               Execution_custody.same_origin first other
             | None | Some { custody = None; _ } -> false) -> Some first
    | None :: _ | Some _ :: _ -> None
  in
  let observed =
    match plan.action with
    | Checkpoint | Stop _ -> []
    | Turn | Compact -> List.filter plan.invocations ~f:observation_contributes
  in
  match observed with
  | [] -> Ok Follow_up_authority.{ custody = ordinary; observations = None }
  | _ :: _ ->
    let%bind observations =
      List.map observed ~f:(fun invocation ->
        match
          Option.bind (find (Invocation_intent invocation.context.id)) ~f:(fun entry ->
            entry.observation)
        with
        | Some observation -> Ok (invocation, observation)
        | None ->
          Error (error Permission_denied "observation follow-up admission is unavailable"))
      |> Result.all
    in
    let other_custodies =
      List.filter_map keys ~f:(fun key ->
        let observed =
          List.find observations ~f:(fun (invocation, _) ->
            equal_agent_intent_key key (Invocation_intent invocation.context.id))
        in
        match observed with
        | None -> Some (Option.bind (find key) ~f:(fun entry -> entry.custody))
        | Some (invocation, observation) ->
          if not (handler_contributes invocation)
          then None
          else (
            let custody = Option.bind (find key) ~f:(fun entry -> entry.custody) in
            (* The SAME immutable native producer's positive legacy-None loan
               also covers its own handler intent. Other absent entries do not. *)
            match custody, Observation_continuation.parent observation with
            | None, Available admission
              when Option.is_none (Execution_custody.Admission.custody admission) -> None
            | (None | Some _), (Available _ | Expired | Unavailable) -> Some custody))
    in
    let%map selection =
      Observation_continuation.Selection.create
        ~observations:(List.map observations ~f:snd)
        ~other_custodies
    in
    Follow_up_authority.
      { custody = Observation_continuation.Selection.custody selection
      ; observations = Some selection
      }
;;

let check_follow_up_authority t (authority : Follow_up_authority.t) =
  let open Result.Let_syntax in
  match authority.observations with
  | None -> Ok ()
  | Some selection ->
    let%bind () = Observation_continuation.Selection.check_current selection in
    (* All original guards precede the joint callback-free resource/source basis. *)
    let%bind () =
      match
        t.state.lifecycle.desired, t.state.lifecycle.observed, t.state.active_operation
      with
      | Running, Idle, None
        when runtime_admission_open t
             && not
                  (t.state.halted || t.idle_moderator_borrowed || moderator_is_borrowed t)
        -> Ok ()
      | (Running | Stopped), _, _ ->
        Error (error Conflict "observation follow-up no longer owns an idle actor")
    in
    let%bind source = observation_source t in
    let%bind observer = Runtime_builder.moderator_snapshot_observer t.state.moderator in
    List.fold_result
      (Observation_continuation.Selection.bindings selection)
      ~init:()
      ~f:(fun () (id, observation) ->
        match
          ( List.find t.intent_agent_custody ~f:(fun entry ->
              equal_agent_intent_key entry.key (Invocation_intent id))
          , List.find t.state.invocations ~f:(fun invocation ->
              Agent_protocol.Id.Invocation.equal invocation.context.id id) )
        with
        | Some entry, Some invocation
          when (not t.stopped)
               && Int.equal entry.generation t.state.identity.generation
               && Option.equal
                    Agent_protocol.Invocation.equal_observer
                    entry.source
                    observer
               && Option.exists entry.observation ~f:(phys_equal observation)
               && observation_pending invocation
               && Observation_continuation.matches observation ~source invocation -> Ok ()
        | (None | Some _), (None | Some _) ->
          Error (error Conflict "observation follow-up source or retained owner changed"))
;;

let constructor_scope_of_borrow t borrow =
  Option.bind t.legacy_callback_scope ~f:(fun scope ->
    if phys_equal scope.borrow borrow then scope.constructor_scope else None)
;;

(* Completed native results own data settlement, not new effect admission.
   Keep the immutable constructor control basis strict, then advance only the
   exact selected Invocation row at its actual ACK. This check has no caller
   policy callback and can also run after a yielding fresh-admission refresh. *)
let check_constructor_invocation_basis t (scope : Initialization_scope.t) =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_owner t scope in
  let%bind () =
    if initialization_callback_is_owned t scope
    then Ok ()
    else Error (error Conflict "constructor no longer owns its Invocation result")
  in
  let%bind () =
    validate_initialization_projection
      (Initialization_scope.Acknowledged.apply scope.acknowledged scope.expected)
      t.state
  in
  match scope.run_preparation with
  | None -> Ok ()
  | Some preparation ->
    Run_preparation.check_basis preparation ~owner:t.run_preparation_owner ~state:t.state
;;

let prepare_constructor_invocation_ack
      ?(permissions = [])
      ?permission_lifecycle
      t
      borrow
      ~before
      ~after
      (prepared : Session_transition.t)
  =
  let open Result.Let_syntax in
  match constructor_scope_of_borrow t borrow with
  | None -> Ok None
  | Some scope ->
    let%bind () = check_constructor_invocation_basis t scope in
    let%bind () =
      Constructor_invocation_checkpoint.validate
        ~previous:t.state
        ~incoming:prepared.state
        ~before
        ~after
    in
    let%bind permission_checkpoint =
      if List.is_empty permissions
      then Ok None
      else
        Constructor_permission_checkpoint.prepare
          ~previous:t.state
          ~incoming:prepared.state
          ~invocation_id:after.context.id
          ~permissions
          ~grant:None
          ~lifecycle:(Option.value permission_lifecycle ~default:t.state.lifecycle)
        |> Result.map ~f:Option.some
    in
    let acknowledgement =
      Initialization_scope.Acknowledgement.prepare
        ?permission_checkpoint
        scope
        prepared.state
    in
    let%map () =
      validate_initialization_projection
        (Initialization_scope.Acknowledgement.expected acknowledgement)
        prepared.state
    in
    Some acknowledgement
;;

let check_constructor_invocation_ack t borrow acknowledgement =
  let open Result.Let_syntax in
  match constructor_scope_of_borrow t borrow, acknowledgement with
  | None, None -> Ok ()
  | Some scope, Some acknowledgement
    when phys_equal scope acknowledgement.Initialization_scope.Acknowledgement.scope ->
    let%bind () = check_constructor_invocation_basis t scope in
    Initialization_scope.Acknowledgement.check_basis acknowledgement
  | None, Some _ | Some _, (Some _ | None) ->
    Error (error Conflict "constructor Invocation owner changed before commit")
;;

let claim_moderator_invocation t operation_id (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind () =
    match invocation.parent_event with
    | None -> Ok ()
    | Some _ -> Error (error Conflict "event-owned tools require their event scope")
  in
  let%bind _ = running_operation t operation_id in
  if t.idle_moderator_borrowed || Option.is_some t.moderator_borrow
  then Error (error Conflict "moderator is already borrowed")
  else (
    let%bind () =
      Extension_invariants.owner
        ~session_id:t.state.identity.session_id
        ~generation:t.state.identity.generation
        invocation.context.session_id
        invocation.context.generation
    in
    let%bind () =
      match invocation.context.parent_invocation with
      | None when Option.is_none invocation.context.parent_job -> Ok ()
      | Some parent ->
        let parent =
          List.find t.invocation_executions ~f:(fun execution ->
            execution.accepts_children
            && Agent_protocol.Id.Invocation.equal execution.dispatched.context.id parent
            &&
            match execution.owner with
            | Foreground id -> Agent_protocol.Id.Operation.equal id operation_id
            | Invocation_moderator _ | Event_moderator _ | Background_job _ -> false)
        in
        (match invocation.context.origin, parent with
         | (Script | Delegated_agent), Some parent
           when Option.is_none invocation.context.parent_job
                && Option.is_none invocation.context.provider_call_id
                && Option.is_none invocation.context.call_entry_id ->
           (match parent.dispatched.context.deadline, invocation.context.deadline with
            | None, _ -> Ok ()
            | Some parent, Some child
              when Agent_protocol.Timestamp.compare child parent <= 0 -> Ok ()
            | _ ->
              Error (error Conflict "nested moderator cannot extend its parent deadline"))
         | _ -> Error (error Conflict "nested moderator requires an active scoped caller"))
      | None ->
        Error (error Conflict "foreground moderator cannot borrow background ownership")
    in
    let%bind origin = operation_invocation_origin t operation_id invocation in
    let%bind invocation, admission = invocation_admission_deltas t invocation ~origin in
    let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
    let%bind _ =
      transition
        t
        ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
        ~payloads:[]
    in
    let borrow =
      { operation_id = Some operation_id
      ; job_scope = None
      ; kind = Invocation
      ; observation = None
      ; invocation = dispatched
      ; committed = false
      ; accepts_children = true
      ; callback_finished = false
      ; authority_live = Atomic.make true
      ; cancel = None
      ; cancel_requested = false
      }
    in
    t.moderator_borrow <- Some borrow;
    Ok borrow)
;;

let has_pending_permission t =
  List.exists t.state.permissions ~f:(fun permission ->
    Agent_protocol.Permission.equal_state permission.state Pending)
;;

let idle_actor_available t =
  runtime_admission_open t
  && (not t.idle_moderator_borrowed)
  && Option.is_none t.moderator_borrow
  && Option.is_none t.state.active_operation
  && Agent_protocol.Session.equal_desired_state t.state.lifecycle.desired Running
  && (match t.state.lifecycle.observed with
      | Agent_protocol.Session.Idle -> true
      | _ -> false)
  && (not t.state.halted)
  && Option.is_none t.state.failure
;;

let runtime_ready t =
  (not (Session_state.execution_migration_required t.state))
  &&
  match
    t.state.runtime_initialization, Inference.Selection.view t.state.spec.inference_target
  with
  | Ready, Captured _ -> true
  | Pending _, _ | Ready, Unresolved -> false
;;

let idle_moderator_eligible t =
  runtime_ready t && idle_actor_available t && not (has_pending_permission t)
;;

let check_run_binding t run_id =
  let open Result.Let_syntax in
  let%bind binding =
    Result.of_option
      (Run_authorities.find t.run_authorities run_id)
      ~error:(error Permission_denied "run no longer has live host authority")
  in
  Run_admission.Scope.authorize (Run_authorities.scope binding) t.state
;;

let claim_queued_event
      t
      id
      operation_id
      (snapshot : Session.Moderator_state.Identity_snapshot.t)
  =
  let open Result.Let_syntax in
  let%bind available =
    match operation_id with
    | None -> Ok (idle_moderator_eligible t)
    | Some id ->
      let%map _ = running_operation t id in
      not (t.idle_moderator_borrowed || moderator_is_borrowed t)
  in
  match available with
  | false -> Ok None
  | true ->
    let%bind receipt, event =
      (match operation_id with
       | None -> Queued_moderator_event.claim
       | Some operation_id -> Queued_moderator_event.claim_foreground ~operation_id)
        ~state:t.state
        ~id
        ~snapshot
        ~now:(t.services.now ())
    in
    let%bind retirement_reason =
      Queued_moderator_event.delivery_retirement_reason
        ~state:t.state
        ~observer:receipt.context.source
        ~event
        ~subscription_expired:(subscription_is_due t)
    in
    let borrow =
      { kind = Queued
      ; job_scope = None
      ; agent_admission = Contextual None
      ; receipt
      ; before = snapshot
      ; event
      ; retirement_reason
      ; run_actions = None
      ; callback_active = true
      ; authority_live = Atomic.make true
      ; committed = false
      ; cancel = None
      ; cancel_requested = false
      }
    in
    (match retirement_reason with
     | Some _ ->
       t.queued_event_borrow <- Some borrow;
       t.idle_moderator_borrowed <- true;
       Ok (Some borrow)
     | None ->
       let previous = t.state in
       let%bind resumed, run_deltas =
         Run_transition.claim_wake previous ~executing:receipt ~now:(t.services.now ())
       in
       let%bind candidate =
         prepare_transition
           t
           ~delta:
             (Session_delta.Batch
                (Session_delta.Moderator_execution_changed receipt :: run_deltas))
           ~payloads:[]
       in
       let%bind acknowledgement =
         Queued_claim_acknowledgement.prepare
           ~previous
           ~authorities:t.run_authorities
           ~resumed
           ~candidate
           ~borrow
       in
       Exn.protect
         ~finally:(fun () -> Queued_claim_acknowledgement.discard acknowledgement)
         ~f:(fun () ->
           let check_basis () =
             let%bind available =
               match operation_id with
               | None -> Ok (idle_moderator_eligible t)
               | Some id ->
                 let%map _ = running_operation t id in
                 runtime_admission_open t
                 && not (t.idle_moderator_borrowed || moderator_is_borrowed t)
             in
             if t.stopped || not available
             then Error (error Conflict "queued claim owner changed before ACK")
             else
               Queued_claim_acknowledgement.check_basis
                 acknowledgement
                 ~previous:t.state
                 ~authorities:t.run_authorities
                 ~current_borrow:t.queued_event_borrow
                 ~candidate
           in
           let check_before_commit () =
             let%bind () = check_basis () in
             let%bind () =
               Option.value_map
                 (Queued_claim_acknowledgement.scope acknowledgement)
                 ~default:(Ok ())
                 ~f:(fun scope -> Run_admission.Scope.authorize scope t.state)
             in
             let%bind () =
               Option.value_map
                 (Queued_claim_acknowledgement.scope acknowledgement)
                 ~default:(Ok ())
                 ~f:Run_admission.Scope.check_current
             in
             check_basis ()
           in
           Eio.Cancel.protect (fun () ->
             let%map () =
               install
                 ~queued_claim_acknowledgement:acknowledgement
                 ~check_before_commit
                 t
                 candidate
             in
             t.queued_event_borrow)))
;;

type lifecycle_claim_handoff =
  | Current_lifecycle_child of
      { original : agent_admission
      ; child : agent_admission
      }
  | Expired_lifecycle_original of
      { original : agent_admission
      ; child : agent_admission
      }

let stage_lifecycle_claim original =
  let expired () =
    match Execution_custody.Admission.custody original with
    | Some custody ->
      Ok
        (Expired_lifecycle_original
           { original; child = create_agent_admission (Some custody) })
    | None -> Error (error Internal_error "expired lifecycle admission lost its original")
  in
  match Execution_custody.Admission.status original with
  | Authority_expired -> expired ()
  | Closed -> Error (error Conflict "lifecycle admission has ended")
  | Current ->
    (match transfer_agent_admission original with
     | Ok child -> Ok (Current_lifecycle_child { original; child })
     | Error failure ->
       (match Execution_custody.Admission.status original with
        | Authority_expired -> expired ()
        | Current | Closed -> Error failure))
;;

let lifecycle_claim_admission = function
  | Current_lifecycle_child { child; _ } | Expired_lifecycle_original { child; _ } ->
    child
;;

let discard_lifecycle_claim = function
  | Current_lifecycle_child { child; _ } -> retire_agent_admission child
  | Expired_lifecycle_original { child; _ } -> Execution_custody.Admission.close child
;;

let check_lifecycle_claim = function
  | Current_lifecycle_child { child; _ } -> check_agent_admission child
  | Expired_lifecycle_original _ -> Ok ()
;;

let acknowledge_lifecycle_claim = function
  | Current_lifecycle_child { original; _ } | Expired_lifecycle_original { original; _ }
    -> retire_agent_admission original
;;

let claim_ordinary_event t id operation_id snapshot event =
  let open Result.Let_syntax in
  let%bind available =
    match operation_id with
    | None ->
      (match event with
       | Chat_response.Moderation.Event.Session_start | Session_resume ->
         Ok (idle_moderator_eligible t)
       | _ -> Error (error Invalid_state "ordinary event requires an active operation"))
    | Some id ->
      let%map _ = running_operation t id in
      not (t.idle_moderator_borrowed || moderator_is_borrowed t)
  in
  match available with
  | false -> Ok None
  | true ->
    let%bind receipt, event =
      Queued_moderator_event.claim_ordinary
        ~state:t.state
        ~id
        ~snapshot
        ~operation_id
        ~event
        ~now:(t.services.now ())
    in
    let%bind handoff =
      match operation_id, t.lifecycle_agent_custody with
      | None, Admitted { pending = Pending admission; _ } ->
        Result.map (stage_lifecycle_claim admission) ~f:Option.some
      | Some _, _ | None, (Unadmitted | Admitted { pending = Consumed; _ }) -> Ok None
    in
    let agent_admission = Option.map handoff ~f:lifecycle_claim_admission in
    let acknowledged = ref false in
    Exn.protect
      ~finally:(fun () ->
        if not !acknowledged then Option.iter handoff ~f:discard_lifecycle_claim)
      ~f:(fun () ->
        let borrow =
          { kind = Ordinary
          ; job_scope = None
          ; agent_admission = Contextual agent_admission
          ; receipt
          ; before = snapshot
          ; event
          ; retirement_reason = None
          ; run_actions = None
          ; callback_active = true
          ; authority_live = Atomic.make true
          ; committed = false
          ; cancel = None
          ; cancel_requested = false
          }
        in
        let%bind prepared =
          prepare_transition
            t
            ~delta:(Session_delta.Moderator_execution_changed receipt)
            ~payloads:[]
        in
        let%bind () =
          Option.value_map handoff ~default:(Ok ()) ~f:check_lifecycle_claim
        in
        Eio.Cancel.protect (fun () ->
          let%bind () = persist_transition t prepared in
          install_committed_transition t prepared;
          Option.iter handoff ~f:(fun handoff ->
            (match t.lifecycle_agent_custody with
             | Admitted admitted ->
               t.lifecycle_agent_custody <- Admitted { admitted with pending = Consumed }
             | Unadmitted -> ());
            acknowledge_lifecycle_claim handoff);
          t.queued_event_borrow <- Some borrow;
          t.idle_moderator_borrowed <- true;
          acknowledged := true;
          publish_committed_transition t prepared;
          Ok (Some borrow)))
;;

let claim_delegated_event t id delegation snapshot event =
  let open Result.Let_syntax in
  match t.state.lifecycle.desired, t.state.halted, t.state.failure with
  | Stopped, _, _ | _, true, _ | _, _, Some _ -> Ok Delegated_unavailable
  | Running, false, None ->
    if t.idle_moderator_borrowed || moderator_is_borrowed t
    then Ok Delegated_unavailable
    else (
      let%bind claimed =
        Queued_moderator_event.claim_delegated
          ~delegation
          ~state:t.state
          ~id
          ~snapshot
          ~event
          ~now:(t.services.now ())
      in
      match claimed with
      | Replayed receipt -> Ok (Delegated_replay receipt)
      | Claimed (receipt, event) ->
        let%bind _ =
          transition
            t
            ~delta:(Session_delta.Moderator_execution_changed receipt)
            ~payloads:[]
        in
        let borrow =
          { kind = Ordinary
          ; job_scope = None
          ; agent_admission = Contextual None
          ; receipt
          ; before = snapshot
          ; event
          ; retirement_reason = None
          ; run_actions = None
          ; callback_active = true
          ; authority_live = Atomic.make true
          ; committed = false
          ; cancel = None
          ; cancel_requested = false
          }
        in
        t.queued_event_borrow <- Some borrow;
        t.idle_moderator_borrowed <- true;
        Ok (Delegated_borrow borrow))
;;

let claim_job_event t scope id snapshot event =
  let open Result.Let_syntax in
  let%bind () = job_scope_can_execute t scope in
  let%bind () =
    match t.idle_moderator_borrowed, moderator_is_borrowed t with
    | false, false -> Ok ()
    | _ -> Error (error Conflict "moderator is already borrowed")
  in
  let%bind receipt, event =
    Queued_moderator_event.claim_job
      ~job:
        { job_id = scope.job.id; attempt = scope.job.attempt; deadline = scope.deadline }
      ~state:t.state
      ~id
      ~snapshot
      ~event
      ~now:(t.services.now ())
  in
  let%bind _ =
    transition t ~delta:(Session_delta.Moderator_execution_changed receipt) ~payloads:[]
  in
  let borrow =
    { kind = Ordinary
    ; job_scope = Some scope
    ; agent_admission = Contextual None
    ; receipt
    ; before = snapshot
    ; event
    ; retirement_reason = None
    ; run_actions = None
    ; callback_active = true
    ; authority_live = Atomic.make true
    ; committed = false
    ; cancel = None
    ; cancel_requested = false
    }
  in
  t.queued_event_borrow <- Some borrow;
  t.idle_moderator_borrowed <- true;
  Ok (Some borrow)
;;

let validate_queued_event_borrow t borrow =
  match t.queued_event_borrow with
  | Some current when phys_equal current borrow && t.idle_moderator_borrowed ->
    let open Result.Let_syntax in
    let%bind () =
      match borrow.job_scope, borrow.receipt.context.operation_id with
      | Some scope, None -> Result.map (job_scope_current t scope) ~f:ignore
      | None, Some id -> Result.map (current_operation t id) ~f:ignore
      | None, None when Option.is_some borrow.receipt.delegation -> Ok ()
      | None, None when Option.is_none t.state.active_operation -> Ok ()
      | _ -> Error (error Conflict "event no longer owns the moderator")
    in
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      borrow.receipt.context.session_id
      borrow.receipt.context.generation
  | _ -> Error (error Conflict "queued event is no longer owned by this callback")
;;

let legacy_boundary_available t parent event =
  let open Result.Let_syntax in
  let%bind () =
    match event with
    | Chat_response.Moderation.Event.Internal_event _ ->
      Error (error Invalid_request "legacy internal events require an exact queue head")
    | Session_start
    | Session_resume
    | Turn_start
    | Item_appended _
    | Pre_tool_call _
    | Post_tool_response _
    | Turn_end -> Ok ()
  in
  match parent with
  | Callback_job scope ->
    let%map () = job_scope_can_execute t scope in
    not (t.idle_moderator_borrowed || moderator_is_borrowed t)
  | Callback_operation (Some id) ->
    let%map _ = running_operation t id in
    not (t.idle_moderator_borrowed || moderator_is_borrowed t)
  | Callback_operation None ->
    (match event with
     | Session_start | Session_resume -> Ok (idle_moderator_eligible t)
     | Turn_start | Item_appended _ | Pre_tool_call _ | Post_tool_response _ | Turn_end ->
       Error (error Invalid_state "legacy boundary requires its active operation")
     | Internal_event _ ->
       Error (error Invalid_request "legacy internal events require an exact queue head"))
;;

let legacy_boundary_admission t parent ~manager ~source =
  match parent with
  | Callback_operation (Some id) ->
    Ok
      ( Option.bind t.operation_agent_custody ~f:(fun (owner, admission) ->
          Option.some_if (Agent_protocol.Id.Operation.equal owner id) admission)
      , false )
  | Callback_operation None ->
    (match t.lifecycle_agent_custody with
     | Admitted { generation; source; pending = Pending admission }
       when agent_source_current t generation source -> Ok (Some admission, true)
     | Admitted _ | Unadmitted -> Ok (None, false))
  | Callback_job scope ->
    (* Missing historical job custody is unavailable, not admitted legacy None.
       Fresh optional job admissions are integrated with their real producers. *)
    Result.map (job_callback_admission t scope ~manager ~source) ~f:(fun admission ->
      admission, false)
;;

let claim_initialization_callback
      t
      (constructor : Initialization_scope.t)
      manager
      callback
  =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_scope t constructor in
  let%bind () = check_initialization_agent_actor constructor in
  let%bind event =
    match Chat_response.Moderator_manager.Callback.selection callback with
    | Boundary (Session_start as event) | Boundary (Session_resume as event) -> Ok event
    | Boundary
        ( Turn_start
        | Item_appended _
        | Pre_tool_call _
        | Post_tool_response _
        | Turn_end
        | Internal_event _ )
    | Queue_head ->
      Error
        (error Invalid_request "constructor admits only its startup or resume callback")
  in
  let%bind () =
    match constructor.callback with
    | Unclaimed -> Ok ()
    | Claimed _ | Settled _ ->
      Error (error Conflict "constructor callback was already claimed")
  in
  let%bind () =
    match t.moderator_manager with
    | None -> Ok ()
    | Some installed when phys_equal installed manager -> Ok ()
    | Some _ ->
      Error (error Conflict "constructor cannot borrow another installed Manager")
  in
  let revision = t.state.counters.revision in
  let%bind checkpoint =
    Constructor_callback_checkpoint.prepare t.state ~manager ~callback
  in
  let snapshot = Chat_response.Moderator_manager.Callback.snapshot callback in
  let checkpoint_state =
    { t.state with
      moderator = Some checkpoint.moderator
    ; run_state = Some checkpoint.index
    }
  in
  let%bind receipt, event =
    Queued_moderator_event.claim_ordinary
      ~state:checkpoint_state
      ~id:(Agent_protocol.Id.Moderator_execution.create ())
      ~operation_id:None
      ~snapshot
      ~event
      ~now:(t.services.now ())
  in
  let%bind origin =
    Invocation_inference_provenance.of_event
      ~runs:checkpoint_state.run_state
      ~jobs:checkpoint_state.jobs
      receipt
  in
  let%bind admission = transfer_agent_admission constructor.agent_admission in
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () -> if not !acknowledged then retire_agent_admission admission)
    ~f:(fun () ->
      let%bind prepared =
        prepare_transition
          ?run_preparation:constructor.run_preparation
          t
          ~delta:
            (Session_delta.Batch
               [ Run_state_changed checkpoint.index
               ; Moderator_changed (Some checkpoint.moderator)
               ; Moderator_execution_changed receipt
               ])
          ~payloads:[]
      in
      let%bind resource =
        Initialization_scope.Resource.select
          constructor.resource
          prepared.state
          ~manager
          ~source:checkpoint.source
          ~ordinals:[]
          ~schedules:[]
      in
      let acknowledgement =
        Initialization_scope.Acknowledgement.prepare
          ~callback:(Claimed receipt.context.id)
          ~resource
          constructor
          prepared.state
      in
      let%bind () =
        validate_initialization_projection
          (Initialization_scope.Acknowledgement.expected acknowledgement)
          prepared.state
      in
      let borrow =
        { kind = Ordinary
        ; job_scope = None
        ; agent_admission = Contextual None
        ; receipt
        ; before = snapshot
        ; event
        ; retirement_reason = None
        ; run_actions = None
        ; callback_active = true
        ; authority_live = Atomic.make true
        ; committed = false
        ; cancel = None
        ; cancel_requested = false
        }
      in
      let scope : Callback_scope.t =
        { manager
        ; constructor_scope = Some constructor
        ; committed_checkpoint = None
        ; callback
        ; borrow
        ; source = checkpoint.source
        ; origin
        ; custody = Admitted admission
        }
      in
      Eio.Cancel.protect (fun () ->
        let%bind () = validate_initialization_scope t constructor in
        let%bind () = check_initialization_agent_actor constructor in
        let%bind () = check_agent_admission admission in
        let%bind () = Initialization_scope.Acknowledgement.check_basis acknowledgement in
        let%bind current =
          Constructor_callback_checkpoint.prepare t.state ~manager ~callback
        in
        let%bind () =
          if
            Int64.equal revision t.state.counters.revision
            && Agent_protocol.Run_source.equal checkpoint.source current.source
          then Ok ()
          else Error (error Conflict "constructor callback basis changed before commit")
        in
        let%bind () = persist_transition t prepared in
        install_committed_transition
          ?run_preparation:constructor.run_preparation
          ~initialization_acknowledgement:acknowledgement
          t
          prepared;
        t.queued_event_borrow <- Some borrow;
        t.idle_moderator_borrowed <- true;
        t.legacy_callback_scope <- Some scope;
        acknowledged := true;
        publish_committed_transition t prepared;
        Ok (Some scope)))
;;

let claim_legacy_boundary t manager callback parent event =
  let open Result.Let_syntax in
  let%bind available = legacy_boundary_available t parent event in
  if not available
  then Ok None
  else (
    let snapshot = Chat_response.Moderator_manager.Callback.snapshot callback in
    let id = Agent_protocol.Id.Moderator_execution.create () in
    let%bind receipt, captured_event =
      match parent with
      | Callback_operation operation_id ->
        Queued_moderator_event.claim_ordinary
          ~state:t.state
          ~id
          ~operation_id
          ~snapshot
          ~event
          ~now:(t.services.now ())
      | Callback_job scope ->
        Queued_moderator_event.claim_job
          ~job:
            { job_id = scope.job.id
            ; attempt = scope.job.attempt
            ; deadline = scope.deadline
            }
          ~state:t.state
          ~id
          ~snapshot
          ~event
          ~now:(t.services.now ())
    in
    let%bind index, source = Moderator_source_installation.capture t.state in
    let%bind metadata = Moderator_checkpoint.queue t.state.moderator in
    let%bind metadata =
      match metadata with
      | Some metadata -> Ok metadata
      | None -> Moderator_queue.bootstrap ~source ~queue:snapshot.queued_internal_events
    in
    let%bind moderator =
      Moderator_checkpoint.update_with_queue t.state.moderator ~snapshot ~queue:metadata
    in
    let%bind origin =
      Invocation_inference_provenance.of_event
        ~runs:t.state.run_state
        ~jobs:t.state.jobs
        receipt
    in
    let%bind parent_admission, consume_lifecycle =
      legacy_boundary_admission t parent ~manager ~source
    in
    let%bind admission =
      match parent_admission with
      | None -> Ok None
      | Some admission -> Result.map (transfer_agent_admission admission) ~f:Option.some
    in
    let acknowledged = ref false in
    Exn.protect
      ~finally:(fun () ->
        if not !acknowledged then Option.iter admission ~f:retire_agent_admission)
      ~f:(fun () ->
        let%bind prepared =
          prepare_transition
            t
            ~delta:
              (Session_delta.Batch
                 [ Run_state_changed index
                 ; Moderator_changed (Some moderator)
                 ; Moderator_execution_changed receipt
                 ])
            ~payloads:[]
        in
        let%bind () =
          if
            Chat_response.Moderator_manager.callback_is_current manager callback
            && Option.exists t.moderator_manager ~f:(phys_equal manager)
          then Ok ()
          else Error (error Conflict "legacy Manager changed before boundary claim")
        in
        let%bind () =
          match parent_admission with
          | None -> Ok ()
          | Some admission -> check_agent_admission admission
        in
        let%bind available = legacy_boundary_available t parent event in
        let%bind () =
          if available
          then Ok ()
          else Error (error Conflict "legacy boundary physical owner changed")
        in
        let%bind _, current_source = Moderator_source_installation.capture t.state in
        let%bind () =
          if Agent_protocol.Run_source.equal source current_source
          then Ok ()
          else Error (error Conflict "legacy boundary source changed before claim")
        in
        Eio.Cancel.protect (fun () ->
          let%bind () = persist_transition t prepared in
          install_committed_transition t prepared;
          let borrow =
            { kind = Ordinary
            ; job_scope =
                (match parent with
                 | Callback_job scope -> Some scope
                 | Callback_operation _ -> None)
            ; agent_admission = Contextual None
            ; receipt
            ; before = snapshot
            ; event = captured_event
            ; retirement_reason = None
            ; callback_active = true
            ; authority_live = Atomic.make true
            ; run_actions = None
            ; committed = false
            ; cancel = None
            ; cancel_requested = false
            }
          in
          let scope : Callback_scope.t =
            { manager
            ; constructor_scope = None
            ; committed_checkpoint = None
            ; callback
            ; borrow
            ; source
            ; origin
            ; custody =
                (match admission with
                 | None -> Unavailable
                 | Some admission -> Admitted admission)
            }
          in
          t.queued_event_borrow <- Some borrow;
          t.idle_moderator_borrowed <- true;
          t.legacy_callback_scope <- Some scope;
          if consume_lifecycle
          then (
            match t.lifecycle_agent_custody with
            | Admitted ({ pending = Pending current; _ } as lifecycle)
              when Option.exists parent_admission ~f:(phys_equal current) ->
              t.lifecycle_agent_custody <- Admitted { lifecycle with pending = Consumed };
              retire_agent_admission current
            | Unadmitted | Admitted _ -> ());
          acknowledged := true;
          publish_committed_transition t prepared;
          Ok (Some scope))))
;;

let claim_legacy_head t manager callback operation_id =
  let open Result.Let_syntax in
  let available () =
    match operation_id with
    | None -> Ok (idle_moderator_eligible t)
    | Some id ->
      let%map _ = running_operation t id in
      not (t.idle_moderator_borrowed || moderator_is_borrowed t)
  in
  let%bind ready = available () in
  if not ready
  then Ok None
  else (
    let snapshot = Chat_response.Moderator_manager.Callback.snapshot callback in
    let%bind receipt, event =
      Queued_moderator_event.claim_legacy
        ~callback
        ~operation_id
        ~state:t.state
        ~id:(Agent_protocol.Id.Moderator_execution.create ())
        ~now:(t.services.now ())
    in
    let%bind index, source = Moderator_source_installation.capture t.state in
    let%bind metadata = Moderator_checkpoint.queue t.state.moderator in
    let%bind metadata =
      match metadata with
      | Some metadata -> Ok metadata
      | None -> Moderator_queue.bootstrap ~source ~queue:snapshot.queued_internal_events
    in
    let%bind entry =
      Result.of_option
        (List.hd (Moderator_queue.entries metadata))
        ~error:
          (error Conflict "legacy callback requires the actual structural queue head")
    in
    let original =
      Legacy_queue_custody.find t.legacy_queue_custody ~manager ~source ~entry
    in
    let%bind admission =
      match original with
      | None -> Ok None
      | Some original ->
        Result.map (Legacy_queue_custody.Row.transfer original) ~f:Option.some
    in
    let acknowledged = ref false in
    Exn.protect
      ~finally:(fun () ->
        if not !acknowledged then Option.iter admission ~f:retire_agent_admission)
      ~f:(fun () ->
        let%bind moderator =
          Moderator_checkpoint.update_with_queue
            t.state.moderator
            ~snapshot
            ~queue:metadata
        in
        (* The installation and original queue precede the v5 Running leaf. No
           incoming receipt can certify its own source or attribution. *)
        let%bind prepared =
          prepare_transition
            t
            ~delta:
              (Session_delta.Batch
                 [ Run_state_changed index
                 ; Moderator_changed (Some moderator)
                 ; Moderator_execution_changed receipt
                 ])
            ~payloads:[]
        in
        let%bind () =
          if
            Chat_response.Moderator_manager.callback_is_current manager callback
            && Option.exists t.moderator_manager ~f:(phys_equal manager)
          then Ok ()
          else Error (error Conflict "legacy Manager changed before queue claim")
        in
        let%bind ready = available () in
        let%bind () =
          if ready
          then Ok ()
          else Error (error Conflict "legacy queue drainer changed before claim")
        in
        let%bind _, current_source = Moderator_source_installation.capture t.state in
        let%bind () =
          if Agent_protocol.Run_source.equal source current_source
          then Ok ()
          else Error (error Conflict "legacy queue source changed before claim")
        in
        let%bind () =
          match original with
          | None -> Ok ()
          | Some row -> Legacy_queue_custody.Row.check row
        in
        let borrow =
          { kind = Queued
          ; job_scope = None
          ; agent_admission = Contextual None
          ; receipt
          ; before = snapshot
          ; event
          ; retirement_reason = None
          ; run_actions = None
          ; callback_active = true
          ; authority_live = Atomic.make true
          ; committed = false
          ; cancel = None
          ; cancel_requested = false
          }
        in
        let scope : Callback_scope.t =
          { manager
          ; constructor_scope = None
          ; committed_checkpoint = None
          ; callback
          ; borrow
          ; source
          ; origin = Moderator_queue.Entry.origin entry
          ; custody =
              (match admission with
               | None -> Unavailable
               | Some admission -> Admitted admission)
          }
        in
        let remaining =
          Legacy_queue_custody.without t.legacy_queue_custody ~selected:original
        in
        Eio.Cancel.protect (fun () ->
          let%bind () = persist_transition t prepared in
          install_committed_transition t prepared;
          t.legacy_queue_custody
          <- Legacy_queue_custody.install t.legacy_queue_custody ~incoming:remaining;
          t.queued_event_borrow <- Some borrow;
          t.idle_moderator_borrowed <- true;
          t.legacy_callback_scope <- Some scope;
          acknowledged := true;
          publish_committed_transition t prepared;
          Ok (Some scope))))
;;

let claim_legacy_callback t callback parent =
  let open Result.Let_syntax in
  let%bind manager =
    match t.moderator_manager with
    | Some manager
      when Chat_response.Moderator_manager.callback_is_current manager callback ->
      Ok manager
    | _ -> Error (error Conflict "legacy callback requires its exact installed Manager")
  in
  match Chat_response.Moderator_manager.Callback.selection callback with
  | Boundary event -> claim_legacy_boundary t manager callback parent event
  | Queue_head ->
    let%bind operation_id =
      match parent with
      | Callback_operation id -> Ok id
      | Callback_job _ ->
        Error (error Invalid_request "job callback cannot claim an unrelated queue head")
    in
    claim_legacy_head t manager callback operation_id
;;

let queued_retirement_available t =
  (not t.idle_moderator_borrowed)
  && (not (moderator_is_borrowed t))
  && Option.is_none t.state.active_operation
  && (not (has_pending_permission t))
  &&
  match t.state.lifecycle.desired, t.state.lifecycle.observed with
  | Running, Idle | Stopped, Stopped -> true
  | _ -> false
;;

let claim_queued_retirement t id snapshot reason =
  let open Result.Let_syntax in
  match queued_retirement_available t with
  | false -> Ok None
  | true ->
    let%bind receipt, event =
      Queued_moderator_event.claim_retirement ~state:t.state ~id ~snapshot
    in
    let%bind _ =
      Agent_protocol.Moderator_execution.retire
        receipt
        ~checkpoint_sha256:receipt.context.checkpoint_sha256
        ~reason
    in
    let borrow =
      { kind = Queued
      ; job_scope = None
      ; agent_admission = Contextual None
      ; receipt
      ; before = snapshot
      ; event
      ; retirement_reason = Some reason
      ; run_actions = None
      ; callback_active = true
      ; authority_live = Atomic.make true
      ; committed = false
      ; cancel = None
      ; cancel_requested = false
      }
    in
    t.queued_event_borrow <- Some borrow;
    t.idle_moderator_borrowed <- true;
    Ok (Some borrow)
;;

let queued_event_can_commit_installed t borrow =
  let open Result.Let_syntax in
  let%bind () = validate_queued_event_borrow t borrow in
  match borrow.job_scope, borrow.receipt.context.operation_id with
  | Some scope, None ->
    let%bind () = job_scope_can_execute t scope in
    (match
       borrow.callback_active && not (borrow.committed || borrow.cancel_requested)
     with
     | true -> Ok ()
     | false -> Error (error Conflict "job event cannot commit after completion or stop"))
  | Some _, Some _ -> Error (error Conflict "job event cannot borrow a model operation")
  | None, None when Option.is_some borrow.receipt.delegation ->
    (match t.state.lifecycle.desired, t.state.failure with
     | Running, None
       when borrow.callback_active
            && not (borrow.committed || borrow.cancel_requested || t.state.halted) ->
       Ok ()
     | _ ->
       Error (error Conflict "delegated policy cannot commit after completion or stop"))
  | None, Some id ->
    let%bind _ = running_operation t id in
    if
      borrow.callback_active
      && not (borrow.committed || borrow.cancel_requested || t.state.halted)
    then Ok ()
    else Error (error Conflict "foreground event cannot commit after completion or stop")
  | None, None ->
    (match
       ( borrow.retirement_reason
       , t.state.lifecycle.desired
       , t.state.lifecycle.observed
       , t.state.failure )
     with
     | (Some _, Running, Idle, _ | Some _, Stopped, Stopped, _)
       when borrow.callback_active
            && (not (borrow.committed || borrow.cancel_requested))
            && Option.is_none t.state.active_operation -> Ok ()
     | None, Running, Idle, None
       when borrow.callback_active
            && (not (borrow.committed || borrow.cancel_requested || t.state.halted))
            && Option.is_none t.state.active_operation -> Ok ()
     | _ -> Error (error Conflict "queued event cannot commit after completion or stop"))
;;

(* Physical constructor checks shared by effect admission and the nonyielding
   final inference guard. Original host policy refresh is a separate phase. *)
let constructor_event_current t borrow scope =
  let open Result.Let_syntax in
  let%bind () = validate_queued_event_borrow t borrow in
  let%bind () = check_initialization_agent_actor scope in
  if initialization_callback_is_owned t scope
  then Ok ()
  else Error (error Conflict "constructor callback is no longer current")
;;

let queued_event_inference_current t borrow =
  match constructor_scope_of_borrow t borrow with
  | None -> queued_event_can_commit_installed t borrow
  | Some scope ->
    let open Result.Let_syntax in
    let%bind () = validate_initialization_scope_current t scope in
    constructor_event_current t borrow scope
;;

let queued_event_can_commit t borrow =
  match constructor_scope_of_borrow t borrow with
  | None -> queued_event_can_commit_installed t borrow
  | Some scope ->
    let open Result.Let_syntax in
    let%bind () = validate_initialization_scope t scope in
    constructor_event_current t borrow scope
;;

let callback_run_preparation (scope : Callback_scope.t) =
  Option.bind scope.constructor_scope ~f:(fun constructor -> constructor.run_preparation)
;;

let check_callback_scope_current t (scope : Callback_scope.t) =
  let open Result.Let_syntax in
  let%bind () =
    match t.legacy_callback_scope, scope.constructor_scope with
    | Some current, Some constructor when phys_equal current scope ->
      (match constructor.resource with
       | Some resource
         when initialization_callback_is_owned t constructor
              && phys_equal resource.manager scope.manager
              && Agent_protocol.Run_source.equal resource.source scope.source
              && not resource.handed_off -> Ok ()
       | None | Some _ -> Error (error Conflict "constructor callback resource changed"))
    | Some current, None when phys_equal current scope ->
      (match t.moderator_manager with
       | Some manager
         when phys_equal manager scope.manager
              && Chat_response.Moderator_manager.callback_is_current
                   manager
                   scope.callback -> Ok ()
       | None | Some _ ->
         Error (error Conflict "legacy callback no longer owns the installed Manager"))
    | None, _ | Some _, _ ->
      Error (error Conflict "legacy callback no longer owns its exact borrow")
  in
  let%bind _, source = Moderator_source_installation.capture t.state in
  let%bind () =
    if Agent_protocol.Run_source.equal source scope.source
    then Ok ()
    else Error (error Conflict "legacy callback source installation changed")
  in
  let%bind () =
    match scope.borrow.job_scope with
    | None -> Ok ()
    | Some job_scope ->
      let%bind current =
        job_callback_admission t job_scope ~manager:scope.manager ~source:scope.source
      in
      (match scope.custody, current with
       | Unavailable, None | Admitted _, Some _ -> Ok ()
       | Unavailable, Some _ | Admitted _, None ->
         Error (error Conflict "Job callback admission changed before commit"))
  in
  match scope.custody with
  | Unavailable -> Ok ()
  | Admitted admission -> check_agent_admission admission
;;

let check_callback_inference_current t (scope : Callback_scope.t) =
  let open Result.Let_syntax in
  let%bind () = queued_event_inference_current t scope.borrow in
  check_callback_scope_current t scope
;;

let check_callback_current t (scope : Callback_scope.t) =
  let open Result.Let_syntax in
  let%bind () = queued_event_can_commit t scope.borrow in
  check_callback_scope_current t scope
;;

let callback_actor_after_current t ~scope =
  let open Result.Let_syntax in
  match scope.Callback_scope.custody with
  | Unavailable ->
    Error (error Permission_denied "legacy queue continuation custody is unavailable")
  | Admitted admission ->
    (match Execution_custody.Admission.custody admission with
     | None -> Ok None
     | Some custody ->
       let%map actor = Execution_custody.actor custody in
       Some
         (Operator_authorization.guarded
            ~principal:(Operator_authorization.principal actor)
            ~is_current:(fun () ->
              Result.is_ok (check_callback_inference_current t scope)
              && Operator_authorization.is_current actor)))
;;

let callback_agent_actor t ~scope =
  let open Result.Let_syntax in
  let%bind () = check_callback_current t scope in
  callback_actor_after_current t ~scope
;;

let callback_inference_actor t ~scope =
  let open Result.Let_syntax in
  let%bind () = check_callback_inference_current t scope in
  callback_actor_after_current t ~scope
;;

let event_execution_owned_by borrow execution =
  match execution.owner with
  | Event_moderator owner -> phys_equal owner borrow
  | _ -> false
;;

let active_script_parent t ~owns (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind parent =
    match invocation.context.origin, invocation.context.parent_invocation with
    | Script, Some parent
      when Option.is_none invocation.parent_event
           && Option.is_none invocation.context.parent_job
           && Option.is_none invocation.context.provider_call_id
           && Option.is_none invocation.context.call_entry_id ->
      List.find t.invocation_executions ~f:(fun execution ->
        execution.accepts_children
        && owns execution
        && Agent_protocol.Id.Invocation.equal execution.dispatched.context.id parent)
      |> Result.of_option
           ~error:(error Conflict "script parent is not active in this owner")
    | _ -> Error (error Conflict "owned descendants require a script parent link")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:parent.dispatched.context.session_id
      ~generation:parent.dispatched.context.generation
      invocation.context.session_id
      invocation.context.generation
  in
  match parent.dispatched.context.deadline, invocation.context.deadline with
  | None, _ -> Ok ()
  | Some parent, Some child when Agent_protocol.Timestamp.compare child parent <= 0 ->
    Ok ()
  | Some _, _ ->
    Error (error Conflict "script descendant cannot extend its parent's deadline")
;;

let compatible_script_observer observer (invocation : Agent_protocol.Invocation.t) =
  match invocation.observation with
  | None -> Ok ()
  | Some observation
    when Agent_protocol.Invocation.equal_observer observer observation.observer -> Ok ()
  | Some _ -> Error (error Conflict "script descendant has a different moderator source")
;;

let claim_event_invocation t borrow (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let revision = t.state.counters.revision in
  let%bind () = queued_event_can_commit t borrow in
  let%bind () =
    match
      ( Option.bind borrow.job_scope ~f:(fun scope -> scope.deadline)
      , invocation.context.deadline )
    with
    | None, _ -> Ok ()
    | Some parent, Some child when Agent_protocol.Timestamp.compare child parent <= 0 ->
      Ok ()
    | _ -> Error (error Conflict "job event invocation cannot extend its deadline")
  in
  let%bind () =
    match borrow.retirement_reason, invocation.parent_event with
    | None, Some parent
      when Agent_protocol.Id.Moderator_execution.equal parent borrow.receipt.context.id ->
      Ok ()
    | None, None ->
      let%bind () =
        active_script_parent t ~owns:(event_execution_owned_by borrow) invocation
      in
      compatible_script_observer borrow.receipt.context.source invocation
    | _ -> Error (error Conflict "invocation does not belong to this executing event")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () =
    Extension_invariants.invocation_event_owner
      ~events:t.state.moderator_executions
      invocation
  in
  let%bind origin = event_invocation_origin t borrow.receipt invocation in
  let%bind invocation, admission = invocation_admission_deltas t invocation ~origin in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let run_preparation =
    Option.bind (constructor_scope_of_borrow t borrow) ~f:(fun scope ->
      scope.Initialization_scope.run_preparation)
  in
  let%bind prepared =
    prepare_transition
      ?run_preparation
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let%bind initialization_acknowledgement =
    prepare_constructor_invocation_ack t borrow ~before:None ~after:dispatched prepared
  in
  let execution =
    { owner = Event_moderator borrow
    ; dispatched
    ; accepts_children = true
    ; authority_live = Atomic.make true
    }
  in
  let%bind () =
    Eio.Cancel.protect (fun () ->
      let%bind () = queued_event_can_commit t borrow in
      let%bind () =
        check_constructor_invocation_ack t borrow initialization_acknowledgement
      in
      let%bind () =
        if Int64.equal revision t.state.counters.revision
        then Ok ()
        else Error (error Conflict "Invocation ACK basis changed before commit")
      in
      let%bind () = persist_transition t prepared in
      install_committed_transition
        ?run_preparation
        ?initialization_acknowledgement
        t
        prepared;
      t.invocation_executions <- execution :: t.invocation_executions;
      publish_committed_transition t prepared;
      Ok ())
  in
  Ok execution
;;

let check_run_authority t scope = check_run_binding t (Run_scope.run_id scope)

let run_turn_deltas t operation events deliveries =
  let open Result.Let_syntax in
  let%bind resumed, deltas =
    Run_transition.admit_turn
      t.state
      ~operation
      ~events
      ~deliveries
      ~now:(t.services.now ())
  in
  let%map () =
    List.fold_result resumed ~init:() ~f:(fun () run_id -> check_run_binding t run_id)
  in
  deltas
;;

let run_actions_internal t (executing : Agent_protocol.Moderator_execution.t) =
  let open Result.Let_syntax in
  let%bind borrow =
    match t.queued_event_borrow with
    | Some borrow when Agent_protocol.Moderator_execution.equal borrow.receipt executing
      -> Ok borrow
    | Some _ | None ->
      Error (error Conflict "run actions require the exact active event borrow")
  in
  let%bind () = queued_event_can_commit t borrow in
  match t.legacy_callback_scope with
  | Some scope when phys_equal scope.borrow borrow ->
    let%map () = check_callback_current t scope in
    None
  | None | Some _ ->
    (match borrow.run_actions with
     | Some owner ->
       let%map () = check_run_authority t (Run_action_service.scope owner) in
       Some owner
     | None ->
       let%bind selected = Run_event_scope.select t.state ~executing in
       (match selected with
        | None -> Ok None
        | Some run ->
          let%bind scope = Run_scope.create ~run ~execution_id:executing.context.id in
          let%map () = check_run_authority t scope in
          let owner = Run_action_service.create ~scope in
          borrow.run_actions <- Some owner;
          Some owner))
;;

module Prepared_run_delta = struct
  type t =
    { deltas : Session_delta.t list
    ; completion_sample : Recurrence_runtime.Completion.sample option
    }

  let empty = { deltas = []; completion_sample = None }
end

let completion_clock_pair t =
  let wall = t.services.now () in
  let monotonic = t.services.monotonic_now () in
  wall, Recurrence_runtime.Completion.sample ~wall ~monotonic
;;

let prepared_run_delta t (borrow : queued_event_borrow) completed requests job_deltas =
  let open Result.Let_syntax in
  match borrow.run_actions with
  | None -> Ok Prepared_run_delta.empty
  | Some owner ->
    let scope = Run_action_service.scope owner in
    let%bind () = check_run_authority t scope in
    let%bind action =
      Run_action_service.prepared owner |> Result.map_error ~f:(error Conflict)
    in
    let%bind _ =
      Run_action_service.compose_requests owner ~action ~requests
      |> Result.map_error ~f:(error Conflict)
    in
    let%bind owned_work =
      List.filter_map job_deltas ~f:(function
        | Session_delta.Job_changed job -> Some (Run_job_occurrence.work job)
        | Schedule_changed schedule ->
          Some
            (Agent_protocol.Run_work.create
               ~key:(Retained (Schedule schedule.id))
               ~generation:schedule.generation)
        | Subscription_changed subscription ->
          Some
            (Agent_protocol.Run_work.create
               ~key:(Retained (Subscription subscription.context.id))
               ~generation:subscription.context.generation)
        | Delivery_changed delivery ->
          Some
            (Agent_protocol.Run_work.create
               ~key:(Retained (Delivery delivery.context.id))
               ~generation:delivery.context.generation)
        | _ -> None)
      |> Result.all
    in
    let run =
      Option.bind t.state.run_state ~f:(fun index ->
        Run_state.find index (Run_scope.run_id scope))
    in
    let owns_callback =
      run
      |> Option.exists ~f:(fun run ->
        List.exists run.Agent_protocol.Run.owned_work ~f:(fun work ->
          match work.Agent_protocol.Run_work.key with
          | Retained (Moderator_execution id) ->
            Agent_protocol.Id.Moderator_execution.equal id borrow.receipt.context.id
          | Operation _
          | Retained (Job _ | Schedule _ | Invocation _ | Subscription _ | Delivery _) ->
            false))
    in
    let initial_single_turn =
      Option.exists run ~f:(fun run ->
        Run_event_scope.is_initial_single_turn run borrow.receipt.context)
    in
    if
      Option.is_none action
      && List.is_empty owned_work
      && (not owns_callback)
      && not initial_single_turn
    then Ok Prepared_run_delta.empty
    else (
      let%bind work_delta =
        Session_delta.capture_new_model_jobs
          t.state
          (Session_delta.Batch (job_deltas @ [ Moderator_execution_changed completed ]))
      in
      let%bind work_state = Session_delta.apply t.state work_delta in
      let now, completion_sample = completion_clock_pair t in
      let%bind prepared =
        Run_transition.prepare
          t.state
          ~scope
          ~executing:borrow.receipt
          ~work_state
          ~owned_work
          ~action
          ~now
      in
      let%bind () =
        match action with
        | Some (Wait wake) ->
          (match wake.occurrence with
           | Job_completion { job_id; attempt = _ } ->
             (match
                List.find work_state.jobs ~f:(fun job ->
                  Agent_protocol.Id.Job.equal job.Agent_protocol.Job.id job_id)
              with
              | None -> Error (error Invalid_request "job wait has no retained producer")
              | Some job ->
                Run_job_capacity.validate_wait
                  job
                  ~run:(Run_transition.run prepared)
                  ~publisher:t.services.job_results)
           | Delivered_timer _ | Subscription_delivery _ -> Ok ())
        | None | Some (Continue | Finish _) -> Ok ()
      in
      let%bind goal_finish =
        Run_action_service.prepared_goal_finish owner
        |> Result.map_error ~f:(fun message -> error Conflict message)
      in
      let%map goal_deltas =
        match goal_finish with
        | None -> Ok []
        | Some intent ->
          Goal_run_transition.prepare_request t.state ~scope ~prepared ~intent
      in
      Prepared_run_delta.
        { deltas = Run_transition.delta prepared :: goal_deltas
        ; completion_sample = Some completion_sample
        })
;;

let commit_queued_event t borrow snapshot requests decision notifications =
  let open Result.Let_syntax in
  let%bind () = queued_event_can_commit t borrow in
  let%bind () =
    match List.exists t.invocation_executions ~f:(event_execution_owned_by borrow) with
    | true -> Error (error Conflict "event native invocations still require completion")
    | false -> Ok ()
  in
  let%bind () =
    match t.legacy_callback_scope with
    | Some scope when phys_equal scope.borrow borrow -> check_callback_current t scope
    | None | Some _ -> Ok ()
  in
  let%bind () =
    match t.legacy_callback_scope with
    | Some scope when phys_equal scope.borrow borrow ->
      (match scope.custody with
       | Unavailable
         when requests.Agent_protocol.Invocation.request_turn
              || requests.request_compaction ->
         Error
           (error
              Permission_denied
              "legacy continuation custody is unavailable for turn or compaction requests")
       | Unavailable | Admitted _ -> Ok ())
    | None | Some _ -> Ok ()
  in
  let%bind receipt, admission_deltas =
    match borrow.retirement_reason, borrow.receipt.status with
    | Some reason, Running ->
      let%bind current_reason =
        Queued_moderator_event.delivery_retirement_reason
          ~state:t.state
          ~observer:borrow.receipt.context.source
          ~event:borrow.event
          ~subscription_expired:(subscription_is_due t)
      in
      let%bind () =
        match current_reason with
        | Some current when String.equal current reason -> Ok ()
        | _ ->
          Error
            (error Conflict "queued delivery retirement no longer matches its admission")
      in
      let%map interrupted =
        Agent_protocol.Moderator_execution.interrupt borrow.receipt ~reason
      in
      ( interrupted
      , [ Session_delta.Moderator_execution_changed borrow.receipt
        ; Moderator_execution_changed interrupted
        ] )
    | _ -> Ok (borrow.receipt, [])
  in
  let%bind completed =
    match borrow.receipt.delegation, decision, borrow.retirement_reason with
    | Some _, Some decision, None ->
      Queued_moderator_event.complete_delegated
        ~decision
        ~claimed:borrow.receipt
        ~before:borrow.before
        ~snapshot
        ~requests
    | Some _, _, _ | None, Some _, _ ->
      Error (error Invalid_state "delegated decision does not match event ownership")
    | None, None, None ->
      (match borrow.kind with
       | Queued -> Queued_moderator_event.complete
       | Ordinary -> Queued_moderator_event.complete_ordinary)
        ~claimed:borrow.receipt
        ~before:borrow.before
        ~snapshot
        ~requests
    | None, None, Some reason ->
      (match
         ( requests.Agent_protocol.Invocation.request_turn
         , requests.request_compaction
         , requests.end_session )
       with
       | false, false, None ->
         Queued_moderator_event.retire
           ~claimed:receipt
           ~before:borrow.before
           ~snapshot
           ~reason
       | _ ->
         Error (error Invalid_state "failed-head retirement cannot schedule new work"))
  in
  let legacy_scope =
    Option.filter t.legacy_callback_scope ~f:(fun scope -> phys_equal scope.borrow borrow)
  in
  let run_preparation = Option.bind legacy_scope ~f:callback_run_preparation in
  let%bind encoded_snapshot, queue_plan =
    match legacy_scope with
    | Some scope ->
      let%bind metadata = Moderator_checkpoint.queue t.state.moderator in
      let%bind metadata =
        Result.of_option
          metadata
          ~error:(error Internal_error "legacy callback queue metadata disappeared")
      in
      let%bind plan =
        Legacy_queue_checkpoint.prepare
          ~metadata
          ~before:borrow.before
          ~snapshot
          ~consumption:
            (match borrow.kind with
             | Ordinary -> Keep
             | Queued -> Consume_head)
          ~completed
          ~origin:scope.origin
      in
      let%map encoded =
        Moderator_checkpoint.update_with_queue
          t.state.moderator
          ~snapshot
          ~queue:(Legacy_queue_checkpoint.metadata plan)
      in
      encoded, Some plan
    | None -> Ok (Runtime_builder.encode_moderator_snapshot snapshot, None)
  in
  let owner = Agent_protocol.Job.Moderator_event borrow.receipt.context.id in
  let%bind job_deltas, job_payloads = staged_work_changes t owner in
  let%bind run_change = prepared_run_delta t borrow completed requests job_deltas in
  let%bind staged =
    match legacy_scope, queue_plan with
    | Some scope, Some plan ->
      (match scope.custody with
       | Unavailable -> Ok Legacy_queue_custody.empty
       | Admitted parent ->
         Legacy_queue_custody.stage
           ~manager:scope.manager
           ~source:scope.source
           ~entries:(Legacy_queue_checkpoint.appended plan)
           ~parent)
    | None, None -> Ok Legacy_queue_custody.empty
    | Some _, None | None, Some _ ->
      Error (error Internal_error "legacy checkpoint plan lost its physical scope")
  in
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () -> if not !acknowledged then Legacy_queue_custody.retire staged)
    ~f:(fun () ->
      let%bind inventory =
        match legacy_scope, queue_plan with
        | Some scope, Some plan ->
          Legacy_queue_custody.candidate
            t.legacy_queue_custody
            ~manager:scope.manager
            ~metadata:(Legacy_queue_checkpoint.metadata plan)
            ~staged
        | None, None -> Ok t.legacy_queue_custody
        | Some _, None | None, Some _ ->
          Error (error Internal_error "legacy checkpoint plan lost its physical scope")
      in
      let%bind admission = event_agent_admission t borrow in
      let custody = Option.bind admission ~f:Execution_custody.Admission.custody in
      with_selected_intent_agent_custody
        t
        owner
        (Event_intent completed.context.id)
        ~pending:(Observation_follow_up.pending_event completed)
        custody
        (fun ~job_custody ~check_jobs ~acknowledge ->
           let%bind prepared =
             prepare_transition
               ?run_preparation
               t
               ~delta:
                 (Session_delta.Batch
                    (admission_deltas
                     @ [ Session_delta.Moderator_execution_changed completed
                       ; Moderator_changed (Some encoded_snapshot)
                       ]
                     @ job_deltas
                     @ run_change.deltas))
               ~payloads:
                 (job_payloads
                  @ List.map notifications ~f:(fun message ->
                    Agent_protocol.Event.Durable.Payload.Moderator_notification
                      (`Object
                          [ "message", `String message
                          ; ( "moderator_execution_id"
                            , Agent_protocol.Id.Moderator_execution.to_json
                                borrow.receipt.context.id )
                          ])))
           in
           let%bind initialization_acknowledgement =
             match legacy_scope, queue_plan with
             | Some scope, Some plan ->
               (match scope.constructor_scope with
                | None -> Ok None
                | Some constructor ->
                  let%bind resource =
                    Initialization_scope.Resource.select
                      constructor.resource
                      prepared.state
                      ~manager:scope.manager
                      ~source:scope.source
                      ~ordinals:
                        (List.map
                           (Legacy_queue_checkpoint.appended plan)
                           ~f:Moderator_queue.Entry.ordinal)
                      ~schedules:[]
                  in
                  let selected_jobs =
                    List.filter_map job_deltas ~f:(function
                      | Session_delta.Job_changed job -> Some job.id
                      | _ -> None)
                  in
                  let jobs =
                    List.dedup_and_sort
                      (constructor.jobs @ selected_jobs)
                      ~compare:Agent_protocol.Id.Job.compare
                  in
                  let acknowledgement =
                    Initialization_scope.Acknowledgement.prepare
                      ~callback:(Settled completed.context.id)
                      ~resource
                      ~jobs
                      constructor
                      prepared.state
                  in
                  let%map () =
                    validate_initialization_projection
                      (Initialization_scope.Acknowledgement.expected acknowledgement)
                      prepared.state
                  in
                  Some acknowledgement)
             | None, None -> Ok None
             | Some _, None | None, Some _ ->
               Error (error Internal_error "constructor queue plan lost its callback")
           in
           let%bind completion_plan =
             prepare_completion_plan t prepared run_change.completion_sample
           in
           let%bind () = queued_event_can_commit t borrow in
           let%bind () =
             match legacy_scope with
             | None -> Ok ()
             | Some scope -> check_callback_current t scope
           in
           (* The checkpoint and each child loan become visible together before
             publication can yield. A post-ACK publication exception must not
             retire acknowledged children or make this callback replayable. *)
           Eio.Cancel.protect (fun () ->
             let%bind () = check_jobs () in
             let%bind () =
               Option.value_map
                 initialization_acknowledgement
                 ~default:(Ok ())
                 ~f:Initialization_scope.Acknowledgement.check_basis
             in
             let%bind () = persist_transition t prepared in
             install_committed_transition
               ?run_preparation
               ~job_custody
               ~legacy_queue_custody:inventory
               ?initialization_acknowledgement
               t
               prepared;
             Option.iter
               completion_plan
               ~f:(Recurrence_runtime.install_completions t.recurrence_runtime);
             borrow.committed <- true;
             Option.iter legacy_scope ~f:(fun scope ->
               scope.Callback_scope.committed_checkpoint <- Some prepared.state.moderator);
             acknowledged := true;
             acknowledge ();
             publish_committed_transition t prepared;
             Ok ())))
;;

(* Cleanup owns the already executed receipt, not admission of another effect.
   In particular it must not call an expired Run-preparation authorization. A
   foreign control/source change still rejects instead of entering the scope's
   acknowledged projection. Ordinary event settlement retains its existing path. *)
let commit_event_failure ?completion_sample t borrow ~delta ~payloads =
  let open Result.Let_syntax in
  match constructor_scope_of_borrow t borrow with
  | None -> transition ?completion_sample t ~delta ~payloads |> Result.map ~f:ignore
  | Some constructor ->
    let%bind () = validate_initialization_owner t constructor in
    let%bind () =
      match t.initialization_scope, constructor.callback with
      | Some active, Claimed id
        when phys_equal active constructor
             && Agent_protocol.Id.Moderator_execution.equal id borrow.receipt.context.id
        -> Ok ()
      | (Some _ | None), (Unclaimed | Claimed _ | Settled _) ->
        Error (error Conflict "constructor no longer owns this failed receipt")
    in
    let%bind () =
      validate_initialization_projection
        (Initialization_scope.Acknowledged.apply
           constructor.acknowledged
           constructor.expected)
        t.state
    in
    let%bind prepared = prepare_transition t ~delta ~payloads in
    let%bind completion_plan = prepare_completion_plan t prepared completion_sample in
    let acknowledgement =
      Initialization_scope.Acknowledgement.prepare
        ~callback:(Settled borrow.receipt.context.id)
        constructor
        prepared.state
    in
    Eio.Cancel.protect (fun () ->
      let%bind () = Initialization_scope.Acknowledgement.check_basis acknowledgement in
      let%bind () = persist_transition t prepared in
      (* No preparation is supplied: the existing ACK installer closes it rather
         than renewing it. This records only the truthful terminal receipt. *)
      install_committed_transition
        ~initialization_acknowledgement:acknowledgement
        t
        prepared;
      Option.iter
        completion_plan
        ~f:(Recurrence_runtime.install_completions t.recurrence_runtime);
      publish_committed_transition t prepared;
      Ok ())
;;

let finish_queued_event t borrow interrupted =
  let open Result.Let_syntax in
  let%bind () = validate_queued_event_borrow t borrow in
  let run_scope = Option.map borrow.run_actions ~f:Run_action_service.scope in
  Option.iter borrow.run_actions ~f:Run_action_service.close;
  borrow.run_actions <- None;
  borrow.callback_active <- false;
  Atomic.set borrow.authority_live false;
  abort_staged_work t ~owner:(Moderator_event borrow.receipt.context.id);
  borrow.cancel <- None;
  let children =
    List.filter t.invocation_executions ~f:(event_execution_owned_by borrow)
  in
  let permissions, permission_deltas, permission_payloads =
    cleanup_invocation_permissions
      t
      (List.map children ~f:(fun child -> child.dispatched.context.id))
  in
  let%bind children_deltas =
    List.map children ~f:(fun execution ->
      execution.accepts_children <- false;
      Atomic.set execution.authority_live false;
      abort_staged_work t ~owner:(Invocation execution.dispatched.context.id);
      Agent_protocol.Invocation.cancel
        execution.dispatched
        ~reason:"event exited before recording its native outcome"
      |> Result.map ~f:(fun invocation -> Session_delta.Invocation_changed invocation))
    |> Result.all
  in
  let%bind () =
    match borrow.committed, borrow.retirement_reason with
    | true, _ | false, Some _ -> Ok ()
    | false, None ->
      let%bind terminal =
        match interrupted || borrow.cancel_requested with
        | true ->
          Agent_protocol.Moderator_execution.interrupt
            borrow.receipt
            ~reason:"queued moderator handler interrupted before checkpoint commit"
        | false ->
          Agent_protocol.Moderator_execution.fail
            borrow.receipt
            { code = "event.handler_failed"
            ; message =
                "queued moderator handler exited without committing its checkpoint"
            ; retryable = false
            ; details = `Null
            }
      in
      let%bind run_change =
        match run_scope with
        | None -> Ok Prepared_run_delta.empty
        | Some scope ->
          let now, completion_sample = completion_clock_pair t in
          let%map deltas =
            Run_transition.fail_callback
              t.state
              ~scope
              ~executing:borrow.receipt
              ~completed:terminal
              ~now
          in
          Prepared_run_delta.{ deltas; completion_sample = Some completion_sample }
      in
      commit_event_failure
        ?completion_sample:run_change.completion_sample
        t
        borrow
        ~delta:
          (Session_delta.Batch
             (children_deltas
              @ permission_deltas
              @ [ Session_delta.Moderator_execution_changed terminal ]
              @ run_change.deltas))
        ~payloads:permission_payloads
  in
  resolve_cleaned_permission_waiters t permissions;
  Option.iter
    (Queued_agent_admission.admission borrow.agent_admission)
    ~f:retire_agent_admission;
  (match t.legacy_callback_scope with
   | Some scope when phys_equal scope.borrow borrow ->
     (match scope.custody with
      | Unavailable -> ()
      | Admitted admission -> retire_agent_admission admission);
     t.legacy_callback_scope <- None
   | None | Some _ -> ());
  t.queued_event_borrow <- None;
  t.invocation_executions
  <- List.filter t.invocation_executions ~f:(fun execution ->
       not (event_execution_owned_by borrow execution));
  t.idle_moderator_borrowed <- false;
  Ok ()
;;

let observation_owner_available t operation_id =
  match operation_id with
  | Some operation_id -> Result.map (running_operation t operation_id) ~f:ignore
  | None ->
    if idle_moderator_eligible t
    then Ok ()
    else Error (error Conflict "session is not available for idle observation")
;;

let validate_installed_observer t observer =
  let open Result.Let_syntax in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match installed, observer with
  | Some installed, Some observer
    when Agent_protocol.Invocation.equal_observer installed observer -> Ok ()
  | _ -> Error (error Conflict "observation source is not the installed moderator")
;;

let claim_moderator_observation t operation_id invocation_id =
  let open Result.Let_syntax in
  let%bind () = observation_owner_available t operation_id in
  let%bind moderator_halted =
    Runtime_builder.moderator_snapshot_is_halted t.state.moderator
  in
  let%bind () =
    match
      t.idle_moderator_borrowed, t.moderator_borrow, t.state.halted || moderator_halted
    with
    | false, None, false -> Ok ()
    | _ -> Error (error Conflict "moderator is borrowed or halted")
  in
  let%bind invocation =
    match
      List.find t.state.invocations ~f:(fun invocation ->
        Agent_protocol.Id.Invocation.equal invocation.context.id invocation_id)
    with
    | Some invocation -> Ok invocation
    | None -> Error (error Invalid_state "observation invocation is not retained")
  in
  let%bind () =
    validate_installed_observer
      t
      (Option.map invocation.observation ~f:(fun observation -> observation.observer))
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () =
    match invocation.context.parent_invocation with
    | Some parent
      when List.exists t.invocation_executions ~f:(fun execution ->
             Agent_protocol.Id.Invocation.equal execution.dispatched.context.id parent) ->
      Error (error Conflict "observation parent is still executing")
    | _ -> Ok ()
  in
  let revision = t.state.counters.revision in
  let%bind source = observation_source t in
  let key = Invocation_intent invocation.context.id in
  let entry =
    List.find t.intent_agent_custody ~f:(fun entry ->
      equal_agent_intent_key entry.key key)
  in
  let%bind original =
    match Option.bind entry ~f:(fun entry -> entry.observation) with
    | Some observation -> Ok observation
    | None ->
      let%bind observation =
        Observation_continuation.stage_completed invocation ~source ~parent:Unavailable
      in
      Result.of_option
        observation
        ~error:(error Internal_error "selected observation has no completed result")
  in
  let%bind observing = Agent_protocol.Invocation.claim_observation invocation in
  let%bind observation =
    Observation_continuation.stage_claim original ~source ~previous:invocation ~observing
  in
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () ->
      if not !acknowledged then Observation_continuation.retire observation)
    ~f:(fun () ->
      let%bind prepared =
        prepare_transition
          t
          ~delta:(Session_delta.Invocation_changed observing)
          ~payloads:[]
      in
      let%bind current_source = observation_source t in
      let%bind () = observation_owner_available t operation_id in
      let%bind () =
        if
          Int64.equal revision t.state.counters.revision
          && Option.equal Agent_protocol.Run_source.equal source current_source
          && List.exists
               t.state.invocations
               ~f:(Agent_protocol.Invocation.equal invocation)
          && Option.equal
               phys_equal
               entry
               (List.find t.intent_agent_custody ~f:(fun entry ->
                  equal_agent_intent_key entry.key key))
        then Ok ()
        else Error (error Conflict "observation claim basis changed before commit")
      in
      let%bind () = persist_transition t prepared in
      install_committed_transition t prepared;
      (* Only this component is consumed. The same result's handler follow-up
         custody, if any, has an independent lifetime. *)
      t.intent_agent_custody
      <- List.filter_map t.intent_agent_custody ~f:(fun current ->
           if equal_agent_intent_key current.key key
           then (
             Option.iter current.observation ~f:Observation_continuation.retire;
             if Observation_follow_up.pending invocation
             then Some { current with observation = None }
             else None)
           else Some current);
      let borrow =
        { operation_id
        ; job_scope = None
        ; kind = Observation
        ; observation = Some observation
        ; invocation = observing
        ; committed = false
        ; accepts_children = Option.is_some operation_id
        ; callback_finished = false
        ; authority_live = Atomic.make true
        ; cancel = None
        ; cancel_requested = false
        }
      in
      t.moderator_borrow <- Some borrow;
      t.idle_moderator_borrowed <- Option.is_none operation_id;
      acknowledged := true;
      publish_committed_transition t prepared;
      Ok borrow)
;;

let claim_next_moderator_observation t operation_id observer =
  let open Result.Let_syntax in
  let%bind () = observation_owner_available t operation_id in
  let%bind () = validate_installed_observer t (Some observer) in
  let%bind moderator_halted =
    Runtime_builder.moderator_snapshot_is_halted t.state.moderator
  in
  let%bind () =
    match t.idle_moderator_borrowed, t.moderator_borrow with
    | false, None -> Ok ()
    | _ -> Error (error Conflict "moderator is already borrowed")
  in
  let eligible (invocation : Agent_protocol.Invocation.t) =
    (not (t.state.halted || moderator_halted))
    && invocation.context.generation = t.state.identity.generation
    && Agent_protocol.Id.Session.equal
         invocation.context.session_id
         t.state.identity.session_id
    && (match invocation.observation, invocation.status with
        | Some { status = Awaiting; observer = owner }, (Resolved _ | Published _) ->
          Agent_protocol.Invocation.equal_observer owner observer
        | _ -> false)
    && not
         (Option.exists invocation.context.parent_invocation ~f:(fun parent ->
            List.exists t.invocation_executions ~f:(fun execution ->
              Agent_protocol.Id.Invocation.equal execution.dispatched.context.id parent)))
  in
  let compare (a : Agent_protocol.Invocation.t) (b : Agent_protocol.Invocation.t) =
    match Agent_protocol.Timestamp.compare a.context.created_at b.context.created_at with
    | 0 -> Agent_protocol.Id.Invocation.compare a.context.id b.context.id
    | order -> order
  in
  let selected =
    List.fold t.state.invocations ~init:None ~f:(fun selected candidate ->
      match eligible candidate, selected with
      | false, _ -> selected
      | true, None -> Some candidate
      | true, Some previous when compare candidate previous < 0 -> Some candidate
      | true, Some _ -> selected)
  in
  match selected with
  | None -> Ok None
  | Some invocation ->
    claim_moderator_observation t operation_id invocation.context.id
    |> Result.map ~f:Option.some
;;

let claim_job_moderator t scope invocation =
  let open Result.Let_syntax in
  let%bind () = job_scope_can_execute t scope in
  let%bind () =
    match t.idle_moderator_borrowed, t.moderator_borrow with
    | false, None -> Ok ()
    | _ -> Error (error Conflict "moderator is already borrowed")
  in
  let%bind () = active_script_parent t ~owns:(job_execution_owned_by scope) invocation in
  let%bind origin =
    invocation_origin t invocation ~fallback:(fun () -> Ok scope.job.inference_origin)
  in
  let%bind invocation, admission = invocation_admission_deltas t invocation ~origin in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let borrow =
    { operation_id = None
    ; job_scope = Some scope
    ; kind = Invocation
    ; observation = None
    ; invocation = dispatched
    ; committed = false
    ; accepts_children = true
    ; callback_finished = false
    ; authority_live = Atomic.make true
    ; cancel = None
    ; cancel_requested = false
    }
  in
  t.moderator_borrow <- Some borrow;
  t.idle_moderator_borrowed <- true;
  Ok borrow
;;

let validate_moderator_borrow t borrow =
  match t.moderator_borrow with
  | Some current when phys_equal current borrow ->
    let open Result.Let_syntax in
    let%bind () =
      match borrow.job_scope, borrow.operation_id with
      | Some scope, None -> Result.map (job_scope_current t scope) ~f:ignore
      | None, Some operation_id -> Result.map (current_operation t operation_id) ~f:ignore
      | None, None
        when t.idle_moderator_borrowed && Option.is_none t.state.active_operation -> Ok ()
      | _ -> Error (error Conflict "moderator no longer owns its execution scope")
    in
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      borrow.invocation.context.session_id
      borrow.invocation.context.generation
  | _ -> Error (error Conflict "moderator borrow is no longer owned by this callback")
;;

let invocation_execution_owned_by borrow execution =
  match execution.owner with
  | Invocation_moderator owner -> phys_equal owner borrow
  | Foreground _ | Event_moderator _ | Background_job _ -> false
;;

let background_owner_active t owner =
  let open Result.Let_syntax in
  let moderator (borrow : moderator_borrow) =
    let%bind () = validate_moderator_borrow t borrow in
    match borrow.callback_finished || borrow.committed || borrow.cancel_requested with
    | true -> Error (error Conflict "background launch owner has finished")
    | false ->
      (match borrow.job_scope, borrow.operation_id with
       | Some scope, None -> job_scope_can_execute t scope
       | None, Some id -> Result.map (running_operation t id) ~f:ignore
       | None, None ->
         (match
            t.state.lifecycle.desired, t.state.lifecycle.observed, t.state.failure
          with
          | Running, Idle, None when not t.state.halted -> Ok ()
          | _ -> Error (error Conflict "background launch owner is not running"))
       | Some _, Some _ -> Error (error Conflict "invalid background launch scope"))
  in
  let event borrow =
    let%bind () = queued_event_can_commit t borrow in
    match borrow.retirement_reason with
    | None -> Ok ()
    | Some _ -> Error (error Conflict "event retirement cannot launch background work")
  in
  match owner with
  | Agent_protocol.Job.Moderator_event id ->
    (match t.queued_event_borrow with
     | Some borrow
       when Agent_protocol.Id.Moderator_execution.equal borrow.receipt.context.id id ->
       event borrow
     | _ -> Error (error Conflict "background launch event is not executing"))
  | Invocation id ->
    (match
       List.find t.invocation_executions ~f:(fun execution ->
         execution.accepts_children
         && Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id)
     with
     | Some execution ->
       (match execution.owner with
        | Foreground id -> Result.map (running_operation t id) ~f:ignore
        | Background_job scope -> job_scope_can_execute t scope
        | Invocation_moderator borrow -> moderator borrow
        | Event_moderator borrow -> event borrow)
     | None ->
       (match t.moderator_borrow with
        | Some borrow
          when Agent_protocol.Id.Invocation.equal borrow.invocation.context.id id ->
          moderator borrow
        | _ -> Error (error Conflict "background launch invocation is not executing")))
;;

let subscription_owner_active t owner source =
  let open Result.Let_syntax in
  let%bind () = background_owner_active t owner in
  let%bind () = validate_installed_observer t (Some source) in
  match owner with
  | Agent_protocol.Job.Invocation id ->
    (match t.moderator_borrow with
     | Some borrow when Agent_protocol.Id.Invocation.equal borrow.invocation.context.id id
       ->
       Ok
         (match borrow.kind with
          | Invocation -> Some borrow.invocation
          | Observation -> None)
     | _ ->
       Error
         (error Permission_denied "subscription requires a moderator invocation borrow"))
  | Moderator_event id ->
    (match t.queued_event_borrow with
     | Some borrow
       when Agent_protocol.Id.Moderator_execution.equal borrow.receipt.context.id id
            && Agent_protocol.Invocation.equal_observer
                 borrow.receipt.context.source
                 source -> Ok None
     | _ ->
       Error (error Permission_denied "subscription requires its moderator event borrow"))
;;

let schedule_owned t source (schedule : Agent_protocol.Schedule.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      schedule.session_id
      schedule.generation
  in
  match schedule.ownership with
  | Some ownership when Agent_protocol.Invocation.equal_observer ownership.source source
    -> Ok ()
  | _ ->
    Error
      (error
         Permission_denied
         "schedule belongs to a different or unbound moderator source")
;;

let provisional_schedule t owner id =
  match Staged_schedules.find t.staged_schedules ~owner ~id with
  | Some value -> Some value
  | None ->
    List.find t.state.schedules ~f:(fun value ->
      Agent_protocol.Id.Schedule.equal value.id id)
;;

let delay_within_limit ~created_at ~due_at max_delay_ms =
  let ns = Agent_protocol.Timestamp.diff_ns due_at created_at in
  (* Compare rounded-up milliseconds, without converting the policy ceiling to
     a span or overflowing the difference between valid timestamp endpoints. *)
  let milliseconds =
    Int64.((ns / 1_000_000L) + if ns % 1_000_000L = 0L then 0L else 1L)
  in
  Int64.(ns >= 0L && milliseconds <= of_int max_delay_ms)
;;

let stage_schedule_mutation_internal
      t
      owner
      source
      previous
      (next : Agent_protocol.Schedule.t)
  =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind () = schedule_owned t source next in
  let%bind () = Agent_protocol.Schedule.validate_transition ~previous next in
  let%bind () =
    match
      Option.equal
        (fun a b ->
           Jsonaf.exactly_equal
             (Agent_protocol.Schedule.to_json a)
             (Agent_protocol.Schedule.to_json b))
        previous
        (provisional_schedule t owner next.id)
    with
    | true -> Ok ()
    | false -> Error (error Conflict "schedule changed before staging")
  in
  let%bind () =
    match previous with
    | Some _ -> Ok ()
    | None ->
      let limits = t.services.schedule_limits in
      let%bind () = Staged_schedules.validate_limits limits in
      let%bind () =
        match next.ownership with
        | Some ownership
          when Agent_protocol.Job.equal_launch_owner ownership.creator owner
               && Agent_protocol.Timestamp.compare next.created_at (t.services.now ())
                  <= 0
               && delay_within_limit
                    ~created_at:next.created_at
                    ~due_at:next.next_due_at
                    limits.max_delay_ms -> Ok ()
        | _ ->
          Error
            (error
               Permission_denied
               "schedule creation has a different owner or invalid timing")
      in
      let reserved = Staged_schedules.reservations t.staged_schedules in
      let active schedule =
        match schedule.Agent_protocol.Schedule.status with
        | Scheduled | Delivering -> true
        | _ -> false
      in
      let source_active schedule =
        active schedule
        && Option.exists schedule.ownership ~f:(fun ownership ->
          Agent_protocol.Invocation.equal_observer ownership.source source)
      in
      let%bind () =
        match
          List.count t.state.schedules ~f:active + reserved < limits.max_active
          && List.count t.state.schedules ~f:source_active + reserved
             < limits.max_per_source
          && List.length t.state.schedules + reserved < limits.max_retained
        with
        | true -> Ok ()
        | false -> Error (error Resource_limit "schedule admission capacity exhausted")
      in
      Agent_protocol.Json_codec.validate_limits
        ~max_bytes:limits.max_payload_bytes
        ~max_depth:limits.max_payload_depth
        next.payload
  in
  let%map receipt = Staged_schedules.stage t.staged_schedules ~owner ~previous ~next in
  sync_extension_clock t;
  receipt
;;

let create_script_schedule_internal t owner source delay_ms payload misfire =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind () =
    match delay_ms >= 0 && delay_ms <= t.services.schedule_limits.max_delay_ms with
    | true -> Ok ()
    | false -> Error (error Invalid_request "schedule delay exceeds host policy")
  in
  let created_at = t.services.now () in
  let monotonic_created_at = t.services.monotonic_now () in
  let%bind next_due_at = Agent_protocol.Timestamp.add_ms created_at delay_ms in
  let schedule : Agent_protocol.Schedule.t =
    { id = Agent_protocol.Id.Schedule.create ()
    ; session_id = t.state.identity.session_id
    ; generation = t.state.identity.generation
    ; payload
    ; created_at
    ; next_due_at
    ; misfire
    ; status = Scheduled
    ; delivery_count = 0
    ; last_delivery_at = None
    ; inference_provenance = None
    ; delivery_cancellation = None
    ; ownership = Some { source; creator = owner; subscription = None }
    }
  in
  let%map receipt = stage_schedule_mutation_internal t owner source None schedule in
  Extension_clock.capture
    t.extension_clock
    (Schedule schedule.id)
    ~now:monotonic_created_at
    ~created_at
    ~due_at:next_due_at;
  receipt, schedule
;;

let subscription_owned t source (subscription : Agent_protocol.Subscription.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      subscription.context.session_id
      subscription.context.generation
  in
  match subscription.context.source with
  | Some actual when Agent_protocol.Invocation.equal_observer actual source -> Ok ()
  | _ ->
    Error
      (error
         Permission_denied
         "subscription belongs to a different or unbound moderator source")
;;

let provisional_subscription t owner id =
  match Staged_subscriptions.find t.staged_subscriptions ~owner ~id with
  | Some value -> Some value
  | None -> lookup_subscription t id
;;

let subscription_parent_job t =
  Option.bind t.moderator_borrow ~f:(fun borrow ->
    Option.map borrow.job_scope ~f:(fun scope -> scope.job.id, scope.job.attempt))
;;

let stage_subscription_mutation_internal
      ?sampled_time
      t
      owner
      source
      previous
      (next : Agent_protocol.Subscription.t)
  =
  let open Result.Let_syntax in
  let%bind originating = subscription_owner_active t owner source in
  let%bind () = subscription_owned t source next in
  let%bind () =
    match
      Option.equal
        Agent_protocol.Subscription.equal
        previous
        (provisional_subscription t owner next.context.id)
    with
    | true -> Ok ()
    | false -> Error (error Conflict "subscription changed before staging")
  in
  let%bind () = Agent_protocol.Subscription.validate_transition ~previous next in
  let now, monotonic_now =
    Option.value_or_thunk sampled_time ~default:(fun () ->
      let now = t.services.now () in
      now, t.services.monotonic_now ())
  in
  let%bind () =
    match previous with
    | Some _ -> Ok ()
    | None ->
      let%bind () =
        match originating with
        | Some invocation
          when Agent_protocol.Id.Invocation.equal
                 invocation.context.id
                 next.context.invocation_id ->
          (* Origin is established by the live borrowed invocation, not wall
             timestamp ordering across a possible clock adjustment. *)
          Ok ()
        | _ ->
          Error
            (error
               Permission_denied
               "subscription creation requires its dispatched moderator tool")
      in
      let%bind () =
        match
          Option.equal
            (fun (id, attempt) (other_id, other_attempt) ->
               Agent_protocol.Id.Job.equal id other_id && Int.equal attempt other_attempt)
            next.context.parent_job
            (subscription_parent_job t)
        with
        | true -> Ok ()
        | false ->
          Error
            (error Permission_denied "subscription has a different creating job attempt")
      in
      let limits = t.services.subscription_limits in
      let reserved = Staged_subscriptions.reservations t.staged_subscriptions in
      let active =
        List.count t.state.subscriptions ~f:(fun sub -> Option.is_none sub.result)
      in
      let%bind () =
        match
          active + reserved < limits.max_active
          && List.length t.state.subscriptions + reserved < limits.max_retained
        with
        | true -> Ok ()
        | false ->
          Error (error Resource_limit "subscription admission capacity exhausted")
      in
      (match
         Agent_protocol.Timestamp.compare next.context.created_at now <= 0
         && Agent_protocol.Timestamp.compare next.context.deadline now > 0
         && delay_within_limit
              ~created_at:next.context.created_at
              ~due_at:next.context.deadline
              limits.max_lifetime_ms
       with
       | true -> Ok ()
       | false ->
         Error (error Invalid_request "subscription lifetime exceeds host policy"))
  in
  let%bind () =
    match previous with
    | None | Some { result = Some _; _ } -> Ok ()
    | Some previous ->
      let%bind due = subscription_is_due_at t previous ~now ~monotonic_now in
      let%bind () =
        match next.result with
        | None -> Ok ()
        | Some completion ->
          let latest = subscription_terminal_time_at previous completion ~now in
          (match
             Option.exists next.completed_at ~f:(fun at ->
               Agent_protocol.Timestamp.compare at latest > 0)
           with
           | true ->
             Error (error Invalid_request "subscription completion is in the future")
           | false -> Ok ())
      in
      (match due, next.result with
       | true, Some Expired | false, (None | Some (Succeeded _ | Failed _ | Cancelled _))
         -> Ok ()
       | false, Some Expired ->
         Error (error Conflict "subscription has not reached its elapsed deadline")
       | true, _ ->
         Error (error Conflict "subscription deadline has passed; record expiry"))
  in
  let%bind () =
    match next.result, next.context.completion_schema with
    | Some (Succeeded payload), Some schema ->
      let%bind schema =
        Chatmd_shell_spec.Tool_schema.compile schema
        |> Result.map_error ~f:(fun _ ->
          error Invalid_request "invalid subscription completion schema")
      in
      Chatmd_shell_spec.Tool_schema.validate schema payload
      |> Result.map_error ~f:(fun _ ->
        error Invalid_request "subscription completion schema mismatch")
    | _ -> Ok ()
  in
  let%map receipt =
    Staged_subscriptions.stage t.staged_subscriptions ~owner ~previous ~next
  in
  sync_extension_clock t;
  receipt
;;

let create_script_subscription_internal
      t
      owner
      source
      kind
      lifetime_ms
      wake
      completion_schema
  =
  let open Result.Let_syntax in
  let%bind originating = subscription_owner_active t owner source in
  let%bind invocation =
    Result.of_option
      originating
      ~error:
        (error
           Permission_denied
           "subscription creation requires a dispatched moderator tool")
  in
  let%bind () =
    match
      lifetime_ms > 0 && lifetime_ms <= t.services.subscription_limits.max_lifetime_ms
    with
    | true -> Ok ()
    | false -> Error (error Invalid_request "subscription lifetime exceeds host policy")
  in
  let created_at = t.services.now () in
  let monotonic_created_at = t.services.monotonic_now () in
  let%bind deadline = Agent_protocol.Timestamp.add_ms created_at lifetime_ms in
  let%bind subscription =
    Agent_protocol.Subscription.create
      { id = Agent_protocol.Id.Subscription.create ()
      ; session_id = invocation.context.session_id
      ; generation = invocation.context.generation
      ; invocation_id = invocation.context.id
      ; source = Some source
      ; parent_job = subscription_parent_job t
      ; kind
      ; created_at
      ; deadline
      ; completion_schema
      ; wake
      ; ingress_capability = None
      }
  in
  let%map receipt =
    stage_subscription_mutation_internal t owner source None subscription
  in
  Extension_clock.capture
    t.extension_clock
    (Subscription subscription.context.id)
    ~now:monotonic_created_at
    ~created_at
    ~due_at:deadline;
  receipt, subscription
;;

let ingress_owned t source (value : External_ingress.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      value.context.session_id
      value.context.generation
  in
  match Agent_protocol.Invocation.equal_observer source value.context.source with
  | true -> Ok ()
  | false ->
    Error
      (error Permission_denied "ingress registration belongs to another moderator source")
;;

let provisional_ingress t owner id =
  match Staged_ingress.find t.staged_ingress ~owner ~id with
  | Some _ as value -> value
  | None -> lookup_ingress t id
;;

let read_script_ingress_internal t owner source id =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind value =
    provisional_ingress t owner id
    |> Result.of_option
         ~error:(error Invalid_state "ingress registration is not retained")
  in
  let%map () = ingress_owned t source value in
  value
;;

let create_script_ingress_internal
      t
      owner
      source
      subscription_id
      expected_epoch
      namespace
      schema
  =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind subscription =
    provisional_subscription t owner subscription_id
    |> Result.of_option
         ~error:(error Invalid_state "ingress subscription is not retained")
  in
  let%bind () = subscription_owned t source subscription in
  let%bind () =
    match subscription.epoch = expected_epoch && Option.is_none subscription.result with
    | true -> Ok ()
    | false ->
      Error (error Conflict "ingress subscription is terminal or has a different epoch")
  in
  let%bind due = subscription_is_due t subscription in
  let%bind () =
    match due with
    | false -> Ok ()
    | true ->
      Error (error Conflict "ingress subscription has reached its elapsed deadline")
  in
  let%bind producer =
    t.state.identity.creating_principal
    |> Result.of_option
         ~error:
           (error
              Permission_denied
              "ingress registration requires a recorded creating principal")
  in
  let%bind next =
    External_ingress.create
      { id = Agent_protocol.Id.Capability.create ()
      ; session_id = t.state.identity.session_id
      ; generation = t.state.identity.generation
      ; subscription_id
      ; epoch = expected_epoch
      ; source
      ; producer
      ; namespace
      ; schema
      ; created_at = t.services.now ()
      ; expires_at = subscription.context.deadline
      ; limits = t.services.ingress_limits.registration
      }
      ~subscription
  in
  let%bind () =
    Staged_ingress.check_capacity
      ~limits:t.services.ingress_limits
      ~generation:t.state.identity.generation
      ~now:(t.services.now ())
      ~subscriptions:
        (t.state.subscriptions @ Staged_subscriptions.values t.staged_subscriptions)
      ~values:
        ((next :: t.state.ingress_registrations) @ Staged_ingress.values t.staged_ingress)
  in
  let%map receipt =
    Staged_ingress.stage t.staged_ingress ~owner ~previous:None ~next ~subscription
  in
  receipt, next
;;

let revoke_script_ingress_internal t owner source id reason =
  let open Result.Let_syntax in
  let%bind previous = read_script_ingress_internal t owner source id in
  let%bind subscription =
    provisional_subscription t owner previous.context.subscription_id
    |> Result.of_option
         ~error:(error Invalid_state "ingress subscription is not retained")
  in
  let%bind next = External_ingress.revoke previous ~reason in
  let%map receipt =
    Staged_ingress.stage
      t.staged_ingress
      ~owner
      ~previous:(Some previous)
      ~next
      ~subscription
  in
  receipt, next
;;

let notification_owned t source (delivery : Agent_protocol.Delivery.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      delivery.context.session_id
      delivery.context.generation
  in
  match delivery.context.ownership with
  | Some ownership when Agent_protocol.Invocation.equal_observer ownership.source source
    -> Ok ()
  | _ ->
    Error
      (error
         Permission_denied
         "notification belongs to a different or unbound moderator source")
;;

let provisional_notification t owner id =
  match Staged_notifications.find t.staged_notifications ~owner ~id with
  | Some value -> Some value
  | None -> lookup_notification t id
;;

let create_script_notification_internal
      t
      owner
      source
      (correlation : Chat_response.Notification_operations.correlation)
      completion
      wake
      disclosure_pins
  =
  let open Result.Let_syntax in
  let module P = Agent_protocol in
  let%bind _ = subscription_owner_active t owner source in
  let limits = t.services.notification_limits in
  let%bind () = P.Completion.validate completion in
  let%bind () =
    P.Json_codec.validate_limits
      ~max_bytes:limits.max_payload_bytes
      ~max_depth:limits.max_payload_depth
      (P.Completion.to_json completion)
  in
  let same_session session_id generation =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      session_id
      generation
  in
  let%bind () =
    match correlation.invocation_id with
    | None -> Ok ()
    | Some id ->
      let%bind invocation =
        Result.of_option
          (List.find t.state.invocations ~f:(fun invocation ->
             P.Id.Invocation.equal invocation.context.id id))
          ~error:(error Invalid_request "notification invocation is not retained")
      in
      same_session invocation.context.session_id invocation.context.generation
  in
  let%bind () =
    match correlation.work with
    | None -> Ok ()
    | Some (P.Invocation.Subscription id) ->
      let%bind subscription =
        Result.of_option
          (provisional_subscription t owner id)
          ~error:(error Invalid_request "notification subscription is not retained")
      in
      let%bind () = subscription_owned t source subscription in
      let%bind () =
        match correlation.invocation_id with
        | Some id when P.Id.Invocation.equal id subscription.context.invocation_id ->
          Ok ()
        | _ ->
          Error
            (error
               Permission_denied
               "notification must retain its subscription's invocation")
      in
      (match subscription.result with
       | Some actual when P.Completion.equal actual completion -> Ok ()
       | _ ->
         Error
           (error
              Invalid_request
              "notification differs from its terminal subscription result"))
    | Some (Job id) ->
      let%bind job =
        Result.of_option
          (List.find t.state.jobs ~f:(fun job -> P.Id.Job.equal job.id id))
          ~error:(error Invalid_request "notification job is not retained")
      in
      let%bind () = same_session job.session_id job.generation in
      let%bind result = P.Job.terminal_result job in
      let%bind result =
        Result.of_option
          result
          ~error:(error Invalid_request "notification job is not terminal")
      in
      let%bind matches = P.Stored_completion.matches result completion in
      (match matches with
       | true -> Ok ()
       | false ->
         Error (error Invalid_request "notification differs from its terminal job result"))
  in
  let retained =
    t.state.deliveries @ Staged_notifications.values t.staged_notifications
  in
  let%bind () =
    match correlation.work with
    | None -> Ok ()
    | Some work ->
      (match
         List.exists retained ~f:(fun delivery ->
           Option.exists delivery.context.work ~f:(fun actual ->
             P.Invocation.compare_work actual work = 0))
       with
       | true -> Error (error Conflict "terminal work already has a delivery owner")
       | false -> Ok ())
  in
  let pending delivery =
    match delivery.P.Delivery.status with
    | Pending -> true
    | Committed _ | Failed _ -> false
  in
  let same_source delivery =
    Option.exists delivery.P.Delivery.context.ownership ~f:(fun ownership ->
      P.Invocation.equal_observer ownership.source source)
  in
  let%bind () =
    match
      List.count retained ~f:pending < limits.max_pending
      && List.count retained ~f:(fun delivery -> pending delivery && same_source delivery)
         < limits.max_per_source
      && List.length retained < limits.max_retained
    with
    | true -> Ok ()
    | false -> Error (error Resource_limit "notification admission capacity exhausted")
  in
  let%bind subscription_binding =
    match correlation.work with
    | None | Some (Job _) -> Ok None
    | Some (Subscription id) ->
      let%bind subscription =
        Result.of_option
          (provisional_subscription t owner id)
          ~error:
            (error Invalid_state "notification subscription disappeared before admission")
      in
      P.Delivery.Subscription_binding.create ~subscription_id:id ~epoch:subscription.epoch
      |> Result.map ~f:Option.some
  in
  let%bind delivery =
    P.Delivery.create
      ?disclosure_pins
      { id = P.Id.Delivery.create ()
      ; session_id = t.state.identity.session_id
      ; generation = t.state.identity.generation
      ; invocation_id = correlation.invocation_id
      ; work = correlation.work
      ; correlation = correlation.key
      ; source = Moderator
      ; completion
      ; wake
      ; created_at = t.services.now ()
      ; ownership = Some { source; creator = owner; subscription_binding }
      }
  in
  let%map receipt =
    Staged_notifications.stage t.staged_notifications ~owner ~previous:None ~next:delivery
  in
  receipt, delivery
;;

let finish_script_subscription_internal t owner source id expected_epoch completion =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind previous =
    Result.of_option
      (provisional_subscription t owner id)
      ~error:(error Invalid_state "subscription is not retained")
  in
  let%bind () = subscription_owned t source previous in
  let now = t.services.now () in
  let monotonic_now = t.services.monotonic_now () in
  let%bind completion =
    match previous.result with
    | Some winner -> Ok winner
    | None ->
      let%map due = subscription_is_due_at t previous ~now ~monotonic_now in
      (match due with
       | true -> Agent_protocol.Completion.Expired
       | false -> completion)
  in
  let%bind next, _ =
    Agent_protocol.Subscription.finish
      previous
      ~expected_epoch
      ~now:(subscription_terminal_time_at previous completion ~now)
      completion
  in
  let%map receipt =
    stage_subscription_mutation_internal
      ~sampled_time:(now, monotonic_now)
      t
      owner
      source
      (Some previous)
      next
  in
  receipt, next
;;

let derive_background_launch t owner =
  let open Result.Let_syntax in
  let%bind () = background_owner_active t owner in
  let%bind launch =
    Job_launch.derive
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      ~invocations:t.state.invocations
      ~events:t.state.moderator_executions
      ~jobs:t.state.jobs
      ~owner
  in
  let%map moderator_source =
    let%bind observer = Runtime_builder.moderator_snapshot_observer t.state.moderator in
    match observer with
    | Some source ->
      (match subscription_owner_active t owner source with
       | Ok _ -> Ok (Some source)
       | Error _ -> Ok None)
    | None -> Ok None
  in
  { launch with moderator_source }
;;

let claim_idle_invocation t borrow (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind () = validate_moderator_borrow t borrow in
  let%bind () =
    match borrow.invocation.context.deadline, invocation.context.deadline with
    | None, _ -> Ok ()
    | Some parent, Some child when Agent_protocol.Timestamp.compare child parent <= 0 ->
      Ok ()
    | _ -> Error (error Conflict "moderator child cannot extend its parent's deadline")
  in
  let%bind () =
    match
      ( borrow.job_scope
      , borrow.operation_id
      , t.state.lifecycle.desired
      , t.state.lifecycle.observed )
    with
    | Some scope, None, _, _
      when borrow.accepts_children && not (borrow.committed || borrow.cancel_requested) ->
      job_scope_can_execute t scope
    | None, None, Running, Idle
      when borrow.accepts_children
           && (not (borrow.committed || borrow.cancel_requested))
           && (not t.state.halted)
           && Option.is_none t.state.failure -> Ok ()
    | _ -> Error (error Conflict "idle moderator cannot admit another invocation")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () =
    match
      ( invocation.context.origin
      , invocation.context.parent_invocation
      , invocation.context.parent_job
      , invocation.observation
      , borrow.invocation.observation )
    with
    | Moderator, Some parent, None, Some child, Some observed
      when Agent_protocol.Id.Invocation.equal parent borrow.invocation.context.id
           && Agent_protocol.Invocation.equal_observer child.observer observed.observer ->
      Ok ()
    | Script, Some _, None, _, Some observed ->
      let%bind () =
        active_script_parent t ~owns:(invocation_execution_owned_by borrow) invocation
      in
      compatible_script_observer observed.observer invocation
    | _ -> Error (error Conflict "idle invocation must belong to its observing moderator")
  in
  let%bind origin =
    invocation_origin t invocation ~fallback:(fun () ->
      Ok borrow.invocation.inference_origin)
  in
  let%bind invocation, admission = invocation_admission_deltas t invocation ~origin in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let execution =
    { owner = Invocation_moderator borrow
    ; dispatched
    ; accepts_children = true
    ; authority_live = Atomic.make true
    }
  in
  t.invocation_executions <- execution :: t.invocation_executions;
  Ok execution
;;

let uncommitted_borrow_delta t (borrow : moderator_borrow) failure =
  if borrow.committed
  then Ok (Session_delta.Batch [])
  else
    let open Result.Let_syntax in
    let outcome =
      Option.value
        failure
        ~default:
          (Agent_protocol.Invocation.Fail
             { code = "invocation.unhandled"
             ; message = "moderator callback returned without committing a resolution"
             ; retryable = false
             ; details = `Null
             })
    in
    let outcome =
      match borrow.operation_id, t.state.active_operation with
      | Some id, Some { id = current; state = Cancelling; _ }
        when Agent_protocol.Id.Operation.equal id current ->
        Agent_protocol.Invocation.Cancelled "operation cancelled"
      | _ -> outcome
    in
    let%map resolved =
      match borrow.kind with
      | Invocation ->
        Agent_protocol.Invocation.resolve
          borrow.invocation
          ~session_id:t.state.identity.session_id
          ~generation:t.state.identity.generation
          outcome
      | Observation ->
        Agent_protocol.Invocation.fail_observation
          borrow.invocation
          ~reason:
            (match outcome with
             | Cancelled _ -> "observation handler cancelled before acknowledgement"
             | _ -> "observation handler failed before acknowledgement")
    in
    Session_delta.Invocation_changed resolved
;;

let finish_moderator_invocation t borrow failure =
  let open Result.Let_syntax in
  let%bind () = validate_moderator_borrow t borrow in
  borrow.callback_finished <- true;
  abort_staged_work t ~owner:(Invocation borrow.invocation.context.id);
  borrow.cancel <- None;
  borrow.accepts_children <- false;
  Atomic.set borrow.authority_live false;
  let was_committed = borrow.committed in
  let unfinished =
    List.filter t.invocation_executions ~f:(invocation_execution_owned_by borrow)
  in
  let permissions, permission_deltas, permission_payloads =
    cleanup_invocation_permissions
      t
      (borrow.invocation.context.id
       :: List.map unfinished ~f:(fun child -> child.dispatched.context.id))
  in
  let%bind children =
    List.map unfinished ~f:(fun execution ->
      execution.accepts_children <- false;
      Atomic.set execution.authority_live false;
      abort_staged_work t ~owner:(Invocation execution.dispatched.context.id);
      Agent_protocol.Invocation.cancel
        execution.dispatched
        ~reason:
          (match borrow.operation_id with
           | None -> "idle moderator exited before recording the invocation outcome"
           | Some _ -> "moderator exited before recording the invocation outcome")
      |> Result.map ~f:(fun invocation -> Session_delta.Invocation_changed invocation))
    |> Result.all
  in
  let%bind delta = uncommitted_borrow_delta t borrow failure in
  let%bind () =
    if was_committed && List.is_empty children && List.is_empty permission_deltas
    then Ok ()
    else
      Result.map
        (transition
           t
           ~delta:(Session_delta.Batch (children @ permission_deltas @ [ delta ]))
           ~payloads:permission_payloads)
        ~f:(fun _ -> ())
  in
  resolve_cleaned_permission_waiters t permissions;
  t.invocation_executions
  <- List.filter t.invocation_executions ~f:(fun execution ->
       not (invocation_execution_owned_by borrow execution));
  Option.iter borrow.observation ~f:Observation_continuation.retire;
  t.moderator_borrow <- None;
  (match borrow.operation_id with
   | None -> t.idle_moderator_borrowed <- false
   | Some _ -> ());
  if (not was_committed) && Option.is_none failure
  then
    Error
      (error Invalid_state "moderator callback returned without committing a resolution")
  else Ok ()
;;

let operation_state t (operation : Agent_protocol.Operation.t) state =
  { operation with state; updated_at = t.services.now () }
;;

let summary_with_operation t operation =
  Session_state.summary { t.state with active_operation = Some operation }
;;

let lifecycle_for_operation t operation_id =
  Session_state.Lifecycle.
    { desired = t.state.lifecycle.desired; observed = Running_turn operation_id }
;;

let lifecycle_for_compaction t operation_id =
  Session_state.Lifecycle.
    { desired = t.state.lifecycle.desired; observed = Compacting operation_id }
;;

let commit_worker_entry t operation_id entry =
  let open Result.Let_syntax in
  let%bind _ = current_operation t operation_id in
  let protocol_entry =
    History_codec.canonical_encoder ~previous:t.state.conversation.canonical_history entry
  in
  match
    List.find t.state.conversation.canonical_history ~f:(fun existing ->
      History_entry.Id.equal existing.id protocol_entry.id)
  with
  | None ->
    let%map _ = append_history t [ protocol_entry ] in
    ()
  | Some existing ->
    if
      Sexp.equal
        ([%sexp_of: Agent_protocol.History.entry] existing)
        ([%sexp_of: Agent_protocol.History.entry] protocol_entry)
    then Ok ()
    else Error (error Conflict "history ID was committed with a different payload")
;;

let prepare_authoring_input t operation_id materialization history effective =
  let module M = Chat_response.Authoring_materialization in
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind () =
    let committed = t.state.conversation.canonical_history in
    let supplied = History_codec.all_to_protocol ~previous:committed history in
    match List.equal equal_agent_entry committed supplied with
    | true -> Ok ()
    | false -> Error (error Conflict "authoring input uses stale canonical history")
  in
  let%bind () =
    match
      String.equal
        (M.scope materialization)
        (M.session_scope
           ~session_id:t.state.identity.session_id
           ~generation:t.state.identity.generation)
    with
    | true -> Ok ()
    | false ->
      Error (error Conflict "authoring context belongs to a different session generation")
  in
  let canonical =
    History_codec.canonical_encoder ~previous:t.state.conversation.canonical_history
  in
  let effective =
    List.map effective ~f:(fun value ->
      let module E = Chat_response.Moderation.Effective_entry in
      match value.E.provenance with
      | Canonical -> canonical value.entry
      | Moderator_inserted _ ->
        History_codec.to_protocol ~provenance:Moderator_inserted value.entry
      | Moderator_replacement { target_id; _ } ->
        { (canonical value.entry) with provenance = Moderator_replaced target_id })
  in
  let%bind references = Session_state.authoring_references t.state in
  let%bind messages =
    M.refresh
      materialization
      ~known:(Chat_response.Authoring_reference_index.receipts references)
      ~effective
  in
  let publication = Chat_response.Authoring_publication.capture materialization in
  let context_deltas =
    match
      Option.equal
        Chat_response.Authoring_publication.equal_context
        t.state.conversation.authoring_publication
        (Some publication)
    with
    | true -> []
    | false -> [ Session_delta.Authoring_publication_changed publication ]
  in
  match messages, context_deltas with
  | [], [] -> Ok []
  | _ ->
    let first =
      Int64.max
        t.state.conversation.next_history_sequence
        t.state.conversation.reserved_history_through
    in
    let count = List.length messages in
    let%bind () =
      match Int64.(first >= 0L && first <= of_int Int.(max_value - count)) with
      | true -> Ok ()
      | false -> Error (error Invalid_state "authoring history sequence overflow")
    in
    let%bind entries =
      List.mapi messages ~f:(fun index message ->
        History_entry.Id.create
          ~namespace:(Agent_protocol.Id.Session.to_string t.state.identity.session_id)
          ~sequence:(Int64.to_int_exn first + index)
        |> Result.map_error ~f:(error Invalid_state)
        |> Result.map ~f:(fun id -> M.entry message ~id))
      |> Result.all
    in
    let%bind returned = History_codec.all_of_protocol entries in
    let%map _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             (context_deltas
              @
              match entries with
              | [] -> []
              | _ ->
                [ History_block_reserved Int64.(first + of_int count)
                ; Canonical_entries_appended entries
                ]))
        ~payloads:
          (match entries with
           | [] -> []
           | _ -> [ Agent_protocol.Event.Durable.Payload.History_appended entries ])
    in
    returned
;;

let publish_invocation_output t operation_id invocation_id entry =
  let open Result.Let_syntax in
  (* A completed tool may publish while a sibling waits for permission. Publishing
     its already-resolved result does not authorize or execute another tool. *)
  let%bind _ =
    running_operation ~allow_stopping:true ~allow_waiting_permission:true t operation_id
  in
  let%bind invocation =
    match
      List.find t.state.invocations ~f:(fun i ->
        Agent_protocol.Id.Invocation.compare i.context.id invocation_id = 0)
    with
    | Some i -> Ok i
    | None -> Error (error Invalid_state "invocation has not been admitted")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () = Invocation_history.validate_output invocation entry in
  let%bind published =
    Agent_protocol.Invocation.publish_with_history
      invocation
      ~output_entry_id:(History_entry.id entry)
  in
  let raw_entry =
    History_codec.canonical_encoder ~previous:t.state.conversation.canonical_history entry
  in
  let existing =
    List.find t.state.conversation.canonical_history ~f:(fun e ->
      History_entry.Id.equal e.id raw_entry.id)
  in
  let%bind protocol_entry =
    match existing with
    | Some entry ->
      let%bind () =
        Chat_response.Authoring_publication.validate_output invocation entry
      in
      (match equal_agent_entry entry { raw_entry with provenance = entry.provenance } with
       | true -> Ok entry
       | false ->
         Error (error Conflict "output occurrence has a different canonical payload"))
    | None ->
      Chat_response.Authoring_publication.encode
        ~context:t.state.conversation.authoring_publication
        invocation
        raw_entry
  in
  match invocation.status with
  | Published _ -> Ok ()
  | _ ->
    let entries = if Option.is_some existing then [] else [ protocol_entry ] in
    let payloads =
      if List.is_empty entries
      then []
      else [ Agent_protocol.Event.Durable.Payload.History_appended entries ]
    in
    let%map _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             [ Canonical_entries_appended entries; Invocation_changed published ])
        ~payloads
    in
    ()
;;

let commit_worker_moderator t operation_id moderator =
  let open Result.Let_syntax in
  let%bind _ = running_operation ~allow_stopping:true t operation_id in
  if moderator_is_borrowed t
  then Error (error Conflict "moderator invocation owns the moderator checkpoint")
  else (
    match t.foreground_moderator with
    | Some (managed_operation, observer) ->
      let%bind installed =
        Runtime_builder.moderator_snapshot_observer t.state.moderator
      in
      let%bind () =
        if
          Agent_protocol.Id.Operation.equal managed_operation operation_id
          && Option.equal
               Agent_protocol.Invocation.equal_observer
               installed
               (Some observer)
        then Ok ()
        else Error (error Conflict "foreground moderator checkpoint owner changed")
      in
      (* Owned handlers already acknowledged the full checkpoint. A worker may
         confirm only its unchanged bare Manager projection or that exact full
         carrier; explicit queue/extension changes must not disappear here. *)
      let%bind acknowledged =
        Moderator_checkpoint.reuse_unchanged_snapshot
          ~previous:t.state.moderator
          ~incoming:moderator
      in
      if Option.equal Jsonaf.exactly_equal t.state.moderator acknowledged
      then Ok ()
      else
        Error
          (error Conflict "foreground moderator checkpoint differs from its Event ACK")
    | None ->
      if Option.equal Jsonaf.exactly_equal t.state.moderator moderator
      then Ok ()
      else Result.map (change_moderator t moderator) ~f:(fun _ -> ()))
;;

let configuration_view t =
  Configuration_capture.view
    t.configuration_capture
    ~generation:t.state.identity.generation
    ~revision:t.state.spec.configuration_revision
    ~selected:t.state.spec.inference_target
;;

let prepare_configuration_update t request =
  let open Result.Let_syntax in
  let%bind _ =
    write_attachment
      t
      request.Agent_protocol.Session_configuration.Update_request.attachment_id
  in
  let%bind policy =
    Result.of_option
      t.configuration_policy
      ~error:(error Invalid_state "host configuration policy is unavailable")
  in
  Configuration_update.create
    ~owner:t.configuration_update_owner
    ~policy
    ~request
    ~state:t.state
;;

let commit_configuration_update t validated =
  let open Result.Let_syntax in
  let request = Configuration_update.Validated.request validated in
  let%bind _ = write_attachment t request.attachment_id in
  let%bind policy =
    Result.of_option
      t.configuration_policy
      ~error:(error Conflict "host configuration policy changed")
  in
  let%bind () =
    Configuration_update.recheck
      validated
      ~owner:t.configuration_update_owner
      ~policy
      ~state:t.state
  in
  let proposed = Configuration_update.Validated.proposed validated in
  let revision = Int64.(t.state.spec.configuration_revision + 1L) in
  let delta =
    Session_delta.Batch
      [ Inference_target_changed proposed; Configuration_revision_changed revision ]
  in
  let%bind candidate = Session_delta.apply t.state delta in
  let%bind _ =
    transition
      t
      ~delta
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_updated
            (Session_state.summary candidate)
        ]
  in
  configuration_view t
;;

let begin_configuration_capture t operation_id =
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind () = require_runtime_admission t in
  let%bind target = Configuration_transition.target t.state in
  let%map policy =
    Result.of_option
      t.configuration_policy
      ~error:(error Invalid_state "root configuration policy is unavailable")
  in
  Configuration_capture.begin_capture
    t.configuration_capture
    ~operation_id
    ~generation:t.state.identity.generation
    ~revision:t.state.spec.configuration_revision
    ~target
    ~policy
;;

let mark_configuration_capture t capture configuration =
  let open Result.Let_syntax in
  let%bind _ = running_operation t (Configuration_capture.Token.operation_id capture) in
  let%bind () = require_runtime_admission t in
  Configuration_capture.mark t.configuration_capture capture configuration
;;

let finish_configuration_capture t capture success =
  Configuration_capture.finish t.configuration_capture capture ~success;
  Ok ()
;;

let root_configuration_context t operation_id =
  Configuration_capture.root_port
    ~begin_capture:(fun () -> call t (Begin_configuration_capture operation_id))
    ~mark:(fun capture configuration ->
      call t (Mark_configuration_capture (capture, configuration)))
    ~finish:(fun capture ~success ->
      call t (Finish_configuration_capture (capture, success)))
;;

let consume_deferred t operation_id =
  if not (runtime_admission_open t)
  then Ok []
  else
    let open Result.Let_syntax in
    let%bind _ = current_operation t operation_id in
    let%bind prepared =
      prepare_pending
        t
        (Adopt
           { boundary = Worker operation_id
           ; runtime_admission_open = runtime_admission_open t
           })
    in
    let entries = Pending_plan.adopted_entries (Pending_transition.plan prepared) in
    let%bind decoded = History_codec.all_of_protocol entries in
    if List.is_empty entries
    then Ok []
    else (
      let%map _ =
        transition
          t
          ~delta:(Pending_transition.delta prepared)
          ~payloads:[ Agent_protocol.Event.Durable.Payload.History_appended entries ]
      in
      decoded)
;;

let admit_standalone_delivery_internal t plan =
  let open Result.Let_syntax in
  let%bind () =
    Standalone_delivery.revalidate
      ~state:t.state
      ~staged:(Staged_notifications.values t.staged_notifications)
      ~limits:t.services.notification_limits
      plan
  in
  let%map _ =
    transition t ~delta:(Session_delta.Delivery_changed plan.delivery) ~payloads:[]
  in
  ()
;;

let checked_pending_job_delivery t revision expected =
  let open Result.Let_syntax in
  let module P = Agent_protocol in
  let%bind () =
    match Int64.equal revision t.state.counters.revision with
    | true -> Ok ()
    | false -> Error (error Conflict "background delivery runtime snapshot changed")
  in
  let%bind job =
    List.find t.state.jobs ~f:(fun value -> P.Id.Job.equal value.id expected.P.Job.id)
    |> Result.of_option ~error:(error Invalid_request "background delivery job missing")
  in
  let%bind () =
    match Jsonaf.exactly_equal (P.Job.to_json expected) (P.Job.to_json job) with
    | true -> Ok ()
    | false -> Error (error Conflict "background delivery job changed")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      job.session_id
      job.generation
  in
  let%bind () =
    match job.delivery with
    | Pending -> Ok ()
    | Delivered _ | Not_required | Discarded _ ->
      Error (error Already_resolved "background job delivery is resolved")
  in
  let%bind () =
    match job.status with
    | Succeeded | Failed _ | Cancelled | Interrupted _ -> Ok ()
    | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
      Error (error Conflict "background job is not terminal")
  in
  let%map () = P.Job.validate_result job in
  job
;;

let retire_job_delivery t (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let module P = Agent_protocol in
  let now = t.services.now () in
  let at =
    match job.completed_at with
    | Some completed when P.Timestamp.compare completed now > 0 -> completed
    | _ -> now
  in
  let job = { job with delivery = Discarded { at; reason = Authority_changed } } in
  let%map _ =
    transition
      t
      ~delta:(Session_delta.Job_changed job)
      ~payloads:[ P.Event.Durable.Payload.Job_state_changed job ]
  in
  ()
;;

let retire_obsolete_moderator_delivery_internal t revision expected =
  let open Result.Let_syntax in
  let%bind job = checked_pending_job_delivery t revision expected in
  let%bind source = Background_job_event.source ~state:t.state job in
  let%bind source =
    Result.of_option
      source
      ~error:
        (error Invalid_request "background delivery has no captured moderator source")
  in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match Option.equal Agent_protocol.Invocation.equal_observer (Some source) installed with
  | true -> Ok false
  | false ->
    let%map () = retire_job_delivery t job in
    true
;;

let deliver_standalone_completion_internal t revision expected current_capabilities policy
  =
  let open Result.Let_syntax in
  let module P = Agent_protocol in
  let%bind job = checked_pending_job_delivery t revision expected in
  let%bind invocation =
    match job.launch with
    | Some { owner = Invocation id; _ } ->
      List.find t.state.invocations ~f:(fun value ->
        P.Id.Invocation.equal value.context.id id)
      |> Result.of_option
           ~error:(error Invalid_request "standalone completion owner missing")
    | _ -> Error (error Invalid_request "standalone completion has no invocation owner")
  in
  match
    Standalone_completion_contract.authorize
      ~invocation
      ~job
      ~current_capabilities
      ~policy
  with
  | Error { code = Permission_denied; _ } -> retire_job_delivery t job
  | Error error -> Error error
  | Ok () ->
    let load_artifact =
      Option.map t.services.job_results ~f:(fun publisher ->
        Agent_store.Job_result_store.Publisher.load publisher)
    in
    let%bind completion = P.Job.terminal_completion ?load_artifact job in
    let%bind completion =
      Result.of_option
        completion
        ~error:(error Invalid_state "standalone job has no terminal result")
    in
    let now = t.services.now () in
    let%bind plan =
      Standalone_delivery.prepare
        ~state:t.state
        ~notification_limits:t.services.notification_limits
        ~invocation_id:invocation.context.id
        ~job_id:job.id
        ~completion
        ~current_capabilities
        ~delivery_id:(P.Id.Delivery.create ())
        ~now
        ~wake:Request_turn
    in
    let%bind () =
      Standalone_delivery.revalidate
        ~state:t.state
        ~staged:(Staged_notifications.values t.staged_notifications)
        ~limits:t.services.notification_limits
        plan
    in
    let job = { job with delivery = Delivered now } in
    let%map _ =
      transition
        t
        ~delta:(Session_delta.Batch [ Delivery_changed plan.delivery; Job_changed job ])
        ~payloads:[ P.Event.Durable.Payload.Job_state_changed job ]
    in
    ()
;;

let validate_notification_plan t (plan : Notification_delivery.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      plan.session_id
      plan.generation
  in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match
    ( Int64.equal plan.revision t.state.counters.revision
    , installed
    , moderator_is_borrowed t )
  with
  | true, source, false
    when Option.equal Agent_protocol.Invocation.equal_observer source plan.source -> Ok ()
  | _ -> Error (error Conflict "notification snapshot or source changed")
;;

let notification_changes t (plan : Notification_delivery.t) =
  let open Result.Let_syntax in
  let%map deltas, entries, committed =
    List.fold_result
      plan.actions
      ~init:([], [], [])
      ~f:(fun (deltas, entries, committed) action ->
        match action with
        | Notification_delivery.Fail value ->
          Ok (Session_delta.Delivery_changed value :: deltas, entries, committed)
        | Publish value ->
          let%bind id =
            History_entry.Id.create
              ~namespace:
                ("notification/" ^ Agent_protocol.Id.Delivery.to_string value.context.id)
              ~sequence:0
            |> Result.map_error ~f:(error Invalid_state)
          in
          let%bind entry = Notification_history.create ~id value in
          let now = t.services.now () in
          let now =
            if Agent_protocol.Timestamp.compare now value.context.created_at < 0
            then value.context.created_at
            else now
          in
          let%map value =
            Agent_protocol.Delivery.commit ~track_wake:true value ~history_id:id ~now
          in
          ( Session_delta.Delivery_committed (value, entry) :: deltas
          , entry :: entries
          , value :: committed ))
  in
  List.rev deltas, List.rev entries, List.rev committed
;;

let notification_payloads = function
  | [] -> []
  | entries -> [ Agent_protocol.Event.Durable.Payload.History_appended entries ]
;;

let consume_notifications_internal
      ?(wakes = [])
      ?(discarded_wakes = [])
      t
      operation_id
      plan
  =
  if not (runtime_admission_open t)
  then Ok Chat_response.In_memory_stream.Safe_point_input.empty
  else
    let open Result.Let_syntax in
    let%bind _ = running_operation ~allow_stopping:true t operation_id in
    match t.state.lifecycle.desired with
    | Stopped ->
      (* An admitted provider may finish during graceful stop. Its final safe point
       must not fail, consume retained deliveries, or request another turn. *)
      Ok Chat_response.In_memory_stream.Safe_point_input.empty
    | Running ->
      let%bind () =
        match
          List.exists t.invocation_executions ~f:(fun execution ->
            match execution.owner with
            | Foreground id -> Agent_protocol.Id.Operation.equal id operation_id
            | _ -> false)
        with
        | true ->
          Error
            (error Conflict "notification insertion waits for the foreground tool batch")
        | false -> Ok ()
      in
      let%bind () = validate_notification_plan t plan in
      let%bind deltas, entries, committed = notification_changes t plan in
      let deltas =
        deltas
        @ List.map discarded_wakes ~f:(fun value ->
          Session_delta.Delivery_wake_changed value)
      in
      let%bind decoded = History_codec.all_of_protocol entries in
      let%map () =
        match deltas with
        | [] -> Ok ()
        | _ ->
          transition
            t
            ~delta:(Session_delta.Batch deltas)
            ~payloads:(notification_payloads entries)
          |> Result.map ~f:ignore
      in
      let existing =
        match t.notification_inputs with
        | Some (id, ids) when Agent_protocol.Id.Operation.equal id operation_id -> ids
        | _ -> []
      in
      let ids =
        List.map (committed @ wakes) ~f:(fun value ->
          value.Agent_protocol.Delivery.context.id)
      in
      t.notification_inputs <- Some (operation_id, ids @ existing);
      let wake =
        List.exists (committed @ wakes) ~f:(fun value ->
          Agent_protocol.Completion.equal_wake value.context.wake Request_turn)
      in
      Chat_response.In_memory_stream.Safe_point_input.notification_entries
        ~request_turn:wake
        decoded
;;

let notification_wake_deltas t operation_id ~accept =
  let open Result.Let_syntax in
  let ids =
    match t.notification_inputs with
    | Some (id, ids) when Agent_protocol.Id.Operation.equal id operation_id -> ids
    | _ -> []
  in
  List.filter t.state.deliveries ~f:(fun value ->
    List.mem ids value.context.id ~equal:Agent_protocol.Id.Delivery.equal
    &&
    match value.wake_disposition with
    | Some Pending_wake -> true
    | _ -> false)
  |> List.map ~f:(fun value ->
    let%map value =
      match accept with
      | true -> Agent_protocol.Delivery.accept_wake value ~operation_id
      | false ->
        Agent_protocol.Delivery.discard_wake
          value
          ~reason:"foreground ended without admitting this notification wake"
    in
    Session_delta.Delivery_wake_changed value)
  |> Result.all
;;

let worker_ready t operation_id cancel =
  match t.state.active_operation with
  | None ->
    cancel ();
    Ok ()
  | Some operation when Agent_protocol.Id.Operation.compare operation.id operation_id <> 0
    ->
    cancel ();
    Ok ()
  | Some operation ->
    t.active_cancel <- Some cancel;
    if not (runtime_admission_open t)
    then (
      cancel ();
      Ok ())
    else (
      match operation.state with
      | Agent_protocol.Operation.Cancelling ->
        cancel ();
        Ok ()
      | Starting ->
        let operation = operation_state t operation Running in
        let open Result.Let_syntax in
        let%map _ =
          transition
            t
            ~delta:(Session_delta.Active_operation_changed (Some operation))
            ~payloads:
              [ Agent_protocol.Event.Durable.Payload.Session_updated
                  (summary_with_operation t operation)
              ]
        in
        ()
      | Running -> Ok ()
      | Completed | Failed _ | Cancelled | Interrupted _ ->
        cancel ();
        Ok ())
;;

let terminal_lifecycle t =
  Session_state.Lifecycle.
    { desired = t.state.lifecycle.desired
    ; observed =
        (match t.state.lifecycle.desired with
         | Running ->
           (match
              List.find t.state.permissions ~f:(fun permission ->
                Agent_protocol.Permission.equal_state permission.state Pending
                &&
                match permission.owner with
                | Operation _ -> false
                | Invocation id ->
                  List.exists t.invocation_executions ~f:(fun execution ->
                    Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id
                    &&
                    match execution.owner with
                    | Background_job _ -> true
                    | _ -> false))
            with
            | Some permission -> Waiting_for_permission permission.id
            | None -> Idle)
         | Stopped -> Stopped)
    }
;;

let history_prefix ~prefix values =
  let equal left right =
    Sexp.equal
      ([%sexp_of: Agent_protocol.History.entry] left)
      ([%sexp_of: Agent_protocol.History.entry] right)
  in
  List.is_prefix values ~prefix ~equal
;;

let end_session_reason requests =
  List.find_map requests ~f:(function
    | Chat_response.Moderation.Runtime_request.End_session reason -> Some reason
    | Request_turn | Request_compaction -> None)
;;

let completed_lifecycle t requests =
  match end_session_reason requests with
  | None -> terminal_lifecycle t
  | Some _ ->
    Session_state.Lifecycle.
      { desired = Agent_protocol.Session.Stopped; observed = Stopped }
;;

let runtime_request_deltas summary =
  end_session_reason summary.Operation_worker.Summary.runtime_requests
  |> Option.map ~f:(fun reason -> Session_delta.Halt_changed (Some reason))
  |> Option.to_list
;;

let runtime_request_payloads summary =
  end_session_reason summary.Operation_worker.Summary.runtime_requests
  |> Option.map ~f:(fun reason ->
    Agent_protocol.Event.Durable.Payload.Moderator_notification
      (`Object [ "end_session_reason", `String reason ]))
  |> Option.to_list
;;

let release_pending_delta t (operation : Agent_protocol.Operation.t) =
  let relevant =
    List.exists t.state.conversation.deferred_user_entries ~f:(fun document ->
      match
        Agent_protocol.Pending_input.binding (Pending_input_document.value document)
      with
      | After_root { operation_id; terminal = None; _ } ->
        Agent_protocol.Id.Operation.equal operation_id operation.id
      | Safe_boundary | Await_idle | After_root { terminal = Some _; _ } -> false)
  in
  if not relevant
  then Ok None
  else
    let open Result.Let_syntax in
    let%bind proof = Agent_protocol.Pending_input.Terminal_proof.of_operation operation in
    let%map prepared = prepare_pending t (Release proof) in
    Some (Pending_transition.delta prepared)
;;

let completed_delta t operation summary =
  let final_protocol =
    History_codec.all_to_protocol
      ~previous:t.state.conversation.canonical_history
      summary.Operation_worker.Summary.final_history
  in
  let committed = t.state.conversation.canonical_history in
  if not (history_prefix ~prefix:committed final_protocol)
  then Error (error Conflict "worker final history diverges from committed history")
  else
    let open Result.Let_syntax in
    let missing = List.drop final_protocol (List.length committed) in
    let operation = operation_state t operation Agent_protocol.Operation.Completed in
    let%bind release = release_pending_delta t operation in
    let lifecycle = completed_lifecycle t summary.runtime_requests in
    let moderator_snapshot =
      match t.foreground_moderator, moderator_is_borrowed t with
      | Some _, _ | _, true -> t.state.moderator
      | None, false -> summary.moderator_snapshot
    in
    let deltas =
      [ release
      ; Option.some_if
          (not (List.is_empty missing))
          (Session_delta.Canonical_entries_appended missing)
      ; Some (Session_delta.Active_operation_changed None)
      ; Some (Session_delta.Lifecycle_changed lifecycle)
      ; Some (Session_delta.Moderator_changed moderator_snapshot)
      ]
      |> List.filter_opt
      |> fun values -> values @ runtime_request_deltas summary
    in
    let payloads =
      [ Option.some_if
          (not (List.is_empty missing))
          (Agent_protocol.Event.Durable.Payload.History_appended missing)
      ; Some (Agent_protocol.Event.Durable.Payload.Operation_completed operation)
      ; Some
          (Agent_protocol.Event.Durable.Payload.Session_state_changed
             { desired_state = lifecycle.desired; observed_state = lifecycle.observed })
      ]
      |> List.filter_opt
      |> fun values -> values @ runtime_request_payloads summary
    in
    Ok (Session_delta.Batch deltas, payloads)
;;

let terminal_delta t operation outcome =
  let open Result.Let_syntax in
  match outcome with
  | Operation_worker.Completed summary -> completed_delta t operation summary
  | Cancelled cancellation ->
    let operation = operation_state t operation Agent_protocol.Operation.Cancelled in
    let%map release = release_pending_delta t operation in
    let lifecycle = terminal_lifecycle t in
    ( Session_delta.Batch
        (Option.to_list release
         @ [ Active_operation_changed None; Lifecycle_changed lifecycle ])
    , [ Agent_protocol.Event.Durable.Payload.Operation_cancelled operation
      ; Moderator_notification
          (`Object [ "cancellation_reason", `String cancellation.reason ])
      ; Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ] )
  | Failed failure ->
    let operation =
      operation_state t operation (Agent_protocol.Operation.Failed failure)
    in
    let%map release = release_pending_delta t operation in
    let lifecycle = terminal_lifecycle t in
    ( Session_delta.Batch
        (Option.to_list release
         @ [ Active_operation_changed None; Lifecycle_changed lifecycle ])
    , [ Agent_protocol.Event.Durable.Payload.Operation_failed operation
      ; Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ] )
;;

let is_permission_review_job (job : Agent_protocol.Job.t) =
  match job.payload with
  | `Object fields ->
    List.Assoc.find fields "type" ~equal:String.equal
    |> Option.value_map ~default:false ~f:(function
      | `String value -> String.equal value "permission_review"
      | _ -> false)
  | _ -> false
;;

let pending_for_operation t (operation : Agent_protocol.Operation.t) permission =
  Agent_protocol.Permission.equal_state permission.Agent_protocol.Permission.state Pending
  &&
  match permission.owner with
  | Operation id -> Agent_protocol.Id.Operation.equal id operation.id
  | Invocation id ->
    List.exists t.invocation_executions ~f:(fun execution ->
      Agent_protocol.Id.Invocation.equal id execution.dispatched.context.id
      &&
      match execution.owner with
      | Foreground id -> Agent_protocol.Id.Operation.equal id operation.id
      | Background_job _ -> false
      | Event_moderator borrow ->
        Option.exists
          borrow.receipt.context.operation_id
          ~f:(Agent_protocol.Id.Operation.equal operation.id)
      | Invocation_moderator borrow ->
        Option.exists
          borrow.operation_id
          ~f:(Agent_protocol.Id.Operation.equal operation.id))
    || Option.exists t.moderator_borrow ~f:(fun borrow ->
      Agent_protocol.Id.Invocation.equal id borrow.invocation.context.id
      && Option.exists
           borrow.operation_id
           ~f:(Agent_protocol.Id.Operation.equal operation.id))
;;

let cancel_operation_permission t operation reason permission =
  if pending_for_operation t operation permission
  then Some (cancel_permission t reason permission)
  else None
;;

let interrupt_permission_review_job t reason (job : Agent_protocol.Job.t) =
  if is_permission_review_job job
  then (
    match job.status with
    | Running ->
      Some
        { job with
          status = Interrupted reason
        ; completed_at = Some (t.services.now ())
        ; delivery = Pending
        }
    | Queued ->
      Some
        { job with
          status = Cancelled
        ; completed_at = Some (t.services.now ())
        ; delivery = Pending
        }
    | Waiting_permission _
    | Waiting_completion _
    | Succeeded
    | Failed _
    | Cancelled
    | Interrupted _ -> None)
  else None
;;

(* Only genuine terminal/control writers call this helper with their exact final
   persisted records. Ordinary DTO changes remain negative-only reconciliation. *)
let prepare_terminal_job_custody t jobs =
  let open Result.Let_syntax in
  let%bind completed =
    List.map jobs ~f:(fun (terminal : Agent_protocol.Job.t) ->
      let%map previous =
        List.find t.state.jobs ~f:(fun job ->
          Agent_protocol.Id.Job.equal job.id terminal.id)
        |> Result.of_option ~error:(error Invalid_request "terminal Job was not found")
      in
      previous, terminal)
    |> Result.all
  in
  Job_custody.prepare_terminals t.job_agent_custody ~completed
;;

let operation_terminal_cleanup t operation outcome =
  let reason =
    match outcome with
    | Operation_worker.Completed _ -> None
    | Cancelled _ -> Some "operation was cancelled"
    | Failed failure -> Some ("operation failed: " ^ failure.message)
  in
  match reason with
  | None -> [], []
  | Some reason ->
    ( List.filter_map
        t.state.permissions
        ~f:(cancel_operation_permission t operation reason)
    , List.filter_map t.state.jobs ~f:(interrupt_permission_review_job t reason) )
;;

let cleanup_delta permissions jobs =
  Session_delta.Batch
    (List.map permissions ~f:(fun permission ->
       Session_delta.Permission_changed permission)
     @ List.map jobs ~f:(fun job -> Session_delta.Job_changed job))
;;

let cleanup_payloads permissions jobs =
  List.map permissions ~f:(fun permission ->
    Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
  @ List.map jobs ~f:(fun job ->
    Agent_protocol.Event.Durable.Payload.Job_state_changed job)
;;

let compaction_generation t =
  if Int.equal t.state.conversation.compaction_generation Int.max_value
  then Error (error Invalid_state "compaction generation overflow")
  else Ok (t.state.conversation.compaction_generation + 1)
;;

let compaction_terminal_base_delta t operation = function
  | Compacted history ->
    let open Result.Let_syntax in
    let%bind generation = compaction_generation t in
    let%bind archive =
      t.persistence.archive_reference
        ~previous:t.state
        ~kind:Compaction
        operation.Agent_protocol.Operation.id
    in
    let history =
      History_codec.all_to_protocol
        ~previous:t.state.conversation.canonical_history
        history
    in
    let operation = operation_state t operation Agent_protocol.Operation.Completed in
    let lifecycle = terminal_lifecycle t in
    Ok
      ( Session_delta.Batch
          [ Compaction_archived archive
          ; Canonical_history_replaced history
          ; Compaction_generation_changed generation
          ; Active_operation_changed None
          ; Lifecycle_changed lifecycle
          ]
      , [ Agent_protocol.Event.Durable.Payload.History_replaced
            (Session_state.history_window history)
        ; Operation_completed operation
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ] )
  | Compaction_cancelled reason ->
    let operation = operation_state t operation Agent_protocol.Operation.Cancelled in
    let lifecycle = terminal_lifecycle t in
    Ok
      ( Session_delta.Batch [ Active_operation_changed None; Lifecycle_changed lifecycle ]
      , [ Agent_protocol.Event.Durable.Payload.Operation_cancelled operation
        ; Moderator_notification (`Object [ "cancellation_reason", `String reason ])
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ] )
  | Compaction_failed failure ->
    let operation =
      operation_state t operation (Agent_protocol.Operation.Failed failure)
    in
    let lifecycle = terminal_lifecycle t in
    Ok
      ( Session_delta.Batch [ Active_operation_changed None; Lifecycle_changed lifecycle ]
      , [ Agent_protocol.Event.Durable.Payload.Operation_failed operation
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ] )
;;

let compaction_terminal_delta t operation outcome =
  let open Result.Let_syntax in
  let%bind delta, payloads = compaction_terminal_base_delta t operation outcome in
  let%bind discarded =
    match outcome with
    | Compacted _ -> Ok []
    | Compaction_cancelled _ | Compaction_failed _ ->
      Observation_follow_up.discard_compaction
        t.state.invocations
        ~operation_id:operation.id
        ~reason:
          (match outcome with
           | Compaction_cancelled _ -> "compaction cancelled"
           | _ -> "compaction failed")
  in
  let%map events =
    match outcome with
    | Compacted _ -> Ok []
    | Compaction_cancelled _ | Compaction_failed _ ->
      Observation_follow_up.discard_event_compaction
        t.state.moderator_executions
        ~operation_id:operation.id
        ~reason:
          (match outcome with
           | Compaction_cancelled _ -> "compaction cancelled"
           | _ -> "compaction failed")
  in
  ( Session_delta.Batch
      (List.map discarded ~f:Observation_follow_up.delta
       @ List.map events ~f:Observation_follow_up.event_delta
       @ [ delta ])
  , payloads )
;;

let compaction_terminal t operation_id outcome =
  match t.state.active_operation with
  | None -> Ok ()
  | Some operation when Agent_protocol.Id.Operation.compare operation.id operation_id <> 0
    -> Ok ()
  | Some operation ->
    let open Result.Let_syntax in
    let outcome =
      match operation.state with
      | Agent_protocol.Operation.Cancelling -> Compaction_cancelled "operation cancelled"
      | _ -> outcome
    in
    let%bind delta, payloads = compaction_terminal_delta t operation outcome in
    let%map _ =
      match transition t ~delta ~payloads, outcome with
      | Ok session, _ -> Ok session
      | Error failure, Compacted _ ->
        let%bind delta, payloads =
          compaction_terminal_delta t operation (Compaction_failed failure)
        in
        transition t ~delta ~payloads
      | Error failure, _ -> Error failure
    in
    t.active_cancel <- None
;;

let compaction_failure exn =
  let message =
    match exn with
    | Context_compaction.Summarizer.Failed No_text ->
      "history compaction failed: summarizer returned no text"
    | _ -> "history compaction failed: " ^ Exn.to_string exn
  in
  Agent_protocol.Error.create Internal_error ~message ~retryable:true ()
;;

exception Compaction_cancel_requested

let compute_compaction t operation selection port installed allocator history =
  let compute inference ~guidance ~reserved_input_tokens =
    match
      Context_compaction.Compactor.compact_entries_with_guidance
        ~guidance
        ~reserved_input_tokens
        ~inference
        ~allocator
        ~env:t.compaction_env
        ~history
    with
    | Error exn -> Error (compaction_failure exn)
    | Ok history ->
      (match History_entry.validate ~allocator history with
       | Ok () -> Ok history
       | Error message -> Error (error Conflict ("invalid compacted history: " ^ message)))
  in
  let result =
    match port, installed with
    | Some port, _ ->
      port.Compaction_inference.with_execution
        ~operation
        ~selection
        ~borrow_agent_actor:(fun () -> call t (Borrow_compaction_agent_actor operation))
        compute
    | None, Some inference ->
      let active =
        Option.value_map t.state.instruction_selection ~default:false ~f:(fun selection ->
          let status = Instruction_selection.summary selection in
          status.project_selected || status.skills > 0)
      in
      if active
      then
        Error
          (error
             Configuration_invalid
             "active instructions require an owned compaction guidance port")
      else compute inference ~guidance:None ~reserved_input_tokens:0
    | None, None ->
      Error (error Configuration_invalid "compaction requires selected runtime inference")
  in
  match result with
  | Ok history -> Compacted history
  | Error failure -> Compaction_failed failure
;;

let run_compaction t operation selection port installed allocator history =
  let operation_id = operation.Agent_protocol.Operation.id in
  try
    Eio.Switch.run (fun operation_switch ->
      let cancel () = Eio.Switch.fail operation_switch Compaction_cancel_requested in
      match call t ~priority:Priority (Worker_ready (operation_id, cancel)) with
      | Error failure -> Compaction_failed failure
      | Ok () ->
        Eio.Switch.check operation_switch;
        compute_compaction t operation selection port installed allocator history)
  with
  | Compaction_cancel_requested -> Compaction_cancelled "operation cancelled"
  | Eio.Cancel.Cancelled reason -> Compaction_cancelled (Exn.to_string reason)
  | exn -> Compaction_failed (compaction_failure exn)
;;

let launch_compaction t operation allocator history =
  let operation_id = operation.Agent_protocol.Operation.id in
  let selection = t.state.spec.inference_target in
  let port = t.compaction_inference in
  let installed = t.inference_execution in
  let admission =
    Option.bind t.operation_agent_custody ~f:(fun (owner, admission) ->
      Option.some_if (Agent_protocol.Id.Operation.equal owner operation_id) admission)
  in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    let outcome =
      Exn.protect
        ~f:(fun () ->
          run_compaction t operation selection port installed allocator history)
        ~finally:(fun () -> Option.iter admission ~f:retire_agent_admission)
    in
    ignore
      (call t ~priority:Priority (Compaction_terminal (operation_id, outcome))
       : (unit, Agent_protocol.Error.t) result))
;;

let create_compaction_operation t =
  let timestamp = t.services.now () in
  Agent_protocol.Operation.
    { id = Agent_protocol.Id.Operation.create ()
    ; generation = t.state.identity.generation
    ; kind = Compaction
    ; state = Starting
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let int_of_history_sequence sequence =
  if Int64.(sequence > of_int Int.max_value)
  then Error (error Invalid_state "history sequence exceeds platform allocation range")
  else Ok (Int64.to_int_exn sequence)
;;

let compaction_allocator t first_sequence reserved_through =
  let open Result.Let_syntax in
  let%bind next_sequence = int_of_history_sequence first_sequence in
  let%bind limit_exclusive = int_of_history_sequence reserved_through in
  History_entry.Allocator.create_bounded
    ~namespace:(Agent_protocol.Id.Session.to_string t.state.identity.session_id)
    ~next_sequence
    ~limit_exclusive
  |> Result.map_error ~f:(fun message -> error Invalid_state message)
;;

let reconcile_foreground_invocations t =
  let open Result.Let_syntax in
  let first =
    Int64.max
      t.state.conversation.next_history_sequence
      t.state.conversation.reserved_history_through
  in
  let%bind first_sequence = int_of_history_sequence first in
  let%bind plan =
    Invocation_recovery.plan_foreground
      ~state:t.state
      ~namespace:(Agent_protocol.Id.Session.to_string t.state.identity.session_id)
      ~first_sequence
      ~reason:"foreground worker ended before recording the invocation outcome"
  in
  if List.is_empty plan.deltas
  then Ok ()
  else
    transition
      t
      ~delta:
        (Session_delta.Batch
           (History_block_reserved (Int64.of_int plan.next_sequence) :: plan.deltas))
      ~payloads:
        (if List.is_empty plan.appended
         then []
         else [ Agent_protocol.Event.Durable.Payload.History_appended plan.appended ])
    |> Result.map ~f:(fun _ -> ())
;;

let retain_reconciliation_failure t failure =
  let lifecycle =
    Session_state.Lifecycle.
      { desired = t.state.lifecycle.desired; observed = Failed failure }
  in
  transition
    t
    ~delta:
      (Session_delta.Batch [ Failure_changed (Some failure); Lifecycle_changed lifecycle ])
    ~payloads:
      [ Agent_protocol.Event.Durable.Payload.Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ]
  |> Result.map ~f:(fun _ -> ())
;;

let start_compaction
      ?operation
      ?follow_up_authority
      ?agent_custody
      ?(extra_deltas : Session_delta.t list = [])
      t
  =
  let open Result.Let_syntax in
  let%bind () = require_runtime_admission t in
  let%bind () = reconcile_foreground_invocations t in
  let revision = t.state.counters.revision in
  let first_sequence = t.state.conversation.next_history_sequence in
  if Int64.equal first_sequence Int64.max_value
  then Error (error Invalid_state "history sequence overflow")
  else (
    let reserved_through = Int64.(first_sequence + 1L) in
    let open Result.Let_syntax in
    let%bind allocator = compaction_allocator t first_sequence reserved_through in
    let%bind history =
      History_codec.all_of_protocol t.state.conversation.canonical_history
    in
    let operation =
      Option.value_or_thunk operation ~default:(fun () -> create_compaction_operation t)
    in
    let lifecycle = lifecycle_for_compaction t operation.id in
    let%bind child =
      match agent_custody with
      | None -> Ok None
      | Some custody -> Result.map (transfer_agent_custody custody) ~f:Option.some
    in
    let installed = ref false in
    Exn.protect
      ~f:(fun () ->
        let%bind session =
          transition
            t
            ~check_before_commit:(fun () ->
              match follow_up_authority with
              | None -> Ok ()
              | Some authority ->
                let%bind () =
                  Option.value_map child ~default:(Ok ()) ~f:(fun custody ->
                    Execution_custody.actor custody |> Result.map ~f:ignore)
                in
                let%bind () = check_follow_up_authority t authority in
                if Int64.equal revision t.state.counters.revision
                then Ok ()
                else
                  Error
                    (error Conflict "observation compaction basis changed before commit"))
            ~delta:
              (Session_delta.Batch
                 (extra_deltas
                  @ [ Session_delta.History_block_reserved reserved_through
                    ; Active_operation_changed (Some operation)
                    ; Lifecycle_changed lifecycle
                    ]))
            ~payloads:
              [ Agent_protocol.Event.Durable.Payload.Operation_started operation
              ; Session_state_changed
                  { desired_state = lifecycle.desired
                  ; observed_state = lifecycle.observed
                  }
              ]
        in
        install_operation_agent_custody t operation child;
        installed := true;
        launch_compaction t operation allocator history;
        Ok session)
      ~finally:(fun () ->
        if not !installed then Option.iter child ~f:retire_agent_custody))
;;

let compact_internal t original_actor attachment_id expected_revision =
  let open Result.Let_syntax in
  let%bind () =
    match original_actor with
    | None -> Ok ()
    | Some actor when Operator_authorization.is_current actor -> Ok ()
    | Some _ -> Error (error Permission_denied "compaction authentication expired")
  in
  let%bind _ = write_attachment t attachment_id in
  let%bind () =
    match expected_revision with
    | None -> Ok ()
    | Some revision when Int64.equal revision t.state.counters.revision -> Ok ()
    | Some _ -> Error (error Conflict "session revision does not match")
  in
  match
    t.idle_moderator_borrowed, t.state.active_operation, t.state.lifecycle.observed
  with
  | true, _, _ -> Error (error Conflict "session moderator is processing background work")
  | false, Some _, _ -> Error (error Conflict "a foreground operation is already active")
  | false, None, (Idle | Stopped) ->
    let custody = Option.map original_actor ~f:create_agent_custody in
    Exn.protect
      ~f:(fun () -> start_compaction ?agent_custody:custody t)
      ~finally:(fun () -> Option.iter custody ~f:retire_agent_custody)
  | false, None, _ -> Error (error Invalid_state "session is not ready for compaction")
;;

let history_edit_precondition t revision =
  if not (Int64.equal revision t.state.counters.revision)
  then Error (error Conflict "session revision does not match")
  else if t.idle_moderator_borrowed || Option.is_some t.state.active_operation
  then Error (error Conflict "history cannot change during active work")
  else (
    match t.state.lifecycle.observed with
    | Idle | Stopped -> Ok ()
    | _ -> Error (error Invalid_state "session is not ready for history editing"))
;;

let outcome_requests_compaction = function
  | Operation_worker.Completed summary ->
    List.exists summary.runtime_requests ~f:(function
      | Chat_response.Moderation.Runtime_request.Request_compaction -> true
      | Request_turn | End_session _ -> false)
  | Cancelled _ | Failed _ -> false
;;

let follow_up_deltas t (plan : Observation_follow_up.t) =
  List.map plan.invocations ~f:(fun invocation ->
    match Int.equal invocation.context.generation t.state.identity.generation with
    | true -> Session_delta.Invocation_changed invocation
    | false -> Session_delta.Invocation_reconciled invocation)
  @ List.map plan.events ~f:Observation_follow_up.event_delta
;;

let managed_moderator t operation_id =
  let open Result.Let_syntax in
  let%bind observer =
    match t.foreground_moderator with
    | Some (id, observer) when Agent_protocol.Id.Operation.equal id operation_id ->
      Ok observer
    | _ -> Error (error Conflict "foreground moderator routing is not installed")
  in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match installed with
  | Some installed when Agent_protocol.Invocation.equal_observer observer installed ->
    Ok observer
  | _ -> Error (error Conflict "foreground moderator source changed")
;;

let manage_moderator_follow_up t operation_id observer =
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match installed with
  | Some installed when Agent_protocol.Invocation.equal_observer observer installed ->
    t.foreground_moderator <- Some (operation_id, observer);
    Ok ()
  | _ -> Error (error Conflict "foreground routing requires the installed moderator")
;;

let admit_moderator_turn t operation_id =
  let open Result.Let_syntax in
  let%bind operation = running_operation t operation_id in
  let%bind observer = managed_moderator t operation_id in
  let%bind halted = Runtime_builder.moderator_snapshot_is_halted t.state.moderator in
  let%bind () =
    match halted, moderator_is_borrowed t with
    | true, _ -> Error (error Conflict "halted moderator cannot admit a provider request")
    | false, true ->
      Error (error Conflict "provider admission cannot interrupt a moderator event")
    | false, false -> Ok ()
  in
  let%bind plan = Observation_follow_up.admit_turn ~state:t.state ~observer in
  let%bind notifications = notification_wake_deltas t operation_id ~accept:true in
  let%bind run_deltas = run_turn_deltas t operation plan.events [] in
  match follow_up_deltas t plan @ notifications @ run_deltas with
  | [] -> Ok ()
  | deltas ->
    transition t ~delta:(Session_delta.Batch deltas) ~payloads:[] |> Result.map ~f:ignore
;;

let admit_notification_turn t operation_id =
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind () =
    match t.state.lifecycle.desired, t.state.halted, moderator_is_borrowed t with
    | Running, false, false -> Ok ()
    | _ -> Error (error Conflict "session cannot admit a provider request")
  in
  let%bind deltas = notification_wake_deltas t operation_id ~accept:true in
  match deltas with
  | [] -> Ok ()
  | _ ->
    transition t ~delta:(Session_delta.Batch deltas) ~payloads:[] |> Result.map ~f:ignore
;;

let foreground_terminal_requests t operation_id outcome =
  let open Result.Let_syntax in
  match t.foreground_moderator with
  | None -> Ok []
  | Some _ ->
    let%bind observer = managed_moderator t operation_id in
    let end_reason, failed =
      match outcome with
      | Operation_worker.Completed summary ->
        end_session_reason summary.runtime_requests, false
      | Failed _ | Cancelled _ -> None, true
    in
    let%map plan =
      match end_reason with
      | None ->
        Observation_follow_up.finish_foreground
          ~state:t.state
          ~observer
          ~operation_id
          ~failed
      | Some _ ->
        Observation_follow_up.plan
          ~state:t.state
          ~observer:(Some observer)
          ~halted:true
          ~compaction_operation_id:(Agent_protocol.Id.Operation.create ())
    in
    follow_up_deltas t plan
;;

let worker_terminal t operation_id outcome compaction_custody =
  match t.state.active_operation with
  | None -> Ok ()
  | Some operation when Agent_protocol.Id.Operation.compare operation.id operation_id <> 0
    -> Ok ()
  | Some operation ->
    let open Result.Let_syntax in
    let belongs operation =
      Option.exists operation ~f:(Agent_protocol.Id.Operation.equal operation_id)
    in
    let borrow =
      Option.filter t.moderator_borrow ~f:(fun borrow -> belongs borrow.operation_id)
    in
    let event_borrow =
      Option.filter t.queued_event_borrow ~f:(fun borrow ->
        belongs borrow.receipt.context.operation_id)
    in
    let foreground, independent =
      List.partition_tf t.invocation_executions ~f:(fun execution ->
        match execution.owner with
        | Foreground id -> Agent_protocol.Id.Operation.equal id operation_id
        | Invocation_moderator borrow -> belongs borrow.operation_id
        | Event_moderator borrow -> belongs borrow.receipt.context.operation_id
        | Background_job _ -> false)
    in
    let%bind unfinished =
      List.map foreground ~f:(fun execution ->
        execution.accepts_children <- false;
        Atomic.set execution.authority_live false;
        abort_staged_work t ~owner:(Invocation execution.dispatched.context.id);
        Agent_protocol.Invocation.cancel
          execution.dispatched
          ~reason:"worker exited before recording the invocation outcome"
        |> Result.map ~f:(fun invocation -> Session_delta.Invocation_changed invocation))
      |> Result.all
    in
    let%bind borrow_delta =
      match borrow with
      | None -> Ok (Session_delta.Batch [])
      | Some borrow ->
        abort_staged_work t ~owner:(Invocation borrow.invocation.context.id);
        uncommitted_borrow_delta
          t
          borrow
          (Some
             (Agent_protocol.Invocation.Cancelled
                "worker exited with an active moderator borrow"))
    in
    let%bind event_delta =
      match event_borrow with
      | Some borrow when not borrow.committed ->
        abort_staged_work t ~owner:(Moderator_event borrow.receipt.context.id);
        let%map interrupted =
          Agent_protocol.Moderator_execution.interrupt
            borrow.receipt
            ~reason:"worker exited before recording its moderator event"
        in
        Session_delta.Moderator_execution_changed interrupted
      | _ -> Ok (Session_delta.Batch [])
    in
    let outcome =
      match
        ( Option.is_some borrow
          || Option.is_some event_borrow
          || not (List.is_empty unfinished)
        , outcome )
      with
      | true, Operation_worker.Completed _ ->
        Operation_worker.Failed
          (error Internal_error "worker completed with an active invocation")
      | _ -> outcome
    in
    let run_outcome : Agent_protocol.Run_work.Terminal.outcome =
      match outcome with
      | Operation_worker.Completed _ -> Succeeded
      | Cancelled _ -> Cancelled
      | Failed _ -> Failed
    in
    let now, completion_sample = completion_clock_pair t in
    let%bind run_deltas =
      Run_transition.settle_operation t.state ~operation_id ~outcome:run_outcome ~now
    in
    let%bind delta, payloads = terminal_delta t operation outcome in
    let%bind follow_up = foreground_terminal_requests t operation_id outcome in
    let%bind notifications = notification_wake_deltas t operation_id ~accept:false in
    let permissions, jobs = operation_terminal_cleanup t operation outcome in
    let%bind job_custody = prepare_terminal_job_custody t jobs in
    let%bind _ =
      transition
        ~completion_sample
        ~job_custody
        t
        ~delta:
          (Session_delta.Batch
             (notifications
              @ follow_up
              @ unfinished
              @ run_deltas
              @ [ borrow_delta; event_delta; delta; cleanup_delta permissions jobs ]))
        ~payloads:(payloads @ cleanup_payloads permissions jobs)
    in
    resolve_cleaned_permission_waiters t permissions;
    Option.iter borrow ~f:(fun _ -> t.moderator_borrow <- None);
    Option.iter event_borrow ~f:(fun borrow ->
      borrow.callback_active <- false;
      Atomic.set borrow.authority_live false;
      borrow.cancel <- None);
    Option.iter event_borrow ~f:(fun _ -> t.queued_event_borrow <- None);
    t.foreground_moderator <- None;
    t.notification_inputs <- None;
    t.idle_moderator_borrowed <- moderator_is_borrowed t;
    t.invocation_executions <- independent;
    t.active_cancel <- None;
    (match t.state.lifecycle.desired, t.state.halted with
     | Stopped, _ | _, true -> cancel_independent_moderator t
     | Running, false -> ());
    let%bind () =
      match reconcile_foreground_invocations t with
      | Ok () -> Ok ()
      | Error failure ->
        let%bind () = retain_reconciliation_failure t failure in
        Error failure
    in
    if runtime_admission_open t && outcome_requests_compaction outcome
    then
      Result.map (start_compaction ?agent_custody:compaction_custody t) ~f:(fun _ -> ())
    else Ok ()
;;

let cancel_operation_internal t attachment_id operation_id =
  let open Result.Let_syntax in
  let%bind _ = write_attachment t attachment_id in
  let%bind operation = current_operation t operation_id in
  match operation.state with
  | Completed | Failed _ | Cancelled | Interrupted _ ->
    Error (error Already_resolved "operation is already terminal")
  | Cancelling -> Ok (Session_state.summary t.state)
  | Starting | Running ->
    let operation = operation_state t operation Cancelling in
    let%bind session =
      transition
        t
        ~delta:(Session_delta.Active_operation_changed (Some operation))
        ~payloads:
          [ Agent_protocol.Event.Durable.Payload.Session_updated
              (summary_with_operation t operation)
          ]
    in
    cancel_event_for_operation t operation.id;
    Option.iter t.active_cancel ~f:(fun cancel -> cancel ());
    Ok session
;;

let worker_failure exn =
  Agent_protocol.Error.create
    Internal_error
    ~message:("foreground worker failed: " ^ Exn.to_string exn)
    ~retryable:true
    ()
;;

let publish_worker_live t buffer payload =
  let event =
    Live_event_buffer.publish
      buffer
      ~anchor_sequence:(Atomic.get t.event_sequence)
      ~timestamp:(t.services.now ())
      ~payload
  in
  match event with
  | Ok event -> broadcast_recoverable t event
  | Error error ->
    raise_s [%message "invalid worker transcript event" (error : Agent_protocol.Error.t)]
;;

let request_review_internal t ~permission ~review =
  let open Result.Let_syntax in
  let%bind response =
    call t (Open_permission (permission, None, Agent_protocol.Permission.Deny))
  in
  let choice, reason =
    match review () with
    | Ok Permission_reviewer.Decision.Allow ->
      Agent_protocol.Permission.Approve_once, None
    | Ok (Deny message) -> Agent_protocol.Permission.Deny, Some message
    | Error (error : Permission_reviewer.Error.t) ->
      Agent_protocol.Permission.Deny, Some error.message
  in
  let%bind (_ : Agent_protocol.Permission.t) =
    call
      t
      ~priority:Priority
      (Resolve_permission_system (permission.id, permission.generation, choice, reason))
  in
  Ok (Eio.Promise.await response)
;;

let review_resolution review =
  match review () with
  | Ok Permission_reviewer.Decision.Allow -> Agent_protocol.Permission.Approve_once, None
  | Ok (Deny message) -> Agent_protocol.Permission.Deny, Some message
  | Error (failure : Permission_reviewer.Error.t) ->
    Agent_protocol.Permission.Deny, Some failure.message
;;

let timeout_review t (permission : Agent_protocol.Permission.t) response seconds review =
  t.sleep seconds;
  let choice, reason = review_resolution review in
  let open Result.Let_syntax in
  let%bind (_ : Agent_protocol.Permission.t) =
    call
      t
      ~priority:Priority
      (Resolve_permission_system (permission.id, permission.generation, choice, reason))
  in
  Ok (Eio.Promise.await response)
;;

let request_permission_with_review_internal
      t
      ~permission
      ~timeout_seconds
      ~fallback
      ~review_on_timeout
  =
  let open Result.Let_syntax in
  let actor_timeout =
    if t.schedule_permission_timeouts && Option.is_none review_on_timeout
    then timeout_seconds
    else None
  in
  let%bind response = call t (Open_permission (permission, actor_timeout, fallback)) in
  match t.schedule_permission_timeouts, timeout_seconds, review_on_timeout with
  | true, Some seconds, Some review ->
    Eio.Fiber.first
      (fun () -> Ok (Eio.Promise.await response))
      (fun () -> timeout_review t permission response seconds review)
  | (false | true), _, _ -> Ok (Eio.Promise.await response)
;;

let invocation_agent_actor t execution invocation =
  let open Result.Let_syntax in
  let%bind () =
    if
      execution.accepts_children
      && Atomic.get execution.authority_live
      && List.mem t.invocation_executions execution ~equal:phys_equal
      && Agent_protocol.Invocation.equal execution.dispatched invocation
    then Ok ()
    else
      Error
        (error
           Permission_denied
           "agent authority belongs to another or expired invocation")
  in
  let%bind owner_current =
    match execution.owner with
    | Foreground id ->
      let%map _ = running_operation t id in
      fun () -> true
    | Background_job scope ->
      let%map () = job_scope_can_execute t scope in
      fun () -> Atomic.get scope.authority_live
    | Invocation_moderator borrow ->
      let%bind () = validate_moderator_borrow t borrow in
      let%map () =
        match borrow.job_scope with
        | Some scope -> job_scope_can_execute t scope
        | None ->
          (match borrow.operation_id with
           | None -> Ok ()
           | Some id -> Result.map (running_operation t id) ~f:ignore)
      in
      fun () ->
        Atomic.get borrow.authority_live
        && Option.for_all borrow.job_scope ~f:(fun scope ->
          Atomic.get scope.authority_live)
    | Event_moderator borrow ->
      let%bind () = validate_queued_event_borrow t borrow in
      let%map () =
        match borrow.job_scope with
        | Some scope -> job_scope_can_execute t scope
        | None ->
          (match borrow.receipt.context.operation_id with
           | None -> Ok ()
           | Some id -> Result.map (running_operation t id) ~f:ignore)
      in
      fun () ->
        Atomic.get borrow.authority_live
        && Option.for_all borrow.job_scope ~f:(fun scope ->
          Atomic.get scope.authority_live)
  in
  let%bind actor =
    let%bind custody = execution_agent_custody t execution in
    match custody with
    | None -> Ok None
    | Some custody -> Result.map (Execution_custody.actor custody) ~f:Option.some
  in
  Ok
    (Option.map actor ~f:(fun actor ->
       Operator_authorization.guarded
         ~principal:(Operator_authorization.principal actor)
         ~is_current:(fun () ->
           Atomic.get execution.authority_live
           && owner_current ()
           && Operator_authorization.is_current actor)))
;;

let with_invocation_claim t claim f =
  let open Result.Let_syntax in
  Eio.Fiber.yield ();
  let%bind execution = Eio.Cancel.protect (fun () -> call t claim) in
  let finish ?requests ?(commit_starts = false) ?authoring_reference outcome =
    Atomic.set execution.authority_live false;
    Eio.Cancel.protect (fun () ->
      call
        t
        (Finish_invocation
           (execution, outcome, requests, commit_starts, authoring_reference)))
  in
  let capabilities =
    Invocation_capabilities.
      { borrow_agent_actor =
          (fun ~invocation ->
            call t (Borrow_invocation_agent_actor (execution, invocation)))
      ; allocate_inference = inference_port t (Inference_invocation execution)
      ; model_jobs = model_job_port t (Inference_invocation execution)
      }
  in
  let failed =
    Agent_protocol.Invocation.Fail
      { code = "invocation.handler_failed"
      ; message = "Tool execution failed."
      ; retryable = false
      ; details = `Null
      }
  in
  let run () =
    match execution.owner with
    | Background_job _ ->
      Chat_response.Runtime_request_scope.collect (fun () ->
        f ~dispatched:execution.dispatched ~capabilities)
    | Foreground _ | Invocation_moderator _ | Event_moderator _ ->
      f ~dispatched:execution.dispatched ~capabilities, []
  in
  match Authoring_reference_scope.collect execution.dispatched.context run with
  | (Ok outcome, collected), references ->
    let outcome, collected, commit_starts =
      match Agent_protocol.Invocation.validate_outcome outcome with
      | Ok () -> outcome, collected, true
      | Error _ ->
        ( Agent_protocol.Invocation.Fail
            { code = "invocation.invalid_output"
            ; message = "Tool execution returned an invalid outcome."
            ; retryable = false
            ; details = `Null
            }
        , []
        , false )
    in
    let requests : Agent_protocol.Invocation.follow_up =
      { request_turn = Chat_response.Runtime_semantics.request_turn collected
      ; request_compaction = Chat_response.Runtime_semantics.request_compaction collected
      ; end_session = Chat_response.Runtime_semantics.should_end_session collected
      }
    in
    let authoring_reference =
      match outcome with
      | Complete value ->
        List.find references ~f:(fun reference ->
          Agent_protocol.Authoring_reference.matches_output reference value)
      | Fail _ | Pending _ | Cancelled _ -> None
    in
    finish ~requests ~commit_starts ?authoring_reference outcome
  | (Error failure, _), _ ->
    let%bind _ = finish failed in
    Error failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let outcome =
      match exn with
      | Eio.Cancel.Cancelled _ ->
        Agent_protocol.Invocation.Cancelled "tool execution cancelled"
      | _ -> failed
    in
    ignore (finish outcome : (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result);
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let callback_tool_executor t ~scope ~invocation callback =
  let open Result.Let_syntax in
  let%bind () = check_callback_current t scope in
  let%bind _ = callback_agent_actor t ~scope in
  with_invocation_claim
    t
    (Claim_event_invocation (scope.borrow, invocation))
    (fun ~dispatched ~capabilities ->
       let%bind () = check_callback_current t scope in
       callback ~dispatched ~capabilities)
;;

let callback_model_jobs t ~scope =
  let admission = model_job_port t (Inference_event scope.Callback_scope.borrow) in
  Model_job_admission.Host.create ~admit:(fun job ->
    let open Result.Let_syntax in
    let%bind () =
      match scope.constructor_scope with
      | None -> Ok ()
      | Some _ ->
        Error
          (error
             Permission_denied
             "installed callback Model admission excludes constructors")
    in
    let%bind () = check_callback_current t scope in
    Model_job_admission.admit admission job)
;;

let with_invocation t operation_id ~invocation f =
  with_invocation_claim t (Claim_invocation (operation_id, invocation)) f
;;

let with_job_scope t ~job_id ~generation ~attempt ~deadline f =
  let open Result.Let_syntax in
  let%bind scope =
    Eio.Cancel.protect (fun () ->
      call t (Claim_job_scope (job_id, generation, attempt, deadline)))
  in
  let active = Atomic.make true in
  let finish () =
    Atomic.set active false;
    Eio.Cancel.protect (fun () -> call t (Finish_job_scope scope))
  in
  let execute ~invocation callback =
    match Atomic.get active with
    | false -> Error (error Conflict "background invocation scope has ended")
    | true ->
      with_invocation_claim
        t
        (Claim_job_invocation (scope, invocation))
        (fun ~dispatched ~capabilities ->
           match Atomic.get active with
           | false -> Error (error Conflict "background invocation scope has ended")
           | true ->
             Eio.Fiber.check ();
             (match Eio.Promise.peek scope.cancelled with
              | Some () -> raise (Eio.Cancel.Cancelled Exit)
              | None ->
                Eio.Fiber.first
                  (fun () -> callback ~dispatched ~capabilities)
                  (fun () ->
                     Eio.Promise.await scope.cancelled;
                     raise (Eio.Cancel.Cancelled Exit))))
  in
  let run () =
    Eio.Cancel.sub (fun context ->
      Exn.protect
        ~finally:(fun () ->
          Atomic.set active false;
          Exn.protect
            ~finally:(fun () -> Eio.Cancel.cancel context Exit)
            ~f:(fun () ->
              Eio.Cancel.protect (fun () ->
                ignore
                  (call t (Seal_job_scope scope) : (unit, Agent_protocol.Error.t) result))))
        ~f:(fun () ->
          let%bind () =
            call
              t
              (Set_job_scope_cancel
                 ( scope
                 , fun () ->
                     match Atomic.get active with
                     | true -> Eio.Cancel.cancel context Exit
                     | false -> () ))
          in
          f ~scope ~execute))
  in
  match run () with
  | result ->
    let%bind () = finish () in
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    ignore (finish () : (unit, Agent_protocol.Error.t) result);
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let run_moderator_callback t borrow f =
  let open Result.Let_syntax in
  let commit ~resolved ~snapshot =
    Eio.Cancel.protect (fun () ->
      call t (Commit_moderator_invocation (borrow, resolved, snapshot)))
  in
  let finish failure =
    Atomic.set borrow.authority_live false;
    Exn.protect
      ~finally:(fun () ->
        Option.iter borrow.observation ~f:Observation_continuation.retire)
      ~f:(fun () ->
        Eio.Cancel.protect (fun () ->
          call t (Finish_moderator_invocation (borrow, failure))))
  in
  let capabilities =
    Invocation_capabilities.
      { borrow_agent_actor =
          (fun ~invocation -> call t (Borrow_moderator_agent_actor (borrow, invocation)))
      ; allocate_inference = inference_port t (Inference_moderator borrow)
      ; model_jobs = model_job_port t (Inference_moderator borrow)
      }
  in
  let failed message =
    let message =
      if String.is_empty message || String.length message > 16_384
      then "moderator handler failed; diagnostic is outside the invocation message limit"
      else message
    in
    Agent_protocol.Invocation.Fail
      { code = "invocation.handler_failed"; message; retryable = false; details = `Null }
  in
  let run () =
    match borrow.kind with
    | Observation -> f ~dispatched:borrow.invocation ~capabilities ~commit
    | Invocation ->
      Authoring_reference_scope.with_scope borrow.invocation.context (fun scope ->
        let commit ~resolved ~snapshot =
          let%bind resolved = Authoring_reference_scope.annotate scope resolved in
          commit ~resolved ~snapshot
        in
        f ~dispatched:borrow.invocation ~capabilities ~commit)
  in
  match run () with
  | result ->
    let failure =
      match result with
      | Ok () -> None
      | Error (failure : Agent_protocol.Error.t) -> Some (failed failure.message)
    in
    let%bind () = finish failure in
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let failure =
      match exn with
      | Eio.Cancel.Cancelled _ ->
        Agent_protocol.Invocation.Cancelled "moderator handler cancelled"
      | _ -> failed (Exn.to_string exn)
    in
    ignore (finish (Some failure) : (unit, Agent_protocol.Error.t) result);
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let run_moderator_borrow t (borrow : moderator_borrow) f =
  match borrow.operation_id with
  | Some _ -> run_moderator_callback t borrow f
  | None ->
    (* A protected claim can return into a cancelled parent. Enter completion
       handling before [sub], which can reject that parent before running its body. *)
    run_moderator_callback t borrow (fun ~dispatched ~capabilities ~commit ->
      Eio.Cancel.sub (fun context ->
        let active = ref true in
        (* Completion clears the actor's cancel callback after this context ends. *)
        Exn.protect
          ~finally:(fun () -> active := false)
          ~f:(fun () ->
            let open Result.Let_syntax in
            let%bind () =
              call
                t
                (Set_idle_moderator_cancel
                   (borrow, fun () -> if !active then Eio.Cancel.cancel context Exit))
            in
            f ~dispatched ~capabilities ~commit)))
;;

let with_moderator_borrow_unlocked t ~claim f =
  let open Result.Let_syntax in
  (* Once admitted, always reach protected completion even during cancellation. *)
  Eio.Fiber.yield ();
  let%bind borrow = Eio.Cancel.protect (fun () -> call t claim) in
  run_moderator_borrow t borrow f
;;

let with_moderator_gate t f =
  match Chat_response.Execution_gate.with_access t.invocation_gate f with
  | Ok result -> result
  | Error failure ->
    let code =
      match failure with
      | Chat_response.Execution_gate.Resource_limit -> Agent_protocol.Error.Resource_limit
      | Reentrant | Wait_cycle -> Conflict
    in
    Error (error code (Chat_response.Execution_gate.error_message failure))
;;

let with_moderator_checkpoint = with_moderator_gate

let run_queued_event_borrow t borrow f =
  let open Result.Let_syntax in
  let finish interrupted =
    Eio.Cancel.protect (fun () -> call t (Finish_queued_event (borrow, interrupted)))
  in
  let finish_after_failure interrupted =
    let report message =
      (* The primary callback failure remains the caller's exception/result.
         Cleanup failure is secondary evidence, never a replacement outcome. *)
      try Eio.traceln "queued moderator cleanup failed: %s" message with
      | _ -> ()
    in
    match finish interrupted with
    | Ok () -> ()
    | Error failure -> report failure.message
    | exception cleanup -> report (Exn.to_string cleanup)
  in
  let execute () =
    Eio.Cancel.sub (fun context ->
      let active = ref true in
      Exn.protect
        ~finally:(fun () -> active := false)
        ~f:(fun () ->
          let%bind () =
            call
              t
              (Set_queued_event_cancel
                 ( borrow
                 , fun () ->
                     match !active with
                     | true -> Eio.Cancel.cancel context Exit
                     | false -> () ))
          in
          f
            ~borrow
            ~event:borrow.event
            ~commit:(fun ~decision ~notifications ~snapshot ~requests ->
              Eio.Cancel.protect (fun () ->
                call
                  t
                  (Commit_queued_event
                     (borrow, snapshot, requests, decision, notifications))))))
  in
  match execute () with
  | Error failure ->
    finish_after_failure false;
    Error failure
  | Ok () ->
    let%bind () = finish false in
    (match borrow.committed with
     | true -> Ok true
     | false ->
       Error (error Invalid_state "queued event returned without a checkpoint commit"))
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let interrupted =
      match exn with
      | Eio.Cancel.Cancelled _ -> true
      | _ -> false
    in
    finish_after_failure interrupted;
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let with_queued_event_borrow t ~claim f =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    Eio.Fiber.yield ();
    let%bind claim = claim () in
    let%bind claimed =
      match claim with
      | None -> Ok None
      | Some claim -> Eio.Cancel.protect (fun () -> call t claim)
    in
    match claimed with
    | None -> Ok false
    | Some borrow ->
      run_queued_event_borrow t borrow (fun ~borrow ~event ~commit ->
        f ~borrow ~event ~commit:(fun ~snapshot ~requests ->
          commit ~decision:None ~notifications:[] ~snapshot ~requests)))
;;

let with_delegated_moderator_event
      ?(notifications = fun () -> [])
      t
      ~delegation
      ~event
      ~authorize
      ~snapshot
      f
  =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    let%bind () = authorize () in
    let%bind snapshot = snapshot () in
    let%bind claimed =
      Eio.Cancel.protect (fun () ->
        call
          t
          (Claim_delegated_event
             (Agent_protocol.Id.Moderator_execution.create (), delegation, snapshot, event)))
    in
    match claimed with
    | Delegated_unavailable -> Ok None
    | Delegated_replay previous ->
      let%map () = authorize () in
      Some previous
    | Delegated_borrow borrow ->
      let%bind _ =
        run_queued_event_borrow t borrow (fun ~borrow ~event ~commit ->
          let active = Atomic.make true in
          let check () =
            let%bind () = authorize () in
            match Atomic.get active with
            | false ->
              Error (error Conflict "delegated policy invocation scope has ended")
            | true -> Ok ()
          in
          let execute ~invocation callback =
            let%bind () = check () in
            let%bind result =
              with_invocation_claim
                t
                (Claim_event_invocation (borrow, invocation))
                (fun ~dispatched ~capabilities ->
                   let%bind () = check () in
                   callback ~dispatched ~capabilities)
            in
            (* Keep the native outcome even if the delegated caller loses access
               while it runs. Rechecking inside the callback after the effect
               would discard known results before the actor could persist them. *)
            let%map () = check () in
            result
          in
          Exn.protect
            ~finally:(fun () -> Atomic.set active false)
            ~f:(fun () ->
              let%bind () = check () in
              f
                ~executing:borrow.receipt
                ~model_jobs:(model_job_port t (Inference_event borrow))
                ~allocate_inference:(inference_port t (Inference_event borrow))
                ~event
                ~execute
                ~commit:(fun ~decision ~snapshot ~requests ->
                  let%bind () = check () in
                  commit
                    ~decision:(Some decision)
                    ~notifications:(notifications ())
                    ~snapshot
                    ~requests)))
      in
      let%bind state = call t State in
      let%bind () = authorize () in
      List.find state.moderator_executions ~f:(fun receipt ->
        Agent_protocol.Id.Moderator_execution.equal
          receipt.context.id
          borrow.receipt.context.id)
      |> Result.of_option
           ~error:(error Internal_error "delegated policy receipt disappeared")
      |> Result.map ~f:Option.some)
;;

let with_legacy_callback_parent t parent ~callback f =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    let%bind scope =
      Eio.Cancel.protect (fun () -> call t (Claim_legacy_callback (callback, parent)))
    in
    match scope with
    | None -> Ok false
    | Some scope ->
      run_queued_event_borrow t scope.borrow (fun ~borrow:_ ~event:_ ~commit ->
        let%bind () = check_callback_current t scope in
        f ~scope ~commit:(fun ~snapshot ~requests ~notifications ->
          commit ~decision:None ~notifications ~snapshot ~requests)))
;;

let with_initialization_callback t ~scope ~manager ~callback f =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    let%bind callback_scope =
      Eio.Cancel.protect (fun () ->
        call t (Claim_initialization_callback (scope, manager, callback)))
    in
    match callback_scope with
    | None -> Ok false
    | Some scope ->
      run_queued_event_borrow t scope.borrow (fun ~borrow:_ ~event:_ ~commit ->
        let%bind () = check_callback_current t scope in
        f ~scope ~commit:(fun ~snapshot ~requests ~notifications ->
          commit ~decision:None ~notifications ~snapshot ~requests)))
;;

let with_legacy_callback t ~operation_id =
  with_legacy_callback_parent t (Callback_operation operation_id)
;;

let with_legacy_queued_callback t ~callback ~operation_id f =
  match Chat_response.Moderator_manager.Callback.selection callback with
  | Boundary _ ->
    Error (error Invalid_request "legacy queued callback requires an actual Queue_head")
  | Queue_head -> with_legacy_callback t ~operation_id ~callback f
;;

let with_idle_queued_moderator_event t ~snapshot f =
  with_queued_event_borrow
    t
    ~claim:(fun () ->
      Ok
        (Some
           (Claim_queued_event
              (Agent_protocol.Id.Moderator_execution.create (), None, snapshot))))
    (fun ~borrow:_ ~event ~commit -> f ~event ~commit)
;;

let with_queued_moderator_retirement t ~id ~snapshot ~reason f =
  with_queued_event_borrow
    t
    ~claim:(fun () -> Ok (Some (Claim_queued_retirement (id, snapshot, reason))))
    (fun ~borrow:_ ~event ~commit ->
       f ~event ~commit:(fun ~snapshot ->
         commit
           ~snapshot
           ~requests:
             { request_turn = false; request_compaction = false; end_session = None }))
;;

let with_event_tools t ~claim f =
  with_queued_event_borrow t ~claim (fun ~borrow ~event ~commit ->
    let active = Atomic.make true in
    let execute ~invocation callback =
      match Atomic.get active with
      | false -> Error (error Conflict "event invocation scope has ended")
      | true ->
        with_invocation_claim t (Claim_event_invocation (borrow, invocation)) callback
    in
    Exn.protect
      ~finally:(fun () -> Atomic.set active false)
      ~f:(fun () ->
        f
          ~executing:borrow.receipt
          ~model_jobs:(model_job_port t (Inference_event borrow))
          ~allocate_inference:(inference_port t (Inference_event borrow))
          ~retirement_reason:borrow.retirement_reason
          ~event
          ~execute
          ~commit))
;;

let with_queued_moderator_event_tools t ~operation_id ~snapshot f =
  with_event_tools
    t
    ~claim:(fun () ->
      Ok
        (Some
           (Claim_queued_event
              (Agent_protocol.Id.Moderator_execution.create (), operation_id, snapshot))))
    f
;;

let with_idle_queued_moderator_event_tools t =
  with_queued_moderator_event_tools t ~operation_id:None
;;

let with_ordinary_moderator_event t ~operation_id ~snapshot ~event f =
  with_event_tools
    t
    ~claim:(fun () ->
      Ok
        (Some
           (Claim_ordinary_event
              ( Agent_protocol.Id.Moderator_execution.create ()
              , operation_id
              , snapshot
              , event ))))
    f
;;

let with_current_moderator_event t ~operation_id ~event ~snapshot f =
  with_event_tools
    t
    ~claim:(fun () ->
      Result.map (snapshot ()) ~f:(fun snapshot ->
        Some
          (Claim_ordinary_event
             ( Agent_protocol.Id.Moderator_execution.create ()
             , operation_id
             , snapshot
             , event ))))
    f
;;

type job_execution =
  { job : Agent_protocol.Job.t
  ; allocate_inference : Inference_allocation.Port.t
  ; model_jobs : Model_job_admission.t
  ; execute : Native_tool_invocation.executor
  ; moderator_execute : Native_tool_invocation.moderator_executor
  ; claim_event : event:Chat_response.Moderation.Event.t -> Moderator_event.claim
  ; claim_legacy_callback : legacy_callback_claim
  }

let with_job_execution t ~job_id ~generation ~attempt ~deadline f =
  with_job_scope t ~job_id ~generation ~attempt ~deadline (fun ~scope ~execute ->
    let claim_event ~event ~snapshot callback =
      with_event_tools
        t
        ~claim:(fun () ->
          Result.map (snapshot ()) ~f:(fun snapshot ->
            Some
              (Claim_job_event
                 (scope, Agent_protocol.Id.Moderator_execution.create (), snapshot, event))))
        callback
    in
    let moderator_execute ~invocation callback =
      with_moderator_gate t (fun () ->
        with_moderator_borrow_unlocked
          t
          ~claim:(Claim_job_moderator (scope, invocation))
          callback)
    in
    let claim_legacy_callback = with_legacy_callback_parent t (Callback_job scope) in
    f
      { job = scope.job
      ; allocate_inference = inference_port t (Inference_job scope)
      ; model_jobs = model_job_port t (Inference_job scope)
      ; execute
      ; moderator_execute
      ; claim_event
      ; claim_legacy_callback
      })
;;

let with_job_invocations t ~job_id ~generation ~attempt ~deadline f =
  with_job_execution t ~job_id ~generation ~attempt ~deadline (fun execution ->
    f ~job:execution.job ~execute:execution.execute)
;;

let with_current_queued_moderator_event_tools t ~operation_id ~snapshot f =
  with_event_tools
    t
    ~claim:(fun () ->
      Result.map (snapshot ()) ~f:(fun snapshot ->
        match
          snapshot.Session.Moderator_state.Identity_snapshot.queued_internal_events
        with
        | [] -> None
        | _ ->
          Some
            (Claim_queued_event
               (Agent_protocol.Id.Moderator_execution.create (), operation_id, snapshot))))
    f
;;

let reconcile_run_recovery t = call t Reconcile_run_recovery
let run_actions t executing = call t (Run_actions executing)

let with_current_idle_queued_moderator_event_tools t =
  with_current_queued_moderator_event_tools t ~operation_id:None
;;

let with_moderator_invocation t operation_id ~invocation f =
  with_moderator_gate t (fun () ->
    with_moderator_borrow_unlocked
      t
      ~claim:(Claim_moderator_invocation (operation_id, invocation))
      f)
;;

let with_moderator_observation t operation_id ~invocation_id f =
  with_moderator_gate t (fun () ->
    with_moderator_borrow_unlocked
      t
      ~claim:(Claim_moderator_observation (operation_id, invocation_id))
      (fun ~dispatched ~capabilities ~commit ->
         f ~observing:dispatched ~capabilities ~commit))
;;

let with_selected_moderator_observation t claim f =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    Eio.Fiber.yield ();
    let%bind borrow = Eio.Cancel.protect (fun () -> call t claim) in
    match borrow with
    | None -> Ok false
    | Some borrow ->
      let%map () =
        run_moderator_borrow t borrow (fun ~dispatched ~capabilities ~commit ->
          f ~observing:dispatched ~capabilities ~commit)
      in
      true)
;;

let with_next_moderator_observation t operation_id ~observer f =
  with_selected_moderator_observation
    t
    (Claim_next_moderator_observation (operation_id, observer))
    f
;;

let with_idle_moderator_observation t ~observer f =
  with_selected_moderator_observation
    t
    (Claim_idle_moderator_observation (observer, false))
    f
;;

let with_idle_moderator_observation_tools t ~observer f =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    Eio.Fiber.yield ();
    let%bind borrow =
      Eio.Cancel.protect (fun () ->
        call t (Claim_idle_moderator_observation (observer, true)))
    in
    match borrow with
    | None -> Ok false
    | Some borrow ->
      let active = Atomic.make true in
      let execute ~invocation callback =
        match Atomic.get active with
        | false -> Error (error Conflict "idle invocation scope has ended")
        | true ->
          with_invocation_claim t (Claim_idle_invocation (borrow, invocation)) callback
      in
      let%map () =
        run_moderator_borrow t borrow (fun ~dispatched ~capabilities ~commit ->
          Exn.protect
            ~f:(fun () -> f ~observing:dispatched ~capabilities ~execute ~commit)
            ~finally:(fun () -> Atomic.set active false))
      in
      true)
;;

let worker_capabilities t operation live id_source buffer =
  let operation_id = operation.Agent_protocol.Operation.id in
  Operation_worker.Capabilities.
    { id_source =
        History_id_source.as_history_entry_source
          ~committed_through:(fun () ->
            Result.map (call t State) ~f:(fun state ->
              state.conversation.reserved_history_through))
          id_source
    ; allocate_inference = inference_port t (Inference_operation (operation, live))
    ; original_agent_actor =
        (fun () -> call t (Borrow_operation_agent_actor operation_id))
    ; commit_entry = (fun entry -> call t (Commit_worker_entry (operation_id, entry)))
    ; prepare_authoring_input =
        (fun materialization ~history ~effective ->
          Eio.Cancel.protect (fun () ->
            call
              t
              (Prepare_authoring_input (operation_id, materialization, history, effective))))
    ; commit_invocation_call =
        (fun ~invocation entry ->
          Eio.Cancel.protect (fun () ->
            call t (Commit_invocation_call (operation_id, invocation, entry))))
    ; publish_invocation_output =
        (fun ~invocation_id entry ->
          Eio.Cancel.protect (fun () ->
            call t (Publish_invocation_output (operation_id, invocation_id, entry))))
    ; commit_moderator =
        (fun snapshot -> call t (Commit_worker_moderator (operation_id, snapshot)))
    ; with_moderator_invocation = with_moderator_invocation t operation_id
    ; with_moderator_observation = with_moderator_observation t operation_id
    ; with_next_moderator_observation = with_next_moderator_observation t operation_id
    ; run_actions = (fun executing -> call t (Run_actions executing))
    ; with_moderator_event =
        (fun ~snapshot ~event ->
          with_current_moderator_event
            t
            ~operation_id:(Some operation_id)
            ~event
            ~snapshot)
    ; with_queued_moderator_event =
        with_current_queued_moderator_event_tools t ~operation_id:(Some operation_id)
    ; manage_moderator_follow_up =
        (fun ~observer -> call t (Manage_moderator_follow_up (operation_id, observer)))
    ; admit_moderator_turn = (fun () -> call t (Admit_moderator_turn operation_id))
    ; admit_notification_turn = (fun () -> call t (Admit_notification_turn operation_id))
    ; with_invocation = with_invocation t operation_id
    ; root_context =
        Option.map t.configuration_policy ~f:(fun _ ->
          root_configuration_context t operation_id)
    ; consume_deferred = (fun () -> call t (Consume_deferred operation_id))
    ; request_permission =
        (fun ~permission ~timeout_seconds ~fallback ~review_on_timeout ->
          request_permission_with_review_internal
            t
            ~permission
            ~timeout_seconds
            ~fallback
            ~review_on_timeout)
    ; request_review =
        (fun ~permission ~review -> request_review_internal t ~permission ~review)
    ; responder_available =
        (fun () ->
          call t Has_writer_attachment |> Result.ok |> Option.value ~default:false)
    ; invocation_granted =
        (fun ~tool_name ~identity_digest ->
          call t (Invocation_granted (tool_name, identity_digest))
          |> Result.ok
          |> Option.value ~default:false)
    ; publish_live = publish_worker_live t buffer
    }
;;

let run_worker_operation worker input capabilities cancelled =
  match
    Eio.Fiber.first
      (fun () ->
         Eio.Switch.run (fun sw ->
           (* The readiness acknowledgement may have cancelled a Starting
              operation. Do not dispatch its synchronous prefix before the
              competing cancellation fiber has a chance to run. *)
           match Eio.Promise.peek cancelled with
           | Some () -> `Cancelled
           | None -> `Finished (Operation_worker.run worker ~sw ~input capabilities)))
      (fun () ->
         Eio.Promise.await cancelled;
         `Cancelled)
  with
  | `Finished outcome -> outcome
  | `Cancelled -> Operation_worker.Cancelled { reason = "operation cancelled" }
;;

let run_worker t worker operation live history =
  let operation_id = operation.Agent_protocol.Operation.id in
  let id_source =
    History_id_source.create
      ~namespace:(Agent_protocol.Id.Session.to_string t.state.identity.session_id)
      ~block_size:64
      ~reserve:(fun ~count -> call t (Reserve_history_block count))
  in
  match id_source with
  | Error failure -> Operation_worker.Failed failure
  | Ok id_source ->
    let buffer =
      Live_event_buffer.create
        ~capacity:2_048
        ~session_id:t.state.identity.session_id
        ~operation_id
    in
    let capabilities = worker_capabilities t operation live id_source buffer in
    let input =
      Operation_worker.Input.
        { session_id = t.state.identity.session_id
        ; session_generation = t.state.identity.generation
        ; operation
        ; history
        }
    in
    let cancelled, cancel_resolver = Eio.Promise.create () in
    let cancel () = ignore (Eio.Promise.try_resolve cancel_resolver ()) in
    (try
       match call t ~priority:Priority (Worker_ready (operation_id, cancel)) with
       | Error failure -> Operation_worker.Failed failure
       | Ok () -> run_worker_operation worker input capabilities cancelled
     with
     | Eio.Cancel.Cancelled reason ->
       Operation_worker.Cancelled { reason = Exn.to_string reason }
     | exn -> Operation_worker.Failed (worker_failure exn))
;;

let launch_worker t operation =
  let operation_id = operation.Agent_protocol.Operation.id in
  (* Capture the physical worker's cell, not a later operation's mutable slot.
     A failed terminal save cannot extend the callback that already returned. *)
  let custody =
    Option.bind t.operation_agent_custody ~f:(fun (owner, admission) ->
      if Agent_protocol.Id.Operation.equal owner operation_id
      then Some admission
      else None)
  in
  let inference_live = Atomic.make true in
  let retire_authority () =
    Atomic.set inference_live false;
    Option.iter custody ~f:retire_agent_admission
  in
  let history =
    let open Result.Let_syntax in
    let%bind () = reconcile_foreground_invocations t in
    History_codec.all_of_protocol t.state.conversation.canonical_history
  in
  match history with
  | Error failure ->
    retire_authority ();
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      ignore
        (call t ~priority:Priority (Worker_terminal (operation_id, Failed failure, None))
         : (unit, Agent_protocol.Error.t) result))
  | Ok history ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let compaction_custody = ref None in
      let outcome =
        Exn.protect ~finally:retire_authority ~f:(fun () ->
          let outcome =
            match t.operation_worker with
            | Some worker -> run_worker t worker operation inference_live history
            | None ->
              Operation_worker.Failed
                (error Invalid_state "session has no foreground operation worker")
          in
          (* This private completion handoff is staged while the exact physical
             worker remains live. Only this Completed result may consume it, and
             only after its terminal transition ACK. It is never a new borrower
             of an already-retired operation. *)
          if outcome_requests_compaction outcome
          then
            Option.iter
              (Option.bind custody ~f:(fun admission ->
                 Execution_custody.Admission.custody admission))
              ~f:(fun custody ->
                match transfer_agent_custody custody with
                | Ok child -> compaction_custody := Some child
                | Error _ ->
                  (* Preserve known-but-unavailable provenance. The parent cell
                   retires below and the actor must reject this handoff, even
                   when no instructions are currently selected. None is only
                   genuinely unbound legacy execution. *)
                  compaction_custody := Some custody);
          outcome)
      in
      Exn.protect
        ~f:(fun () ->
          ignore
            (call
               t
               ~priority:Priority
               (Worker_terminal (operation_id, outcome, !compaction_custody))
             : (unit, Agent_protocol.Error.t) result))
        ~finally:(fun () -> Option.iter !compaction_custody ~f:retire_agent_custody))
;;

let create_turn_operation t reason =
  let timestamp = t.services.now () in
  Agent_protocol.Operation.
    { id = Agent_protocol.Id.Operation.create ()
    ; generation = t.state.identity.generation
    ; kind = Turn reason
    ; state = Starting
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let prune_pending_agent_custody t =
  t.pending_agent_custody
  <- List.filter t.pending_agent_custody ~f:(fun (entry, custody) ->
       let retained =
         List.exists t.state.conversation.deferred_user_entries ~f:(fun pending ->
           equal_agent_entry entry (Pending_input_document.entry pending))
       in
       if not retained then retire_agent_custody custody;
       retained)
;;

let remember_pending_agent_actor t entry original_actor =
  prune_pending_agent_custody t;
  Option.iter original_actor ~f:(fun actor ->
    if
      List.exists t.state.conversation.deferred_user_entries ~f:(fun pending ->
        equal_agent_entry entry (Pending_input_document.entry pending))
    then
      t.pending_agent_custody
      <- (entry, create_agent_custody actor) :: t.pending_agent_custody)
;;

let pending_adoption_agent_custody t entries =
  match entries with
  | [ entry ] ->
    (match
       List.find t.pending_agent_custody ~f:(fun (pending, _) ->
         equal_agent_entry entry pending)
     with
     | None -> Ok None
     | Some (_, custody) -> Result.map (transfer_agent_custody custody) ~f:Option.some)
  | [] | _ :: _ :: _ -> Ok None
;;

let admission_agent_custody t admission =
  Turn_admission.payloads admission
  |> List.concat_map ~f:(function
    | Agent_protocol.Event.Durable.Payload.History_appended entries -> entries
    | _ -> [])
  |> pending_adoption_agent_custody t
;;

let adopted_agent_custody t entries incoming original_actor =
  match entries with
  | [ entry ] when equal_agent_entry entry incoming ->
    Ok (Option.map original_actor ~f:create_agent_custody)
  | _ -> pending_adoption_agent_custody t entries
;;

let check_turn_custody t custody =
  let open Result.Let_syntax in
  let%bind () = require_runtime_admission t in
  let%bind () =
    Option.value_map custody ~default:(Ok ()) ~f:(fun custody ->
      Execution_custody.actor custody |> Result.map ~f:ignore)
  in
  require_runtime_admission t
;;

let submit_idle_message
      ?original_actor
      ?run_preparation
      ?commit_attempted
      ?(extra_deltas = [])
      ?(owner = Pending_input_document.Owner.Host_internal)
      ?(timing = Agent_protocol.Pending_input.Timing.Safe_boundary)
      ?operation
      t
      entry
  =
  let open Result.Let_syntax in
  let%bind () = require_runtime_admission t in
  let%bind () = reconcile_foreground_invocations t in
  let%bind enqueue = prepare_pending_inputs t ~owner timing [ entry ] in
  let%bind queued = Pending_plan.apply (Pending_transition.plan enqueue) t.state in
  let retention = Pending_disposition.Retention.default in
  let%bind adoption =
    Pending_transition.prepare
      queued
      ~change:
        (Adopt
           { boundary = Idle_start; runtime_admission_open = runtime_admission_open t })
      ~retention
      ~archive:None
      ~limits:Session_delta.native_limits
  in
  let entries = Pending_plan.adopted_entries (Pending_transition.plan adoption) in
  let%bind operation_custody =
    match operation, original_actor with
    | Some _, Some actor -> Ok (Some (create_agent_custody actor))
    | Some _, None | None, _ -> adopted_agent_custody t entries entry original_actor
  in
  let accepted =
    List.exists entries ~f:(fun candidate ->
      Agent_protocol.History.Id.equal candidate.id entry.id)
  in
  let deferred_payloads =
    if accepted
    then []
    else [ Agent_protocol.Event.Durable.Payload.History_message_deferred entry ]
  in
  let%bind () =
    if Option.is_some operation && not accepted
    then
      Error
        (error Pending_input_conflict "run input is blocked by the pending FIFO prefix")
    else Ok ()
  in
  if List.is_empty entries
  then (
    let%map session =
      transition
        ?run_preparation
        ?commit_attempted
        t
        ~delta:(Session_delta.Batch (extra_deltas @ [ Pending_transition.delta enqueue ]))
        ~payloads:deferred_payloads
    in
    remember_pending_agent_actor t entry original_actor;
    { session; history_id = entry.id; disposition = Deferred; operation_id = None })
  else (
    let operation =
      Option.value_or_thunk operation ~default:(fun () ->
        create_turn_operation t User_submit)
    in
    let lifecycle = lifecycle_for_operation t operation.id in
    let delta =
      Session_delta.Batch
        (extra_deltas
         @ [ Pending_transition.delta enqueue
           ; Pending_transition.delta adoption
           ; Active_operation_changed (Some operation)
           ; Lifecycle_changed lifecycle
           ])
    in
    let payloads =
      deferred_payloads
      @ [ Agent_protocol.Event.Durable.Payload.History_appended entries
        ; Operation_started operation
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ]
    in
    let%map session =
      transition
        ?run_preparation
        ?commit_attempted
        ~check_before_commit:(fun () ->
          let%bind () = check_turn_custody t operation_custody in
          match run_preparation with
          | None -> Ok ()
          | Some preparation ->
            let%bind () =
              Run_preparation.check_current
                preparation
                ~owner:t.run_preparation_owner
                ~state:t.state
            in
            Run_preparation.check_basis
              preparation
              ~owner:t.run_preparation_owner
              ~state:t.state)
        t
        ~delta
        ~payloads
    in
    install_operation_agent_custody t operation operation_custody;
    remember_pending_agent_actor t entry original_actor;
    launch_worker t operation;
    { session
    ; history_id = entry.id
    ; disposition = (if accepted then Started else Deferred)
    ; operation_id = Option.some_if accepted operation.id
    })
;;

let submit_deferred_message
      ?original_actor
      ?(extra_deltas = [])
      ?(owner = Pending_input_document.Owner.Host_internal)
      ?(timing = Agent_protocol.Pending_input.Timing.Safe_boundary)
      t
      entry
  =
  let open Result.Let_syntax in
  let%map session = defer_history ~extra_deltas ~owner ~timing t [ entry ] in
  remember_pending_agent_actor t entry original_actor;
  { session; history_id = entry.id; disposition = Deferred; operation_id = None }
;;

let submit_authorized_message
      ?original_actor
      ?(extra_deltas = [])
      ?(owner = Pending_input_document.Owner.Host_internal)
      ?(timing = Agent_protocol.Pending_input.Timing.Safe_boundary)
      t
      entry
  =
  if not (runtime_admission_open t)
  then Error (error Conflict "session runtime is retiring")
  else if t.state.halted
  then Error (error Invalid_state "session is halted")
  else if Option.is_some t.state.failure
  then Error (error Invalid_state "session has failed")
  else if
    not (Agent_protocol.Session.equal_desired_state t.state.lifecycle.desired Running)
  then Error (error Invalid_state "session is not running")
  else (
    match
      t.idle_moderator_borrowed, t.state.active_operation, t.state.lifecycle.observed
    with
    | true, _, _ ->
      submit_deferred_message ?original_actor ~extra_deltas ~owner ~timing t entry
    | false, Some _, _
    | false, None, (Running_turn _ | Compacting _ | Waiting_for_permission _) ->
      submit_deferred_message ?original_actor ~extra_deltas ~owner ~timing t entry
    | false, None, Idle ->
      submit_idle_message ?original_actor ~extra_deltas ~owner ~timing t entry
    | ( false
      , None
      , (Stopped | Queued_for_slot | Starting | Recovering | Stopping | Failed _) ) ->
      Error (error Invalid_state "session is not ready to accept a turn"))
;;

let submit_message ?original_actor t attachment_id submitting_principal timing entry =
  Result.bind (write_attachment t attachment_id) ~f:(fun _ ->
    submit_authorized_message
      ?original_actor
      ~owner:(Submitting_principal submitting_principal)
      ~timing
      t
      entry)
;;

let check_run_caller ?preparation t caller =
  match caller with
  | Run_admission_request.Caller.Writer attachment_id ->
    Result.map (write_attachment t attachment_id) ~f:ignore
  | Headless_command ->
    (match preparation with
     | Some preparation -> check_run_preparation t preparation
     | None -> Error (error Permission_denied "headless admission requires actor custody"))
;;

let begin_run_preparation_internal
      ?original_actor
      t
      ~authorize
      ~principal_id
      ~request
      ~request_sha256
  =
  let open Result.Let_syntax in
  let%bind () = authorize t.state in
  let%bind () =
    match request.Run_admission_request.caller with
    | Writer attachment_id -> Result.map (write_attachment t attachment_id) ~f:ignore
    | Headless_command -> Ok ()
  in
  let%bind () =
    if Agent_protocol.Id.Session.equal request.session_id t.state.identity.session_id
    then Ok ()
    else Error (error Invalid_request "run preparation targets another session")
  in
  let%bind receipt =
    Run_state.receipt
      (Option.value t.state.run_state ~default:Run_state.empty)
      ~principal_id
      ~key:request.key
      ~request_sha256
  in
  match receipt with
  | Some ({ kind = Admission; _ } as receipt) ->
    Ok (Run_preparation.Decision.Retained receipt)
  | Some { kind = Action | Terminal; _ } ->
    Error (error Conflict "run start key belongs to another action")
  | None ->
    (match t.run_preparation with
     | Some _ -> Error (error Conflict "another run admission is being prepared")
     | None ->
       let%map preparation =
         (match original_actor with
          | None -> Run_preparation.create_request
          | Some actor ->
            fun ~owner ~state ~principal_id:_ ~request ~request_sha256 ~authorize ->
              Run_preparation.create_authenticated_request
                ~owner
                ~state
                ~actor
                ~request
                ~request_sha256
                ~authorize)
           ~owner:t.run_preparation_owner
           ~state:t.state
           ~principal_id
           ~request
           ~request_sha256
           ~authorize
       in
       t.run_preparation <- Some preparation;
       Run_preparation.Decision.Prepare preparation)
;;

let admit_run
      ?preparation
      ?commit_attempted
      t
      scope
      (request : Run_admission_request.t)
      session
      request_sha256
      entry
  =
  let open Result.Let_syntax in
  let%bind () = Run_admission.Scope.authorize scope t.state in
  let%bind () =
    if
      Agent_protocol.Id.Session.equal request.session_id t.state.identity.session_id
      && Agent_protocol.Id.Session.equal
           (Agent_protocol.Session_ref.session_id session)
           t.state.identity.session_id
    then Ok ()
    else Error (error Invalid_request "run request does not identify this session")
  in
  let%bind () = check_run_caller ?preparation t request.caller in
  let index = Option.value t.state.run_state ~default:Run_state.empty in
  let%bind retained =
    Run_state.receipt
      index
      ~principal_id:(Run_admission.Scope.principal_id scope)
      ~key:request.key
      ~request_sha256
  in
  match retained with
  | Some receipt ->
    (match receipt.kind with
     | Admission -> Ok receipt
     | Action | Terminal ->
       Error (error Conflict "run start key belongs to another action"))
  | None ->
    let%bind () =
      match request.input with
      | User_submission _ -> Ok ()
      | Authored_start ->
        Authored_run_acknowledgement.require_unadmitted t.lifecycle_agent_custody
    in
    let%bind () =
      match preparation with
      | None -> Ok ()
      | Some preparation -> check_run_preparation t preparation
    in
    let%bind () = require_runtime_admission t in
    let%bind operation =
      match request.input, entry with
      | User_submission _, Some _ ->
        let%bind () = reconcile_foreground_invocations t in
        (match
           ( t.state.lifecycle.desired
           , t.state.lifecycle.observed
           , t.state.active_operation
           , t.state.failure )
         with
         | Running, Idle, None, None when not (t.state.halted || t.idle_moderator_borrowed)
           -> Ok (Some (create_turn_operation t User_submit))
         | _ -> Error (error Conflict "run user input requires an available idle session"))
      | Authored_start, None -> Ok None
      | User_submission _, None | Authored_start, Some _ ->
        Error (error Invalid_request "run input does not match its host-prepared history")
    in
    let%bind admission =
      let run_id = Agent_protocol.Id.Run.create () in
      match preparation with
      | None ->
        Run_admission.prepare_request
          t.state
          ~scope
          ~request
          ~session
          ~run_id
          ~operation
          ~request_sha256
          ~now:(t.services.now ())
      | Some preparation ->
        Run_admission.prepare_from_preparation
          t.state
          ~preparation
          ~owner:t.run_preparation_owner
          ~scope
          ~session
          ~run_id
          ~operation
          ~now:(t.services.now ())
    in
    let run = Run_admission.run admission in
    let delta = Session_delta.Run_state_changed (Run_admission.index admission) in
    let%bind () =
      match operation, entry with
      | Some operation, Some entry ->
        let%bind authorities =
          Run_authorities.add_for_caller
            t.run_authorities
            ~run
            ~scope
            ~caller:request.caller
        in
        let outcome =
          submit_idle_message
            ?original_actor:(Run_admission.Scope.original_actor scope)
            ?run_preparation:preparation
            ?commit_attempted
            ~extra_deltas:[ delta ]
            ~operation
            ~owner:(Submitting_principal (Run_admission.Scope.principal_id scope))
            t
            entry
          |> Result.map ~f:(fun _ -> ())
        in
        (* Preserve the existing user-input authority path. Authored startup
           installs its separate plan at the FIRST durable lifecycle ACK. *)
        Option.iter t.state.run_state ~f:(fun index ->
          match Run_state.find index run.id with
          | Some committed when Agent_protocol.Run.equal committed run ->
            t.run_authorities <- authorities
          | Some _ | None -> ());
        outcome
      | None, None ->
        let%bind acknowledgement =
          Authored_run_acknowledgement.prepare
            ~previous:t.state
            ~authorities:t.run_authorities
            ~lifecycle:t.lifecycle_agent_custody
            ~scope
            ~request
            ~run
            ~receipt:(Run_admission.receipt admission)
        in
        Exn.protect
          ~finally:(fun () -> Authored_run_acknowledgement.discard acknowledgement)
          ~f:(fun () ->
            start_internal
              ~authored_run_acknowledgement:acknowledgement
              ?run_preparation:preparation
              ?commit_attempted
              ~extra_deltas:[ delta ]
              t
            |> Result.map ~f:(fun _ -> ()))
      | Some _, None | None, Some _ ->
        Error (error Invalid_state "run admission lost its prepared owner")
    in
    Ok (Run_admission.receipt admission)
;;

let submit_managed_message t reference key request_sha256 generation max_receipts entry =
  let module M = Managed_submission in
  let open Result.Let_syntax in
  let%bind () =
    match t.state.spec.delegation with
    | Some current
      when Agent_store.Delegation_store.Reference.equal current reference
           && Agent_protocol.Id.Session.equal
                reference.child_session_id
                t.state.identity.session_id -> Ok ()
    | _ ->
      Error (error Permission_denied "delegation.send: child relationship does not match")
  in
  let%bind candidate =
    M.create
      ~reference
      ~key
      ~request_sha256
      ~generation
      ~history_id:entry.Agent_protocol.History.id
      ~now:(t.services.now ())
  in
  match List.find t.state.managed_submissions ~f:(M.same_key candidate) with
  | Some receipt ->
    (match String.equal receipt.request_sha256 request_sha256 with
     | true -> Ok receipt
     | false -> Error (error Conflict "managed send key was used for a different request"))
  | None ->
    let%bind () =
      match
        ( Int.equal generation t.state.identity.generation
        , entry.role
        , entry.kind
        , entry.provenance )
      with
      | true, User, Message, Canonical -> Ok ()
      | _ -> Error (error Conflict "managed message generation or input is invalid")
    in
    let%bind () =
      match max_receipts with
      | Some maximum
        when maximum < 1 || List.length t.state.managed_submissions >= maximum ->
        Error (error Invalid_state "managed submission receipt capacity reached")
      | None | Some _ -> Ok ()
    in
    let%bind _ =
      submit_authorized_message
        t
        entry
        ~extra_deltas:[ Managed_submission_admitted candidate ]
    in
    List.find t.state.managed_submissions ~f:(M.same_key candidate)
    |> Result.of_option
         ~error:(error Invalid_state "managed send did not retain its receipt")
;;

let find_permission t permission_id =
  List.find t.state.permissions ~f:(fun permission ->
    Agent_protocol.Id.Permission.compare permission.id permission_id = 0)
  |> Result.of_option ~error:(error Invalid_request "permission request does not exist")
;;

let permission_state = function
  | Agent_protocol.Permission.Deny -> Agent_protocol.Permission.Denied
  | Approve_once | Approve_session | Approve_prefix | Durable_exact -> Approved
;;

let grant_scope = function
  | Agent_protocol.Permission.Approve_session -> Some Agent_protocol.Grant.Exact_session
  | Approve_prefix -> Some Prefix_session
  | Durable_exact -> Some Durable_exact
  | Approve_once | Deny -> None
;;

let grant_identity permission scope =
  match scope with
  | Agent_protocol.Grant.Prefix_session ->
    Ok
      (Permission_policy.prefix_identity_digest
         ~tool_name:permission.Agent_protocol.Permission.tool_name)
  | Exact_session | Durable_exact ->
    permission.runtime_identity
    |> Result.of_option
         ~error:(error Invalid_state "permission request has no runtime identity")
;;

let grant_for_resolution
      (t : t)
      (permission : Agent_protocol.Permission.t)
      principal_id
      choice
  =
  match grant_scope choice with
  | None -> Ok None
  | Some scope ->
    let open Result.Let_syntax in
    let%bind principal_id =
      principal_id
      |> Result.of_option
           ~error:(error Permission_denied "grant approval requires a principal")
    in
    let%map identity_digest = grant_identity permission scope in
    Some
      Agent_protocol.Grant.
        { id = Agent_protocol.Id.Grant.create ()
        ; session_id = t.state.identity.session_id
        ; principal_id
        ; tool_name = permission.tool_name
        ; identity_digest
        ; scope
        ; state = Active
        ; created_at = t.services.now ()
        ; expires_at = None
        ; revoked_at = None
        ; revocation_reason = None
        }
;;

let permission_owner_active t (permission : Agent_protocol.Permission.t) =
  match permission.owner with
  | Operation id ->
    (match t.state.active_operation with
     | Some operation when not (Agent_protocol.Id.Operation.equal operation.id id) ->
       Error (error Conflict "permission request belongs to another active operation")
     | _ -> Ok ())
  | Invocation id ->
    let live =
      List.exists t.invocation_executions ~f:(fun execution ->
        execution.accepts_children
        && Agent_protocol.Id.Invocation.equal id execution.dispatched.context.id
        && execution.dispatched.context.generation = permission.generation
        &&
        match execution.owner with
        | Foreground id ->
          Option.exists t.state.active_operation ~f:(fun operation ->
            Agent_protocol.Id.Operation.equal operation.id id
            &&
            match operation.state with
            | Cancelling -> false
            | _ -> true)
        | Background_job scope -> Result.is_ok (job_scope_can_execute t scope)
        | Invocation_moderator borrow ->
          (not (borrow.committed || borrow.cancel_requested))
          && Option.exists t.moderator_borrow ~f:(phys_equal borrow)
          &&
            (match borrow.job_scope, borrow.operation_id with
            | Some scope, None -> Result.is_ok (job_scope_can_execute t scope)
            | Some _, Some _ -> false
            | None, None ->
              t.idle_moderator_borrowed && Option.is_none t.state.active_operation
            | None, Some id ->
              Option.exists t.state.active_operation ~f:(fun operation ->
                Agent_protocol.Id.Operation.equal operation.id id
                &&
                match operation.state with
                | Cancelling -> false
                | _ -> true))
        | Event_moderator borrow ->
          t.idle_moderator_borrowed
          && borrow.callback_active
          && (not (borrow.committed || borrow.cancel_requested))
          && Option.exists t.queued_event_borrow ~f:(phys_equal borrow)
          &&
            (match borrow.job_scope, borrow.receipt.context.operation_id with
            | Some scope, None -> Result.is_ok (job_scope_can_execute t scope)
            | Some _, Some _ -> false
            | None, None -> Option.is_none t.state.active_operation
            | None, Some id ->
              Option.exists t.state.active_operation ~f:(fun operation ->
                Agent_protocol.Id.Operation.equal id operation.id
                &&
                match operation.state with
                | Cancelling -> false
                | _ -> true)))
      || Option.exists t.moderator_borrow ~f:(fun borrow ->
        borrow.accepts_children
        && (match borrow.kind with
            | Invocation -> true
            | Observation -> false)
        && (not (borrow.committed || borrow.cancel_requested))
        && Agent_protocol.Id.Invocation.equal id borrow.invocation.context.id
        && Int.equal borrow.invocation.context.generation permission.generation
        &&
        match borrow.job_scope, borrow.operation_id with
        | Some scope, None -> Result.is_ok (job_scope_can_execute t scope)
        | None, None | Some _, Some _ -> false
        | None, Some id ->
          Option.exists t.state.active_operation ~f:(fun operation ->
            Agent_protocol.Id.Operation.equal operation.id id
            &&
            match operation.state with
            | Cancelling -> false
            | _ -> true))
    in
    (match t.state.lifecycle.desired, live, t.state.halted, t.state.failure with
     | Running, true, false, None -> Ok ()
     | _ -> Error (error Conflict "permission requires a live invocation owner"))
;;

module Constructor_permission_owner = struct
  module Write = struct
    type t =
      | Admission
      | Negative_resolution
  end

  type t =
    { execution : invocation_execution
    ; borrow : queued_event_borrow
    ; scope : Initialization_scope.t
    }
end

let check_constructor_permission_owner t (owner : Constructor_permission_owner.t) =
  let open Result.Let_syntax in
  let%bind () = check_constructor_invocation_basis t owner.scope in
  if
    List.exists t.invocation_executions ~f:(phys_equal owner.execution)
    && owner.execution.accepts_children
    && Atomic.get owner.execution.authority_live
    && Option.exists
         (constructor_scope_of_borrow t owner.borrow)
         ~f:(phys_equal owner.scope)
    && List.exists t.state.invocations ~f:(fun retained ->
      Agent_protocol.Invocation.equal retained owner.execution.dispatched)
  then Ok ()
  else
    Error (error Conflict "constructor permission no longer owns its native invocation")
;;

let constructor_permission_owner t (permission : Agent_protocol.Permission.t) =
  let open Result.Let_syntax in
  match permission.owner with
  | Operation _ -> Ok None
  | Invocation id ->
    (match
       List.find t.invocation_executions ~f:(fun execution ->
         Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id)
     with
     | Some ({ owner = Event_moderator borrow; _ } as execution) ->
       (match constructor_scope_of_borrow t borrow with
        | None -> Ok None
        | Some scope ->
          let owner : Constructor_permission_owner.t = { execution; borrow; scope } in
          let%map () = check_constructor_permission_owner t owner in
          Some owner)
     | Some { owner = Foreground _ | Invocation_moderator _ | Background_job _; _ } | None
       -> Ok None)
;;

(* The selected owner is captured before clocks/IDs are sampled. Only this
   validated DATA plan can advance the permission projection. The normal writer
   and its final currentness fence remain the sole persistence path. *)
let commit_permission_transition
      t
      owner
      ~(write : Constructor_permission_owner.Write.t)
      ~revision
      ~permissions
      ~grant
      ~lifecycle
      ~delta
      ~payloads
  =
  let open Result.Let_syntax in
  match owner with
  | None -> transition t ~delta ~payloads |> Result.map ~f:(fun _ -> ())
  | Some (owner : Constructor_permission_owner.t) ->
    let run_preparation = owner.scope.run_preparation in
    let%bind () = check_constructor_permission_owner t owner in
    let%bind prepared =
      match write with
      | Admission -> prepare_transition ?run_preparation t ~delta ~payloads
      | Negative_resolution ->
        (* A factual denial must unblock its exact waiter even when the original
           proof expired. Keep physical/source/Run-preparation basis checks, not
           fresh-effect authorization. Install advances the SAME preparation. *)
        let%bind () =
          if
            Option.is_none grant
            && List.for_all permissions ~f:(fun permission ->
              Option.exists
                permission.Agent_protocol.Permission.resolution
                ~f:(fun resolution ->
                  Agent_protocol.Permission.equal_choice resolution.choice Deny))
          then Ok ()
          else Error (error Conflict "negative permission ACK contains approval facts")
        in
        prepare_transition t ~delta ~payloads
    in
    let%bind permission_checkpoint =
      Constructor_permission_checkpoint.prepare
        ~previous:t.state
        ~incoming:prepared.state
        ~invocation_id:owner.execution.dispatched.context.id
        ~permissions
        ~grant
        ~lifecycle
    in
    let acknowledgement =
      Initialization_scope.Acknowledgement.prepare
        ~permission_checkpoint
        owner.scope
        prepared.state
    in
    let%bind () =
      validate_initialization_projection
        (Initialization_scope.Acknowledgement.expected acknowledgement)
        prepared.state
    in
    let check_before_commit () =
      let%bind () =
        match write with
        | Admission -> validate_initialization_scope_current t owner.scope
        | Negative_resolution -> Ok ()
      in
      let%bind () = check_constructor_permission_owner t owner in
      let%bind () = Initialization_scope.Acknowledgement.check_basis acknowledgement in
      if Int64.equal revision t.state.counters.revision
      then Ok ()
      else Error (error Conflict "constructor permission basis changed before commit")
    in
    install
      ~initialization_acknowledgement:acknowledgement
      ~check_before_commit
      ?run_preparation
      t
      prepared
;;

let resolve_permission
      t
      (permission : Agent_protocol.Permission.t)
      ~principal_id
      ~choice
      ~reason
      ~resume_observed
  =
  let open Result.Let_syntax in
  let revision = t.state.counters.revision in
  let%bind owner = constructor_permission_owner t permission in
  let resolution =
    Agent_protocol.Permission.
      { choice; principal_id; resolved_at = t.services.now (); reason }
  in
  let permission =
    { permission with state = permission_state choice; resolution = Some resolution }
  in
  let open Result.Let_syntax in
  let%bind () =
    match choice with
    | Agent_protocol.Permission.Deny -> Ok ()
    | Approve_once | Approve_session | Approve_prefix | Durable_exact ->
      permission_owner_active t permission
  in
  let%bind grant = grant_for_resolution t permission principal_id choice in
  let lifecycle =
    Session_state.Lifecycle.
      { desired = t.state.lifecycle.desired
      ; observed =
          permission_resume_observed
            t
            ~resolved:[ permission.id ]
            ~fallback:resume_observed
      }
  in
  let%bind _ =
    commit_permission_transition
      t
      owner
      ~write:
        (match choice with
         | Agent_protocol.Permission.Deny -> Negative_resolution
         | Approve_once | Approve_session | Approve_prefix | Durable_exact -> Admission)
      ~revision
      ~permissions:[ permission ]
      ~grant
      ~lifecycle
      ~delta:
        (Session_delta.Batch
           ([ Session_delta.Permission_changed permission
            ; Session_delta.Lifecycle_changed lifecycle
            ]
            @ Option.to_list
                (Option.map grant ~f:(fun value -> Session_delta.Grant_changed value))))
      ~payloads:
        ([ Agent_protocol.Event.Durable.Payload.Permission_resolved permission
         ; Agent_protocol.Event.Durable.Payload.Session_state_changed
             { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
         ]
         @ Option.to_list
             (Option.map grant ~f:(fun value ->
                Agent_protocol.Event.Durable.Payload.Grant_created value)))
  in
  Option.iter (Map.find !(t.permission_waiters) permission.id) ~f:(fun waiter ->
    Eio.Promise.resolve waiter.resolver resolution);
  t.permission_waiters := Map.remove !(t.permission_waiters) permission.id;
  Ok permission
;;

let schedule_permission_expiry t permission timeout_seconds fallback =
  if t.schedule_permission_timeouts
  then
    Option.iter timeout_seconds ~f:(fun seconds ->
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        t.sleep seconds;
        let promise, resolver = Eio.Promise.create () in
        let request =
          Expire_permission
            (permission.Agent_protocol.Permission.id, permission.generation, fallback)
        in
        match
          Mailbox.push t.mailbox ~priority:Priority (Pack (None, request, resolver))
        with
        | Error _ -> ()
        | Ok () ->
          ignore (Actor_reply.await promise : (unit, Agent_protocol.Error.t) result)))
;;

let open_permission t (permission : Agent_protocol.Permission.t) timeout_seconds fallback =
  let open Result.Let_syntax in
  let revision = t.state.counters.revision in
  let%bind owner = constructor_permission_owner t permission in
  let%bind () = permission_owner_active t permission in
  if
    Agent_protocol.Id.Session.compare permission.session_id t.state.identity.session_id
    <> 0
  then Error (error Invalid_request "permission request belongs to another session")
  else if permission.generation <> t.state.identity.generation
  then Error (error Conflict "permission request belongs to a stale session generation")
  else if not (Agent_protocol.Permission.equal_state permission.state Pending)
  then Error (error Invalid_request "new permission request must be pending")
  else if
    not
      (List.mem permission.choices fallback ~equal:Agent_protocol.Permission.equal_choice)
  then Error (error Invalid_request "permission fallback choice is not offered")
  else if
    List.exists t.state.permissions ~f:(fun previous ->
      Agent_protocol.Id.Permission.equal previous.id permission.id)
  then Error (error Conflict "permission request identity is already retained")
  else (
    let response, resolver = Eio.Promise.create () in
    let resume_observed =
      match t.state.lifecycle.observed with
      | Waiting_for_permission id ->
        Option.value_map
          (Map.find !(t.permission_waiters) id)
          ~default:t.state.lifecycle.observed
          ~f:(fun waiter -> waiter.resume_observed)
      | observed -> observed
    in
    let lifecycle =
      Session_state.Lifecycle.
        { desired = t.state.lifecycle.desired
        ; observed = Waiting_for_permission permission.id
        }
    in
    let open Result.Let_syntax in
    let%bind _ =
      commit_permission_transition
        t
        owner
        ~write:Admission
        ~revision
        ~permissions:[ permission ]
        ~grant:None
        ~lifecycle
        ~delta:
          (Session_delta.Batch
             [ Permission_changed permission; Lifecycle_changed lifecycle ])
        ~payloads:
          [ Agent_protocol.Event.Durable.Payload.Permission_requested permission
          ; Session_state_changed
              { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
          ]
    in
    t.permission_waiters
    := Map.set
         !(t.permission_waiters)
         ~key:permission.id
         ~data:{ resolver; resume_observed };
    schedule_permission_expiry t permission timeout_seconds fallback;
    Ok response)
;;

let respond_permission_internal
      t
      attachment_id
      principal_id
      permission_id
      permission_generation
      choice
      reason
  =
  let open Result.Let_syntax in
  let%bind _ = write_attachment t attachment_id in
  let%bind permission = find_permission t permission_id in
  if permission.generation <> permission_generation
  then Error (error Conflict "permission generation does not match")
  else if not (Agent_protocol.Permission.equal_state permission.state Pending)
  then Error (error Already_resolved "permission has already been resolved")
  else if
    Option.value_map permission.expires_at ~default:false ~f:(fun deadline ->
      Agent_protocol.Timestamp.compare deadline (t.services.now ()) <= 0)
  then Error (error Already_resolved "permission request has expired")
  else if
    not (List.mem permission.choices choice ~equal:Agent_protocol.Permission.equal_choice)
  then Error (error Invalid_request "permission choice is not offered")
  else (
    match Map.find !(t.permission_waiters) permission_id with
    | None -> Error (error Interrupted "permission continuation is unavailable")
    | Some waiter ->
      resolve_permission
        t
        permission
        ~principal_id
        ~choice
        ~reason
        ~resume_observed:waiter.resume_observed)
;;

let resolve_permission_system t permission_id generation choice reason =
  let open Result.Let_syntax in
  let%bind permission = find_permission t permission_id in
  if permission.generation <> generation
  then Error (error Conflict "permission generation does not match")
  else if not (Agent_protocol.Permission.equal_state permission.state Pending)
  then Error (error Already_resolved "permission has already been resolved")
  else if
    not (List.mem permission.choices choice ~equal:Agent_protocol.Permission.equal_choice)
  then Error (error Invalid_request "permission choice is not offered")
  else (
    match Map.find !(t.permission_waiters) permission_id with
    | None -> Error (error Interrupted "permission continuation is unavailable")
    | Some waiter ->
      resolve_permission
        t
        permission
        ~principal_id:None
        ~choice
        ~reason
        ~resume_observed:waiter.resume_observed)
;;

let expire_permission_internal t permission_id generation fallback =
  match
    find_permission t permission_id, Map.find !(t.permission_waiters) permission_id
  with
  | Ok permission, Some waiter
    when permission.generation = generation
         && Agent_protocol.Permission.equal_state permission.state Pending ->
    resolve_permission
      t
      permission
      ~principal_id:None
      ~choice:fallback
      ~reason:(Some "permission request timed out")
      ~resume_observed:waiter.resume_observed
    |> Result.map ~f:(fun _ -> ())
  | _ -> Ok ()
;;

let revoke_grant t attachment_id grant_id reason =
  with_writer t attachment_id (fun () ->
    let open Result.Let_syntax in
    let%bind update =
      Security_grant.revoke
        ~now:(t.services.now ())
        ~reason
        ~session_id:t.state.identity.session_id
        ~creating_principal:t.state.identity.creating_principal
        ~generic:t.state.grants
        ~shell:t.state.shell
        grant_id
    in
    let delta, grant =
      match update with
      | Security_grant.Generic grant -> Session_delta.Grant_changed grant, grant
      | Shell (shell, grant) -> Session_delta.Shell_changed shell, grant
    in
    let%map session =
      transition
        t
        ~delta
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Grant_revoked grant ]
    in
    grant, session)
;;

let replace_shell_approval_grants ?run_preparation t approval_grants =
  let shell = { t.state.shell with approval_grants } in
  transition ?run_preparation t ~delta:(Session_delta.Shell_changed shell) ~payloads:[]
  |> Result.map ~f:(fun _ -> ())
;;

let replace_shell_extension_snapshots ?run_preparation t extension_snapshots =
  let shell = { t.state.shell with extension_snapshots } in
  transition ?run_preparation t ~delta:(Session_delta.Shell_changed shell) ~payloads:[]
  |> Result.map ~f:(fun _ -> ())
;;

let add_shell_manifest_grant ?run_preparation t grant =
  let manifest_grants = grant :: t.state.shell.manifest_grants in
  let shell = { t.state.shell with manifest_grants } in
  let projected =
    Security_grant.project_manifest
      ~now:(t.services.now ())
      ~session_id:t.state.identity.session_id
      ~creating_principal:t.state.identity.creating_principal
      grant
  in
  transition
    ?run_preparation
    t
    ~delta:(Session_delta.Shell_changed shell)
    ~payloads:[ Agent_protocol.Event.Durable.Payload.Grant_created projected ]
  |> Result.map ~f:(fun _ -> ())
;;

let change_job t attachment_id (job : Agent_protocol.Job.t) =
  with_writer t attachment_id (fun () ->
    match
      List.exists t.job_scopes ~f:(fun scope ->
        Agent_protocol.Id.Job.equal scope.job.id job.id)
    with
    | true -> Error (error Conflict "job execution owns its current record")
    | false ->
      transition
        t
        ~delta:(Session_delta.Job_changed job)
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Job_state_changed job ])
;;

let find_job t job_id =
  List.find t.state.jobs ~f:(fun job -> Agent_protocol.Id.Job.compare job.id job_id = 0)
  |> Result.of_option ~error:(error Invalid_request "job was not found")
;;

let capture_inference_target t target limits =
  let open Result.Let_syntax in
  let%bind _ =
    Inference.Selection.capture t.state.spec.inference_target ~target ~limits
    |> Result.map_error ~f:(fun failure ->
      error
        Invalid_request
        (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t failure)))
  in
  transition t ~delta:(Session_delta.Inference_target_captured target) ~payloads:[]
  |> Result.map ~f:ignore
;;

let find_model_job_target t job_id generation =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () =
    if
      Int.equal job.generation generation
      && Int.equal generation t.state.identity.generation
    then Ok ()
    else Error (error Conflict "model job capture belongs to a stale generation")
  in
  let%bind () =
    match job.kind with
    | Model_call -> Ok ()
    | Nested_agent | Scheduled_event | Async_tool | Shell_process | Compaction ->
      Error
        (error Invalid_request "only an actual model job may capture an inference target")
  in
  let%map binding =
    List.find t.state.model_job_targets ~f:(fun binding ->
      Agent_protocol.Id.Job.equal (Model_job_target.job_id binding) job_id
      && Int.equal (Model_job_target.generation binding) generation)
    |> Result.of_option
         ~error:(error Invalid_request "model job target binding is absent")
  in
  job, binding
;;

let capture_model_job_source ?run_preparation t job_id generation target limits =
  let open Result.Let_syntax in
  let%bind _, binding = find_model_job_target t job_id generation in
  let%bind captured = Model_job_target.capture_source binding ~target ~limits in
  transition
    ?run_preparation
    t
    ~delta:(Session_delta.Model_job_target_captured captured)
    ~payloads:[]
  |> Result.map ~f:ignore
;;

let capture_recipe_target ?run_preparation t job_id generation attempt target limits =
  let open Result.Let_syntax in
  let%bind job, binding = find_model_job_target t job_id generation in
  let%bind () =
    match job.status with
    | Running when Int.equal job.attempt attempt -> Ok ()
    | Running
    | Queued
    | Waiting_permission _
    | Waiting_completion _
    | Succeeded
    | Failed _
    | Cancelled
    | Interrupted _ ->
      Error (error Conflict "recipe capture is not owned by this running attempt")
  in
  let%bind captured = Model_job_target.capture_recipe binding ~target ~limits in
  transition
    ?run_preparation
    t
    ~delta:(Session_delta.Model_job_recipe_target_captured captured)
    ~payloads:[]
  |> Result.map ~f:ignore
;;

let job_with_progress t (job : Agent_protocol.Job.t) =
  let progress =
    List.find_map t.job_scopes ~f:(fun scope ->
      match
        scope.active
        && (not scope.cancel_requested)
        && Agent_protocol.Id.Job.equal scope.job.id job.id
        && Int.equal scope.job.generation job.generation
        && Int.equal scope.job.attempt job.attempt
      with
      | true -> Job_progress_buffer.snapshot scope.progress
      | false -> None)
  in
  { job with progress }
;;

let publish_job_progress_internal t id progress =
  match
    List.find t.invocation_executions ~f:(fun execution ->
      execution.accepts_children
      && Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id)
  with
  | None -> ()
  | Some execution ->
    let scope =
      match execution.owner with
      | Background_job scope -> Some scope
      | Invocation_moderator borrow -> borrow.job_scope
      | Event_moderator borrow -> borrow.job_scope
      | Foreground _ -> None
    in
    Option.iter scope ~f:(fun scope ->
      match job_scope_can_execute t scope with
      | Ok () -> Job_progress_buffer.update scope.progress progress
      | Error _ -> ())
;;

let validate_job_generation t (job : Agent_protocol.Job.t) generation =
  if job.generation <> generation || t.state.identity.generation <> generation
  then Error (error Conflict "job belongs to a stale session generation")
  else Ok ()
;;

let update_jobs
      ?run_preparation
      ?job_custody
      ?(subscriptions = [])
      ?(run_deltas = [])
      t
      jobs
  =
  let timer_ids =
    List.filter_map subscriptions ~f:(fun next ->
      match
        next.Agent_protocol.Subscription.result, lookup_subscription t next.context.id
      with
      | Some _, Some previous -> previous.timer_id
      | _ -> None)
    |> Hash_set.of_list (module Agent_protocol.Id.Schedule)
  in
  let cancelled_timers =
    List.filter_map t.state.schedules ~f:(fun schedule ->
      match Hash_set.mem timer_ids schedule.id, schedule.status with
      | true, (Agent_protocol.Schedule.Scheduled | Delivering) ->
        Some { schedule with status = Agent_protocol.Schedule.Cancelled }
      | _ -> None)
  in
  let cancelled =
    List.filter jobs ~f:(fun job ->
      match job.Agent_protocol.Job.status with
      | Cancelled | Interrupted _ -> true
      | Queued
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _ -> false)
  in
  let owns (scope : job_scope) =
    List.exists cancelled ~f:(fun job ->
      Agent_protocol.Id.Job.equal scope.job.id job.Agent_protocol.Job.id
      && Int.equal scope.job.generation job.generation
      && Int.equal scope.job.attempt job.attempt)
  in
  let children =
    List.filter t.invocation_executions ~f:(fun execution ->
      List.exists t.job_scopes ~f:(fun scope ->
        job_execution_owned_by scope execution && owns scope))
  in
  let permissions, deltas, payloads =
    cleanup_invocation_permissions
      t
      (List.map children ~f:(fun child -> child.dispatched.context.id)
       @
       match t.moderator_borrow with
       | Some borrow when Option.exists borrow.job_scope ~f:owns ->
         [ borrow.invocation.context.id ]
       | _ -> [])
  in
  let open Result.Let_syntax in
  let%map session =
    transition
      ?run_preparation
      ?job_custody
      t
      ~delta:
        (Session_delta.Batch
           (List.map jobs ~f:(fun job -> Session_delta.Job_changed job)
            @ List.map subscriptions ~f:(fun value ->
              Session_delta.Subscription_changed value)
            @ List.map cancelled_timers ~f:(fun value ->
              Session_delta.Schedule_changed value)
            @ deltas
            @ run_deltas))
      ~payloads:
        (List.map jobs ~f:(fun job ->
           Agent_protocol.Event.Durable.Payload.Job_state_changed job)
         @ payloads
         @ List.map cancelled_timers ~f:(fun schedule ->
           Agent_protocol.Event.Durable.Payload.Schedule_cancelled schedule))
  in
  cancel_job_scopes t cancelled;
  resolve_cleaned_permission_waiters t permissions;
  session
;;

let update_job ?run_preparation ?job_custody t job =
  update_jobs ?run_preparation ?job_custody t [ job ]
;;

let validate_new_job t (job : Agent_protocol.Job.t) =
  if not (Agent_protocol.Id.Session.equal job.session_id t.state.identity.session_id)
  then Error (error Invalid_request "job belongs to another session")
  else if not (Int.equal job.generation t.state.identity.generation)
  then Error (error Conflict "job belongs to a stale session generation")
  else if
    List.exists t.state.jobs ~f:(fun existing ->
      Agent_protocol.Id.Job.equal existing.id job.id)
  then Error (error Conflict "job ID is already present")
  else Ok ()
;;

let add_job ?run_preparation t (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let%bind () = validate_new_job t job in
  let%map _ = update_job ?run_preparation t job in
  job
;;

let job_is_due t (job : Agent_protocol.Job.t) =
  Option.value_map job.next_run_at ~default:true ~f:(fun next_run_at ->
    Agent_protocol.Timestamp.compare next_run_at (t.services.now ()) <= 0)
;;

let retire_unauthorized_run_job t (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  match t.state.run_state with
  | None -> Ok false
  | Some original ->
    let%bind work = Run_job_occurrence.work job in
    let%bind index, retired =
      List.fold_result
        (Run_state.runs original)
        ~init:(original, false)
        ~f:(fun (index, retired) run ->
          if
            not
              (List.exists
                 run.Agent_protocol.Run.owned_work
                 ~f:(Agent_protocol.Run_work.equal work))
          then Ok (index, retired)
          else (
            match run.lifecycle with
            | Terminal _ ->
              Ok
                ( index
                , retired
                  || not
                       (List.exists
                          run.relinquished_work
                          ~f:(Agent_protocol.Run_work.equal work)) )
            | Admitted | Active | Waiting _ ->
              (match check_run_binding t run.id with
               | Ok () -> Ok (index, retired)
               | Error _ ->
                 let%map index =
                   Run_retirement.interrupt_run
                     index
                     ~run_id:run.id
                     ~session_revision:(Int64.succ t.state.counters.revision)
                     ~now:(t.services.now ())
                 in
                 index, true)))
    in
    if not retired
    then Ok false
    else (
      let job =
        { job with
          status = Cancelled
        ; result =
            background_terminal_result
              job
              (Cancelled "run authority retired before dispatch")
        ; completed_at = Some (t.services.now ())
        ; delivery = Discarded { at = t.services.now (); reason = Authority_changed }
        }
      in
      let%map _ =
        update_jobs t [ job ] ~run_deltas:[ Session_delta.Run_state_changed index ]
      in
      true)
;;

let claim_job ?run_preparation t job_id generation =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () =
    if Session_state.execution_migration_required t.state
    then
      Error (error Migration_required "job execution requires explicit runtime migration")
    else Ok ()
  in
  let%bind () =
    match
      List.exists t.job_scopes ~f:(fun scope ->
        Agent_protocol.Id.Job.equal scope.job.id job_id)
    with
    | false -> Ok ()
    | true -> Error (error Conflict "job's prior execution scope has not finished")
  in
  match job.status with
  | Agent_protocol.Job.Queued when job_is_due t job ->
    let%bind retired = retire_unauthorized_run_job t job in
    if retired
    then Ok None
    else if job.attempt = Int.max_value
    then Error (error Invalid_state "job attempt counter overflow")
    else (
      let previous = job in
      let job =
        { job with
          status = Running
        ; attempt = job.attempt + 1
        ; started_at = Some (t.services.now ())
        ; next_run_at = None
        ; result = None
        }
      in
      let%bind row = Job_custody.find t.job_agent_custody ~job:previous in
      let%bind () =
        Option.value_map row ~default:(Ok ()) ~f:(fun row ->
          let%bind () = check_job_moderator_binding t (Job_custody.Row.binding row) in
          let%bind admission = Job_custody.Row.admission row in
          check_agent_admission admission)
      in
      let%bind job_custody =
        Job_custody.prepare_claim t.job_agent_custody ~previous ~claimed:job
      in
      let%map _ = update_job ?run_preparation ~job_custody t job in
      Some job)
  | Queued -> Ok None
  | Running
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> Ok None
;;

let job_failure message =
  Agent_protocol.Error.create Internal_error ~message ~retryable:false ()
;;

let retry_limits = function
  | Agent_protocol.Job.Never -> None
  | Safe_retry { max_attempts; backoff_ms } | Idempotent { max_attempts; backoff_ms; _ }
    -> Some (max_attempts, backoff_ms)
;;

let retry_at t backoff_ms =
  let now = t.services.now () in
  let span = Time_ns.Span.of_ms (Float.of_int backoff_ms) in
  Agent_protocol.Timestamp.to_time_ns now
  |> Fn.flip Time_ns.add span
  |> Agent_protocol.Timestamp.of_time_ns
;;

let retry_result message = `Object [ "last_error", `String message ]

let retry_job t (job : Agent_protocol.Job.t) message backoff_ms =
  { job with
    status = Queued
  ; started_at = None
  ; next_run_at = Some (retry_at t backoff_ms)
  ; completed_at = None
  ; result = Some (retry_result message)
  }
;;

let terminal_job t (job : Agent_protocol.Job.t) status result =
  let delivery =
    match job.delivery with
    | Agent_protocol.Job.Not_required -> Agent_protocol.Job.Not_required
    | Pending | Delivered _ -> Pending
    | Discarded _ as delivery -> delivery
  in
  { job with status; result; completed_at = Some (t.services.now ()); delivery }
;;

let validate_job_dependency t job =
  Job_dependency.validate
    ~invocations:t.state.invocations
    ~events:t.state.moderator_executions
    ~jobs:t.state.jobs
    ~subscriptions:t.state.subscriptions
    job
;;

let retained_subscription t id =
  Result.of_option
    (lookup_subscription t id)
    ~error:(error Invalid_state "subscription dependency is missing")
;;

let cancelled_work_dependencies t (parent : Agent_protocol.Job.t) =
  let rec walk seen (job : Agent_protocol.Job.t) =
    let open Result.Let_syntax in
    match job.status with
    | Waiting_completion dependency ->
      let%bind () = validate_job_dependency t job in
      (match dependency.work with
       | Subscription id ->
         let%bind subscription = retained_subscription t id in
         (match subscription.result with
          | Some _ -> Ok ([], [])
          | None ->
            let%map terminal, _ =
              Agent_protocol.Subscription.finish
                subscription
                ~expected_epoch:subscription.epoch
                ~now:
                  (subscription_terminal_time
                     t
                     subscription
                     (Cancelled "owning job stopped waiting"))
                (Cancelled "owning job stopped waiting")
            in
            [], [ terminal ])
       | Job id ->
         let key = Agent_protocol.Id.Job.to_string id in
         (match Set.mem seen key with
          | true -> Error (error Invalid_state "job dependency cycle")
          | false ->
            let%bind child = find_job t id in
            let%bind terminal = Agent_protocol.Job.terminal_result child in
            (match terminal with
             | Some _ -> Ok ([], [])
             | None ->
               let%map rest, subscriptions = walk (Set.add seen key) child in
               ( terminal_job
                   t
                   child
                   Cancelled
                   (background_terminal_result
                      child
                      (Cancelled "owning job stopped waiting"))
                 :: rest
               , subscriptions ))))
    | _ -> Ok ([], [])
  in
  walk (String.Set.singleton (Agent_protocol.Id.Job.to_string parent.id)) parent
;;

let complete_job_outcome t (job : Agent_protocol.Job.t) = function
  | Runtime_builder.Model_succeeded result ->
    terminal_job t job Agent_protocol.Job.Succeeded (Some result)
  | Model_failed message ->
    (match retry_limits job.retry_policy with
     | Some (max_attempts, backoff_ms) when job.attempt < max_attempts ->
       retry_job t job message backoff_ms
     | None | Some _ -> terminal_job t job (Failed (job_failure message)) None)
;;

let validate_job_attempt (job : Agent_protocol.Job.t) attempt =
  match Int.equal job.attempt attempt with
  | true -> Ok ()
  | false -> Error (error Conflict "job callback belongs to a stale execution attempt")
;;

let claim_job_scope t job_id generation attempt deadline =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  let%bind () =
    match
      ( job.status
      , List.exists t.job_scopes ~f:(fun scope ->
          Agent_protocol.Id.Job.equal scope.job.id job_id) )
    with
    | Running, false -> Ok ()
    | _ -> Error (error Conflict "job must be running without another execution owner")
  in
  let cancelled, cancel_signal = Eio.Promise.create () in
  let scope =
    { job
    ; progress = Job_progress_buffer.create ()
    ; deadline
    ; cancelled
    ; cancel_signal
    ; active = true
    ; cancel_requested = false
    ; unfinished_on_return = false
    ; authority_live = Atomic.make true
    ; cancel = None
    }
  in
  (* Validate before exposing the scope; undo the process-local reservation on
     rejection. No durable state or runnable invocation is produced here. *)
  t.job_scopes <- scope :: t.job_scopes;
  match job_scope_can_execute t scope with
  | Ok () -> Ok scope
  | Error failure ->
    t.job_scopes
    <- List.filter t.job_scopes ~f:(fun other -> not (phys_equal scope other));
    Error failure
;;

let claim_job_invocation t scope (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind () = job_scope_can_execute t scope in
  let%bind () =
    match
      Agent_protocol.Id.Session.equal scope.job.session_id invocation.context.session_id
      && Int.equal scope.job.generation invocation.context.generation
    with
    | true -> Ok ()
    | false ->
      Error (error Conflict "invocation belongs to another job session or generation")
  in
  let%bind () =
    match invocation.context.parent_job with
    | Some id
      when Agent_protocol.Id.Job.equal id scope.job.id
           && Agent_protocol.Invocation.equal_origin invocation.context.origin Script
           && Option.is_none invocation.context.parent_invocation
           && Option.is_none invocation.context.provider_call_id
           && Option.is_none invocation.context.call_entry_id
           && Option.is_none invocation.parent_event -> Ok ()
    | None -> active_script_parent t ~owns:(job_execution_owned_by scope) invocation
    | Some _ -> Error (error Conflict "invocation does not belong to this job scope")
  in
  let%bind () =
    match scope.deadline, invocation.context.deadline with
    | None, _ -> Ok ()
    | Some parent, Some child when Agent_protocol.Timestamp.compare child parent <= 0 ->
      Ok ()
    | _ -> Error (error Conflict "job invocation cannot extend its execution deadline")
  in
  let%bind () =
    match invocation.context.deadline with
    | Some deadline
      when Agent_protocol.Timestamp.compare (t.services.now ()) deadline >= 0 ->
      Error (error Conflict "job invocation deadline has elapsed")
    | _ -> Ok ()
  in
  let%bind origin =
    invocation_origin t invocation ~fallback:(fun () -> Ok scope.job.inference_origin)
  in
  let%bind invocation, admission = invocation_admission_deltas t invocation ~origin in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let execution =
    { owner = Background_job scope
    ; dispatched
    ; accepts_children = true
    ; authority_live = Atomic.make true
    }
  in
  t.invocation_executions <- execution :: t.invocation_executions;
  Ok execution
;;

let finish_job_scope t scope =
  let open Result.Let_syntax in
  let%bind () =
    match List.mem t.job_scopes scope ~equal:phys_equal with
    | true -> Ok ()
    | false -> Error (error Conflict "background execution scope has ended")
  in
  scope.active <- false;
  Atomic.set scope.authority_live false;
  scope.cancel <- None;
  let%bind () =
    match t.moderator_borrow with
    | Some borrow
      when borrow.callback_finished
           && Option.exists borrow.job_scope ~f:(phys_equal scope) ->
      finish_moderator_invocation
        t
        borrow
        (Some
           (Agent_protocol.Invocation.Cancelled
              "background scope ended before moderator cleanup committed"))
    | _ -> Ok ()
  in
  let%bind () =
    match t.queued_event_borrow with
    | Some borrow
      when (not borrow.callback_active)
           && Option.exists borrow.job_scope ~f:(phys_equal scope) ->
      finish_queued_event t borrow true
    | _ -> Ok ()
  in
  let%bind () =
    match job_has_moderator t scope with
    | false -> Ok ()
    | true -> Error (error Conflict "background callback left an owned moderator running")
  in
  let children = List.filter t.invocation_executions ~f:(job_execution_owned_by scope) in
  let permissions, permission_deltas, payloads =
    cleanup_invocation_permissions
      t
      (List.map children ~f:(fun child -> child.dispatched.context.id))
  in
  let%bind deltas =
    List.map children ~f:(fun execution ->
      execution.accepts_children <- false;
      Atomic.set execution.authority_live false;
      abort_staged_work t ~owner:(Invocation execution.dispatched.context.id);
      Agent_protocol.Invocation.cancel
        execution.dispatched
        ~reason:"background scope exited before recording the invocation outcome"
      |> Result.map ~f:(fun invocation -> Session_delta.Invocation_changed invocation))
    |> Result.all
  in
  let%bind () =
    match deltas @ permission_deltas with
    | [] -> Ok ()
    | deltas ->
      transition t ~delta:(Session_delta.Batch deltas) ~payloads |> Result.map ~f:ignore
  in
  resolve_cleaned_permission_waiters t permissions;
  t.invocation_executions
  <- List.filter t.invocation_executions ~f:(fun execution ->
       not (job_execution_owned_by scope execution));
  t.job_scopes <- List.filter t.job_scopes ~f:(fun other -> not (phys_equal scope other));
  match scope.unfinished_on_return || not (List.is_empty children) with
  | false -> Ok ()
  | true -> Error (error Conflict "background callback left unfinished invocations")
;;

let running_job_for_completion t job_id generation attempt =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  let%bind () =
    match
      List.exists t.job_scopes ~f:(fun scope ->
        Agent_protocol.Id.Job.equal scope.job.id job_id)
    with
    | false -> Ok ()
    | true -> Error (error Conflict "job execution scope still owns its invocations")
  in
  match job.status with
  | Agent_protocol.Job.Running -> Ok job
  | Queued | Waiting_permission _ | Waiting_completion _ ->
    Error (error Conflict "job is not running")
  | Succeeded | Failed _ | Cancelled | Interrupted _ ->
    Error (error Already_resolved "job is already terminal")
;;

let complete_job ?run_preparation t job_id generation attempt outcome =
  let open Result.Let_syntax in
  let%bind previous = running_job_for_completion t job_id generation attempt in
  let job = complete_job_outcome t previous outcome in
  let%bind job_custody =
    match job.status with
    | Queued ->
      Result.map
        (Job_custody.prepare_retry t.job_agent_custody ~previous ~queued:job)
        ~f:Option.some
    | Succeeded | Failed _ | Cancelled | Interrupted _ ->
      Result.map (prepare_terminal_job_custody t [ job ]) ~f:Option.some
    | Running | Waiting_permission _ | Waiting_completion _ ->
      Error (error Invalid_state "completed Job retained an active state")
  in
  let%map _ = update_job ?run_preparation ?job_custody t job in
  job
;;

(* These are actual actor-issued constructor owners, not identity-bearing DTOs.
   A preparation's genuine legacy None is an admission only while that exact
   preparation is current; missing historical Job inventory never enters here. *)
type constructor_job_owner =
  | Initialization_job of Initialization_scope.t
  | Run_constructor_job of Run_preparation.t

let check_constructor_job_owner t = function
  | Initialization_job scope ->
    let open Result.Let_syntax in
    let%bind () = validate_initialization_scope t scope in
    check_initialization_agent_actor scope
  | Run_constructor_job preparation -> check_run_preparation t preparation
;;

let add_constructor_job t owner (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let%bind () = check_constructor_job_owner t owner in
  let%bind () = validate_new_job t job in
  let run_preparation, parent, temporary =
    match owner with
    | Initialization_job scope -> scope.run_preparation, scope.agent_admission, false
    | Run_constructor_job preparation ->
      ( Some preparation
      , create_agent_admission
          (Option.map
             (Run_preparation.original_actor preparation)
             ~f:create_agent_custody)
      , true )
  in
  Exn.protect
    ~finally:(fun () -> if temporary then retire_agent_admission parent)
    ~f:(fun () ->
      let%bind selection =
        match owner with
        | Initialization_job _ -> Ok None
        | Run_constructor_job preparation ->
          Result.map
            (Run_preparation.Constructor_jobs.prepare_add
               preparation
               ~owner:t.run_preparation_owner
               ~state:t.state
               ~job)
            ~f:Option.some
      in
      let%bind binding = job_moderator_binding t in
      let%bind job_custody =
        Job_custody.prepare_fresh t.job_agent_custody ~job ~binding ~parent
      in
      let acknowledged = ref false in
      Exn.protect
        ~finally:(fun () ->
          if not !acknowledged then Job_custody.Update.discard job_custody)
        ~f:(fun () ->
          let%bind prepared =
            prepare_transition
              ?run_preparation
              t
              ~delta:(Session_delta.Job_changed job)
              ~payloads:[ Agent_protocol.Event.Durable.Payload.Job_state_changed job ]
          in
          Eio.Cancel.protect (fun () ->
            let%bind () = check_constructor_job_owner t owner in
            let%bind () = check_job_moderator_binding t binding in
            let%bind () =
              Option.value_map selection ~default:(Ok ()) ~f:(fun selection ->
                Run_preparation.Constructor_jobs.check_before_commit
                  selection
                  ~owner:t.run_preparation_owner
                  ~state:t.state)
            in
            let%bind () =
              Job_custody.Update.check_before_commit
                job_custody
                ~current:t.job_agent_custody
            in
            let%bind () = persist_transition t prepared in
            install_committed_transition ?run_preparation ~job_custody t prepared;
            (match owner with
             | Initialization_job scope -> scope.jobs <- job.id :: scope.jobs
             | Run_constructor_job _ ->
               Option.iter selection ~f:Run_preparation.Constructor_jobs.acknowledge);
            acknowledged := true;
            publish_committed_transition t prepared;
            Ok job)))
;;

let add_initialization_model_job t scope (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let%bind () =
    match job.kind, job.status with
    | Model_call, Queued when Int.equal job.attempt 0 -> Ok ()
    | _ ->
      Error (error Invalid_request "initialization requires a fresh queued model job")
  in
  add_constructor_job t (Initialization_job scope) job
;;

let start_initialization_model_job t scope (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let%bind () =
    match job.delivery with
    | Not_required when job_is_due t job -> Ok ()
    | Not_required | Pending | Delivered _ | Discarded _ ->
      Error
        (error
           Invalid_request
           "synchronous initialization job must be immediate and not deliverable")
  in
  let%bind job = add_initialization_model_job t scope job in
  let%bind claimed =
    claim_job
      ?run_preparation:scope.Initialization_scope.run_preparation
      t
      job.id
      job.generation
  in
  Result.of_option
    claimed
    ~error:(error Conflict "initialization job could not be claimed")
;;

let claim_initialization_model_job t scope job_id generation =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_scope t scope in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () =
    if
      List.mem scope.Initialization_scope.jobs job_id ~equal:Agent_protocol.Id.Job.equal
      && Agent_protocol.Job.equal_kind job.kind Model_call
      && (match job.status with
          | Queued -> true
          | _ -> false)
      && Agent_protocol.Job.equal_delivery job.delivery Not_required
    then Ok ()
    else Error (error Permission_denied "initializer does not own this queued model call")
  in
  let%bind () = check_initialization_agent_actor scope in
  let%bind claimed =
    claim_job ?run_preparation:scope.run_preparation t job_id generation
  in
  Result.of_option
    claimed
    ~error:(error Conflict "initializer model call could not be claimed")
;;

let initialization_model_job_is_owned t scope job_id generation attempt =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_owner t scope in
  match validate_initialization_scope t scope with
  | Error _ -> Ok false
  | Ok () ->
    Ok
      (List.mem scope.Initialization_scope.jobs job_id ~equal:Agent_protocol.Id.Job.equal
       && List.exists t.state.jobs ~f:(fun job ->
         Agent_protocol.Id.Job.equal job.id job_id
         && Int.equal job.generation generation
         && Int.equal job.attempt attempt
         && Agent_protocol.Job.equal_kind job.kind Model_call
         &&
         match job.status with
         | Running -> true
         | Queued
         | Waiting_permission _
         | Waiting_completion _
         | Succeeded
         | Failed _
         | Cancelled
         | Interrupted _ -> false))
;;

let initialization_model_job_is_current t scope job_id generation attempt =
  let open Result.Let_syntax in
  let%map owned = initialization_model_job_is_owned t scope job_id generation attempt in
  owned && Result.is_ok (check_initialization_agent_actor scope)
;;

let validate_initialization_model_job t scope job_id generation attempt =
  let open Result.Let_syntax in
  let%bind active =
    initialization_model_job_is_current t scope job_id generation attempt
  in
  if active
  then Ok ()
  else Error (error Conflict "initialization does not own this running model attempt")
;;

type inference_basis =
  { run : Agent_protocol.Run.t option
  ; attribution : Goal_work_attribution.t option
  ; operation_id : Agent_protocol.Id.Operation.t option
  ; invocation_id : Agent_protocol.Id.Invocation.t option
  ; preparation : Run_preparation.t option
  }

let inference_run_binding_current t (run : Agent_protocol.Run.t) =
  let open Result.Let_syntax in
  let%bind binding =
    Run_authorities.find t.run_authorities run.id
    |> Result.of_option ~error:(error Permission_denied "inference Run authority retired")
  in
  let%bind () = Run_admission.Scope.check_current (Run_authorities.scope binding) in
  let%bind index =
    t.state.run_state
    |> Result.of_option ~error:(error Invalid_state "inference Run index is absent")
  in
  let%bind installed =
    Run_source_installation.captured
      (Run_state.installation index)
      ~generation:t.state.identity.generation
  in
  let%bind observer = Moderator_checkpoint.observer t.state.moderator in
  if
    Agent_protocol.Run_source.equal installed run.source
    && Option.equal
         Agent_protocol.Invocation.equal_observer
         observer
         (Some installed.observer)
    && Agent_protocol.Id.Principal.equal
         run.principal_id
         (Run_admission.Scope.principal_id (Run_authorities.scope binding))
    &&
    match run.lifecycle with
    | Admitted | Active | Waiting _ -> true
    | Terminal _ -> false
  then Ok binding
  else Error (error Permission_denied "inference Run binding or installed source changed")
;;

type execution_origin_lookup =
  { job :
      Agent_protocol.Job.t -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result
  ; invocation :
      Agent_protocol.Invocation.t
      -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result
  ; event :
      Callback_scope.t -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result
  }

(* Immutable captured attribution never borrows the operation which drains it.
   The caller authenticates the exact Callback_scope independently. *)
let captured_callback_run t ~allow_unavailable (scope : Callback_scope.t) =
  let open Result.Let_syntax in
  match scope.origin with
  | None ->
    if allow_unavailable
    then Ok None
    else Error (error Permission_denied "callback inference origin is unavailable")
  | Some Unbound -> Ok None
  | Some (Run id) ->
    let%bind run =
      Option.bind t.state.run_state ~f:(fun index -> Run_state.find index id)
      |> Result.of_option ~error:(error Invalid_state "callback original Run is absent")
    in
    if
      Agent_protocol.Run_source.equal run.source scope.source
      && Agent_protocol.Id.Session.equal
           (Agent_protocol.Session_ref.session_id run.session)
           t.state.identity.session_id
    then Ok (Some run)
    else Error (error Permission_denied "callback original Run source or session changed")
;;

let execution_basis t (origins : execution_origin_lookup) execution =
  let open Result.Let_syntax in
  let checked_admission admission =
    let%bind admission = admission in
    Option.value_map admission ~default:(Ok ()) ~f:check_agent_admission
  in
  let operation id generation =
    let%bind work = Agent_protocol.Run_work.create ~key:(Operation id) ~generation in
    Goal_work_attribution.run_of_work t.state work
  in
  let basis ?operation_id ?invocation_id ?preparation run =
    let%map attribution =
      match run with
      | None -> Ok None
      | Some run -> Goal_work_attribution.of_run t.state run
    in
    { run; attribution; operation_id; invocation_id; preparation }
  in
  let job scope =
    let%bind () = job_scope_can_execute t scope in
    let%bind () = checked_admission (job_agent_admission t scope) in
    let%bind run = origins.job scope.job in
    let%map () =
      match run with
      | None -> Ok ()
      | Some run ->
        let%bind work = Run_job_occurrence.work scope.job in
        let live =
          match run.Agent_protocol.Run.lifecycle with
          | Admitted | Active | Waiting _ -> true
          | Terminal _ -> false
        in
        if
          live
          && List.mem run.owned_work work ~equal:Agent_protocol.Run_work.equal
          && (not
                (List.mem run.relinquished_work work ~equal:Agent_protocol.Run_work.equal))
          && not
               (List.exists run.terminal_work ~f:(fun proof ->
                  Agent_protocol.Run_work.equal proof.work work))
        then Ok ()
        else Error (error Permission_denied "Run does not own this current Job attempt")
    in
    run
  in
  let moderator (borrow : moderator_borrow) =
    let%bind () = validate_moderator_borrow t borrow in
    let%bind () =
      if
        Atomic.get borrow.authority_live
        && borrow.accepts_children
        && not (borrow.callback_finished || borrow.committed || borrow.cancel_requested)
      then Ok ()
      else Error (error Permission_denied "moderator inference owner expired")
    in
    let%bind () = checked_admission (moderator_agent_admission t borrow) in
    let%bind () =
      match borrow.job_scope, borrow.operation_id with
      | Some scope, None -> Result.map (job scope) ~f:ignore
      | None, Some id -> Result.map (running_operation t id) ~f:ignore
      | None, None -> Ok ()
      | Some _, Some _ ->
        Error (error Invalid_state "moderator inference has mixed owners")
    in
    (* The checks above authenticate the physical callback. Attribution belongs
       to its original invocation even when a later operation drains it. *)
    let%bind run = origins.invocation borrow.invocation in
    basis
      ?operation_id:borrow.operation_id
      ~invocation_id:borrow.invocation.context.id
      run
  in
  let event (borrow : queued_event_borrow) =
    let%bind () = queued_event_inference_current t borrow in
    let%bind () =
      if Atomic.get borrow.authority_live && Option.is_none borrow.retirement_reason
      then Ok ()
      else Error (error Permission_denied "event inference owner expired")
    in
    let%bind () = checked_admission (event_agent_admission t borrow) in
    let%bind run =
      match t.legacy_callback_scope with
      | Some scope when phys_equal scope.borrow borrow ->
        let%bind _ = callback_inference_actor t ~scope in
        origins.event scope
      | None | Some _ ->
        let%bind causal =
          Run_event_ancestry.find
            (Option.value_map t.state.run_state ~default:[] ~f:Run_state.runs)
            ~executing:borrow.receipt
        in
        let fallback () =
          match borrow.job_scope, borrow.receipt.context.operation_id with
          | Some scope, None -> job scope
          | None, Some id -> operation id borrow.receipt.context.generation
          | None, None ->
            (match borrow.run_actions with
             | None -> Run_event_scope.select t.state ~executing:borrow.receipt
             | Some owner ->
               Result.map
                 (Result.of_option
                    (Option.bind t.state.run_state ~f:(fun index ->
                       Run_state.find
                         index
                         (Run_scope.run_id (Run_action_service.scope owner))))
                    ~error:(error Invalid_state "callback Run is absent"))
                 ~f:Option.some)
          | Some _, Some _ ->
            Error (error Invalid_state "event inference has mixed owners")
        in
        let%bind run =
          match causal with
          | None -> fallback ()
          | Some original ->
            let%bind selected =
              Run_event_scope.select t.state ~executing:borrow.receipt
            in
            (match selected with
             | Some selected when Agent_protocol.Id.Run.equal original.id selected.id ->
               Ok (Some selected)
             | None | Some _ ->
               Error
                 (error Permission_denied "causal callback Run is no longer executable"))
        in
        Ok run
    in
    let preparation =
      match t.legacy_callback_scope with
      | Some scope when phys_equal scope.borrow borrow -> callback_run_preparation scope
      | None | Some _ -> None
    in
    basis ?operation_id:borrow.receipt.context.operation_id ?preparation run
  in
  let%bind result =
    match execution with
    | Inference_operation (expected, live) ->
      let%bind current = running_operation t expected.id in
      let%bind () =
        if
          Atomic.get live
          && Int.equal expected.generation current.generation
          && Agent_protocol.Operation.equal_kind expected.kind current.kind
        then Ok ()
        else Error (error Permission_denied "operation inference owner expired")
      in
      let%bind () = operation_agent_actor t expected.id |> Result.map ~f:ignore in
      let%bind run = operation current.id current.generation in
      basis ~operation_id:current.id run
    | Inference_invocation current ->
      let%bind _ = invocation_agent_actor t current current.dispatched in
      let%map parent =
        match current.owner with
        | Foreground id ->
          let%bind run = operation id current.dispatched.context.generation in
          basis ~operation_id:id run
        | Background_job scope ->
          let%bind run = job scope in
          basis run
        | Invocation_moderator borrow -> moderator borrow
        | Event_moderator borrow -> event borrow
      in
      { parent with invocation_id = Some current.dispatched.context.id }
    | Inference_moderator borrow -> moderator borrow
    | Inference_event borrow -> event borrow
    | Inference_job scope ->
      let%bind run = job scope in
      basis run
    | Inference_initialization_job (scope, expected) ->
      let%bind () = validate_initialization_scope_current t scope in
      let%bind () = check_initialization_agent_actor scope in
      let%bind () =
        if
          List.mem scope.jobs expected.id ~equal:Agent_protocol.Id.Job.equal
          && List.exists t.state.jobs ~f:(fun job ->
            Agent_protocol.Id.Job.equal job.id expected.id
            && Int.equal job.generation expected.generation
            && Int.equal job.attempt expected.attempt
            && Agent_protocol.Job.equal_kind job.kind Model_call
            && Jsonaf.exactly_equal job.payload expected.payload
            &&
            match job.status with
            | Running -> true
            | _ -> false)
        then Ok ()
        else
          Error
            (error
               Permission_denied
               "initializer no longer owns this current model attempt")
      in
      basis ?preparation:scope.run_preparation None
    | Inference_initialization scope ->
      let%bind () = validate_initialization_scope_current t scope in
      let%bind () = check_initialization_agent_actor scope in
      (* A future controller episode requires a genuine reservation. Existing
         initializer identity is retained; source or auth alone cannot invent it. *)
      basis ?preparation:scope.run_preparation None
  in
  let%map () =
    match result.run with
    | None -> Ok ()
    | Some run -> inference_run_binding_current t run |> Result.map ~f:ignore
  in
  result
;;

let inference_execution_basis t execution =
  execution_basis
    t
    { job = Goal_work_attribution.run_of_job t.state
    ; invocation = Goal_work_attribution.run_of_invocation t.state
    ; event = captured_callback_run t ~allow_unavailable:false
    }
    execution
;;

(* Unavailable inference classification does not erase actual retained work.
   These lookups use original Job/Invocation ancestry, never the operation which
   happens to drain a callback. They are data lookup after physical admission;
   [execution_basis] and the final guard still check the live owner and authority. *)
let check_native_parent t = function
  | None -> Ok None
  | Some ({ run; work } : Goal_work_attribution.Retained_parent.t) ->
    let open Result.Let_syntax in
    let%bind _ = inference_run_binding_current t run in
    if
      List.mem run.owned_work work ~equal:Agent_protocol.Run_work.equal
      && (not (List.mem run.relinquished_work work ~equal:Agent_protocol.Run_work.equal))
      && not
           (List.exists run.terminal_work ~f:(fun terminal ->
              Agent_protocol.Run_work.equal terminal.work work))
    then Ok (Some (run, work))
    else Error (error Permission_denied "native child lost its actual Run parent")
;;

let native_job_parent t job =
  Goal_work_attribution.Retained_parent.of_job t.state job
  |> Result.bind ~f:(check_native_parent t)
;;

let native_event_parent t event =
  Goal_work_attribution.Retained_parent.of_event t.state event
  |> Result.bind ~f:(check_native_parent t)
;;

let native_invocation_parent t invocation =
  Goal_work_attribution.Retained_parent.of_invocation t.state invocation
  |> Result.bind ~f:(check_native_parent t)
;;

let native_execution_origins t : execution_origin_lookup =
  let open Result.Let_syntax in
  { job =
      (fun job ->
        match job.Agent_protocol.Job.inference_origin with
        | Some _ -> Goal_work_attribution.run_of_job t.state job
        | None ->
          let%map parent = native_job_parent t job in
          Option.map parent ~f:fst)
  ; invocation =
      (fun invocation ->
        match invocation.Agent_protocol.Invocation.inference_origin with
        | Some _ -> Goal_work_attribution.run_of_invocation t.state invocation
        | None ->
          let%map parent = native_invocation_parent t invocation in
          Option.map parent ~f:fst)
  ; event = captured_callback_run t ~allow_unavailable:true
  }
;;

type native_child_basis =
  { execution : inference_basis
  ; classification : Agent_protocol.Inference_origin.t option
  ; parent_work : Agent_protocol.Run_work.t option
  }

let native_child_basis_of_execution t execution (basis : inference_basis) =
  let open Result.Let_syntax in
  let%bind classification, original_parent =
    let invocation (invocation : Agent_protocol.Invocation.t) =
      let%map parent =
        match invocation.inference_origin with
        | Some (Run _) -> Ok None
        | None | Some Unbound -> native_invocation_parent t invocation
      in
      invocation.inference_origin, parent
    in
    match execution with
    | Inference_invocation execution -> invocation execution.dispatched
    | Inference_moderator borrow -> invocation borrow.invocation
    | Inference_job scope ->
      let%map parent = native_job_parent t scope.job in
      scope.job.inference_origin, parent
    | Inference_event borrow ->
      let%bind classification =
        match t.legacy_callback_scope with
        | Some scope when phys_equal scope.borrow borrow -> Ok scope.origin
        | None | Some _ -> event_inference_origin t borrow.receipt
      in
      let%map parent =
        match classification with
        | None -> native_event_parent t borrow.receipt
        | Some _ -> Ok None
      in
      classification, parent
    | Inference_operation _ | Inference_initialization _ | Inference_initialization_job _
      ->
      Ok
        ( Some
            (match basis.run with
             | None -> Agent_protocol.Inference_origin.Unbound
             | Some run -> Run run.id)
        , None )
  in
  let%bind () =
    match classification, basis.run, original_parent with
    | None, None, None | Some Unbound, None, None -> Ok ()
    | Some (Run id), Some run, _ when Agent_protocol.Id.Run.equal id run.id -> Ok ()
    | None, Some run, Some (parent, _) when Agent_protocol.Id.Run.equal run.id parent.id
      -> Ok ()
    | _ -> Error (error Permission_denied "native child origin and work owner disagree")
  in
  Ok
    { execution = basis; classification; parent_work = Option.map original_parent ~f:snd }
;;

let native_child_basis t execution =
  let%bind.Result basis = execution_basis t (native_execution_origins t) execution in
  native_child_basis_of_execution t execution basis
;;

type inference_currentness =
  { basis : inference_basis
  ; check_current : unit -> (unit, Agent_protocol.Error.t) result
  }

let authorize_inference_basis t ~read_current =
  let open Result.Let_syntax in
  let%bind basis = read_current () in
  let%bind binding =
    match basis.run with
    | None -> Ok None
    | Some run ->
      let%bind binding = inference_run_binding_current t run in
      let%map () =
        Run_admission.Scope.authorize (Run_authorities.scope binding) t.state
      in
      Some binding
  in
  let%bind () =
    match basis.preparation with
    | None -> Ok ()
    | Some preparation -> check_run_preparation t preparation
  in
  let check_current () =
    let%bind current = read_current () in
    let same_run left right =
      Agent_protocol.Id.Run.equal left.Agent_protocol.Run.id right.Agent_protocol.Run.id
      && Agent_protocol.Run_source.equal left.source right.source
      && Agent_protocol.Id.Principal.equal left.principal_id right.principal_id
      && Agent_protocol.Session_ref.equal left.session right.session
    in
    let same_attribution left right =
      Agent_protocol.Id.Goal.equal
        (Goal_work_attribution.goal_id left)
        (Goal_work_attribution.goal_id right)
      && Agent_protocol.Goal_episode.equal
           (Goal_work_attribution.episode left)
           (Goal_work_attribution.episode right)
    in
    if
      Option.equal same_run basis.run current.run
      && Option.equal same_attribution basis.attribution current.attribution
      &&
      match basis.run, binding with
      | None, None -> true
      | Some run, Some binding ->
        Run_authorities.is_current t.run_authorities binding ~run_id:run.id
      | None, Some _ | Some _, None -> false
    then Ok ()
    else
      Error
        (error
           Permission_denied
           "inference physical owner or immutable origin changed after refresh")
  in
  let%map () = check_current () in
  { basis; check_current }
;;

let authorized_inference_execution_basis t execution =
  authorize_inference_basis t ~read_current:(fun () ->
    inference_execution_basis t execution)
;;

type native_child_currentness =
  { selected : native_child_basis
  ; check_current : unit -> (unit, Agent_protocol.Error.t) result
  }

let authorize_native_child_basis t ~read_current =
  let open Result.Let_syntax in
  let%bind (selected : native_child_basis) = read_current () in
  let%bind current =
    authorize_inference_basis t ~read_current:(fun () ->
      let%map next = read_current () in
      next.execution)
  in
  let check_current () =
    let%bind () = current.check_current () in
    let%bind next = read_current () in
    if
      Option.equal
        Agent_protocol.Inference_origin.equal
        selected.classification
        next.classification
      && Option.equal Agent_protocol.Run_work.equal selected.parent_work next.parent_work
    then Ok ()
    else Error (error Permission_denied "native child classification or parent changed")
  in
  let%map () = check_current () in
  { selected; check_current }
;;

let authorized_native_child_basis t execution =
  authorize_native_child_basis t ~read_current:(fun () -> native_child_basis t execution)
;;

(* A returned native invocation may settle only its already selected child set.
   Its native callback epoch stays closed. The underlying physical parent and
   original custody must still be live; no fresh inference or selection uses this
   path. *)
let selected_invocation_basis t (execution : invocation_execution) =
  let open Result.Let_syntax in
  let%bind () =
    if List.exists t.invocation_executions ~f:(phys_equal execution)
    then Ok ()
    else Error (error Conflict "selected work lost its invocation owner")
  in
  let%bind admission = execution_agent_admission t execution in
  let%bind () = Option.value_map admission ~default:(Ok ()) ~f:check_agent_admission in
  let origins = native_execution_origins t in
  let%bind basis =
    match execution.owner with
    | Background_job scope -> execution_basis t origins (Inference_job scope)
    | Invocation_moderator borrow ->
      execution_basis t origins (Inference_moderator borrow)
    | Event_moderator borrow -> execution_basis t origins (Inference_event borrow)
    | Foreground id ->
      let%bind current = running_operation t id in
      let%bind _ = operation_agent_actor t id in
      let%bind work =
        Agent_protocol.Run_work.create ~key:(Operation id) ~generation:current.generation
      in
      let%bind run = Goal_work_attribution.run_of_work t.state work in
      let%bind attribution =
        match run with
        | None -> Ok None
        | Some run ->
          let%bind _ = inference_run_binding_current t run in
          Goal_work_attribution.of_run t.state run
      in
      Ok
        { run
        ; attribution
        ; operation_id = Some id
        ; invocation_id = Some execution.dispatched.context.id
        ; preparation = None
        }
  in
  native_child_basis_of_execution t (Inference_invocation execution) basis
;;

let background_inference_execution t = function
  | Agent_protocol.Job.Moderator_event id ->
    (match t.queued_event_borrow with
     | Some borrow
       when Agent_protocol.Id.Moderator_execution.equal borrow.receipt.context.id id ->
       Ok (Inference_event borrow)
     | Some _ | None -> Error (error Conflict "background event owner is unavailable"))
  | Invocation id ->
    (match
       List.find t.invocation_executions ~f:(fun execution ->
         Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id)
     with
     | Some execution -> Ok (Inference_invocation execution)
     | None ->
       (match t.moderator_borrow with
        | Some borrow
          when Agent_protocol.Id.Invocation.equal borrow.invocation.context.id id ->
          Ok (Inference_moderator borrow)
        | Some _ | None ->
          Error (error Conflict "background invocation owner is unavailable")))
;;

let inference_origin (basis : inference_basis) =
  match basis.run with
  | None -> Agent_protocol.Job.Inference_origin.Unbound
  | Some run -> Run run.id
;;

type child_parent =
  | Parent_work of Agent_protocol.Run_work.t
  | Parent_event of queued_event_borrow
  | Parent_observation of moderator_borrow

let child_parent t execution =
  let operation id =
    let%bind.Result current = running_operation t id in
    Agent_protocol.Run_work.create ~key:(Operation id) ~generation:current.generation
    |> Result.map ~f:(fun work -> Parent_work work)
  in
  let job (scope : job_scope) =
    Run_job_occurrence.work scope.job |> Result.map ~f:(fun work -> Parent_work work)
  in
  let moderator (borrow : moderator_borrow) =
    match borrow.kind with
    | Observation -> Ok (Parent_observation borrow)
    | Invocation ->
      (match borrow.job_scope, borrow.operation_id with
       | Some scope, None -> job scope
       | None, Some id -> operation id
       | None, None | Some _, Some _ ->
         Error (error Invalid_state "Run child has no unique retained parent"))
  in
  match execution with
  | Inference_operation (operation_value, _) -> operation operation_value.id
  | Inference_job scope -> job scope
  | Inference_event borrow -> Ok (Parent_event borrow)
  | Inference_moderator borrow -> moderator borrow
  | Inference_invocation execution ->
    (match execution.owner with
     | Foreground id -> operation id
     | Background_job scope -> job scope
     | Invocation_moderator borrow -> moderator borrow
     | Event_moderator borrow -> Ok (Parent_event borrow))
  | Inference_initialization _ | Inference_initialization_job _ ->
    Error (error Invalid_state "initializer has no reserved Run child parent")
;;

(* The returned callback runs only after the exact Job/Run transaction ACK.
   Failed or uncertain saves leave the callback scope at its prior revision. *)
let staged_child_run_delta t execution ~basis ~classification ~parent_work job_deltas =
  let open Result.Let_syntax in
  let jobs =
    List.filter_map job_deltas ~f:(function
      | Session_delta.Job_changed job -> Some job
      | _ -> None)
  in
  let%bind () =
    if
      List.for_all jobs ~f:(fun job ->
        Option.equal
          Agent_protocol.Job.Inference_origin.equal
          job.inference_origin
          classification)
    then Ok ()
    else
      Error (error Permission_denied "selected Job origin differs from its live parent")
  in
  match jobs, basis.run with
  | [], _ | _, None -> Ok ([], fun () -> Ok ())
  | _, Some run ->
    let%bind children = List.map jobs ~f:Run_job_occurrence.work |> Result.all in
    let%bind work_delta =
      Session_delta.capture_new_model_jobs t.state (Session_delta.Batch job_deltas)
    in
    let%bind work_state = Session_delta.apply t.state work_delta in
    let%bind parent =
      match parent_work with
      | Some parent -> Ok (Parent_work parent)
      | None -> child_parent t execution
    in
    (match parent with
     | Parent_work parent ->
       let%map delta =
         Run_child_work.prepare
           t.state
           ~run
           ~parent
           ~work_state
           ~children
           ~now:(t.services.now ())
       in
       [ delta ], fun () -> Ok ()
     | Parent_observation borrow ->
       let%map delta =
         Run_child_work.prepare_observation
           t.state
           ~run
           ~observing:borrow.invocation
           ~work_state
           ~children:jobs
           ~now:(t.services.now ())
       in
       [ delta ], fun () -> Ok ()
     | Parent_event borrow ->
       (match t.legacy_callback_scope with
        | Some scope when phys_equal scope.borrow borrow ->
          let%bind _ = callback_agent_actor t ~scope in
          let%map delta =
            Run_child_work.prepare_callback
              t.state
              ~run
              ~executing:borrow.receipt
              ~work_state
              ~children:jobs
              ~now:(t.services.now ())
          in
          [ delta ], fun () -> Ok ()
        | None | Some _ ->
          let%bind owner =
            run_actions_internal t borrow.receipt
            |> Result.bind ~f:(fun owner ->
              Result.of_option
                owner
                ~error:(error Invalid_state "Run event owner is absent"))
          in
          let scope = Run_action_service.scope owner in
          let%bind prepared =
            Run_transition.add_owned_work
              t.state
              ~scope
              ~executing:borrow.receipt
              ~work_state
              ~owned_work:children
              ~now:(t.services.now ())
          in
          let%map advance =
            Run_scope.Host.prepare_advance
              scope
              ~previous:run
              ~next:(Run_transition.run prepared)
              ~execution_id:borrow.receipt.context.id
          in
          ( [ Run_transition.delta prepared ]
          , fun () -> Run_scope.Host.acknowledge_advance advance )))
;;

let prepare_selected_child_run t execution job_deltas =
  let open Result.Let_syntax in
  if
    not
      (List.exists job_deltas ~f:(function
         | Session_delta.Job_changed _ -> true
         | _ -> false))
  then Ok ([], (fun () -> Ok ()), fun () -> Ok ())
  else (
    let%bind current =
      match execution with
      | Inference_invocation invocation ->
        authorize_native_child_basis t ~read_current:(fun () ->
          selected_invocation_basis t invocation)
      | Inference_operation _
      | Inference_moderator _
      | Inference_event _
      | Inference_job _
      | Inference_initialization _
      | Inference_initialization_job _ -> authorized_native_child_basis t execution
    in
    let%bind deltas, acknowledge =
      staged_child_run_delta
        t
        execution
        ~basis:current.selected.execution
        ~classification:current.selected.classification
        ~parent_work:current.selected.parent_work
        job_deltas
    in
    let%map () = current.check_current () in
    deltas, current.check_current, acknowledge)
;;

let inference_agent_admission t = function
  | Inference_operation (operation, _) -> operation_agent_admission t operation.id
  | Inference_invocation execution -> execution_agent_admission t execution
  | Inference_moderator borrow -> moderator_agent_admission t borrow
  | Inference_event borrow -> event_agent_admission t borrow
  | Inference_job scope -> job_agent_admission t scope
  | Inference_initialization scope | Inference_initialization_job (scope, _) ->
    Ok (Some scope.agent_admission)
;;

(* Only the actual issuing constructor or its physically registered callback
   owns constructor bookkeeping. A current Init elsewhere is never a fallback. *)
let initialization_of_inference_execution t execution =
  let open Result.Let_syntax in
  let callback borrow =
    match t.legacy_callback_scope with
    | Some callback when phys_equal callback.borrow borrow ->
      let%bind _ = callback_inference_actor t ~scope:callback in
      (match callback.constructor_scope with
       | None -> Ok None
       | Some scope ->
         let%map () = validate_initialization_scope_current t scope in
         Some scope)
    | None | Some _ -> Ok None
  in
  match execution with
  | Inference_initialization scope | Inference_initialization_job (scope, _) ->
    let%map () = validate_initialization_scope_current t scope in
    Some scope
  | Inference_event borrow -> callback borrow
  | Inference_invocation execution ->
    (match execution.owner with
     | Event_moderator borrow -> callback borrow
     | Foreground _ | Background_job _ | Invocation_moderator _ -> Ok None)
  | Inference_operation _ | Inference_job _ | Inference_moderator _ -> Ok None
;;

let admit_model_job t execution candidate =
  let open Result.Let_syntax in
  let%bind current = authorized_inference_execution_basis t execution in
  let%bind constructor = initialization_of_inference_execution t execution in
  let%bind preparation =
    match constructor with
    | None -> Ok current.basis.preparation
    | Some scope ->
      if
        Option.for_all current.basis.preparation ~f:(fun existing ->
          Option.exists scope.run_preparation ~f:(phys_equal existing))
      then Ok scope.run_preparation
      else Error (error Conflict "constructor model preparation changed")
  in
  let%bind job =
    Model_job_admission.prepare
      candidate
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      ~origin:(inference_origin current.basis)
  in
  let%bind () =
    if
      List.exists t.state.jobs ~f:(fun existing ->
        Agent_protocol.Id.Job.equal existing.id job.id)
    then Error (error Conflict "model Job ID already exists")
    else Ok ()
  in
  let%bind () =
    match execution with
    | Inference_initialization _ | Inference_initialization_job _ ->
      if
        Option.exists t.state.run_state ~f:(fun index ->
          List.exists (Run_state.runs index) ~f:(fun run ->
            match run.Agent_protocol.Run.lifecycle with
            | Admitted | Active | Waiting _ -> true
            | Terminal _ -> false))
      then
        Error
          (error
             Permission_denied
             "constructor model work requires its actual originating Run reservation")
      else Ok ()
    | Inference_operation _
    | Inference_invocation _
    | Inference_moderator _
    | Inference_event _
    | Inference_job _ -> Ok ()
  in
  let job_delta = Session_delta.Job_changed job in
  let%bind run_deltas, acknowledge =
    staged_child_run_delta
      t
      execution
      ~basis:current.basis
      ~classification:(Some (inference_origin current.basis))
      ~parent_work:None
      [ job_delta ]
  in
  let%bind parent =
    inference_agent_admission t execution
    |> Result.bind ~f:(fun parent ->
      Result.of_option
        parent
        ~error:(error Permission_denied "model parent admission is unavailable"))
  in
  let%bind selection =
    match constructor, preparation with
    | Some _, Some preparation ->
      Run_preparation.Constructor_jobs.prepare_add
        preparation
        ~owner:t.run_preparation_owner
        ~state:t.state
        ~job
      |> Result.map ~f:Option.some
    | None, _ | Some _, None -> Ok None
  in
  let%bind binding = job_moderator_binding t in
  let%bind job_custody =
    Job_custody.prepare_fresh t.job_agent_custody ~job ~binding ~parent
  in
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () -> if not !acknowledged then Job_custody.Update.discard job_custody)
    ~f:(fun () ->
      let revision = t.state.counters.revision in
      let%bind prepared =
        prepare_transition
          ?run_preparation:preparation
          t
          ~delta:(Session_delta.Batch (job_delta :: run_deltas))
          ~payloads:[ Agent_protocol.Event.Durable.Payload.Job_state_changed job ]
      in
      let initialization_acknowledgement =
        Option.map constructor ~f:(fun scope ->
          Initialization_scope.Acknowledgement.prepare
            ~jobs:(job.id :: scope.jobs)
            scope
            prepared.state)
      in
      Eio.Cancel.protect (fun () ->
        (* This selection check may refresh host policy. All original/physical
           and immutable-basis checks below run after that refresh. *)
        let%bind () =
          Option.value_map selection ~default:(Ok ()) ~f:(fun selection ->
            Run_preparation.Constructor_jobs.check_before_commit
              selection
              ~owner:t.run_preparation_owner
              ~state:t.state)
        in
        let%bind () = current.check_current () in
        let%bind () =
          Option.value_map constructor ~default:(Ok ()) ~f:(fun scope ->
            let%bind () = validate_initialization_scope_current t scope in
            check_initialization_agent_actor scope)
        in
        let%bind () = check_job_moderator_binding t binding in
        let%bind () =
          Job_custody.Update.check_before_commit job_custody ~current:t.job_agent_custody
        in
        let%bind () =
          Option.value_map
            initialization_acknowledgement
            ~default:(Ok ())
            ~f:Initialization_scope.Acknowledgement.check_basis
        in
        let%bind () =
          Option.value_map preparation ~default:(Ok ()) ~f:(fun preparation ->
            let%bind () =
              Run_preparation.check_current
                preparation
                ~owner:t.run_preparation_owner
                ~state:t.state
            in
            Run_preparation.check_basis
              preparation
              ~owner:t.run_preparation_owner
              ~state:t.state)
        in
        let%bind () =
          if Int64.equal revision t.state.counters.revision
          then Ok ()
          else Error (error Conflict "model admission basis changed before persistence")
        in
        let%bind () = persist_transition t prepared in
        install_committed_transition
          ?run_preparation:preparation
          ~job_custody
          ?initialization_acknowledgement
          t
          prepared;
        acknowledged := true;
        Option.iter selection ~f:Run_preparation.Constructor_jobs.acknowledge;
        let%map () = acknowledge () in
        publish_committed_transition t prepared;
        job))
;;

let prepare_background_job t owner request =
  let open Result.Let_syntax in
  let%bind launch = derive_background_launch t owner in
  let%bind execution = background_inference_execution t owner in
  let%bind current = authorized_native_child_basis t execution in
  let basis = current.selected.execution in
  let%bind () =
    match basis.run with
    | None -> Ok ()
    | Some _ ->
      let%bind parent = child_parent t execution in
      (match parent with
       | Parent_work _ | Parent_observation _ -> Ok ()
       | Parent_event borrow ->
         (* The actual event owns this scope even if its caller has not staged
            a Run action. A child cannot rely on a later optional lookup. *)
         run_actions_internal t borrow.receipt |> Result.map ~f:ignore)
  in
  let%map () = current.check_current () in
  Agent_protocol.Job.
    { id = Agent_protocol.Id.Job.create ()
    ; session_id = t.state.identity.session_id
    ; generation = t.state.identity.generation
    ; kind = Async_tool
    ; payload = Chat_response.Background_request.to_json request
    ; status = Queued
    ; retry_policy = Never
    ; attempt = 0
    ; created_at = t.services.now ()
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Pending
    ; launch = Some launch
    ; inference_origin = current.selected.classification
    ; progress = None
    }
;;

let stage_background_job_internal t (job : Agent_protocol.Job.t) capacity =
  let open Result.Let_syntax in
  let result =
    let%bind launch =
      Result.of_option job.launch ~error:(error Invalid_request "job has no launch owner")
    in
    let%bind expected = derive_background_launch t launch.owner in
    let%bind execution = background_inference_execution t launch.owner in
    let%bind current = authorized_native_child_basis t execution in
    let%bind () =
      if
        Option.equal
          Agent_protocol.Inference_origin.equal
          job.inference_origin
          current.selected.classification
      then Ok ()
      else
        Error
          (error
             Permission_denied
             "background Job origin differs from its current parent")
    in
    let%bind () =
      Extension_invariants.owner
        ~session_id:t.state.identity.session_id
        ~generation:t.state.identity.generation
        job.session_id
        job.generation
    in
    let%bind () =
      match
        ( Agent_protocol.Job.equal_launch launch expected
        , job.kind
        , job.status
        , job.retry_policy
        , job.attempt
        , job.started_at
        , job.next_run_at
        , job.completed_at
        , job.result
        , job.delivery )
      with
      | true, Async_tool, Queued, Never, 0, None, None, None, None, Pending -> Ok ()
      | _ ->
        Error
          (error
             Invalid_request
             "staged launch must be a fresh job with current ancestry")
    in
    let%bind _ = Agent_protocol.Job.of_json (Agent_protocol.Job.to_json job) in
    let%bind () =
      match
        List.exists t.state.jobs ~f:(fun existing ->
          Agent_protocol.Id.Job.equal existing.id job.id)
      with
      | true -> Error (error Conflict "background job already exists")
      | false -> Ok ()
    in
    let%bind parent =
      match launch.owner with
      | Invocation id ->
        (match
           List.find t.invocation_executions ~f:(fun execution ->
             Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id)
         with
         | Some execution -> execution_agent_admission t execution
         | None ->
           (match t.moderator_borrow with
            | Some borrow
              when Agent_protocol.Id.Invocation.equal borrow.invocation.context.id id ->
              moderator_agent_admission t borrow
            | Some _ | None -> Error (error Conflict "staged Job parent is unavailable")))
      | Moderator_event id ->
        (match t.queued_event_borrow with
         | Some borrow
           when Agent_protocol.Id.Moderator_execution.equal borrow.receipt.context.id id
           -> event_agent_admission t borrow
         | Some _ | None ->
           Error (error Conflict "staged Job event parent is unavailable"))
    in
    let%bind () = current.check_current () in
    let%bind fresh =
      match parent with
      | None -> Ok None
      | Some parent ->
        let%bind binding = job_moderator_binding t in
        Job_custody.Fresh.stage ~job ~binding ~parent |> Result.map ~f:Option.some
    in
    let capacity =
      Staged_jobs.
        { publish = capacity.publish
        ; abort =
            (fun () ->
              Option.iter fresh ~f:Job_custody.Fresh.discard;
              capacity.abort ())
        }
    in
    let staged = ref false in
    Exn.protect
      ~finally:(fun () ->
        if not !staged then Option.iter fresh ~f:Job_custody.Fresh.discard)
      ~f:(fun () ->
        let%map () = Staged_jobs.stage t.staged_jobs ~job ~payload:fresh ~capacity in
        staged := true)
  in
  match result with
  | Ok () -> Ok ()
  | Error _ as failure ->
    Eio.Cancel.protect capacity.Staged_jobs.abort;
    failure
;;

let finish_invocation t execution outcome requests commit_starts authoring_reference =
  let open Result.Let_syntax in
  let revision = t.state.counters.revision in
  let owner = Agent_protocol.Job.Invocation execution.dispatched.context.id in
  let%bind cancelled =
    match execution.owner with
    | Foreground operation_id ->
      let%map operation = current_operation t operation_id in
      (match operation.state with
       | Cancelling -> true
       | _ -> false)
    | Invocation_moderator borrow ->
      (match t.moderator_borrow with
       | Some current when phys_equal current borrow ->
         (match borrow.operation_id with
          | None when t.idle_moderator_borrowed ->
            Ok
              (borrow.cancel_requested
               || Option.exists borrow.job_scope ~f:(fun scope -> scope.cancel_requested)
              )
          | Some id ->
            let%map operation = current_operation t id in
            borrow.cancel_requested
            ||
              (match operation.state with
              | Cancelling -> true
              | _ -> false)
          | None -> Error (error Conflict "moderator invocation scope has ended"))
       | _ -> Error (error Conflict "moderator invocation scope has ended"))
    | Event_moderator borrow ->
      (match t.queued_event_borrow with
       | Some current when phys_equal current borrow && t.idle_moderator_borrowed ->
         Ok
           (borrow.cancel_requested
            || Option.exists borrow.job_scope ~f:(fun scope -> scope.cancel_requested)
            || Option.exists t.state.active_operation ~f:(fun operation ->
              Option.exists
                borrow.receipt.context.operation_id
                ~f:(Agent_protocol.Id.Operation.equal operation.id)
              &&
              match operation.state with
              | Cancelling -> true
              | _ -> false))
       | _ -> Error (error Conflict "event invocation scope has ended"))
    | Background_job scope ->
      let%map job = job_scope_current t scope in
      scope.cancel_requested
      ||
        (match job.status with
        | Running | Waiting_permission _ -> false
        | Waiting_completion _ | Queued | Succeeded | Failed _ | Cancelled | Interrupted _
          -> true)
  in
  let%bind () =
    if List.exists t.invocation_executions ~f:(phys_equal execution)
    then Ok ()
    else Error (error Conflict "invocation callback no longer owns its result")
  in
  let outcome =
    match cancelled with
    | true ->
      Agent_protocol.Invocation.Cancelled
        (match execution.owner with
         | Foreground _ -> "operation cancelled"
         | Background_job _ -> "background job cancelled"
         | Invocation_moderator { operation_id = Some _; _ } ->
           "moderator scope cancelled"
         | Invocation_moderator { operation_id = None; _ } | Event_moderator _ ->
           "idle moderator cancelled")
    | false -> outcome
  in
  (* The callback has returned. Even when outcome persistence fails and the
     execution stays registered for cleanup, it cannot authorize new children. *)
  execution.accepts_children <- false;
  Atomic.set execution.authority_live false;
  (match commit_starts && not cancelled, outcome with
   | true, (Complete _ | Fail _ | Pending _) -> ()
   | false, _ | true, Cancelled _ -> abort_staged_work t ~owner);
  let%bind resolved =
    Agent_protocol.Invocation.resolve
      ?authoring_reference:
        (match outcome with
         | Complete _ -> authoring_reference
         | Fail _ | Pending _ | Cancelled _ -> None)
      execution.dispatched
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      outcome
  in
  let%bind resolved =
    match requests, outcome with
    | Some requests, (Complete _ | Fail _ | Pending _) ->
      Agent_protocol.Invocation.record_handler_intent resolved ~requests
    | None, _ | Some _, Cancelled _ -> Ok resolved
  in
  let permissions, permission_deltas, permission_payloads =
    cleanup_invocation_permissions t [ execution.dispatched.context.id ]
  in
  let%bind job_deltas, job_payloads = staged_work_changes t owner in
  let%bind run_deltas, check_run, acknowledge_run =
    prepare_selected_child_run t (Inference_invocation execution) job_deltas
  in
  (* Completing the selected result retains negative expiry evidence. New work
     and proof borrows separately check the Admission; this is not a renewal. *)
  let%bind parent = execution_settlement_parent t execution in
  let custody = settlement_custody parent in
  let%bind source = observation_source t in
  let%bind observation =
    Observation_continuation.stage_completed resolved ~source ~parent
  in
  let%bind _ =
    with_selected_intent_agent_custody
      ?observation
      t
      owner
      (Invocation_intent resolved.context.id)
      ~pending:(Observation_follow_up.pending resolved)
      custody
      (fun ~job_custody ~check_jobs ~acknowledge ->
         let%bind prepared =
           prepare_transition
             t
             ~delta:
               (Session_delta.Batch
                  ((Session_delta.Invocation_changed resolved :: permission_deltas)
                   @ job_deltas
                   @ run_deltas))
             ~payloads:(permission_payloads @ job_payloads)
         in
         let constructor_borrow =
           match execution.owner with
           | Event_moderator borrow -> Some borrow
           | Foreground _ | Invocation_moderator _ | Background_job _ -> None
         in
         let%bind initialization_acknowledgement =
           match constructor_borrow with
           | None -> Ok None
           | Some borrow ->
             prepare_constructor_invocation_ack
               ~permissions
               ?permission_lifecycle:
                 (List.find_map permission_deltas ~f:(function
                    | Session_delta.Lifecycle_changed lifecycle -> Some lifecycle
                    | _ -> None))
               t
               borrow
               ~before:(Some execution.dispatched)
               ~after:resolved
               prepared
         in
         let run_preparation =
           Option.bind constructor_borrow ~f:(fun borrow ->
             Option.bind (constructor_scope_of_borrow t borrow) ~f:(fun scope ->
               scope.Initialization_scope.run_preparation))
         in
         Eio.Cancel.protect (fun () ->
           let%bind () = check_jobs () in
           let%bind () = check_run () in
           let%bind () =
             Option.value_map constructor_borrow ~default:(Ok ()) ~f:(fun borrow ->
               check_constructor_invocation_ack t borrow initialization_acknowledgement)
           in
           let%bind () =
             if Int64.equal revision t.state.counters.revision
             then Ok ()
             else Error (error Conflict "Invocation ACK basis changed before commit")
           in
           let%bind () = persist_transition t prepared in
           install_committed_transition
             ~job_custody
             ?run_preparation
             ?initialization_acknowledgement
             t
             prepared;
           acknowledge ();
           let%bind () = acknowledge_run () in
           publish_committed_transition t prepared;
           Ok ()))
  in
  resolve_cleaned_permission_waiters t permissions;
  t.invocation_executions
  <- List.filter t.invocation_executions ~f:(fun other ->
       not (phys_equal other execution));
  Ok resolved
;;

let commit_moderator_invocation t borrow (resolved : Agent_protocol.Invocation.t) snapshot
  =
  let open Result.Let_syntax in
  let%bind () = validate_moderator_borrow t borrow in
  let%bind () =
    match borrow.callback_finished with
    | false -> Ok ()
    | true -> Error (error Conflict "moderator callback has ended")
  in
  let%bind () =
    match
      List.exists t.invocation_executions ~f:(invocation_execution_owned_by borrow)
    with
    | true ->
      Error (error Conflict "moderator child invocations still require completion")
    | false -> Ok ()
  in
  let%bind () =
    match borrow.job_scope, borrow.operation_id with
    | Some scope, None -> job_scope_can_execute t scope
    | Some _, Some _ ->
      Error (error Conflict "job moderator cannot own a model operation")
    | None, Some operation_id ->
      Result.map (running_operation ~allow_stopping:true t operation_id) ~f:ignore
    | None, None ->
      (match t.state.lifecycle.desired, t.state.lifecycle.observed, t.state.failure with
       | Running, Idle, None when not t.state.halted -> Ok ()
       | _ -> Error (error Conflict "idle observation was stopped before acknowledgement"))
  in
  let%bind () =
    if borrow.committed
    then Error (error Already_resolved "moderator invocation is already committed")
    else if
      Agent_protocol.Id.Invocation.compare
        resolved.context.id
        borrow.invocation.context.id
      <> 0
    then Error (error Conflict "resolution does not belong to this moderator borrow")
    else (
      match borrow.kind, resolved.status with
      | Invocation, Resolved _ ->
        Agent_protocol.Invocation.validate_transition
          ~previous:(Some borrow.invocation)
          resolved
      | Observation, (Resolved _ | Published _) ->
        let%bind () =
          match resolved.observation with
          | Some { status = Observed; observer }
            when String.equal
                   observer.script_id
                   snapshot.Session.Moderator_state.Identity_snapshot.script_id
                 && String.equal observer.source_sha256 snapshot.script_source_hash ->
            Ok ()
          | _ ->
            Error
              (error
                 Conflict
                 "observation acknowledgement requires its source-bound moderator \
                  checkpoint")
        in
        Agent_protocol.Invocation.validate_transition
          ~previous:(Some borrow.invocation)
          resolved
      | _ -> Error (error Invalid_state "moderator commit requires a resolved invocation"))
  in
  let owner = Agent_protocol.Job.Invocation borrow.invocation.context.id in
  (match borrow.kind, resolved.status with
   | Invocation, Resolved (Cancelled _) -> abort_staged_work t ~owner
   | _ -> ());
  let%bind job_deltas, job_payloads = staged_work_changes t owner in
  let%bind run_deltas, check_run, acknowledge_run =
    prepare_selected_child_run t (Inference_moderator borrow) job_deltas
  in
  let%bind parent = moderator_settlement_parent t borrow in
  let custody = settlement_custody parent in
  let%bind observation =
    match borrow.kind with
    | Invocation ->
      let%bind source = observation_source t in
      Observation_continuation.stage_completed resolved ~source ~parent
    | Observation ->
      if observation_pending resolved
      then (
        match borrow.observation with
        | Some observation ->
          Observation_continuation.stage_follow_up observation ~resolved
          |> Result.map ~f:Option.some
        | None ->
          Error (error Internal_error "observation borrow lost its settlement component"))
      else Ok None
  in
  let%bind _ =
    with_selected_intent_agent_custody
      ?observation
      t
      owner
      (Invocation_intent resolved.context.id)
      ~pending:(Observation_follow_up.pending resolved)
      custody
      (fun ~job_custody ~check_jobs ~acknowledge ->
         let%bind prepared =
           prepare_transition
             t
             ~delta:
               (Session_delta.Batch
                  ([ Session_delta.Invocation_changed resolved
                   ; Moderator_changed
                       (Some (Runtime_builder.encode_moderator_snapshot snapshot))
                   ]
                   @ job_deltas
                   @ run_deltas))
             ~payloads:job_payloads
         in
         Eio.Cancel.protect (fun () ->
           let%bind () = check_jobs () in
           let%bind () = check_run () in
           let%bind () = persist_transition t prepared in
           install_committed_transition ~job_custody t prepared;
           borrow.committed <- true;
           acknowledge ();
           let%bind () = acknowledge_run () in
           publish_committed_transition t prepared;
           Ok ()))
  in
  Ok ()
;;

let check_inference_allocation t (current : inference_currentness) owner handle =
  let open Result.Let_syntax in
  let%bind registration = inference_registration t owner handle in
  (* Final checks run after every composed refresh. Sample only an actual
     registered Goal deadline, before original/physical/policy checks. Sampling
     in a refresh could stale across a later refresh. *)
  let%bind deadline_sample =
    match Inference_owner.Registration.goal_id registration with
    | None -> Ok None
    | Some id ->
      let%map goal =
        Option.bind t.state.goal_catalog ~f:(fun catalog -> Goal_catalog.find catalog id)
        |> Result.of_option ~error:(error Invalid_state "inference Goal is absent")
      in
      Option.map
        (Agent_protocol.Goal.Budget.deadline (Agent_protocol.Goal.budget goal))
        ~f:(fun deadline -> deadline, t.services.now ())
  in
  let%bind current_registration = inference_registration t owner handle in
  let%bind () =
    if phys_equal registration current_registration
    then Ok ()
    else Error (error Conflict "inference registration changed during deadline sampling")
  in
  let%bind () =
    if Inference_owner.Registration.policy_is_current registration
    then Ok ()
    else Error (error Permission_denied "goal inference policy changed")
  in
  let%bind () = current.check_current () in
  let actual = Option.map current.basis.attribution ~f:Goal_work_attribution.goal_id in
  let%bind () =
    if
      Option.equal
        Agent_protocol.Id.Goal.equal
        actual
        (Inference_owner.Registration.goal_id registration)
    then Ok ()
    else Error (error Permission_denied "inference Goal origin changed")
  in
  match deadline_sample with
  | Some (deadline, now) when Agent_protocol.Timestamp.compare now deadline >= 0 ->
    Error (error Permission_denied "goal deadline reached before submission")
  | None | Some _ -> Ok ()
;;

let allocate_inference t execution owner relation configuration =
  let open Result.Let_syntax in
  let%bind () =
    match execution with
    | Inference_initialization _ ->
      Error
        (error
           Permission_denied
           "initializer inference requires its exact admitted running model Job")
    | Inference_initialization_job _
    | Inference_operation _
    | Inference_invocation _
    | Inference_moderator _
    | Inference_event _
    | Inference_job _ -> Ok ()
  in
  let%bind () = validate_inference_owner t owner in
  let%bind () =
    if
      Int.equal (Graph_owner.generation owner) t.state.identity.generation
      &&
      match Graph_owner.phase owner with
      | Open -> true
      | Sealed | Finished -> false
    then Ok ()
    else Error (error Conflict "inference graph excludes new attempts")
  in
  let%bind current = authorized_inference_execution_basis t execution in
  let basis = current.basis in
  let%bind decision =
    Goal_charge_admission.prepare
      t.state
      ~attribution:basis.attribution
      ~now:(t.services.now ())
      ~dimension:Attempt
  in
  match decision with
  | Held { goal_id; reason = _; delta } ->
    let%bind () =
      match delta with
      | None -> Ok ()
      | Some delta ->
        transition
          ?run_preparation:basis.preparation
          ~admission_hold:goal_id
          t
          ~delta
          ~payloads:[]
        |> Result.map ~f:ignore
    in
    Error (error Permission_denied "goal policy holds fresh inference admission")
  | (Unbound | Ready _) as decision ->
    let%bind protected_ordinals =
      Goal_ledger_reconciliation.protected_ordinals
        t.state.goal_accounts
        ~ledger:t.state.inference_ledger
    in
    let%bind ledger, handle, tracking =
      Inference_ledger.admit
        ~protected_ordinals
        t.state.inference_ledger
        ~source:(Graph_owner.source owner)
        ~relation
        ~operation_id:basis.operation_id
        ~invocation_id:basis.invocation_id
        ~configuration
      |> Result.map_error ~f:inference_error
    in
    let admission =
      match decision with
      | Unbound -> None
      | Ready ready ->
        Some
          ( Goal_charge_admission.goal_id ready
          , Goal_accounting.Admission.Attempt
              { episode = Goal_charge_admission.episode ready; handle; tracking } )
      | Held _ -> assert false
    in
    let%bind () =
      commit_inference_ledger ?run_preparation:basis.preparation ?admission t ledger
    in
    Graph_owner.register
      owner
      ~handle
      ~configuration
      ~goal_id:(Option.map admission ~f:fst);
    let submission_guard =
      Inference_runtime.Submission_guard.create ~refresh:(fun () ->
        match call t (Refresh_inference (execution, owner, handle)) with
        | Ok check -> check
        | Error error -> raise (Inference_allocation.Rejected error))
    in
    Ok (Inference_allocation.Host.create ~handle ~submission_guard)
;;

let refresh_inference t execution owner handle =
  let open Result.Let_syntax in
  let%bind current = authorized_inference_execution_basis t execution in
  let%map () = check_inference_allocation t current owner handle in
  (* Host authorization refresh is finished. The captured exact physical owner,
     binding, original proof, source and policy are checked directly after every
     composed refresh and auth/transport wait. This closure samples the trusted
     nonyielding wall clock before its final checks, performs no actor request or
     host authorization refresh, and charges no fresh attempt. *)
  fun () ->
    match check_inference_allocation t current owner handle with
    | Ok () -> ()
    | Error error -> raise (Inference_allocation.Rejected error)
;;

let capture_initialization_recipe_target t scope job_id generation attempt target limits =
  let open Result.Let_syntax in
  let%bind () = validate_initialization_model_job t scope job_id generation attempt in
  capture_recipe_target
    ?run_preparation:scope.Initialization_scope.run_preparation
    t
    job_id
    generation
    attempt
    target
    limits
;;

let complete_initialization_model_job t scope job_id generation attempt outcome =
  let open Result.Let_syntax in
  (* Acknowledging an already completed attempt remains cleanup after original
     expiry. New provider admission/target capture uses the currentness guard. *)
  let%bind owned = initialization_model_job_is_owned t scope job_id generation attempt in
  let%bind () =
    if owned
    then Ok ()
    else Error (error Conflict "initialization does not own this running model attempt")
  in
  complete_job
    ?run_preparation:scope.Initialization_scope.run_preparation
    t
    job_id
    generation
    attempt
    outcome
;;

let finish_background_scopes t job_id generation attempt =
  List.filter t.job_scopes ~f:(fun scope ->
    (not scope.active)
    && Agent_protocol.Id.Job.equal scope.job.id job_id
    && Int.equal scope.job.generation generation
    && Int.equal scope.job.attempt attempt)
  |> List.fold_result ~init:() ~f:(fun () scope ->
    match finish_job_scope t scope with
    | Ok () -> Ok ()
    | Error failure ->
      (match List.mem t.job_scopes scope ~equal:phys_equal with
       | true -> Error failure
       | false -> Ok ()))
;;

let defer_background_job t job_id generation attempt dependency =
  let open Result.Let_syntax in
  let%bind () = finish_background_scopes t job_id generation attempt in
  let%bind job = running_job_for_completion t job_id generation attempt in
  let next = { job with status = Waiting_completion dependency; result = None } in
  let%bind () = validate_job_dependency t next in
  let%bind job_custody =
    Job_custody.prepare_defer t.job_agent_custody ~previous:job ~waiting:next
  in
  let%map _ = update_job ~job_custody t next in
  next
;;

type background_control =
  | Cancelled_control
  | Interrupted_control

let complete_background_job
      ?(waiting = false)
      ?completed_at
      ?control
      t
      job_id
      generation
      attempt
      completion
  =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  let%bind () =
    match job.kind with
    | Async_tool -> Ok ()
    | _ -> Error (error Invalid_request "generic completion requires an async tool job")
  in
  let%bind () = Agent_protocol.Completion.validate completion in
  let original_completion = completion in
  let%bind completion =
    match control with
    | None -> Ok completion
    | Some (Cancelled_control | Interrupted_control) ->
      Run_job_capacity.normalize_host_control completion |> Result.map ~f:fst
  in
  let awaited = Run_job_completion.awaits t.state job in
  let%bind capacity =
    if awaited && (Option.is_none control || Option.is_some t.services.job_results)
    then
      Result.map
        (Run_job_capacity.capture job ~publisher:t.services.job_results)
        ~f:Option.some
    else Ok None
  in
  let%bind completion, delivery_limited =
    match capacity with
    | None -> Ok (completion, false)
    | Some capacity -> Run_job_capacity.normalize_completion capacity completion
  in
  let completion, storage_limited =
    match t.services.job_results with
    | Some publisher
      when Result.is_error
             (Agent_store.Job_result_store.Publisher.check_completion
                publisher
                completion) ->
      ( (match completion with
         | Cancelled _ when awaited ->
           Agent_protocol.Completion.Cancelled
             "The job was cancelled; its diagnostic exceeds the host storage limit."
         | Cancelled _ | Expired -> completion
         | Succeeded _ | Failed _ ->
           Agent_protocol.Completion.Failed
             { code = "background.result_limit"
             ; message = "The completed result exceeds the host storage limit."
             ; retryable = false
             ; details = `Null
             })
      , true )
    | _ -> completion, delivery_limited
  in
  let%bind () = finish_background_scopes t job_id generation attempt in
  let%bind job =
    match job.status, waiting, control with
    | Waiting_permission _, _, Some (Cancelled_control | Interrupted_control)
    | Waiting_completion _, true, _ -> Ok job
    | ( ( Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ )
      , _
      , _ ) -> running_job_for_completion t job_id generation attempt
  in
  let encoded = Agent_protocol.Completion.to_json completion in
  let%bind cancelled, subscriptions =
    match completion with
    | Cancelled _ | Expired -> cancelled_work_dependencies t job
    | Succeeded _ | Failed _ -> Ok ([], [])
  in
  let terminal status =
    let terminal = terminal_job t job status (Some encoded) in
    match completed_at with
    | None -> terminal
    | Some at -> { terminal with completed_at = Some at }
  in
  let running = job in
  let%bind interruption =
    match control, completion with
    | Some Interrupted_control, Failed failure -> Ok (Some failure.message)
    | Some Cancelled_control, Cancelled _
    | None, (Succeeded _ | Failed _ | Cancelled _ | Expired) -> Ok None
    | Some Interrupted_control, (Succeeded _ | Cancelled _ | Expired)
    | Some Cancelled_control, (Succeeded _ | Failed _ | Expired) ->
      Error (error Invalid_request "host control disagrees with its actual outcome")
  in
  let job =
    match interruption, completion with
    | Some reason, (Succeeded _ | Failed _ | Cancelled _ | Expired) ->
      terminal (Interrupted reason)
    | None, Agent_protocol.Completion.Succeeded _ -> terminal Succeeded
    | None, Cancelled _ -> terminal Cancelled
    | None, Expired ->
      terminal (Failed (error Resource_limit "background job deadline elapsed"))
    | None, Failed failure ->
      let retry =
        match job.status with
        | Waiting_completion _ -> None
        | _ -> retry_limits job.retry_policy
      in
      (match failure.retryable, retry with
       | true, Some (maximum, backoff_ms) when job.attempt < maximum ->
         { (retry_job t job failure.message backoff_ms) with result = Some encoded }
       | _ ->
         terminal
           (Failed
              (Agent_protocol.Error.create
                 Internal_error
                 ~message:failure.message
                 ~retryable:failure.retryable
                 ~data:encoded
                 ())))
  in
  let persist stored =
    let status =
      match job.status, stored with
      | Failed failure, Agent_protocol.Stored_completion.Artifact _ ->
        Agent_protocol.Job.Failed { failure with data = `Null }
      | status, _ -> status
    in
    let job =
      { job with
        status
      ; result =
          Some
            (match status with
             | Queued -> encoded
             | Running
             | Waiting_permission _
             | Waiting_completion _
             | Succeeded
             | Failed _
             | Cancelled
             | Interrupted _ -> Agent_protocol.Stored_completion.to_json stored)
      }
    in
    let terminal_status =
      match interruption, completion with
      | Some reason, (Succeeded _ | Failed _ | Cancelled _ | Expired) ->
        Agent_protocol.Job.Interrupted reason
      | None, Succeeded _ -> Succeeded
      | None, Failed failure ->
        Failed
          (Agent_protocol.Error.create
             Internal_error
             ~message:failure.message
             ~retryable:failure.retryable
             ~data:`Null
             ())
      | None, Cancelled _ -> Cancelled
      | None, Expired -> Failed (error Resource_limit "background job deadline elapsed")
    in
    let terminal =
      terminal terminal_status
      |> fun (terminal : Agent_protocol.Job.t) ->
      { terminal with result = Some (Agent_protocol.Stored_completion.to_json stored) }
    in
    let outcome =
      match control with
      | Some Interrupted_control -> Agent_protocol.Run_work.Terminal.Interrupted
      | None | Some Cancelled_control ->
        (match original_completion with
         | Succeeded _ ->
           if storage_limited then Agent_protocol.Run_work.Terminal.Limited else Succeeded
         | Failed _ -> Failed
         | Cancelled _ -> Cancelled
         | Expired -> Limited)
    in
    let%bind run_deltas =
      Run_job_completion.prepare
        t.state
        ~running
        ~terminal
        ~successor:job
        ~outcome
        ~now:(t.services.now ())
    in
    let%bind job_custody =
      match job.status with
      | Queued ->
        Result.map
          (Job_custody.prepare_retry t.job_agent_custody ~previous:running ~queued:job)
          ~f:Option.some
      | Succeeded | Failed _ | Cancelled | Interrupted _ ->
        Result.map (prepare_terminal_job_custody t (job :: cancelled)) ~f:Option.some
      | Running | Waiting_permission _ | Waiting_completion _ ->
        Error (error Invalid_state "completed background Job retained an active state")
    in
    let%map _ =
      update_jobs ?job_custody ~subscriptions ~run_deltas t (job :: cancelled)
    in
    job
  in
  match job.status, t.services.job_results, storage_limited, awaited with
  | Queued, _, _, false | _, None, _, _ | _, _, true, _ ->
    persist (Agent_protocol.Stored_completion.Inline completion)
  | _, Some publisher, false, _ ->
    Agent_store.Job_result_store.Publisher.publish
      publisher
      ~storage:
        (match capacity with
         | Some capacity -> Run_job_capacity.storage capacity
         | None -> Automatic)
      ~jobs:t.state.jobs
      ~job:running
      ~now:(t.services.now ())
      completion
      ~persist
;;

let recover_background_results t max_count max_total_bytes =
  let open Result.Let_syntax in
  let%bind () =
    match
      t.job_scopes, t.invocation_executions, t.moderator_borrow, t.queued_event_borrow
    with
    | [], [], None, None -> Ok ()
    | _ -> Error (error Conflict "result recovery requires an idle execution host")
  in
  match t.services.job_results with
  | None -> Ok ()
  | Some publisher ->
    let%bind restored =
      Agent_store.Job_result_store.Publisher.restore
        publisher
        ~jobs:t.state.jobs
        ~generation:t.state.identity.generation
        ~max_count
        ~max_total_bytes
    in
    List.fold_result restored ~init:() ~f:(fun () (previous, completion, completed_at) ->
      let%bind job = find_job t previous.id in
      match job.status with
      | Running | Waiting_completion _ ->
        let%bind () = validate_job_dependency t job in
        complete_background_job
          ~waiting:true
          ~completed_at
          t
          job.id
          job.generation
          job.attempt
          completion
        |> Result.map ~f:ignore
      | _ -> Ok ())
;;

let refresh_background_job t job_id generation attempt =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  match job.status with
  | Waiting_completion dependency ->
    let%bind () = validate_job_dependency t job in
    let selected =
      Option.bind t.services.job_results ~f:(fun publisher ->
        Agent_store.Job_result_store.Publisher.pending_completion publisher ~job)
    in
    (match selected with
     | Some completion ->
       complete_background_job ~waiting:true t job_id generation attempt completion
     | None ->
       let%bind completion, completed_at =
         match dependency.work with
         | Subscription id ->
           let%map target = retained_subscription t id in
           target.result, target.completed_at
         | Job id ->
           let%bind target = find_job t id in
           let load_artifact =
             Option.map t.services.job_results ~f:(fun publisher ->
               Agent_store.Job_result_store.Publisher.load publisher)
           in
           let%map completion =
             match Agent_protocol.Job.terminal_completion ?load_artifact target with
             | Ok completion -> Ok completion
             | Error failure when not failure.retryable ->
               Ok
                 (Some
                    (Agent_protocol.Completion.Failed
                       { code = "background.artifact_unavailable"
                       ; message =
                           "The saved background result is unavailable or failed \
                            verification."
                       ; retryable = false
                       ; details = `Null
                       }))
             | Error _
               when Agent_protocol.Timestamp.compare
                      (t.services.now ())
                      dependency.deadline
                    >= 0 -> Ok (Some Agent_protocol.Completion.Expired)
             | Error _ as failure -> failure
           in
           completion, target.completed_at
       in
       (match completion with
        | Some completion ->
          let completion =
            match completed_at with
            | Some at when Agent_protocol.Timestamp.compare at dependency.deadline <= 0 ->
              completion
            | _ -> Agent_protocol.Completion.Expired
          in
          let%bind completion = Job_dependency.completion dependency completion in
          complete_background_job ~waiting:true t job_id generation attempt completion
        | None
          when Agent_protocol.Timestamp.compare (t.services.now ()) dependency.deadline
               >= 0 ->
          complete_background_job ~waiting:true t job_id generation attempt Expired
        | None -> Ok job))
  | _ -> Ok job
;;

let check_expected_moderator_checkpoint t = function
  | None -> Ok ()
  | Some expected ->
    (match t.state.moderator with
     | Some installed
       when Jsonaf.exactly_equal
              installed
              (Runtime_builder.encode_moderator_snapshot expected) -> Ok ()
     | _ -> Error (error Conflict "moderator checkpoint changed before external delivery"))
;;

let prepare_run_job_enqueue t ~frame =
  let open Result.Let_syntax in
  match t.state.run_state with
  | None -> Ok []
  | Some index ->
    (match
       List.find (Run_state.job_deliveries index) ~f:(fun delivery ->
         Chat_response.Background_delivery.equal (Run_job_delivery.frame delivery) frame)
     with
     | None -> Ok []
     | Some delivery ->
       let%bind () = check_run_binding t (Run_job_delivery.run_id delivery) in
       let%bind delivery = Run_job_delivery.enqueue delivery ~at:(t.services.now ()) in
       let%map index = Run_state.replace_job_delivery index delivery in
       [ Session_delta.Run_state_changed index ])
;;

let enqueue_run_job_delivery t expected before after =
  let open Result.Let_syntax in
  let%bind () = check_expected_moderator_checkpoint t (Some before) in
  let%bind () =
    if moderator_is_borrowed t
    then Error (error Conflict "moderator callback owns the checkpoint")
    else Ok ()
  in
  let%bind index =
    Result.of_option t.state.run_state ~error:(error Conflict "run index is absent")
  in
  let%bind retained =
    Result.of_option
      (Run_state.find_job_delivery index (Run_job_delivery.key expected))
      ~error:(error Conflict "run delivery is absent")
  in
  let%bind () =
    if Run_job_delivery.equal retained expected
    then Ok ()
    else Error (error Conflict "run delivery changed before enqueue")
  in
  let%bind () =
    match Run_job_delivery.disposition retained with
    | Pending -> Ok ()
    | Enqueued _ | Claimed _ | Retired _ ->
      Error (error Already_resolved "run delivery is no longer pending")
  in
  let frame = Run_job_delivery.frame retained in
  let observer : Agent_protocol.Invocation.observer =
    { script_id = before.script_id; source_sha256 = before.script_source_hash }
  in
  let%bind () =
    if Agent_protocol.Invocation.equal_observer observer frame.source
    then Ok ()
    else Error (error Conflict "run delivery source differs from moderator")
  in
  let%bind () =
    match check_run_binding t (Run_job_delivery.run_id retained) with
    | Ok () -> Ok ()
    | Error denied ->
      let%bind index =
        Run_retirement.interrupt_run
          index
          ~run_id:(Run_job_delivery.run_id retained)
          ~session_revision:(Int64.succ t.state.counters.revision)
          ~now:(t.services.now ())
      in
      let%bind _ =
        transition t ~delta:(Session_delta.Run_state_changed index) ~payloads:[]
      in
      Error denied
  in
  let%bind event =
    Chat_response.Background_delivery.capture frame
    |> Session.Snapshot.of_value
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  let appended =
    { before with queued_internal_events = before.queued_internal_events @ [ event ] }
  in
  let%bind () =
    if Jsonaf.exactly_equal (Runtime_builder.encode_moderator_snapshot appended) after
    then Ok ()
    else Error (error Conflict "run delivery must append its exact retained frame only")
  in
  let%bind deltas = prepare_run_job_enqueue t ~frame in
  let job_deltas, payloads =
    match
      List.find t.state.jobs ~f:(fun job ->
        Agent_protocol.Id.Job.equal job.id frame.job_id)
    with
    | None -> [], []
    | Some job ->
      (match job.delivery, Background_job_event.frame ~state:t.state ~observer job with
       | Pending, Ok current when Chat_response.Background_delivery.equal current frame ->
         let job = { job with delivery = Delivered (t.services.now ()) } in
         ( [ Session_delta.Job_changed job ]
         , [ Agent_protocol.Event.Durable.Payload.Job_state_changed job ] )
       | (Not_required | Pending | Delivered _ | Discarded _), (Ok _ | Error _) -> [], [])
  in
  let%map _ =
    transition
      t
      ~delta:
        (Session_delta.Batch (deltas @ job_deltas @ [ Moderator_changed (Some after) ]))
      ~payloads
  in
  ()
;;

let deliver_job_without_legacy
      t
      job_id
      generation
      expected
      expected_job
      moderator_snapshot
  =
  let open Result.Let_syntax in
  let%bind () = check_expected_moderator_checkpoint t expected in
  let%bind () =
    match moderator_is_borrowed t with
    | true -> Error (error Conflict "moderator callback owns the checkpoint")
    | false -> Ok ()
  in
  let%bind job = find_job t job_id in
  let%bind () =
    match expected_job with
    | None -> Ok ()
    | Some expected_job ->
      if
        Jsonaf.exactly_equal
          (Agent_protocol.Job.to_json expected_job)
          (Agent_protocol.Job.to_json job)
      then Ok ()
      else Error (error Conflict "job changed before completion delivery")
  in
  let%bind () = validate_job_generation t job generation in
  match job.status, job.delivery with
  | (Succeeded | Failed _ | Cancelled | Interrupted _), Agent_protocol.Job.Pending ->
    let%bind () =
      match job.kind, job.launch with
      | Async_tool, Some _ ->
        (match expected, moderator_snapshot with
         | Some before, Some after ->
           let observer : Agent_protocol.Invocation.observer =
             { script_id = before.script_id; source_sha256 = before.script_source_hash }
           in
           let%bind frame = Background_job_event.frame ~state:t.state ~observer job in
           let%bind event =
             Chat_response.Background_delivery.capture frame
             |> Session.Snapshot.of_value
             |> Result.map_error ~f:Agent_protocol.Error.invalid_request
           in
           let appended =
             { before with
               queued_internal_events = before.queued_internal_events @ [ event ]
             }
           in
           (match
              Jsonaf.exactly_equal
                (Runtime_builder.encode_moderator_snapshot appended)
                after
            with
            | true -> Ok ()
            | false ->
              Error
                (error
                   Conflict
                   "background delivery must append its captured terminal frame only"))
         | _ ->
           Error
             (error Conflict "background delivery requires an exact moderator checkpoint"))
      | _ -> Ok ()
    in
    let%bind run_deltas =
      match job.kind, job.launch, expected with
      | Async_tool, Some _, Some before ->
        let observer : Agent_protocol.Invocation.observer =
          { script_id = before.script_id; source_sha256 = before.script_source_hash }
        in
        let%bind frame = Background_job_event.frame ~state:t.state ~observer job in
        prepare_run_job_enqueue t ~frame
      | ( ( Model_call
          | Nested_agent
          | Scheduled_event
          | Async_tool
          | Shell_process
          | Compaction )
        , _
        , _ ) -> Ok []
    in
    let job = { job with delivery = Delivered (t.services.now ()) } in
    let%map _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             (run_deltas @ [ Job_changed job; Moderator_changed moderator_snapshot ]))
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Job_state_changed job ]
    in
    job
  | (Queued | Running | Waiting_permission _ | Waiting_completion _), _ ->
    Error (error Conflict "job is not terminal")
  | _, (Not_required | Delivered _ | Discarded _) ->
    Error (error Already_resolved "job delivery is resolved")
;;

let complete_legacy_job t manager job expected expected_job moderator_snapshot =
  let open Result.Let_syntax in
  let current_source () =
    let%bind () = require_runtime_admission t in
    let%bind () =
      if Option.exists t.moderator_manager ~f:(phys_equal manager)
      then Ok ()
      else Error (error Conflict "legacy Job Manager changed before delivery")
    in
    Moderator_source_installation.capture t.state
  in
  let%bind () =
    if Option.is_some expected_job
    then Ok ()
    else Error (error Conflict "legacy Job delivery requires its exact saved record")
  in
  let%bind before =
    Result.of_option
      expected
      ~error:(error Conflict "legacy Job delivery requires its previous checkpoint")
  in
  let%bind _, source = current_source () in
  let%bind named_run =
    match job.Agent_protocol.Job.inference_origin with
    | Some (Run id) ->
      (match Option.bind t.state.run_state ~f:(fun index -> Run_state.find index id) with
       | Some run
         when Agent_protocol.Id.Session.equal
                (Agent_protocol.Session_ref.session_id run.session)
                job.session_id
              && Agent_protocol.Run_source.equal run.source source -> Ok true
       | Some _ | None ->
         Error (error Conflict "legacy Job original Run source is unavailable or changed"))
    | Some Unbound | None -> Ok false
  in
  let%bind row = Job_custody.find t.job_agent_custody ~job in
  let%bind capture =
    if Option.is_some row || named_run
    then
      Moderator_queue.Capture.create
        ~producer:
          (Job_completion
             { id = job.id; generation = job.generation; attempt = job.attempt })
        ~origin:job.inference_origin
    else Ok Moderator_queue.Capture.unavailable
  in
  let%bind plan =
    Legacy_job_delivery.prepare t.state ~job ~before ~snapshot:moderator_snapshot ~capture
  in
  let%bind continuation =
    Job_custody.transfer_delivery t.job_agent_custody ~manager ~source ~job
  in
  let staged =
    match continuation with
    | None -> Legacy_queue_custody.empty
    | Some continuation ->
      Legacy_queue_custody.stage_delivery
        ~manager
        ~source
        ~entry:(Legacy_job_delivery.entry plan)
        ~continuation
  in
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () -> if not !acknowledged then Legacy_queue_custody.retire staged)
    ~f:(fun () ->
      let%bind inventory =
        Legacy_queue_custody.candidate
          t.legacy_queue_custody
          ~manager
          ~metadata:(Legacy_job_delivery.metadata plan)
          ~staged
      in
      let delivered = { job with delivery = Delivered (t.services.now ()) } in
      let%bind prepared =
        prepare_transition
          t
          ~delta:
            (Session_delta.Batch
               [ Run_state_changed (Legacy_job_delivery.index plan)
               ; Job_changed delivered
               ; Moderator_changed (Some (Legacy_job_delivery.checkpoint plan))
               ])
          ~payloads:[ Agent_protocol.Event.Durable.Payload.Job_state_changed delivered ]
      in
      let%bind _, final_source = current_source () in
      let%bind () =
        if Agent_protocol.Run_source.equal source final_source
        then Ok ()
        else Error (error Conflict "legacy Job source changed before delivery ACK")
      in
      (* Completed facts remain truthful after original authority expiry. The
         typed expired continuation cannot authorize the later queued callback. *)
      Eio.Cancel.protect (fun () ->
        let%bind () = persist_transition t prepared in
        install_committed_transition t prepared;
        t.legacy_queue_custody
        <- Legacy_queue_custody.install t.legacy_queue_custody ~incoming:inventory;
        acknowledged := true;
        publish_committed_transition t prepared;
        Ok delivered))
;;

let deliver_job t job_id generation expected expected_job moderator_snapshot manager =
  let open Result.Let_syntax in
  let%bind () =
    match manager with
    | Some manager when not (Option.exists t.moderator_manager ~f:(phys_equal manager)) ->
      Error (error Conflict "Job delivery requires its actual installed Manager")
    | Some _ | None -> Ok ()
  in
  match manager with
  | Some manager
    when Option.is_none (Chat_response.Moderator_manager.invocation_observer manager) ->
    let%bind () =
      if moderator_is_borrowed t
      then Error (error Conflict "moderator callback owns the checkpoint")
      else Ok ()
    in
    let%bind job = find_job t job_id in
    let%bind () = validate_job_generation t job generation in
    let%bind () =
      match expected_job with
      | Some expected_job
        when Jsonaf.exactly_equal
               (Agent_protocol.Job.to_json expected_job)
               (Agent_protocol.Job.to_json job) -> Ok ()
      | Some _ | None ->
        Error (error Conflict "Job changed before legacy completion delivery")
    in
    complete_legacy_job t manager job expected expected_job moderator_snapshot
  | Some _ | None ->
    let%bind metadata = Moderator_checkpoint.queue t.state.moderator in
    let%bind () =
      if Option.is_some metadata
      then
        Error (error Conflict "captured legacy Job delivery requires its actual Manager")
      else (
        match t.moderator_manager with
        | Some installed
          when Option.is_none
                 (Chat_response.Moderator_manager.invocation_observer installed) ->
          let%bind job = find_job t job_id in
          let%bind row = Job_custody.find t.job_agent_custody ~job in
          if
            Option.is_some row
            || Option.is_some job.inference_origin
            || Option.exists job.launch ~f:(fun launch ->
              Option.is_some launch.moderator_source)
          then
            Error
              (error Conflict "captured legacy Job delivery requires its actual Manager")
          else Ok ()
        | None ->
          let%bind job = find_job t job_id in
          let%bind row = Job_custody.find t.job_agent_custody ~job in
          if Option.is_some row || Option.is_some job.inference_origin
          then Error (error Conflict "captured Job delivery requires its actual Manager")
          else Ok ()
        | Some _ -> Ok ())
    in
    deliver_job_without_legacy
      t
      job_id
      generation
      expected
      expected_job
      moderator_snapshot
;;

let cancel_job_internal t job_id =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind job =
    match job.status with
    | Waiting_completion _ -> refresh_background_job t job_id job.generation job.attempt
    | _ -> Ok job
  in
  let owned = Run_job_completion.owns t.state job in
  match job.status, job.delivery with
  | _, Agent_protocol.Job.Discarded _ -> Ok job
  | (Running | Waiting_permission _ | Waiting_completion _), _
    when owned && Agent_protocol.Job.equal_kind job.kind Async_tool ->
    let waiting =
      match job.status with
      | Waiting_completion _ -> true
      | Queued
      | Running
      | Waiting_permission _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ -> false
    in
    complete_background_job
      ~waiting
      ~control:Cancelled_control
      t
      job.id
      job.generation
      job.attempt
      (Cancelled "job cancelled")
  | (Queued | Running | Waiting_permission _ | Waiting_completion _ | Interrupted _), _ ->
    let%bind cancelled, subscriptions = cancelled_work_dependencies t job in
    let%bind run_deltas =
      match job.status with
      | Queued when owned && Agent_protocol.Job.equal_kind job.kind Async_tool ->
        Run_job_completion.cancel_queued t.state ~job ~now:(t.services.now ())
      | Queued
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ -> Ok []
    in
    let job =
      { job with
        status = Cancelled
      ; result = background_terminal_result job (Cancelled "job cancelled")
      ; completed_at = Some (t.services.now ())
      ; delivery =
          (match job.delivery with
           | Not_required -> Not_required
           | _ -> Pending)
      }
    in
    let%bind job_custody = prepare_terminal_job_custody t (job :: cancelled) in
    let%map _ =
      update_jobs ~job_custody ~subscriptions ~run_deltas t (job :: cancelled)
    in
    job
  | (Succeeded | Failed _ | Cancelled), _ -> Ok job
;;

let interrupt_job ?run_preparation t job_id generation attempt reason =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  match job.status with
  | Agent_protocol.Job.Running
    when Agent_protocol.Job.equal_kind job.kind Async_tool
         && Run_job_completion.owns t.state job ->
    complete_background_job
      ~control:Interrupted_control
      t
      job.id
      job.generation
      job.attempt
      (Failed
         { code = "background.interrupted"
         ; message = reason
         ; retryable = false
         ; details = `Null
         })
  | Agent_protocol.Job.Running ->
    let job =
      { job with
        status = Interrupted reason
      ; result =
          background_terminal_result
            job
            (Failed
               { code = "background.interrupted"
               ; message = reason
               ; retryable = false
               ; details = `Null
               })
      ; completed_at = Some (t.services.now ())
      ; delivery =
          (match job.delivery with
           | Not_required -> Not_required
           | _ -> Pending)
      }
    in
    let%bind job_custody = prepare_terminal_job_custody t [ job ] in
    let%map _ = update_job ?run_preparation ~job_custody t job in
    job
  | Queued
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> Ok job
;;

let change_schedule t attachment_id event schedule =
  let payload =
    match event with
    | `Created -> Agent_protocol.Event.Durable.Payload.Schedule_created schedule
    | `Cancelled -> Schedule_cancelled schedule
  in
  with_writer t attachment_id (fun () ->
    transition t ~delta:(Session_delta.Schedule_changed schedule) ~payloads:[ payload ])
;;

let add_schedule ?run_preparation t schedule =
  if
    Agent_protocol.Id.Session.compare
      schedule.Agent_protocol.Schedule.session_id
      t.state.identity.session_id
    <> 0
  then Error (error Invalid_request "schedule belongs to another session")
  else if schedule.generation <> t.state.identity.generation
  then Error (error Conflict "schedule belongs to a stale session generation")
  else if
    List.exists t.state.schedules ~f:(fun existing ->
      Agent_protocol.Id.Schedule.compare existing.id schedule.id = 0)
  then Error (error Conflict "schedule ID is already present")
  else
    let open Result.Let_syntax in
    let%map _ =
      transition
        ?run_preparation
        t
        ~delta:(Session_delta.Schedule_changed schedule)
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Schedule_created schedule ]
    in
    schedule
;;

let find_schedule t schedule_id =
  List.find t.state.schedules ~f:(fun schedule ->
    Agent_protocol.Id.Schedule.compare schedule.id schedule_id = 0)
  |> Result.of_option ~error:(error Invalid_request "schedule was not found")
;;

let validate_schedule_generation t schedule generation =
  if
    schedule.Agent_protocol.Schedule.generation <> generation
    || t.state.identity.generation <> generation
  then Error (error Conflict "schedule belongs to a stale session generation")
  else Ok ()
;;

let update_schedule ?run_preparation t schedule =
  transition
    ?run_preparation
    t
    ~delta:(Session_delta.Schedule_changed schedule)
    ~payloads:[ Agent_protocol.Event.Durable.Payload.Schedule_state_changed schedule ]
;;

let schedule_is_due t (schedule : Agent_protocol.Schedule.t) =
  match schedule.ownership with
  | None ->
    Ok (Agent_protocol.Timestamp.compare schedule.next_due_at (t.services.now ()) <= 0)
  | Some _ ->
    Extension_clock.is_due
      t.extension_clock
      (Schedule schedule.id)
      ~now:(t.services.monotonic_now ())
;;

let due_schedules t =
  if not (runtime_ready t)
  then Ok (t.state.lifecycle.observed, [])
  else
    let open Result.Let_syntax in
    let%map schedules =
      List.filter_map t.state.schedules ~f:(fun schedule ->
        match schedule.Agent_protocol.Schedule.status with
        | Scheduled when Int.equal schedule.generation t.state.identity.generation ->
          Some
            (schedule_is_due t schedule
             |> Result.map ~f:(fun due -> Option.some_if due schedule))
        | _ -> None)
      |> Result.all
      |> Result.map ~f:List.filter_opt
    in
    t.state.lifecycle.observed, schedules
;;

let claim_schedule t schedule_id generation =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () = validate_schedule_generation t schedule generation in
  let%bind () =
    if Session_state.execution_migration_required t.state
    then
      Error
        (error
           Migration_required
           "schedule execution requires explicit runtime migration")
    else Ok ()
  in
  match schedule.status with
  | Agent_protocol.Schedule.Scheduled ->
    let%bind due = schedule_is_due t schedule in
    (match due with
     | false -> Ok None
     | true ->
       let schedule = { schedule with status = Delivering } in
       let%map _ = update_schedule t schedule in
       Some schedule)
  | Delivering | Delivered | Cancelled | Failed _ -> Ok None
;;

let retry_schedule t schedule_id generation =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Delivering ->
    let schedule = { schedule with status = Scheduled } in
    let%map _ = update_schedule t schedule in
    schedule
  | Scheduled -> Ok schedule
  | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let ingress_enqueue_available t =
  match
    moderator_is_borrowed t || t.idle_moderator_borrowed, t.state.halted, t.state.failure
  with
  | true, _, _ -> Error (error Conflict "moderator callback owns the ingress checkpoint")
  | _, true, _ | _, _, Some _ ->
    Error (error Invalid_state "moderator cannot accept external data")
  | false, false, None -> Ok ()
;;

let ingress_new_admission t candidate =
  let open Result.Let_syntax in
  let%bind subscription =
    lookup_subscription t candidate.External_ingress.context.subscription_id
    |> Result.of_option
         ~error:(error Invalid_state "ingress subscription is not retained")
  in
  let%bind due = subscription_is_due t subscription in
  let%bind () =
    match due with
    | true ->
      Error (error Invalid_state "ingress subscription has reached its elapsed deadline")
    | false -> Ok ()
  in
  Staged_ingress.check_capacity
    ~limits:t.services.ingress_limits
    ~generation:t.state.identity.generation
    ~now:(t.services.now ())
    ~subscriptions:
      (t.state.subscriptions @ Staged_subscriptions.values t.staged_subscriptions)
    ~values:
      ((candidate :: t.state.ingress_registrations)
       @ Staged_ingress.values t.staged_ingress)
;;

let prepare_ingress_submission_internal
      t
      source
      producer
      registration_id
      namespace
      key
      payload
  =
  let open Result.Let_syntax in
  let%bind () = validate_installed_observer t (Some source) in
  let%bind decision =
    Ingress_submission.prepare
      ~state:t.state
      ~source
      ~producer
      ~registration_id
      ~namespace
      ~key
      ~payload
      ~now:(t.services.now ())
      ~create_event_id:Agent_protocol.Id.Ingress_event.create
  in
  match decision with
  | Duplicate _ -> Ok decision
  | Enqueue proposal ->
    let%bind () = ingress_enqueue_available t in
    let%map () = ingress_new_admission t proposal.candidate in
    decision
;;

let commit_ingress_submission_internal t proposal before snapshot =
  let open Result.Let_syntax in
  let%bind () = ingress_enqueue_available t in
  let%bind () = validate_installed_observer t (Some proposal.Ingress_submission.source) in
  let%bind () = check_expected_moderator_checkpoint t (Some before) in
  let%bind next, receipt =
    Ingress_submission.revalidate ~state:t.state ~now:(t.services.now ()) proposal
  in
  let%bind () = ingress_new_admission t next in
  let%bind frame = External_ingress.delivery_frame next receipt in
  let%bind event =
    Chat_response.Ingress_delivery.capture frame
    |> Session.Snapshot.of_value
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  let expected =
    { before with queued_internal_events = before.queued_internal_events @ [ event ] }
  in
  let%bind () =
    match
      String.equal before.script_id proposal.source.script_id
      && String.equal before.script_source_hash proposal.source.source_sha256
      && Jsonaf.exactly_equal
           (Runtime_builder.encode_moderator_snapshot expected)
           (Runtime_builder.encode_moderator_snapshot snapshot)
    with
    | true -> Ok ()
    | false ->
      Error (error Conflict "ingress delivery must append its captured data frame only")
  in
  let%map _ =
    transition
      t
      ~payloads:[]
      ~delta:
        (Session_delta.Batch
           [ Ingress_changed next
           ; Moderator_changed (Some (Runtime_builder.encode_moderator_snapshot snapshot))
           ])
  in
  receipt
;;

let complete_legacy_schedule
      t
      manager
      expected_schedule
      (before : Session.Moderator_state.Identity_snapshot.t option)
      moderator_snapshot
      schedule
  =
  let open Result.Let_syntax in
  let current_source () =
    let%bind () = require_runtime_admission t in
    let%bind () =
      if Option.exists t.moderator_manager ~f:(phys_equal manager)
      then Ok ()
      else Error (error Conflict "legacy timer Manager changed before delivery")
    in
    Moderator_source_installation.capture t.state
  in
  let%bind () =
    if Option.is_some expected_schedule
    then Ok ()
    else Error (error Conflict "legacy timer delivery requires its exact saved record")
  in
  let%bind before =
    Result.of_option
      before
      ~error:(error Conflict "legacy timer delivery requires its previous checkpoint")
  in
  (* The Manager's known snapshot has no persistence metadata. Compare against
     the original installed checkpoint, while validating the complete retained
     queue child independently. Never compare a rewritten candidate to itself. *)
  let%bind () = Moderator_checkpoint.validate_encoded_capacity t.state.moderator in
  let%bind installed = Moderator_checkpoint.decode t.state.moderator in
  let%bind () =
    if
      Option.exists installed ~f:(fun installed ->
        Sexp.equal
          (Session.Moderator_state.Identity_snapshot.sexp_of_t installed)
          (Session.Moderator_state.Identity_snapshot.sexp_of_t before))
    then Ok ()
    else Error (error Conflict "legacy timer checkpoint changed before delivery")
  in
  let%bind snapshot = Moderator_checkpoint.decode moderator_snapshot in
  let%bind snapshot =
    Result.of_option
      snapshot
      ~error:(error Conflict "legacy timer delivery requires its prospective checkpoint")
  in
  let%bind index, source = current_source () in
  let%bind () =
    match schedule.Agent_protocol.Schedule.inference_provenance with
    | None -> Ok ()
    | Some provenance ->
      if
        Agent_protocol.Run_source.equal
          source
          (Agent_protocol.Schedule_provenance.source provenance)
      then Ok ()
      else Error (error Conflict "legacy timer original source is no longer installed")
  in
  let%bind metadata = Moderator_checkpoint.queue t.state.moderator in
  let%bind metadata =
    match metadata with
    | None -> Moderator_queue.bootstrap ~source ~queue:before.queued_internal_events
    | Some metadata ->
      if Agent_protocol.Run_source.equal source (Moderator_queue.source metadata)
      then Ok metadata
      else Error (error Conflict "legacy timer queue lifetime changed")
  in
  let%bind payload =
    Session.Snapshot.of_jsonaf schedule.payload
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  let expected_snapshot =
    { before with queued_internal_events = before.queued_internal_events @ [ payload ] }
  in
  let%bind () =
    if
      Sexp.equal
        (Session.Moderator_state.Identity_snapshot.sexp_of_t expected_snapshot)
        (Session.Moderator_state.Identity_snapshot.sexp_of_t snapshot)
    then Ok ()
    else Error (error Conflict "legacy timer delivery must append its exact raw payload")
  in
  let%bind capture =
    match schedule.inference_provenance with
    | None -> Ok Moderator_queue.Capture.unavailable
    | Some provenance ->
      Moderator_queue.Capture.create
        ~producer:
          (Schedule_delivery
             { id = schedule.id
             ; generation = schedule.generation
             ; delivery_count = schedule.delivery_count
             })
        ~origin:(Agent_protocol.Schedule_provenance.origin provenance)
  in
  let%bind plan =
    Legacy_queue_checkpoint.append_delivery ~metadata ~before ~snapshot ~capture
  in
  let%bind encoded =
    Moderator_checkpoint.update_with_queue
      t.state.moderator
      ~snapshot
      ~queue:(Legacy_queue_checkpoint.metadata plan)
  in
  (* Stage while the Delivering timer's actual loan still exists. Installing
     Delivered will retire that phase, so no later lookup may reconstruct it. *)
  let%bind continuation =
    Legacy_schedule_custody.transfer_delivery
      t.legacy_schedule_custody
      ~manager
      ~source
      ~schedule
  in
  let staged =
    match continuation with
    | None -> Legacy_queue_custody.empty
    | Some continuation ->
      Legacy_queue_custody.stage_delivery
        ~manager
        ~source
        ~entry:(List.hd_exn (Legacy_queue_checkpoint.appended plan))
        ~continuation
  in
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () -> if not !acknowledged then Legacy_queue_custody.retire staged)
    ~f:(fun () ->
      let%bind inventory =
        Legacy_queue_custody.candidate
          t.legacy_queue_custody
          ~manager
          ~metadata:(Legacy_queue_checkpoint.metadata plan)
          ~staged
      in
      let%bind prepared =
        prepare_transition
          t
          ~delta:
            (Session_delta.Batch
               [ Run_state_changed index
               ; Schedule_changed schedule
               ; Moderator_changed (Some encoded)
               ])
          ~payloads:
            [ Agent_protocol.Event.Durable.Payload.Schedule_state_changed schedule ]
      in
      let%bind _, final_source = current_source () in
      let%bind () =
        if Agent_protocol.Run_source.equal source final_source
        then Ok ()
        else Error (error Conflict "legacy timer source changed before delivery ACK")
      in
      (* This is a truthful completed-fact write, not a new effect admission.
         Known expiry is carried into the queue and must not erase Delivered. *)
      Eio.Cancel.protect (fun () ->
        let%bind () = persist_transition t prepared in
        install_committed_transition t prepared;
        t.legacy_queue_custody
        <- Legacy_queue_custody.install t.legacy_queue_custody ~incoming:inventory;
        acknowledged := true;
        publish_committed_transition t prepared;
        Ok schedule))
;;

let complete_schedule
      t
      schedule_id
      generation
      expected
      expected_schedule
      moderator_snapshot
      moderator_manager
  =
  let open Result.Let_syntax in
  let%bind () =
    match moderator_is_borrowed t with
    | true -> Error (error Conflict "moderator callback owns the checkpoint")
    | false -> Ok ()
  in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () =
    match schedule.ownership, moderator_manager with
    | None, Some manager
      when Option.is_none (Chat_response.Moderator_manager.invocation_observer manager) ->
      (* Only the actual legacy helper below performs known comparison together
         with resource/source/queue validation. Generic/v1 remains byte-strict. *)
      Ok ()
    | (Some _ | None), _ -> check_expected_moderator_checkpoint t expected
  in
  let%bind () =
    match expected_schedule with
    | None -> Ok ()
    | Some expected_schedule ->
      if
        Jsonaf.exactly_equal
          (Agent_protocol.Schedule.to_json expected_schedule)
          (Agent_protocol.Schedule.to_json schedule)
      then Ok ()
      else Error (error Conflict "schedule changed before event delivery")
  in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Delivering ->
    let%bind () =
      match schedule.ownership, expected with
      | None, _ -> Ok ()
      | Some ownership, Some before ->
        let%bind event =
          Chat_response.Schedule_delivery.capture schedule
          |> Result.bind ~f:Session.Snapshot.of_value
          |> Result.map_error ~f:Agent_protocol.Error.invalid_request
        in
        let after =
          { before with
            queued_internal_events = before.queued_internal_events @ [ event ]
          }
        in
        (match
           String.equal before.script_id ownership.source.script_id
           && String.equal before.script_source_hash ownership.source.source_sha256
           && Option.equal
                Jsonaf.exactly_equal
                moderator_snapshot
                (Some (Runtime_builder.encode_moderator_snapshot after))
         with
         | true -> Ok ()
         | false ->
           Error
             (error Conflict "owned timer delivery must append its captured frame only"))
      | Some _, None ->
        Error (error Conflict "owned timer delivery requires its installed checkpoint")
    in
    if schedule.delivery_count = Int.max_value
    then Error (error Invalid_state "schedule delivery count overflow")
    else (
      let schedule =
        { schedule with
          status = Delivered
        ; delivery_count = schedule.delivery_count + 1
        ; last_delivery_at =
            Some
              (match schedule.ownership with
               | None -> t.services.now ()
               | Some _ ->
                 let now = t.services.now () in
                 (match Agent_protocol.Timestamp.compare now schedule.created_at < 0 with
                  | true -> schedule.created_at
                  | false -> now))
        }
      in
      let%bind metadata = Moderator_checkpoint.queue t.state.moderator in
      match schedule.ownership, moderator_manager with
      | None, Some manager
        when Option.is_none (Chat_response.Moderator_manager.invocation_observer manager)
        ->
        complete_legacy_schedule
          t
          manager
          expected_schedule
          expected
          moderator_snapshot
          schedule
      | None, _
        when Option.is_some schedule.inference_provenance || Option.is_some metadata ->
        Error
          (error Conflict "captured legacy timer delivery requires its actual Manager")
      | (Some _ | None), _ ->
        let%map _ =
          transition
            t
            ~delta:
              (Session_delta.Batch
                 [ Schedule_changed schedule; Moderator_changed moderator_snapshot ])
            ~payloads:
              [ Agent_protocol.Event.Durable.Payload.Schedule_state_changed schedule ]
        in
        schedule)
  | Scheduled -> Error (error Conflict "schedule delivery was not claimed")
  | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let fail_schedule t schedule_id generation failure =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Scheduled | Delivering ->
    let schedule = { schedule with status = Failed failure } in
    let%map _ = update_schedule t schedule in
    schedule
  | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let skip_schedule t schedule_id generation =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Scheduled ->
    let schedule = { schedule with status = Delivered } in
    let%map _ = update_schedule t schedule in
    schedule
  | Delivering -> Error (error Conflict "schedule delivery is already claimed")
  | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let cancel_schedule_internal ?run_preparation t schedule_id =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  match schedule.status with
  | Agent_protocol.Schedule.Scheduled ->
    let schedule = { schedule with status = Cancelled } in
    let%map _ =
      transition
        ?run_preparation
        t
        ~delta:(Session_delta.Schedule_changed schedule)
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Schedule_cancelled schedule ]
    in
    schedule
  | Delivering | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let callback_schedule_admission t (scope : Callback_scope.t) =
  let open Result.Let_syntax in
  let%bind () = require_runtime_admission t in
  let%bind () = check_callback_current t scope in
  match scope.custody with
  | Admitted admission -> Ok admission
  | Unavailable ->
    Error (error Permission_denied "legacy Schedule continuation custody is unavailable")
;;

let callback_schedule_after_ms t (scope : Callback_scope.t) delay_ms payload =
  let open Result.Let_syntax in
  let%bind parent = callback_schedule_admission t scope in
  let created_at = t.services.now () in
  (* Preserve immediate legacy wall-clock semantics. Timestamp.add_ms performs
     the existing nonnegative and representable-endpoint checks. *)
  let%bind next_due_at = Agent_protocol.Timestamp.add_ms created_at delay_ms in
  let%bind provenance =
    Agent_protocol.Schedule_provenance.create ~source:scope.source ~origin:scope.origin
  in
  let schedule : Agent_protocol.Schedule.t =
    { id = Agent_protocol.Id.Schedule.create ()
    ; session_id = t.state.identity.session_id
    ; generation = t.state.identity.generation
    ; payload
    ; created_at
    ; next_due_at
    ; misfire = Deliver_once_immediately
    ; status = Scheduled
    ; delivery_count = 0
    ; last_delivery_at = None
    ; inference_provenance = Some provenance
    ; delivery_cancellation = None
    ; ownership = None
    }
  in
  let%bind staged =
    Legacy_schedule_custody.stage
      ~manager:scope.manager
      ~source:scope.source
      ~schedule
      ~parent
  in
  let acknowledged = ref false in
  Exn.protect
    ~finally:(fun () -> if not !acknowledged then Legacy_schedule_custody.retire staged)
    ~f:(fun () ->
      let%bind inventory =
        Legacy_schedule_custody.candidate t.legacy_schedule_custody ~staged
      in
      let%bind prepared =
        prepare_transition
          ?run_preparation:(callback_run_preparation scope)
          t
          ~delta:(Session_delta.Schedule_changed schedule)
          ~payloads:[ Agent_protocol.Event.Durable.Payload.Schedule_created schedule ]
      in
      let%bind initialization_acknowledgement =
        match scope.constructor_scope with
        | None -> Ok None
        | Some constructor ->
          let%map resource =
            Initialization_scope.Resource.select
              constructor.resource
              prepared.state
              ~manager:scope.manager
              ~source:scope.source
              ~ordinals:[]
              ~schedules:[ schedule.id ]
          in
          Some
            (Initialization_scope.Acknowledgement.prepare
               ~resource
               constructor
               prepared.state)
      in
      let%bind _ = callback_schedule_admission t scope in
      Eio.Cancel.protect (fun () ->
        let%bind () =
          Option.value_map
            initialization_acknowledgement
            ~default:(Ok ())
            ~f:Initialization_scope.Acknowledgement.check_basis
        in
        let%bind () = persist_transition t prepared in
        install_committed_transition
          ?run_preparation:(callback_run_preparation scope)
          ~legacy_schedule_custody:inventory
          ?initialization_acknowledgement
          t
          prepared;
        acknowledged := true;
        publish_committed_transition t prepared;
        Ok schedule))
;;

let callback_schedule_cancel t scope schedule_id =
  let open Result.Let_syntax in
  let%bind _ = callback_schedule_admission t scope in
  let%bind schedule = find_schedule t schedule_id in
  match schedule.status with
  | Scheduled ->
    (* The genuine caller is checked, not the target timer's provenance. Keep
       legacy cancellation of any retained Scheduled ID, including old timers. *)
    let schedule = { schedule with status = Agent_protocol.Schedule.Cancelled } in
    let%bind prepared =
      prepare_transition
        ?run_preparation:(callback_run_preparation scope)
        t
        ~delta:(Session_delta.Schedule_changed schedule)
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Schedule_cancelled schedule ]
    in
    let%bind _ = callback_schedule_admission t scope in
    Eio.Cancel.protect (fun () ->
      let%bind () = persist_transition t prepared in
      (* The ordinary installation retention pass closes a matching timer loan
         before publication. Historical targets need not have a loan. *)
      install_committed_transition
        ?run_preparation:(callback_run_preparation scope)
        t
        prepared;
      publish_committed_transition t prepared;
      Ok schedule)
  | Delivering | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let claim_idle_moderator t =
  if not (idle_moderator_eligible t)
  then Ok None
  else
    History_codec.all_of_protocol t.state.conversation.canonical_history
    |> Result.map ~f:(fun history ->
      t.idle_moderator_borrowed <- true;
      Some history)
;;

let notification_payload message =
  Agent_protocol.Event.Durable.Payload.Moderator_notification
    (`Object [ "message", `String message ])
;;

let drain_payloads (drain : Runtime_builder.moderator_drain) =
  List.map drain.Runtime_builder.notifications ~f:notification_payload
;;

let start_idle_turn_unchecked
      ?continuation_custody
      ?follow_up_authority
      ?(extra_deltas = [])
      ?(extra_payloads = [])
      ?(notification_wakes = [])
      t
      (drain : Runtime_builder.moderator_drain)
      ~reason
      ~adopt_deferred
  =
  let open Result.Let_syntax in
  let%bind () = require_runtime_admission t in
  let%bind () = reconcile_foreground_invocations t in
  let operation = create_turn_operation t reason in
  let%bind admission =
    let retention = Pending_disposition.Retention.default in
    Turn_admission.create
      ~pending_retention:(Some retention)
      ~runtime_admission_open:(runtime_admission_open t)
      t.state
      ~operation
      ~notification_wakes
      ~adopt_deferred
  in
  let%bind agent_custody = admission_agent_custody t admission in
  let%bind agent_custody =
    let has_input =
      List.exists (Turn_admission.payloads admission) ~f:(function
        | Agent_protocol.Event.Durable.Payload.History_appended entries ->
          not (List.is_empty entries)
        | _ -> false)
    in
    match agent_custody, continuation_custody with
    | None, Some custody when (not has_input) && List.is_empty notification_wakes ->
      Result.map (transfer_agent_custody custody) ~f:Option.some
    | custody, (None | Some _) -> Ok custody
  in
  let leading_deltas, turn_deltas =
    match Turn_admission.deltas admission with
    | (Pending_inputs_changed _ as delta) :: rest -> [ delta ], rest
    | deltas -> [], deltas
  in
  let leading_payloads, turn_payloads =
    match Turn_admission.payloads admission with
    | History_appended entries :: rest ->
      [ Agent_protocol.Event.Durable.Payload.History_appended entries ], rest
    | payloads -> [], payloads
  in
  let events =
    List.filter_map extra_deltas ~f:(function
      | Session_delta.Moderator_execution_changed event -> Some event
      | _ -> None)
  in
  let%bind run_deltas = run_turn_deltas t operation events notification_wakes in
  let deltas =
    run_deltas
    @ leading_deltas
    @ extra_deltas
    @ [ Session_delta.Moderator_changed drain.Runtime_builder.moderator_snapshot ]
    @ turn_deltas
  in
  let payloads =
    drain_payloads drain @ leading_payloads @ extra_payloads @ turn_payloads
  in
  let installed = ref false in
  Exn.protect
    ~f:(fun () ->
      let%map _ =
        transition
          t
          ~check_before_commit:(fun () ->
            let%bind () = check_turn_custody t agent_custody in
            Option.value_map
              follow_up_authority
              ~default:(Ok ())
              ~f:(check_follow_up_authority t))
          ~delta:(Session_delta.Batch deltas)
          ~payloads
      in
      if Option.is_none t.operation_agent_custody
      then (
        install_operation_agent_custody t operation agent_custody;
        installed := true);
      prune_pending_agent_custody t;
      launch_worker t operation)
    ~finally:(fun () ->
      if not !installed then Option.iter agent_custody ~f:retire_agent_custody)
;;

let commit_history_transition t ~delta ~payloads ~admission =
  let open Result.Let_syntax in
  let%bind agent_custody =
    match admission with
    | None -> Ok None
    | Some admission -> admission_agent_custody t admission
  in
  let%bind prepared = prepare_transition t ~delta ~payloads in
  Eio.Cancel.protect (fun () ->
    let%map () =
      persist_transition
        ~check_before_commit:(fun () -> check_turn_custody t agent_custody)
        t
        prepared
    in
    install_committed_transition t prepared;
    publish_committed_transition t prepared;
    Option.iter admission ~f:(fun admission ->
      let operation = Turn_admission.operation admission in
      if Option.is_none t.operation_agent_custody
      then install_operation_agent_custody t operation agent_custody;
      prune_pending_agent_custody t;
      launch_worker t operation);
    Session_state.summary t.state)
;;

let history_command_precondition t session_id attachment_id generation revision =
  let open Result.Let_syntax in
  let%bind () =
    if Agent_protocol.Id.Session.equal session_id t.state.identity.session_id
    then Ok ()
    else Error (error Invalid_request "history command targets a different session")
  in
  let%bind _ = write_attachment t attachment_id in
  let%bind () =
    if Int.equal generation t.state.identity.generation
    then Ok ()
    else Error (error Conflict "session generation does not match")
  in
  let%bind () = history_edit_precondition t revision in
  if
    Option.is_some t.moderator_borrow
    || Option.is_some t.queued_event_borrow
    || Option.is_some t.initialization_scope
    || Option.is_some t.runtime_retirement
    || List.exists t.invocation_executions ~f:(fun execution ->
      match execution.owner with
      | Foreground _ | Invocation_moderator _ | Event_moderator _ -> true
      | Background_job _ -> false)
  then
    Error
      (error
         Conflict
         "history cannot change while a foreground or runtime owner is active")
  else Ok ()
;;

let control_pending_internal
      t
      principal
      project
      (request : Agent_protocol.Pending_control.Cancel_request.t)
      replacement
  =
  with_writer t request.attachment_id (fun () ->
    let open Result.Let_syntax in
    let%bind () =
      if
        (not
           (Agent_protocol.Id.Session.equal
              request.session_id
              t.state.identity.session_id))
        || not (Int.equal request.expected_generation t.state.identity.generation)
      then Error (error Conflict "pending control session or generation differs")
      else Ok ()
    in
    let%bind () =
      Pending_inspection.authorize_control
        t.state
        ~history_id:request.history_id
        ~principal
    in
    let%bind outcome =
      Pending_inspection.lookup t.state ~history_id:request.history_id ~project
    in
    let%bind () =
      match outcome with
      | Pending _ ->
        if
          Agent_protocol.Pending_input.Revision.equal
            request.expected_pending_revision
            t.state.conversation.pending_revision
        then Ok ()
        else Error (error Conflict "pending queue revision differs")
      | Adopted _ | Cancelled _ | Retired _ | Unavailable _ -> Ok ()
    in
    let%bind () =
      match outcome with
      | Adopted _ | Cancelled _ | Retired _ | Unavailable _ -> Ok ()
      | Pending _ ->
        let change =
          match replacement with
          | None ->
            Pending_plan.Change.Cancel
              { history_id = request.history_id
              ; expected_content_revision = request.expected_content_revision
              }
          | Some text ->
            Replace_text
              { history_id = request.history_id
              ; expected_content_revision = request.expected_content_revision
              ; text
              }
        in
        let retention = Pending_disposition.Retention.default in
        let%bind _ =
          Pending_plan.prepare
            t.state
            ~expected_pending_revision:request.expected_pending_revision
            ~change
            ~retention
            ~limits:Session_delta.native_limits
        in
        let%bind archive =
          t.persistence.archive_reference
            ~previous:t.state
            ~kind:Pending_input
            (Agent_protocol.Id.Operation.create ())
        in
        let%bind prepared = prepare_pending t ~archive:(Some archive) change in
        let%bind candidate =
          Pending_plan.apply (Pending_transition.plan prepared) t.state
        in
        let%map _ =
          transition
            t
            ~delta:(Pending_transition.delta prepared)
            ~payloads:
              [ Agent_protocol.Event.Durable.Payload.Session_updated
                  (Session_state.summary candidate)
              ]
        in
        ()
    in
    let%map outcome =
      Pending_inspection.lookup t.state ~history_id:request.history_id ~project
    in
    Agent_protocol.Pending_control.Result.
      { pending_revision = t.state.conversation.pending_revision
      ; outcome
      ; mutation =
          { revision = t.state.counters.revision
          ; latest_event_sequence = t.state.counters.event_sequence
          }
      })
;;

let delete_history_internal t attachment_id revision history_id =
  let open Result.Let_syntax in
  let%bind () =
    history_command_precondition
      t
      t.state.identity.session_id
      attachment_id
      t.state.identity.generation
      revision
  in
  let%bind plan = History_deletion.prepare t.state ~history_id in
  let%bind archive =
    t.persistence.archive_reference
      ~previous:t.state
      ~kind:Delete
      (Agent_protocol.Id.Operation.create ())
  in
  commit_history_transition
    t
    ~delta:(Session_delta.History_deleted (history_id, archive))
    ~payloads:
      [ Agent_protocol.Event.Durable.Payload.History_replaced
          (Session_state.history_window (History_deletion.canonical_history plan))
      ]
    ~admission:None
;;

let history_continuation_availability t =
  match t.state.lifecycle.observed, t.operation_worker with
  | Stopped, _ -> Some Agent_protocol.History_edit.Continuation.Stopped
  | Idle, Some _ -> None
  | _ -> Some Agent_protocol.History_edit.Continuation.Runtime_unavailable
;;

let edit_history_internal t (request : Agent_protocol.History_edit.Edit_request.t) =
  let open Result.Let_syntax in
  let%bind () =
    history_command_precondition
      t
      request.session_id
      request.attachment_id
      request.expected_generation
      request.expected_revision
  in
  let%bind plan = History_edit.prepare t.state ~edit:request.edit in
  let continue =
    Agent_protocol.History_edit.Mode.equal
      (Agent_protocol.History_edit.mode request.edit)
      Edit_and_continue
  in
  let%bind () =
    if continue && not (List.is_empty t.state.conversation.deferred_user_entries)
    then
      Error
        (error
           Pending_input_conflict
           "edit-and-continue requires an empty pending-input queue")
    else Ok ()
  in
  let%bind archive =
    t.persistence.archive_reference
      ~previous:t.state
      ~kind:Edit
      (Agent_protocol.Id.Operation.create ())
  in
  let continuation =
    if not continue
    then History_edit_transition.Save_only
    else (
      match history_continuation_availability t with
      | Some reason -> History_edit_transition.Unavailable reason
      | None -> History_edit_transition.Start (create_turn_operation t Administrative))
  in
  let%bind prepared =
    History_edit_transition.create t.state ~plan ~edit:request.edit ~archive ~continuation
  in
  let%map session =
    commit_history_transition
      t
      ~delta:(History_edit_transition.delta prepared)
      ~payloads:(History_edit_transition.payloads prepared)
      ~admission:(History_edit_transition.admission prepared)
  in
  let edited = History_edit_transition.edited_entry prepared in
  ({ session
   ; history_id = edited.id
   ; content_revision = edited.content_revision
   ; archived_revision = History_edit_transition.archived_revision prepared
   ; continuation = History_edit_transition.continuation prepared
   }
   : History_edit_result.t)
;;

let continue_history_internal t (request : Agent_protocol.History_edit.Continue_request.t)
  =
  let open Result.Let_syntax in
  let%bind () =
    history_command_precondition
      t
      request.session_id
      request.attachment_id
      request.expected_generation
      request.expected_revision
  in
  let%bind () =
    if List.is_empty t.state.conversation.deferred_user_entries
    then Ok ()
    else
      Error
        (error
           Pending_input_conflict
           "history continuation requires an empty pending-input queue")
  in
  match history_continuation_availability t with
  | Some reason ->
    Ok
      ({ session = Session_state.summary t.state
       ; continuation = Agent_protocol.History_edit.Continuation.Not_started reason
       }
       : History_continuation_result.t)
  | None ->
    let%bind recovery = History_continuation.prepare t.state ~retiring_history:false in
    let operation = create_turn_operation t Administrative in
    let%bind admission =
      Turn_admission.create
        t.state
        ~operation
        ~notification_wakes:[]
        ~adopt_deferred:false
    in
    let delta =
      Session_delta.Batch
        (History_continuation.deltas recovery @ Turn_admission.deltas admission)
    in
    let payloads =
      History_continuation.payloads recovery @ Turn_admission.payloads admission
    in
    let%map session =
      commit_history_transition t ~delta ~payloads ~admission:(Some admission)
    in
    ({ session
     ; continuation = Agent_protocol.History_edit.Continuation.Started operation.id
     }
     : History_continuation_result.t)
;;

let suppress_follow_up_deltas t deltas ~reason =
  let open Result.Let_syntax in
  List.map deltas ~f:(function
    | (Session_delta.Invocation_changed next | Invocation_reconciled next) as delta ->
      let%bind original =
        List.find t.state.invocations ~f:(fun value ->
          Agent_protocol.Id.Invocation.equal value.context.id next.context.id)
        |> Result.of_option
             ~error:(error Invalid_state "automatic-turn invocation is missing")
      in
      let%map values = Observation_follow_up.discard [ original ] ~reason in
      (match values with
       | [] -> delta
       | value :: _ -> Observation_follow_up.delta value)
    | (Moderator_execution_changed next | Moderator_execution_reconciled next) as delta ->
      let%bind original =
        List.find t.state.moderator_executions ~f:(fun value ->
          Agent_protocol.Id.Moderator_execution.equal value.context.id next.context.id)
        |> Result.of_option ~error:(error Invalid_state "automatic-turn event is missing")
      in
      let%map values = Observation_follow_up.discard_events [ original ] ~reason in
      (match values with
       | [] -> delta
       | value :: _ -> Observation_follow_up.event_delta value)
    | delta -> Ok delta)
  |> Result.all
;;

let start_idle_turn
      ?continuation_custody
      ?follow_up_authority
      ?(extra_deltas = [])
      ?(extra_payloads = [])
      ?(notification_wakes = [])
      t
      (drain : Runtime_builder.moderator_drain)
      ~reason
      ~adopt_deferred
  =
  let open Result.Let_syntax in
  let%bind eligible = if adopt_deferred then eligible_pending t Idle_start else Ok [] in
  let adopt_deferred = not (List.is_empty eligible) in
  let reason = if adopt_deferred then Agent_protocol.Operation.User_submit else reason in
  let decision =
    match reason, t.state.automatic_turn_budget with
    | (Agent_protocol.Operation.User_submit | Administrative | Recovery_retry), _
    | _, None -> Chat_response.Automatic_turn_policy.Allow_automatic_turn
    | (Moderator_request | Idle_followup), Some budget ->
      (match budget.policy.honor_request_turn with
       | true -> Automatic_turn_budget.decide budget ~now:(t.services.now ())
       | false ->
         Suppress_automatic_turn
           { notice_key = "budget:request-turn-disabled"
           ; notice_text = "Automatic follow-up turns are disabled by runtime policy."
           })
  in
  match decision with
  | Allow_automatic_turn ->
    start_idle_turn_unchecked
      ?continuation_custody
      ?follow_up_authority
      ~extra_deltas
      ~extra_payloads
      ~notification_wakes
      t
      drain
      ~reason
      ~adopt_deferred
  | Suppress_automatic_turn { notice_key; notice_text } ->
    let open Result.Let_syntax in
    let%bind extra_deltas =
      suppress_follow_up_deltas t extra_deltas ~reason:notice_text
    in
    let%bind wake_deltas =
      List.map notification_wakes ~f:(fun value ->
        Agent_protocol.Delivery.discard_wake value ~reason:notice_text
        |> Result.map ~f:(fun value -> Session_delta.Delivery_wake_changed value))
      |> Result.all
    in
    transition
      t
      ~delta:
        (Session_delta.Batch
           (extra_deltas @ wake_deltas @ [ Moderator_changed drain.moderator_snapshot ]))
      ~payloads:
        (extra_payloads
         @ drain_payloads drain
         @ [ Agent_protocol.Event.Durable.Payload.Moderator_notification
               (`Object [ "key", `String notice_key; "message", `String notice_text ])
           ])
    |> Result.map ~f:ignore
;;

let stop_from_idle_moderator
      ?(extra_deltas : Session_delta.t list = [])
      t
      (drain : Runtime_builder.moderator_drain)
      reason
  =
  let open Result.Let_syntax in
  let jobs = stopped_jobs t Cancel in
  let job_custody = Job_custody.prepare_retirement t.job_agent_custody in
  let permissions =
    pending_invocation_permissions t ~matches:(fun _ -> true)
    |> List.map ~f:(cancel_permission t "session ended")
  in
  let changed invocation =
    List.exists extra_deltas ~f:(function
      | Session_delta.Invocation_changed updated | Invocation_reconciled updated ->
        Agent_protocol.Id.Invocation.equal
          updated.context.id
          invocation.Agent_protocol.Invocation.context.id
      | _ -> false)
  in
  let%bind discarded =
    Observation_follow_up.discard
      (List.filter t.state.invocations ~f:(fun invocation -> not (changed invocation)))
      ~reason:"moderator ended session"
  in
  let%bind events =
    Observation_follow_up.discard_events
      (List.filter t.state.moderator_executions ~f:(fun event ->
         not
           (List.exists extra_deltas ~f:(function
              | Session_delta.Moderator_execution_changed updated
              | Moderator_execution_reconciled updated ->
                Agent_protocol.Id.Moderator_execution.equal
                  event.context.id
                  updated.context.id
              | _ -> false))))
      ~reason:"moderator ended session"
  in
  let extra_deltas =
    extra_deltas
    @ List.map discarded ~f:Observation_follow_up.delta
    @ List.map events ~f:Observation_follow_up.event_delta
    @ List.map jobs ~f:(fun job -> Session_delta.Job_changed job)
    @ List.map permissions ~f:(fun permission ->
      Session_delta.Permission_changed permission)
  in
  let lifecycle = Session_state.Lifecycle.{ desired = Stopped; observed = Stopped } in
  let payloads =
    drain_payloads drain
    @ List.map jobs ~f:(fun job ->
      Agent_protocol.Event.Durable.Payload.Job_state_changed job)
    @ List.map permissions ~f:(fun permission ->
      Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
    @ [ Agent_protocol.Event.Durable.Payload.Moderator_notification
          (`Object [ "end_session_reason", `String reason ])
      ; Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ]
  in
  let%map _ =
    transition
      ~retire_legacy_custody:()
      ~job_custody
      t
      ~delta:
        (Session_delta.Batch
           (extra_deltas
            @ [ Session_delta.Moderator_changed drain.moderator_snapshot
              ; Halt_changed (Some reason)
              ; Lifecycle_changed lifecycle
              ]))
      ~payloads
  in
  cancel_job_scopes t jobs;
  resolve_cleaned_permission_waiters t permissions
;;

let deliver_idle_notifications_internal t (proposal : Notification_delivery.idle) =
  let open Result.Let_syntax in
  match idle_moderator_eligible t with
  | false -> Ok false
  | true ->
    let%bind () = validate_notification_plan t proposal.pending in
    let%bind halted = Runtime_builder.moderator_snapshot_is_halted t.state.moderator in
    (match halted with
     | true -> Ok false
     | false ->
       let%bind follow_up =
         Observation_follow_up.plan
           ~state:t.state
           ~observer:proposal.pending.source
           ~halted:false
           ~compaction_operation_id:(Agent_protocol.Id.Operation.create ())
       in
       (match follow_up.action with
        | Stop _ | Compact -> Ok false
        | Checkpoint | Turn ->
          let%bind deltas, entries, committed = notification_changes t proposal.pending in
          let deltas =
            deltas
            @ List.map proposal.discarded_wakes ~f:(fun value ->
              Session_delta.Delivery_wake_changed value)
          in
          let notification_wakes =
            proposal.wakes
            @ List.filter committed ~f:(fun value ->
              match value.wake_disposition with
              | Some Pending_wake -> true
              | _ -> false)
          in
          (match deltas, notification_wakes with
           | [], [] -> Ok false
           | _ ->
             let extra_deltas = deltas @ follow_up_deltas t follow_up in
             let extra_payloads = notification_payloads entries in
             let drain : Runtime_builder.moderator_drain =
               { moderator_snapshot = t.state.moderator
               ; runtime_requests = []
               ; notifications = []
               ; remaining_events = false
               }
             in
             let%bind eligible = eligible_pending t Idle_start in
             let adopt_deferred = not (List.is_empty eligible) in
             let needs_turn =
               adopt_deferred
               || (not (List.is_empty notification_wakes))
               ||
               match follow_up.action with
               | Turn -> true
               | _ -> false
             in
             let%map () =
               match needs_turn, t.operation_worker with
               | false, _ ->
                 transition
                   t
                   ~delta:(Session_delta.Batch extra_deltas)
                   ~payloads:extra_payloads
                 |> Result.map ~f:ignore
               | true, None ->
                 Error
                   (error Invalid_state "notification wake requires an installed worker")
               | true, Some _ ->
                 start_idle_turn
                   ~extra_deltas
                   ~extra_payloads
                   ~notification_wakes
                   t
                   drain
                   ~reason:Idle_followup
                   ~adopt_deferred
             in
             true)))
;;

let apply_observation_follow_up t =
  if not (idle_actor_available t)
  then Ok false
  else
    let open Result.Let_syntax in
    let%bind observer = Runtime_builder.moderator_snapshot_observer t.state.moderator in
    let%bind halted = Runtime_builder.moderator_snapshot_is_halted t.state.moderator in
    let compaction = create_compaction_operation t in
    let%bind plan =
      Observation_follow_up.plan
        ~state:t.state
        ~observer
        ~halted
        ~compaction_operation_id:compaction.id
    in
    let%bind authority = plan_agent_custody t plan in
    let continuation_custody = authority.Follow_up_authority.custody in
    let follow_up_authority = Option.map authority.observations ~f:(fun _ -> authority) in
    let extra_deltas = follow_up_deltas t plan in
    let drain : Runtime_builder.moderator_drain =
      { moderator_snapshot = t.state.moderator
      ; runtime_requests = []
      ; notifications = []
      ; remaining_events = false
      }
    in
    let%bind eligible = eligible_pending t Idle_start in
    let resume_deferred = (not halted) && not (List.is_empty eligible) in
    let%map () =
      match plan.action, extra_deltas with
      | Checkpoint, _ when resume_deferred && Option.is_none t.operation_worker ->
        Error (error Invalid_state "deferred user turn requires an installed worker")
      | Checkpoint, _ when resume_deferred ->
        (* Queued/background callbacks can defer a user submission without
           requesting a moderator turn. Resume it once the callback releases
           its borrow; no notification or later event should be required. *)
        start_idle_turn ~extra_deltas t drain ~reason:User_submit ~adopt_deferred:true
      | Checkpoint, [] -> Ok ()
      | Checkpoint, _ ->
        transition t ~delta:(Session_delta.Batch extra_deltas) ~payloads:[]
        |> Result.map ~f:ignore
      | Stop reason, _ -> stop_from_idle_moderator ~extra_deltas t drain reason
      | Compact, _ ->
        start_compaction
          ?agent_custody:continuation_custody
          ?follow_up_authority
          ~operation:compaction
          ~extra_deltas
          t
        |> Result.map ~f:ignore
      | Turn, _ when Option.is_none t.operation_worker ->
        Error (error Invalid_state "follow-up turn requires an installed worker")
      | Turn, _ ->
        start_idle_turn
          ?continuation_custody
          ?follow_up_authority
          ~extra_deltas
          t
          drain
          ~reason:Moderator_request
          ~adopt_deferred:(not (List.is_empty eligible))
    in
    resume_deferred || not (List.is_empty extra_deltas)
;;

let checkpoint_idle_moderator t (drain : Runtime_builder.moderator_drain) =
  transition
    t
    ~delta:(Session_delta.Moderator_changed drain.Runtime_builder.moderator_snapshot)
    ~payloads:(drain_payloads drain)
  |> Result.map ~f:(fun _ -> ())
;;

let complete_running_idle_moderator t (drain : Runtime_builder.moderator_drain) =
  let open Result.Let_syntax in
  let requests = drain.Runtime_builder.runtime_requests in
  let%bind eligible = eligible_pending t Idle_start in
  match eligible with
  | _ :: _ -> start_idle_turn t drain ~reason:User_submit ~adopt_deferred:true
  | [] when Chat_response.Runtime_semantics.request_compaction requests ->
    let open Result.Let_syntax in
    let%bind () = checkpoint_idle_moderator t drain in
    Result.map (start_compaction t) ~f:(fun _ -> ())
  | [] when Chat_response.Runtime_semantics.request_turn requests ->
    start_idle_turn t drain ~reason:Moderator_request ~adopt_deferred:false
  | [] -> checkpoint_idle_moderator t drain
;;

let complete_idle_moderator t (drain : Runtime_builder.moderator_drain) =
  if (not t.idle_moderator_borrowed) || moderator_is_borrowed t
  then Error (error Conflict "session moderator is not borrowed for idle work")
  else (
    let result =
      match Chat_response.Runtime_semantics.should_end_session drain.runtime_requests with
      | Some reason -> stop_from_idle_moderator t drain reason
      | None
        when runtime_admission_open t
             && Agent_protocol.Session.equal_desired_state
                  t.state.lifecycle.desired
                  Running -> complete_running_idle_moderator t drain
      | None -> checkpoint_idle_moderator t drain
    in
    t.idle_moderator_borrowed <- false;
    result)
;;

let fail_idle_moderator t failure =
  if moderator_is_borrowed t
  then Error (error Conflict "observation callback owns the idle moderator")
  else (
    t.idle_moderator_borrowed <- false;
    let lifecycle =
      Session_state.Lifecycle.
        { desired = t.state.lifecycle.desired
        ; observed = Agent_protocol.Session.Failed failure
        }
    in
    transition
      t
      ~delta:
        (Session_delta.Batch
           [ Failure_changed (Some failure); Lifecycle_changed lifecycle ])
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ]
    |> Result.map ~f:(fun _ -> ()))
;;

let lease_generation t = Int64.(t.state.counters.owner_lease_generation + 1L)

let reclaim_token_sha256 token =
  Digestif.SHA256.digest_string token |> Digestif.SHA256.to_hex
;;

let reclaim_token_matches lease token =
  match lease.Agent_protocol.Session.Owner_lease.reclaim_token_sha256 with
  | None -> false
  | Some expected ->
    Option.value_map
      (Digestif.SHA256.of_hex_opt expected)
      ~default:false
      ~f:(fun expected ->
        Digestif.SHA256.equal expected (Digestif.SHA256.digest_string token))
;;

let owner_principal_matches t lease principal_id =
  let expected =
    Option.first_some
      lease.Agent_protocol.Session.Owner_lease.principal_id
      t.state.identity.creating_principal
  in
  Option.both expected principal_id
  |> Option.exists ~f:(fun (expected, actual) ->
    Agent_protocol.Id.Principal.compare expected actual = 0)
;;

let owner_lease t generation ~principal_id ~reclaim_token =
  match t.state.spec.protocol.liveness with
  | Agent_protocol.Session.Owner_bound _ ->
    let now = t.services.now () |> Agent_protocol.Timestamp.to_time_ns in
    let span = Time_ns.Span.of_ms (Float.of_int t.owner_lease_duration_ms) in
    let expires_at = Time_ns.add now span |> Agent_protocol.Timestamp.of_time_ns in
    Some
      Agent_protocol.Session.Owner_lease.
        { generation
        ; expires_at
        ; disconnect_grace_until = None
        ; principal_id
        ; reclaim_token_sha256 = Option.map reclaim_token ~f:reclaim_token_sha256
        }
  | Detached | Process_bound -> None
;;

let owner_attachment t =
  List.find t.state.attachments ~f:(fun attachment ->
    Agent_protocol.Session.equal_attachment_mode attachment.mode Owner_read_write)
;;

let lease_deadline lease =
  Option.value
    lease.Agent_protocol.Session.Owner_lease.disconnect_grace_until
    ~default:lease.expires_at
;;

let schedule_owner_expiry_at t generation deadline =
  Option.iter t.owner_timer_cancel ~f:(fun resolver -> Eio.Promise.resolve resolver ());
  let cancelled, cancel = Eio.Promise.create () in
  t.owner_timer_cancel <- Some cancel;
  let now = t.services.now () |> Agent_protocol.Timestamp.to_time_ns in
  let deadline = Agent_protocol.Timestamp.to_time_ns deadline in
  let delay = Time_ns.diff deadline now |> Time_ns.Span.to_sec |> Float.max 0. in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    let expired =
      Eio.Fiber.first
        (fun () ->
           t.sleep delay;
           true)
        (fun () ->
           Eio.Promise.await cancelled;
           false)
    in
    if expired
    then (
      let promise, resolver = Eio.Promise.create () in
      let packed = Pack (None, Owner_expired generation, resolver) in
      match Mailbox.push t.mailbox ~priority:Priority packed with
      | Error _ -> ()
      | Ok () ->
        ignore (Actor_reply.await promise : (unit, Agent_protocol.Error.t) result)))
;;

let schedule_owner_lease t lease =
  schedule_owner_expiry_at
    t
    lease.Agent_protocol.Session.Owner_lease.generation
    (lease_deadline lease)
;;

let disconnect_grace_deadline t disconnect_grace_ms =
  let now = t.services.now () |> Agent_protocol.Timestamp.to_time_ns in
  Time_ns.add now (Time_ns.Span.of_ms (Float.of_int disconnect_grace_ms))
  |> Agent_protocol.Timestamp.of_time_ns
;;

let mark_owner_disconnected t attachment disconnect_grace_ms =
  let open Result.Let_syntax in
  let%bind lease =
    attachment.Agent_protocol.Session.Attachment.owner_lease
    |> Result.of_option ~error:(error Invalid_state "owner attachment has no lease")
  in
  let lease =
    let grace = disconnect_grace_deadline t disconnect_grace_ms in
    let disconnect_grace_until =
      if Agent_protocol.Timestamp.compare grace lease.expires_at <= 0
      then grace
      else lease.expires_at
    in
    { lease with disconnect_grace_until = Some disconnect_grace_until }
  in
  let attachment = { attachment with owner_lease = Some lease } in
  let%map _ =
    transition
      t
      ~delta:(Session_delta.Attachment_added attachment)
      ~payloads:[ Agent_protocol.Event.Durable.Payload.Attachment_owner_changed None ]
  in
  schedule_owner_lease t lease
;;

let owner_expired t generation =
  t.owner_timer_cancel <- None;
  match owner_attachment t with
  | None -> Ok ()
  | Some attachment ->
    (match attachment.owner_lease with
     | None -> Ok ()
     | Some lease when not (Int64.equal lease.generation generation) -> Ok ()
     | Some lease ->
       let now = t.services.now () in
       let deadline = lease_deadline lease in
       if Agent_protocol.Timestamp.compare deadline now > 0
       then (
         schedule_owner_expiry_at t generation deadline;
         Ok ())
       else (
         match lease.disconnect_grace_until, t.state.spec.protocol.liveness with
         | None, Owner_bound { disconnect_grace_ms; _ } ->
           mark_owner_disconnected t attachment disconnect_grace_ms
         | Some _, Owner_bound { stop_mode; _ } ->
           let open Result.Let_syntax in
           let%bind _ =
             transition
               t
               ~delta:(Session_delta.Attachment_removed attachment.id)
               ~payloads:
                 [ Agent_protocol.Event.Durable.Payload.Attachment_owner_changed None ]
           in
           Result.map (stop_internal t stop_mode) ~f:(fun _ -> ())
         | _, (Detached | Process_bound) -> Ok ()))
;;

let renew_owner t attachment_id expected_generation =
  let open Result.Let_syntax in
  let%bind attachment = write_attachment t attachment_id in
  match attachment.mode, attachment.owner_lease with
  | Owner_read_write, Some lease ->
    if not (Int64.equal lease.generation expected_generation)
    then Error (error Lease_stale "owner lease generation does not match")
    else if Option.is_some lease.disconnect_grace_until
    then Error (error Lease_stale "disconnected owner lease must be reclaimed")
    else if Agent_protocol.Timestamp.compare lease.expires_at (t.services.now ()) <= 0
    then Error (error Lease_stale "owner lease has expired")
    else (
      let generation = lease_generation t in
      let%bind owner_lease =
        owner_lease t generation ~principal_id:lease.principal_id ~reclaim_token:None
        |> Result.of_option ~error:(error Invalid_state "owner lease is unavailable")
      in
      let owner_lease =
        { owner_lease with reclaim_token_sha256 = lease.reclaim_token_sha256 }
      in
      let attachment = { attachment with owner_lease = Some owner_lease } in
      let%map session =
        transition
          t
          ~delta:
            (Session_delta.Batch
               [ Attachment_added attachment; Owner_lease_generation_changed generation ])
          ~payloads:
            [ Agent_protocol.Event.Durable.Payload.Attachment_owner_changed
                (Some attachment)
            ]
      in
      schedule_owner_lease t owner_lease;
      owner_lease, session)
  | Owner_read_write, None -> Error (error Invalid_state "owner attachment has no lease")
  | (Read_write | Read_only), _ ->
    Error (error Permission_denied "attachment does not own the session")
;;

let owner_reclaim_authorized t lease ~principal_id ~reclaim_token =
  owner_principal_matches t lease principal_id
  || Option.exists reclaim_token ~f:(reclaim_token_matches lease)
;;

let install_subscriber t attachment_id subscribe =
  let subscriber =
    Option.some_if subscribe (Subscriber.create ~capacity:t.subscriber_capacity)
  in
  Option.iter subscriber ~f:(fun value ->
    Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
      t.subscribers := Map.set !(t.subscribers) ~key:attachment_id ~data:value));
  subscriber
;;

let validate_attachment_capacity t ~replacing =
  if replacing || List.length t.state.attachments < t.max_attachments
  then Ok ()
  else Error (error Resource_limit "session attachment limit reached")
;;

let attach_owner t subscribe ~principal_id ~reclaim_token =
  let open Result.Let_syntax in
  let%bind () =
    match owner_attachment t with
    | Some { owner_lease = Some lease; _ }
      when Agent_protocol.Timestamp.compare (lease_deadline lease) (t.services.now ())
           <= 0 -> owner_expired t lease.generation
    | _ -> Ok ()
  in
  let previous = owner_attachment t in
  let%bind () = validate_attachment_capacity t ~replacing:(Option.is_some previous) in
  let%bind () =
    match previous with
    | None -> Ok ()
    | Some { owner_lease = None; _ } ->
      Error (error Conflict "session already has an owner attachment")
    | Some { owner_lease = Some lease; _ } ->
      (match lease.disconnect_grace_until with
       | None -> Error (error Conflict "session already has an active owner attachment")
       | Some _ ->
         if owner_reclaim_authorized t lease ~principal_id ~reclaim_token
         then Ok ()
         else Error (error Permission_denied "owner reclaim credential is invalid"))
  in
  let issued_reclaim_token = t.services.create_reclaim_token () in
  let%bind () =
    if String.is_empty issued_reclaim_token
    then
      Error (error Invalid_state "owner reclaim token generator returned an empty token")
    else Ok ()
  in
  let generation = lease_generation t in
  let%bind lease =
    owner_lease t generation ~principal_id ~reclaim_token:(Some issued_reclaim_token)
    |> Result.of_option ~error:(error Invalid_state "owner lease is unavailable")
  in
  let attachment_id = t.services.create_attachment_id () in
  let attachment =
    Agent_protocol.Session.Attachment.
      { id = attachment_id
      ; session_id = t.state.identity.session_id
      ; mode = Owner_read_write
      ; owner_lease = Some lease
      }
  in
  let deltas =
    Option.to_list
      (Option.map previous ~f:(fun previous ->
         Session_delta.Attachment_removed previous.id))
    @ [ Session_delta.Attachment_added attachment
      ; Session_delta.Owner_lease_generation_changed generation
      ]
  in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch deltas)
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Attachment_owner_changed (Some attachment)
        ]
  in
  schedule_owner_lease t lease;
  let subscriber = install_subscriber t attachment_id subscribe in
  Ok (attachment, subscriber, current_snapshot t, Some issued_reclaim_token)
;;

let attach_nonowner t mode subscribe =
  let open Result.Let_syntax in
  let%bind () = validate_attachment_capacity t ~replacing:false in
  let attachment_id = t.services.create_attachment_id () in
  let attachment =
    Agent_protocol.Session.Attachment.
      { id = attachment_id
      ; session_id = t.state.identity.session_id
      ; mode
      ; owner_lease = None
      }
  in
  let%bind _ =
    transition t ~delta:(Session_delta.Attachment_added attachment) ~payloads:[]
  in
  let subscriber = install_subscriber t attachment_id subscribe in
  Ok (attachment, subscriber, current_snapshot t, None)
;;

let attach t mode subscribe principal_id reclaim_token =
  match mode, t.state.spec.protocol.liveness with
  | Agent_protocol.Session.Owner_read_write, Agent_protocol.Session.Owner_bound _ ->
    attach_owner t subscribe ~principal_id ~reclaim_token
  | Owner_read_write, (Detached | Process_bound) -> attach_nonowner t mode subscribe
  | (Read_write | Read_only), _ -> attach_nonowner t mode subscribe
;;

let detach t attachment_id =
  match
    List.find t.state.attachments ~f:(fun value ->
      Agent_protocol.Id.Attachment.compare value.id attachment_id = 0)
  with
  | None -> Error (error Invalid_request "attachment is not active")
  | Some attachment ->
    Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
      Option.iter (Map.find !(t.subscribers) attachment_id) ~f:Subscriber.close;
      t.subscribers := Map.remove !(t.subscribers) attachment_id);
    (match attachment.mode, t.state.spec.protocol.liveness with
     | Owner_read_write, Owner_bound { disconnect_grace_ms; _ } ->
       mark_owner_disconnected t attachment disconnect_grace_ms
     | _, _ ->
       Result.map
         (transition
            t
            ~delta:(Session_delta.Attachment_removed attachment_id)
            ~payloads:[])
         ~f:(fun _ -> ()))
;;

let enable_automatic_turn_budget ?run_preparation t policy =
  let open Result.Let_syntax in
  let%bind () =
    match run_preparation with
    | None -> Ok ()
    | Some preparation -> check_run_preparation t preparation
  in
  match
    t.state.automatic_turn_budget, t.state.active_operation, moderator_is_borrowed t
  with
  | Some budget, _, _
    when Chat_response.Runtime_semantics.equal_policy budget.policy policy -> Ok ()
  | _, None, false ->
    transition
      ?run_preparation
      t
      ~delta:(Session_delta.Automatic_turn_budget_enabled policy)
      ~payloads:[]
    |> Result.map ~f:ignore
  | _ ->
    Error (error Conflict "cannot enable automatic-turn accounting during active work")
;;

let lifecycle_request_allowed : type a. a request -> bool = function
  | Set_operation_worker None
  | Set_runtime_worker (_, None, _, _)
  | Set_compaction_inference None -> true
  | Set_operation_worker (Some _)
  | Set_runtime_worker (_, Some _, _, _)
  | Set_compaction_inference (Some _) -> false
  | Acknowledge_inference _
  | Observe_inference _
  | Observe_owned_inference _
  | Complete_inference _
  | Release_inference _
  | Seal_inference_owner _
  | Finish_inference_owner _
  | Close_run_preparation _
  | Release_recurrence_due _
  | Run_start_receipt _
  | Complete_run_constructor_model_job _
  | Interrupt_run_constructor_job _
  | Cancel_run_constructor_schedule _
  | Claim_queued_retirement _
  | Commit_queued_event _
  | Finish_queued_event _
  | Set_queued_event_cancel _
  | Set_job_scope_cancel _
  | Seal_job_scope _
  | Finish_job_scope _
  | Abort_subscription_mutation _
  | Read_script_subscription _
  | Read_script_ingress _
  | Abort_ingress_mutation _
  | Abort_schedule_mutation _
  | Read_script_schedule _
  | Read_script_notification _
  | Abort_notification_mutation _
  | Abort_background_job _
  | Has_staged_background_job _
  | Read_script_job _
  | Read_script_job_result _
  | Finish_invocation _
  | Finish_moderator_invocation _
  | Set_idle_moderator_cancel _
  | Configuration
  | Finish_configuration_capture _
  | Lifecycle_state _
  | Retire_lifecycle _
  | Abort_lifecycle _
  | Read_goals _
  | State
  | Snapshot
  | Observe
  | Authorize_writer _
  | Retire_runtime_worker _
  | Shell_approval_grants
  | Shell_manifest_grants
  | End_initialization _
  | Retire_initialization_resource _
  | Fail_initialization _
  | Stop_delegated _
  | Stop_delegated_at_epoch _
  | Commit_worker_entry _
  | Publish_invocation_output _
  | Commit_worker_moderator _
  | Has_writer_attachment
  | Invocation_granted _
  | Worker_ready _
  | Worker_terminal _
  | Compaction_terminal _
  | Cancel_operation _
  | Resolve_permission_system _
  | Initialization_model_job_is_current _
  | Complete_initialization_model_job _
  | Read_job _
  | Publish_job_progress _
  | Complete_job _
  | Complete_background_job _
  | Defer_background_job _
  | Cancel_job_internal _
  | Interrupt_job _
  | Cancel_schedule_internal _
  | Complete_schedule _
  | Fail_schedule _
  | Skip_schedule _
  | Complete_idle_moderator _
  | Fail_idle_moderator _
  | Expire_permission _
  | Detach _
  | Owner_expired _
  | Checkpoint _
  | Quiescent_checkpoint _
  | Shutdown -> true
  | Open_inference_owner _
  | Admit_inference _
  | Allocate_inference _
  | Admit_model_job _
  | Refresh_inference _
  | Reconcile_inference_recovery
  | Claim_delegated_event _
  | Claim_job_moderator _
  | Claim_job_event _
  | Manage_moderator_follow_up _
  | Admit_moderator_turn _
  | Admit_notification_turn _
  | Claim_ordinary_event _
  | Claim_queued_event _
  | Claim_legacy_callback _
  | Claim_initialization_callback _
  | Reconcile_run_recovery
  | Run_actions _
  | Commit_invocation_call _
  | Claim_invocation _
  | Claim_idle_invocation _
  | Claim_event_invocation _
  | Claim_job_scope _
  | Prepare_background_job _
  | Stage_background_job _
  | Stage_subscription_mutation _
  | Create_script_subscription _
  | Finish_script_subscription _
  | Select_subscription_mutations _
  | Expire_subscriptions
  | Create_script_ingress _
  | Revoke_script_ingress _
  | Select_ingress_mutations _
  | Stage_schedule_mutation _
  | Create_script_schedule _
  | Select_schedule_mutations _
  | Select_background_jobs _
  | Create_script_notification _
  | Select_notification_mutations _
  | Cancel_script_job _
  | Claim_job_invocation _
  | Claim_moderator_invocation _
  | Claim_moderator_observation _
  | Claim_next_moderator_observation _
  | Claim_idle_moderator_observation _
  | Commit_moderator_invocation _
  | Commit_extensions _
  | Set_organization_admission _
  | Set_configuration_policy _
  | Prepare_configuration_update _
  | Commit_configuration_update _
  | Begin_configuration_capture _
  | Mark_configuration_capture _
  | Check_runtime_admission
  | Begin_lifecycle _
  | Enable_automatic_turn_budget _
  | Enable_run_constructor_turn_budget _
  | Set_automatic_turn_pauses _
  | Change_moderator _
  | Change_workspace _
  | Replace_shell_approval_grants _
  | Replace_shell_extension_snapshots _
  | Add_shell_manifest_grant _
  | Reset _
  | Upgrade_prompt _
  | Commit_administration _
  | Validate_administration_basis _
  | Commit_reconciled_administration _
  | Begin_initialization _
  | Borrow_initialization_agent_actor _
  | Borrow_operation_agent_actor _
  | Borrow_compaction_agent_actor _
  | Borrow_invocation_agent_actor _
  | Borrow_moderator_agent_actor _
  | Begin_run_initialization _
  | Capture_run_constructor_model_source _
  | Capture_run_constructor_recipe_target _
  | Start_run_constructor_model_job _
  | Add_run_constructor_job _
  | Add_run_constructor_schedule _
  | Reserve_run_history_block _
  | Checkpoint_run_moderator _
  | Admit_prepared_run _
  | Begin_run_preparation _
  | Begin_headless_run_preparation _
  | Complete_initialization _
  | Checkpoint_runtime_construction _
  | Start _
  | Start_initial_delegated _
  | Fail_initial_delegated _
  | Queue_start _
  | Activate_queued_start
  | Update_organization _
  | Apply_goal_command _
  | Apply_instruction_command _
  | Set_recurrence_controller _
  | Set_recurrence_planning_host _
  | Recurrence_due_ids
  | Begin_recurrence_due _
  | Finish_recurrence_due _
  | Read_recurrence_planning _
  | Apply_recurrence_command _
  | Reserve_recurrence_child _
  | Check_recurrence_creation _
  | Record_recurrence_execution _
  | Verify_recurrence_child _
  | Acquire_recurrence_activation _
  | Check_recurrence_activation _
  | Release_recurrence_activation _
  | Observe_recurrence_run _
  | Import_recurrence_execution _
  | Link_recurrence_child _
  | Read_recurrences _
  | Update_metadata _
  | Stop _
  | Stop_managed _
  | Append_history _
  | Defer_history _
  | Submit_message _
  | Control_pending _
  | Admit_run _
  | Submit_managed_message _
  | Compact _
  | Delete_history _
  | Edit_history _
  | Continue_history _
  | Adopt_deferred
  | Reserve_history_block _
  | Prepare_authoring_input _
  | Consume_deferred _
  | Consume_notifications _
  | Deliver_idle_notifications _
  | Admit_standalone_delivery _
  | Retire_obsolete_moderator_delivery _
  | Deliver_standalone_completion _
  | Consume_initial_notifications _
  | Open_permission _
  | Respond_permission _
  | Revoke_grant _
  | Change_job _
  | Capture_inference_target _
  | Capture_model_job_source _
  | Capture_recipe_target _
  | Add_initialization_model_job _
  | Start_initialization_model_job _
  | Claim_initialization_model_job _
  | Capture_initialization_recipe_target _
  | Add_job _
  | Claim_job _
  | Refresh_background_job _
  | Recover_background_results _
  | Enqueue_run_job_delivery _
  | Deliver_job _
  | Cancel_job _
  | Change_schedule _
  | Add_schedule _
  | Callback_schedule_after_ms _
  | Callback_schedule_cancel _
  | Cancel_schedule _
  | Claim_schedule _
  | Due_schedules
  | Retry_schedule _
  | Prepare_ingress_submission _
  | Commit_ingress_submission _
  | Claim_idle_moderator
  | Apply_observation_follow_up
  | Attach _
  | Renew_owner _ -> false
;;

let check_recurrence_creation_admission t prepared =
  let open Result.Let_syntax in
  let%bind () =
    if Option.is_none (Eio.Promise.peek t.finished)
    then Ok ()
    else Error (error Server_shutting_down "recurrence actor has terminated")
  in
  let%bind () = require_runtime_admission t in
  let%bind moderator_halted =
    Runtime_builder.moderator_snapshot_is_halted t.state.moderator
  in
  let%bind () =
    match t.state.lifecycle.desired, t.state.runtime_initialization, t.state.failure with
    | Running, Ready, None when not (t.stopped || t.state.halted || moderator_halted) ->
      Ok ()
    | (Running | Stopped), (Ready | Pending _), (Some _ | None) ->
      Error (error Conflict "recurrence creation controller runtime is unavailable")
  in
  let%bind () = Recurrence_creation_admission.check_current prepared t.state in
  let%bind snapshot =
    Recurrence_runtime.snapshot
      t.recurrence_runtime
      ~wall:(t.services.now ())
      ~monotonic:(t.services.monotonic_now ())
  in
  let entry = Recurrence_creation_admission.entry prepared in
  let%bind late =
    Recurrence_runtime.selection_lateness
      t.recurrence_runtime
      ~entry
      ~nominal:(Recurrence_creation_admission.nominal prepared)
      ~snapshot
  in
  let%bind () =
    match late with
    | Some late
      when Int64.(
             late
             <= Recurrence_policy.grace_ns
                  (Recurrence_definition.policy (Recurrence_entry.current entry))) ->
      Ok ()
    | None | Some _ ->
      Error (error Conflict "recurrence creation nominal is outside current grace")
  in
  let%bind () = require_runtime_admission t in
  Recurrence_creation_admission.check_current prepared t.state
;;

let check_recurrence_execution_admission t prepared =
  let open Result.Let_syntax in
  let%bind () =
    if
      (not t.stopped)
      && Option.is_none (Eio.Promise.peek t.finished)
      && Option.is_none t.lifecycle_fence
    then Ok ()
    else Error (error Conflict "recurrence execution controller owner is unavailable")
  in
  let%bind () = Recurrence_execution_admission.check_current prepared t.state in
  match Recurrence_execution_admission.decision prepared with
  | Retained _ -> Ok ()
  | New _ ->
    let%bind () = require_runtime_admission t in
    let%bind moderator_halted =
      Runtime_builder.moderator_snapshot_is_halted t.state.moderator
    in
    let%bind () =
      match
        t.state.lifecycle.desired, t.state.runtime_initialization, t.state.failure
      with
      | Running, Ready, None when not (t.state.halted || moderator_halted) -> Ok ()
      | (Running | Stopped), (Ready | Pending _), (Some _ | None) ->
        Error (error Conflict "recurrence execution controller runtime is unavailable")
    in
    let%bind snapshot =
      Recurrence_runtime.snapshot
        t.recurrence_runtime
        ~wall:(t.services.now ())
        ~monotonic:(t.services.monotonic_now ())
    in
    let entry = Recurrence_execution_admission.entry prepared in
    let%bind late =
      Recurrence_runtime.selection_lateness
        t.recurrence_runtime
        ~entry
        ~nominal:(Recurrence_execution_admission.nominal prepared)
        ~snapshot
    in
    let%bind () =
      match late with
      | Some late
        when Int64.(
               late
               <= Recurrence_policy.grace_ns
                    (Recurrence_definition.policy (Recurrence_entry.current entry))) ->
        Ok ()
      | None | Some _ ->
        Error (error Conflict "recurrence execution nominal is outside current grace")
    in
    let%bind () = require_runtime_admission t in
    Recurrence_execution_admission.check_current prepared t.state
;;

let record_recurrence_execution_internal t ~actor ~authorize ~intent ~attempt =
  let open Result.Let_syntax in
  let%bind controller = require_recurrence_controller t in
  let prepare () =
    Recurrence_execution_admission.prepare
      t.state
      ~controller
      ~actor
      ~authorize
      ~intent
      ~attempt
  in
  let%bind prepared = prepare () in
  let%bind () = check_recurrence_execution_admission t prepared in
  match Recurrence_execution_admission.decision prepared with
  | Retained row -> Ok row
  | New { catalog; execution } ->
    let%bind candidate =
      prepare_transition
        t
        ~delta:(Session_delta.Recurrence_catalog_changed catalog)
        ~payloads:[]
    in
    let%bind () = check_recurrence_execution_admission t prepared in
    let%bind () = persist_transition t candidate in
    install_committed_transition t candidate;
    publish_committed_transition t candidate;
    (* Our own canonical projection rotates the genuine Current. Refresh rather
       than testing the precommit observation or claiming rollback after ACK. *)
    let%bind refreshed = prepare () in
    let%bind () = check_recurrence_execution_admission t refreshed in
    (match Recurrence_execution_admission.decision refreshed with
     | Retained row when Recurrence_execution.equal row execution -> Ok row
     | Retained _ | New _ ->
       Error (error Conflict "committed recurrence execution changed before return"))
;;

let check_lifecycle_fence t fence =
  if
    phys_equal t.lifecycle_owner fence.Lifecycle_fence.owner
    && Option.exists t.lifecycle_fence ~f:(fun current -> phys_equal current fence)
  then Ok ()
  else Error (error Conflict "session lifecycle fence is no longer current")
;;

let begin_lifecycle t attachment_id expected_generation expected_revision =
  let open Result.Let_syntax in
  let%bind () =
    match attachment_id with
    | None -> Ok ()
    | Some id -> Result.map (write_attachment t id) ~f:(fun _ -> ())
  in
  if Option.is_some t.lifecycle_fence
  then Error (error Conflict "session lifecycle change is already reserved")
  else if
    (not (Int.equal t.state.identity.generation expected_generation))
    || not (Int64.equal t.state.counters.revision expected_revision)
  then Error (error Conflict "session lifecycle canonical anchor changed")
  else (
    let fence = Lifecycle_fence.{ owner = t.lifecycle_owner; phase = Reserved } in
    close_recurrence_activations t;
    t.lifecycle_fence <- Some fence;
    Ok fence)
;;

let refresh_recurrence_creation t permit =
  let open Result.Let_syntax in
  let%bind () =
    if phys_equal permit.Recurrence_creation.owner t.recurrence_creation_owner
    then Ok ()
    else Error (error Conflict "recurrence creation permit belongs to another actor")
  in
  let%bind () = require_runtime_admission t in
  let%bind controller = require_recurrence_controller t in
  let%bind prepared =
    Recurrence_creation_admission.prepare
      t.state
      ~controller
      ~actor:permit.actor
      ~authorize:permit.authorize
      ~occurrence:(Recurrence_child_reservation.occurrence permit.reservation)
      ~expected_control_revision:
        (Recurrence_child_reservation.control_revision permit.reservation)
  in
  let%bind () = check_recurrence_creation_admission t prepared in
  let%bind () =
    match Recurrence_creation_admission.reservation prepared with
    | Some row when Recurrence_child_reservation.equal row permit.reservation -> Ok ()
    | None | Some _ ->
      Error (error Conflict "recurrence creation permit lost exact durable reservation")
  in
  let definition =
    Recurrence_entry.current (Recurrence_creation_admission.entry prepared)
  in
  let check_current () =
    let%bind () = check_recurrence_creation_admission t prepared in
    (* The trusted policy callback is nonyielding. Repeat the original proof
       and actual owner-state checks after it before the caller's effect. *)
    let%bind () = permit.authorize t.state definition in
    check_recurrence_creation_admission t prepared
  in
  let checked : Recurrence_creation.Checked.t =
    { definition
    ; principal = Operator_authorization.principal permit.actor
    ; check_current
    }
  in
  Ok checked
;;

let observe_recurrence_run_internal t ~actor ~authorize ~intent ~attempt =
  let open Result.Let_syntax in
  let owner_current () =
    if
      (not t.stopped)
      && Option.is_none (Eio.Promise.peek t.finished)
      && Option.is_none t.lifecycle_fence
    then Ok ()
    else Error (error Conflict "recurrence target observation owner is unavailable")
  in
  let check_read () =
    let%bind () = owner_current () in
    Recurrence_command_admission.check_read actor ~authorize t.state
  in
  let%bind () = check_read () in
  let%bind () = Recurrence_execution_attempt.validate attempt ~intent in
  let%bind controller = require_recurrence_controller t in
  let%bind () =
    if
      Agent_protocol.Session_ref.equal
        (Recurrence_controller.session controller)
        (Recurrence_execution_intent.target intent)
    then Ok ()
    else Error (error Conflict "recurrence observation targets another session")
  in
  let lookup () =
    match t.state.run_state with
    | None -> Ok None
    | Some index ->
      let%bind receipt =
        Run_state.receipt
          index
          ~principal_id:(Recurrence_execution_attempt.principal_id attempt)
          ~key:(Recurrence_execution_attempt.key attempt)
          ~request_sha256:(Recurrence_execution_attempt.request_sha256 attempt)
      in
      (match receipt with
       | None -> Ok None
       | Some admission ->
         let%bind origin =
           Result.of_option
             (Run_state.recurrence_origin index admission.run_id)
             ~error:(error Conflict "retained Run lacks its exact recurrence origin")
         in
         let%bind run =
           Result.of_option
             (Run_state.find index admission.run_id)
             ~error:(error Conflict "recurrence origin lost its retained Run")
         in
         let%bind () = Recurrence_run_origin.validate origin ~intent ~attempt in
         let%bind () = Recurrence_run_origin.validate_run origin ~run ~admission in
         let%map terminal =
           match run.lifecycle with
           | Admitted | Active | Waiting _ -> Ok None
           | Terminal _ ->
             let receipts =
               List.filter (Run_state.receipts index) ~f:(fun receipt ->
                 Agent_protocol.Run_receipt.Kind.equal receipt.kind Terminal
                 && Agent_protocol.Id.Run.equal receipt.run_id run.id
                 && Int64.equal receipt.run_revision run.revision)
             in
             (match receipts with
              | [ receipt ] ->
                Recurrence_run_reference.create ~origin ~run ~receipt
                |> Result.map ~f:Option.some
              | [] | _ :: _ :: _ ->
                Error
                  (error
                     Conflict
                     "recurrence terminal Run lacks one exact terminal receipt"))
         in
         Some (origin, run, admission, terminal))
  in
  let%bind evidence = lookup () in
  let%bind current = Recurrence_controller.observe controller t.state in
  let check_owner () =
    let%bind () = owner_current () in
    if Recurrence_controller.is_current current
    then Ok ()
    else Error (error Conflict "recurrence target lifecycle observation changed")
  in
  let check_basis () =
    let%bind () =
      if
        Operator_authorization.is_current actor
        && Agent_protocol.Principal.has_scope
             (Operator_authorization.principal actor)
             View_session_transcript
      then Ok ()
      else
        Error
          (error
             Permission_denied
             "recurrence original authority expired, was revoked or lacks scope")
    in
    let%bind () = check_owner () in
    let%bind actual = lookup () in
    let same =
      match evidence, actual with
      | None, None -> true
      | ( Some (origin, run, admission, terminal)
        , Some (next_origin, next_run, next_admission, next_terminal) ) ->
        Recurrence_run_origin.equal origin next_origin
        && Agent_protocol.Run.equal run next_run
        && Agent_protocol.Run_receipt.equal admission next_admission
        && Option.equal Recurrence_run_reference.equal terminal next_terminal
      | None, Some _ | Some _, None -> false
    in
    if same
    then check_owner ()
    else Error (error Conflict "recurrence target retained evidence changed")
  in
  let check () =
    let%bind () = check_read () in
    check_basis ()
  in
  let%bind () = check () in
  match evidence with
  | None -> Ok None
  | Some (observed_origin, observed_run, _, observed_terminal) ->
    let validate_reference reference =
      let%bind () = check () in
      let%bind () =
        match observed_terminal with
        | None -> Error (error Conflict "recurrence observation has no terminal payload")
        | Some terminal ->
          Recurrence_run_reference.validate
            reference
            ~origin:observed_origin
            ~run:observed_run
            ~receipt:(Recurrence_run_reference.receipt terminal)
      in
      check ()
    in
    let%map completion =
      match observed_terminal with
      | None -> Ok None
      | Some terminal ->
        Recurrence_runtime.completion
          t.recurrence_runtime
          ~origin:observed_origin
          ~terminal
    in
    let observation : Recurrence_run_observation.t =
      { origin = observed_origin
      ; terminal = observed_terminal
      ; controller
      ; check_current = check
      ; check_basis
      ; validate_reference
      ; completion
      ; check_admitted =
          (fun () ->
            let%bind () = check_basis () in
            match observed_run.lifecycle with
            | Admitted -> Ok ()
            | Active | Waiting _ | Terminal _ ->
              Error (error Conflict "activation target Run is no longer Admitted"))
      }
    in
    Some observation
;;

let check_recurrence_import_owner t =
  if
    (not t.stopped)
    && Option.is_none (Eio.Promise.peek t.finished)
    && Option.is_none t.lifecycle_fence
  then Ok ()
  else Error (error Conflict "recurrence settlement controller owner is unavailable")
;;

let recurrence_import_evidence t (observation : Recurrence_run_observation.t) =
  let open Result.Let_syntax in
  let origin = observation.origin in
  let occurrence = Recurrence_run_origin.occurrence origin in
  let recurrence_id =
    occurrence
    |> Recurrence_identity.Occurrence.definition
    |> Recurrence_identity.Definition.recurrence_id
  in
  let%bind retained =
    match
      Option.bind t.state.recurrence_catalog ~f:(fun catalog ->
        Recurrence_catalog.find catalog recurrence_id)
    with
    | None -> Ok None
    | Some entry ->
      let%map frontier = Recurrence_entry.frontier entry in
      Recurrence_frontier.find_execution frontier occurrence
  in
  match retained with
  | None -> Ok (origin, observation.terminal)
  | Some execution ->
    let attempt = Recurrence_execution.current_attempt execution in
    let%bind () =
      Recurrence_run_origin.validate
        origin
        ~intent:(Recurrence_execution.intent execution)
        ~attempt:(Recurrence_execution.Attempt.request attempt)
    in
    let%bind () =
      match Recurrence_execution.Attempt.origin attempt with
      | None -> Ok ()
      | Some saved when Recurrence_run_origin.equal saved origin -> Ok ()
      | Some _ ->
        Error (error Conflict "observed recurrence origin differs from retained data")
    in
    (match Recurrence_execution.Attempt.terminal attempt with
     | None -> Ok (origin, observation.terminal)
     | Some saved ->
       (* Authenticate annotations against the actual retained target payload,
          then keep this complete original carrier for strict pure equality. *)
       let%map () = observation.validate_reference saved in
       origin, Some saved)
;;

let prepare_recurrence_import_candidate
      t
      (observation : Recurrence_run_observation.t)
      admission
  =
  let open Result.Let_syntax in
  let prepare admission =
    prepare_transition
      t
      ~delta:
        (Session_delta.Recurrence_catalog_changed
           (Recurrence_settlement_admission.catalog admission))
      ~payloads:[]
  in
  let%bind candidate = prepare admission in
  match Recurrence_settlement_admission.settlement admission with
  | Imported { next = Successor basis; _ } ->
    let recurrence_id =
      Recurrence_delay_basis.occurrence basis
      |> Recurrence_identity.Occurrence.definition
      |> Recurrence_identity.Definition.recurrence_id
    in
    let%bind entry =
      Result.of_option
        (Option.bind candidate.state.recurrence_catalog ~f:(fun catalog ->
           Recurrence_catalog.find catalog recurrence_id))
        ~error:(error Conflict "settlement successor entry is absent from candidate")
    in
    let wall = t.services.now () in
    let monotonic = t.services.monotonic_now () in
    let%bind phase =
      Recurrence_runtime.prepare_completion_phase
        t.recurrence_runtime
        ~completion:observation.completion
        ~basis
        ~entry
        ~state:candidate.state
        ~wall
        ~monotonic
    in
    (match phase with
     | Prepared phase ->
       let timing : Recurrence_import.Timing.t =
         match Recurrence_runtime.Completion_phase.source phase with
         | Live_completion -> Live_phase
         | Recovery_adoption -> Recovery_adoption
       in
       Ok
         Recurrence_import_candidate.
           { admission; transition = candidate; phase = Some phase; timing }
     | Timing_unavailable diagnostic ->
       (* Rebuild from the original catalog, never commit the automatic
          alternative first or classify contradictory facts as clock trouble. *)
       let%bind admission = Recurrence_settlement_admission.hold admission in
       let%map candidate = prepare admission in
       Recurrence_import_candidate.
         { admission; transition = candidate; phase = None; timing = Held diagnostic })
  | Imported { next = Evidence_only | Review_hold _; _ } ->
    Ok
      Recurrence_import_candidate.
        { admission; transition = candidate; phase = None; timing = No_successor }
  | Retained_exact _ | Detail_not_retained _ ->
    Error (error Conflict "unchanged recurrence import cannot prepare a transaction")
;;

let import_recurrence_execution_internal
      t
      ~actor
      ~authorize
      ~automatic_policy
      ~(observation : Recurrence_run_observation.t)
  =
  let open Result.Let_syntax in
  let%bind () = check_recurrence_import_owner t in
  let%bind () =
    Recurrence_command_admission.check_write actor ~authorize:(fun _ -> Ok ()) t.state
  in
  let%bind controller = require_recurrence_controller t in
  let%bind () =
    if Recurrence_controller.same_store controller observation.controller
    then Ok ()
    else Error (error Conflict "recurrence import requires the actual same Store")
  in
  let%bind () = observation.check_current () in
  let%bind origin, terminal = recurrence_import_evidence t observation in
  let%bind admission =
    Recurrence_settlement_admission.prepare
      t.state
      ~controller
      ~actor
      ~authorize
      ~automatic_policy
      ~origin
      ~terminal
  in
  let check_current admission =
    let%bind () = check_recurrence_import_owner t in
    let%bind () = observation.check_current () in
    let%bind () = Recurrence_settlement_admission.check_current admission t.state in
    (* Trusted nonyielding policies can still revoke either original proof.
       After every policy callback, validate both owners without more callbacks. *)
    let%bind () = observation.check_basis () in
    let%bind () = Recurrence_settlement_admission.check_basis admission t.state in
    check_recurrence_import_owner t
  in
  let%bind () = check_current admission in
  match Recurrence_settlement_admission.settlement admission with
  | (Retained_exact _ | Detail_not_retained _) as settlement ->
    Ok Recurrence_import.{ settlement; timing = No_successor }
  | Imported _ ->
    let%bind candidate = prepare_recurrence_import_candidate t observation admission in
    let%bind () = check_current candidate.admission in
    let settlement = Recurrence_settlement_admission.settlement candidate.admission in
    let%bind () = persist_transition t candidate.transition in
    install_committed_transition t candidate.transition;
    Option.iter
      candidate.phase
      ~f:(Recurrence_runtime.install_completion_phase t.recurrence_runtime);
    publish_committed_transition t candidate.transition;
    (* Own ACK rotates Current, including a self-target's target observation.
       Refresh controller visibility only; acknowledged facts need no requery. *)
    let%bind current = Recurrence_controller.observe controller t.state in
    let%bind () =
      Recurrence_command_admission.check_write
        actor
        ~authorize:(fun state ->
          authorize state (Recurrence_settlement_admission.definition candidate.admission))
        t.state
    in
    let%bind () = check_recurrence_import_owner t in
    if Recurrence_controller.is_current current
    then Ok Recurrence_import.{ settlement; timing = candidate.timing }
    else Error (error Conflict "settlement controller visibility changed after ACK")
;;

let check_recurrence_activation_internal
      t
      custody
      (observation : Recurrence_run_observation.t)
  =
  let open Result.Let_syntax in
  let%bind controller = require_recurrence_controller t in
  let owner () =
    let%bind () = check_recurrence_import_owner t in
    require_runtime_admission t
  in
  let%bind () = owner () in
  (* Reject foreign, closed or stale custody before its policy sees this state. *)
  let%bind () =
    Activation_custody.check_basis custody ~owner:t.recurrence_activation_owner t.state
  in
  let%bind () = Activation_custody.check_policy custody t.state in
  let%bind () =
    if
      Recurrence_controller.same_store controller observation.controller
      && Recurrence_run_origin.equal
           (Activation_custody.origin custody)
           observation.origin
    then Ok ()
    else
      Error (error Conflict "activation requires the exact same-Store target observation")
  in
  let%bind () = observation.check_current () in
  let%bind current = Recurrence_controller.observe controller t.state in
  let check_basis () =
    let%bind () = owner () in
    let%bind () =
      Activation_custody.check_basis custody ~owner:t.recurrence_activation_owner t.state
    in
    let%bind () = observation.check_basis () in
    let%bind () = observation.check_admitted () in
    let definition = Activation_custody.definition custody in
    let%bind () =
      if
        Recurrence_controller.is_current current
        && Recurrence_controller.automatic current
        && Agent_protocol.Session_lifecycle.Revision.equal
             (Recurrence_controller.revision current)
             (Recurrence_definition.birth_lifecycle_revision definition)
      then Ok ()
      else Error (error Conflict "activation controller lifecycle observation changed")
    in
    let%bind entry = Activation_custody.entry custody t.state in
    let%bind snapshot =
      Recurrence_runtime.snapshot
        t.recurrence_runtime
        ~wall:(t.services.now ())
        ~monotonic:(t.services.monotonic_now ())
    in
    let%bind late =
      Recurrence_runtime.selection_lateness
        t.recurrence_runtime
        ~entry
        ~nominal:(Activation_custody.nominal custody)
        ~snapshot
    in
    let%bind () =
      match late with
      | Some late
        when Int64.(
               late
               <= Recurrence_policy.grace_ns (Recurrence_definition.policy definition)) ->
        Ok ()
      | None | Some _ ->
        Error (error Conflict "activation nominal is outside current grace")
    in
    owner ()
  in
  let check_current () =
    (* A returned view may outlive its cell. Never invoke its stored policy then. *)
    let%bind () = check_basis () in
    let%bind () = Activation_custody.check_policy custody t.state in
    let%bind () = observation.check_current () in
    check_basis ()
  in
  let%map () = check_current () in
  let checked : Recurrence_activation.Checked.t = { check_current; check_basis } in
  checked
;;

let acquire_recurrence_activation_internal t ~actor ~authorize ~observation =
  let open Result.Let_syntax in
  let%bind custody =
    Activation_custody.create
      ~owner:t.recurrence_activation_owner
      ~state:t.state
      ~actor
      ~authorize
      ~origin:observation.Recurrence_run_observation.origin
  in
  let%bind _checked = check_recurrence_activation_internal t custody observation in
  (* One live cell per actual retained current execution; no independent quota. *)
  List.iter t.recurrence_activations ~f:(fun old ->
    if Activation_custody.same_occurrence old custody then Activation_custody.close old);
  t.recurrence_activations
  <- custody :: List.filter t.recurrence_activations ~f:Activation_custody.is_live;
  Ok custody
;;

let observe_recurrence_child t reservation =
  let open Result.Let_syntax in
  let%bind () = require_runtime_admission t in
  let%bind controller = require_recurrence_controller t in
  let%bind reference =
    Result.of_option
      t.state.spec.delegation
      ~error:(error Invalid_state "captured child has no delegation reference")
  in
  let%bind record = Recurrence_controller.resolve_delegation controller reference in
  let%bind () =
    match record.stage, record.revocation with
    | (Child_installed | Linked), None -> Ok ()
    | (Reserved | Artifact_installed), None | _, Some _ ->
      Error (error Conflict "captured child has no current installed delegation")
  in
  let%bind origin = Recurrence_child_origin.of_record record in
  let%bind () =
    if
      Recurrence_child_reservation.equal
        reservation
        (Recurrence_child_origin.reservation origin)
    then Ok ()
    else Error (error Conflict "captured child belongs to another occurrence")
  in
  let%bind () = Recurrence_child_origin.validate_initial_state origin t.state in
  let%bind current = Recurrence_controller.observe controller t.state in
  let basis_revision = t.state.counters.revision in
  let check_current () =
    let%bind () = require_runtime_admission t in
    let%bind () =
      if
        Option.is_none (Eio.Promise.peek t.finished)
        && (not t.stopped)
        && Recurrence_controller.is_current current
        && Recurrence_controller.automatic current
        && Int64.equal basis_revision t.state.counters.revision
      then Ok ()
      else Error (error Conflict "captured child observation is no longer current")
    in
    Recurrence_child_origin.validate_initial_state origin t.state
  in
  let%bind () = check_current () in
  let%map child_association =
    Recurrence_child_association.create ~reservation ~reference
  in
  let proof : Recurrence_child_link.t =
    { association = child_association; controller; check_current }
  in
  proof
;;

(* Nonyielding owner-epoch retirement, shared by orderly Shutdown and exceptional
   owner exit. This does not persist, invoke callbacks, cancel/join work or claim
   rollback of already committed effects. *)
let retire_execution_custody t =
  close_recurrence_activations t;
  Legacy_queue_custody.retire t.legacy_queue_custody;
  t.legacy_queue_custody <- Legacy_queue_custody.empty;
  Legacy_schedule_custody.retire t.legacy_schedule_custody;
  t.legacy_schedule_custody <- Legacy_schedule_custody.empty;
  Option.iter t.legacy_callback_scope ~f:(fun scope ->
    match scope.Callback_scope.custody with
    | Unavailable -> ()
    | Admitted admission -> retire_agent_admission admission);
  t.legacy_callback_scope <- None;
  t.moderator_manager <- None;
  retire_instruction_continuations t;
  Option.iter t.operation_agent_custody ~f:(fun (_, admission) ->
    retire_agent_admission admission);
  List.iter t.pending_agent_custody ~f:(fun (_, custody) -> retire_agent_custody custody);
  Job_custody.retire t.job_agent_custody;
  Option.iter t.initialization_scope ~f:(fun scope ->
    retire_agent_admission scope.agent_admission);
  List.iter t.invocation_executions ~f:(fun execution ->
    Atomic.set execution.authority_live false);
  List.iter t.job_scopes ~f:(fun scope -> Atomic.set scope.authority_live false);
  Option.iter t.moderator_borrow ~f:(fun borrow ->
    Atomic.set borrow.authority_live false;
    Option.iter borrow.observation ~f:Observation_continuation.retire);
  Option.iter t.queued_event_borrow ~f:(fun borrow ->
    Option.iter
      (Queued_agent_admission.admission borrow.agent_admission)
      ~f:retire_agent_admission;
    borrow.callback_active <- false;
    Atomic.set borrow.authority_live false);
  close_run_preparation t;
  t.initialization_scope <- None
;;

let handle : type a. t -> a request -> (a, Agent_protocol.Error.t) result =
  fun t request ->
  if Option.is_some t.lifecycle_fence && not (lifecycle_request_allowed request)
  then Error (error Conflict "session lifecycle is reserved")
  else (
    match request with
    | Begin_lifecycle (attachment_id, generation, revision) ->
      begin_lifecycle t attachment_id generation revision
    | Lifecycle_state fence ->
      Result.map (check_lifecycle_fence t fence) ~f:(fun () -> t.state)
    | Retire_lifecycle fence ->
      Result.map (check_lifecycle_fence t fence) ~f:(fun () -> fence.phase <- Retiring)
    | Abort_lifecycle fence ->
      let open Result.Let_syntax in
      let%bind () = check_lifecycle_fence t fence in
      (match fence.phase with
       | Retiring -> Error (error Conflict "retired lifecycle owner cannot reopen")
       | Reserved ->
         t.lifecycle_fence <- None;
         Ok ())
    | Manage_moderator_follow_up (operation_id, observer) ->
      manage_moderator_follow_up t operation_id observer
    | Admit_moderator_turn operation_id -> admit_moderator_turn t operation_id
    | Admit_notification_turn operation_id -> admit_notification_turn t operation_id
    | Claim_delegated_event (id, delegation, snapshot, event) ->
      claim_delegated_event t id delegation snapshot event
    | Claim_job_event (scope, id, snapshot, event) ->
      claim_job_event t scope id snapshot event
    | Claim_job_moderator (scope, invocation) -> claim_job_moderator t scope invocation
    | Claim_ordinary_event (id, operation_id, snapshot, event) ->
      claim_ordinary_event t id operation_id snapshot event
    | Claim_queued_event (id, operation_id, snapshot) ->
      claim_queued_event t id operation_id snapshot
    | Claim_legacy_callback (callback, operation_id) ->
      claim_legacy_callback t callback operation_id
    | Claim_initialization_callback (scope, manager, callback) ->
      claim_initialization_callback t scope manager callback
    | Retire_initialization_resource (scope, manager) ->
      retire_initialization_resource t scope manager
    | Claim_queued_retirement (id, snapshot, reason) ->
      claim_queued_retirement t id snapshot reason
    | Commit_queued_event (borrow, snapshot, requests, decision, notifications) ->
      let open Result.Let_syntax in
      let%bind () = validate_queued_event_borrow t borrow in
      with_staged_transaction t (Moderator_event borrow.receipt.context.id) (fun () ->
        commit_queued_event t borrow snapshot requests decision notifications)
    | Reconcile_run_recovery ->
      let open Result.Let_syntax in
      let%bind delta =
        Goal_catalog_source.recovery_delta t.state ~now:(t.services.now ())
      in
      (match delta with
       | None -> Ok ()
       | Some delta -> transition t ~delta ~payloads:[] |> Result.map ~f:ignore)
    | Run_actions executing -> run_actions_internal t executing
    | Finish_queued_event (borrow, interrupted) ->
      finish_queued_event t borrow interrupted
    | Set_queued_event_cancel (borrow, cancel) ->
      Result.map (queued_event_can_commit t borrow) ~f:(fun () ->
        borrow.cancel <- Some cancel)
    | Commit_invocation_call (operation_id, invocation, entry) ->
      commit_invocation_call t operation_id invocation entry
    | Claim_invocation (operation_id, invocation) ->
      claim_invocation t operation_id invocation
    | Claim_idle_invocation (borrow, invocation) ->
      claim_idle_invocation t borrow invocation
    | Claim_event_invocation (borrow, invocation) ->
      claim_event_invocation t borrow invocation
    | Claim_job_scope (id, generation, attempt, deadline) ->
      claim_job_scope t id generation attempt deadline
    | Claim_job_invocation (scope, invocation) ->
      (match t.moderator_borrow with
       | Some borrow
         when Option.exists borrow.job_scope ~f:(phys_equal scope)
              && Option.exists invocation.context.parent_invocation ~f:(fun parent ->
                Agent_protocol.Id.Invocation.equal parent borrow.invocation.context.id
                || List.exists t.invocation_executions ~f:(fun execution ->
                  invocation_execution_owned_by borrow execution
                  && Agent_protocol.Id.Invocation.equal
                       parent
                       execution.dispatched.context.id)) ->
         claim_idle_invocation t borrow invocation
       | _ -> claim_job_invocation t scope invocation)
    | Finish_job_scope scope -> finish_job_scope t scope
    | Seal_job_scope scope ->
      (match List.mem t.job_scopes scope ~equal:phys_equal with
       | false -> Error (error Conflict "background execution scope has ended")
       | true ->
         scope.active <- false;
         Atomic.set scope.authority_live false;
         scope.unfinished_on_return
         <- job_has_moderator t scope
            || List.exists t.invocation_executions ~f:(job_execution_owned_by scope);
         signal_job_scope scope;
         cancel_job_moderators t scope;
         Ok ())
    | Set_job_scope_cancel (scope, cancel) ->
      let open Result.Let_syntax in
      let%map () = job_scope_can_execute t scope in
      scope.cancel <- Some cancel
    | Finish_invocation (execution, outcome, requests, commit_starts, authoring_reference)
      ->
      let open Result.Let_syntax in
      let%bind () =
        match List.mem t.invocation_executions execution ~equal:phys_equal with
        | true -> Ok ()
        | false -> Error (error Conflict "invocation callback no longer owns its result")
      in
      with_staged_transaction t (Invocation execution.dispatched.context.id) (fun () ->
        finish_invocation t execution outcome requests commit_starts authoring_reference)
    | Claim_moderator_invocation (operation_id, invocation) ->
      claim_moderator_invocation t operation_id invocation
    | Claim_moderator_observation (operation_id, invocation_id) ->
      claim_moderator_observation t (Some operation_id) invocation_id
    | Claim_next_moderator_observation (operation_id, observer) ->
      claim_next_moderator_observation t (Some operation_id) observer
    | Claim_idle_moderator_observation (observer, tools) ->
      if idle_moderator_eligible t
      then
        Result.map (claim_next_moderator_observation t None observer) ~f:(fun borrow ->
          Option.iter borrow ~f:(fun borrow -> borrow.accepts_children <- tools);
          borrow)
      else Ok None
    | Commit_moderator_invocation (borrow, resolved, snapshot) ->
      let open Result.Let_syntax in
      let%bind () = validate_moderator_borrow t borrow in
      with_staged_transaction t (Invocation borrow.invocation.context.id) (fun () ->
        commit_moderator_invocation t borrow resolved snapshot)
    | Finish_moderator_invocation (borrow, failure) ->
      finish_moderator_invocation t borrow failure
    | Set_idle_moderator_cancel (borrow, cancel) ->
      let open Result.Let_syntax in
      let%bind () = validate_moderator_borrow t borrow in
      (match
         ( borrow.job_scope
         , borrow.operation_id
         , t.state.lifecycle.desired
         , t.state.lifecycle.observed )
       with
       | Some scope, None, _, _ ->
         let%map () = job_scope_can_execute t scope in
         borrow.cancel <- Some cancel
       | None, None, Running, Idle ->
         borrow.cancel <- Some cancel;
         Ok ()
       | _ ->
         Error (error Conflict "idle observation was stopped before callback execution"))
    | Commit_extensions (generation, revision, changes) ->
      commit_extensions_internal t generation revision changes
    | State -> Ok t.state
    | Prepare_ingress_submission
        (source, producer, registration_id, namespace, key, payload) ->
      prepare_ingress_submission_internal
        t
        source
        producer
        registration_id
        namespace
        key
        payload
    | Commit_ingress_submission (proposal, before, snapshot) ->
      commit_ingress_submission_internal t proposal before snapshot
    | Due_schedules -> due_schedules t
    | Read_job id -> Result.map (find_job t id) ~f:(job_with_progress t)
    | Publish_job_progress (id, progress) ->
      publish_job_progress_internal t id progress;
      Ok ()
    | Prepare_background_job (owner, request) -> prepare_background_job t owner request
    | Stage_background_job (job, capacity) -> stage_background_job_internal t job capacity
    | Stage_subscription_mutation (owner, source, previous, next) ->
      stage_subscription_mutation_internal t owner source previous next
    | Create_script_subscription
        (owner, source, kind, lifetime_ms, wake, completion_schema) ->
      create_script_subscription_internal
        t
        owner
        source
        kind
        lifetime_ms
        wake
        completion_schema
    | Select_subscription_mutations (owner, source, receipts) ->
      let open Result.Let_syntax in
      let%bind _ = subscription_owner_active t owner source in
      Staged_subscriptions.select
        t.staged_subscriptions
        ~owner
        ~receipts
        ~lookup:(lookup_subscription t)
    | Abort_subscription_mutation (owner, receipt) ->
      let open Result.Let_syntax in
      let%map () = Staged_subscriptions.abort t.staged_subscriptions ~owner ~receipt in
      sync_extension_clock t
    | Finish_script_subscription (owner, source, id, expected_epoch, completion) ->
      finish_script_subscription_internal t owner source id expected_epoch completion
    | Read_script_subscription (owner, source, id) ->
      let open Result.Let_syntax in
      let%bind _ = subscription_owner_active t owner source in
      let%bind subscription =
        Result.of_option
          (provisional_subscription t owner id)
          ~error:(error Invalid_state "subscription is not retained")
      in
      let%map () = subscription_owned t source subscription in
      subscription
    | Expire_subscriptions -> expire_subscriptions_internal t
    | Create_script_ingress (owner, source, id, epoch, namespace, schema) ->
      create_script_ingress_internal t owner source id epoch namespace schema
    | Revoke_script_ingress (owner, source, id, reason) ->
      revoke_script_ingress_internal t owner source id reason
    | Read_script_ingress (owner, source, id) ->
      read_script_ingress_internal t owner source id
    | Abort_ingress_mutation (owner, receipt) ->
      Staged_ingress.abort t.staged_ingress ~owner ~receipt
    | Select_ingress_mutations (owner, source, receipts) ->
      let open Result.Let_syntax in
      let%bind _ = subscription_owner_active t owner source in
      Staged_ingress.select
        t.staged_ingress
        ~owner
        ~receipts
        ~lookup:(lookup_ingress t)
        ~subscription:
          (ingress_subscription_lookup
             t
             (Staged_subscriptions.values t.staged_subscriptions))
    | Create_script_notification
        (owner, source, correlation, completion, wake, disclosure_pins) ->
      create_script_notification_internal
        t
        owner
        source
        correlation
        completion
        wake
        disclosure_pins
    | Read_script_notification (owner, source, id) ->
      let open Result.Let_syntax in
      let%bind _ = subscription_owner_active t owner source in
      let%bind value =
        Result.of_option
          (provisional_notification t owner id)
          ~error:(error Invalid_state "notification is not retained")
      in
      let%map () = notification_owned t source value in
      value
    | Select_notification_mutations (owner, source, receipts) ->
      let open Result.Let_syntax in
      let%bind _ = subscription_owner_active t owner source in
      Staged_notifications.select
        t.staged_notifications
        ~owner
        ~receipts
        ~lookup:(lookup_notification t)
    | Abort_notification_mutation (owner, receipt) ->
      Staged_notifications.abort t.staged_notifications ~owner ~receipt
    | Stage_schedule_mutation (owner, source, previous, next) ->
      stage_schedule_mutation_internal t owner source previous next
    | Create_script_schedule (owner, source, delay_ms, payload, misfire) ->
      create_script_schedule_internal t owner source delay_ms payload misfire
    | Select_schedule_mutations (owner, source, receipts) ->
      let open Result.Let_syntax in
      let%bind _ = subscription_owner_active t owner source in
      Staged_schedules.select t.staged_schedules ~owner ~receipts ~lookup:(fun id ->
        List.find t.state.schedules ~f:(fun value ->
          Agent_protocol.Id.Schedule.equal value.id id))
    | Abort_schedule_mutation (owner, receipt) ->
      let open Result.Let_syntax in
      let%map () = Staged_schedules.abort t.staged_schedules ~owner ~receipt in
      sync_extension_clock t
    | Read_script_schedule (owner, source, id) ->
      let open Result.Let_syntax in
      let%bind _ = subscription_owner_active t owner source in
      let%bind value =
        Result.of_option
          (provisional_schedule t owner id)
          ~error:(error Invalid_state "schedule is not retained")
      in
      let%map () = schedule_owned t source value in
      value
    | Select_background_jobs (owner, ids) ->
      let open Result.Let_syntax in
      let%bind () = background_owner_active t owner in
      Staged_jobs.select t.staged_jobs ~owner ~ids
    | Abort_background_job (owner, id) -> Staged_jobs.abort t.staged_jobs ~owner ~id
    | Has_staged_background_job (owner, id) ->
      let open Result.Let_syntax in
      let%map () = background_owner_active t owner in
      Staged_jobs.contains t.staged_jobs ~owner ~id
    | Read_script_job (owner, id) ->
      let open Result.Let_syntax in
      let%bind () = background_owner_active t owner in
      let%bind staged = Staged_jobs.find t.staged_jobs ~owner ~id in
      (match staged with
       | Some job -> Ok job
       | None ->
         let%bind job = find_job t id in
         let%map () = validate_job_generation t job t.state.identity.generation in
         job_with_progress t job)
    | Read_script_job_result (owner, expected) ->
      let open Result.Let_syntax in
      let%bind () = background_owner_active t owner in
      let%bind job = find_job t expected.id in
      let%bind () = validate_job_generation t job t.state.identity.generation in
      let%bind () =
        match
          Jsonaf.exactly_equal
            (Agent_protocol.Job.to_json job)
            (Agent_protocol.Job.to_json expected)
        with
        | true -> Ok ()
        | false -> Error (error Conflict "job changed before artifact materialization")
      in
      let load_artifact =
        Option.map t.services.job_results ~f:(fun publisher ->
          Agent_store.Job_result_store.Publisher.load publisher)
      in
      let%bind completion = Agent_protocol.Job.terminal_completion ?load_artifact job in
      Result.of_option
        completion
        ~error:(error Invalid_state "job has no terminal completion")
    | Cancel_script_job (owner, id) ->
      let open Result.Let_syntax in
      let%bind () = background_owner_active t owner in
      let%bind staged =
        Staged_jobs.cancel t.staged_jobs ~owner ~id ~now:(t.services.now ())
      in
      (match staged with
       | Some _ -> Ok ()
       | None ->
         let%bind job = find_job t id in
         let%bind () = validate_job_generation t job t.state.identity.generation in
         Result.map (cancel_job_internal t id) ~f:ignore)
    | Configuration -> configuration_view t
    | Set_organization_admission admission ->
      t.organization_admission <- Some admission;
      Ok ()
    | Set_configuration_policy policy ->
      t.configuration_policy <- Some policy;
      Ok ()
    | Prepare_configuration_update request -> prepare_configuration_update t request
    | Commit_configuration_update validated -> commit_configuration_update t validated
    | Begin_configuration_capture operation_id ->
      begin_configuration_capture t operation_id
    | Mark_configuration_capture (capture, configuration) ->
      mark_configuration_capture t capture configuration
    | Finish_configuration_capture (capture, success) ->
      finish_configuration_capture t capture success
    | Snapshot -> Ok (current_snapshot t)
    | Observe -> Ok (t.state, current_snapshot t)
    | Set_operation_worker worker -> set_operation_worker t worker
    | Set_runtime_worker (original_actor, worker, inference, moderator_manager) ->
      let open Result.Let_syntax in
      let%bind () =
        match original_actor with
        | None -> Ok ()
        | Some actor when Operator_authorization.is_current actor -> Ok ()
        | Some _ ->
          Error (error Permission_denied "runtime installation authentication expired")
      in
      set_runtime_worker t worker inference moderator_manager
    | Check_runtime_admission -> require_runtime_admission t
    | Retire_runtime_worker closing -> retire_runtime_worker t ~closing
    | Set_compaction_inference port ->
      t.compaction_inference <- port;
      Ok ()
    | Enable_automatic_turn_budget (original_actor, policy) ->
      let open Result.Let_syntax in
      let%bind () =
        match original_actor with
        | None -> Ok ()
        | Some actor when Operator_authorization.is_current actor -> Ok ()
        | Some _ -> Error (error Permission_denied "turn budget authentication expired")
      in
      enable_automatic_turn_budget t policy
    | Enable_run_constructor_turn_budget (preparation, policy) ->
      enable_automatic_turn_budget ~run_preparation:preparation t policy
    | Set_automatic_turn_pauses conditions ->
      (match
         t.state.active_operation, moderator_is_borrowed t, t.idle_moderator_borrowed
       with
       | None, false, false ->
         let open Result.Let_syntax in
         let%bind budget =
           t.state.automatic_turn_budget
           |> Result.of_option
                ~error:(error Invalid_state "automatic-turn policy is not enabled")
         in
         let next = Automatic_turn_budget.with_pauses budget conditions in
         (match Automatic_turn_budget.equal budget next with
          | true -> Ok ()
          | false ->
            transition
              t
              ~delta:
                (Session_delta.Automatic_turn_pauses_changed
                   next.policy.budget.pause_conditions)
              ~payloads:[]
            |> Result.map ~f:ignore)
       | _ ->
         Error
           (error Conflict "pause changes wait for active moderator or foreground work"))
    | Change_moderator (original_actor, moderator) ->
      let open Result.Let_syntax in
      let%bind () =
        match original_actor with
        | None -> Ok ()
        | Some actor when Operator_authorization.is_current actor -> Ok ()
        | Some _ ->
          Error (error Permission_denied "moderator checkpoint authentication expired")
      in
      change_moderator t moderator
    | Change_workspace workspace -> change_workspace t workspace
    | Shell_approval_grants -> Ok t.state.shell.approval_grants
    | Replace_shell_approval_grants (run_preparation, approval_grants) ->
      replace_shell_approval_grants ?run_preparation t approval_grants
    | Replace_shell_extension_snapshots (run_preparation, extension_snapshots) ->
      replace_shell_extension_snapshots ?run_preparation t extension_snapshots
    | Shell_manifest_grants -> Ok t.state.shell.manifest_grants
    | Add_shell_manifest_grant (run_preparation, grant) ->
      add_shell_manifest_grant ?run_preparation t grant
    | Reset (attachment_id, expected_revision, options) ->
      reset_internal t attachment_id expected_revision options
    | Upgrade_prompt (attachment_id, expected_revision, target_revision) ->
      upgrade_prompt_internal t attachment_id expected_revision target_revision
    | Commit_administration (attachment_id, expected_revision, kind, state) ->
      commit_administration t attachment_id expected_revision kind state
    | Validate_administration_basis (attachment_id, expected) ->
      validate_administration_basis t attachment_id expected
    | Commit_reconciled_administration (attachment_id, expected, kind, candidate) ->
      commit_reconciled_administration t attachment_id expected kind candidate
    | Begin_initialization (purpose, original_actor, expected) ->
      let open Result.Let_syntax in
      let%bind () =
        match original_actor with
        | None -> Ok ()
        | Some actor ->
          if Operator_authorization.is_current actor
          then Ok ()
          else Error (error Permission_denied "initialization authentication expired")
      in
      begin_initialization ?original_actor ~purpose t expected
    | Borrow_initialization_agent_actor scope ->
      let open Result.Let_syntax in
      let%bind () = validate_initialization_scope t scope in
      let%bind custody = custody_of_admission (Some scope.agent_admission) in
      (match custody with
       | None -> Ok None
       | Some custody -> Result.map (Execution_custody.actor custody) ~f:Option.some)
    | Borrow_operation_agent_actor id ->
      let open Result.Let_syntax in
      let%bind _ = running_operation t id in
      operation_agent_actor t id
    | Borrow_compaction_agent_actor operation ->
      let open Result.Let_syntax in
      let%bind _ = running_compaction t operation in
      operation_agent_actor t operation.id
    | Borrow_invocation_agent_actor (execution, invocation) ->
      invocation_agent_actor t execution invocation
    | Borrow_moderator_agent_actor (borrow, invocation) ->
      let open Result.Let_syntax in
      let%bind () = validate_moderator_borrow t borrow in
      let%bind () =
        if
          Agent_protocol.Invocation.equal borrow.invocation invocation
          && Atomic.get borrow.authority_live
        then Ok ()
        else
          Error
            (error
               Permission_denied
               "agent authority belongs to another or expired moderator")
      in
      let%bind custody = moderator_agent_custody t borrow in
      (match custody with
       | None -> Ok None
       | Some custody ->
         Result.map (Execution_custody.actor custody) ~f:(fun actor ->
           Some
             (Operator_authorization.guarded
                ~principal:(Operator_authorization.principal actor)
                ~is_current:(fun () ->
                  Atomic.get borrow.authority_live
                  && Operator_authorization.is_current actor))))
    | Begin_run_initialization (purpose, preparation, expected) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      begin_initialization
        ~run_preparation:preparation
        ?original_actor:(Run_preparation.original_actor preparation)
        ~purpose
        t
        expected
    | End_initialization scope -> end_initialization t scope
    | Complete_initialization (scope, candidate, moderator_manager) ->
      complete_initialization t scope candidate moderator_manager
    | Checkpoint_runtime_construction (scope, moderator, moderator_manager) ->
      checkpoint_runtime_construction t scope moderator moderator_manager
    | Fail_initialization (scope, failure) -> fail_initialization t scope failure
    | Start (original_actor, attachment_id, expected_parent_stop_epoch) ->
      let open Result.Let_syntax in
      let%bind () =
        match original_actor with
        | None -> Ok ()
        | Some actor when Operator_authorization.is_current actor -> Ok ()
        | Some _ -> Error (error Permission_denied "start authentication expired")
      in
      with_writer t attachment_id (fun () ->
        let%bind source = Runtime_builder.moderator_snapshot_observer t.state.moderator in
        let%map summary = start_internal ?expected_parent_stop_epoch t in
        (match t.lifecycle_agent_custody with
         | Admitted _ -> ()
         | Unadmitted ->
           t.lifecycle_agent_custody
           <- Admitted
                { generation = t.state.identity.generation
                ; source
                ; pending =
                    Pending
                      (create_agent_admission
                         (Option.map original_actor ~f:create_agent_custody))
                });
        summary)
    | Start_initial_delegated (reference, expected_parent_stop_epoch) ->
      (match t.state.spec.delegation with
       | Some current
         when Agent_store.Delegation_store.Reference.equal current reference
              && Agent_protocol.Id.Session.equal
                   reference.child_session_id
                   t.state.identity.session_id ->
         (match t.state.pending_initial_start with
          | true -> start_internal ?expected_parent_stop_epoch t
          | false -> Ok (Session_state.summary t.state))
       | _ ->
         Error
           (error Permission_denied "delegation.start: child relationship does not match"))
    | Fail_initial_delegated (reference, failure) ->
      (match t.state.spec.delegation with
       | Some current
         when Agent_store.Delegation_store.Reference.equal current reference
              && Agent_protocol.Id.Session.equal
                   reference.child_session_id
                   t.state.identity.session_id ->
         (match t.state.pending_initial_start with
          | false -> Ok (Session_state.summary t.state)
          | true ->
            let open Result.Let_syntax in
            let%map _ =
              transition
                t
                ~delta:
                  (Batch
                     [ Initial_start_consumed
                     ; Lifecycle_changed { desired = Stopped; observed = Failed failure }
                     ; Failure_changed (Some failure)
                     ])
                ~payloads:
                  [ Session_state_changed
                      { desired_state = Stopped; observed_state = Failed failure }
                  ]
            in
            Session_state.summary t.state)
       | _ ->
         Error
           (error Permission_denied "delegation.start: child relationship does not match"))
    | Queue_start attachment_id ->
      with_writer t attachment_id (fun () -> queue_start_internal t)
    | Activate_queued_start -> activate_queued_start t
    | Update_organization (principal, request) ->
      with_writer t request.attachment_id (fun () ->
        let open Result.Let_syntax in
        let%bind admission =
          Result.of_option
            t.organization_admission
            ~error:(error Invalid_state "organization admission is not configured")
        in
        let%bind plan =
          Session_organization_transition.create
            t.state
            ~expected_metadata_revision:request.expected_metadata_revision
            ~patch:request.patch
        in
        let delta = Session_organization_transition.delta plan in
        let%bind candidate = Session_delta.apply t.state delta in
        let payloads =
          if Session_organization_transition.changed plan
          then
            [ Agent_protocol.Event.Durable.Payload.Session_updated
                (Session_state.summary candidate)
            ]
          else []
        in
        let%bind transition = prepare_transition t ~delta ~payloads in
        let%bind () =
          Eio.Cancel.protect (fun () ->
            let%map () =
              Session_organization_admission.persist
                admission
                ~principal
                ~host_id:request.host_id
                ~additions:(Session_organization_transition.additions plan)
                ~commit:(fun () -> persist_transition t transition)
            in
            install_committed_transition t transition;
            publish_committed_transition t transition)
        in
        Ok (Session_state.summary t.state))
    | Set_recurrence_controller controller ->
      if
        not
          (Agent_protocol.Id.Session.equal
             (Agent_protocol.Session_ref.session_id
                (Recurrence_controller.session controller))
             t.state.identity.session_id)
      then Error (error Invalid_request "recurrence controller belongs to another actor")
      else (
        match t.recurrence_controller with
        | Some _ -> Error (error Conflict "recurrence controller is already installed")
        | None ->
          t.recurrence_controller <- Some controller;
          Ok ())
    | Reserve_recurrence_child (actor, authorize, occurrence, expected_control_revision)
      ->
      let open Result.Let_syntax in
      let%bind () = require_runtime_admission t in
      let%bind controller = require_recurrence_controller t in
      let prepare () =
        Recurrence_creation_admission.prepare
          t.state
          ~controller
          ~actor
          ~authorize
          ~occurrence
          ~expected_control_revision
      in
      let%bind prepared = prepare () in
      let%bind () = check_recurrence_creation_admission t prepared in
      let catalog =
        Option.value t.state.recurrence_catalog ~default:Recurrence_catalog.empty
      in
      let child_session_id, transaction_id =
        match Recurrence_creation_admission.reservation prepared with
        | Some row ->
          ( Recurrence_child_reservation.child_session_id row
          , Recurrence_child_reservation.transaction_id row )
        | None ->
          Agent_protocol.Id.Session.create (), Agent_protocol.Id.Transaction.create ()
      in
      let%bind next, reservation =
        Recurrence_catalog.reserve_child
          catalog
          ~recurrence_id:
            (Recurrence_entry.id (Recurrence_creation_admission.entry prepared))
          ~principal_id:(Operator_authorization.principal actor).id
          ~child_session_id
          ~transaction_id
          ~occurrence
      in
      let%bind () =
        if
          Int64.equal
            (Recurrence_catalog.revision next)
            (Recurrence_catalog.revision catalog)
        then Ok ()
        else (
          let%bind candidate =
            prepare_transition
              t
              ~delta:(Session_delta.Recurrence_catalog_changed next)
              ~payloads:[]
          in
          let%bind () = check_recurrence_creation_admission t prepared in
          let%bind () = persist_transition t candidate in
          install_committed_transition t candidate;
          publish_committed_transition t candidate;
          Ok ())
      in
      let%bind refreshed = prepare () in
      let%bind () = check_recurrence_creation_admission t refreshed in
      let%bind () =
        match Recurrence_creation_admission.reservation refreshed with
        | Some row when Recurrence_child_reservation.equal row reservation -> Ok ()
        | None | Some _ ->
          Error (error Conflict "committed creation reservation changed before permit")
      in
      let permit : Recurrence_creation.t =
        { owner = t.recurrence_creation_owner; actor; authorize; reservation }
      in
      Ok permit
    | Check_recurrence_creation permit -> refresh_recurrence_creation t permit
    | Record_recurrence_execution (actor, authorize, intent, attempt) ->
      record_recurrence_execution_internal t ~actor ~authorize ~intent ~attempt
    | Observe_recurrence_run (actor, authorize, intent, attempt) ->
      observe_recurrence_run_internal t ~actor ~authorize ~intent ~attempt
    | Import_recurrence_execution (actor, authorize, automatic_policy, observation) ->
      import_recurrence_execution_internal
        t
        ~actor
        ~authorize
        ~automatic_policy
        ~observation
    | Acquire_recurrence_activation (actor, authorize, observation) ->
      acquire_recurrence_activation_internal t ~actor ~authorize ~observation
    | Check_recurrence_activation (custody, observation) ->
      check_recurrence_activation_internal t custody observation
    | Release_recurrence_activation custody ->
      if Activation_custody.belongs_to custody ~owner:t.recurrence_activation_owner
      then (
        Activation_custody.close custody;
        t.recurrence_activations
        <- List.filter t.recurrence_activations ~f:Activation_custody.is_live;
        Ok ())
      else Error (error Conflict "activation custody belongs to another actor")
    | Verify_recurrence_child reservation -> observe_recurrence_child t reservation
    | Link_recurrence_child (permit, child) ->
      let open Result.Let_syntax in
      let%bind checked = refresh_recurrence_creation t permit in
      let%bind controller = require_recurrence_controller t in
      let%bind () =
        if
          Recurrence_controller.same_store
            controller
            child.Recurrence_child_link.controller
        then Ok ()
        else Error (error Conflict "recurrence child belongs to another Store")
      in
      let association = child.Recurrence_child_link.association in
      let%bind () =
        Recurrence_child_association.validate
          association
          ~reservation:permit.Recurrence_creation.reservation
      in
      let%bind catalog =
        Result.of_option
          t.state.recurrence_catalog
          ~error:(error Conflict "recurrence parent lost its catalog")
      in
      let%bind next = Recurrence_catalog.link_child catalog association in
      let check () =
        let%bind () = Recurrence_creation.Checked.check_current checked in
        let%bind () = child.check_current () in
        Recurrence_creation.Checked.check_current checked
      in
      let%bind () = check () in
      let%bind () =
        if
          Int64.equal
            (Recurrence_catalog.revision next)
            (Recurrence_catalog.revision catalog)
        then Ok ()
        else (
          let%bind transition =
            prepare_transition
              t
              ~delta:(Session_delta.Recurrence_catalog_changed next)
              ~payloads:[]
          in
          let%bind () = check () in
          let%bind () = persist_transition t transition in
          install_committed_transition t transition;
          publish_committed_transition t transition;
          Ok ())
      in
      Ok association
    | Read_recurrences (actor, authorize) ->
      let open Result.Let_syntax in
      let%bind controller = require_recurrence_controller t in
      Recurrence_command_admission.read t.state ~controller ~actor ~authorize
    | Read_recurrence_planning (actor, authorize) ->
      let open Result.Let_syntax in
      let%bind () = Recurrence_command_admission.check_read actor ~authorize t.state in
      let%bind controller = require_recurrence_controller t in
      let%bind planning =
        Recurrence_runtime.status t.recurrence_runtime ~state:t.state ~controller
      in
      (* Status observation may yield. The existing metadata reader repeats the
         original read policy and genuine Current checks before disclosure. *)
      let%map entries =
        Recurrence_command_admission.read t.state ~controller ~actor ~authorize
      in
      Recurrence_planning_read.{ entries; planning }
    | Set_recurrence_planning_host host ->
      let open Result.Let_syntax in
      let%bind (_ : Recurrence_controller.t) = require_recurrence_controller t in
      Recurrence_runtime.set_host t.recurrence_runtime host
    | Recurrence_due_ids ->
      if runtime_admission_open t then Ok (Recurrence_runtime.ids t.state) else Ok []
    | Begin_recurrence_due recurrence_id ->
      let open Result.Let_syntax in
      if not (runtime_admission_open t)
      then Error (error Conflict "recurrence runtime admission is closed")
      else (
        let%bind controller = require_recurrence_controller t in
        let%bind () = reconcile_recurrences_internal t in
        let%bind snapshot =
          Recurrence_runtime.snapshot
            t.recurrence_runtime
            ~wall:(t.services.now ())
            ~monotonic:(t.services.monotonic_now ())
        in
        Recurrence_runtime.begin_due
          t.recurrence_runtime
          ~state:t.state
          ~controller
          ~recurrence_id
          ~snapshot)
    | Finish_recurrence_due prepared ->
      let open Result.Let_syntax in
      if not (runtime_admission_open t)
      then Error (error Conflict "recurrence runtime admission is closed")
      else (
        let%bind controller = require_recurrence_controller t in
        let%bind validation =
          Recurrence_runtime.validate
            t.recurrence_runtime
            prepared
            ~state:t.state
            ~controller
        in
        let%bind candidate =
          match Recurrence_runtime.delta validation with
          | None -> Ok None
          | Some delta ->
            prepare_transition t ~delta ~payloads:[] |> Result.map ~f:Option.some
        in
        let%bind () =
          Recurrence_runtime.check_current t.recurrence_runtime validation ~state:t.state
        in
        let%bind () =
          if runtime_admission_open t
          then Ok ()
          else Error (error Conflict "recurrence runtime admission closed before commit")
        in
        match candidate with
        | None ->
          Recurrence_runtime.install_phase t.recurrence_runtime validation ~state:t.state;
          Ok ()
        | Some candidate ->
          let%bind () = persist_transition t candidate in
          install_committed_transition t candidate;
          Recurrence_runtime.install_phase t.recurrence_runtime validation ~state:t.state;
          publish_committed_transition t candidate;
          Ok ())
    | Release_recurrence_due preparation ->
      Recurrence_runtime.release t.recurrence_runtime preparation;
      Ok ()
    | Apply_recurrence_command (actor, authorize, request) ->
      let open Result.Let_syntax in
      let%bind () = Recurrence_command_admission.check_write actor ~authorize t.state in
      let%bind controller = require_recurrence_controller t in
      (* Reconciliation is an independently durable safety change. If it changes
         CAS, the original command must be retried with fresh expectations. *)
      let%bind () =
        if Recurrence_command.Mutation.needs_source request.mutation
        then reconcile_recurrences_internal t
        else Ok ()
      in
      let%bind prepared =
        Recurrence_command_admission.prepare
          t.state
          ~controller
          ~actor
          ~authorize
          ~request
          ~now:(t.services.now ())
      in
      let%bind () = Recurrence_command_admission.check_write actor ~authorize t.state in
      let%bind () = Recurrence_command_admission.check_current prepared in
      let%map () =
        match prepared.delta with
        | None -> Ok ()
        | Some delta ->
          let%bind candidate = prepare_transition t ~delta ~payloads:[] in
          let previous = t.state in
          let%bind adoption =
            if Recurrence_runtime.needs_adoption ~previous ~current:candidate.state
            then
              Recurrence_runtime.snapshot
                t.recurrence_runtime
                ~wall:(t.services.now ())
                ~monotonic:(t.services.monotonic_now ())
              |> Result.map ~f:Option.some
            else Ok None
          in
          let%bind () =
            Recurrence_command_admission.check_write actor ~authorize t.state
          in
          let%bind () = Recurrence_command_admission.check_current prepared in
          let%bind () = persist_transition t candidate in
          install_committed_transition t candidate;
          Option.iter adoption ~f:(fun snapshot ->
            Recurrence_runtime.adopt_control
              t.recurrence_runtime
              ~previous
              ~current:t.state
              ~snapshot);
          publish_committed_transition t candidate;
          Ok ()
      in
      prepared.receipt
    | Read_goals (actor, authorize) ->
      Goal_command_admission.read t.state ~actor ~authorize
    | Apply_instruction_command (admission, source, request) ->
      let open Result.Let_syntax in
      let%bind decision =
        Instruction_command_admission.prepare
          t.state
          ~admission
          ~source
          ~request
          ~now:(t.services.now ())
      in
      (match decision with
       | Retained receipt -> Ok receipt
       | Prepared { selection; receipt } ->
         let%bind candidate =
           prepare_transition
             t
             ~delta:(Instruction_selection_changed selection)
             ~payloads:[]
         in
         let%bind () =
           Instruction_command_admission.check_now t.state ~admission ~source ~request
         in
         let%map () = install t candidate in
         receipt)
    | Apply_goal_command (actor, authorize, session, request) ->
      let open Result.Let_syntax in
      let%bind prepared =
        Goal_command_admission.prepare
          t.state
          ~actor
          ~authorize
          ~session
          ~request
          ~now:(t.services.now ())
      in
      let%map () =
        match prepared.delta with
        | None -> Ok ()
        | Some delta ->
          let%bind transition = prepare_transition t ~delta ~payloads:[] in
          let%bind () = Goal_command_admission.check_write actor ~authorize t.state in
          let%map () = install t transition in
          (* An explicit Hold is a revocation even when an earlier automatic
             budget hold has exactly the same public value. Retained retries
             and failed persistence never enter this acknowledged fresh branch. *)
          (match request.Goal_command.mutation with
           | Hold (id, _) -> invalidate_goal_inference_allocations t id
           | Create _
           | Revise_objective _
           | Record_progress _
           | Change_budget _
           | Adopt_source _
           | Abandon _ -> ())
      in
      prepared.receipt
    | Update_metadata (attachment_id, expected_metadata_revision, patch) ->
      with_writer t attachment_id (fun () ->
        let open Result.Let_syntax in
        let%bind delta =
          Session_metadata_transition.apply t.state ~expected_metadata_revision ~patch
        in
        match delta with
        | None -> transition t ~delta:(Session_delta.Batch []) ~payloads:[]
        | Some delta ->
          let%bind candidate = Session_delta.apply t.state delta in
          transition
            t
            ~delta
            ~payloads:
              [ Agent_protocol.Event.Durable.Payload.Session_updated
                  (Session_state.summary candidate)
              ])
    | Stop (attachment_id, mode) ->
      with_writer t attachment_id (fun () -> stop_internal t mode)
    | Stop_delegated (reference, mode) ->
      (match t.state.spec.delegation with
       | Some current
         when Agent_store.Delegation_store.Reference.equal current reference
              && Agent_protocol.Id.Session.equal
                   reference.child_session_id
                   t.state.identity.session_id -> stop_internal t mode
       | _ ->
         Error
           (error Permission_denied "delegation.stop: child relationship does not match"))
    | Stop_managed (reference, key, mode, generation, max_receipts) ->
      stop_managed_internal t reference key mode generation max_receipts
    | Append_history (attachment_id, entries) ->
      with_writer t attachment_id (fun () -> append_history t entries)
    | Stop_delegated_at_epoch (reference, epoch, force) ->
      (match t.state.spec.delegation with
       | Some current
         when Agent_store.Delegation_store.Reference.equal current reference
              && Agent_protocol.Id.Session.equal
                   reference.child_session_id
                   t.state.identity.session_id ->
         (match t.state.parent_stop_epoch with
          | Some previous when (not force) && Int64.(epoch <= previous) ->
            Ok (Session_state.summary t.state)
          | _ -> stop_internal ~parent_stop_epoch:epoch t Cancel)
       | _ ->
         Error
           (error Permission_denied "delegation.stop: child relationship does not match"))
    | Defer_history (attachment_id, entries) ->
      with_writer t attachment_id (fun () -> defer_history t entries)
    | Submit_message (original_actor, attachment_id, submitting_principal, timing, entry)
      ->
      let open Result.Let_syntax in
      let%bind () =
        match original_actor with
        | None -> Ok ()
        | Some actor ->
          let principal = Operator_authorization.principal actor in
          if
            Operator_authorization.is_current actor
            && Agent_protocol.Principal.has_scope principal Send_messages
            && Agent_protocol.Id.Principal.equal principal.id submitting_principal
          then Ok ()
          else
            Error
              (error
                 Permission_denied
                 "message authentication expired or differs from submitter")
      in
      submit_message ?original_actor t attachment_id submitting_principal timing entry
    | Control_pending (principal, project, request, replacement) ->
      control_pending_internal t principal project request replacement
    | Capture_run_constructor_model_source
        (preparation, job_id, generation, target, limits) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      capture_model_job_source
        ~run_preparation:preparation
        t
        job_id
        generation
        target
        limits
    | Capture_run_constructor_recipe_target
        (preparation, job_id, generation, attempt, target, limits) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      capture_recipe_target
        ~run_preparation:preparation
        t
        job_id
        generation
        attempt
        target
        limits
    | Start_run_constructor_model_job (preparation, job) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      let%bind job = add_constructor_job t (Run_constructor_job preparation) job in
      let%bind claimed = claim_job ~run_preparation:preparation t job.id job.generation in
      Result.of_option
        claimed
        ~error:(error Conflict "constructor model call could not claim its durable job")
    | Complete_run_constructor_model_job
        (preparation, job_id, generation, attempt, outcome) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      complete_job ~run_preparation:preparation t job_id generation attempt outcome
    | Interrupt_run_constructor_job (preparation, job_id, generation, attempt, reason) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      interrupt_job ~run_preparation:preparation t job_id generation attempt reason
    | Add_run_constructor_job (preparation, job) ->
      add_constructor_job t (Run_constructor_job preparation) job
    | Add_run_constructor_schedule (preparation, schedule) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      add_schedule ~run_preparation:preparation t schedule
    | Cancel_run_constructor_schedule (preparation, id) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      cancel_schedule_internal ~run_preparation:preparation t id
    | Reserve_run_history_block (preparation, count) ->
      let open Result.Let_syntax in
      let%bind () = check_run_preparation t preparation in
      reserve_history_block ~run_preparation:preparation t count
    | Checkpoint_run_moderator (preparation, moderator, moderator_manager) ->
      checkpoint_run_moderator t preparation moderator moderator_manager
    | Admit_prepared_run (preparation, scope, session, entry) ->
      if not (Run_preparation.belongs_to preparation ~owner:t.run_preparation_owner)
      then
        Ok
          (Run_admission_outcome.Rejected
             (error Conflict "run preparation belongs to another actor"))
      else
        Exn.protect
          ~finally:(fun () ->
            Run_preparation.close preparation;
            match t.run_preparation with
            | Some active when phys_equal active preparation -> t.run_preparation <- None
            | Some _ | None -> ())
          ~f:(fun () ->
            let attempted = ref false in
            let result =
              let open Result.Let_syntax in
              let%bind () = check_run_preparation t preparation in
              admit_run
                ~preparation
                ~commit_attempted:attempted
                t
                scope
                (Run_preparation.request preparation)
                session
                (Run_preparation.request_sha256 preparation)
                entry
            in
            match result with
            | Ok receipt -> Ok (Run_admission_outcome.Admitted receipt)
            | Error failure ->
              let retained =
                Run_state.receipt
                  (Option.value t.state.run_state ~default:Run_state.empty)
                  ~principal_id:(Run_preparation.principal_id preparation)
                  ~key:(Run_preparation.request preparation).key
                  ~request_sha256:(Run_preparation.request_sha256 preparation)
              in
              (match retained with
               | Ok (Some ({ kind = Admission; _ } as receipt)) ->
                 Ok (Run_admission_outcome.Admitted receipt)
               | Ok (Some { kind = Action | Terminal; _ }) | Ok None | Error _ ->
                 Ok
                   (if !attempted
                    then Run_admission_outcome.Uncertain failure
                    else Run_admission_outcome.Rejected failure)))
    | Begin_run_preparation
        (original_actor, authorize, principal_id, request, request_sha256) ->
      begin_run_preparation_internal
        ?original_actor
        t
        ~authorize
        ~principal_id
        ~request:(Run_admission_request.of_writer request)
        ~request_sha256
    | Begin_headless_run_preparation (actor, authorize, request) ->
      let open Result.Let_syntax in
      let authorize = Run_command_authorization.check actor ~authorize in
      let principal_id = (Operator_authorization.principal actor).id in
      let request = Run_admission_request.of_headless request in
      let%bind request_sha256 = Run_admission_request.request_sha256 request in
      begin_run_preparation_internal
        ~original_actor:actor
        t
        ~authorize
        ~principal_id
        ~request
        ~request_sha256
    | Close_run_preparation preparation ->
      if not (Run_preparation.belongs_to preparation ~owner:t.run_preparation_owner)
      then Error (error Conflict "run preparation belongs to another actor")
      else (
        Run_preparation.close preparation;
        (match t.run_preparation with
         | Some active when phys_equal active preparation -> t.run_preparation <- None
         | Some _ | None -> ());
        Ok ())
    | Run_start_receipt (authorize, principal_id, request, request_sha256) ->
      let open Result.Let_syntax in
      let%bind () = authorize t.state in
      let%bind _ = write_attachment t request.attachment_id in
      let%bind () =
        if Agent_protocol.Id.Session.equal request.session_id t.state.identity.session_id
        then Ok ()
        else Error (error Invalid_request "run receipt targets another session")
      in
      let%bind retained =
        Run_state.receipt
          (Option.value t.state.run_state ~default:Run_state.empty)
          ~principal_id
          ~key:request.key
          ~request_sha256
      in
      (match retained with
       | None -> Ok None
       | Some ({ kind = Admission; _ } as receipt) -> Ok (Some receipt)
       | Some { kind = Action | Terminal; _ } ->
         Error (error Conflict "run start key belongs to another action"))
    | Admit_run (scope, request, session, request_sha256, entry) ->
      admit_run
        t
        scope
        (Run_admission_request.of_writer request)
        session
        request_sha256
        entry
    | Submit_managed_message (reference, key, fingerprint, generation, maximum, entry) ->
      submit_managed_message t reference key fingerprint generation maximum entry
    | Compact (original_actor, attachment_id, expected_revision) ->
      compact_internal t original_actor attachment_id expected_revision
    | Delete_history (attachment_id, revision, history_id) ->
      delete_history_internal t attachment_id revision history_id
    | Edit_history request -> edit_history_internal t request
    | Continue_history request -> continue_history_internal t request
    | Adopt_deferred -> adopt_deferred t
    | Reserve_history_block count -> reserve_history_block t count
    | Commit_worker_entry (operation_id, entry) ->
      commit_worker_entry t operation_id entry
    | Prepare_authoring_input (operation_id, materialization, history, effective) ->
      prepare_authoring_input t operation_id materialization history effective
    | Publish_invocation_output (operation_id, invocation_id, entry) ->
      publish_invocation_output t operation_id invocation_id entry
    | Consume_deferred operation_id -> consume_deferred t operation_id
    | Consume_notifications (operation_id, plan) ->
      consume_notifications_internal t operation_id plan
    | Deliver_idle_notifications plan -> deliver_idle_notifications_internal t plan
    | Admit_standalone_delivery plan -> admit_standalone_delivery_internal t plan
    | Retire_obsolete_moderator_delivery (revision, job) ->
      retire_obsolete_moderator_delivery_internal t revision job
    | Deliver_standalone_completion (revision, job, capabilities, policy) ->
      deliver_standalone_completion_internal t revision job capabilities policy
    | Consume_initial_notifications (operation_id, proposal) ->
      consume_notifications_internal
        ~wakes:proposal.wakes
        ~discarded_wakes:proposal.discarded_wakes
        t
        operation_id
        proposal.pending
    | Commit_worker_moderator (operation_id, snapshot) ->
      commit_worker_moderator t operation_id snapshot
    | Has_writer_attachment -> Ok (has_writer_attachment t)
    | Authorize_writer attachment_id -> with_writer t attachment_id (fun () -> Ok ())
    | Invocation_granted (tool_name, identity_digest) ->
      Ok (invocation_granted t ~tool_name ~identity_digest)
    | Worker_ready (operation_id, cancel) -> worker_ready t operation_id cancel
    | Worker_terminal (operation_id, outcome, compaction_custody) ->
      with_runtime_terminal t operation_id (fun () ->
        worker_terminal t operation_id outcome compaction_custody)
    | Compaction_terminal (operation_id, outcome) ->
      with_runtime_terminal t operation_id (fun () ->
        compaction_terminal t operation_id outcome)
    | Cancel_operation (attachment_id, operation_id) ->
      cancel_operation_internal t attachment_id operation_id
    | Open_permission (permission, timeout_seconds, fallback) ->
      open_permission t permission timeout_seconds fallback
    | Respond_permission
        (attachment_id, principal_id, permission_id, generation, choice, reason) ->
      respond_permission_internal
        t
        attachment_id
        principal_id
        permission_id
        generation
        choice
        reason
    | Resolve_permission_system (permission_id, generation, choice, reason) ->
      resolve_permission_system t permission_id generation choice reason
    | Expire_permission (permission_id, generation, fallback) ->
      expire_permission_internal t permission_id generation fallback
    | Revoke_grant (attachment_id, grant_id, reason) ->
      revoke_grant t attachment_id grant_id reason
    | Change_job (attachment_id, job) -> change_job t attachment_id job
    | Capture_inference_target (target, limits) ->
      capture_inference_target t target limits
    | Capture_model_job_source (job_id, generation, target, limits) ->
      capture_model_job_source t job_id generation target limits
    | Capture_recipe_target (job_id, generation, attempt, target, limits) ->
      capture_recipe_target t job_id generation attempt target limits
    | Add_initialization_model_job (scope, job) ->
      add_initialization_model_job t scope job
    | Start_initialization_model_job (scope, job) ->
      start_initialization_model_job t scope job
    | Claim_initialization_model_job (scope, job_id, generation) ->
      claim_initialization_model_job t scope job_id generation
    | Initialization_model_job_is_current (scope, job_id, generation, attempt) ->
      initialization_model_job_is_current t scope job_id generation attempt
    | Capture_initialization_recipe_target
        (scope, job_id, generation, attempt, target, limits) ->
      capture_initialization_recipe_target t scope job_id generation attempt target limits
    | Complete_initialization_model_job (scope, job_id, generation, attempt, outcome) ->
      complete_initialization_model_job t scope job_id generation attempt outcome
    | Add_job job -> add_job t job
    | Claim_job (job_id, generation) -> claim_job t job_id generation
    | Complete_job (job_id, generation, attempt, outcome) ->
      complete_job t job_id generation attempt outcome
    | Complete_background_job (job_id, generation, attempt, outcome) ->
      complete_background_job t job_id generation attempt outcome
    | Defer_background_job (job_id, generation, attempt, dependency) ->
      defer_background_job t job_id generation attempt dependency
    | Refresh_background_job (job_id, generation, attempt) ->
      refresh_background_job t job_id generation attempt
    | Recover_background_results (max_count, max_total_bytes) ->
      recover_background_results t max_count max_total_bytes
    | Enqueue_run_job_delivery (delivery, before, after) ->
      enqueue_run_job_delivery t delivery before after
    | Deliver_job (job_id, generation, expected, expected_job, moderator_snapshot, manager)
      -> deliver_job t job_id generation expected expected_job moderator_snapshot manager
    | Cancel_job_internal job_id -> cancel_job_internal t job_id
    | Cancel_job (attachment_id, job_id, expected_generation, expected_attempt) ->
      with_writer t attachment_id (fun () ->
        let open Result.Let_syntax in
        let%bind job = find_job t job_id in
        let%bind () =
          match expected_generation, expected_attempt with
          | None, None -> Ok ()
          | Some generation, Some attempt ->
            if
              Int.equal generation t.state.identity.generation
              && Int.equal generation job.generation
              && Int.equal attempt job.attempt
            then Ok ()
            else Error (error Conflict "job occurrence has changed")
          | Some _, None | None, Some _ ->
            Error (error Invalid_request "job occurrence requires generation and attempt")
        in
        cancel_job_internal t job_id)
    | Interrupt_job (job_id, generation, attempt, reason) ->
      interrupt_job t job_id generation attempt reason
    | Change_schedule (attachment_id, event, schedule) ->
      change_schedule t attachment_id event schedule
    | Add_schedule schedule -> add_schedule t schedule
    | Callback_schedule_after_ms (scope, delay_ms, payload) ->
      callback_schedule_after_ms t scope delay_ms payload
    | Callback_schedule_cancel (scope, schedule_id) ->
      callback_schedule_cancel t scope schedule_id
    | Cancel_schedule (attachment_id, schedule_id, expected_generation) ->
      with_writer t attachment_id (fun () ->
        let open Result.Let_syntax in
        let%bind schedule = find_schedule t schedule_id in
        let%bind () =
          match expected_generation with
          | None -> Ok ()
          | Some generation ->
            if
              Int.equal generation t.state.identity.generation
              && Int.equal generation schedule.generation
            then Ok ()
            else Error (error Conflict "schedule occurrence has changed")
        in
        cancel_schedule_internal t schedule_id)
    | Cancel_schedule_internal schedule_id -> cancel_schedule_internal t schedule_id
    | Claim_schedule (schedule_id, generation) -> claim_schedule t schedule_id generation
    | Retry_schedule (schedule_id, generation) -> retry_schedule t schedule_id generation
    | Complete_schedule
        ( schedule_id
        , generation
        , expected
        , expected_schedule
        , moderator_snapshot
        , moderator_manager ) ->
      complete_schedule
        t
        schedule_id
        generation
        expected
        expected_schedule
        moderator_snapshot
        moderator_manager
    | Fail_schedule (schedule_id, generation, failure) ->
      fail_schedule t schedule_id generation failure
    | Skip_schedule (schedule_id, generation) -> skip_schedule t schedule_id generation
    | Claim_idle_moderator -> claim_idle_moderator t
    | Apply_observation_follow_up -> apply_observation_follow_up t
    | Complete_idle_moderator drain -> complete_idle_moderator t drain
    | Fail_idle_moderator failure -> fail_idle_moderator t failure
    | Attach (mode, subscribe, principal_id, reclaim_token) ->
      attach t mode subscribe principal_id reclaim_token
    | Detach attachment_id -> detach t attachment_id
    | Renew_owner (attachment_id, generation) -> renew_owner t attachment_id generation
    | Owner_expired generation -> owner_expired t generation
    | Checkpoint persist -> persist t.state
    | Quiescent_checkpoint inspect ->
      (match
         ( t.state.active_operation
         , t.active_cancel
         , t.invocation_executions
         , t.job_scopes
         , t.moderator_borrow
         , t.queued_event_borrow
         , t.foreground_moderator
         , t.idle_moderator_borrowed
         , Staged_jobs.is_empty t.staged_jobs
           && Staged_subscriptions.is_empty t.staged_subscriptions
           && Staged_ingress.is_empty t.staged_ingress
           && Staged_schedules.is_empty t.staged_schedules
           && Staged_notifications.is_empty t.staged_notifications
         , Active_calls.snapshot t.active_calls )
       with
       | None, None, [], [], None, None, None, false, true, ([], []) ->
         Result.map (inspect t.state) ~f:Option.some
       | _ -> Ok None)
    | Open_inference_owner source -> open_inference_owner t source
    | Admit_inference
        (run_preparation, owner, relation, operation_id, invocation_id, configuration) ->
      admit_inference
        ?run_preparation
        t
        owner
        relation
        operation_id
        invocation_id
        configuration
    | Allocate_inference (execution, owner, relation, configuration) ->
      allocate_inference t execution owner relation configuration
    | Admit_model_job (execution, job) -> admit_model_job t execution job
    | Refresh_inference (execution, owner, handle) ->
      refresh_inference t execution owner handle
    | Acknowledge_inference (run_preparation, owner, handle, attempt) ->
      acknowledge_inference ?run_preparation t owner handle attempt
    | Observe_inference (run_preparation, handle, incoming) ->
      observe_inference ?run_preparation t handle incoming
    | Observe_owned_inference (run_preparation, owner, incoming) ->
      observe_owned_inference ?run_preparation t owner incoming
    | Complete_inference (run_preparation, owner, handle, completion) ->
      complete_inference ?run_preparation t owner handle completion
    | Release_inference (run_preparation, owner, handle) ->
      release_inference ?run_preparation t owner handle
    | Seal_inference_owner owner -> seal_inference_owner t owner
    | Finish_inference_owner (run_preparation, owner) ->
      finish_inference_owner ?run_preparation t owner
    | Reconcile_inference_recovery -> reconcile_inference_recovery t
    | Shutdown ->
      retire_execution_custody t;
      abort_all_staged_work t;
      Option.iter t.owner_timer_cancel ~f:(fun resolver ->
        Eio.Promise.resolve resolver ());
      t.owner_timer_cancel <- None;
      Option.iter t.active_cancel ~f:(fun cancel -> cancel ());
      List.iter t.job_scopes ~f:cancel_job_scope;
      Option.iter t.queued_event_borrow ~f:(fun borrow ->
        borrow.callback_active <- false;
        Atomic.set borrow.authority_live false;
        borrow.cancel_requested <- true;
        Option.iter borrow.cancel ~f:(fun cancel -> cancel ()));
      Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
        Map.iter !(t.subscribers) ~f:Subscriber.close;
        t.subscribers := Map.Poly.empty);
      Map.iter !(t.permission_waiters) ~f:(fun waiter ->
        Eio.Promise.resolve
          waiter.resolver
          Agent_protocol.Permission.
            { choice = Deny
            ; principal_id = None
            ; resolved_at = t.services.now ()
            ; reason = Some "session actor shut down"
            });
      t.permission_waiters := Map.Poly.empty;
      t.stopped <- true;
      Ok ())
;;

let rec reject_pending mailbox =
  match Mailbox.pop mailbox with
  | None -> ()
  | Some (Pack (_, _, resolver)) ->
    Actor_reply.resolve
      resolver
      (Error
         (Agent_protocol.Error.create
            Server_shutting_down
            ~message:"session actor is shut down"
            ~retryable:true
            ()));
    reject_pending mailbox
;;

let close_owner t =
  t.stopped <- true;
  t.command_audit <- None;
  retire_execution_custody t;
  Mailbox.close t.mailbox
;;

let rec run t =
  match Mailbox.pop t.mailbox with
  | None -> ()
  | Some (Pack (command_audit, request, resolver)) ->
    t.command_audit <- command_audit;
    (match handle t request with
     | result ->
       t.command_audit <- None;
       if t.stopped then Mailbox.close t.mailbox;
       Actor_reply.resolve resolver result;
       if not t.stopped then run t
     | exception exn ->
       let backtrace = Stdlib.Printexc.get_raw_backtrace () in
       Eio.Cancel.protect (fun () ->
         close_owner t;
         Actor_reply.resolve_exception resolver exn backtrace);
       Exn.raise_with_original_backtrace exn backtrace)
;;

let create_with_owner_lease_duration
      ~schedule_permission_timeouts
      ~sw
      ~clock
      ~mailbox_capacity
      ~owner_lease_duration_ms
      ~max_attachments
      ~subscriber_capacity
      ~compaction_env
      ~(initial_state : Session_state.t)
      ~persistence
      ~operation_worker
      ~services
  =
  if max_attachments <= 0 then invalid_arg "max_attachments must be positive";
  if subscriber_capacity <= 0 then invalid_arg "subscriber_capacity must be positive";
  (match Staged_subscriptions.validate_limits services.subscription_limits with
   | Ok () -> ()
   | Error error -> invalid_arg error.message);
  (match Staged_ingress.validate_limits services.ingress_limits with
   | Ok () -> ()
   | Error error -> invalid_arg error.message);
  (match Staged_schedules.validate_limits services.schedule_limits with
   | Ok () -> ()
   | Error error -> invalid_arg error.message);
  (match Staged_notifications.validate_limits services.notification_limits with
   | Ok () -> ()
   | Error error -> invalid_arg error.message);
  let finished, finish = Eio.Promise.create () in
  let t =
    { sw
    ; sleep = (fun seconds -> Eio.Time.sleep clock seconds)
    ; mailbox = Mailbox.create ~capacity:mailbox_capacity
    ; finished
    ; persistence
    ; services
    ; subscriber_mutex = Eio.Mutex.create ()
    ; active_calls = Active_calls.create ()
    ; subscribers = ref Map.Poly.empty
    ; permission_waiters = ref Map.Poly.empty
    ; organization_admission = None
    ; configuration_policy = None
    ; recurrence_controller = None
    ; recurrence_runtime = Recurrence_runtime.create ~origin:(services.monotonic_now ())
    ; recurrence_creation_owner = ref ()
    ; recurrence_activation_owner = Activation_custody.Owner.create ()
    ; recurrence_activations = []
    ; configuration_update_owner = Configuration_update.Owner.create ()
    ; configuration_capture = Configuration_capture.create ()
    ; operation_worker
    ; compaction_inference = None
    ; inference_execution = None
    ; moderator_manager = None
    ; compaction_env
    ; owner_lease_duration_ms
    ; max_attachments
    ; subscriber_capacity
    ; schedule_permission_timeouts
    ; owner_timer_cancel = None
    ; active_cancel = None
    ; lifecycle_owner = ref ()
    ; lifecycle_fence = None
    ; runtime_retirement = None
    ; idle_moderator_borrowed = false
    ; moderator_borrow = None
    ; queued_event_borrow = None
    ; legacy_callback_scope = None
    ; legacy_queue_custody = Legacy_queue_custody.empty
    ; legacy_schedule_custody = Legacy_schedule_custody.empty
    ; operation_agent_custody = None
    ; lifecycle_agent_custody = Unadmitted
    ; intent_agent_custody = []
    ; pending_agent_custody = []
    ; job_agent_custody = Job_custody.empty
    ; run_authorities = Run_authorities.empty
    ; run_preparation_owner = Run_preparation.Owner.create ()
    ; run_preparation = None
    ; foreground_moderator = None
    ; invocation_executions = []
    ; inference_owner_identity = ref ()
    ; inference_owners = []
    ; initialization_owner = ref ()
    ; initialization_scope = None
    ; job_scopes = []
    ; staged_jobs = Staged_jobs.create ()
    ; staged_subscriptions = Staged_subscriptions.create ()
    ; staged_ingress = Staged_ingress.create ()
    ; staged_schedules = Staged_schedules.create ()
    ; staged_notifications = Staged_notifications.create ()
    ; notification_inputs = None
    ; extension_clock = Extension_clock.create ()
    ; invocation_gate = Chat_response.Execution_gate.create ()
    ; event_sequence = Atomic.make initial_state.counters.event_sequence
    ; state = initial_state
    ; stopped = false
    ; command_audit = None
    }
  in
  sync_extension_clock t;
  (* A persisted actor outlives the tool/script that created it. Its future
     operations acquire their own budgets and delegated authority; inheriting
     this caller's lexical ChatML frame would poison them after creation ends. *)
  Chatml_execution.without_ambient_context (fun () ->
    Eio.Fiber.fork ~sw (fun () ->
      Exn.protect
        ~f:(fun () -> run t)
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
            Exn.protect
              ~f:(fun () ->
                close_owner t;
                reject_pending t.mailbox)
              ~finally:(fun () -> Eio.Promise.resolve finish ())))));
  Option.iter (owner_attachment t) ~f:(fun attachment ->
    Option.iter attachment.owner_lease ~f:(schedule_owner_lease t));
  t
;;

let create
      ~sw
      ~clock
      ~mailbox_capacity
      ~compaction_env
      ~initial_state
      ~persistence
      ~operation_worker
      ~services
  =
  create_with_owner_lease_duration
    ~schedule_permission_timeouts:true
    ~sw
    ~clock
    ~mailbox_capacity
    ~owner_lease_duration_ms:60_000
    ~max_attachments:1_024
    ~subscriber_capacity:512
    ~compaction_env
    ~initial_state
    ~persistence
    ~operation_worker
    ~services
;;

let snapshot t = call t Snapshot
let observe t = call t Observe
let state t = call t State
let due_schedules t = call t Due_schedules

let commit_extensions t ~generation ~expected_revision changes =
  call t (Commit_extensions (generation, expected_revision, changes))
;;

let enable_automatic_turn_budget ?actor t policy =
  call t (Enable_automatic_turn_budget (actor, policy))
;;

let set_automatic_turn_pauses t conditions = call t (Set_automatic_turn_pauses conditions)

let set_operation_worker t worker =
  call t ~priority:Priority (Set_operation_worker worker)
;;

let set_runtime_worker ?actor ?moderator_manager t ~worker ~inference =
  call
    t
    ~priority:Priority
    (Set_runtime_worker (actor, worker, inference, moderator_manager))
;;

let retire_runtime_worker t ~closing =
  call t ~priority:Priority (Retire_runtime_worker closing)
;;

let set_compaction_inference t port =
  call t ~priority:Priority (Set_compaction_inference port)
;;

let change_moderator ?actor t moderator = call t (Change_moderator (actor, moderator))
let shell_approval_grants t = call t Shell_approval_grants

let replace_shell_approval_grants t approval_grants =
  call t (Replace_shell_approval_grants (None, approval_grants))
;;

let replace_shell_extension_snapshots t extension_snapshots =
  call t (Replace_shell_extension_snapshots (None, extension_snapshots))
;;

let shell_manifest_grants t = call t Shell_manifest_grants
let add_shell_manifest_grant t grant = call t (Add_shell_manifest_grant (None, grant))

let commit_administration t ~command_audit ~attachment_id ~expected_revision ~kind state =
  call
    t
    ?command_audit
    (Commit_administration (attachment_id, expected_revision, kind, state))
;;

let validate_administration_basis t ~attachment_id ~expected =
  call t (Validate_administration_basis (attachment_id, expected))
;;

let commit_reconciled_administration
      t
      ~command_audit
      ~attachment_id
      ~expected
      ~kind
      candidate
  =
  call
    t
    ?command_audit
    (Commit_reconciled_administration (attachment_id, expected, kind, candidate))
;;

let open_inference_owner t ~source = call t (Open_inference_owner source)

let admit_inference t ~owner ~relation ~operation_id ~invocation_id ~configuration =
  call
    t
    (Admit_inference (None, owner, relation, operation_id, invocation_id, configuration))
;;

let acknowledge_inference t ~owner ~handle attempt =
  call t (Acknowledge_inference (None, owner, handle, attempt))
;;

let observe_inference t ~handle incoming =
  call t (Observe_inference (None, handle, incoming))
;;

let observe_owned_inference t ~owner incoming =
  call t (Observe_owned_inference (None, owner, incoming))
;;

let complete_inference t ~owner ~handle completion =
  call t (Complete_inference (None, owner, handle, completion))
;;

let release_inference t ~owner ~handle = call t (Release_inference (None, owner, handle))
let seal_inference_owner t ~owner = call t (Seal_inference_owner owner)
let finish_inference_owner t ~owner = call t (Finish_inference_owner (None, owner))

module Constructor_mutations = struct
  let admit_inference
        t
        ~preparation
        ~owner
        ~relation
        ~operation_id
        ~invocation_id
        ~configuration
    =
    call
      t
      (Admit_inference
         (Some preparation, owner, relation, operation_id, invocation_id, configuration))
  ;;

  let acknowledge_inference t ~preparation ~owner ~handle attempt =
    call t (Acknowledge_inference (Some preparation, owner, handle, attempt))
  ;;

  let observe_inference t ~preparation ~handle incoming =
    call t (Observe_inference (Some preparation, handle, incoming))
  ;;

  let observe_owned_inference t ~preparation ~owner incoming =
    call t (Observe_owned_inference (Some preparation, owner, incoming))
  ;;

  let complete_inference t ~preparation ~owner ~handle completion =
    call t (Complete_inference (Some preparation, owner, handle, completion))
  ;;

  let release_inference t ~preparation ~owner ~handle =
    call t (Release_inference (Some preparation, owner, handle))
  ;;

  let finish_inference_owner t ~preparation ~owner =
    call t (Finish_inference_owner (Some preparation, owner))
  ;;

  let replace_shell_approval_grants t ~preparation approval_grants =
    call t (Replace_shell_approval_grants (Some preparation, approval_grants))
  ;;

  let replace_shell_extension_snapshots t ~preparation extension_snapshots =
    call t (Replace_shell_extension_snapshots (Some preparation, extension_snapshots))
  ;;

  let add_shell_manifest_grant t ~preparation grant =
    call t (Add_shell_manifest_grant (Some preparation, grant))
  ;;
end

let reconcile_inference_recovery t = call t Reconcile_inference_recovery

let begin_initialization
      ?(purpose = Initialization_scope.Purpose.Pending_initialization)
      t
      ~expected
  =
  call t (Begin_initialization (purpose, None, expected))
;;

let begin_initialization_authenticated
      ?(purpose = Initialization_scope.Purpose.Pending_initialization)
      t
      ~actor
      ~expected
  =
  call t (Begin_initialization (purpose, Some actor, expected))
;;

let initialization_agent_actor t ~scope = call t (Borrow_initialization_agent_actor scope)

let begin_run_initialization
      ?(purpose = Initialization_scope.Purpose.Pending_initialization)
      t
      ~preparation
      ~expected
  =
  call t (Begin_run_initialization (purpose, preparation, expected))
;;

let end_initialization t ~scope = call t ~priority:Priority (End_initialization scope)

let retire_initialization_resource t ~scope ~moderator_manager =
  call t ~priority:Priority (Retire_initialization_resource (scope, moderator_manager))
;;

let complete_initialization ?moderator_manager t ~scope ~candidate =
  call t (Complete_initialization (scope, candidate, moderator_manager))
;;

let checkpoint_runtime_construction ?moderator_manager t ~scope moderator =
  call t (Checkpoint_runtime_construction (scope, moderator, moderator_manager))
;;

let fail_initialization t ~scope failure = call t (Fail_initialization (scope, failure))

let reset t ~attachment_id ~expected_revision options =
  call t ~priority:Priority (Reset (attachment_id, expected_revision, options))
;;

let reset_with_command_audit t ~command_audit ~attachment_id ~expected_revision options =
  call
    t
    ~priority:Priority
    ~command_audit
    (Reset (attachment_id, expected_revision, options))
;;

let upgrade_prompt t ~attachment_id ~expected_revision ~target_revision =
  call
    t
    ~priority:Priority
    (Upgrade_prompt (attachment_id, expected_revision, target_revision))
;;

let upgrade_prompt_with_command_audit
      t
      ~command_audit
      ~attachment_id
      ~expected_revision
      ~target_revision
  =
  call
    t
    ~priority:Priority
    ~command_audit
    (Upgrade_prompt (attachment_id, expected_revision, target_revision))
;;

let start ?actor ?expected_parent_stop_epoch t ~attachment_id =
  call t (Start (actor, attachment_id, expected_parent_stop_epoch))
;;

let start_initial_delegated ?expected_parent_stop_epoch t ~reference =
  call t (Start_initial_delegated (reference, expected_parent_stop_epoch))
;;

let fail_initial_delegated t ~reference failure =
  call t ~priority:Priority (Fail_initial_delegated (reference, failure))
;;

let replace_workspace t workspace = call t ~priority:Priority (Change_workspace workspace)
let queue_start t ~attachment_id = call t (Queue_start attachment_id)

let start_with_command_audit
      ?actor
      ?expected_parent_stop_epoch
      t
      ~command_audit
      ~attachment_id
  =
  call t ~command_audit (Start (actor, attachment_id, expected_parent_stop_epoch))
;;

let queue_start_with_command_audit t ~command_audit ~attachment_id =
  call t ~command_audit (Queue_start attachment_id)
;;

let activate_queued_start t = call t ~priority:Priority Activate_queued_start
let stop t ~attachment_id ~mode = call t ~priority:Priority (Stop (attachment_id, mode))

let stop_with_command_audit t ~command_audit ~attachment_id ~mode =
  call t ~priority:Priority ~command_audit (Stop (attachment_id, mode))
;;

let stop_delegated t ~reference ~mode =
  call t ~priority:Priority (Stop_delegated (reference, mode))
;;

let stop_managed t ~reference ~key ~mode ~generation ~max_receipts =
  call
    t
    ~priority:Priority
    (Stop_managed (reference, key, mode, generation, max_receipts))
;;

let stop_delegated_at_epoch ?(force = false) t ~reference ~epoch =
  call t ~priority:Priority (Stop_delegated_at_epoch (reference, epoch, force))
;;

let append_history t ~attachment_id entries =
  call t (Append_history (attachment_id, entries))
;;

let defer_history t ~attachment_id entries =
  call t (Defer_history (attachment_id, entries))
;;

let submit_message
      t
      ?(timing = Agent_protocol.Pending_input.Timing.Safe_boundary)
      ~submitting_principal
      ~attachment_id
      entry
  =
  call t (Submit_message (None, attachment_id, submitting_principal, timing, entry))
;;

let enable_run_constructor_turn_budget t ~preparation policy =
  call t (Enable_run_constructor_turn_budget (preparation, policy))
;;

let capture_run_constructor_model_source
      t
      ~preparation
      ~job_id
      ~generation
      ~target
      ~limits
  =
  call
    t
    (Capture_run_constructor_model_source (preparation, job_id, generation, target, limits))
;;

let capture_run_constructor_recipe_target
      t
      ~preparation
      ~job_id
      ~generation
      ~attempt
      ~target
      ~limits
  =
  call
    t
    (Capture_run_constructor_recipe_target
       (preparation, job_id, generation, attempt, target, limits))
;;

let start_run_constructor_model_job t ~preparation job =
  call t (Start_run_constructor_model_job (preparation, job))
;;

let complete_run_constructor_model_job t ~preparation ~job_id ~generation ~attempt outcome
  =
  call
    t
    (Complete_run_constructor_model_job (preparation, job_id, generation, attempt, outcome))
;;

let interrupt_run_constructor_job t ~preparation ~job_id ~generation ~attempt ~reason =
  call
    t
    ~priority:Priority
    (Interrupt_run_constructor_job (preparation, job_id, generation, attempt, reason))
;;

let add_run_constructor_job t ~preparation job =
  call t (Add_run_constructor_job (preparation, job))
;;

let add_run_constructor_schedule t ~preparation schedule =
  call t (Add_run_constructor_schedule (preparation, schedule))
;;

let cancel_run_constructor_schedule t ~preparation ~schedule_id =
  call t (Cancel_run_constructor_schedule (preparation, schedule_id))
;;

let reserve_run_history_block t ~preparation ~count =
  call t (Reserve_run_history_block (preparation, count))
;;

let checkpoint_run_moderator ?moderator_manager t ~preparation moderator =
  call t (Checkpoint_run_moderator (preparation, moderator, moderator_manager))
;;

let admit_prepared_run t ~command_audit ~preparation ~scope ~session ~entry =
  call t ?command_audit (Admit_prepared_run (preparation, scope, session, entry))
;;

let begin_run_preparation t ~authorize ~principal_id ~request ~request_sha256 =
  call t (Begin_run_preparation (None, authorize, principal_id, request, request_sha256))
;;

let begin_run_preparation_authenticated t ~actor ~authorize ~request ~request_sha256 =
  let principal_id = (Operator_authorization.principal actor).id in
  call
    t
    (Begin_run_preparation
       ( Some actor
       , Run_command_authorization.check actor ~authorize
       , principal_id
       , request
       , request_sha256 ))
;;

let begin_headless_run_preparation t ~actor ~authorize ~request =
  call t (Begin_headless_run_preparation (actor, authorize, request))
;;

let end_run_preparation t preparation = call t (Close_run_preparation preparation)

let run_start_receipt t ~authorize ~principal_id ~request ~request_sha256 =
  call t (Run_start_receipt (authorize, principal_id, request, request_sha256))
;;

let admit_run t ~scope ~request ~session ~request_sha256 ~entry =
  call t (Admit_run (scope, request, session, request_sha256, entry))
;;

let admit_run_with_command_audit
      t
      ~command_audit
      ~scope
      ~request
      ~session
      ~request_sha256
      ~entry
  =
  call t ~command_audit (Admit_run (scope, request, session, request_sha256, entry))
;;

let submit_managed_message
      t
      ~reference
      ~key
      ~request_sha256
      ~generation
      ~max_receipts
      entry
  =
  Eio.Cancel.protect (fun () ->
    call
      t
      (Submit_managed_message
         (reference, key, request_sha256, generation, max_receipts, entry)))
;;

let submit_message_authenticated
      t
      ~actor
      ?(timing = Agent_protocol.Pending_input.Timing.Safe_boundary)
      ~attachment_id
      entry
  =
  let submitting_principal = (Operator_authorization.principal actor).id in
  call t (Submit_message (Some actor, attachment_id, submitting_principal, timing, entry))
;;

let submit_message_authenticated_with_command_audit
      t
      ~actor
      ?(timing = Agent_protocol.Pending_input.Timing.Safe_boundary)
      ~command_audit
      ~attachment_id
      entry
  =
  let submitting_principal = (Operator_authorization.principal actor).id in
  call
    t
    ~command_audit
    (Submit_message (Some actor, attachment_id, submitting_principal, timing, entry))
;;

let submit_message_with_command_audit
      t
      ?(timing = Agent_protocol.Pending_input.Timing.Safe_boundary)
      ~command_audit
      ~submitting_principal
      ~attachment_id
      entry
  =
  call
    t
    ~command_audit
    (Submit_message (None, attachment_id, submitting_principal, timing, entry))
;;

let cancel_pending t ?command_audit ~principal ~project request =
  call t ?command_audit (Control_pending (principal, project, request, None))
;;

let replace_pending
      t
      ?command_audit
      ~principal
      ~project
      (request : Agent_protocol.Pending_control.Replace_request.t)
  =
  call
    t
    ?command_audit
    (Control_pending (principal, project, request.target, Some request.text))
;;

let compact t ~attachment_id ~expected_revision =
  call t (Compact (None, attachment_id, expected_revision))
;;

let compact_with_command_audit t ~command_audit ~attachment_id ~expected_revision =
  call t ~command_audit (Compact (None, attachment_id, expected_revision))
;;

let compact_authenticated t ~actor ~attachment_id ~expected_revision =
  call t (Compact (Some actor, attachment_id, expected_revision))
;;

let compact_authenticated_with_command_audit
      t
      ~actor
      ~command_audit
      ~attachment_id
      ~expected_revision
  =
  call t ~command_audit (Compact (Some actor, attachment_id, expected_revision))
;;

let delete_history t ?command_audit ~attachment_id ~expected_revision history_id =
  call t ?command_audit (Delete_history (attachment_id, expected_revision, history_id))
;;

let adopt_deferred t = call t Adopt_deferred
let reserve_history_block t ~count = call t (Reserve_history_block count)

let commit_worker_entry t ~operation_id entry =
  call t (Commit_worker_entry (operation_id, entry))
;;

let consume_deferred t ~operation_id = call t (Consume_deferred operation_id)

let consume_notifications t ~operation_id plan =
  call t (Consume_notifications (operation_id, plan))
;;

let deliver_idle_notifications t plan = call t (Deliver_idle_notifications plan)

let admit_standalone_delivery t plan =
  Eio.Cancel.protect (fun () -> call t (Admit_standalone_delivery plan))
;;

let retire_obsolete_moderator_delivery t ~revision ~job =
  Eio.Cancel.protect (fun () ->
    call t (Retire_obsolete_moderator_delivery (revision, job)))
;;

let deliver_standalone_completion t ~revision ~job ~current_capabilities ~policy =
  Eio.Cancel.protect (fun () ->
    call t (Deliver_standalone_completion (revision, job, current_capabilities, policy)))
;;

let consume_initial_notifications t ~operation_id plan =
  call t (Consume_initial_notifications (operation_id, plan))
;;

let cancel_operation t ~attachment_id ~operation_id =
  call t ~priority:Priority (Cancel_operation (attachment_id, operation_id))
;;

let cancel_operation_with_command_audit t ~command_audit ~attachment_id ~operation_id =
  call
    t
    ~priority:Priority
    ~command_audit
    (Cancel_operation (attachment_id, operation_id))
;;

let request_permission t ~permission ~timeout_seconds ~fallback =
  let open Result.Let_syntax in
  let%bind response = call t (Open_permission (permission, timeout_seconds, fallback)) in
  Ok (Eio.Promise.await response)
;;

let request_permission_with_review_fallback
      t
      ~permission
      ~timeout_seconds
      ~fallback
      ~review_on_timeout
  =
  request_permission_with_review_internal
    t
    ~permission
    ~timeout_seconds
    ~fallback
    ~review_on_timeout:(Some review_on_timeout)
;;

let request_review t ~permission ~review = request_review_internal t ~permission ~review

let resolve_permission_as_system t ~permission_id ~permission_generation ~choice ~reason =
  call
    t
    ~priority:Priority
    (Resolve_permission_system (permission_id, permission_generation, choice, reason))
;;

let expire_permission t ~permission_id ~permission_generation ~fallback =
  call
    t
    ~priority:Priority
    (Expire_permission (permission_id, permission_generation, fallback))
;;

let respond_permission
      t
      ~attachment_id
      ~principal_id
      ~permission_id
      ~permission_generation
      ~choice
      ~reason
  =
  call
    t
    ~priority:Priority
    (Respond_permission
       (attachment_id, principal_id, permission_id, permission_generation, choice, reason))
;;

let respond_permission_with_command_audit
      t
      ~command_audit
      ~attachment_id
      ~principal_id
      ~permission_id
      ~permission_generation
      ~choice
      ~reason
  =
  call
    t
    ~priority:Priority
    ~command_audit
    (Respond_permission
       (attachment_id, principal_id, permission_id, permission_generation, choice, reason))
;;

let revoke_grant t ~attachment_id ~grant_id ~reason =
  call t (Revoke_grant (attachment_id, grant_id, reason))
;;

let revoke_grant_with_command_audit t ~command_audit ~attachment_id ~grant_id ~reason =
  call t ~command_audit (Revoke_grant (attachment_id, grant_id, reason))
;;

let change_job t ~attachment_id job = call t (Change_job (attachment_id, job))

let capture_inference_target t ~target ~limits =
  call t (Capture_inference_target (target, limits))
;;

let capture_model_job_source t ~job_id ~generation ~target ~limits =
  call t (Capture_model_job_source (job_id, generation, target, limits))
;;

let capture_recipe_target t ~job_id ~generation ~attempt ~target ~limits =
  call t (Capture_recipe_target (job_id, generation, attempt, target, limits))
;;

let initialization_model_admission t ~scope =
  model_job_port t (Inference_initialization scope)
;;

let initialization_model_allocation t ~scope ~job =
  inference_port t (Inference_initialization_job (scope, job))
;;

let add_initialization_model_job t ~scope job =
  call t (Add_initialization_model_job (scope, job))
;;

let start_initialization_model_job t ~scope job =
  call t (Start_initialization_model_job (scope, job))
;;

let claim_initialization_model_job t ~scope ~job_id ~generation =
  Eio.Cancel.protect (fun () ->
    call t (Claim_initialization_model_job (scope, job_id, generation)))
;;

let initialization_model_job_is_current t ~scope ~job_id ~generation ~attempt =
  call t (Initialization_model_job_is_current (scope, job_id, generation, attempt))
;;

let capture_initialization_recipe_target
      t
      ~scope
      ~job_id
      ~generation
      ~attempt
      ~target
      ~limits
  =
  call
    t
    (Capture_initialization_recipe_target
       (scope, job_id, generation, attempt, target, limits))
;;

let complete_initialization_model_job t ~scope ~job_id ~generation ~attempt outcome =
  call
    t
    ~priority:Priority
    (Complete_initialization_model_job (scope, job_id, generation, attempt, outcome))
;;

let add_job t job = call t (Add_job job)
let read_job t ~job_id = call t (Read_job job_id)

let publish_job_progress t ~invocation_id progress =
  match Job_progress_buffer.valid progress with
  | false -> ()
  | true ->
    let _promise, resolver = Eio.Promise.create () in
    ignore
      (Mailbox.try_push
         t.mailbox
         ~priority:Transient
         (Pack (None, Publish_job_progress (invocation_id, progress), resolver))
       : bool)
;;

let claim_job t ~job_id ~generation = call t (Claim_job (job_id, generation))

let complete_job t ~job_id ~generation ~attempt outcome =
  call t ~priority:Priority (Complete_job (job_id, generation, attempt, outcome))
;;

let complete_background_job t ~job_id ~generation ~attempt outcome =
  call
    t
    ~priority:Priority
    (Complete_background_job (job_id, generation, attempt, outcome))
;;

let defer_background_job t ~job_id ~generation ~attempt dependency =
  call
    t
    ~priority:Priority
    (Defer_background_job (job_id, generation, attempt, dependency))
;;

let refresh_background_job t ~job_id ~generation ~attempt =
  call t ~priority:Priority (Refresh_background_job (job_id, generation, attempt))
;;

let recover_background_results t ~max_count ~max_total_bytes =
  call t ~priority:Priority (Recover_background_results (max_count, max_total_bytes))
;;

let enqueue_run_job_delivery t ~delivery ~before ~after =
  call t (Enqueue_run_job_delivery (delivery, before, after))
;;

let deliver_job
      ?expected
      ?expected_job
      ?moderator_manager
      t
      ~job_id
      ~generation
      ~moderator_snapshot
  =
  call
    t
    (Deliver_job
       (job_id, generation, expected, expected_job, moderator_snapshot, moderator_manager))
;;

let cancel_job_internal t ~job_id = call t ~priority:Priority (Cancel_job_internal job_id)
let authorize_writer t ~attachment_id = call t (Authorize_writer attachment_id)

let cancel_job
      t
      ?command_audit
      ?expected_generation
      ?expected_attempt
      ~attachment_id
      ~job_id
      ()
  =
  call
    t
    ~priority:Priority
    ?command_audit
    (Cancel_job (attachment_id, job_id, expected_generation, expected_attempt))
;;

let cancel_job_internal_with_command_audit t ~command_audit ~job_id =
  call t ~priority:Priority ~command_audit (Cancel_job_internal job_id)
;;

let interrupt_job t ~job_id ~generation ~attempt ~reason =
  call t ~priority:Priority (Interrupt_job (job_id, generation, attempt, reason))
;;

let change_schedule t ~attachment_id ~event schedule =
  call t (Change_schedule (attachment_id, event, schedule))
;;

let change_schedule_with_command_audit t ~command_audit ~attachment_id ~event schedule =
  call t ~command_audit (Change_schedule (attachment_id, event, schedule))
;;

let add_schedule t schedule = call t (Add_schedule schedule)

let callback_schedule_services t ~scope : Runtime_builder.schedule_services =
  let message (error : Agent_protocol.Error.t) = error.message in
  { after_ms =
      (fun ~delay_ms ~payload ->
        call t (Callback_schedule_after_ms (scope, delay_ms, payload))
        |> Result.map ~f:(fun schedule ->
          Agent_protocol.Id.Schedule.to_string schedule.Agent_protocol.Schedule.id)
        |> Result.map_error ~f:message)
  ; cancel =
      (fun ~id ->
        let open Result.Let_syntax in
        let%bind schedule_id =
          Agent_protocol.Id.Schedule.of_string id |> Result.map_error ~f:message
        in
        call t (Callback_schedule_cancel (scope, schedule_id))
        |> Result.map ~f:ignore
        |> Result.map_error ~f:message)
  }
;;

let cancel_schedule t ?command_audit ?expected_generation ~attachment_id ~schedule_id () =
  call
    t
    ~priority:Priority
    ?command_audit
    (Cancel_schedule (attachment_id, schedule_id, expected_generation))
;;

let cancel_schedule_internal t ~schedule_id =
  call t (Cancel_schedule_internal schedule_id)
;;

let claim_schedule t ~schedule_id ~generation =
  call t (Claim_schedule (schedule_id, generation))
;;

let retry_schedule t ~schedule_id ~generation =
  call t ~priority:Priority (Retry_schedule (schedule_id, generation))
;;

let complete_schedule
      ?expected
      ?expected_schedule
      ?moderator_manager
      t
      ~schedule_id
      ~generation
      ~moderator_snapshot
  =
  call
    t
    (Complete_schedule
       ( schedule_id
       , generation
       , expected
       , expected_schedule
       , moderator_snapshot
       , moderator_manager ))
;;

let fail_schedule t ~schedule_id ~generation failure =
  call t (Fail_schedule (schedule_id, generation, failure))
;;

let skip_schedule t ~schedule_id ~generation =
  call t (Skip_schedule (schedule_id, generation))
;;

let claim_idle_moderator t = call t Claim_idle_moderator
let apply_observation_follow_up t = call t Apply_observation_follow_up
let apply_moderator_follow_up t = call t Apply_observation_follow_up

let invocation_granted t ~tool_name ~identity_digest =
  call t (Invocation_granted (tool_name, identity_digest))
;;

let complete_idle_moderator t drain =
  call t ~priority:Priority (Complete_idle_moderator drain)
;;

let fail_idle_moderator t failure =
  call t ~priority:Priority (Fail_idle_moderator failure)
;;

let attach_with_snapshot t ~principal_id ~reclaim_token ~mode ~subscribe =
  call t (Attach (mode, subscribe, principal_id, reclaim_token))
;;

let attach_with_snapshot_and_command_audit
      t
      ~command_audit
      ~principal_id
      ~reclaim_token
      ~mode
      ~subscribe
  =
  call t ~command_audit (Attach (mode, subscribe, principal_id, reclaim_token))
;;

let attach t ~mode ~subscribe =
  Result.map
    (attach_with_snapshot t ~principal_id:None ~reclaim_token:None ~mode ~subscribe)
    ~f:(fun (attachment, subscriber, _, _) -> attachment, subscriber)
;;

let detach t attachment_id = call t ~priority:Priority (Detach attachment_id)

let detach_with_command_audit t ~command_audit attachment_id =
  call t ~priority:Priority ~command_audit (Detach attachment_id)
;;

let renew_owner t ~attachment_id ~lease_generation =
  call t ~priority:Priority (Renew_owner (attachment_id, lease_generation))
;;

let renew_owner_with_command_audit t ~command_audit ~attachment_id ~lease_generation =
  call t ~priority:Priority ~command_audit (Renew_owner (attachment_id, lease_generation))
;;

let publish_recoverable t event = broadcast_recoverable t event
let checkpoint t ~persist = call t ~priority:Priority (Checkpoint persist)
let with_quiescent_state t ~f = call t ~priority:Priority (Quiescent_checkpoint f)

let prepare_ingress_submission
      t
      ~source
      ~producer
      ~registration_id
      ~namespace
      ~key
      ~payload
  =
  call
    t
    (Prepare_ingress_submission
       (source, producer, registration_id, namespace, key, payload))
;;

let commit_ingress_submission t proposal ~before ~snapshot =
  call t (Commit_ingress_submission (proposal, before, snapshot))
;;

module For_testing = struct
  let deliver_compaction_result t ~operation_id ~history =
    call t ~priority:Priority (Compaction_terminal (operation_id, Compacted history))
  ;;
end

let shutdown t =
  ignore (call t ~priority:Priority Shutdown : (unit, Agent_protocol.Error.t) result)
;;

let update_metadata t ?command_audit ~attachment_id ~expected_metadata_revision ~patch () =
  call
    t
    ?command_audit
    (Update_metadata (attachment_id, expected_metadata_revision, patch))
;;

let configuration t = call t Configuration
let set_configuration_policy t policy = call t (Set_configuration_policy policy)

let update_configuration t ?command_audit request =
  let open Result.Let_syntax in
  let%bind basis = call t (Prepare_configuration_update request) in
  let%bind validated = Configuration_update.validate basis in
  call t ?command_audit (Commit_configuration_update validated)
;;

let set_organization_admission t admission = call t (Set_organization_admission admission)

let update_organization t ?command_audit ~principal request =
  call t ?command_audit (Update_organization (principal, request))
;;

let edit_history t ?command_audit request = call t ?command_audit (Edit_history request)

let continue_history t ?command_audit request =
  call t ?command_audit (Continue_history request)
;;

let begin_lifecycle t ~attachment_id ~expected_generation ~expected_revision =
  call
    t
    ~priority:Priority
    (Begin_lifecycle (attachment_id, expected_generation, expected_revision))
;;

let lifecycle_state t fence = call t ~priority:Priority (Lifecycle_state fence)
let retire_lifecycle t fence = call t ~priority:Priority (Retire_lifecycle fence)
let abort_lifecycle t fence = call t ~priority:Priority (Abort_lifecycle fence)
let check_runtime_admission t = call t ~priority:Priority Check_runtime_admission

let goal_command t ~actor ~authorize ~session ~request =
  call t (Apply_goal_command (actor, authorize, session, request))
;;

let read_goals t ~actor ~authorize = call t (Read_goals (actor, authorize))

let instruction_selection_current t (basis : Session_state.t) =
  (not t.stopped)
  && Int.equal t.state.identity.generation basis.identity.generation
  && Option.equal phys_equal t.state.instruction_selection basis.instruction_selection
;;

let instruction_command t ~admission ~source ~request =
  (* Native instruction receipts are journaled with selection state. They are not
     external server-request idempotency records and cannot mint command_audit. *)
  call t (Apply_instruction_command (admission, source, request))
;;

let set_recurrence_controller t controller = call t (Set_recurrence_controller controller)

let recurrence_command t ~actor ~authorize ~request =
  call t (Apply_recurrence_command (actor, authorize, request))
;;

let read_recurrences t ~actor ~authorize = call t (Read_recurrences (actor, authorize))

let read_recurrence_planning t ~actor ~authorize =
  call t (Read_recurrence_planning (actor, authorize))
;;

let set_recurrence_planning_host t host = call t (Set_recurrence_planning_host host)

(* Only recurrence acquisition/cleanup mask caller cancellation. Every sweep
   request observes owner exit, since the scheduler may outlive that actor.
   [Fiber.first] cancels and joins the losing waiter; termination grants no ACK. *)
let recurrence_call t ?(priority = Mailbox.Normal) request =
  let unavailable () =
    Error (error Server_shutting_down "recurrence actor has terminated")
  in
  match Eio.Promise.peek t.finished with
  | Some () -> unavailable ()
  | None ->
    Eio.Fiber.first
      (fun () -> call t ~priority request)
      (fun () ->
         Eio.Promise.await t.finished;
         unavailable ())
;;

let reserve_recurrence_child t ~actor ~authorize ~occurrence ~expected_control_revision =
  recurrence_call
    t
    (Reserve_recurrence_child (actor, authorize, occurrence, expected_control_revision))
;;

let check_recurrence_creation t permit =
  recurrence_call t (Check_recurrence_creation permit)
;;

let record_recurrence_execution t ~actor ~authorize ~intent ~attempt =
  recurrence_call t (Record_recurrence_execution (actor, authorize, intent, attempt))
;;

let observe_recurrence_run t ~actor ~authorize ~intent ~attempt =
  recurrence_call t (Observe_recurrence_run (actor, authorize, intent, attempt))
;;

let import_recurrence_execution t ~actor ~authorize ~automatic_policy ~observation =
  recurrence_call
    t
    (Import_recurrence_execution (actor, authorize, automatic_policy, observation))
;;

let acquire_recurrence_activation t ~actor ~authorize ~observation =
  recurrence_call t (Acquire_recurrence_activation (actor, authorize, observation))
;;

let check_recurrence_activation t custody ~observation =
  recurrence_call t (Check_recurrence_activation (custody, observation))
;;

let release_recurrence_activation t custody =
  if
    Activation_custody.belongs_to custody ~owner:t.recurrence_activation_owner
    && ((not (Activation_custody.is_live custody))
        || Option.is_some (Eio.Promise.peek t.finished))
  then (
    Activation_custody.close custody;
    Ok ())
  else recurrence_call t ~priority:Priority (Release_recurrence_activation custody)
;;

let verify_recurrence_child t ~reservation =
  recurrence_call t (Verify_recurrence_child reservation)
;;

let link_recurrence_child t ~permit ~child =
  recurrence_call t (Link_recurrence_child (permit, child))
;;

let sweep_recurrences t =
  let open Result.Let_syntax in
  let%bind ids = recurrence_call t Recurrence_due_ids in
  List.fold ids ~init:(Ok ()) ~f:(fun previous id ->
    let outcome =
      let%bind acquired =
        Eio.Cancel.protect (fun () -> recurrence_call t (Begin_recurrence_due id))
      in
      match acquired with
      | None -> Ok ()
      | Some preparation ->
        let release () =
          Eio.Cancel.protect (fun () ->
            recurrence_call t ~priority:Priority (Release_recurrence_due preparation))
        in
        let outcome =
          match
            (fun () ->
               Eio.Fiber.check ();
               let%bind prepared = Recurrence_runtime.prepare preparation in
               recurrence_call t (Finish_recurrence_due prepared))
              ()
          with
          | result -> result
          | exception exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            (* Cleanup cannot replace the original planner/cancellation failure.
               Neither a rejected nor a failed release implies any commit ACK. *)
            (try ignore (release () : (unit, Agent_protocol.Error.t) result) with
             | _ -> ());
            Exn.raise_with_original_backtrace exn backtrace
        in
        let%bind () = release () in
        outcome
    in
    (* Each retained ID is attempted once: one unavailable/capacity-saturated
       definition cannot starve its unrelated controller definitions. *)
    match previous with
    | Error _ -> previous
    | Ok () -> outcome)
;;
