open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Row = Store.Command.Row
module Root = Developer_root_owner
module Lifecycle = Developer_command_lifecycle
module Job_scope = Agent_session.Developer_job_scope
module Broker = Shell_runtime.Approval_broker
module Approval = Shell_access.Approval

let error code message = P.Error.create code ~message ~retryable:false ()
let stale () = error P.Error.Lease_stale "developer approval owner changed"
let denied () = error P.Error.Permission_denied "developer approval denied"
let limited () = error P.Error.Resource_limit "developer approval limit reached"
let invalid message = Error (P.Error.invalid_request message)

let store_error = function
  | Store.Error.Foreign_or_stale -> stale ()
  | Conflict | Prewrite_conflict ->
    error P.Error.Conflict "developer approval state changed"
  | Limit_exceeded -> limited ()
  | Closed -> error P.Error.Invalid_state "developer approval store is closed"
  | Store _ ->
    P.Error.create
      P.Error.Persistence_error
      ~message:"developer approval store is unavailable"
      ~retryable:true
      ()
;;

module Limits = struct
  type t =
    { max_pending : int
    ; max_request_bytes : int
    ; max_display_bytes : int
    ; max_decision_reason_bytes : int
    ; wait_seconds : float
    ; wait_ms : int
    ; broker : Broker.Limits.t
    }

  let max_pending t = t.max_pending
  let max_display_bytes t = t.max_display_bytes
  let max_decision_reason_bytes t = t.max_decision_reason_bytes

  let create
        ~max_pending
        ~max_request_bytes
        ~max_display_bytes
        ~max_decision_reason_bytes
        ~wait_seconds
    =
    if
      max_request_bytes < 2048 || max_display_bytes <= 0 || max_decision_reason_bytes <= 0
    then invalid "developer approval byte limits must be positive"
    else (
      match Broker.Limits.create ~max_pending ~wait_seconds with
      | Error message -> invalid message
      | Ok broker ->
        let wait_ms = Int.of_float (Float.round_up (wait_seconds *. 1000.)) in
        Ok
          { max_pending
          ; max_request_bytes
          ; max_display_bytes
          ; max_decision_reason_bytes
          ; wait_seconds
          ; wait_ms
          ; broker
          })
  ;;
end

module Configuration = struct
  type t =
    { limits : Limits.t
    ; command_read : Operator_authorization.t -> Row.t -> bool
    ; command_respond : Operator_authorization.t -> Row.t -> bool
    ; job_read : Operator_authorization.t -> P.Developer_job.t -> bool
    ; job_respond : Operator_authorization.t -> P.Developer_job.t -> bool
    }

  let create ~limits ~command_read ~command_respond ~job_read ~job_respond =
    { limits; command_read; command_respond; job_read; job_respond }
  ;;
end

module Pending = struct
  type owner =
    | Command of Agent_store.Developer_command_receipt.Id.t
    | Claimed_job of
        { job_id : P.Id.Job.t
        ; attempt : int
        }

  type execution =
    | Native_invocation of P.Id.Invocation.t
    | Claimed_model_job of
        { job_id : P.Id.Job.t
        ; attempt : int
        }

  type t =
    { id : P.Id.Permission.t
    ; owner : owner
    ; execution : execution
    ; runtime_id : string
    ; manifest_sha256 : string
    ; redacted_display : string
    ; expires_at : P.Timestamp.t
    }

  let id t = t.id
  let owner t = t.owner
  let execution t = t.execution
  let runtime_id t = t.runtime_id
  let manifest_sha256 t = t.manifest_sha256
  let redacted_display t = t.redacted_display
  let expires_at t = t.expires_at
end

type decision =
  | Approve_once
  | Deny of string

type source_origin =
  | Started_command of
      { active : Lifecycle.Active.t
      ; started : Lifecycle.Active.Started.t
      ; actor : Operator_authorization.t
      ; row : Row.t
      }
  | Claimed_job of
      { active : Job_scope.active
      ; parent : Job_scope.Parent.t
      ; job : P.Developer_job.t
      }

type source_scope =
  { issuer : unit ref
  ; origin : source_origin
  ; live : bool ref
  }

type execution_origin =
  | Native of
      { terminal : Root.Native_terminal.t
      ; dispatched : P.Host_invocation.t
      }
  | Claimed_model of
      { active : Job_scope.active
      ; scope : Root.Model_inference.t
      }

type execution_scope =
  { issuer : unit ref
  ; origin : execution_origin
  ; live : bool ref
  }

type state =
  | Awaiting_pair of Lifecycle.Active.Preparing.t
  | Bound_root of Root.run
  | Closed

type retained =
  { ui_request : Broker.ui_request
  ; projection : Pending.t
  ; source : source_scope
  ; execution : execution_scope
  }

type core =
  { identity : unit ref
  ; store : Store.t
  ; configuration : Configuration.t
  ; on_change : (Row.t -> unit) option
  ; mutable state : state
  ; pending_by_broker : retained String.Table.t
  ; pending_by_public : retained String.Table.t
  ; mutex : Eio.Mutex.t
  }

type t =
  { core : core
  ; broker : Broker.t
  ; approval_store : Approval.store
  ; shell_owner_label : string
  ; mutable release_hook : Eio.Switch.hook
  }

let source_key : source_scope option Eio.Fiber.key = Eio.Fiber.create_key ()
let execution_key : execution_scope option Eio.Fiber.key = Eio.Fiber.create_key ()
let with_lock core f = Eio.Mutex.use_rw ~protect:true core.mutex f

let bound_root core =
  match core.state with
  | Bound_root root when Root.issued_by_store root core.store -> Ok root
  | Awaiting_pair _ | Bound_root _ | Closed -> Error (stale ())
;;

let source_now core =
  match Option.join (Eio.Fiber.get source_key) with
  | Some scope when phys_equal scope.issuer core.identity && !(scope.live) -> Ok scope
  | None | Some _ -> Error (stale ())
;;

let execution_now core =
  match Option.join (Eio.Fiber.get execution_key) with
  | Some scope when phys_equal scope.issuer core.identity && !(scope.live) -> Ok scope
  | None | Some _ -> Error (stale ())
;;

let execution_check core (scope : execution_scope) =
  let open Result.Let_syntax in
  let%bind root = bound_root core in
  if (not !(scope.live)) || not (phys_equal scope.issuer core.identity)
  then Error (stale ())
  else (
    match scope.origin with
    | Native { terminal; _ } ->
      if
        Root.Native_terminal.issued_by_root terminal ~root
        && Root.Native_terminal.effect_parent_live_now terminal
      then Ok ()
      else Error (stale ())
    | Claimed_model { active; scope } ->
      Root.Model_inference.check_claimed_approval root ~scope ~active)
;;

let source_check core (scope : source_scope) =
  let open Result.Let_syntax in
  let%bind root = bound_root core in
  if (not !(scope.live)) || not (phys_equal scope.issuer core.identity)
  then Error (stale ())
  else (
    match scope.origin with
    | Started_command { active; started; actor; _ } ->
      Lifecycle.Active.check_started_now active ~started ~root ~actor
    | Claimed_job { parent; _ } ->
      let owner = Root.owner root in
      let capture_fingerprint = Root.Source.fingerprint (Root.source root) in
      let%bind () = Job_scope.Parent.check parent ~owner ~capture_fingerprint in
      Job_scope.Parent.check_now parent ~owner ~capture_fingerprint)
;;

let source_check_now core (scope : source_scope) =
  let open Result.Let_syntax in
  let%bind root = bound_root core in
  if (not !(scope.live)) || not (phys_equal scope.issuer core.identity)
  then Error (stale ())
  else (
    match scope.origin with
    | Started_command { active; started; actor; _ } ->
      Lifecycle.Active.check_started_now active ~started ~root ~actor
    | Claimed_job { parent; _ } ->
      Job_scope.Parent.check_now
        parent
        ~owner:(Root.owner root)
        ~capture_fingerprint:(Root.Source.fingerprint (Root.source root)))
;;

let local_admit core =
  let open Result.Let_syntax in
  let%bind source = source_now core in
  let%bind execution = execution_now core in
  let%bind () = source_check_now core source in
  let%bind root = bound_root core in
  let%bind () =
    match execution.origin with
    | Native { terminal; _ } ->
      if
        Root.Native_terminal.issued_by_root terminal ~root
        && Root.Native_terminal.effect_parent_live_now terminal
      then Ok ()
      else Error (stale ())
    | Claimed_model { active; _ } ->
      (match source.origin with
       | Claimed_job { active = source_active; _ } when phys_equal active source_active ->
         Ok ()
       | Started_command _ | Claimed_job _ -> Error (stale ()))
  in
  if not !(execution.live) then Error (stale ()) else Ok ()
;;

let live_execution core =
  let open Result.Let_syntax in
  let%bind source = source_now core in
  let%bind execution = execution_now core in
  let%bind () = source_check core source in
  let%map () = execution_check core execution in
  source, execution
;;

let request_fits (limits : Limits.t) request =
  let approval = request.Broker.request in
  let context = approval.context in
  let identity = approval.identity in
  let executable = context.executable in
  let capabilities = context.capabilities in
  let command = context.command in
  let remaining = ref limits.max_request_bytes in
  let charge bytes =
    if bytes < 0 || bytes > !remaining
    then false
    else (
      remaining := !remaining - bytes;
      true)
  in
  let text value = charge 16 && charge (String.length value) in
  let optional value = Option.value_map value ~default:true ~f:text in
  let list values = List.for_all values ~f:text in
  let charge_effect = function
    | Shell_access.Effect.Read_path path | Write_path path | Unknown path -> text path
    | Network | Child_processes | Arbitrary_code | Privilege_change -> charge 16
  in
  (* Charge fixed numeric/variant fields, every retained variable text field,
     and per-field/list framing before publishing or copying any request. *)
  charge 2048
  && text request.id
  && text request.runtime_id
  && text request.manifest_sha256
  && text context.request_id
  && text context.runtime_id
  && text context.manifest_sha256
  && text command.program
  && list command.arguments
  && text executable.requested
  && text executable.path
  && text executable.canonical_path
  && text executable.fingerprint.sha256
  && text context.cwd
  && Array.for_all context.environment ~f:text
  && list capabilities.read_roots
  && list capabilities.write_roots
  && List.for_all context.effects ~f:charge_effect
  && list context.policy_matches
  && optional context.policy_action
  && optional context.session_id
  && optional context.stdin_sha256
  && optional context.script_sha256
  && optional context.script_preview
  && (match context.origin with
      | Shell_access.Context.Tool | Moderator -> charge 16
      | Host label -> text label)
  && text approval.policy.reason
  && List.for_all approval.policy.matches ~f:(fun match_ ->
    text match_.rule_id && optional match_.reason)
  && text approval.display_command
  && optional approval.rationale
  && text identity.manifest_sha256
  && text identity.runtime_id
  && text identity.command_hash
  && text identity.executable_sha256
  && list identity.argv
  && text identity.cwd_sha256
  && text identity.environment_sha256
  && optional identity.stdin_sha256
  && optional identity.script_sha256
  && List.for_all request.scopes ~f:(fun _ -> charge 16)
  && String.length approval.display_command <= limits.max_display_bytes
;;

let broker_admit core request =
  if not (request_fits core.configuration.limits request)
  then Error "developer approval request limit reached"
  else (
    match local_admit core with
    | Ok () -> Ok ()
    | Error _ -> Error "developer approval source is unavailable")
;;

let pending_owner = function
  | Started_command { row; _ } -> Pending.Command (Row.id row)
  | Claimed_job { parent; _ } ->
    Pending.Claimed_job
      { job_id = Job_scope.Parent.job_id parent
      ; attempt = Job_scope.Parent.attempt parent
      }
;;

let project_execution = function
  | Native { dispatched; _ } ->
    Pending.Native_invocation (P.Host_invocation.context dispatched).id
  | Claimed_model { active; _ } ->
    let job = Store.Claim.job (Job_scope.claim active) in
    Pending.Claimed_model_job
      { job_id = P.Developer_job.id job; attempt = (P.Developer_job.state job).attempt }
;;

let notify_live_change core (source : source_scope) =
  match source.origin with
  | Started_command { row; _ }
    when Store.Command.Receipt.Kind.equal
           (Row.kind row)
           Store.Command.Receipt.Kind.Live_run ->
    Option.iter core.on_change ~f:(fun notify -> notify row)
  | Started_command _ | Claimed_job _ -> ()
;;

let broker_on_pending core (request : Broker.ui_request) =
  match source_now core, execution_now core with
  | Ok source_scope, Ok execution_scope ->
    let deadline_wall =
      let now = P.Timestamp.now () in
      match P.Timestamp.add_ms now core.configuration.limits.wait_ms with
      | Ok later -> later
      | Error _ -> now
    in
    let projection =
      Pending.
        { id = P.Id.Permission.create ()
        ; owner = pending_owner source_scope.origin
        ; execution = project_execution execution_scope.origin
        ; runtime_id = request.runtime_id
        ; manifest_sha256 = request.manifest_sha256
        ; redacted_display = request.request.display_command
        ; expires_at = deadline_wall
        }
    in
    let retained =
      { ui_request = request
      ; projection
      ; source = source_scope
      ; execution = execution_scope
      }
    in
    with_lock core (fun () ->
      Hashtbl.set core.pending_by_broker ~key:request.id ~data:retained;
      Hashtbl.set
        core.pending_by_public
        ~key:(P.Id.Permission.to_string projection.id)
        ~data:retained);
    notify_live_change core source_scope
  | Error _, _ | _, Error _ ->
    (* An entered scope cannot normally expire between broker admission and
       this callback on its own fiber. If cancellation did so, the bounded
       broker's waiter removes the unprojected request on unwind. *)
    ()
;;

let broker_on_finished core (request : Broker.ui_request) =
  let removed =
    with_lock core (fun () ->
      match Hashtbl.find core.pending_by_broker request.id with
      | Some retained when phys_equal retained.ui_request request ->
        Hashtbl.remove core.pending_by_broker request.id;
        Hashtbl.remove
          core.pending_by_public
          (P.Id.Permission.to_string retained.projection.id);
        Some retained.source
      | None | Some _ -> None)
  in
  Option.iter removed ~f:(notify_live_change core)
;;

let close t =
  if not (phys_equal t.release_hook Eio.Switch.null_hook)
  then (
    ignore (Eio.Switch.try_remove_hook t.release_hook : bool);
    t.release_hook <- Eio.Switch.null_hook);
  t.core.state <- Closed;
  Broker.close t.broker
;;

let create_with_change_option ~sw ~env ~store ~preparing ~configuration ~on_change =
  let identity = ref () in
  let core =
    { identity
    ; store
    ; configuration
    ; on_change
    ; state = Awaiting_pair preparing
    ; pending_by_broker = String.Table.create ()
    ; pending_by_public = String.Table.create ()
    ; mutex = Eio.Mutex.create ()
    }
  in
  let broker =
    Broker.create_bounded
      ~clock:env#mono_clock
      ~limits:configuration.limits.broker
      ~admit:(broker_admit core)
      ~on_pending:(broker_on_pending core)
      ~on_finished:(broker_on_finished core)
      ()
  in
  let approval_store =
    Approval.create_store
      ~lookup:(fun ~now:_ ~session_id:_ _ -> Ok false)
      ~remember:(fun ~session_id:_ _ scope _ ->
        match scope with
        | Approval.Once -> Ok ()
        | Exact_session _ | Prefix_session _ | Durable_exact _ ->
          Error "developer shell grants are limited to one invocation")
      ()
  in
  let shell_owner_label =
    "developer-approval-" ^ P.Id.Permission.to_string (P.Id.Permission.create ())
  in
  let owner =
    { core
    ; broker
    ; approval_store
    ; shell_owner_label
    ; release_hook = Eio.Switch.null_hook
    }
  in
  owner.release_hook <- Eio.Switch.on_release_cancellable sw (fun () -> close owner);
  Ok owner
;;

let create ~sw ~env ~store ~preparing ~configuration =
  create_with_change_option ~sw ~env ~store ~preparing ~configuration ~on_change:None
;;

let create_with_change ~sw ~env ~store ~preparing ~configuration ~on_change =
  create_with_change_option
    ~sw
    ~env
    ~store
    ~preparing
    ~configuration
    ~on_change:(Some on_change)
;;

let bind_root t ~root ~started =
  let result =
    match t.core.state with
    | Awaiting_pair preparing ->
      if
        Lifecycle.Active.Preparing.issued_started preparing started
        && Lifecycle.Active.Started.issued_by_root started root
        && Root.issued_by_store root t.core.store
      then (
        t.core.state <- Bound_root root;
        Ok ())
      else Error (stale ())
    | Bound_root _ | Closed -> Error (stale ())
  in
  if Result.is_error result then close t;
  result
;;

let issued_by_root t root =
  match t.core.state with
  | Bound_root current ->
    phys_equal current root && Root.issued_by_store root t.core.store
  | Awaiting_pair _ | Closed -> false
;;

let with_source_scope core origin f =
  let scope : source_scope = { issuer = core.identity; origin; live = ref true } in
  Exn.protect
    ~finally:(fun () -> scope.live := false)
    ~f:(fun () -> Eio.Fiber.with_binding source_key (Some scope) f)
;;

let with_execution_scope core origin f =
  let scope : execution_scope = { issuer = core.identity; origin; live = ref true } in
  Exn.protect
    ~finally:(fun () -> scope.live := false)
    ~f:(fun () -> Eio.Fiber.with_binding execution_key (Some scope) f)
;;

let with_started_command t ~active ~started ~actor f =
  let open Result.Let_syntax in
  let%bind root = bound_root t.core in
  let%bind () = Lifecycle.Active.check_started_now active ~started ~root ~actor in
  let row = Lifecycle.View.row (Lifecycle.Active.view active) in
  with_source_scope t.core (Started_command { active; started; actor; row }) f
;;

let with_claimed_job t ~active f =
  let open Result.Let_syntax in
  let%bind root = bound_root t.core in
  let%bind parent = Root.job_parent root active in
  let job = Store.Claim.job (Job_scope.claim active) in
  with_source_scope t.core (Claimed_job { active; parent; job }) f
;;

let with_native_invocation t ~terminal ~dispatched f =
  let open Result.Let_syntax in
  let%bind root = bound_root t.core in
  let%bind _ = source_now t.core in
  if
    Root.Native_terminal.issued_by_root terminal ~root
    && Root.Native_terminal.effect_parent_live_now terminal
    && P.Id.Invocation.equal
         (P.Host_invocation.context (Root.Native_terminal.invocation terminal)).id
         (P.Host_invocation.context dispatched).id
  then with_execution_scope t.core (Native { terminal; dispatched }) f
  else Error (stale ())
;;

let with_claimed_model_inference t ~active ~scope f =
  let open Result.Let_syntax in
  let%bind root = bound_root t.core in
  let%bind () = Root.Model_inference.check_claimed_approval root ~scope ~active in
  with_claimed_job t ~active (fun () ->
    with_execution_scope
      t.core
      (Claimed_model { active; scope })
      (fun () ->
         let services () =
           let result =
             let open Result.Let_syntax in
             let%bind source = source_now t.core in
             let%bind execution = execution_now t.core in
             let%bind () = source_check t.core source in
             let%bind () = execution_check t.core execution in
             Ok
               Shell_runtime.Call_context.
                 { session_id = t.shell_owner_label
                 ; approval_store = t.approval_store
                 ; approval_provider = Broker.Callback t.broker
                 ; check =
                     (fun () ->
                       match live_execution t.core with
                       | Ok _ -> Ok ()
                       | Error error -> Error error.P.Error.message)
                 }
           in
           Result.map_error result ~f:(fun error -> error.P.Error.message)
         in
         Shell_runtime.Call_context.with_services services f))
;;

let provider t = Broker.Callback t.broker
let approval_store t = t.approval_store
let shell_owner_label t = t.shell_owner_label

let same_principal root actor =
  let responding = Operator_authorization.principal actor in
  let original = Operator_authorization.principal (Root.original_actor root) in
  Operator_authorization.is_current actor
  && Operator_authorization.is_current (Root.original_actor root)
  && P.Id.Principal.equal responding.id original.id
  && String.equal responding.authentication_kind original.authentication_kind
;;

let observe_root core actor =
  let open Result.Let_syntax in
  let%bind root = bound_root core in
  if not (same_principal root actor)
  then Error (denied ())
  else (
    let%bind result_root = Root.result_root root in
    let%bind observation =
      Store.Result_root.observe_latest result_root |> Result.map_error ~f:store_error
    in
    let checkpoint = Store.Observation.checkpoint observation in
    if
      (not
         (P.Execution_owner.equal_developer
            (Store.Checkpoint.owner checkpoint)
            (Root.owner root)))
      || (not
            (Chatml_source.Digest.equal
               (Store.Checkpoint.capture_fingerprint checkpoint)
               (Root.Source.fingerprint (Root.source root))))
      || not
           (Store.Checkpoint.phase checkpoint
            |> Store.Checkpoint.equal_phase Store.Checkpoint.Active)
    then Error (stale ())
    else (
      match bound_root core with
      | Ok current when phys_equal current root && same_principal root actor ->
        Ok (root, result_root, observation)
      | Ok _ | Error _ -> Error (stale ())))
;;

let observe_retained core actor retained ~respond =
  let open Result.Let_syntax in
  if not (!(retained.source.live) && !(retained.execution.live))
  then Error (stale ())
  else (
    let%bind root, result_root, observation = observe_root core actor in
    let%bind selected =
      match retained.source.origin with
      | Started_command { row; _ } ->
        let principal = (Operator_authorization.principal actor).id in
        let%map current =
          Store.Result_root.find_command_snapshot
            result_root
            ~principal
            ~key:(Row.key row)
          |> Result.map_error ~f:store_error
        in
        Option.bind current ~f:(fun current ->
          if
            Row.same_identity current row
            &&
            match Row.phase current with
            | Row.Phase.Execution_started -> true
            | Reserved | Waiting_permission | Terminal _ -> false
          then Some (`Command current)
          else None)
      | Claimed_job { job; parent; _ } ->
        let current = Store.find_job observation (P.Developer_job.id job) in
        Ok
          (Option.bind current ~f:(fun current ->
             if
               P.Developer_job.same_immutable current job
               && Int.equal
                    (P.Developer_job.state current).attempt
                    (Job_scope.Parent.attempt parent)
               &&
               match (P.Developer_job.state current).status with
               | P.Job.Running | Waiting_permission _ -> true
               | Queued
               | Waiting_completion _
               | Succeeded
               | Failed _
               | Cancelled
               | Interrupted _ -> false
             then Some (`Job current)
             else None))
    in
    let%bind selected =
      Option.value_map selected ~default:(Error (stale ())) ~f:(fun value -> Ok value)
    in
    let%bind final_root, _, final_observation = observe_root core actor in
    let%bind selected =
      match selected with
      | `Command row ->
        let principal = (Operator_authorization.principal actor).id in
        let%map current =
          Store.Result_root.find_command_snapshot
            result_root
            ~principal
            ~key:(Row.key row)
          |> Result.map_error ~f:store_error
        in
        Option.bind current ~f:(fun current ->
          if Row.same_identity current row then Some (`Command current) else None)
      | `Job job ->
        Ok
          (Option.bind
             (Store.find_job final_observation (P.Developer_job.id job))
             ~f:(fun current ->
               if
                 P.Developer_job.same_immutable current job
                 && Int.equal
                      (P.Developer_job.state current).attempt
                      (P.Developer_job.state job).attempt
               then Some (`Job current)
               else None))
    in
    let%bind selected =
      Option.value_map selected ~default:(Error (stale ())) ~f:(fun value -> Ok value)
    in
    if
      (not (phys_equal final_root root))
      || Result.is_error (bound_root core)
      || (not (!(retained.source.live) && !(retained.execution.live)))
      || (not (phys_equal retained.source.issuer core.identity))
      || (not (phys_equal retained.execution.issuer core.identity))
      || not (same_principal root actor)
    then Error (stale ())
    else (
      let allowed =
        match selected with
        | `Command row ->
          (match Row.phase row with
           | Row.Phase.Execution_started ->
             if respond
             then core.configuration.command_respond actor row
             else core.configuration.command_read actor row
           | Reserved | Waiting_permission | Terminal _ -> false)
        | `Job current ->
          (match (P.Developer_job.state current).status with
           | P.Job.Running | Waiting_permission _ ->
             if respond
             then core.configuration.job_respond actor current
             else core.configuration.job_read actor current
           | Queued
           | Waiting_completion _
           | Succeeded
           | Failed _
           | Cancelled
           | Interrupted _ -> false)
      in
      if allowed then Ok () else Error (denied ())))
;;

let same_pending core retained =
  with_lock core (fun () ->
    match
      Hashtbl.find
        core.pending_by_public
        (P.Id.Permission.to_string retained.projection.id)
    with
    | Some current -> phys_equal current retained
    | None -> false)
;;

let pending t ~actor =
  let open Result.Let_syntax in
  let%bind _root, _result_root, _observation = observe_root t.core actor in
  let requests = Broker.pending_all t.broker in
  let rec visit acc (requests : Broker.ui_request list) =
    match requests with
    | [] -> Ok (List.rev acc)
    | request :: rest ->
      let retained =
        with_lock t.core (fun () -> Hashtbl.find t.core.pending_by_broker request.id)
      in
      (match retained with
       | None -> visit acc rest
       | Some retained ->
         let%bind () = observe_retained t.core actor retained ~respond:false in
         if same_pending t.core retained
         then visit (retained.projection :: acc) rest
         else visit acc rest)
  in
  visit [] requests
;;

let resolve_authorized t ~actor ~id response =
  let open Result.Let_syntax in
  let retained =
    with_lock t.core (fun () ->
      Hashtbl.find t.core.pending_by_public (P.Id.Permission.to_string id))
  in
  let%bind retained =
    Option.value_map retained ~default:(Error (stale ())) ~f:(fun retained -> Ok retained)
  in
  let%bind () = observe_retained t.core actor retained ~respond:true in
  if not (same_pending t.core retained)
  then Error (stale ())
  else
    Broker.respond t.broker ~id:retained.ui_request.id response
    |> Result.map_error ~f:(function
      | Broker.Closed | Unknown_request _ | Duplicate_request _ -> stale ()
      | Capacity_exhausted | Admission_denied _ | Deadline_unavailable -> limited ())
;;

let respond t ~actor ~id decision =
  match decision with
  | Approve_once -> resolve_authorized t ~actor ~id Broker.Approve_once
  | Deny reason ->
    if
      String.is_empty reason
      || String.length reason > t.core.configuration.limits.max_decision_reason_bytes
    then invalid "developer approval denial reason is invalid"
    else resolve_authorized t ~actor ~id (Broker.Deny reason)
;;

let cancel t ~actor ~id =
  resolve_authorized t ~actor ~id (Broker.Deny "developer approval was cancelled")
;;
