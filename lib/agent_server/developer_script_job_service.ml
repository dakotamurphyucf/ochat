open! Core
module P = Agent_protocol
module O = Developer_root_owner
module D = Developer_result_disclosure
module K = Agent_session.Script_job_kernel
module R = Agent_session.Script_job_request
module Ops = Chat_response.Background_job_operations
module C = Chat_response.Tool_capability
module B = Chat_response.Background_request
module N = Agent_session.Native_tool_invocation
module Child = Developer_child_selection
module M = Chat_response.Managed_tool_registry
module H = P.Host_invocation
module Runtime = Chatml_host_runtime
module Value = Chatml.Chatml_value_codec
module Claimed = Agent_session.Claimed_child_runtime
module Deadline = Developer_claim_deadline

module Claim_budget = struct
  type t =
    { deadline : P.Timestamp.t
    ; control : Chatml.Chatml_lang.execution_control
    }

  let deadline t = t.deadline
  let control t = t.control
end

type t =
  { root : O.run
  ; queued_issuer : Developer_queued_issuer.t option
  ; disclosure : D.t
  ; env : Eio_unix.Stdenv.base
  ; current_actor : unit -> Operator_authorization.t
  ; prepare_source :
      parent:source_parent
      -> binding:C.binding
      -> request:B.t
      -> checkpoint:(unit -> unit)
      -> (B.Agent_artifact.t option, P.Error.t) result
  }

and source_parent = { scope : scope }

and physical =
  | Root of O.Action.t
  | Native of O.Native_terminal.t
  | Worker of O.Worker_action.t

and native_selection =
  { terminal : O.Native_terminal.t
  ; physical : O.Native_terminal.Selection.t
  ; acknowledge : unit -> unit
  ; mutable acknowledged : bool
  }

and issuing_source =
  | Root_source
  | Foreground_child of Child.t
  | Claimed_issuer of Claimed.t

and scope =
  { service : t
  ; physical : physical
  ; issuing_source : issuing_source
  ; worker_parent : Agent_session.Developer_job_scope.Parent.t option
  ; mutable selected : C.t
  ; mutable selected_now : unit -> (C.t, P.Error.t) result
  ; current_capabilities : unit -> (C.t, P.Error.t) result
  ; kernel : K.t
  ; check_parent_now : unit -> (unit, P.Error.t) result
  ; mutable managed_admitted : bool
  ; mutable frozen : (P.Invocation.outcome * native_selection) option
  }

let invalid message = P.Error.invalid_request message
let ( let* ) result f = Result.bind result ~f
let message result = Result.map_error result ~f:(fun error -> error.P.Error.message)
let kernel_result result = Result.map_error result ~f:invalid

let create ~root ~store ~disclosure ~env ~current_actor ~queued_issuer ~prepare_source =
  if not (O.issued_by_store root store && D.issued_by_store disclosure store)
  then Error (invalid "developer job services do not share the physical Store")
  else
    let* () =
      match queued_issuer with
      | None -> Ok ()
      | Some issuer ->
        if Developer_queued_issuer.issued_by_root issuer root
        then Ok ()
        else Error (invalid "queued issuer belongs to another physical Root")
    in
    Ok { root; queued_issuer; disclosure; env; current_actor; prepare_source }
;;

let issued_by_root t root = phys_equal t.root root
let queued_issuer t = t.queued_issuer

let current scope =
  let* witness = O.current scope.service.root in
  let* selected_now = scope.selected_now () in
  let* current_capabilities = scope.current_capabilities () in
  if
    (not (String.equal (C.fingerprint scope.selected) (C.fingerprint selected_now)))
    || not
         (List.for_all (C.references scope.selected) ~f:(fun reference ->
            Result.is_ok
              (C.resolve
                 current_capabilities
                 ~id:reference.id
                 ~fingerprint:reference.fingerprint)))
  then Error (invalid "developer job selection changed")
  else
    let* () = O.check_effect_now scope.service.root witness in
    scope.check_parent_now ()
;;

let completion_current scope terminal =
  let* () = O.Native_terminal.check_completion_now terminal in
  let* selected_now = scope.selected_now () in
  let* current_capabilities = scope.current_capabilities () in
  if
    (not (String.equal (C.fingerprint scope.selected) (C.fingerprint selected_now)))
    || not
         (List.for_all (C.references scope.selected) ~f:(fun reference ->
            Result.is_ok
              (C.resolve
                 current_capabilities
                 ~id:reference.id
                 ~fingerprint:reference.fingerprint)))
  then Error (invalid "developer job selection changed")
  else
    let* () = O.Native_terminal.check_completion_now terminal in
    scope.check_parent_now ()
;;

let check scope =
  let* () = kernel_result (K.check_open scope.kernel) in
  current scope
;;

let check_scope = check

module Source_parent = struct
  type t = source_parent

  type runtime_source =
    | Root_runtime
    | Selected_child_runtime of Child.t
    | Claimed_runtime of Claimed.t

  let check t ~binding =
    let* () = check_scope t.scope in
    let reference = C.reference binding in
    let* selected =
      C.resolve t.scope.selected ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map_error ~f:(fun error -> invalid error.C.message)
    in
    let* current_capabilities = t.scope.current_capabilities () in
    let* current =
      C.resolve current_capabilities ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map_error ~f:(fun error -> invalid error.C.message)
    in
    match C.native_implementation binding with
    | None -> Error (invalid "developer source binding is not native")
    | Some implementation ->
      if
        Option.value_map
          (C.native_implementation selected)
          ~default:false
          ~f:(phys_equal implementation)
        && Option.value_map
             (C.native_implementation current)
             ~default:false
             ~f:(phys_equal implementation)
      then check_scope t.scope
      else Error (invalid "developer source binding changed")
  ;;

  let launch t =
    let* () = check_scope t.scope in
    let* parent =
      match t.scope.physical, t.scope.worker_parent with
      | Root _, None -> Ok Chat_response.Source_launch.Developer_parent.Root_action
      | Native terminal, None ->
        Ok
          (Chat_response.Source_launch.Developer_parent.Native_invocation
             (H.context (O.Native_terminal.invocation terminal)).id)
      | Worker _, Some parent ->
        Ok
          (Chat_response.Source_launch.Developer_parent.Worker_claim
             { job_id = Agent_session.Developer_job_scope.Parent.job_id parent
             ; attempt = Agent_session.Developer_job_scope.Parent.attempt parent
             })
      | (Root _ | Native _), Some _ | Worker _, None ->
        Error (invalid "developer source parent is inconsistent")
    in
    Chat_response.Source_launch.developer ~owner:(O.owner t.scope.service.root) ~parent
  ;;

  let runtime_source t =
    match t.scope.issuing_source with
    | Root_source -> Root_runtime
    | Foreground_child child -> Selected_child_runtime child
    | Claimed_issuer issuer -> Claimed_runtime issuer
  ;;
end

let stage_physical scope ~checkpoint request =
  let checkpoint () =
    checkpoint ();
    K.checkpoint scope.kernel
  in
  match scope.physical with
  | Root action -> O.Action.stage_background action ~request ~checkpoint
  | Native terminal ->
    (match scope.issuing_source with
     | Root_source ->
       O.Native_terminal.stage_background
         terminal
         ~request
         ~checkpoint
         ~check_parent_now:scope.check_parent_now
     | Foreground_child child ->
       let* borrowed = N.borrow_selected () in
       let check_selected_parent_now () =
         let* () = scope.check_parent_now () in
         let* admission = Child.refresh child in
         Child.Admission.check_now admission
       in
       O.Native_terminal.stage_background_from_selected_borrow
         terminal
         ~borrowed
         ~request
         ~checkpoint
         ~check_parent_now:check_selected_parent_now
     | Claimed_issuer issuer ->
       let* borrowed = N.borrow_selected () in
       let check_claimed_parent_now () =
         let* () = scope.check_parent_now () in
         Claimed.check_now issuer
       in
       O.Native_terminal.stage_background_from_selected_borrow
         terminal
         ~borrowed
         ~request
         ~checkpoint
         ~check_parent_now:check_claimed_parent_now)
  | Worker action ->
    (match scope.issuing_source with
     | Root_source -> O.Worker_action.stage_background action ~request ~checkpoint
     | Claimed_issuer issuer ->
       O.Worker_action.stage_background_from_claimed_issuer
         action
         ~issuer
         ~request
         ~checkpoint
     | Foreground_child _ -> Error (invalid "foreground child cannot be a worker issuer"))
;;

let rollback_physical (scope : scope) id =
  match scope.physical with
  | Root action -> O.Action.rollback_staged action id
  | Native terminal -> O.Native_terminal.rollback_staged terminal id
  | Worker action -> O.Worker_action.rollback_staged action id
;;

let cleanup_physical physical id =
  match physical with
  | Root action -> O.Action.cleanup_staged action id
  | Native terminal -> O.Native_terminal.cleanup_staged terminal id
  | Worker action -> O.Worker_action.cleanup_staged action id
;;

let stage scope ~checkpoint request =
  let* () = check scope in
  let* () =
    if Option.is_some (B.issuer_source request)
    then Error (invalid "caller-supplied queued issuer cannot be staged")
    else Ok ()
  in
  let* request =
    match scope.issuing_source, scope.service.queued_issuer with
    | Root_source, _ -> Ok request
    | Claimed_issuer issuer, _ ->
      (match scope.physical with
       | Worker _ -> Ok request
       | Native _ | Root _ -> B.with_issuer_source request (Claimed.issuer_source issuer))
    | Foreground_child child, Some issuer ->
      let* source = Developer_queued_issuer.capture_child issuer ~child ~checkpoint in
      B.with_issuer_source request source
    | Foreground_child _, None ->
      Error (invalid "selected child queued issuer is unavailable")
  in
  let* () = check scope in
  let* () = B.validate_capabilities request ~capabilities:scope.selected in
  Eio.Cancel.protect (fun () ->
    let* id = stage_physical scope ~checkpoint request in
    let recorded =
      try `Returned (K.record_start scope.kernel id) with
      | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    match recorded with
    | `Returned (Ok ()) -> Ok id
    | `Returned (Error error) ->
      let* () = rollback_physical scope id in
      Error (invalid error)
    | `Raised (exn, backtrace) ->
      (try ignore (rollback_physical scope id : (unit, _) result) with
       | _ -> ());
      Exn.raise_with_original_backtrace exn backtrace)
;;

let accessible scope id =
  let* () = check scope in
  let* snapshot = O.Job_control.find_current scope.service.root id in
  let* job =
    match snapshot with
    | Some snapshot -> Ok (O.Job_control.job snapshot, snapshot)
    | None -> Error (invalid "developer job was not found in this run")
  in
  let job, snapshot = job in
  let* () = check scope in
  match P.Developer_job.kind job with
  | Async_tool ->
    let* request =
      B.of_json ~policy:(O.job_policy scope.service.root) (P.Developer_job.payload job)
    in
    let* () = B.validate_capabilities request ~capabilities:scope.selected in
    Ok (job, snapshot)
  | Model_call -> Ok (job, snapshot)
  | Nested_agent | Scheduled_event | Shell_process | Compaction ->
    Error (invalid "job is not generic script work")
;;

let view job =
  let state = P.Developer_job.state job in
  let status = P.Job.status_name state.status in
  let result_kind =
    match state.result with
    | None -> `Null
    | Some (Inline _) -> `String "inline"
    | Some (Artifact _) -> `String "artifact"
  in
  `Object
    [ "version", `Number "1"
    ; "id", P.Id.Job.to_json (P.Developer_job.id job)
    ; "status", `String status
    ; "progress", `Null
    ; "attempt", `Number (Int.to_string state.attempt)
    ; "created_at", P.Timestamp.to_json (P.Developer_job.created_at job)
    ; ( "completed_at"
      , Option.value_map state.completed_at ~default:`Null ~f:P.Timestamp.to_json )
    ; "completion", `Null
    ; "completion_kind", result_kind
    ]
;;

let read_result scope job =
  let state = P.Developer_job.state job in
  let* selection =
    D.Selection.create
      ~owner:(P.Developer_job.owner job)
      ~capture_fingerprint:(P.Developer_job.capture_fingerprint job)
      ~job_id:(P.Developer_job.id job)
      ~attempt:state.attempt
  in
  let* request =
    D.begin_request
      scope.service.disclosure
      ~original_actor:(O.original_actor scope.service.root)
      selection
  in
  let* disclosed =
    Eio.Switch.run (fun sw ->
      D.read
        scope.service.disclosure
        request
        ~sw
        ~current_actor:(scope.service.current_actor ()))
  in
  let* () = check scope in
  let* () =
    D.check_request_now
      scope.service.disclosure
      request
      ~current_actor:(scope.service.current_actor ())
  in
  match disclosed with
  | Pending _ | Terminal_without_result _ -> Ok `Null
  | Completion completion -> Ok (P.Completion.to_json completion)
;;

let handlers scope : Ops.handlers =
  let local result = message result in
  { start_tool =
      (fun ~name ~input ->
        let* () = check scope |> local in
        let* request =
          R.tool
            ~selected:scope.selected
            ~policy:(O.job_policy scope.service.root)
            ~prepare_source:(fun ~binding ~request ~checkpoint ->
              match C.implementation binding with
              | Managed _ -> Ok None
              | Native _ ->
                let parent = { scope } in
                let* () = Source_parent.check parent ~binding in
                let* artifact =
                  scope.service.prepare_source ~parent ~binding ~request ~checkpoint
                in
                let* () = Source_parent.check parent ~binding in
                Ok artifact)
            ~name
            ~input
            ~checkpoint:(fun () -> K.checkpoint scope.kernel)
          |> local
        in
        let* () = check scope |> local in
        stage scope ~checkpoint:ignore request |> local)
  ; start_script =
      (fun json ->
        let* () = check scope |> local in
        K.checkpoint scope.kernel;
        let* request =
          R.script
            ~env:scope.service.env
            ~selected:scope.selected
            ~policy:(O.job_policy scope.service.root)
            json
          |> local
        in
        K.checkpoint scope.kernel;
        let* () = check scope |> local in
        stage scope ~checkpoint:ignore request |> local)
  ; get =
      (fun id ->
        let* job, _ = accessible scope id |> local in
        Ok (view job))
  ; read_result =
      (fun id ->
        let* job, _ = accessible scope id |> local in
        read_result scope job |> local)
  ; cancel =
      (fun id ->
        let* _, snapshot = accessible scope id |> local in
        let* _ = O.Job_control.cancel_current scope.service.root snapshot |> local in
        Ok ())
  ; rollback_start = (fun id -> K.rollback_start scope.kernel id)
  }
;;

let model_spawn_operation ?control scope =
  let on_model_spawn _session ~recipe ~payload =
    let* () = check scope |> message in
    if not (String.equal recipe P.Model_spawn_request.recipe_name)
    then Error "Model.spawn: unsupported recipe"
    else (
      K.checkpoint scope.kernel;
      let* original = Value.export_json ?control payload in
      let* request = P.Model_spawn_request.decode original in
      let* () = check scope |> message in
      let checkpoint () = K.checkpoint scope.kernel in
      let admitted =
        match scope.physical with
        | Root action -> O.Action.admit_external_model action ~request ~checkpoint
        | Native terminal ->
          O.Native_terminal.admit_external_model
            terminal
            ~request
            ~checkpoint
            ~check_parent_now:scope.check_parent_now
        | Worker _ -> Error (invalid "bare worker scripts cannot spawn model jobs")
      in
      admitted |> message |> Result.map ~f:P.Id.Job.to_string)
  in
  let handlers = { Runtime.default_handlers with on_model_spawn } in
  match
    List.find (Runtime.default_operations ~handlers ()) ~f:(fun operation ->
      String.equal operation.name "Model.spawn")
  with
  | Some operation -> operation
  | None -> failwith "canonical Model.spawn operation is unavailable"
;;

module Scope = struct
  type t = scope

  let issued_by_terminal (t : t) terminal =
    match t.physical with
    | Native actual ->
      phys_equal actual terminal
      && O.Native_terminal.issued_by_root terminal ~root:t.service.root
      && Result.is_ok (K.check_open t.kernel)
    | Root _ | Worker _ -> false
  ;;

  let admit_managed (t : t) execution =
    match t.physical with
    | Root _ | Worker _ ->
      Error (invalid "managed native admission requires a native scope")
    | Native terminal ->
      let* () = check t in
      let* () =
        if O.Native_terminal.effect_parent_live_now terminal
        then Ok ()
        else Error (invalid "managed native callback has ended")
      in
      let physical = O.Native_terminal.invocation terminal in
      let admitted = M.owned_invocation execution in
      let reference = C.reference (M.owned_binding execution) in
      let* () =
        if
          t.managed_admitted
          || (not (H.equal_context (H.context physical) (H.context admitted)))
          || (not (P.Invocation.equal_status (H.status admitted) Dispatching))
          || (not (String.equal (H.context physical).tool_name reference.name))
          || not
               (Result.is_ok
                  (C.resolve
                     t.selected
                     ~id:reference.id
                     ~fingerprint:reference.fingerprint))
        then Error (invalid "managed preparation is foreign or already admitted")
        else Ok ()
      in
      let prepared = M.owned_prepared execution in
      let dependencies = Chat_response.Extension_compiler.capabilities prepared in
      let selected_now () =
        let* current = t.current_capabilities () in
        let* names =
          List.fold_result (C.references dependencies) ~init:[] ~f:(fun names pin ->
            let* _ =
              C.resolve current ~id:pin.id ~fingerprint:pin.fingerprint
              |> Result.map_error ~f:(fun error -> invalid error.message)
            in
            Ok (pin.name :: names))
        in
        C.select current ~names:(List.rev names)
        |> Result.map_error ~f:(fun error -> invalid error.message)
      in
      let* exact = selected_now () in
      let* () =
        if String.equal (C.fingerprint exact) (C.fingerprint dependencies)
        then Ok ()
        else Error (invalid "managed dependency selection changed")
      in
      let* () =
        K.rebind_selected t.kernel ~selected:exact ~current_capabilities:(fun () ->
          Result.map_error (t.current_capabilities ()) ~f:(fun (error : P.Error.t) ->
            error.message))
        |> kernel_result
      in
      t.selected <- exact;
      t.selected_now <- selected_now;
      t.managed_admitted <- true;
      Ok ()
  ;;

  let stage_background t ~request ~checkpoint = stage t ~checkpoint request

  let install ?control t config =
    let config = K.install ?control t.kernel ~handlers:(handlers t) config in
    match t.physical with
    | Worker _ -> config
    | Root _ | Native _ ->
      let model = model_spawn_operation ?control t in
      { config with
        operations =
          model
          :: List.filter config.operations ~f:(fun operation ->
            not (String.equal operation.name model.name))
      }
  ;;

  let install_selected ?control t ~prepared config =
    let config = install ?control t config in
    let scoped_async_operation =
      match Chat_response.Extension_compiler.compiler_target prepared with
      | Chatml_compilation.Developer_tool_v1
        when Option.is_some (O.selected_model t.service.root) -> Some Runtime.Model_spawn
      | One_off_v1 | Tool_v1 | Developer_tool_v1 | Moderator_v1 | Delegated_moderator_v1
        -> None
    in
    N.{ config; scoped_async_operation }
  ;;

  let split_starts t effects = kernel_result (K.split_starts t.kernel effects)
  let validate_work t work = kernel_result (K.validate_work t.kernel work)
end

let with_kernel
      service
      ~physical
      ~issuing_source
      ~selected
      ~selected_now
      ~current_capabilities
      ~check_parent_now
      f
  =
  K.with_scope
    ~selected
    ~current_capabilities:(fun () ->
      Result.map_error (current_capabilities ()) ~f:(fun (error : P.Error.t) ->
        error.message))
    ~abort:(fun id ->
      match cleanup_physical physical id with
      | Ok O.Action.Aborted | Ok Physically_owned -> ()
      | Error error -> failwith error.message)
    ~error:invalid
    (fun kernel ->
       f
         { service
         ; physical
         ; issuing_source
         ; worker_parent = None
         ; selected
         ; selected_now
         ; current_capabilities
         ; kernel
         ; check_parent_now
         ; managed_admitted = false
         ; frozen = None
         })
;;

let with_root_action service ~phase f =
  let* witness = O.current service.root in
  let selected = O.Current.selected witness in
  let* () = O.check_effect_now service.root witness in
  let action = ref None in
  K.with_scope
    ~selected
    ~current_capabilities:(fun () -> Ok (O.native_selected service.root))
    ~abort:(fun id ->
      match !action with
      | None -> failwith "root job stage has no issuing physical action"
      | Some action ->
        (match O.Action.cleanup_staged action id with
         | Ok Aborted | Ok Physically_owned -> ()
         | Error error -> failwith error.message))
    ~error:invalid
    (fun kernel ->
       let* durable =
         O.with_root_action service.root ~phase (fun ~action:issued ~sw ->
           action := Some issued;
           let scope =
             { service
             ; physical = Root issued
             ; issuing_source = Root_source
             ; worker_parent = None
             ; selected
             ; selected_now = (fun () -> Ok (O.native_selected service.root))
             ; current_capabilities = (fun () -> Ok (O.native_selected service.root))
             ; kernel
             ; check_parent_now = (fun () -> Ok ())
             ; managed_admitted = false
             ; frozen = None
             }
           in
           let* value, effects = f ~scope ~sw in
           let* ids, ordinary = Scope.split_starts scope effects in
           let* selection = O.Action.seal issued ~selected:ids in
           let* acknowledge = kernel_result (K.mark_prepared kernel) in
           Ok ((value, ordinary, acknowledge), selection))
       in
       let value, ordinary, acknowledge = durable in
       acknowledge ();
       Ok (value, ordinary))
;;

let with_claimed_worker service ~candidate ?on_capacity_decision ~expired_state ?select f =
  let* witness = O.claim_preflight service.root in
  let* saved =
    B.of_json
      ~policy:(O.job_policy service.root)
      (P.Developer_job.payload (O.Job_candidate.job candidate))
  in
  let claimed = Option.is_some (B.issuer_source saved) in
  let* () =
    if claimed && Option.is_none service.queued_issuer
    then Error (invalid "claimed child issuer installer is unavailable")
    else Ok ()
  in
  let* selected =
    if claimed
    then
      C.select (O.Claim_preflight.selected witness) ~names:[]
      |> Result.map_error ~f:(fun error -> invalid error.C.message)
    else B.selected_capabilities saved ~capabilities:(O.Claim_preflight.selected witness)
  in
  let root_selected_now () =
    B.selected_capabilities saved ~capabilities:(O.native_selected service.root)
  in
  let action = ref None in
  K.with_scope
    ~selected
    ~current_capabilities:(fun () -> Ok (O.native_selected service.root))
    ~abort:(fun id ->
      match !action with
      | None -> failwith "claimed job stage has no issuing physical action"
      | Some action ->
        (match O.Worker_action.cleanup_staged action id with
         | Ok O.Action.Aborted | Ok Physically_owned -> ()
         | Error error -> failwith error.message))
    ~error:invalid
    (fun kernel ->
       let acknowledgement = ref None in
       let* result =
         O.Job_candidate.with_claimed_selected
           service.root
           candidate
           ?on_capacity_decision
           ~selection_witness:witness
           ?select
           (fun ~active ~action:issued ~sw ->
              action := Some issued;
              let run_scope ~selected ~selected_now ~current_capabilities ~issuer ~run =
                let* parent = O.job_parent service.root active in
                let capture_fingerprint =
                  Chatml_developer_capture.fingerprint (O.capture service.root)
                in
                let check_parent_now () =
                  let* () =
                    Agent_session.Developer_job_scope.Parent.check_now
                      parent
                      ~owner:(O.owner service.root)
                      ~capture_fingerprint
                  in
                  match issuer with
                  | None -> Ok ()
                  | Some issuer -> Claimed.check_now issuer
                in
                let scope =
                  { service
                  ; physical = Worker issued
                  ; issuing_source =
                      (match issuer with
                       | None -> Root_source
                       | Some issuer -> Claimed_issuer issuer)
                  ; worker_parent = Some parent
                  ; selected
                  ; selected_now
                  ; current_capabilities
                  ; kernel
                  ; check_parent_now
                  ; managed_admitted = false
                  ; frozen = None
                  }
                in
                let* state, effects = run scope in
                let* ids, _ordinary = Scope.split_starts scope effects in
                let* selection = O.Worker_action.seal issued ~selected:ids in
                let* acknowledge = kernel_result (K.mark_prepared kernel) in
                acknowledgement := Some acknowledge;
                Ok (state, selection)
              in
              if not claimed
              then
                run_scope
                  ~selected
                  ~selected_now:root_selected_now
                  ~current_capabilities:(fun () -> Ok (O.native_selected service.root))
                  ~issuer:None
                  ~run:(fun scope -> f ~active ~scope ~issuer:None ~budget:None ~sw)
              else
                let* timing =
                  Deadline.create
                    ~now:(O.native_now service.root)
                    ~job:(O.Job_candidate.job candidate)
                    ~request:saved
                in
                match Deadline.disposition timing with
                | Expired ->
                  run_scope
                    ~selected
                    ~selected_now:(fun () -> Ok selected)
                    ~current_capabilities:(fun () -> Ok (O.native_selected service.root))
                    ~issuer:None
                    ~run:(fun _scope ->
                      Result.map (expired_state ~active) ~f:(fun state -> state, []))
                | Active effective ->
                  (match service.queued_issuer with
                   | None ->
                     Error (invalid "claimed child issuer installer is unavailable")
                   | Some queued_issuer ->
                     Chatml_execution.with_host_budget
                       ~env:service.env
                       ~policy:(Bounded effective)
                       (fun control ->
                          let* control =
                            control
                            |> Result.of_option
                                 ~error:
                                   (P.Error.invalid_request
                                      "bounded claimed Script control is unavailable")
                          in
                          Developer_queued_issuer.with_claimed
                            queued_issuer
                            ~active
                            ~request:saved
                            ~checkpoint:(fun () ->
                              Eio.Fiber.check ();
                              control.diagnostic_checkpoint ())
                            (fun issuer ->
                               let selected = Claimed.pinned_capabilities issuer in
                               let current_capabilities () =
                                 let* () = Claimed.check issuer in
                                 Ok (Claimed.selected issuer)
                               in
                               let selected_now () =
                                 let* capabilities = current_capabilities () in
                                 B.selected_capabilities saved ~capabilities
                               in
                               let* () =
                                 K.rebind_selected
                                   kernel
                                   ~selected
                                   ~current_capabilities:(fun () ->
                                     Result.map_error
                                       (current_capabilities ())
                                       ~f:(fun error -> error.P.Error.message))
                                 |> kernel_result
                               in
                               let* () = Claimed.check issuer in
                               run_scope
                                 ~selected
                                 ~selected_now
                                 ~current_capabilities
                                 ~issuer:(Some issuer)
                                 ~run:(fun scope ->
                                   f
                                     ~active
                                     ~scope
                                     ~issuer:(Some issuer)
                                     ~budget:
                                       (Some
                                          { Claim_budget.deadline =
                                              Deadline.deadline timing
                                          ; control
                                          })
                                     ~sw)))
                     |> Result.map_error ~f:(fun error ->
                       P.Error.create
                         Resource_limit
                         ~message:error.message
                         ~retryable:false
                         ())
                     |> Result.join))
       in
       match result, !acknowledgement with
       | None, None -> Ok None
       | ( Some (job, Developer_job_worker.Children.Settlement.Selected_installed)
         , Some acknowledge ) ->
         acknowledge ();
         Ok (Some job)
       | Some (_, Selected_omitted_after_epoch_change), Some _ ->
         Error
           (invalid "developer claimed child selection was omitted after epoch change")
       | None, Some _ | Some _, None ->
         Error (invalid "developer claimed job acknowledgement is unavailable"))
;;

module Native_selection = struct
  type t = native_selection

  let physical (t : t) = t.physical
end

module Capability_source = struct
  type t =
    | Root
    | Worker of C.t
    | Admitted_child of Child.t
    | Claimed_child of Claimed.t
end

let current_capabilities service = function
  | Capability_source.Root | Worker _ -> Ok (O.native_selected service.root)
  | Admitted_child child ->
    let* () =
      if Child.issued_by_root child service.root
      then Ok ()
      else Error (invalid "admitted child belongs to another physical Root")
    in
    let* admission = Child.refresh child in
    let* () = Child.Admission.check_now admission in
    Ok (Child.Admission.capabilities admission)
  | Claimed_child issuer ->
    let* () = Claimed.check issuer in
    Ok (Claimed.selected issuer)
;;

let borrowed_selected service terminal borrowed capabilities =
  let context = H.context (O.Native_terminal.invocation terminal) in
  let* () =
    if P.Execution_owner.equal context.owner (Developer (O.owner service.root))
    then Ok ()
    else Error (invalid "native job owner differs from its physical root")
  in
  let* selected =
    match context.parent_invocation, borrowed, capabilities with
    | None, None, Capability_source.Root -> current_capabilities service capabilities
    | None, None, Worker selected when Option.is_some context.parent_job -> Ok selected
    | None, None, Claimed_child issuer when Option.is_some context.parent_job ->
      let* () = Claimed.check issuer in
      Ok (Claimed.pinned_capabilities issuer)
    | None, None, Worker _ ->
      Error (invalid "root native scope cannot borrow a claimed job ceiling")
    | None, None, Claimed_child _ ->
      Error (invalid "claimed child has no physical Worker parent")
    | Some expected, Some borrowed, (Root | Admitted_child _ | Claimed_child _) ->
      let parent = H.context (N.selected_borrowed_invocation borrowed) in
      if
        (not (P.Id.Invocation.equal expected parent.id))
        || not (P.Execution_owner.equal context.owner parent.owner)
      then Error (invalid "native job parent borrow is foreign")
      else (
        match capabilities with
        | Root -> N.selected_borrowed_capabilities borrowed
        | Admitted_child child -> current_capabilities service (Admitted_child child)
        | Claimed_child _ -> N.selected_borrowed_capabilities borrowed
        | Worker _ -> assert false)
    | None, None, Admitted_child _
    | None, Some _, _
    | Some _, None, _
    | Some _, Some _, Worker _ ->
      Error (invalid "native job parent borrow is missing or unexpected")
  in
  let* current_capabilities = current_capabilities service capabilities in
  let* current =
    List.fold_result (C.references selected) ~init:[] ~f:(fun names reference ->
      let* _actual =
        C.resolve current_capabilities ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map_error ~f:(fun error -> invalid error.message)
      in
      Ok (reference.name :: names))
  in
  let* narrowed =
    C.select current_capabilities ~names:(List.rev current)
    |> Result.map_error ~f:(fun error -> invalid error.message)
  in
  if String.equal context.capability_fingerprint (C.fingerprint narrowed)
  then Ok narrowed
  else Error (invalid "native job borrow differs from admitted child selection")
;;

let with_native_scope service ~terminal ~borrowed ~capabilities ~check_parent_now f =
  if not (O.Native_terminal.issued_by_root terminal ~root:service.root)
  then Error (invalid "native job terminal belongs to another physical root")
  else
    let* selected = borrowed_selected service terminal borrowed capabilities in
    let selected_now, current_capabilities_now =
      match capabilities with
      | Admitted_child child ->
        let current_source () =
          let* () =
            if Child.issued_by_root child service.root
            then Ok ()
            else Error (invalid "admitted child belongs to another physical Root")
          in
          let* () =
            match borrowed with
            | Some borrowed ->
              Result.map (N.selected_borrowed_capabilities borrowed) ~f:ignore
            | None -> Error (invalid "admitted child has no borrowed parent")
          in
          Child.source_current child
        in
        let selected_now () =
          let* source = current_source () in
          let* names =
            List.fold_result (C.references selected) ~init:[] ~f:(fun names reference ->
              let* _ =
                C.resolve source ~id:reference.id ~fingerprint:reference.fingerprint
                |> Result.map_error ~f:(fun error -> invalid error.message)
              in
              Ok (reference.name :: names))
          in
          let* current =
            C.select source ~names:(List.rev names)
            |> Result.map_error ~f:(fun error -> invalid error.message)
          in
          if String.equal (C.fingerprint current) (C.fingerprint selected)
          then Ok current
          else Error (invalid "admitted child Job selection changed")
        in
        selected_now, current_source
      | Root | Worker _ | Claimed_child _ ->
        ( (fun () -> borrowed_selected service terminal borrowed capabilities)
        , fun () -> current_capabilities service capabilities )
    in
    with_kernel
      service
      ~physical:(Native terminal)
      ~issuing_source:
        (match capabilities with
         | Admitted_child child -> Foreground_child child
         | Claimed_child issuer -> Claimed_issuer issuer
         | Root | Worker _ -> Root_source)
      ~selected
      ~selected_now
      ~current_capabilities:current_capabilities_now
      ~check_parent_now
      (fun scope -> f ~scope)
;;

let seal_native (scope : scope) ~terminal ~outcome ~selected_jobs =
  match scope.physical with
  | Root _ -> Error (invalid "root job scope cannot seal a native terminal")
  | Worker _ -> Error (invalid "worker job scope cannot seal a native terminal")
  | Native actual when not (phys_equal actual terminal) ->
    Error (invalid "native job terminal changed before sealing")
  | Native _ ->
    (match scope.frozen with
     | Some (prior, selection) ->
       if
         P.Invocation.equal_outcome prior outcome
         && List.equal
              P.Id.Job.equal
              (List.map
                 (O.Native_terminal.Selection.jobs selection.physical)
                 ~f:P.Developer_job.id)
              selected_jobs
       then Ok selection
       else Error (invalid "native job selection changed after sealing")
     | None ->
       let* () = completion_current scope terminal in
       let* () = kernel_result (K.check_open scope.kernel) in
       let* acknowledge = kernel_result (K.mark_prepared scope.kernel) in
       let* () = completion_current scope terminal in
       let* physical = O.Native_terminal.seal terminal ~outcome ~selected:selected_jobs in
       let selection = { terminal; physical; acknowledge; acknowledged = false } in
       scope.frozen <- Some (outcome, selection);
       Ok selection)
;;

let frozen_native (scope : scope) ~terminal =
  match scope.physical, scope.frozen with
  | Native actual, Some pair when phys_equal actual terminal -> Some pair
  | Native _, Some _ | Native _, None | Root _, _ | Worker _, _ -> None
;;

let acknowledge_native (scope : scope) selection =
  match scope.physical, scope.frozen with
  | Native terminal, Some (_, frozen)
    when phys_equal terminal selection.terminal
         && phys_equal frozen selection
         && not selection.acknowledged ->
    selection.acknowledge ();
    selection.acknowledged <- true;
    Ok ()
  | Native _, Some _ | Native _, None | Root _, _ | Worker _, _ ->
    Error (invalid "native job acknowledgement is foreign, absent, or repeated")
;;
