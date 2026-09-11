open Core
module I = Agent_protocol.Invocation
module C = Chat_response.Tool_capability
module EC = Chat_response.Extension_compiler
module M = Chat_response.Moderation.Capabilities
module E = Agent_protocol.Moderator_execution
module Managed = Chat_response.Managed_tool_registry

type managed_service =
  { env : Eio_unix.Stdenv.base
  ; definition : Managed.t
  ; execution_limits : EC.t -> Chatml_execution.limits
  }

type moderator_dispatch =
  execute:Native_tool_invocation.moderator_executor
  -> native_execute:Native_tool_invocation.executor
  -> selected:C.t
  -> reference:C.reference
  -> invocation:I.t
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (I.t, Agent_protocol.Error.t) result

type preparation_owner =
  | Model_call of Agent_protocol.Id.Operation.t * History_entry.Id.t
  | Native_call of Agent_protocol.Id.Invocation.t
  | Moderator_event of Agent_protocol.Id.Moderator_execution.t

type preparation =
  { invocation_id : Agent_protocol.Id.Invocation.t
  ; session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; owner : preparation_owner
  ; selected : C.t
  ; call : Chat_response.Moderation.Tool_call.t
  }

type preparation_policy =
  preparation
  -> (Chat_response.Moderation.Tool_moderation.t option, Agent_protocol.Error.t) result

type t =
  { registry : unit -> C.t
  ; moderator_names : String.Set.t
  ; now : unit -> Agent_protocol.Timestamp.t
  ; is_halted : unit -> bool
  ; durable_requests : bool
  ; requires_active_moderator : C.reference -> bool
  ; authorize : I.t -> C.binding -> (unit, Agent_protocol.Error.t) result
  ; preparation : preparation_policy option
  ; prepare_output :
      Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, Agent_protocol.Error.t) result
  ; defer_observation : I.t -> (unit, Agent_protocol.Error.t) result
  ; managed : (t -> Native_tool_invocation.managed_dispatch) option
  ; moderator : (t -> moderator_dispatch) option
  ; jobs : Script_job_service.t option
  ; subscriptions : Script_subscription_service.t option
  ; schedules : Script_schedule_service.t option
  ; notifications : Script_notification_service.t option
  ; ingress : Script_ingress_service.t option
  ; progress : (I.t -> Ochat_function.Progress.t -> unit) option
  ; progress_ceiling : C.t option
  ; shell_context : (unit -> (Shell_runtime.Call_context.t, string) result) option
  ; authoring_validation_host : Chat_response.Authoring_validation.host option
  ; generated_creation_service : Generated_session_request.service option
  ; managed_session_service : Managed_session_service.t option
  }

type native_services =
  { tools : t
  ; session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; active : bool Atomic.t
  }

let native_services_key = Eio.Fiber.create_key ()

let with_native_services tools ~session_id ~generation f =
  let context = { tools; session_id; generation; active = Atomic.make true } in
  Exn.protect
    ~finally:(fun () -> Atomic.set context.active false)
    ~f:(fun () ->
      Eio.Fiber.with_binding native_services_key context (fun () ->
        match tools.shell_context with
        | None -> Shell_runtime.Call_context.without_services f
        | Some services ->
          let check () =
            let open Result.Let_syntax in
            let%bind invocation =
              match Native_tool_invocation.current_scope () with
              | Active invocation
                when Atomic.get context.active
                     && Agent_protocol.Id.Session.equal
                          session_id
                          invocation.context.session_id
                     && Int.equal generation invocation.context.generation ->
                Ok invocation
              | Active _ | Expired | Unbound ->
                Error "shell caller scope is stale or foreign"
            in
            let%bind binding =
              C.find (tools.registry ()) ~name:invocation.context.tool_name
              |> Result.map_error ~f:(fun error -> error.C.message)
            in
            let%bind () =
              match tools.is_halted () with
              | true -> Error "shell caller is halted or stopping"
              | false ->
                (match
                   String.equal
                     (C.reference binding).implementation_revision
                     invocation.context.implementation_revision
                 with
                 | true -> Ok ()
                 | false -> Error "shell registered implementation changed")
            in
            tools.authorize invocation binding
            |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
          in
          Shell_runtime.Call_context.with_services
            (fun () ->
               let open Result.Let_syntax in
               let%map services = services () in
               { services with
                 check =
                   (fun () ->
                     let%bind () = check () in
                     services.check ())
               })
            f))
;;

let current_native_services () =
  match Eio.Fiber.get native_services_key, Native_tool_invocation.current_scope () with
  | Some context, Active invocation
    when Atomic.get context.active
         && Agent_protocol.Id.Session.equal
              context.session_id
              invocation.context.session_id
         && Int.equal context.generation invocation.context.generation -> Ok context.tools
  | _ -> Error "native tool services do not belong to an active invoking session"
;;

let create
      ~registry
      ~moderator_names
      ~now
      ~is_halted
      ~requires_active_moderator
      ~authorize
      ~prepare_output
      ~defer_observation
  =
  { registry
  ; moderator_names
  ; now
  ; is_halted
  ; durable_requests = false
  ; requires_active_moderator
  ; authorize
  ; preparation = None
  ; prepare_output
  ; defer_observation
  ; managed = None
  ; moderator = None
  ; jobs = None
  ; subscriptions = None
  ; schedules = None
  ; notifications = None
  ; ingress = None
  ; progress = None
  ; progress_ceiling = None
  ; shell_context = None
  ; authoring_validation_host = None
  ; generated_creation_service = None
  ; managed_session_service = None
  }
;;

let with_shell_context t services = { t with shell_context = Some services }
let with_authoring_validation_host t host = { t with authoring_validation_host = host }
let authoring_validation_host t = t.authoring_validation_host

let with_generated_creation_service t service =
  { t with generated_creation_service = Some service }
;;

let generated_creation_service t = t.generated_creation_service

let with_managed_session_service t service =
  { t with managed_session_service = Some service }
;;

let managed_session_service t = t.managed_session_service

let with_preparation t ~prepare =
  match t.preparation with
  | None -> { t with preparation = Some prepare }
  | Some _ -> invalid_arg "script tools already have a preparation policy"
;;

let native_dispatch t ~declared ~input ~capabilities =
  let registry () =
    C.select
      (t.registry ())
      ~names:(List.map (C.references declared) ~f:(fun reference -> reference.C.name))
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
  in
  let dispatch =
    Native_tool_dispatch.create
      ~input
      ~capabilities
      ~declared
      ~registry
      ~now:t.now
      ~is_halted:t.is_halted
      ~admit:t.authorize
      ~prepare_output:t.prepare_output
  in
  { dispatch with
    run =
      (fun request ~authorize ->
        with_native_services
          t
          ~session_id:input.session_id
          ~generation:input.session_generation
          (fun () -> dispatch.run request ~authorize))
  }
;;

let is_halted t = t.is_halted ()
let current_capabilities t = t.registry ()
let authorize t = t.authorize

let with_authorization_guard t ~check =
  let authorize invocation binding =
    let open Result.Let_syntax in
    let%bind () = check () in
    let%bind () = t.authorize invocation binding in
    check ()
  in
  let prepare_output output =
    let open Result.Let_syntax in
    let%bind () = check () in
    let%bind output = t.prepare_output output in
    let%map () = check () in
    output
  in
  { t with authorize; prepare_output }
;;

let with_lifecycle t ~is_halted = { t with is_halted }
let with_durable_requests t = { t with durable_requests = true }
let durable_requests t = t.durable_requests
let with_moderator_dispatch t ~dispatch = { t with moderator = Some dispatch }
let with_job_service t jobs = { t with jobs = Some jobs }

let with_subscription_service t subscriptions =
  { t with subscriptions = Some subscriptions }
;;

let with_schedule_service t schedules = { t with schedules = Some schedules }

let with_notification_service t notifications =
  { t with notifications = Some notifications }
;;

let with_progress t ~emit = { t with progress = Some emit }
let with_ingress_service t ingress = { t with ingress = Some ingress }
let with_progress_ceiling t ~ceiling = { t with progress_ceiling = Some ceiling }

let progress_for t (reference : C.reference) =
  match t.progress, t.progress_ceiling with
  | Some emit, Some ceiling ->
    (match C.resolve ceiling ~id:reference.id ~fingerprint:reference.fingerprint with
     | Ok _ -> Some emit
     | Error _ -> None)
  | _ -> None
;;

let with_job_scope t ~owner ~selected ~error f =
  match t.jobs with
  | None -> f None
  | Some service ->
    Script_job_service.with_scope service ~owner ~selected ~error (fun scope ->
      f (Some scope))
;;

let with_moderator_work t ~owner ~selected ~source ~originating ~error f =
  let f ~jobs ~subscriptions ~schedules ~notifications =
    match t.ingress with
    | None -> f ~jobs ~subscriptions ~schedules ~notifications ~ingress:None
    | Some service ->
      Script_ingress_service.with_scope service ~owner ~source ~error (fun scope ->
        f ~jobs ~subscriptions ~schedules ~notifications ~ingress:(Some scope))
  in
  with_job_scope t ~owner ~selected ~error (fun jobs ->
    let with_schedules f =
      match t.schedules with
      | None -> f None
      | Some service ->
        Script_schedule_service.with_scope service ~owner ~source ~error (fun scope ->
          f (Some scope))
    in
    with_schedules (fun schedules ->
      let with_notifications subscriptions =
        match t.notifications with
        | None -> f ~jobs ~subscriptions ~schedules ~notifications:None
        | Some service ->
          Script_notification_service.with_scope
            service
            ~owner
            ~source
            ~selected
            ~jobs
            ~error
            (fun scope -> f ~jobs ~subscriptions ~schedules ~notifications:(Some scope))
      in
      match t.subscriptions with
      | None -> with_notifications None
      | Some service ->
        Script_subscription_service.with_scope
          ?schedules
          service
          ~owner
          ~source
          ~originating
          ~error
          (fun scope -> with_notifications (Some scope))))
;;

let validate_pending_work ~jobs ~subscriptions ~fallback work =
  match work, jobs, subscriptions with
  | I.Job _, Some scope, _ -> Script_job_service.validate_work scope work
  | I.Subscription id, _, Some scope -> Script_subscription_service.validate_work scope id
  | _ -> fallback work
;;

let validate_definition t definition =
  let captured = EC.definition_capabilities definition in
  let names = List.map (C.references captured) ~f:(fun reference -> reference.C.name) in
  let open Result.Let_syntax in
  let%bind selected =
    C.select (t.registry ()) ~names
    |> Result.map_error ~f:(fun _ -> "captured native capability is no longer available")
  in
  match String.equal (C.fingerprint selected) (C.fingerprint captured) with
  | true -> Ok ()
  | false -> Error "captured native capability bindings changed"
;;

let tool_error code = Ok (M.Tool_error code)
let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let checked error f =
  match f () with
  | Ok value -> Ok value
  | Error _ -> Error error
  | exception
      ((Eio.Cancel.Cancelled _ | Eio.Time.Timeout | Chatml_execution.Budget_exhausted _)
       as exn) -> raise exn
  | exception _ -> Error error
;;

type parent =
  | Invocation of I.t
  | Event of E.t

type prepared_call =
  { reference : C.reference
  ; input : Jsonaf.t
  ; routing : I.routing option
  ; rejection : I.outcome option
  }

let call_kind selected (reference : C.reference) =
  match C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint with
  | Ok binding when String.equal (C.descriptor binding).type_ "custom" ->
    Chat_response.Moderation.Tool_call.Custom
  | _ -> Function
;;

let call_payload kind input =
  match kind, input with
  | Chat_response.Moderation.Tool_call.Custom, `String text -> text
  | Function, _ | Custom, _ -> Jsonaf.to_string input
;;

let prepare_host_call t ~selected ~session_id ~generation ~owner ~id prepared =
  match t.preparation, prepared.rejection with
  | None, _ | _, Some _ -> Ok prepared
  | Some prepare, None ->
    let open Result.Let_syntax in
    let original = prepared.reference in
    let kind = call_kind selected original in
    let original_payload = call_payload kind prepared.input in
    let fingerprint value : I.payload_fingerprint =
      { sha256 = Chatmd_shell_spec.Source_ref.digest value
      ; byte_length = String.length value
      }
    in
    let route reference input preparation rejection =
      let final_kind = call_kind selected reference in
      let routing : I.routing =
        { kind =
            (match final_kind with
             | Function -> Function
             | Custom -> Custom)
        ; original_name =
            Option.value_map prepared.routing ~default:original.name ~f:(fun r ->
              r.original_name)
        ; original_payload =
            Option.value_map
              prepared.routing
              ~default:(fingerprint original_payload)
              ~f:(fun r -> r.original_payload)
        ; final_payload = fingerprint (call_payload final_kind input)
        ; canonical_payload = None
        ; preparation
        }
      in
      { reference; input; routing = Some routing; rejection }
    in
    let reject preparation code message =
      Ok (route original prepared.input preparation (Some (fail code message)))
    in
    let validate reference input =
      let%bind _ =
        C.resolve selected ~id:reference.C.id ~fingerprint:reference.fingerprint
        |> Result.map_error ~f:(fun e -> e.C.message)
      in
      let%bind schema =
        Chatmd_shell_spec.Tool_schema.compile reference.input_schema
        |> Result.map_error ~f:(fun _ -> "invalid tool input schema")
      in
      Chatmd_shell_spec.Tool_schema.validate schema input
      |> Result.map_error ~f:(fun _ -> "invalid tool input")
    in
    (match validate original prepared.input with
     | Error _ ->
       reject Invalid_input "invocation.invalid_input" "The tool arguments are invalid."
     | Ok () ->
       let meta =
         match owner with
         | Model_call (operation_id, call_entry_id) ->
           `Object
             [ "origin", `String "model"
             ; "operation_id", Agent_protocol.Id.Operation.to_json operation_id
             ; "call_entry_id", History_entry.Id.jsonaf_of_t call_entry_id
             ]
         | Native_call parent ->
           `Object
             [ "origin", `String "script"
             ; "parent_invocation", Agent_protocol.Id.Invocation.to_json parent
             ]
         | Moderator_event parent ->
           `Object
             [ "origin", `String "moderator"
             ; "parent_event", Agent_protocol.Id.Moderator_execution.to_json parent
             ]
       in
       let call : Chat_response.Moderation.Tool_call.t =
         { id = Agent_protocol.Id.Invocation.to_string id
         ; name = original.name
         ; args = prepared.input
         ; kind
         ; payload_text = original_payload
         ; meta
         }
       in
       (match
          checked () (fun () ->
            let current () =
              C.select
                (t.registry ())
                ~names:(List.map (C.references selected) ~f:(fun r -> r.C.name))
              |> Result.map_error ~f:(fun error ->
                Agent_protocol.Error.invalid_request error.C.message)
              |> Result.bind ~f:(fun current ->
                match String.equal (C.fingerprint current) (C.fingerprint selected) with
                | true -> Ok ()
                | false ->
                  Error
                    (Agent_protocol.Error.invalid_request
                       "selected tool bindings changed"))
            in
            let%bind () = current () in
            let%bind decision =
              prepare
                { invocation_id = id; session_id; generation; owner; selected; call }
            in
            let%map () = current () in
            decision)
        with
        | Error () ->
          reject
            Pre_tool_failed
            "invocation.pre_tool_failed"
            "Host tool preparation failed."
        | Ok (Some (Reject _)) ->
          reject
            Pre_tool_rejected
            "invocation.pre_tool_rejected"
            "Host policy rejected the call."
        | Ok decision ->
          let name, input =
            match decision with
            | None | Some Approve -> original.name, prepared.input
            | Some (Rewrite_args input) -> original.name, input
            | Some (Redirect (name, input)) -> name, input
            | Some (Reject _) -> assert false
          in
          (match
             List.find (C.references selected) ~f:(fun reference ->
               String.equal reference.name name)
           with
           | None ->
             reject
               Pre_tool_rejected
               "invocation.unselected_tool"
               "The redirected tool is not selected."
           | Some reference ->
             (match validate reference input with
              | Error _ ->
                reject
                  Pre_tool_failed
                  "invocation.pre_tool_failed"
                  "Host policy returned invalid tool arguments."
              | Ok () -> Ok (route reference input Passed None)))))
;;

let with_model_preparation t ~selected ~(input : Operation_worker.Input.t) dispatch =
  match t.preparation with
  | None -> dispatch
  | Some _ ->
    Chat_response.In_memory_stream.Tool_dispatch.with_preparation
      dispatch
      ~prepare:(fun request ->
        let open Result.Let_syntax in
        let%bind () =
          match request.source, request.parent_call_id with
          | None, None -> Ok ()
          | Some _, _ | _, Some _ -> Error "host preparation requires its persisted owner"
        in
        let%bind reference =
          C.find selected ~name:request.name
          |> Result.map ~f:C.reference
          |> Result.map_error ~f:(fun error -> error.C.message)
        in
        let%bind value =
          Stream_invocation.parse_input ~kind:request.kind ~payload:request.payload
        in
        let call_id = History_entry.id request.call in
        let%bind prepared =
          prepare_host_call
            t
            ~selected
            ~session_id:input.session_id
            ~generation:input.session_generation
            ~owner:(Model_call (input.operation.id, call_id))
            ~id:(Stream_invocation.id_for_call ~input ~call_id)
            { reference; input = value; routing = None; rejection = None }
          |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
        in
        match prepared.rejection with
        | Some (I.Fail { code = "invocation.pre_tool_failed"; _ }) ->
          Error "host tool preparation failed"
        | Some _ ->
          Ok
            (Some
               (Chat_response.Moderation.Tool_moderation.Reject
                  "Host policy rejected the call."))
        | None ->
          let original_kind = call_kind selected reference in
          let final_kind = call_kind selected prepared.reference in
          (match original_kind, final_kind with
           | Function, Custom | Custom, Function ->
             Error "host preparation cannot change model tool kind"
           | Function, Function | Custom, Custom ->
             if not (String.equal reference.name prepared.reference.name)
             then Ok (Some (Redirect (prepared.reference.name, prepared.input)))
             else if Jsonaf.exactly_equal value prepared.input
             then Ok None
             else Ok (Some (Rewrite_args prepared.input))))
;;

let with_scope_results
      ~result_of_invocation
      ~result_of_error
      ?prepare
      ?moderator_execute
      ?(max_nested_calls = Chat_response.Moderator_invocation.max_nested_calls)
      t
      ~selected
      ~limits
      ~origin
      ~observer
      ~valid_parent
      ~execute
      ~parent
      f
  =
  let tool_error code = Ok (result_of_error code) in
  let session_id, generation, parent_invocation, parent_event, deadline =
    match parent with
    | Invocation parent ->
      ( parent.context.session_id
      , parent.context.generation
      , Some parent.context.id
      , None
      , parent.context.deadline )
    | Event parent ->
      ( parent.context.session_id
      , parent.context.generation
      , None
      , Some parent.context.id
      , Option.bind parent.context.job ~f:(fun job -> job.deadline) )
  in
  let active = Atomic.make true in
  let attempts = Atomic.make 0 in
  let validate_value value =
    let open Result.Let_syntax in
    let%bind () = I.validate_outcome (Complete value) in
    Chat_response.Moderator_invocation.snapshot_state
      ~limits
      (Chatml.Chatml_value_codec.jsonaf_to_value value)
    |> Result.map ~f:(fun _ -> ())
    |> Result.map_error ~f:(fun _ ->
      Agent_protocol.Error.invalid_request "nested value exceeds script limits")
  in
  let prepare_output output =
    let open Result.Let_syntax in
    let%bind value = t.prepare_output output in
    let%map () = validate_value value in
    value
  in
  let references = C.references selected in
  let reentrant name =
    Set.mem t.moderator_names name
    ||
    match Native_tool_moderation.active_moderator (), C.find selected ~name with
    | Some owner, Ok binding ->
      (match C.implementation binding with
       | Managed (Moderator script) -> String.equal script owner.script_id
       | _ -> false)
    | _ -> false
  in
  let names = List.map references ~f:(fun reference -> reference.C.name) in
  let registry () =
    match C.select (t.registry ()) ~names with
    | Ok selected -> selected
    | Error _ -> failwith "captured tool subset is no longer available"
  in
  let call ~name ~args =
    if (not (Atomic.get active)) || not valid_parent
    then tool_error "invocation.inactive_scope"
    else if Atomic.fetch_and_add attempts 1 >= max_nested_calls
    then tool_error "invocation.nested_call_limit"
    else if reentrant name
    then tool_error "moderator_reentrancy"
    else (
      match
        List.find references ~f:(fun reference -> String.equal reference.C.name name)
      with
      | None -> tool_error "invocation.unselected_tool"
      | Some reference ->
        let execute () =
          let open Result.Let_syntax in
          let%bind () = checked `Input (fun () -> validate_value args) in
          let id = Agent_protocol.Id.Invocation.create () in
          let%bind prepared =
            checked `Admission (fun () ->
              match prepare with
              | None -> Ok { reference; input = args; routing = None; rejection = None }
              | Some prepare -> prepare ~id reference args)
          in
          let%bind prepared =
            checked `Admission (fun () ->
              let owner =
                match parent with
                | Invocation parent -> Native_call parent.context.id
                | Event parent -> Moderator_event parent.context.id
              in
              prepare_host_call t ~selected ~session_id ~generation ~owner ~id prepared)
          in
          let reference = prepared.reference in
          let%bind () = checked `Input (fun () -> validate_value prepared.input) in
          let%bind invocation =
            checked `Admission (fun () ->
              I.create
                ?observer
                ?parent_event
                ?routing:prepared.routing
                { id
                ; session_id
                ; generation
                ; origin
                ; provider_call_id = None
                ; call_entry_id = None
                ; parent_invocation
                ; parent_job = None
                ; tool_name = reference.name
                ; implementation_revision = reference.implementation_revision
                ; capability_fingerprint = C.fingerprint selected
                ; input = prepared.input
                ; created_at = t.now ()
                ; deadline
                })
          in
          let moderation = Native_tool_moderation.capture () in
          let execute ~invocation f =
            execute ~invocation (fun ~(dispatched : I.t) ->
              (* A borrowed executor restores its lending context. This call's
                 actual host pre-hook scope may have been installed afterwards;
                 retain it for descendants without extending its lifetime. *)
              Native_tool_moderation.with_context moderation (fun () ->
                with_native_services
                  t
                  ~session_id:dispatched.context.session_id
                  ~generation:dispatched.context.generation
                  (fun () -> f ~dispatched)))
          in
          let%bind resolved =
            checked `Admission (fun () ->
              match prepared.rejection with
              | Some outcome -> execute ~invocation (fun ~dispatched:_ -> Ok outcome)
              | None when Option.is_none prepare && t.requires_active_moderator reference
                ->
                execute ~invocation (fun ~dispatched:_ ->
                  Ok
                    (fail
                       "moderator_reentrancy"
                       "Tool execution requires a decision from the active moderator."))
              | None when reentrant reference.name ->
                execute ~invocation (fun ~dispatched:_ ->
                  Ok
                    (fail
                       "moderator_reentrancy"
                       "Tool execution would re-enter the active moderator."))
              | None ->
                let moderator_target =
                  match
                    C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
                  with
                  | Ok binding ->
                    (match C.implementation binding with
                     | Managed (Moderator _) -> true
                     | _ -> false)
                  | Error _ -> false
                in
                (match moderator_target, t.moderator, moderator_execute with
                 | true, Some install, Some moderator_execute ->
                   install
                     t
                     ~execute:moderator_execute
                     ~native_execute:execute
                     ~selected
                     ~reference
                     ~invocation
                     ~prepare_output
                 | _ ->
                   Native_tool_invocation.run_scoped_with_managed
                     ~on_progress:(progress_for t reference)
                     ~managed:(Option.map t.managed ~f:(fun install -> install t))
                     ~moderator_execute
                     ~execute
                     ~registry
                     ~reference
                     ~invocation
                     ~is_halted:t.is_halted
                     ~authorize:t.authorize
                     ~prepare_output))
          in
          let%map () =
            match resolved.observation with
            | None -> Ok ()
            | Some _ -> checked `Observation (fun () -> t.defer_observation resolved)
          in
          result_of_invocation resolved
        in
        (match execute () with
         | Ok result -> Ok result
         | Error `Observation -> tool_error "invocation.observation_failed"
         | Error `Input -> tool_error "invocation.invalid_input"
         | Error `Admission -> tool_error "invocation.admission_failed"
         | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
         | exception _ -> tool_error "invocation.host_failed"))
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set active false)
    ~f:(fun () ->
      match origin with
      | Moderator ->
        Native_tool_moderation.with_handler
          ?active_moderator:observer
          ~observer
          ~prepare:(fun call ->
            match
              List.find references ~f:(fun reference ->
                String.equal reference.C.name call.Chat_response.Moderation.Tool_call.name)
            with
            | None -> Error "invocation.unselected_tool"
            | Some reference when t.requires_active_moderator reference ->
              Error "moderator_reentrancy"
            | Some _ -> Ok None)
          (fun () -> f call)
      | Model | Script | Delegated_agent | External_adapter -> f call)
;;

let tool_result (resolved : I.t) =
  match resolved.status with
  | Resolved (Complete value) -> M.Tool_ok value
  | Resolved (Pending (_, acknowledgement)) -> M.Tool_ok acknowledgement
  | Resolved (Fail error) -> Tool_error error.code
  | Resolved (Cancelled _) -> Tool_error "invocation.cancelled"
  | _ -> Tool_error "invocation.invalid_outcome"
;;

let with_scope
      ?prepare
      ?moderator_execute
      ?max_nested_calls
      t
      ~selected
      ~limits
      ~origin
      ~observer
      ~valid_parent
      ~execute
      ~parent
      f
  =
  with_scope_results
    ~result_of_invocation:tool_result
    ~result_of_error:(fun code -> M.Tool_error code)
    ?prepare
    ?moderator_execute
    ?max_nested_calls
    t
    ~selected
    ~limits
    ~origin
    ~observer
    ~valid_parent
    ~execute
    ~parent
    f
;;

let with_invocation t ~prepared ~capabilities ~(parent : I.t) f =
  let selected = EC.capabilities prepared in
  let valid_parent =
    match parent.status with
    | Dispatching ->
      String.equal parent.context.tool_name (EC.declaration prepared).name
      && String.equal parent.context.implementation_revision (EC.fingerprint prepared)
      && String.equal parent.context.capability_fingerprint (C.fingerprint selected)
    | _ -> false
  in
  let t =
    { t with moderator_names = Set.add t.moderator_names (EC.declaration prepared).name }
  in
  with_scope
    ~moderator_execute:
      capabilities.Operation_worker.Capabilities.with_moderator_invocation
    t
    ~selected
    ~limits:(EC.execution_limits prepared)
    ~origin:Moderator
    ~observer:
      (Some
         { script_id = (EC.script prepared).id
         ; source_sha256 = (EC.script prepared).source_sha256
         })
    ~valid_parent
    ~execute:capabilities.Operation_worker.Capabilities.with_invocation
    ~parent:(Invocation parent)
    f
;;

let with_managed_invocation t ~execution ~borrowed f =
  let prepared = Managed.prepared execution in
  let parent = Managed.invocation execution in
  let selected = EC.capabilities prepared in
  let moderator_names =
    List.fold (C.references selected) ~init:t.moderator_names ~f:(fun names reference ->
      match C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint with
      | Ok binding ->
        (match C.implementation binding with
         | Managed (Moderator script) when String.equal script (EC.script prepared).id ->
           Set.add names reference.name
         | _ -> names)
      | Error _ -> names)
  in
  let valid_parent =
    I.equal parent (Native_tool_invocation.borrowed_invocation borrowed)
    && (match (EC.declaration prepared).implementation with
        | Moderator _ -> true
        | Standalone _ -> false)
    &&
    match Native_tool_invocation.borrowed_capabilities borrowed with
    | Ok ceiling -> String.equal (C.fingerprint selected) (C.fingerprint ceiling)
    | Error _ -> false
  in
  with_scope
    { t with moderator_names }
    ~selected
    ~limits:(EC.execution_limits prepared)
    ~origin:Moderator
    ~observer:
      (Some
         { script_id = (EC.script prepared).id
         ; source_sha256 = (EC.script prepared).source_sha256
         })
    ~valid_parent
    ~execute:(Native_tool_invocation.execute_borrowed borrowed)
    ~parent:(Invocation parent)
    f
;;

let with_script_native_results
      ~result_of_invocation
      ~result_of_error
      ?observer
      ?max_nested_calls
      ?moderator_execute
      t
      ~selected
      ~limits
      ~execute
      ~valid_parent
      ~(parent : I.t)
      ~moderate
      f
  =
  let kind (reference : C.reference) =
    match C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint with
    | Ok binding when String.equal (C.descriptor binding).type_ "custom" ->
      Chat_response.Moderation.Tool_call.Custom
    | _ -> Function
  in
  let payload reference input =
    match kind reference, input with
    | Custom, `String text -> text
    | Function, _ | Custom, _ -> Jsonaf.to_string input
  in
  let fingerprint payload =
    I.
      { sha256 = Chatmd_shell_spec.Source_ref.digest payload
      ; byte_length = String.length payload
      }
  in
  let prepare ~id (original : C.reference) input =
    let original_payload = payload original input in
    let route reference input preparation rejection =
      let routing : I.routing =
        { kind =
            (match kind reference with
             | Custom -> Custom
             | Function -> Function)
        ; original_name = original.name
        ; original_payload = fingerprint original_payload
        ; final_payload = fingerprint (payload reference input)
        ; canonical_payload = None
        ; preparation
        }
      in
      Ok { reference; input; routing = Some routing; rejection }
    in
    let reject preparation code message =
      route original input preparation (Some (fail code message))
    in
    let valid_input =
      let open Result.Let_syntax in
      let%bind schema = Chatmd_shell_spec.Tool_schema.compile original.input_schema in
      Chatmd_shell_spec.Tool_schema.validate schema input
    in
    match valid_input with
    | Error _ ->
      reject Invalid_input "invocation.invalid_input" "The tool arguments are invalid."
    | Ok () ->
      let call : Chat_response.Moderation.Tool_call.t =
        { id = Agent_protocol.Id.Invocation.to_string id
        ; name = original.name
        ; args = input
        ; kind = kind original
        ; payload_text = original_payload
        ; meta =
            `Object
              [ "origin", `String "script"
              ; ( "parent_invocation"
                , `String (Agent_protocol.Id.Invocation.to_string parent.context.id) )
              ]
        }
      in
      let decision = checked () (fun () -> moderate call) in
      (match decision with
       | Error () ->
         reject Pre_tool_failed "invocation.pre_tool_failed" "Pre-tool moderation failed."
       | Ok (Some (Chat_response.Moderation.Tool_moderation.Reject _)) ->
         reject
           Pre_tool_rejected
           "invocation.pre_tool_rejected"
           "Pre-tool moderation rejected the call."
       | Ok action ->
         let name, args =
           match action with
           | None | Some Approve -> original.name, input
           | Some (Rewrite_args args) -> original.name, args
           | Some (Redirect (name, args)) -> name, args
           | Some (Reject _) -> assert false
         in
         (match
            List.find (C.references selected) ~f:(fun reference ->
              String.equal reference.name name)
          with
          | None ->
            reject
              Pre_tool_rejected
              "invocation.unselected_tool"
              "The redirected tool is not selected."
          | Some reference -> route reference args Passed None))
  in
  Native_tool_moderation.with_handler ~observer ~prepare:moderate (fun () ->
    with_scope_results
      ~result_of_invocation
      ~result_of_error
      ~prepare
      ?max_nested_calls
      ?moderator_execute
      t
      ~selected
      ~limits
      ~origin:Script
      ~observer
      ~valid_parent
      ~execute
      ~parent:(Invocation parent)
      f)
;;

let with_script_native_calls
      ?observer
      ?max_nested_calls
      ?moderator_execute
      t
      ~selected
      ~limits
      ~execute
      ~valid_parent
      ~parent
      ~moderate
      f
  =
  with_script_native_results
    ~result_of_invocation:tool_result
    ~result_of_error:(fun code -> M.Tool_error code)
    ?observer
    ?max_nested_calls
    ?moderator_execute
    t
    ~selected
    ~limits
    ~execute
    ~valid_parent
    ~parent
    ~moderate
    f
;;

type background_target =
  { invocation : I.t
  ; completion_schema : Jsonaf.t option
  }

let call_background ?observer t ~borrowed ~limits ~max_nested_calls ~moderate ~name ~args =
  let open Result.Let_syntax in
  let%bind selected =
    Native_tool_invocation.borrowed_capabilities borrowed
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
  in
  let parent = Native_tool_invocation.borrowed_invocation borrowed in
  let%bind binding =
    C.find selected ~name |> Result.map_error ~f:(fun error -> error.C.message)
  in
  let%bind completion_schema =
    match C.implementation binding, t.managed with
    | Native _, _ -> Ok None
    | Managed _, None -> Error "managed background target has no definition service"
    | Managed _, Some managed ->
      let%map prepared =
        Managed.resolve (managed t).definition binding
        |> Result.map_error ~f:(fun error -> error.C.message)
      in
      Option.map (EC.completion_schema prepared) ~f:Chatmd_shell_spec.Tool_schema.to_json
  in
  let valid_parent =
    match parent.status, parent.context.origin with
    | Dispatching, Script -> Option.is_some parent.context.parent_job
    | _ -> false
  in
  with_script_native_results
    ~result_of_invocation:(fun resolved ->
      match resolved.I.status with
      | Resolved _ -> Ok { invocation = resolved; completion_schema }
      | _ -> Error "invocation.invalid_outcome")
    ~result_of_error:(fun code -> Error code)
    ?observer
    ~max_nested_calls
    ?moderator_execute:(Native_tool_invocation.moderator_executor borrowed)
    t
    ~selected
    ~limits
    ~execute:(Native_tool_invocation.execute_borrowed borrowed)
    ~valid_parent
    ~parent
    ~moderate
    (fun call -> Result.join (call ~name ~args))
;;

let run_managed t service execution borrowed with_result =
  let module ABI = Chat_response.Moderator_invocation in
  let module R = Chatml_host_runtime in
  let module L = Chatml.Chatml_lang in
  let module V = Chatml.Chatml_value_codec in
  let module N = Native_tool_invocation in
  let prepared = Managed.prepared execution in
  let parent = Managed.invocation execution in
  let selected = EC.capabilities prepared in
  let execute control =
    with_job_scope
      t
      ~owner:(Agent_protocol.Job.Invocation parent.context.id)
      ~selected
      ~error:(fail "invocation.background_unavailable")
      (fun jobs ->
         let open Result.Let_syntax in
         let start_effects = ref [] in
         let validate_work work =
           match jobs with
           | None -> Error "background completion is not installed"
           | Some jobs -> Script_job_service.validate_work jobs work
         in
         let%bind entrypoint =
           match (EC.declaration prepared).implementation with
           | Standalone { entrypoint; _ } -> Ok entrypoint
           | Moderator _ ->
             Error
               (fail
                  "invocation.managed_dispatch_required"
                  "Moderator tool handoff is not installed.")
         in
         let%bind scope =
           checked
             (fail "invocation.invalid_input" "The standalone arguments are invalid.")
             (fun () ->
                ABI.create_managed_standalone
                  ~control
                  ~execution
                  ~limits:(EC.execution_limits prepared)
                  ~validate_work)
         in
         let%bind moderation =
           checked
             (fail
                "invocation.pre_tool_failed"
                "The tool moderation scope is unavailable.")
             Native_tool_moderation.current
         in
         let%bind ceiling =
           checked
             (fail "invocation.inactive_scope" "The tool scope is no longer active.")
             (fun () -> N.borrowed_capabilities borrowed)
         in
         let valid_parent =
           I.equal parent (N.borrowed_invocation borrowed)
           && String.equal (C.fingerprint selected) (C.fingerprint ceiling)
         in
         let%bind () =
           match valid_parent with
           | true -> Ok ()
           | false ->
             Error
               (fail
                  "invocation.inactive_scope"
                  "The managed tool scope does not own this invocation.")
         in
         let%bind value =
           with_script_native_calls
             ?observer:(Native_tool_moderation.observer moderation)
             ?moderator_execute:(N.moderator_executor borrowed)
             t
             ~selected
             ~limits:(EC.execution_limits prepared)
             ~execute:(N.execute_borrowed borrowed)
             ~valid_parent
             ~parent
             ~moderate:(Native_tool_moderation.prepare moderation)
             (fun on_tool_call ->
                let handlers =
                  { R.default_handlers with
                    on_tool_call =
                      (fun _ ~name ~args ->
                        let%bind args = V.export_json ?control args in
                        let%map result = on_tool_call ~name ~args in
                        match result with
                        | M.Tool_ok value ->
                          L.VVariant ("Ok", [ V.import_json ?control value ])
                        | Tool_error message -> L.VVariant ("Error", [ L.VString message ]))
                  }
                in
                let config : R.runtime_config =
                  { surface = Chatml.Chatml_extension_surface.tool_v1
                  ; operations = R.default_operations ~handlers ()
                  }
                in
                let config =
                  match jobs with
                  | None -> config
                  | Some jobs -> Script_job_service.install ?control jobs config
                in
                let prepare_result =
                  Option.map jobs ~f:(fun _ ~value:_ ~local_effects ->
                    start_effects := local_effects;
                    Ok ignore)
                in
                Chatml_execution.run_in_scope
                  ?prepare_result
                  ~control
                  ~config
                  ~program:(EC.program prepared)
                  ~entrypoint
                  ~arguments:[ ABI.context scope; ABI.input scope ]
                  ())
           |> Result.map_error ~f:(fun error ->
             fail error.Chatml_execution.code "Standalone execution failed.")
         in
         let%bind outcome =
           checked
             (fail
                "invocation.invalid_output"
                "The standalone handler returned an invalid outcome.")
             (fun () -> ABI.decode_outcome ?control scope value)
         in
         let%bind result = with_result ~validate_work ~outcome in
         let%map () =
           checked
             (fail
                "invocation.background_rejected"
                "The background starts could not be committed.")
             (fun () ->
                match jobs with
                | None -> Ok ()
                | Some jobs ->
                  let%bind ordinary = Script_job_service.select jobs !start_effects in
                  (match ordinary with
                   | [] -> Ok ()
                   | _ -> Error "standalone returned unsupported local effects"))
         in
         result)
  in
  Chatml_execution.with_control
    ~context:(N.borrowed_execution_context borrowed)
    ~policy:(Bounded (service.execution_limits prepared))
    ~env:service.env
    execute
  |> Result.map_error ~f:(fun error -> fail error.code error.message)
  |> Result.join
;;

let with_managed_tools t ~env ~definition ~execution_limits =
  let service = { env; definition; execution_limits } in
  { t with
    managed =
      Some
        (fun scope ->
          { Native_tool_invocation.definition
          ; current = scope.registry
          ; run =
              (fun execution borrowed with_result ->
                run_managed scope service execution borrowed with_result)
          })
  }
;;

let managed_registry t =
  Option.map t.managed ~f:(fun service -> (service t).Native_tool_invocation.definition)
;;

let with_inherited_managed_tools t ~env ~delegation ~current ~execution_limits =
  let definition = Managed.delegation_registry delegation in
  let registry () =
    let current = current () in
    Managed.revalidate definition ~current
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith;
    C.select
      current
      ~names:
        (List.map
           (C.references (Managed.capabilities definition))
           ~f:(fun reference -> reference.C.name))
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
  in
  let jobs =
    Option.map t.jobs ~f:(fun jobs ->
      Script_job_service.with_current_capabilities jobs registry)
  in
  with_managed_tools { t with registry; jobs } ~env ~definition ~execution_limits
;;

let with_standalone ?observer t ~prepared ~capabilities ~(parent : I.t) ~moderate f =
  let selected = EC.capabilities prepared in
  let valid_parent =
    match parent.status, (EC.declaration prepared).implementation with
    | Dispatching, Standalone _ ->
      String.equal parent.context.tool_name (EC.declaration prepared).name
      && String.equal parent.context.implementation_revision (EC.fingerprint prepared)
      && String.equal parent.context.capability_fingerprint (C.fingerprint selected)
    | _ -> false
  in
  with_script_native_calls
    ?observer
    ~moderator_execute:
      capabilities.Operation_worker.Capabilities.with_moderator_invocation
    t
    ~selected
    ~limits:(EC.execution_limits prepared)
    ~valid_parent
    ~execute:capabilities.Operation_worker.Capabilities.with_invocation
    ~parent
    ~moderate
    f
;;

let validate_one_off t prepared =
  Chat_response.One_off_script.revalidate prepared ~capabilities:(t.registry ())
  |> Result.map_error ~f:(fun error -> error.C.message)
;;

let with_one_off ?observer t ~prepared ~limits ~max_nested_calls ~borrowed ~moderate f =
  let module P = Chat_response.One_off_script in
  let parent = Native_tool_invocation.borrowed_invocation borrowed in
  let selected = P.capabilities prepared in
  let valid_parent =
    match parent.status, parent.context.origin with
    | Dispatching, Script ->
      String.equal parent.context.implementation_revision (P.fingerprint prepared)
      && String.equal parent.context.capability_fingerprint (C.fingerprint selected)
      &&
        (match Native_tool_invocation.borrowed_capabilities borrowed with
        | Ok ceiling -> String.equal (C.fingerprint ceiling) (C.fingerprint selected)
        | Error _ -> false)
    | _ -> false
  in
  with_script_native_calls
    ?observer
    ~max_nested_calls
    ?moderator_execute:(Native_tool_invocation.moderator_executor borrowed)
    t
    ~selected
    ~limits
    ~valid_parent
    ~execute:(Native_tool_invocation.execute_borrowed borrowed)
    ~parent
    ~moderate
    f
;;

let with_moderator_scope t ~definition ~execute ~parent ~valid_parent f =
  match
    List.find_map (EC.compiled_scripts definition) ~f:(fun (script, _) ->
      match script.Chatmd_shell_spec.Extension_spec.kind with
      | Moderator_script -> Some script
      | Tool_script -> None)
  with
  | None -> f (fun ~name:_ ~args:_ -> tool_error "invocation.inactive_scope")
  | Some script ->
    let observer : I.observer =
      { script_id = script.id; source_sha256 = script.source_sha256 }
    in
    let moderator_names =
      List.fold
        (EC.prepared_tools definition)
        ~init:t.moderator_names
        ~f:(fun names prepared ->
          match (EC.declaration prepared).implementation with
          | Moderator _ -> Set.add names (EC.declaration prepared).name
          | Standalone _ -> names)
    in
    with_scope
      { t with moderator_names }
      ~selected:(EC.definition_capabilities definition)
      ~limits:script.limits
      ~origin:Moderator
      ~observer:(Some observer)
      ~valid_parent:(valid_parent observer)
      ~execute
      ~parent
      f
;;

let with_observation t ~definition ~execute ~(observing : I.t) f =
  with_moderator_scope
    t
    ~definition
    ~execute
    ~parent:(Invocation observing)
    ~valid_parent:(fun observer ->
      match observing.status, observing.observation with
      | (Resolved _ | Published _), Some { observer = owner; status = Observing; _ } ->
        I.equal_observer owner observer
      | _ -> false)
    f
;;

let with_event t ~definition ~execute ~(executing : E.t) f =
  with_moderator_scope
    t
    ~definition
    ~execute
    ~parent:(Event executing)
    ~valid_parent:(fun observer ->
      match executing.status with
      | Running ->
        Result.is_ok (E.validate executing)
        && I.equal_observer executing.context.source observer
      | Completed _ | Failed _ | Interrupted _ -> false)
    f
;;
