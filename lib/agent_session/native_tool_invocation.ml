open Core
module I = Agent_protocol.Invocation
module H = Agent_protocol.Host_invocation
module D = Developer_invocation_owner
module C = Chat_response.Tool_capability
module S = Chatmd_shell_spec.Tool_schema
module Managed = Chat_response.Managed_tool_registry

type executor =
  invocation:I.t
  -> (dispatched:I.t -> (I.outcome, Agent_protocol.Error.t) result)
  -> (I.t, Agent_protocol.Error.t) result

type owned_executor =
  invocation:H.t
  -> (active:D.active -> dispatched:H.t -> (I.outcome, Agent_protocol.Error.t) result)
  -> (H.t, Agent_protocol.Error.t) result

type moderator_executor =
  invocation:I.t
  -> (dispatched:I.t
      -> commit:
           (resolved:I.t
            -> snapshot:Session.Moderator_state.Identity_snapshot.t
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result

type scope =
  | Unbound
  | Active of I.t
  | Expired

type borrowed =
  { invocation : I.t
  ; active : bool Atomic.t
  ; execute : executor
  ; moderator_execute : moderator_executor option
  ; ceiling : C.t option
  ; child_origin : I.origin
  ; coordination_context : Chat_response.Execution_gate.context
  ; execution_context : Chatml_execution.context
  ; runtime_context : Chat_response.Runtime_request_scope.t option
  ; moderation_context : Native_tool_moderation.t option
  ; authoring_reference_context : Authoring_reference_scope.t option
  }

type owned_borrowed =
  { invocation : H.t
  ; issuer : D.t
  ; parent_claim : D.active
  ; active : bool Atomic.t
  ; domain_id : int
  ; ceiling : C.t option
  ; child_origin : I.origin
  ; coordination_context : Chat_response.Execution_gate.context
  ; execution_context : Chatml_execution.context
  ; runtime_context : Chat_response.Runtime_request_scope.t option
  }

type managed_dispatch =
  { definition : Managed.t
  ; current : unit -> C.t
  ; run :
      'a.
      Managed.execution
      -> borrowed
      -> (validate_work:(I.work -> (unit, string) result)
          -> outcome:I.outcome
          -> ('a, I.outcome) result)
      -> ('a, I.outcome) result
  }

type owned_managed_dispatch =
  { definition : Managed.t
  ; current : unit -> C.t
  ; run :
      'a.
      Managed.owned_execution
      -> owned_borrowed
      -> (validate_work:(I.work -> (unit, string) result)
          -> outcome:I.outcome
          -> ('a, I.outcome) result)
      -> ('a, I.outcome) result
  }

let scope_key : borrowed Eio.Fiber.key = Eio.Fiber.create_key ()
let owned_scope_key : owned_borrowed Eio.Fiber.key = Eio.Fiber.create_key ()

type native_admission =
  { invocation_context : I.context
  ; admitted_binding : C.binding
  ; admission_active : bool Atomic.t
  }

let native_admission_key : native_admission Eio.Fiber.key = Eio.Fiber.create_key ()

type owned_native_admission =
  { invocation_context : H.context
  ; admitted_binding : C.binding
  ; admission_active : bool Atomic.t
  }

let owned_native_admission_key : owned_native_admission Eio.Fiber.key =
  Eio.Fiber.create_key ()
;;

let current_scope () =
  match Eio.Fiber.get owned_scope_key with
  | Some _ -> Expired
  | None ->
    (match Eio.Fiber.get scope_key with
     | None -> Unbound
     | Some scope ->
       (match Atomic.get scope.active with
        | true -> Active scope.invocation
        | false -> Expired))
;;

let borrow () =
  match Eio.Fiber.get owned_scope_key, Eio.Fiber.get scope_key with
  | Some _, _ ->
    Error
      (Agent_protocol.Error.invalid_request
         "conversation invocation scope is unavailable")
  | None, Some scope when Atomic.get scope.active -> Ok scope
  | None, (None | Some _) ->
    Error (Agent_protocol.Error.invalid_request "native invocation scope is not active")
;;

let borrowed_invocation (scope : borrowed) = scope.invocation
let borrowed_execution_context (scope : borrowed) = scope.execution_context

let borrowed_capabilities (scope : borrowed) =
  match Atomic.get scope.active, scope.ceiling with
  | true, Some selected -> Ok selected
  | _ ->
    Error
      (Agent_protocol.Error.invalid_request
         "borrowed scope has no active verified tool capabilities")
;;

let require_native_admission (scope : borrowed) ~binding =
  let open Result.Let_syntax in
  let denied () =
    Error (Agent_protocol.Error.invalid_request "native handler admission is unavailable")
  in
  let%bind ceiling = borrowed_capabilities scope in
  match Eio.Fiber.get native_admission_key, current_scope () with
  | Some admission, Active current
    when Atomic.get admission.admission_active
         && I.equal_context admission.invocation_context current.context
         && I.equal_context scope.invocation.context current.context ->
    let reference = C.reference binding in
    let admitted = C.reference admission.admitted_binding in
    let%bind selected =
      C.resolve ceiling ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request error.C.message)
    in
    (match
       ( C.native_implementation binding
       , C.native_implementation admission.admitted_binding
       , C.native_implementation selected )
     with
     | Some implementation, Some admitted_implementation, Some selected_implementation
       when Agent_protocol.Id.Capability.equal reference.id admitted.id
            && String.equal reference.fingerprint admitted.fingerprint
            && phys_equal implementation admitted_implementation
            && phys_equal implementation selected_implementation -> Ok ()
     | Some _, _, _ | None, _, _ -> denied ())
  | Some _, _ | None, _ -> denied ()
;;

let with_native_admission invocation binding f =
  let admission : native_admission =
    { invocation_context = invocation.I.context
    ; admitted_binding = binding
    ; admission_active = Atomic.make true
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set admission.admission_active false)
    ~f:(fun () -> Eio.Fiber.with_binding native_admission_key admission f)
;;

let owned_active (scope : owned_borrowed) =
  Atomic.get scope.active && Int.equal scope.domain_id (Stdlib.Domain.self () :> int)
;;

let borrow_owned () =
  match Eio.Fiber.get owned_scope_key with
  | Some scope when owned_active scope -> Ok scope
  | Some _ | None ->
    Error
      (Agent_protocol.Error.invalid_request "owned native invocation scope is not active")
;;

let owned_borrowed_invocation (scope : owned_borrowed) = scope.invocation
let owned_borrowed_execution_context (scope : owned_borrowed) = scope.execution_context

let owned_borrowed_capabilities (scope : owned_borrowed) =
  match owned_active scope, scope.ceiling with
  | true, Some selected -> Ok selected
  | _ ->
    Error
      (Agent_protocol.Error.invalid_request
         "owned borrowed scope has no active verified tool capabilities")
;;

let require_owned_native_admission (scope : owned_borrowed) ~binding =
  let open Result.Let_syntax in
  let denied () =
    Error (Agent_protocol.Error.invalid_request "native handler admission is unavailable")
  in
  let%bind ceiling = owned_borrowed_capabilities scope in
  match Eio.Fiber.get owned_native_admission_key, Eio.Fiber.get owned_scope_key with
  | Some admission, Some current
    when owned_active current
         && Atomic.get admission.admission_active
         && H.equal_context admission.invocation_context (H.context current.invocation)
         && H.equal_context (H.context scope.invocation) (H.context current.invocation)
         && phys_equal scope.issuer current.issuer ->
    let reference = C.reference binding in
    let admitted = C.reference admission.admitted_binding in
    let%bind selected =
      C.resolve ceiling ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request error.C.message)
    in
    (match
       ( C.native_implementation binding
       , C.native_implementation admission.admitted_binding
       , C.native_implementation selected )
     with
     | Some implementation, Some admitted_implementation, Some selected_implementation
       when Agent_protocol.Id.Capability.equal reference.id admitted.id
            && String.equal reference.fingerprint admitted.fingerprint
            && phys_equal implementation admitted_implementation
            && phys_equal implementation selected_implementation -> Ok ()
     | Some _, _, _ | None, _, _ -> denied ())
  | Some _, _ | None, _ -> denied ()
;;

let with_owned_native_admission invocation binding f =
  let admission : owned_native_admission =
    { invocation_context = H.context invocation
    ; admitted_binding = binding
    ; admission_active = Atomic.make true
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set admission.admission_active false)
    ~f:(fun () -> Eio.Fiber.with_binding owned_native_admission_key admission f)
;;

let select_owned_tools (scope : owned_borrowed) ~names =
  let open Result.Let_syntax in
  let%bind ceiling = owned_borrowed_capabilities scope in
  let%map selected =
    C.select ceiling ~names
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request error.C.message)
  in
  { scope with ceiling = Some selected }
;;

let with_owned_scope
      ?ceiling
      ?(child_origin = I.Script)
      ?execution_context
      ?runtime_context
      ~issuer
      ~parent_claim
      invocation
      f
  =
  let execution_context =
    Chatml_execution.capture_context ?inherited:execution_context ()
  in
  let runtime_context =
    Option.value_or_thunk
      runtime_context
      ~default:Chat_response.Runtime_request_scope.capture
  in
  let scope =
    { invocation
    ; issuer
    ; parent_claim
    ; active = Atomic.make true
    ; domain_id = (Stdlib.Domain.self () :> int)
    ; ceiling
    ; child_origin
    ; coordination_context = Chat_response.Execution_gate.capture_context ()
    ; execution_context
    ; runtime_context
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set scope.active false)
    ~f:(fun () ->
      Chat_response.Runtime_request_scope.with_context runtime_context (fun () ->
        Eio.Fiber.with_binding owned_scope_key scope f))
;;

let select_tools (scope : borrowed) ~names =
  let open Result.Let_syntax in
  let%bind ceiling = borrowed_capabilities scope in
  let%map selected =
    C.select ceiling ~names
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request error.C.message)
  in
  { scope with ceiling = Some selected }
;;

let borrow_for_fork () =
  let open Result.Let_syntax in
  let%bind scope = borrow () in
  let%bind selected = borrowed_capabilities scope in
  let%bind binding =
    C.find selected ~name:scope.invocation.context.tool_name
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request error.C.message)
  in
  match C.implementation binding with
  | Native implementation
    when phys_equal implementation.run_with_progress Functions.fork.run_with_progress ->
    Ok { scope with child_origin = I.Delegated_agent }
  | Native _ | Managed _ ->
    Error
      (Agent_protocol.Error.invalid_request
         "fork requires its active built-in invocation")
;;

let record_authoring_reference (scope : borrowed) response =
  let open Result.Let_syntax in
  let%bind selected = borrowed_capabilities scope in
  match scope.authoring_reference_context with
  | None -> Ok ()
  | Some collector ->
    Authoring_reference_scope.record
      collector
      ~invocation_id:scope.invocation.context.id
      ~capability_fingerprint:(C.fingerprint selected)
      response
;;

let with_scope
      ?ceiling
      ?moderator_execute
      ?(child_origin = I.Script)
      ?execution_context
      ?runtime_context
      ?moderation_context
      ~execute
      invocation
      f
  =
  let execute, moderator_execute, ceiling, execution_context, authoring_reference_context =
    match Eio.Fiber.get scope_key with
    | Some scope when Atomic.get scope.active && I.equal scope.invocation invocation ->
      (* Preserve the real actor executor rather than inheriting a direct-child
         adapter from the parent. Each lexical scope still owns its lifetime. *)
      ( scope.execute
      , Option.first_some scope.moderator_execute moderator_execute
      , Option.first_some ceiling scope.ceiling
      , Option.value execution_context ~default:scope.execution_context
      , scope.authoring_reference_context )
    | None | Some _ ->
      ( execute
      , moderator_execute
      , ceiling
      , Option.value_or_thunk execution_context ~default:(fun () ->
          Chatml_execution.capture_context ())
      , Authoring_reference_scope.capture ~invocation_id:invocation.context.id )
  in
  let execution_context =
    Chatml_execution.capture_context ~inherited:execution_context ()
  in
  let active = Atomic.make true in
  let runtime_context =
    Option.value_or_thunk
      runtime_context
      ~default:Chat_response.Runtime_request_scope.capture
  in
  let moderation_context =
    Option.value_or_thunk moderation_context ~default:Native_tool_moderation.capture
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set active false)
    ~f:(fun () ->
      Chat_response.Runtime_request_scope.with_context runtime_context (fun () ->
        Native_tool_moderation.with_context moderation_context (fun () ->
          Eio.Fiber.with_binding
            scope_key
            { invocation
            ; active
            ; execute
            ; moderator_execute
            ; ceiling
            ; child_origin
            ; coordination_context = Chat_response.Execution_gate.capture_context ()
            ; execution_context
            ; runtime_context
            ; moderation_context
            ; authoring_reference_context
            }
            f)))
;;

let check_active (scope : borrowed) =
  match Atomic.get scope.active with
  | true -> Ok ()
  | false ->
    Error (Agent_protocol.Error.invalid_request "borrowed invocation scope expired")
;;

let check_child (scope : borrowed) invocation =
  let open Result.Let_syntax in
  let%bind () = check_active scope in
  let%bind ceiling = borrowed_capabilities scope in
  let parent = scope.invocation.context in
  let child = invocation.I.context in
  let deadline_within_parent =
    match parent.deadline, child.deadline with
    | None, _ -> true
    | Some parent, Some child -> Agent_protocol.Timestamp.compare child parent <= 0
    | Some _, None -> false
  in
  let%map () =
    match child.origin, child.parent_invocation with
    | origin, Some parent_id
      when I.equal_origin origin scope.child_origin
           && Agent_protocol.Id.Invocation.equal parent_id parent.id
           && Agent_protocol.Id.Session.equal child.session_id parent.session_id
           && Int.equal child.generation parent.generation
           && Option.is_none child.provider_call_id
           && Option.is_none child.call_entry_id
           && Option.is_none child.parent_job
           && Option.is_none invocation.parent_event
           && String.equal child.capability_fingerprint (C.fingerprint ceiling)
           && deadline_within_parent -> Ok ()
    | _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "borrowed execution requires a script child of the current invocation")
  in
  ceiling
;;

let execute_borrowed (scope : borrowed) ~invocation f =
  let open Result.Let_syntax in
  let%bind ceiling = check_child scope invocation in
  Chat_response.Execution_gate.with_context scope.coordination_context (fun () ->
    scope.execute ~invocation (fun ~dispatched ->
      (* Actor admission can yield while the native callback returns. A retained
       executor must not start effects after its lending scope has expired. *)
      let%bind () = check_active scope in
      with_scope
        ~ceiling
        ?moderator_execute:scope.moderator_execute
        ~execution_context:scope.execution_context
        ~runtime_context:scope.runtime_context
        ~moderation_context:scope.moderation_context
        ~execute:scope.execute
        dispatched
        (fun () -> f ~dispatched)))
;;

let execute_owned_borrowed (scope : owned_borrowed) ~invocation f =
  let open Result.Let_syntax in
  let%bind selected = owned_borrowed_capabilities scope in
  let context = H.context invocation in
  let parent = H.context scope.invocation in
  let%bind () =
    if
      I.equal_origin context.origin scope.child_origin
      && Option.exists context.parent_invocation ~f:(fun id ->
        Agent_protocol.Id.Invocation.equal id parent.id)
      && String.equal context.capability_fingerprint (C.fingerprint selected)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "owned borrowed execution requires a selected direct child")
  in
  Chat_response.Execution_gate.with_context scope.coordination_context (fun () ->
    D.with_invocation
      scope.issuer
      ~parent:(D.Child scope.parent_claim)
      ~invocation
      (fun ~active ~dispatched ->
         let%bind () =
           if owned_active scope
           then Ok ()
           else Error (Agent_protocol.Error.invalid_request "owned borrow expired")
         in
         let%bind () = D.current scope.issuer in
         with_owned_scope
           ~ceiling:selected
           ~child_origin:scope.child_origin
           ~execution_context:scope.execution_context
           ~runtime_context:scope.runtime_context
           ~issuer:scope.issuer
           ~parent_claim:active
           dispatched
           (fun () -> f ~active ~dispatched)))
;;

let execute_moderator_borrowed (scope : borrowed) ~invocation f =
  let open Result.Let_syntax in
  let%bind () =
    match invocation.I.context.origin with
    | Script | Delegated_agent -> Ok ()
    | _ ->
      Error
        (Agent_protocol.Error.invalid_request "moderator handoff requires a scoped child")
  in
  let%bind ceiling = check_child scope invocation in
  let%bind execute =
    Result.of_option
      scope.moderator_execute
      ~error:
        (Agent_protocol.Error.invalid_request
           "borrowed moderator executor is unavailable")
  in
  Chat_response.Execution_gate.with_context scope.coordination_context (fun () ->
    execute ~invocation (fun ~dispatched ~commit ->
      let%bind () = check_active scope in
      with_scope
        ~ceiling
        ?moderator_execute:scope.moderator_execute
        ~execution_context:scope.execution_context
        ~runtime_context:scope.runtime_context
        ~moderation_context:scope.moderation_context
        ~execute:scope.execute
        dispatched
        (fun () -> f ~dispatched ~commit)))
;;

let moderator_executor (scope : borrowed) =
  Option.map scope.moderator_execute ~f:(fun _ -> execute_moderator_borrowed scope)
;;

let with_managed_scope execution f =
  let open Result.Let_syntax in
  let%bind scope = borrow () in
  let%bind selected = borrowed_capabilities scope in
  let reference = C.reference (Managed.binding execution) in
  let%bind () =
    match I.equal scope.invocation (Managed.invocation execution) with
    | false ->
      Error
        (Agent_protocol.Error.invalid_request
           "managed admission belongs to another invocation")
    | true ->
      C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map ~f:ignore
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request error.C.message)
  in
  let prepared = Managed.prepared execution in
  let child_origin =
    match (Chat_response.Extension_compiler.declaration prepared).implementation with
    | Standalone _ -> I.Script
    | Moderator _ -> I.Moderator
  in
  with_scope
    ~ceiling:(Chat_response.Extension_compiler.capabilities prepared)
    ~child_origin
    ?moderator_execute:scope.moderator_execute
    ~execution_context:scope.execution_context
    ~runtime_context:scope.runtime_context
    ~moderation_context:scope.moderation_context
    ~execute:scope.execute
    scope.invocation
    (fun () -> Result.bind (borrow ()) ~f)
;;

let with_owned_managed_scope execution f =
  let open Result.Let_syntax in
  let%bind scope = borrow_owned () in
  let%bind selected = owned_borrowed_capabilities scope in
  let reference = C.reference (Managed.owned_binding execution) in
  let%bind () =
    if
      H.equal_context
        (H.context scope.invocation)
        (H.context (Managed.owned_invocation execution))
      && I.equal_status
           (H.status scope.invocation)
           (H.status (Managed.owned_invocation execution))
    then
      C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map ~f:ignore
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request error.C.message)
    else
      Error
        (Agent_protocol.Error.invalid_request
           "owned managed admission belongs to another invocation")
  in
  let prepared = Managed.owned_prepared execution in
  let%bind () =
    match (Chat_response.Extension_compiler.declaration prepared).implementation with
    | Standalone _ -> Ok ()
    | Moderator _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "developer managed invocation requires a standalone handler")
  in
  with_owned_scope
    ~ceiling:(Chat_response.Extension_compiler.capabilities prepared)
    ~issuer:scope.issuer
    ~parent_claim:scope.parent_claim
    ~child_origin:I.Script
    ~execution_context:scope.execution_context
    ~runtime_context:scope.runtime_context
    scope.invocation
    (fun () -> Result.bind (borrow_owned ()) ~f)
;;

let with_dispatched_scope ~execute ?moderator_execute ~selected ~invocation f =
  let open Result.Let_syntax in
  let%bind () = I.validate invocation in
  match invocation.status with
  | Dispatching
    when String.equal invocation.context.capability_fingerprint (C.fingerprint selected)
    -> with_scope ~execute ?moderator_execute ~ceiling:selected invocation f
  | _ ->
    Error
      (Agent_protocol.Error.invalid_request
         "expected an actor-dispatched capability selection")
;;

let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let checked failure f =
  match f () with
  | Ok value -> Ok value
  | Error _ -> Error failure
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception _ -> Error failure
;;

let with_selected_capabilities selected f =
  let failure =
    fail
      "invocation.unselected_tool"
      "Native dispatch would widen its borrowed tool capabilities."
  in
  match Eio.Fiber.get scope_key with
  | None -> Error failure
  | Some scope ->
    let open Result.Let_syntax in
    let%bind () =
      checked failure (fun () ->
        match scope.ceiling with
        | None -> Ok ()
        | Some ceiling ->
          List.fold (C.references selected) ~init:(Ok ()) ~f:(fun result reference ->
            let%bind () = result in
            C.resolve ceiling ~id:reference.id ~fingerprint:reference.fingerprint
            |> Result.map ~f:ignore))
    in
    with_scope ~ceiling:selected ~execute:scope.execute scope.invocation f
;;

let with_owned_selected_capabilities selected f =
  let failure =
    fail
      "invocation.unselected_tool"
      "Native dispatch would widen its borrowed tool capabilities."
  in
  match Eio.Fiber.get owned_scope_key with
  | None -> Error failure
  | Some scope ->
    let open Result.Let_syntax in
    let%bind () =
      checked failure (fun () ->
        let%bind ceiling = owned_borrowed_capabilities scope in
        List.fold (C.references selected) ~init:(Ok ()) ~f:(fun result reference ->
          let%bind () = result in
          C.resolve ceiling ~id:reference.id ~fingerprint:reference.fingerprint
          |> Result.map_error ~f:(fun error ->
            Agent_protocol.Error.invalid_request error.C.message)
          |> Result.map ~f:ignore))
    in
    with_owned_scope
      ~ceiling:selected
      ~issuer:scope.issuer
      ~parent_claim:scope.parent_claim
      ~child_origin:scope.child_origin
      ~execution_context:scope.execution_context
      ~runtime_context:scope.runtime_context
      scope.invocation
      f
;;

type managed_target = Native_tool_policy.managed_target

let conversation_context invocation =
  match H.of_conversation invocation with
  | Ok owned -> H.context owned
  | Error _ -> failwith "actor-dispatched conversation invocation is invalid"
;;

let conversation_kind_matches invocation binding =
  match invocation.I.routing, (C.descriptor binding).type_ with
  | None, ("function" | "custom") -> true
  | Some { kind = I.Function; _ }, "function" | Some { kind = I.Custom; _ }, "custom" ->
    true
  | _ -> false
;;

let conversation_managed_target
      (service : managed_dispatch)
      ~reference
      ~selected
      ~dispatched
  =
  Managed.admit
    service.definition
    ~current:(service.current ())
    ~selected
    ~reference
    ~invocation:dispatched
  |> Result.map ~f:(fun execution ->
    ({ prepared = Managed.prepared execution
     ; run =
         (fun continuation ->
           match
             with_managed_scope execution (fun borrowed ->
               Ok (service.run execution borrowed continuation))
           with
           | Ok result -> result
           | Error _ -> Error (fail "invocation.handler_failed" "Tool execution failed."))
     }
     : managed_target))
;;

let owned_managed_target
      (service : owned_managed_dispatch)
      ~reference
      ~selected
      ~dispatched
  =
  Managed.admit_owned
    service.definition
    ~current:(service.current ())
    ~selected
    ~reference
    ~invocation:dispatched
  |> Result.map ~f:(fun execution ->
    ({ prepared = Managed.owned_prepared execution
     ; run =
         (fun continuation ->
           match
             with_owned_managed_scope execution (fun borrowed ->
               Ok (service.run execution borrowed continuation))
           with
           | Ok result -> result
           | Error _ -> Error (fail "invocation.handler_failed" "Tool execution failed."))
     }
     : managed_target))
;;

let run_with_native
      ~run_native
      ~on_progress
      ~(managed : managed_dispatch option)
      ~moderator_execute
      ~execute
      ~registry
      ~(reference : C.reference)
      ~invocation
      ~is_halted
      ~authorize
      ~prepare_output
  =
  let scoped_execute ~invocation f =
    execute ~invocation (fun ~dispatched ->
      with_scope ?moderator_execute ~execute dispatched (fun () -> f ~dispatched))
  in
  let managed_revision binding =
    match C.implementation binding, managed with
    | Managed _, Some service -> Managed.execution_revision service.definition binding
    | Native _, _ | Managed _, None -> Ok reference.implementation_revision
  in
  let managed_admit =
    Option.map managed ~f:(fun service binding selected dispatched ->
      conversation_managed_target service ~reference ~selected ~dispatched)
  in
  let check_halted () =
    let failure = fail "invocation.session_ended" "The session has ended." in
    match checked failure (fun () -> Ok (is_halted ())) with
    | Ok false -> Ok ()
    | Ok true | Error _ -> Error failure
  in
  Native_tool_policy.run_policy
    ~run_native
    ~on_progress
    ~managed_revision
    ~managed_admit
    ~execute:scoped_execute
    ~registry:(fun () -> Ok (registry ()))
    ~reference
    ~invocation
    ~check_halted
    ~context:conversation_context
    ~kind_matches:conversation_kind_matches
    ~preparation:(fun invocation ->
      Option.map invocation.I.routing ~f:(fun routing -> routing.preparation))
    ~with_selected:with_selected_capabilities
    ~with_native_admission
    ~authorize
    ~prepare_output
;;

let run_owned
      ~owner
      ~parent_claim
      ~execute
      ~registry
      ~(managed : owned_managed_dispatch option)
      ~(reference : C.reference)
      ~invocation
      ~authorize
      ~prepare_output
  =
  let managed_revision binding =
    match C.implementation binding, managed with
    | Managed _, Some service -> Managed.execution_revision service.definition binding
    | Native _, _ | Managed _, None -> Ok reference.implementation_revision
  in
  let managed_admit =
    Option.map managed ~f:(fun service _binding selected dispatched ->
      owned_managed_target service ~reference ~selected ~dispatched)
  in
  let check_halted () =
    match D.check_invocation owner ~parent:parent_claim invocation with
    | Ok () -> Ok ()
    | Error _ -> Error (fail "invocation.run_ended" "The developer run is unavailable.")
  in
  Native_tool_policy.run_policy
    ~run_native:None
    ~on_progress:None
    ~managed_revision
    ~managed_admit
    ~execute
    ~registry
    ~reference
    ~invocation
    ~check_halted
    ~context:H.context
    ~kind_matches:(fun _ binding ->
      match (C.descriptor binding).type_ with
      | "function" | "custom" -> true
      | _ -> false)
    ~preparation:(fun _ -> None)
    ~with_selected:with_owned_selected_capabilities
    ~with_native_admission:with_owned_native_admission
    ~authorize
    ~prepare_output
;;

let run_owned_root ~owner ~managed ~reference ~invocation ~authorize ~prepare_output =
  let execute ~invocation f =
    D.with_invocation owner ~parent:D.Root ~invocation (fun ~active ~dispatched ->
      with_owned_scope
        ~ceiling:(D.ceiling owner)
        ~issuer:owner
        ~parent_claim:active
        dispatched
        (fun () -> f ~dispatched))
  in
  run_owned
    ~owner
    ~parent_claim:D.Root
    ~execute
    ~registry:(fun () ->
      D.selected owner
      |> Result.map_error ~f:(fun _ ->
        C.
          { code = "capability.invalid_selection"
          ; message = "developer selection widened"
          }))
    ~managed
    ~reference
    ~invocation
    ~authorize
    ~prepare_output
;;

let run_owned_child ~parent ~managed ~reference ~invocation ~authorize ~prepare_output =
  let registry () =
    let open Result.Let_syntax in
    let%bind ceiling =
      owned_borrowed_capabilities parent
      |> Result.map_error ~f:(fun _ ->
        C.{ code = "capability.expired_borrow"; message = "owned borrow expired" })
    in
    let%bind current =
      D.selected parent.issuer
      |> Result.map_error ~f:(fun _ ->
        C.
          { code = "capability.invalid_selection"
          ; message = "developer selection widened"
          })
    in
    let%bind names =
      List.fold (C.references ceiling) ~init:(Ok []) ~f:(fun result reference ->
        let%bind names = result in
        let%map _ =
          C.resolve current ~id:reference.id ~fingerprint:reference.fingerprint
        in
        reference.name :: names)
    in
    C.select current ~names:(List.rev names)
  in
  run_owned
    ~owner:parent.issuer
    ~parent_claim:(D.Child parent.parent_claim)
    ~registry
    ~execute:(fun ~invocation f ->
      execute_owned_borrowed parent ~invocation (fun ~active:_ ~dispatched ->
        f ~dispatched))
    ~managed
    ~reference
    ~invocation
    ~authorize
    ~prepare_output
;;

let run_scoped_with_managed = run_with_native ~run_native:None

let run_scoped_in_driver ~run_native =
  run_with_native ~run_native:(Some run_native) ~on_progress:None
;;

let run_scoped =
  run_scoped_with_managed ~on_progress:None ~managed:None ~moderator_execute:None
;;

let run ~capabilities =
  run_scoped_with_managed
    ~on_progress:None
    ~managed:None
    ~moderator_execute:
      (Some capabilities.Operation_worker.Capabilities.with_moderator_invocation)
    ~execute:capabilities.Operation_worker.Capabilities.with_invocation
;;

let run_in_driver ~run_native ~capabilities =
  run_with_native
    ~run_native:(Some run_native)
    ~on_progress:None
    ~managed:None
    ~moderator_execute:
      (Some capabilities.Operation_worker.Capabilities.with_moderator_invocation)
    ~execute:capabilities.Operation_worker.Capabilities.with_invocation
;;
