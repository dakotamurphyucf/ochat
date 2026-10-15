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

type selected_borrowed =
  { invocation : H.t
  ; issuer : D.Selected.t
  ; parent_claim : D.Selected.active
  ; active : bool Atomic.t
  ; domain_id : int
  ; ceiling : C.t option
  ; effect_entry : (C.binding -> (unit, Agent_protocol.Error.t) result) option
  ; execution_context : Chatml_execution.context
  ; runtime_context : Chat_response.Runtime_request_scope.t option
  ; selected_ancestor : selected_borrowed option
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

type managed_script_install =
  { config : Chatml_host_runtime.runtime_config
  ; scoped_async_operation : Chatml_host_runtime.scoped_async_operation option
  }

type selected_managed_dispatch =
  { definition : Managed.t
  ; current : unit -> C.t
  ; run :
      'a.
      Managed.owned_execution
      -> selected_borrowed
      -> effect_entry:(unit -> (unit, Agent_protocol.Error.t) result)
      -> stage_background:
           (request:Chat_response.Background_request.t
            -> checkpoint:(unit -> unit)
            -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result)
      -> install_jobs:(Chatml_host_runtime.runtime_config -> managed_script_install)
      -> (validate_work:(I.work -> (unit, string) result)
          -> outcome:I.outcome
          -> selected_jobs:Agent_protocol.Id.Job.t list
          -> ('a, I.outcome) result)
      -> ('a, I.outcome) result
  }

let scope_key : borrowed Eio.Fiber.key = Eio.Fiber.create_key ()
let owned_scope_key : owned_borrowed Eio.Fiber.key = Eio.Fiber.create_key ()
let selected_scope_key : selected_borrowed Eio.Fiber.key = Eio.Fiber.create_key ()

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
  match Eio.Fiber.get owned_scope_key, Eio.Fiber.get selected_scope_key with
  | Some _, _ | _, Some _ -> Expired
  | None, None ->
    (match Eio.Fiber.get scope_key with
     | None -> Unbound
     | Some scope ->
       (match Atomic.get scope.active with
        | true -> Active scope.invocation
        | false -> Expired))
;;

let borrow () =
  match
    ( Eio.Fiber.get owned_scope_key
    , Eio.Fiber.get selected_scope_key
    , Eio.Fiber.get scope_key )
  with
  | Some _, _, _ | _, Some _, _ ->
    Error
      (Agent_protocol.Error.invalid_request
         "conversation invocation scope is unavailable")
  | None, None, Some scope when Atomic.get scope.active -> Ok scope
  | None, None, (None | Some _) ->
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

let selected_active (scope : selected_borrowed) =
  Atomic.get scope.active && Int.equal scope.domain_id (Stdlib.Domain.self () :> int)
;;

let borrow_selected () =
  match Eio.Fiber.get selected_scope_key with
  | Some scope when selected_active scope -> Ok scope
  | Some _ | None ->
    Error
      (Agent_protocol.Error.invalid_request
         "selected developer native invocation scope is not active")
;;

let selected_borrowed_issuer (scope : selected_borrowed) = scope.issuer
let selected_borrowed_invocation (scope : selected_borrowed) = scope.invocation
let selected_borrowed_parent (scope : selected_borrowed) = scope.parent_claim

let selected_borrowed_execution_context (scope : selected_borrowed) =
  match borrow_selected () with
  | Ok current when phys_equal current scope -> Ok scope.execution_context
  | Ok _ | Error _ ->
    Error
      (Agent_protocol.Error.invalid_request
         "selected developer native invocation scope is not active")
;;

let selected_borrowed_capabilities (scope : selected_borrowed) =
  match selected_active scope, scope.ceiling with
  | true, Some selected -> Ok selected
  | _ ->
    Error
      (Agent_protocol.Error.invalid_request
         "selected developer borrow has no live verified capabilities")
;;

let check_selected_ancestor ~ancestor ~child =
  let invalid () =
    Error
      (Agent_protocol.Error.invalid_request
         "selected developer child has no live physical saved-source ancestor")
  in
  match borrow_selected () with
  | Error _ -> invalid ()
  | Ok current when (not (phys_equal current child)) || phys_equal child ancestor ->
    invalid ()
  | Ok _ ->
    let rec walk scope crossed_invocation =
      if not (selected_active scope)
      then false
      else if phys_equal scope ancestor
      then crossed_invocation
      else (
        match scope.selected_ancestor with
        | None -> false
        | Some parent ->
          let child_context = H.context scope.invocation in
          let parent_context = H.context parent.invocation in
          let same_invocation =
            Agent_protocol.Id.Invocation.equal child_context.id parent_context.id
          in
          if
            (same_invocation && not (H.equal_context child_context parent_context))
            || ((not same_invocation)
                && not
                     (Option.value_map
                        child_context.parent_invocation
                        ~default:false
                        ~f:(Agent_protocol.Id.Invocation.equal parent_context.id)))
          then false
          else walk parent (crossed_invocation || not same_invocation))
    in
    if walk child false then Ok () else invalid ()
;;

let check_selected_source (scope : selected_borrowed) ~implementation =
  let open Result.Let_syntax in
  let invalid message = Error (Agent_protocol.Error.invalid_request message) in
  let current_scope () =
    selected_active scope
    && Option.value_map
         (Eio.Fiber.get selected_scope_key)
         ~default:false
         ~f:(fun active -> phys_equal active scope)
  in
  if not (current_scope ())
  then invalid "selected developer source callback has expired"
  else (
    let%bind ceiling = selected_borrowed_capabilities scope in
    let%bind selected = D.Selected.selected scope.issuer in
    if not (current_scope ())
    then invalid "selected developer source callback changed after admission"
    else (
      let context = Agent_protocol.Host_invocation.context scope.invocation in
      if not (String.equal (C.fingerprint ceiling) context.capability_fingerprint)
      then invalid "selected developer source registry differs from invocation"
      else (
        let%bind binding =
          C.find ceiling ~name:context.tool_name
          |> Result.map_error ~f:(fun error ->
            Agent_protocol.Error.invalid_request error.C.message)
        in
        let reference = C.reference binding in
        let%bind current =
          C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
          |> Result.map_error ~f:(fun error ->
            Agent_protocol.Error.invalid_request error.C.message)
        in
        if
          String.equal reference.implementation_revision context.implementation_revision
          && Option.value_map
               (C.native_implementation binding)
               ~default:false
               ~f:(phys_equal implementation)
          && Option.value_map
               (C.native_implementation current)
               ~default:false
               ~f:(phys_equal implementation)
          && current_scope ()
        then (
          let%bind effect_entry =
            scope.effect_entry
            |> Result.of_option
                 ~error:
                   (Agent_protocol.Error.invalid_request
                      "selected source has no physical effect admission")
          in
          let%bind () = effect_entry binding in
          if current_scope ()
          then Ok ()
          else invalid "selected developer source callback expired after admission")
        else invalid "selected developer source implementation changed")))
;;

let check_selected_content_context scope =
  let open Result.Let_syntax in
  let%bind ceiling = selected_borrowed_capabilities scope in
  let context = Agent_protocol.Host_invocation.context scope.invocation in
  let%bind binding =
    C.find ceiling ~name:context.tool_name
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request error.C.message)
  in
  let%bind implementation =
    C.native_implementation binding
    |> Result.of_option
         ~error:
           (Agent_protocol.Error.invalid_request
              "selected developer content has no native invocation")
  in
  check_selected_source scope ~implementation
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

let with_selected_scope
      ?ceiling
      ?effect_entry
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
  let selected_ancestor = Eio.Fiber.get selected_scope_key in
  let scope =
    { invocation
    ; issuer
    ; parent_claim
    ; active = Atomic.make true
    ; domain_id = (Stdlib.Domain.self () :> int)
    ; ceiling
    ; effect_entry
    ; execution_context
    ; runtime_context
    ; selected_ancestor
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set scope.active false)
    ~f:(fun () ->
      Chat_response.Runtime_request_scope.with_context runtime_context (fun () ->
        Eio.Fiber.with_binding selected_scope_key scope f))
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

let with_selected_managed_scope execution f =
  let open Result.Let_syntax in
  let%bind scope = borrow_selected () in
  let%bind selected = selected_borrowed_capabilities scope in
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
           "selected managed admission belongs to another invocation")
  in
  let prepared = Managed.owned_prepared execution in
  let%bind () =
    match (Chat_response.Extension_compiler.declaration prepared).implementation with
    | Standalone _ -> Ok ()
    | Moderator _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "developer selected managed invocation requires a standalone handler")
  in
  with_selected_scope
    ~ceiling:(Chat_response.Extension_compiler.capabilities prepared)
    ?effect_entry:scope.effect_entry
    ~issuer:scope.issuer
    ~parent_claim:scope.parent_claim
    ~execution_context:scope.execution_context
    ~runtime_context:scope.runtime_context
    scope.invocation
    (fun () -> Result.bind (borrow_selected ()) ~f)
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

let with_developer_selected_capabilities selected f =
  let failure =
    fail
      "invocation.unselected_tool"
      "Native dispatch would widen its selected developer borrow."
  in
  match Eio.Fiber.get selected_scope_key with
  | None -> Error failure
  | Some scope ->
    let open Result.Let_syntax in
    let%bind () =
      checked failure (fun () ->
        let%bind ceiling = selected_borrowed_capabilities scope in
        List.fold (C.references selected) ~init:(Ok ()) ~f:(fun result reference ->
          let%bind () = result in
          C.resolve ceiling ~id:reference.id ~fingerprint:reference.fingerprint
          |> Result.map_error ~f:(fun error ->
            Agent_protocol.Error.invalid_request error.C.message)
          |> Result.map ~f:ignore))
    in
    with_selected_scope
      ~ceiling:selected
      ?effect_entry:scope.effect_entry
      ~issuer:scope.issuer
      ~parent_claim:scope.parent_claim
      ~execution_context:scope.execution_context
      ~runtime_context:scope.runtime_context
      scope.invocation
      f
;;

type 'a managed_target = 'a Native_tool_policy.managed_target

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
               Ok
                 (service.run execution borrowed (fun ~validate_work ~outcome ->
                    continuation ~validate_work ~outcome ~selected_jobs:[])))
           with
           | Ok result -> result
           | Error _ -> Error (fail "invocation.handler_failed" "Tool execution failed."))
     }
     : I.outcome managed_target))
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
               Ok
                 (service.run execution borrowed (fun ~validate_work ~outcome ->
                    continuation ~validate_work ~outcome ~selected_jobs:[])))
           with
           | Ok result -> result
           | Error _ -> Error (fail "invocation.handler_failed" "Tool execution failed."))
     }
     : I.outcome managed_target))
;;

let selected_managed_target
      (service : selected_managed_dispatch)
      ~reference
      ~selected
      ~dispatched
      ~admit_managed
      ~effect_entry
      ~stage_background
      ~install_jobs
  =
  Managed.admit_owned
    service.definition
    ~current:(service.current ())
    ~selected
    ~reference
    ~invocation:dispatched
  |> Result.map ~f:(fun execution ->
    let prepared = Managed.owned_prepared execution in
    { Native_tool_policy.prepared
    ; run =
        (fun continuation ->
          match admit_managed execution with
          | Error _ -> Error (fail "invocation.handler_failed" "Tool execution failed.")
          | Ok () ->
            (match
               with_selected_managed_scope execution (fun borrowed ->
                 Ok
                   (service.run
                      execution
                      borrowed
                      ~effect_entry
                      ~stage_background
                      ~install_jobs:(install_jobs prepared)
                      continuation))
             with
             | Ok result -> result
             | Error _ ->
               Error (fail "invocation.handler_failed" "Tool execution failed.")))
    })
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
    ~complete:(fun outcome ~selected_jobs:_ -> Ok outcome)
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
    ~complete:(fun outcome ~selected_jobs:_ -> Ok outcome)
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

let run_owned_selected
      ~admit_managed
      ~install_jobs
      ~owner
      ~parent
      ~dispatch
      ~parent_live_now
      ~frozen
      ~enter_callback
      ~(managed : selected_managed_dispatch option)
      ~registry
      ~(reference : C.reference)
      ~invocation
      ~authorize
      ~prepare_output
      ~effect_entry
      ~stage_background
      ~seal
  =
  let execute ~invocation f =
    D.Selected.with_invocation
      owner
      ~parent
      ~invocation
      ~dispatch
      ~parent_live_now
      ~frozen
      (fun ~active ~dispatched ->
         enter_callback ~dispatched (fun () ->
           with_selected_scope
             ~ceiling:(D.Selected.ceiling owner)
             ~effect_entry
             ~issuer:owner
             ~parent_claim:active
             dispatched
             (fun () -> f ~dispatched)))
  in
  let managed_revision binding =
    match C.implementation binding, managed with
    | Managed _, Some service -> Managed.execution_revision service.definition binding
    | Native _, _ | Managed _, None -> Ok reference.implementation_revision
  in
  let managed_admit =
    Option.map managed ~f:(fun service binding selected dispatched ->
      selected_managed_target
        service
        ~reference
        ~selected
        ~dispatched
        ~admit_managed
        ~effect_entry:(fun () -> effect_entry binding)
        ~stage_background:(fun ~request ~checkpoint ->
          stage_background binding ~request ~checkpoint)
        ~install_jobs)
  in
  let check_halted () =
    match D.Selected.check_invocation owner ~parent invocation with
    | Ok () -> Ok ()
    | Error _ -> Error (fail "invocation.run_ended" "The developer run is unavailable.")
  in
  let complete outcome ~selected_jobs =
    match outcome with
    | (I.Fail _ | Cancelled _) when List.is_empty selected_jobs -> Ok (outcome, None)
    | I.Fail _ | Cancelled _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "failed native invocation cannot select new jobs")
    | Complete _ | Pending _ ->
      let open Result.Let_syntax in
      let%bind selected = seal outcome ~selected_jobs in
      (match outcome, selected with
       | Pending _, None ->
         Error
           (Agent_protocol.Error.invalid_request
              "pending native job was not selected by this physical scope")
       | (Complete _ | Pending _), (None | Some _) -> Ok (outcome, selected)
       | (Fail _ | Cancelled _), _ ->
         Error (Agent_protocol.Error.invalid_request "invalid selected outcome"))
  in
  Native_tool_policy.run_policy
    ~run_native:None
    ~on_progress:None
    ~managed_revision
    ~managed_admit
    ~execute
    ~registry:(fun () ->
      registry ()
      |> Result.map_error ~f:(fun _ ->
        C.
          { code = "capability.invalid_selection"
          ; message = "developer selection widened"
          }))
    ~reference
    ~invocation
    ~check_halted
    ~context:H.context
    ~kind_matches:(fun _ binding ->
      match (C.descriptor binding).type_ with
      | "function" | "custom" -> true
      | _ -> false)
    ~preparation:(fun _ -> None)
    ~with_selected:with_developer_selected_capabilities
    ~with_native_admission:(fun dispatched binding f ->
      with_owned_native_admission dispatched binding (fun () ->
        match effect_entry binding with
        | Ok () -> f ()
        | Error _ ->
          Error (fail "invocation.run_ended" "The developer run is unavailable.")))
    ~authorize
    ~prepare_output
    ~complete
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
