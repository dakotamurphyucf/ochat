open! Core
module P = Agent_protocol
module H = P.Host_invocation
module I = P.Invocation
module O = Developer_root_owner
module Jobs = Developer_script_job_service
module N = Agent_session.Native_tool_invocation
module C = Chat_response.Tool_capability
module M = Chat_response.Managed_tool_registry
module Child = Developer_child_selection
module Claimed = Agent_session.Claimed_child_runtime
module EC = Chat_response.Extension_compiler
module ABI = Chat_response.Moderator_invocation.Owned
module X = Chatml_execution
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module Duration = Chatmd_shell_spec.Duration

type t =
  { root : O.run
  ; definition : M.t
  ; env : Eio_unix.Stdenv.base
  ; source : source
  }

and source =
  | Root
  | Child of Child.t
  | Claimed of Claimed.t

exception Fatal_effect of P.Error.t

let ( let* ) result f = Result.bind result ~f
let invalid message = P.Error.invalid_request message
let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let utf8_prefix text max_bytes =
  let prefix = String.prefix text (Int.min (String.length text) max_bytes) in
  let rec shorten length =
    if length <= 0
    then "Tool execution failed."
    else (
      let candidate = String.prefix prefix length in
      if Stdlib.String.is_valid_utf_8 candidate then candidate else shorten (length - 1))
  in
  shorten (String.length prefix)
;;

let error_outcome error =
  fail "invocation.handler_failed" (utf8_prefix error.P.Error.message 1024)
;;

let create ~root ~jobs ~definition ~env =
  if not (Jobs.issued_by_root jobs root)
  then Error (invalid "managed Job service belongs to another developer Root")
  else (
    let selected = O.native_selected root in
    let available = M.capabilities definition in
    let* () =
      List.fold_result (C.references selected) ~init:() ~f:(fun () reference ->
        let* binding =
          C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
          |> Result.map_error ~f:(fun error -> invalid error.message)
        in
        match C.implementation binding with
        | Native _ -> Ok ()
        | Managed _ ->
          C.resolve available ~id:reference.id ~fingerprint:reference.fingerprint
          |> Result.map ~f:ignore
          |> Result.map_error ~f:(fun error -> invalid error.message))
    in
    Ok { root; definition; env; source = Root })
;;

let create_child ~root ~jobs ~selection ~env =
  let* admission = Child.refresh selection in
  let* () = Child.Admission.check_now admission in
  let definition = Child.managed_definition selection in
  let available = M.capabilities definition in
  let selected = Child.Admission.capabilities admission in
  let* () =
    if Jobs.issued_by_root jobs root && Child.issued_by_root selection root
    then Ok ()
    else Error (invalid "child managed Job service belongs to another Root")
  in
  let* () =
    List.fold_result (C.references selected) ~init:() ~f:(fun () reference ->
      let* binding =
        C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map_error ~f:(fun error -> invalid error.message)
      in
      match C.implementation binding with
      | Native _ -> Ok ()
      | Managed _ ->
        C.resolve available ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map ~f:ignore
        |> Result.map_error ~f:(fun error -> invalid error.message))
  in
  Ok { root; definition; env; source = Child selection }
;;

let create_claimed ~root ~jobs ~active ~issuer ~env =
  let* parent = O.job_parent root active in
  let* () =
    if Claimed.same_parent issuer parent
    then Ok ()
    else Error (invalid "claimed managed issuer belongs to another Worker Claim")
  in
  let* () = Claimed.check issuer in
  let definition = Claimed.managed_definition issuer in
  let available = M.capabilities definition in
  let selected = Claimed.selected issuer in
  let* () =
    if Jobs.issued_by_root jobs root
    then Ok ()
    else Error (invalid "claimed managed Job service belongs to another Root")
  in
  let* () =
    List.fold_result (C.references selected) ~init:() ~f:(fun () reference ->
      let* binding =
        C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map_error ~f:(fun error -> invalid error.message)
      in
      match C.implementation binding with
      | Native _ -> Ok ()
      | Managed _ ->
        C.resolve available ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map ~f:ignore
        |> Result.map_error ~f:(fun error -> invalid error.message))
  in
  let* () = Claimed.check_now issuer in
  Ok { root; definition; env; source = Claimed issuer }
;;

let issued_by_root t root = phys_equal t.root root
let definition t = t.definition

let effective_limits t prepared invocation =
  let declaration = EC.execution_limits prepared in
  let host = (O.job_policy t.root).execution in
  let remaining =
    Option.value_map
      (H.context invocation).deadline
      ~default:host.wall_seconds
      ~f:(fun deadline ->
        Time_ns.diff
          (P.Timestamp.to_time_ns deadline)
          (P.Timestamp.to_time_ns (O.native_now t.root))
        |> Time_ns.Span.to_sec)
  in
  { host with
    wall_seconds =
      Float.min
        remaining
        (Float.min host.wall_seconds (Duration.to_seconds declaration.wall_time))
  ; fuel = Int.min host.fuel declaration.fuel
  ; max_tasks = Int.min host.max_tasks declaration.max_tasks
  ; max_value_bytes =
      Int.min
        host.max_value_bytes
        (Duration.bytes_to_int64 declaration.max_value_bytes |> Int64.to_int_exn)
  ; max_array_items = Int.min host.max_array_items declaration.max_array_items
  ; max_depth = Int.min host.max_depth declaration.max_depth
  }
;;

let effective_abi_limits t prepared (limits : X.limits) =
  let declaration = EC.execution_limits prepared in
  let output_bytes =
    Int.min
      (Duration.bytes_to_int64 declaration.max_output_bytes |> Int64.to_int_exn)
      (O.job_policy t.root).max_output_bytes
  in
  let output_bytes = Int.min output_bytes limits.max_value_bytes in
  let bytes value = Duration.parse_bytes (Int.to_string value ^ "B") in
  let* wall_time =
    Duration.parse (Printf.sprintf "%.17gs" limits.wall_seconds)
    |> Result.map_error ~f:invalid
  in
  let* max_value_bytes = bytes limits.max_value_bytes |> Result.map_error ~f:invalid in
  let* max_output_bytes = bytes output_bytes |> Result.map_error ~f:invalid in
  Ok
    Chatmd_shell_spec.Chatmd_script_spec.
      { wall_time
      ; fuel = limits.fuel
      ; max_tasks = limits.max_tasks
      ; max_value_bytes
      ; max_output_bytes
      ; max_array_items = limits.max_array_items
      ; max_depth = limits.max_depth
      }
;;

type admission =
  | Root_admission
  | Child_admission of Child.Admission.t
  | Claimed_admission of Claimed.t

let refresh_source t =
  match t.source with
  | Root -> Ok Root_admission
  | Child child ->
    Result.map (Child.refresh child) ~f:(fun admission -> Child_admission admission)
  | Claimed issuer ->
    Result.map (Claimed.check issuer) ~f:(fun () -> Claimed_admission issuer)
;;

let selected_now t admission =
  match admission with
  | Root_admission -> Ok (O.native_selected t.root)
  | Child_admission admission ->
    let* () = Child.Admission.check_now admission in
    Ok (Child.Admission.source_capabilities admission)
  | Claimed_admission issuer ->
    let* () = Claimed.check_now issuer in
    Ok (Claimed.selected issuer)
;;

let check_run_now t ~terminal ~scope ~execution ~borrowed ~admission =
  let admitted = M.owned_invocation execution in
  let current = N.borrow_selected () in
  let reference = C.reference (M.owned_binding execution) in
  let* () = O.Native_terminal.check_completion_now terminal in
  let* selected = selected_now t admission in
  if
    (not (Jobs.Scope.issued_by_terminal scope terminal))
    || (not (O.Native_terminal.issued_by_root terminal ~root:t.root))
    || (not
          (match current with
           | Ok current -> phys_equal current borrowed
           | Error _ -> false))
    || (not (phys_equal (N.selected_borrowed_invocation borrowed) admitted))
    || (not
          (H.equal_context
             (H.context (O.Native_terminal.invocation terminal))
             (H.context admitted)))
    || (not (P.Invocation.equal_status (H.status admitted) Dispatching))
    || not
         (Result.is_ok
            (C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint))
  then Error (invalid "managed dispatch belongs to another or expired native scope")
  else Ok ()
;;

let child_outcome ~control ~max_depth ~max_bytes outcome =
  let* () = P.Invocation.validate_outcome outcome in
  let* () =
    P.Json_codec.validate_limits
      ~max_depth
      ~max_bytes
      (P.Invocation.outcome_to_json outcome)
  in
  match outcome with
  | Complete value -> Ok (L.VVariant ("Ok", [ V.import_json ?control value ]))
  | Pending (Job _, acknowledgement) ->
    Ok (L.VVariant ("Ok", [ V.import_json ?control acknowledgement ]))
  | Fail error -> Ok (L.VVariant ("Error", [ L.VString error.message ]))
  | Cancelled reason -> Ok (L.VVariant ("Error", [ L.VString reason ]))
  | Pending (Subscription _, _) ->
    Error (invalid "managed nested Tool.call returned unsupported work")
;;

let call_tool t ~control ~prepared ~(limits : X.limits) ~parent ~call_child ~name ~args =
  let selected = EC.capabilities prepared in
  let* binding =
    C.find selected ~name |> Result.map_error ~f:(fun error -> invalid error.message)
  in
  let reference = C.reference binding in
  let* revision =
    match C.implementation binding with
    | Native _ -> Ok reference.implementation_revision
    | Managed _ ->
      M.execution_revision t.definition binding
      |> Result.map_error ~f:(fun error -> invalid error.message)
  in
  let parent_context = H.context parent in
  let context : H.context =
    { id = P.Id.Invocation.create ()
    ; owner = parent_context.owner
    ; origin = Script
    ; parent_invocation = Some parent_context.id
    ; parent_job = parent_context.parent_job
    ; tool_name = reference.name
    ; implementation_revision = revision
    ; capability_fingerprint = C.fingerprint selected
    ; input = args
    ; created_at = O.native_now t.root
    ; deadline = parent_context.deadline
    }
  in
  let* invocation = H.create_developer context in
  let* resolved = call_child ~reference ~invocation in
  let* outcome =
    match H.status resolved with
    | Resolved outcome -> Ok outcome
    | Admitted | Dispatching | Published _ ->
      Error (invalid "selected child did not resolve to a native outcome")
  in
  child_outcome
    ~control
    ~max_depth:limits.max_depth
    ~max_bytes:limits.max_value_bytes
    outcome
;;

let run_handler
      t
      terminal
      scope
      execution
      borrowed
      ~effect_entry
      ~install_jobs
      ~call_child
      continuation
  =
  let prepared = M.owned_prepared execution in
  let parent = M.owned_invocation execution in
  let* admission = refresh_source t |> Result.map_error ~f:error_outcome in
  let* () =
    check_run_now t ~terminal ~scope ~execution ~borrowed ~admission
    |> Result.map_error ~f:error_outcome
  in
  let* context =
    N.selected_borrowed_execution_context borrowed |> Result.map_error ~f:error_outcome
  in
  let limits = effective_limits t prepared parent in
  if Float.(limits.wall_seconds <= 0.)
  then Error (fail "invocation.deadline" "Managed tool deadline has expired.")
  else
    let* abi_limits =
      effective_abi_limits t prepared limits |> Result.map_error ~f:error_outcome
    in
    let evaluate control =
      let* () = effect_entry () |> Result.map_error ~f:error_outcome in
      let* admission = refresh_source t |> Result.map_error ~f:error_outcome in
      let* () =
        check_run_now t ~terminal ~scope ~execution ~borrowed ~admission
        |> Result.map_error ~f:error_outcome
      in
      let validate_work work =
        Jobs.Scope.validate_work scope work
        |> Result.map_error ~f:(fun error -> error.P.Error.message)
      in
      let* abi =
        ABI.create_managed_standalone
          ~control
          ~execution
          ~limits:abi_limits
          ~max_nested_calls:limits.max_calls
          ~max_invocation_depth:limits.max_invocation_depth
          ~validate_work
        |> Result.map_error ~f:(fun message -> fail "invocation.invalid_input" message)
      in
      let* entrypoint =
        match (EC.declaration prepared).implementation with
        | Standalone { entrypoint; _ } -> Ok entrypoint
        | Moderator _ ->
          Error (fail "invocation.managed_dispatch_required" "Not a standalone tool.")
      in
      let on_tool_call _ ~name ~args =
        let* args = V.export_json ?control args in
        match call_tool t ~control ~prepared ~limits ~parent ~call_child ~name ~args with
        | Ok value -> Ok value
        | Error error -> raise (Fatal_effect error)
      in
      let handlers = { R.default_handlers with on_tool_call } in
      let surface =
        match EC.compiler_target prepared with
        | Chatml_compilation.Developer_tool_v1 ->
          Chatml.Chatml_extension_surface.developer_tool_v1
        | Tool_v1 -> Chatml.Chatml_extension_surface.tool_v1
        | One_off_v1 | Moderator_v1 | Delegated_moderator_v1 ->
          failwith "selected managed tool has a non-tool compiler target"
      in
      let config : R.runtime_config =
        { surface; operations = R.default_operations ~handlers () }
      in
      let (install : N.managed_script_install) = install_jobs config in
      let local_effects = ref [] in
      let prepare_result ~value:_ ~local_effects:effects =
        local_effects := effects;
        Ok Fn.id
      in
      let* value =
        Chat_response.Script_program.run_in_scope
          (EC.program prepared)
          ?scoped_async_operation:install.scoped_async_operation
          ~prepare_result
          ~control
          ~config:install.config
          ~entrypoint
          ~arguments:[ ABI.context abi; ABI.input abi ]
          ()
        |> Result.map_error ~f:(fun error ->
          fail error.Chatml_execution.code error.message)
      in
      let* () = effect_entry () |> Result.map_error ~f:error_outcome in
      let* admission = refresh_source t |> Result.map_error ~f:error_outcome in
      let* () =
        check_run_now t ~terminal ~scope ~execution ~borrowed ~admission
        |> Result.map_error ~f:error_outcome
      in
      let* outcome =
        ABI.decode_outcome ?control abi value
        |> Result.map_error ~f:(fun message -> fail "invocation.invalid_output" message)
      in
      let* selected_jobs, ordinary =
        Jobs.Scope.split_starts scope !local_effects |> Result.map_error ~f:error_outcome
      in
      let* () =
        match ordinary with
        | [] -> Ok ()
        | _ -> Error (fail "invocation.invalid_effect" "Unsupported local effects.")
      in
      continuation ~validate_work ~outcome ~selected_jobs
    in
    match
      try X.with_control ~context ~policy:(Bounded limits) ~env:t.env evaluate with
      | Fatal_effect error -> Ok (Error (error_outcome error))
    with
    | Error error -> Error (fail error.code error.message)
    | Ok result -> result
;;

let bind t ~terminal ~scope ~call_child =
  if
    (not (O.Native_terminal.issued_by_root terminal ~root:t.root))
    || not (Jobs.Scope.issued_by_terminal scope terminal)
  then Error (invalid "managed tool scope belongs to another native terminal")
  else
    Ok
      N.
        { definition = t.definition
        ; current =
            (fun () ->
              let* admission = refresh_source t in
              selected_now t admission)
        ; run =
            (fun execution
              borrowed
              ~effect_entry
              ~stage_background:_
              ~install_jobs
              continuation ->
              run_handler
                t
                terminal
                scope
                execution
                borrowed
                ~effect_entry
                ~install_jobs
                ~call_child
                continuation)
        }
;;
