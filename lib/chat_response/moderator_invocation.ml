open Core
module I = Agent_protocol.Invocation
module Id = Agent_protocol.Id
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module S = Chatmd_shell_spec.Chatmd_script_spec
module D = Chatmd_shell_spec.Duration
module Schema = Chatmd_shell_spec.Tool_schema
module EC = Extension_compiler
module Tool_codec = Chatml_tool_codec

let error code message = Error (code ^ ": " ^ message)
let protocol result = Result.map_error result ~f:(fun e -> e.Agent_protocol.Error.message)
let record fields = L.VRecord (String.Map.of_alist_exn fields)
let int x = L.VInt x

let origin_value = function
  | I.Model -> L.VVariant ("Model", [])
  | Moderator -> L.VVariant ("Moderator", [])
  | Script -> L.VVariant ("Script", [])
  | Delegated_agent -> L.VVariant ("Delegated_agent", [])
  | External_adapter -> L.VVariant ("External_adapter", [])
;;

let max_nested_calls = 100
let bytes x = D.bytes_to_int64 x |> Int64.to_int_exn

let internal_event ?control payload =
  let open Result.Let_syntax in
  let%map _ = Tool_codec.Json.project ?control ~max_bytes:(1024 * 1024) payload in
  L.VVariant ("Internal_event", [ payload ])
;;

let normalize (eff : L.eff) =
  match eff.op, eff.args with
  | "Runtime.emit_json", [ payload ] ->
    Result.map (internal_event payload) ~f:(fun event ->
      L.{ op = "Runtime.emit"; args = [ event ] })
  | "Runtime.emit_json", _ ->
    error "invocation.invalid_event" "emit expects one JSON argument"
  | _ -> Ok eff
;;

let ordinary_effects effects =
  if
    List.exists effects ~f:(fun (eff : L.eff) -> String.equal eff.op "Invocation.resolve")
  then error "invocation.not_dispatched" "resolution is only valid during Tool_invoked"
  else Result.all (List.map effects ~f:normalize)
;;

let operations base =
  let find name =
    List.find_exn base ~f:(fun (op : R.op_def) -> String.equal op.name name)
  in
  let emit = find "Runtime.emit"
  and schedule = find "Schedule.after_ms" in
  base
  @ [ R.
        { name = "Invocation.resolve"
        ; kind = Local_transactional
        ; phase_check = require_phases [ "tool_invoked" ]
        ; perform = (fun _ _ -> Ok L.VUnit)
        }
    ; { emit with
        name = "Runtime.emit_json"
      ; perform =
          (fun session args ->
            match args with
            | [ payload ] ->
              Result.bind (internal_event payload) ~f:(fun event ->
                emit.perform session [ event ])
            | _ -> Error "Runtime.emit_json: expected one JSON argument")
      }
    ; { schedule with
        name = "Schedule.after_ms_json"
      ; perform =
          (fun session args ->
            match args with
            | [ delay; payload ] ->
              Result.bind (internal_event payload) ~f:(fun event ->
                schedule.perform session [ delay; event ])
            | _ -> Error "Schedule.after_ms_json: expected delay and JSON payload")
      }
    ]
;;

type t =
  { prepared : EC.t
  ; invocation : I.t
  ; limits : S.limits
  ; validate_work : I.work -> (unit, string) result
  ; context : L.value
  ; input : L.value
  }

let context t = t.context
let input t = t.input

let event t =
  L.VVariant
    ( "Tool_invoked"
    , [ record [ "version", int 1; "context", t.context; "input", t.input ] ] )
;;

let invocation t = t.invocation

let prepare_input ?control ~prepared ~(limits : S.limits) value =
  let open Result.Let_syntax in
  let%bind selected =
    Tool_codec.Context.Limits.of_script limits ~max_nested_calls ~max_invocation_depth:8
  in
  Tool_codec.Input.prepare
    ?control
    ~schema:(EC.input_schema prepared)
    ~limits:selected
    value
;;

let validate_limits (limits : S.limits) =
  Result.map
    (Tool_codec.Context.Limits.of_script limits ~max_nested_calls ~max_invocation_depth:8)
    ~f:ignore
;;

let context_value
      ~control
      ~capabilities
      ~(limits : S.limits)
      ~max_nested_calls:admitted_nested_calls
      ~max_invocation_depth:admitted_invocation_depth
      ~invocation_id
      ~provider_call_id
      ~session_label
      ~generation
      ~origin
      ~parent_invocation
      ~parent_event
      ~parent_job
      ~tool_name
      ~implementation_revision
      ~capability_fingerprint
      ~created_at
      ~deadline
  =
  let open Result.Let_syntax in
  let%bind selected =
    Tool_codec.Context.Limits.of_script
      limits
      ~max_nested_calls:admitted_nested_calls
      ~max_invocation_depth:admitted_invocation_depth
  in
  let references = Tool_capability.references capabilities in
  let available_tools =
    List.map references ~f:(fun (reference : Tool_capability.reference) ->
      Tool_codec.Context.
        { id = Id.Capability.to_string reference.id
        ; name = reference.name
        ; implementation_revision = reference.implementation_revision
        ; fingerprint = reference.fingerprint
        ; input_schema = reference.input_schema
        })
  in
  let%bind context =
    Tool_codec.Context.create
      ~invocation_id:(Id.Invocation.to_string invocation_id)
      ~provider_call_id
      ~session_label
      ~generation
      ~origin
      ~parent_invocation:(Option.map parent_invocation ~f:Id.Invocation.to_string)
      ~parent_event:(Option.map parent_event ~f:Id.Moderator_execution.to_string)
      ~parent_job:(Option.map parent_job ~f:Id.Job.to_string)
      ~tool_name
      ~implementation_revision
      ~capability_fingerprint
      ~created_at
      ~deadline
      ~limits:selected
      ~available_tools
      ~checkpoint:(fun () ->
        Option.iter control ~f:(fun value -> value.L.diagnostic_checkpoint ()))
  in
  Tool_codec.Context.project ?control context
;;

let create_for
      ~control
      ~managed
      ~implementation
      ~prepared
      ~invocation
      ~(limits : S.limits)
      ~validate_work
  =
  let open Result.Let_syntax in
  let%bind () = validate_limits limits in
  let%bind () = protocol (I.validate invocation) in
  let c = invocation.I.context in
  let%bind () =
    match invocation.status, implementation, (EC.declaration prepared).implementation with
    | I.Dispatching, `Moderator, Moderator _ | I.Dispatching, `Standalone, Standalone _ ->
      Ok ()
    | _ ->
      error "invocation.not_dispatched" "expected a dispatched tool of the requested kind"
  in
  let capabilities = EC.capabilities prepared in
  let%bind () =
    let matches =
      match managed with
      | None ->
        String.equal c.tool_name (EC.declaration prepared).name
        && String.equal c.implementation_revision (EC.fingerprint prepared)
        && String.equal
             c.capability_fingerprint
             (Tool_capability.fingerprint capabilities)
      | Some execution ->
        I.equal invocation (Managed_tool_registry.invocation execution)
        && phys_equal prepared (Managed_tool_registry.prepared execution)
    in
    if matches
    then Ok ()
    else error "invocation.stale_binding" "invocation does not match the prepared handler"
  in
  let%bind input = prepare_input ?control ~prepared ~limits c.input in
  let%bind context =
    context_value
      ~control
      ~capabilities
      ~limits
      ~max_nested_calls
      ~max_invocation_depth:8
      ~invocation_id:c.id
      ~provider_call_id:c.provider_call_id
      ~session_label:(Id.Session.to_string c.session_id)
      ~generation:c.generation
      ~origin:c.origin
      ~parent_invocation:c.parent_invocation
      ~parent_event:invocation.parent_event
      ~parent_job:c.parent_job
      ~tool_name:c.tool_name
      ~implementation_revision:c.implementation_revision
      ~capability_fingerprint:c.capability_fingerprint
      ~created_at:c.created_at
      ~deadline:c.deadline
  in
  Ok { prepared; invocation; limits; validate_work; context; input }
;;

let create = create_for ~managed:None ~implementation:`Moderator
let create_standalone = create_for ~managed:None ~implementation:`Standalone

let create_managed_standalone ~control ~execution ~limits ~validate_work =
  create_for
    ~control
    ~managed:(Some execution)
    ~implementation:`Standalone
    ~prepared:(Managed_tool_registry.prepared execution)
    ~invocation:(Managed_tool_registry.invocation execution)
    ~limits
    ~validate_work
;;

let create_managed ~control ~execution ~limits ~validate_work =
  create_for
    ~control
    ~managed:(Some execution)
    ~implementation:`Moderator
    ~prepared:(Managed_tool_registry.prepared execution)
    ~invocation:(Managed_tool_registry.invocation execution)
    ~limits
    ~validate_work
;;

let decode_projection ?control ~prepared ~(limits : S.limits) ~validate_work value =
  let open Result.Let_syntax in
  let%bind selected =
    Tool_codec.Context.Limits.of_script limits ~max_nested_calls ~max_invocation_depth:8
  in
  Tool_codec.Outcome.decode
    ?control
    ~output_schema:(EC.output_schema prepared)
    ~limits:selected
    ~validate_work
    value
  |> Result.map_error ~f:(function
      | Tool_codec.Outcome.Invalid message | Work_rejected message -> message)
;;

let decode_outcome ?control t value =
  decode_projection
    ?control
    ~prepared:t.prepared
    ~limits:t.limits
    ~validate_work:t.validate_work
    value
;;

module Owned = struct
  type t =
    { prepared : EC.t
    ; limits : S.limits
    ; validate_work : I.work -> (unit, string) result
    ; context : L.value
    ; input : L.value
    }

  let create_managed_standalone
        ~control
        ~execution
        ~limits
        ~max_nested_calls
        ~max_invocation_depth
        ~validate_work
    =
    let open Result.Let_syntax in
    let prepared = Managed_tool_registry.owned_prepared execution in
    let invocation = Managed_tool_registry.owned_invocation execution in
    let c = Agent_protocol.Host_invocation.context invocation in
    let%bind () = validate_limits limits in
    let%bind () =
      if max_nested_calls < 0 || max_invocation_depth <= 0
      then error "invocation.invalid_limits" "invalid managed call or depth limit"
      else Ok ()
    in
    let%bind () = protocol (Agent_protocol.Host_invocation.validate invocation) in
    let%bind owner =
      match c.owner with
      | Agent_protocol.Execution_owner.Developer owner -> Ok owner
      | Conversation _ ->
        error "invocation.invalid_owner" "expected a developer invocation"
    in
    let%bind () =
      match
        ( Agent_protocol.Host_invocation.status invocation
        , (EC.declaration prepared).implementation )
      with
      | I.Dispatching, Standalone _ -> Ok ()
      | I.Dispatching, Moderator _ | (Admitted | Resolved _ | Published _), _ ->
        error "invocation.not_dispatched" "expected a dispatched standalone tool"
    in
    let%bind input = prepare_input ?control ~prepared ~limits c.input in
    let capabilities = EC.capabilities prepared in
    let%bind context =
      context_value
        ~control
        ~capabilities
        ~limits
        ~max_nested_calls
        ~max_invocation_depth
        ~invocation_id:c.id
        ~provider_call_id:None
        ~session_label:
          ("developer-run:" ^ Agent_protocol.Id.Developer_run.to_string owner.run_id)
        ~generation:owner.epoch
        ~origin:c.origin
        ~parent_invocation:c.parent_invocation
        ~parent_event:None
        ~parent_job:c.parent_job
        ~tool_name:c.tool_name
        ~implementation_revision:c.implementation_revision
        ~capability_fingerprint:c.capability_fingerprint
        ~created_at:c.created_at
        ~deadline:c.deadline
    in
    Ok { prepared; limits; validate_work; context; input }
  ;;

  let context t = t.context
  let input t = t.input

  let decode_outcome ?control t value =
    decode_projection
      ?control
      ~prepared:t.prepared
      ~limits:t.limits
      ~validate_work:t.validate_work
      value
  ;;
end

let snapshot_state ?control ~(limits : S.limits) value =
  let open Result.Let_syntax in
  let%bind () =
    Tool_codec.Json.check_value
      ~code:"invocation.invalid_state"
      ~max_depth:limits.max_depth
      ~max_array_items:limits.max_array_items
      ~max_bytes:(bytes limits.max_value_bytes)
      value
  in
  Option.iter control ~f:(fun c -> c.L.before_json_export value);
  let%bind snapshot = V.Snapshot.of_value value in
  if
    String.length (Jsonaf.to_string (V.Snapshot.to_jsonaf snapshot))
    > bytes limits.max_value_bytes
  then error "invocation.state_limit" "encoded moderator state exceeds its byte limit"
  else Ok snapshot
;;

type failure =
  | Unhandled
  | Duplicate_resolution
  | Wrong_id
  | Invalid_output
  | Invalid_state
  | Suspended
  | Handler_failed
  | Session_ended

let run_impl ?control ?task_limits t ~runtime ~context ~prepare_commit ~failure_kind =
  let open Result.Let_syntax in
  let%bind () =
    match (EC.declaration t.prepared).implementation with
    | Moderator _ -> Ok ()
    | Standalone _ ->
      error "invocation.wrong_handler" "standalone tools have no moderator event"
  in
  let reject kind code message =
    failure_kind := kind;
    error code message
  in
  let%bind () =
    match context with
    | L.VRecord fields ->
      (match Map.find fields "phase", Map.find fields "session_id" with
       | Some (L.VString "tool_invoked"), Some (L.VString session_id)
         when String.equal
                session_id
                (Id.Session.to_string t.invocation.context.session_id) -> Ok ()
       | _ ->
         error
           "invocation.wrong_context"
           "dispatch requires the owning session and tool_invoked phase")
    | _ -> error "invocation.wrong_context" "expected moderator context"
  in
  let checked_state value =
    snapshot_state ?control ~limits:t.limits value
    |> Result.map_error ~f:(fun message ->
      failure_kind := Invalid_state;
      message)
  in
  let resolved = ref None in
  let prepare (transaction : R.transaction) =
    let resolutions, other =
      List.partition_tf transaction.local_effects ~f:(fun (eff : L.eff) ->
        String.equal eff.op "Invocation.resolve")
    in
    let%bind outcome =
      match resolutions with
      | [] ->
        reject Unhandled "invocation.unhandled" "moderator did not resolve the invocation"
      | [ { args = [ L.VString id; value ]; _ } ] ->
        if String.equal id (Id.Invocation.to_string t.invocation.context.id)
        then
          decode_outcome ?control t value
          |> Result.map_error ~f:(fun message ->
            failure_kind := Invalid_output;
            message)
        else
          reject
            Wrong_id
            "invocation.wrong_id"
            "resolution does not belong to the dispatched invocation"
      | [ _ ] ->
        reject Invalid_output "invocation.invalid_output" "malformed resolution arguments"
      | _ ->
        reject
          Duplicate_resolution
          "invocation.duplicate_resolution"
          "moderator resolved more than once"
    in
    let%bind next =
      protocol
        (I.resolve
           t.invocation
           ~session_id:t.invocation.context.session_id
           ~generation:t.invocation.context.generation
           outcome)
    in
    let%bind local_effects = ordinary_effects other in
    let%map install =
      prepare_commit ~resolved:next ~transaction:{ transaction with local_effects }
    in
    fun () ->
      install ();
      resolved := Some next
  in
  let%bind () =
    R.handle_event
      runtime
      ~context
      ~event:(event t)
      ?limits:task_limits
      ~validate_suspension:(fun () ->
        reject
          Suspended
          "invocation.suspended"
          "moderator tools cannot retain a legacy UI continuation")
      ~validate_state:(fun value -> Result.map (checked_state value) ~f:(fun _ -> ()))
      ~copy_state:(fun value -> Result.bind (checked_state value) ~f:V.Snapshot.to_value)
      ~prepare_transaction:prepare
  in
  match !resolved with
  | Some value -> Ok value
  | None -> reject Suspended "invocation.suspended" "tool handler did not complete"
;;

let run
      ?(on_failure = ignore)
      ?control
      ?execution
      ?execution_context
      t
      ~runtime
      ~context
      ~prepare_commit
  =
  let failure_kind = ref Handler_failed in
  let result =
    match execution, execution_context, control with
    | Some _, _, Some _ ->
      Error "invocation.execution_scope_conflict: use a runner or its active control"
    | None, Some _, _ ->
      Error
        "invocation.execution_scope_missing: inherited budgets require a controlled \
         runner"
    | None, None, None ->
      run_impl
        ~task_limits:R.{ fuel = t.limits.fuel; max_tasks = t.limits.max_tasks }
        t
        ~runtime
        ~context
        ~prepare_commit
        ~failure_kind
    | None, None, Some control ->
      (try run_impl ~control t ~runtime ~context ~prepare_commit ~failure_kind with
       | Chatml_execution.Budget_exhausted error ->
         Error (error.code ^ ": " ^ error.message))
    | Some runner, execution_context, None ->
      Chatml_execution.run_scoped ?context:execution_context runner (fun () ->
        run_impl
          ~control:(Chatml_execution.runner_control runner)
          t
          ~runtime
          ~context
          ~prepare_commit
          ~failure_kind)
      |> Result.map_error ~f:(fun error -> error.code ^ ": " ^ error.message)
      |> Result.join
  in
  (match result with
   | Error _ -> on_failure !failure_kind
   | Ok _ -> ());
  result
;;
