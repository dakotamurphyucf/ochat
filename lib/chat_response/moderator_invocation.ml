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

let error code message = Error (code ^ ": " ^ message)
let protocol result = Result.map_error result ~f:(fun e -> e.Agent_protocol.Error.message)
let record fields = L.VRecord (String.Map.of_alist_exn fields)

let option f = function
  | None -> L.VVariant ("None", [])
  | Some x -> L.VVariant ("Some", [ f x ])
;;

let string x = L.VString x
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

(* Bound traversal before recursive conversion/serialization, including cyclic
   arrays created by script code. Runtime-only values cannot become tool output. *)
let check_value
      ?(code = "invocation.invalid_output")
      ?(max_depth = 128)
      ?(max_array_items = 100_000)
      ~max_bytes
      value
  =
  let remaining = ref max_bytes
  and nodes = ref 100_000 in
  let rec walk depth value =
    decr nodes;
    if depth > max_depth || !nodes < 0 then failwith "value depth/node limit";
    let consume n =
      if n > !remaining then failwith "value byte limit";
      remaining := !remaining - n
    in
    consume 1;
    let child = walk (depth + 1) in
    match value with
    | L.VUnit | VInt _ | VBool _ -> ()
    | VFloat f -> if not (Float.is_finite f) then failwith "non-finite number"
    | VString s -> consume (String.length s)
    | VArray values ->
      if Array.length values > max_array_items then failwith "array item limit";
      Array.iter values ~f:child
    | VRecord fields ->
      Map.iteri fields ~f:(fun ~key ~data ->
        consume (String.length key);
        child data)
    | VVariant (tag, args) ->
      consume (String.length tag);
      List.iter args ~f:child
    | _ -> failwith "runtime-only value"
  in
  try
    walk 0 value;
    Ok ()
  with
  | Failure message -> error code message
;;

let json ~max_bytes value =
  let open Result.Let_syntax in
  let%bind () = check_value ~max_bytes value in
  let%bind json = V.value_to_jsonaf_result value in
  (* The shared schema validator also rejects duplicate object keys and invalid
     JSON numbers before any payload reaches the protocol. *)
  let%bind any =
    Schema.compile `True |> Result.map_error ~f:(fun _ -> "invalid JSON validator")
  in
  let%bind () =
    Schema.validate any json
    |> Result.map_error ~f:(fun _ -> "invocation.invalid_output: invalid JSON value")
  in
  if String.length (Jsonaf.to_string json) > max_bytes
  then error "invocation.output_limit" "encoded result exceeds its byte limit"
  else Ok json
;;

let internal_event payload =
  let open Result.Let_syntax in
  let%map _ = json ~max_bytes:(1024 * 1024) payload in
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

let ms timestamp =
  Agent_protocol.Timestamp.to_time_ns timestamp
  |> Time_ns.to_int63_ns_since_epoch
  |> fun ns -> Int63.(to_int_exn (ns / of_int 1_000_000))
;;

let prepare_input ~prepared ~(limits : S.limits) value =
  let open Result.Let_syntax in
  let%bind () = protocol (I.validate_outcome (Complete value)) in
  let%bind () =
    Schema.validate (EC.input_schema prepared) value
    |> Result.map_error ~f:(fun _ -> "invocation.invalid_input: input schema mismatch")
  in
  let input = V.jsonaf_to_value value in
  let%map () =
    check_value
      ~code:"invocation.invalid_input"
      ~max_depth:limits.max_depth
      ~max_array_items:limits.max_array_items
      ~max_bytes:(bytes limits.max_value_bytes)
      input
  in
  input
;;

let create_for ~implementation ~prepared ~invocation ~(limits : S.limits) ~validate_work =
  let open Result.Let_syntax in
  let%bind () =
    if
      limits.fuel <= 0
      || limits.fuel > 10_000_000
      || limits.max_tasks <= 0
      || limits.max_tasks > 100_000
      || limits.max_depth <= 0
      || limits.max_depth > 128
      || limits.max_array_items <= 0
      || limits.max_array_items > 100_000
      || Int64.(
           D.bytes_to_int64 limits.max_value_bytes <= 0L
           || D.bytes_to_int64 limits.max_value_bytes > 8_388_608L)
      || Int64.(
           D.bytes_to_int64 limits.max_output_bytes <= 0L
           || D.bytes_to_int64 limits.max_output_bytes > 1_048_576L)
    then error "invocation.invalid_limits" "unsupported invocation limits"
    else Ok ()
  in
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
    if
      String.equal c.tool_name (EC.declaration prepared).name
      && String.equal c.implementation_revision (EC.fingerprint prepared)
      && String.equal c.capability_fingerprint (Tool_capability.fingerprint capabilities)
    then Ok ()
    else error "invocation.stale_binding" "invocation does not match the prepared handler"
  in
  let%bind input = prepare_input ~prepared ~limits c.input in
  let capability (r : Tool_capability.reference) =
    record
      [ "id", string (Id.Capability.to_string r.id)
      ; "name", string r.name
      ; "implementation_revision", string r.implementation_revision
      ; "fingerprint", string r.fingerprint
      ; "input_schema", V.jsonaf_to_value r.input_schema
      ]
  in
  let limits_value =
    record
      [ "fuel", int limits.fuel
      ; "max_tasks", int limits.max_tasks
      ; "max_value_bytes", int (bytes limits.max_value_bytes)
      ; "max_output_bytes", int (bytes limits.max_output_bytes)
      ; "max_array_items", int limits.max_array_items
      ; "max_depth", int limits.max_depth
      ; "max_nested_calls", int max_nested_calls
      ; "max_invocation_depth", int 8
      ]
  in
  let context =
    record
      [ "version", int 1
      ; "invocation_id", string (Id.Invocation.to_string c.id)
      ; "provider_call_id", option string c.provider_call_id
      ; "session_id", string (Id.Session.to_string c.session_id)
      ; "generation", int c.generation
      ; "origin", origin_value c.origin
      ; ( "parent_invocation"
        , option (fun id -> string (Id.Invocation.to_string id)) c.parent_invocation )
      ; ( "parent_event"
        , option
            (fun id -> string (Id.Moderator_execution.to_string id))
            invocation.parent_event )
      ; "parent_job", option (fun id -> string (Id.Job.to_string id)) c.parent_job
      ; "tool_name", string c.tool_name
      ; "implementation_revision", string c.implementation_revision
      ; "capability_fingerprint", string c.capability_fingerprint
      ; "created_at_ms", int (ms c.created_at)
      ; "deadline_ms", option (fun t -> int (ms t)) c.deadline
      ; "limits", limits_value
      ; ( "available_tools"
        , L.VArray
            (Tool_capability.references capabilities
             |> List.map ~f:capability
             |> Array.of_list) )
      ]
  in
  Ok { prepared; invocation; limits; validate_work; context; input }
;;

let create = create_for ~implementation:`Moderator
let create_standalone = create_for ~implementation:`Standalone

let decode t value =
  let open Result.Let_syntax in
  let max_bytes = bytes t.limits.max_output_bytes in
  let%bind () =
    check_value
      ~max_depth:t.limits.max_depth
      ~max_array_items:t.limits.max_array_items
      ~max_bytes
      value
  in
  let%bind outcome =
    match value with
    | L.VVariant ("Complete", [ value ]) ->
      Result.map (json ~max_bytes value) ~f:(fun value -> I.Complete value)
    | VVariant ("Pending", [ VVariant (tag, [ VString id ]); value ]) ->
      let%bind work =
        match tag with
        | "Job" -> protocol (Id.Job.of_string id) |> Result.map ~f:(fun id -> I.Job id)
        | "Subscription" ->
          protocol (Id.Subscription.of_string id)
          |> Result.map ~f:(fun id -> I.Subscription id)
        | _ -> error "invocation.invalid_work" "expected Job or Subscription"
      in
      let%map value = json ~max_bytes value in
      I.Pending (work, value)
    | VVariant ("Fail", [ VRecord fields ]) when Map.length fields = 4 ->
      let%bind code =
        V.expect_record_field "tool_error" fields "code"
        |> Result.bind ~f:(V.expect_string "code")
      in
      let%bind message =
        V.expect_record_field "tool_error" fields "message"
        |> Result.bind ~f:(V.expect_string "message")
      in
      let%bind retryable =
        match Map.find fields "retryable" with
        | Some (VBool b) -> Ok b
        | _ -> Error "invalid retryable flag"
      in
      let%map details =
        V.expect_record_field "tool_error" fields "details"
        |> Result.bind ~f:(json ~max_bytes)
      in
      I.Fail { code; message; retryable; details }
    | _ -> error "invocation.invalid_output" "expected Complete, Pending or Fail"
  in
  let%bind () = protocol (I.validate_outcome outcome) in
  let%bind () =
    match outcome with
    | I.Complete value | Pending (_, value) ->
      Schema.validate (EC.output_schema t.prepared) value
      |> Result.map_error ~f:(fun _ ->
        "invocation.invalid_output: output schema mismatch")
    | Fail _ -> Ok ()
    | Cancelled _ -> assert false
  in
  let%bind () =
    match outcome with
    | Pending (work, _) -> t.validate_work work
    | _ -> Ok ()
  in
  if String.length (Jsonaf.to_string (I.outcome_to_json outcome)) > max_bytes
  then error "invocation.output_limit" "outcome envelope exceeds its byte limit"
  else Ok outcome
;;

let decode_outcome = decode

let snapshot_state ~(limits : S.limits) value =
  let open Result.Let_syntax in
  let%bind () =
    check_value
      ~code:"invocation.invalid_state"
      ~max_depth:limits.max_depth
      ~max_array_items:limits.max_array_items
      ~max_bytes:(bytes limits.max_value_bytes)
      value
  in
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

let run_impl ?task_limits t ~runtime ~context ~prepare_commit ~failure_kind =
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
    snapshot_state ~limits:t.limits value
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
          decode t value
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

let run ?(on_failure = ignore) ?execution t ~runtime ~context ~prepare_commit =
  let failure_kind = ref Handler_failed in
  let result =
    match execution with
    | None ->
      run_impl
        ~task_limits:R.{ fuel = t.limits.fuel; max_tasks = t.limits.max_tasks }
        t
        ~runtime
        ~context
        ~prepare_commit
        ~failure_kind
    | Some runner ->
      Chatml_execution.run_scoped runner (fun () ->
        run_impl t ~runtime ~context ~prepare_commit ~failure_kind)
      |> Result.map_error ~f:(fun error -> error.code ^ ": " ^ error.message)
      |> Result.join
  in
  (match result with
   | Error _ -> on_failure !failure_kind
   | Ok _ -> ());
  result
;;
