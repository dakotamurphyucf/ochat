open Core
module I = Agent_protocol.Invocation
module Id = Agent_protocol.Id
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module Schema = Chatmd_shell_spec.Tool_schema
module D = Chatmd_shell_spec.Duration

let error code message = Error (code ^ ": " ^ message)
let protocol result = Result.map_error result ~f:(fun e -> e.Agent_protocol.Error.message)
let record fields = L.VRecord (String.Map.of_alist_exn fields)
let string value = L.VString value
let int value = L.VInt value

let option f = function
  | None -> L.VVariant ("None", [])
  | Some value -> L.VVariant ("Some", [ f value ])
;;

let origin_value = function
  | I.Model -> L.VVariant ("Model", [])
  | Moderator -> L.VVariant ("Moderator", [])
  | Script -> L.VVariant ("Script", [])
  | Delegated_agent -> L.VVariant ("Delegated_agent", [])
  | External_adapter -> L.VVariant ("External_adapter", [])
;;

module Json = struct
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
      | VFloat value -> if not (Float.is_finite value) then failwith "non-finite number"
      | VString value -> consume (String.length value)
      | VArray values ->
        if Array.length values > max_array_items then failwith "array item limit";
        Array.iter values ~f:child
      | VTuple values ->
        if Chatml.Chatml_tuple.length values > max_array_items
        then failwith "tuple item limit";
        Chatml.Chatml_tuple.iter values ~f:child
      | VRecord fields ->
        Map.iteri fields ~f:(fun ~key ~data ->
          consume (String.length key);
          child data)
      | VVariant (tag, arguments) ->
        consume (String.length tag);
        List.iter arguments ~f:child
      | _ -> failwith "runtime-only value"
    in
    try
      walk 0 value;
      Ok ()
    with
    | Failure message -> error code message
  ;;

  let project ?control ~max_bytes value =
    let open Result.Let_syntax in
    let%bind () = check_value ~max_bytes value in
    let%bind json = V.export_json ?control value in
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
end

module Context = struct
  type capability =
    { id : string
    ; name : string
    ; implementation_revision : string
    ; fingerprint : string
    ; input_schema : Jsonaf.t
    }

  module Limits = struct
    type t =
      { fuel : int
      ; max_tasks : int
      ; max_value_bytes : int
      ; max_output_bytes : int
      ; max_array_items : int
      ; max_depth : int
      ; max_nested_calls : int
      ; max_invocation_depth : int
      }

    let validate t =
      if
        t.fuel <= 0
        || t.fuel > 10_000_000
        || t.max_tasks <= 0
        || t.max_tasks > 100_000
        || t.max_value_bytes <= 0
        || t.max_value_bytes > 8_388_608
        || t.max_output_bytes <= 0
        || t.max_output_bytes > 1_048_576
        || t.max_array_items <= 0
        || t.max_array_items > 100_000
        || t.max_depth <= 0
        || t.max_depth > 128
        || t.max_nested_calls < 0
        || t.max_nested_calls > 100
        || t.max_invocation_depth <= 0
        || t.max_invocation_depth > 8
      then error "invocation.invalid_limits" "unsupported invocation limits"
      else Ok t
    ;;

    let of_script
          (limits : Chatmd_shell_spec.Chatmd_script_spec.limits)
          ~max_nested_calls
          ~max_invocation_depth
      =
      let value_bytes = D.bytes_to_int64 limits.max_value_bytes
      and output_bytes = D.bytes_to_int64 limits.max_output_bytes in
      if
        Int64.compare value_bytes 8_388_608L > 0
        || Int64.compare output_bytes 1_048_576L > 0
      then error "invocation.invalid_limits" "unsupported invocation limits"
      else
        validate
          { fuel = limits.fuel
          ; max_tasks = limits.max_tasks
          ; max_value_bytes = Int64.to_int_exn value_bytes
          ; max_output_bytes = Int64.to_int_exn output_bytes
          ; max_array_items = limits.max_array_items
          ; max_depth = limits.max_depth
          ; max_nested_calls
          ; max_invocation_depth
          }
    ;;

    let of_execution (limits : Chatml_execution.limits) ~max_output_bytes =
      if not (Chatml_execution.valid_limits limits)
      then error "invocation.invalid_limits" "invalid execution limits"
      else
        validate
          { fuel = limits.fuel
          ; max_tasks = limits.max_tasks
          ; max_value_bytes = limits.max_value_bytes
          ; max_output_bytes
          ; max_array_items = limits.max_array_items
          ; max_depth = limits.max_depth
          ; max_nested_calls = limits.max_calls
          ; max_invocation_depth = limits.max_invocation_depth
          }
    ;;
  end

  type t =
    { invocation_id : string
    ; provider_call_id : string option
    ; session_label : string
    ; generation : int
    ; origin : I.origin
    ; parent_invocation : string option
    ; parent_event : string option
    ; parent_job : string option
    ; tool_name : string
    ; implementation_revision : string
    ; capability_fingerprint : string
    ; created_at : Agent_protocol.Timestamp.t
    ; deadline : Agent_protocol.Timestamp.t option
    ; limits : Limits.t
    ; available_tools : capability list
    }

  let capability_fingerprint fingerprints =
    fingerprints
    |> [%sexp_of: string list]
    |> Sexp.to_string
    |> Chatmd_shell_spec.Source_ref.digest
  ;;

  let bounded ~nonempty ~max value =
    ((not nonempty) || not (String.is_empty value)) && String.length value <= max
  ;;

  let create
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
        ~(limits : Limits.t)
        ~available_tools
        ~checkpoint
    =
    let valid_optional = Option.for_all ~f:(bounded ~nonempty:true ~max:256) in
    let invalid_identity =
      generation < 0
      || (not (bounded ~nonempty:true ~max:256 invocation_id))
      || (not (bounded ~nonempty:true ~max:256 session_label))
      || (not (bounded ~nonempty:true ~max:256 tool_name))
      || (not (bounded ~nonempty:true ~max:256 implementation_revision))
      || (not (bounded ~nonempty:true ~max:256 capability_fingerprint))
      || (not (valid_optional provider_call_id))
      || (not (valid_optional parent_invocation))
      || (not (valid_optional parent_event))
      || (not (valid_optional parent_job))
      || Option.exists deadline ~f:(fun value ->
        Agent_protocol.Timestamp.compare value created_at < 0)
      || List.length available_tools > 100_000
    in
    let valid_tools =
      match
        Document_schema.Limits.create
          ~max_bytes:(Int.min limits.max_value_bytes 1_048_576)
          ~max_depth:128
          ~max_fields:100_000
          ~max_nodes:100_000
      with
      | Error _ -> false
      | Ok json_limits ->
        let total = ref 0 in
        List.for_all available_tools ~f:(fun reference ->
          checkpoint ();
          let names =
            [ reference.id
            ; reference.name
            ; reference.implementation_revision
            ; reference.fingerprint
            ]
          in
          if not (List.for_all names ~f:(bounded ~nonempty:true ~max:256))
          then false
          else (
            match
              Document_schema.Json.validate_and_measure_with_checkpoint
                ~limits:json_limits
                ~checkpoint
                reference.input_schema
            with
            | Error _ -> false
            | Ok bytes ->
              let identity_bytes =
                List.fold names ~init:128 ~f:(fun size name -> size + String.length name)
              in
              if
                bytes > limits.max_value_bytes - !total
                || identity_bytes > limits.max_value_bytes - !total - bytes
              then false
              else (
                total := !total + bytes + identity_bytes;
                true)))
    in
    if invalid_identity || not valid_tools
    then error "invocation.invalid_context" "invalid tool context"
    else
      Ok
        { invocation_id
        ; provider_call_id
        ; session_label
        ; generation
        ; origin
        ; parent_invocation
        ; parent_event
        ; parent_job
        ; tool_name
        ; implementation_revision
        ; capability_fingerprint
        ; created_at
        ; deadline
        ; limits
        ; available_tools
        }
  ;;

  let developer_fixture
        ~run
        ~execution_id
        ~generation
        ~tool_name
        ~implementation_revision
        ~created_at
        ~deadline
        ~limits
    =
    create
      ~invocation_id:("developer-command:" ^ execution_id)
      ~provider_call_id:None
      ~session_label:("developer-run:" ^ Id.Developer_run.to_string run)
      ~generation
      ~origin:I.Script
      ~parent_invocation:None
      ~parent_event:None
      ~parent_job:None
      ~tool_name
      ~implementation_revision
      ~capability_fingerprint:(capability_fingerprint [])
      ~created_at
      ~deadline
      ~limits
      ~available_tools:[]
      ~checkpoint:(fun () -> ())
  ;;

  let ms timestamp =
    Agent_protocol.Timestamp.to_time_ns timestamp
    |> Time_ns.to_int63_ns_since_epoch
    |> fun ns -> Int63.(to_int_exn (ns / of_int 1_000_000))
  ;;

  let project ?control t =
    let open Result.Let_syntax in
    let record fields =
      Option.iter control ~f:(fun value -> value.L.allocate (64 * List.length fields));
      record fields
    in
    let capability reference =
      record
        [ "id", string reference.id
        ; "name", string reference.name
        ; "implementation_revision", string reference.implementation_revision
        ; "fingerprint", string reference.fingerprint
        ; "input_schema", V.import_json ?control reference.input_schema
        ]
    in
    let limits = t.limits in
    let limits_value =
      record
        [ "fuel", int limits.fuel
        ; "max_tasks", int limits.max_tasks
        ; "max_value_bytes", int limits.max_value_bytes
        ; "max_output_bytes", int limits.max_output_bytes
        ; "max_array_items", int limits.max_array_items
        ; "max_depth", int limits.max_depth
        ; "max_nested_calls", int limits.max_nested_calls
        ; "max_invocation_depth", int limits.max_invocation_depth
        ]
    in
    Option.iter control ~f:(fun value ->
      value.L.allocate (16 * List.length t.available_tools));
    let context =
      record
        [ "version", int 1
        ; "invocation_id", string t.invocation_id
        ; "provider_call_id", option string t.provider_call_id
        ; "session_id", string t.session_label
        ; "generation", int t.generation
        ; "origin", origin_value t.origin
        ; "parent_invocation", option string t.parent_invocation
        ; "parent_event", option string t.parent_event
        ; "parent_job", option string t.parent_job
        ; "tool_name", string t.tool_name
        ; "implementation_revision", string t.implementation_revision
        ; "capability_fingerprint", string t.capability_fingerprint
        ; "created_at_ms", int (ms t.created_at)
        ; "deadline_ms", option (fun value -> int (ms value)) t.deadline
        ; "limits", limits_value
        ; "available_tools", L.VArray (Array.of_list_map t.available_tools ~f:capability)
        ]
    in
    Option.iter control ~f:(fun value -> value.L.check_value context);
    let%map () =
      Json.check_value
        ~code:"invocation.invalid_context"
        ~max_depth:limits.max_depth
        ~max_array_items:limits.max_array_items
        ~max_bytes:limits.max_value_bytes
        context
    in
    context
  ;;
end

module Input = struct
  let prepare ?control ~schema ~limits value =
    let open Result.Let_syntax in
    let%bind () = protocol (I.validate_outcome (Complete value)) in
    let%bind () =
      Schema.validate schema value
      |> Result.map_error ~f:(fun _ -> "invocation.invalid_input: input schema mismatch")
    in
    let imported = V.import_json ?control value in
    let%map () =
      Json.check_value
        ~code:"invocation.invalid_input"
        ~max_depth:limits.Context.Limits.max_depth
        ~max_array_items:limits.max_array_items
        ~max_bytes:limits.max_value_bytes
        imported
    in
    imported
  ;;
end

module Outcome = struct
  type error =
    | Invalid of string
    | Work_rejected of string
  [@@deriving sexp_of]

  let decode ?control ~output_schema ~limits ~validate_work value =
    let open Result.Let_syntax in
    let invalid value = Invalid value in
    let max_bytes = limits.Context.Limits.max_output_bytes in
    let%bind () =
      Json.check_value
        ~max_depth:limits.max_depth
        ~max_array_items:limits.max_array_items
        ~max_bytes
        value
      |> Result.map_error ~f:invalid
    in
    let%bind outcome =
      (match value with
       | L.VVariant ("Complete", [ value ]) ->
         Result.map (Json.project ?control ~max_bytes value) ~f:(fun value ->
           I.Complete value)
       | VVariant ("Pending", [ VVariant (tag, [ VString id ]); value ]) ->
         let%bind work =
           match tag with
           | "Job" -> protocol (Id.Job.of_string id) |> Result.map ~f:(fun id -> I.Job id)
           | "Subscription" ->
             protocol (Id.Subscription.of_string id)
             |> Result.map ~f:(fun id -> I.Subscription id)
           | _ -> error "invocation.invalid_work" "expected Job or Subscription"
         in
         let%map value = Json.project ?control ~max_bytes value in
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
           | Some (VBool value) -> Ok value
           | _ -> Error "invalid retryable flag"
         in
         let%map details =
           V.expect_record_field "tool_error" fields "details"
           |> Result.bind ~f:(Json.project ?control ~max_bytes)
         in
         I.Fail { code; message; retryable; details }
       | _ -> error "invocation.invalid_output" "expected Complete, Pending or Fail")
      |> Result.map_error ~f:invalid
    in
    let%bind () = protocol (I.validate_outcome outcome) |> Result.map_error ~f:invalid in
    let%bind () =
      (match outcome with
       | I.Complete value | Pending (_, value) ->
         Schema.validate output_schema value
         |> Result.map_error ~f:(fun _ ->
           "invocation.invalid_output: output schema mismatch")
       | Fail _ -> Ok ()
       | Cancelled _ -> assert false)
      |> Result.map_error ~f:invalid
    in
    let%bind () =
      match outcome with
      | Pending (work, _) ->
        validate_work work |> Result.map_error ~f:(fun value -> Work_rejected value)
      | Complete _ | Fail _ -> Ok ()
      | Cancelled _ -> assert false
    in
    if String.length (Jsonaf.to_string (I.outcome_to_json outcome)) > max_bytes
    then
      Error (Invalid "invocation.output_limit: outcome envelope exceeds its byte limit")
    else Ok outcome
  ;;
end
