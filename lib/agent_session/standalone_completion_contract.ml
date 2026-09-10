open Core
module P = Agent_protocol
module C = Chat_response.Tool_capability
module E = Chat_response.Extension_compiler
module B = Chat_response.Background_request

let denied () =
  P.Error.create
    Permission_denied
    ~message:"standalone completion publisher is unavailable or changed"
    ~retryable:false
    ()
;;

let capture ~prepared ~current_capabilities =
  let open Result.Let_syntax in
  let tool = E.declaration prepared in
  let%bind () =
    match tool.implementation with
    | Standalone _ -> Ok ()
    | Moderator _ -> Error (denied ())
  in
  let%bind binding =
    C.find current_capabilities ~name:tool.name
    |> Result.map_error ~f:(fun _ -> denied ())
  in
  let%bind capability_pins = B.capability_pins (E.capabilities prepared) in
  let limits = E.execution_limits prepared in
  let contract : P.Completion_contract.t =
    { tool_name = tool.name
    ; tool_fingerprint = C.permission_fingerprint binding
    ; capability_pins
    ; completion_schema =
        Option.map (E.completion_schema prepared) ~f:Chatmd_shell_spec.Tool_schema.to_json
    ; max_output_bytes =
        Chatmd_shell_spec.Duration.bytes_to_int64 limits.max_output_bytes
        |> Int64.to_int_exn
    ; max_output_depth = limits.max_depth
    }
  in
  let%map () = P.Completion_contract.validate contract in
  contract
;;

let rebind (contract : P.Completion_contract.t) ~current_capabilities =
  let open Result.Let_syntax in
  let%bind () = P.Completion_contract.validate contract in
  let%bind publisher =
    C.find current_capabilities ~name:contract.tool_name
    |> Result.map_error ~f:(fun _ -> denied ())
  in
  let%bind () =
    match String.equal (C.permission_fingerprint publisher) contract.tool_fingerprint with
    | true -> Ok ()
    | false -> Error (denied ())
  in
  B.rebind_capabilities ~pins:contract.capability_pins ~capabilities:current_capabilities
;;

let validate_result (contract : P.Completion_contract.t) completion =
  let failure = P.Completion_projection.rejection in
  let valid_schema () =
    match completion, contract.completion_schema with
    | P.Completion.Succeeded value, Some schema ->
      Chatmd_shell_spec.Tool_schema.compile schema
      |> Result.bind ~f:(fun schema ->
        Chatmd_shell_spec.Tool_schema.validate schema value)
      |> Result.is_ok
    | _ -> true
  in
  match
    Result.is_ok (P.Completion_contract.validate contract)
    && Result.is_ok (P.Completion.validate completion)
    && Result.is_ok
         (P.Json_codec.validate_limits
            ~max_bytes:contract.max_output_bytes
            ~max_depth:contract.max_output_depth
            (P.Completion.to_json completion))
    && valid_schema ()
  with
  | true -> Ok ()
  | false -> Error failure
;;

type projection =
  { receipt : P.Completion_projection.t
  ; completion : P.Completion.t
  ; disclosure_pins : (string * string) list
  }

let invalid message = Error (P.Error.invalid_request message)

let subject (invocation : P.Invocation.t) (job : P.Job.t) =
  let open Result.Let_syntax in
  let%bind () = P.Invocation.validate invocation in
  let%bind contract =
    match invocation.completion_contract, invocation.status, job.kind, job.launch with
    | ( Some contract
      , Published (Pending (Job id, _))
      , Async_tool
      , Some { owner = Invocation owner; parent_job = None; _ } )
      when P.Id.Job.equal id job.id
           && P.Id.Invocation.equal owner invocation.context.id
           && P.Id.Session.equal invocation.context.session_id job.session_id
           && Int.equal invocation.context.generation job.generation -> Ok contract
    | _ -> invalid "standalone completion does not match its admitted owned job"
  in
  let%bind stored = P.Job.terminal_result job in
  let%map stored =
    Result.of_option
      stored
      ~error:(P.Error.invalid_request "standalone job is not terminal")
  in
  contract, stored
;;

let project ~invocation ~job ~completion ~current_capabilities =
  let open Result.Let_syntax in
  let%bind contract, stored = subject invocation job in
  let%bind () =
    match invocation.publication_discarded with
    | None -> Ok ()
    | Some _ -> invalid "standalone acknowledgement was discarded"
  in
  let%bind selected = rebind contract ~current_capabilities in
  let%bind matches = P.Stored_completion.matches stored completion in
  let%bind () =
    match matches with
    | true -> Ok ()
    | false -> invalid "standalone adapter input differs from its terminal job"
  in
  let%map disclosure_pins = B.capability_pins selected in
  let completion, rejected =
    match validate_result contract completion with
    | Ok () -> completion, false
    | Error error -> P.Completion.Failed error, true
  in
  { receipt =
      { job_attempt = job.attempt
      ; contract_sha256 = P.Completion_projection.contract_digest contract
      ; result_sha256 = P.Completion_projection.result_digest stored
      ; rejected
      ; result_reference = None
      }
  ; completion
  ; disclosure_pins
  }
;;

let authorize ~invocation ~job ~current_capabilities ~policy =
  let open Result.Let_syntax in
  let%bind contract, _ = subject invocation job in
  let%bind () =
    match invocation.publication_discarded with
    | None -> Ok ()
    | Some _ -> invalid "standalone acknowledgement was discarded"
  in
  let%bind selected = rebind contract ~current_capabilities in
  let%bind request = B.of_json ~policy job.payload in
  B.validate_capabilities request ~capabilities:selected
;;

let reference_if_needed ~invocation ~job ~max_bytes ~max_depth projection =
  let open Result.Let_syntax in
  let%bind () =
    match max_bytes > 0 && max_depth > 0 with
    | true -> Ok ()
    | false -> invalid "invalid completion presentation limits"
  in
  let within value =
    P.Json_codec.validate_limits ~max_bytes ~max_depth (P.Completion.to_json value)
  in
  match within projection.completion with
  | Ok () -> Ok projection
  | Error _ when projection.receipt.rejected ->
    invalid "notification bounds cannot represent the completion rejection"
  | Error _ ->
    let%bind contract, stored = subject invocation job in
    let%bind () =
      match
        Int.equal projection.receipt.job_attempt job.P.Job.attempt
        && String.equal
             projection.receipt.contract_sha256
             (P.Completion_projection.contract_digest contract)
        && String.equal
             projection.receipt.result_sha256
             (P.Completion_projection.result_digest stored)
      with
      | true -> Ok ()
      | false -> invalid "result reference projection belongs to another completion"
    in
    let%bind reference = P.Job_result_reference.of_job job in
    let completion = P.Completion.Succeeded (P.Job_result_reference.to_json reference) in
    let%map () = within completion in
    { projection with
      completion
    ; receipt = { projection.receipt with result_reference = Some reference }
    }
;;

let validate_projection ~invocation ~job (delivery : P.Delivery.t) =
  let open Result.Let_syntax in
  let%bind () = P.Delivery.validate delivery in
  let%bind contract, stored = subject invocation job in
  let%bind receipt =
    Result.of_option
      delivery.completion_projection
      ~error:(P.Error.invalid_request "standalone delivery has no completion projection")
  in
  let%bind () =
    match
      delivery.context.invocation_id, delivery.context.work, delivery.disclosure_pins
    with
    | Some id, Some (Job job_id), Some pins
      when P.Id.Invocation.equal id invocation.context.id
           && P.Id.Job.equal job_id job.id
           && P.Id.Session.equal delivery.context.session_id job.session_id
           && Int.equal delivery.context.generation job.generation
           && Int.equal receipt.job_attempt job.attempt
           && String.equal
                receipt.contract_sha256
                (P.Completion_projection.contract_digest contract)
           && String.equal
                receipt.result_sha256
                (P.Completion_projection.result_digest stored)
           && List.equal
                (Tuple2.equal ~eq1:String.equal ~eq2:String.equal)
                pins
                contract.capability_pins -> Ok ()
    | _ -> invalid "standalone completion projection differs from its captured evidence"
  in
  match receipt.result_reference, receipt.rejected with
  | Some reference, false ->
    let%bind () = P.Job_result_reference.validate_job reference job in
    (match stored with
     | Artifact _ -> Ok ()
     | Inline completion ->
       validate_result contract completion
       |> Result.map_error ~f:(fun _ ->
         P.Error.invalid_request "result reference exposes an invalid original completion"))
  | Some _, true -> invalid "rejected completion cannot disclose a result reference"
  | None, false ->
    let%bind matches = P.Stored_completion.matches stored delivery.context.completion in
    (match
       matches && Result.is_ok (validate_result contract delivery.context.completion)
     with
     | true -> Ok ()
     | false -> invalid "standalone completion projection contains an invalid result")
  | None, true ->
    (* Inline results can be rechecked during replay. An artifact receipt binds the
       exact immutable storage descriptor; admission has materialized and checked
       that descriptor before recording the rejection. Replay never reads files. *)
    (match stored with
     | Artifact _ -> Ok ()
     | Inline completion ->
       (match validate_result contract completion with
        | Error _ -> Ok ()
        | Ok () -> invalid "standalone completion projection rejects a valid result"))
;;
