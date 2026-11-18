open! Core
module P = Agent_protocol
module N = Native_tool_invocation
module A = Instruction_native_admission
module R = Instruction_native_request
module S = Instruction_service
module C = Chat_response.Tool_capability

let execute effect_kind json =
  let open Result.Let_syntax in
  let%bind request = R.decode ~effect_kind json in
  let%bind borrowed = N.borrow () in
  let%bind capabilities = N.borrowed_capabilities borrowed in
  let%bind binding =
    C.find capabilities ~name:(A.Effect.name effect_kind)
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "instruction binding is unavailable")
  in
  let%bind token = N.Admission.capture borrowed ~binding in
  let%bind admission = A.create token ~binding ~effect_kind in
  let%bind services =
    Script_tool_calls.current_native_services ()
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let%bind service =
    Script_tool_calls.instruction_service services
    |> Result.of_option
         ~error:
           (P.Error.create
              Method_not_found
              ~message:"instruction session service is unavailable"
              ~retryable:false
              ())
  in
  match request with
  | R.Refresh selection -> S.refresh service ~admission ~selection
  | Read request -> S.read service ~admission ~request
  | Activate { request; capture_id; expected_revision; key } ->
    S.activate service ~admission ~request ~capture_id ~expected_revision ~key
    |> Result.map ~f:Instruction_command.Receipt.to_json
  | Deactivate { all; members; expected_revision; key } ->
    (match members with
     | Some members -> S.remove_skills service ~admission ~members ~expected_revision ~key
     | None ->
       S.deactivate
         service
         ~admission
         ~change:(if all then Clear else Remove_project)
         ~expected_revision
         ~key)
    |> Result.map ~f:Instruction_command.Receipt.to_json
  | Status -> S.status service ~admission |> Result.map ~f:P.Instruction_status.to_json
;;

let registration effect_kind =
  let module Definition = struct
    type input = Jsonaf.t

    let name = A.Effect.name effect_kind
    let type_ = "function"

    let description =
      Some
        (match effect_kind with
         | A.Effect.Refresh ->
           "Discover configured instruction roots, or refresh project AGENTS.md / direct \
            skill SKILL.md metadata. selection=null lists admitted root IDs. Discovery \
            never activates content. Use explicit project scope (empty string for root) \
            or ordered skill roots with null scope."
         | Read ->
           "Read exact current instruction members/digests from discovery and return a \
            capture ID plus original bytes. Project members must be the complete ordered \
            root-to-scope AGENTS.md chain. Reading never activates content."
         | Activate ->
           "Explicitly activate the exact capture ID and descriptor returned by read. \
            Uses selection CAS expected_revision and durable idempotency key. Project \
            guidance replaces project selection; skills replace ordered skill selection. \
            Files grant no tools or permissions."
         | Deactivate ->
           "Explicitly remove all instructions, project guidance, or listed selected \
            skills using CAS and durable idempotency key. Set members=null for \
            all/project. Requires session authority even when source access has been \
            revoked."
         | Status ->
           "Inspect durable instruction selection revision and counts without bodies, \
            paths or root IDs. Stored selection alone does not prove current source \
            access or execution authority.")
    ;;

    let parameters = R.parameters effect_kind
    let input_of_string = R.decode_text_exn
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:true
      (fun json ->
         let outcome =
           match execute effect_kind json with
           | Ok value -> P.Invocation.Complete value
           | Error error ->
             P.Invocation.Fail
               { code = P.Error.code_to_string error.code
               ; message = error.message
               ; retryable = error.retryable
               ; details = error.data
               }
         in
         Openai.Responses.Tool_output.Output.Text
           (Jsonaf.to_string (P.Invocation.outcome_to_json outcome)))
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest
          ("ochat.instructions.native.v1:" ^ A.Effect.name effect_kind)
    ; result_contract = Invocation_v1
    ; source_requirement = Ordinary
    ; authoring_metadata = None
    }
;;
