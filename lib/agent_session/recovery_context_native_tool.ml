open! Core
module N = Native_tool_invocation
module P = Agent_protocol
module S = Recovery_context_service
module C = Chat_response.Tool_capability
module J = P.Json_codec

let execute effect_kind json =
  let open Result.Let_syntax in
  let%bind borrowed = N.borrow () in
  let%bind capabilities = N.borrowed_capabilities borrowed in
  let%bind binding =
    C.find capabilities ~name:(S.name effect_kind)
    |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "context tool is unselected")
  in
  let%bind token = N.Admission.capture borrowed ~binding in
  let%bind access = S.capture token ~binding ~effect_kind in
  let%bind services =
    Script_tool_calls.current_native_services ()
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let%bind service =
    Script_tool_calls.recovery_context_service services
    |> Result.of_option
         ~error:
           (P.Error.create
              Method_not_found
              ~message:"context host unavailable"
              ~retryable:false
              ())
  in
  let%bind fields = J.fields json in
  match effect_kind, J.to_alist fields with
  | S.Remaining, [] -> Result.map (service.remaining access) ~f:S.to_json
  | New_window, [ ("expected_window", value) ] ->
    let%bind window = P.Id.Context_window.of_json value in
    Result.map
      (service.new_window access window)
      ~f:Recovery_rollover.Receipt.to_public_json
  | _ -> Error (P.Error.invalid_request "invalid context tool arguments")
;;

let registration effect_kind =
  let module Definition = struct
    type input = Jsonaf.t

    let name = S.name effect_kind
    let type_ = "function"

    let description =
      Some
        (match effect_kind with
         | S.Remaining ->
           "Inspect the current local window and safe remaining-capacity evidence, \
            including stale or unknown status."
         | New_window ->
           "Request a local rollover for the expected current window. A pending receipt \
            means admission only.")
    ;;

    let parameters =
      let properties, required =
        match effect_kind with
        | S.Remaining -> [], []
        | New_window ->
          ( [ "expected_window", `Object [ "type", `String "string" ] ]
          , [ `String "expected_window" ] )
      in
      `Object
        [ "type", `String "object"
        ; "properties", `Object properties
        ; "required", `Array required
        ; "additionalProperties", `False
        ]
    ;;

    let input_of_string = Jsonaf.of_string
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
          ("ochat.recovery.context.native.v1:" ^ S.name effect_kind)
    ; result_contract = Invocation_v1
    ; source_requirement = Ordinary
    ; authoring_metadata = None
    }
;;
