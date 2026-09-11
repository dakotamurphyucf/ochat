open Core
module V = Chat_response.Authoring_validation
module N = Native_tool_invocation

let name = Chatmd_shell_spec.Authoring_metadata.helper_name Validation

let registration ~env ~host =
  let module Definition = struct
    type input = Jsonaf.t

    let name = name

    let description =
      Some
        "Validate ChatML or captured ChatMD without evaluating it. Version 1 targets: \
         one_off_script, standalone_tool, moderator, generated_chatmd. Returns \
         source-bound diagnostics and deferred runtime checks; validation grants no \
         execution authority. Topic: runtime.invocations.validation."
    ;;

    let type_ = "function"
    let parameters = V.parameters
    let input_of_string = Jsonaf.of_string
  end
  in
  let require = Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message) in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:false
      (fun json ->
         let actual_host () =
           Script_tool_calls.current_native_services ()
           |> Result.bind ~f:(fun tools ->
             Result.of_option
               (Script_tool_calls.authoring_validation_host tools)
               ~error:"authoring.unavailable: invoking session has no validation host")
           |> Result.ok_or_failwith
         in
         let caller_host = actual_host () in
         let borrowed = N.borrow () |> require |> Result.ok_or_failwith in
         let capabilities =
           N.borrowed_capabilities borrowed |> require |> Result.ok_or_failwith
         in
         let report = V.validate ~env ~host:caller_host ~capabilities json in
         (match
            String.equal
              (V.host_fingerprint caller_host)
              (V.host_fingerprint (actual_host ()))
          with
          | true -> ()
          | false ->
            failwith "authoring.unavailable: invoking host changed during validation");
         ignore
           (N.borrowed_capabilities borrowed |> require |> Result.ok_or_failwith
            : Chat_response.Tool_capability.t);
         Openai.Responses.Tool_output.Output.Text (V.to_json report |> Jsonaf.to_string))
  in
  let implementation_revision =
    Chatmd_shell_spec.Source_ref.digest
      ("ochat.validate.sources.v1:" ^ V.host_fingerprint host)
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision
    ; result_contract = Native_output
    ; authoring_metadata = Some V.helper_metadata
    }
;;
