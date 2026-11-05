open! Core
module P = Agent_protocol
module N = Native_tool_invocation
module A = Recurrence_native_admission
module R = Recurrence_native_request
module C = Chat_response.Tool_capability

let execute effect_kind json =
  let open Result.Let_syntax in
  let%bind request = R.decode ~effect_kind json in
  let%bind borrowed = N.borrow () in
  let%bind capabilities = N.borrowed_capabilities borrowed in
  let%bind binding =
    C.find capabilities ~name:(A.Effect.name effect_kind)
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "recurrence binding is unavailable")
  in
  let%bind admission = A.create borrowed ~binding ~effect_kind in
  let%bind services =
    Script_tool_calls.current_native_services ()
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let%bind () =
    Script_tool_calls.check_current_metadata_admission
      services
      (A.invocation admission)
      (A.binding admission)
  in
  let%bind service =
    Script_tool_calls.recurrence_service services
    |> Result.of_option
         ~error:
           (P.Error.create
              Method_not_found
              ~message:"recurrence session service is unavailable"
              ~retryable:false
              ())
  in
  Recurrence_service.run service ~admission ~request
;;

let registration effect_kind =
  let module Definition = struct
    type input = Jsonaf.t

    let name = A.Effect.name effect_kind
    let type_ = "function"

    let description =
      Some
        (match effect_kind with
         | A.Effect.Create ->
           "Create a paused recurrence on this invoking controller. Echo the read \
            generation, catalog, source and lifecycle basis. Select an existing \
            session/input or a declared authored worker with an explicit mode. A retry \
            uses the identical key and request; creation grants no execution permission."
         | Edit ->
           "Replace a paused recurrence definition using explicit read CAS. A new key \
            captures the currently selected source once. An exact retry retains the \
            acknowledged target rather than recapturing newer code."
         | List ->
           "List retained recurrence definitions and current controller CAS. Stored \
            target kind and source are historical metadata, not target access or an \
            execution grant."
         | Get ->
           "Read one retained recurrence definition and current controller CAS without \
            private input, captured files or credentials."
         | Pause ->
           "Pause future recurrence launches with exact catalog/control CAS, even when \
            source access was revoked. Already admitted work is not cancelled."
         | Resume ->
           "Explicitly enable a paused recurrence using current source/lifecycle and \
            catalog/control CAS. This does not grant target permissions or catch up \
            unbounded missed work."
         | Cancel ->
           "Permanently retire a recurrence identity with exact catalog/control CAS. \
            This fences future launches; it does not promise cancellation of admitted or \
            external work."
         | Status ->
           "Read bounded planning status, unresolved occurrences and the latest \
            occurrence. Target runtime/outcome visibility requires current access; \
            controller history alone grants no target visibility."
         | History ->
           "Read bounded retained occurrence/attempt history and omitted ranges. Target \
            details require current access. Missing detail and unknown outcomes are not \
            no-effects evidence or retry permission.")
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
          ("ochat.recurrence.native.v1:" ^ A.Effect.name effect_kind)
    ; result_contract = Invocation_v1
    ; source_requirement = Ordinary
    ; authoring_metadata = None
    }
;;
