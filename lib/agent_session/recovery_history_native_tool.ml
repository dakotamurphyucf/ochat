open! Core
module N = Native_tool_invocation
module R = Recovery_history_native_request
module Tool = R.Tool
module C = Chat_response.Tool_capability
module Delivery = Recovery_native_result
module P = Agent_protocol

let rejection (error : P.Error.t) : Delivery.rejection =
  match error.code with
  | Invalid_request -> Invalid
  | Permission_denied | Unauthenticated | Lease_stale -> Denied
  | Resource_limit -> Limit
  | Cursor_expired -> Expired_cursor
  | Conflict -> Conflict
  | _ -> Unavailable
;;

let execute tool json =
  let open Result.Let_syntax in
  let%bind borrowed = N.borrow () in
  let%bind capabilities = N.borrowed_capabilities borrowed in
  let%bind binding =
    C.find capabilities ~name:(Tool.name tool)
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "recovery history tool unselected")
  in
  let%bind token = N.Admission.capture borrowed ~binding in
  let prepared =
    let%bind services =
      Script_tool_calls.current_native_services ()
      |> Result.map_error ~f:P.Error.invalid_request
    in
    let%bind prepare =
      Script_tool_calls.recovery_history_service services
      |> Result.of_option
           ~error:
             (P.Error.create
                Invalid_state
                ~message:"recovery history host unavailable"
                ~retryable:false
                ())
    in
    prepare ~tool token ~binding json
  in
  match prepared with
  | Error error -> Delivery.reject token ~binding (rejection error)
  | Ok selected ->
    Delivery.deliver
      token
      ~binding
      ~custody:selected.custody
      ~materialize:selected.materialize
      ~refresh:selected.refresh
;;

let registration tool =
  let module Definition = struct
    type input = Jsonaf.t

    let name = Tool.name tool
    let type_ = "function"

    let description =
      Some
        (match tool with
         | Tool.List_windows ->
           "List local recovery windows in the current generation. Start with a null \
            cursor."
         | List_items ->
           "List ordinary user and assistant occurrences in an exact selected local \
            window. Choose canonical or effective and copy returned citations exactly."
         | Search ->
           "Find literal UTF-8 text in ordinary local user and assistant history. Choose \
            canonical or effective. Continue with the returned next cursor and the same \
            filters."
         | Read_text ->
           "Read a bounded half-open UTF-8 byte range from an exact selected text part. \
            Retain the original preparation revision for effective citations."
         | Read_media ->
           "Read an admitted retained original from one exact local canonical part. \
            Images are returned as image input. Documents return a bounded byte chunk \
            with its media type, digest and next offset.")
    ;;

    let parameters = R.parameters tool
    let input_of_string = R.decode_text_exn
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:true
      (fun json ->
         ignore (execute tool json : (unit, P.Error.t) result);
         Openai.Responses.Tool_output.Output.Text "")
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision = Tool.implementation_revision tool
    ; result_contract = Protected_recovery_v1
    ; source_requirement = Ordinary
    ; authoring_metadata = None
    }
;;
