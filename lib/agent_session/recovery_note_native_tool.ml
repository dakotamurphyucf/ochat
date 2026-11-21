open! Core
module N = Native_tool_invocation
module A = Recovery_note_access
module R = Recovery_note_native_request
module C = Chat_response.Tool_capability
module Result_selection = Recovery_native_result
module P = Agent_protocol

let reject (error : P.Error.t) : Result_selection.rejection =
  match error.code, error.data with
  | Conflict, `String "recovery.note_missing" -> Missing
  | (Conflict | Idempotency_conflict), _ -> Conflict
  | Invalid_request, _ -> Invalid
  | (Permission_denied | Unauthenticated | Lease_stale), _ -> Denied
  | Resource_limit, _ -> Limit
  | Cursor_expired, _ -> Expired_cursor
  | _ -> Unavailable
;;

let execute effect_kind json =
  let open Result.Let_syntax in
  let%bind borrowed = N.borrow () in
  let%bind capabilities = N.borrowed_capabilities borrowed in
  let%bind binding =
    C.find capabilities ~name:(A.Effect.name effect_kind)
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "recovery note tool unselected")
  in
  let%bind token = N.Admission.capture borrowed ~binding in
  let with_service f =
    let%bind access = A.native token ~binding ~effect_kind in
    let%bind services =
      Script_tool_calls.current_native_services ()
      |> Result.map_error ~f:P.Error.invalid_request
    in
    let%bind service =
      Script_tool_calls.recovery_note_service services
      |> Result.of_option
           ~error:
             (P.Error.create
                Invalid_state
                ~message:"recovery note host unavailable"
                ~retryable:false
                ())
    in
    f access service
  in
  let deliver (delivery : Recovery_note_native_service.delivery) =
    Result_selection.deliver
      token
      ~binding
      ~custody:delivery.custody
      ~materialize:delivery.materialize
      ~refresh:delivery.refresh
  in
  let apply () =
    let%bind request = R.decode ~effect_kind json in
    match request with
    | R.Query query ->
      with_service (fun access service ->
        Result.bind (service.prepare ~access query) ~f:deliver)
    | Shared_read (reference, range) ->
      (match
         with_service (fun access service ->
           Result.bind (service.prepare_shared ~access reference range) ~f:deliver)
       with
       | Ok () -> Ok ()
       | Error _ -> Result_selection.reject token ~binding Unavailable)
    | Mutate mutation ->
      with_service (fun access service ->
        let%bind _receipt = service.mutate ~access mutation in
        Result_selection.acknowledge token ~binding)
  in
  match apply () with
  | Ok () -> Ok ()
  | Error error -> Result_selection.reject token ~binding (reject error)
;;

let registration effect_kind =
  let module Definition = struct
    type input = Jsonaf.t

    let name = A.Effect.name effect_kind
    let type_ = "function"

    let description =
      Some
        (match effect_kind with
         | A.Effect.Read ->
           "Read a half-open UTF-8 byte range of an exact local revision or an expressly \
            granted shared recovery note. Use the returned next_offset to continue."
         | List ->
           "List up to 16 local recovery notes in path order and obtain the catalog \
            revision. Pass null cursor for the first page."
         | Search ->
           "Find one literal match per local note in path order. Matches preserve UTF-8 \
            byte offsets. Pass null cursor for the first page."
         | Write ->
           "Create a local note using the expected catalog revision and virtual path, or \
            replace an exact note revision. Fill unused fields with null. After success, \
            list to obtain the committed selection."
         | Append ->
           "Append a nonempty suffix to an exact local recovery note revision. No \
            separator is inserted. After success, list to obtain the committed \
            selection."
         | Delete -> "Delete one exact local recovery note revision.")
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
         ignore (execute effect_kind json : (unit, P.Error.t) result);
         Openai.Responses.Tool_output.Output.Text "")
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision = A.Effect.implementation_revision effect_kind
    ; result_contract = Protected_recovery_v1
    ; source_requirement = Ordinary
    ; authoring_metadata = None
    }
;;
