open! Core
module Query = Chat_response.Authoring_context
module V = Chat_response.Authoring_validation
module N = Native_tool_invocation
module C = Chat_response.Tool_capability

let name = Chatmd_shell_spec.Authoring_metadata.helper_name Reference

let registration ~host =
  let open Result.Let_syntax in
  let secret =
    Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
  in
  let%map service = Query.create ~secret () in
  let module Definition = struct
    type input = Jsonaf.t

    let name = name

    let description =
      Some
        "Query installed ChatML, ChatMD and runtime documentation without network or \
         model calls. Version 1 operations: search, topic, prepare, continue. Choose an \
         authoring task; the runtime supplies the actual target and tools. Start with \
         prepare for a flat feature map explaining what each feature enables, when to \
         use it, and direct guides to read before implementing it. Use this to discover \
         useful capabilities beyond the first approach you considered. Search returns \
         topic IDs/excerpts; topic includes prerequisites. Prepare also includes exact \
         selected tool schemas and compiler signatures; retrieve these directly with \
         topic_id=reference.tools or reference.signatures. Follow next_cursor when \
         complete is false, increasing max_tokens when minimum_next_tokens is returned. \
         Prepared packages are currently reviewed foundations, not full feature \
         coverage. All fields are required; set fields unused by the chosen operation to \
         null. Set max_tokens to null for the host default."
    ;;

    let type_ = "function"
    let parameters = Query.parameters
    let input_of_string = Jsonaf.of_string
  end
  in
  let require result =
    Result.map_error result ~f:(fun error -> error.Agent_protocol.Error.message)
    |> Result.ok_or_failwith
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:true
      (fun request ->
         let actual_host () =
           Script_tool_calls.current_native_services ()
           |> Result.bind ~f:(fun tools ->
             Script_tool_calls.authoring_validation_host tools
             |> Result.of_option
                  ~error:"authoring.unavailable: invoking session has no authoring target")
           |> Result.ok_or_failwith
         in
         let caller_host = actual_host () in
         let borrowed = require (N.borrow ()) in
         let capabilities = require (N.borrowed_capabilities borrowed) in
         let invocation = N.borrowed_invocation borrowed in
         let scope =
           [%sexp
             (invocation.context.session_id : Agent_protocol.Id.Session.t)
           , (invocation.context.generation : int)]
           |> Sexp.to_string
         in
         let response =
           Query.query service ~host:caller_host ~capabilities ~scope request
         in
         let current = require (N.borrowed_capabilities borrowed) in
         (match
            String.equal (C.fingerprint current) (C.fingerprint capabilities)
            && String.equal
                 (V.host_fingerprint caller_host)
                 (V.host_fingerprint (actual_host ()))
          with
          | true -> ()
          | false ->
            failwith "authoring.unavailable: invoking authority changed during retrieval");
         Openai.Responses.Tool_output.Output.Text (Jsonaf.to_string response))
  in
  let implementation_revision =
    Chatmd_shell_spec.Source_ref.digest
      ("ochat.authoring-context.native.v1:"
       ^ Query.fingerprint service
       ^ ":"
       ^ V.host_fingerprint host)
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision
    ; result_contract = Native_output
    ; authoring_metadata = Some { authoring = None; helper = Some Reference }
    }
;;
