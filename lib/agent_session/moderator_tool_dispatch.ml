open Core
module P = Agent_protocol
module I = P.Invocation
module Stream = Chat_response.In_memory_stream
module EC = Chat_response.Extension_compiler
module M = Chat_response.Moderator_manager
module Schema = Chatmd_shell_spec.Tool_schema

exception Dispatch_error of P.Error.t

let require = function
  | Ok value -> value
  | Error error -> raise (Dispatch_error error)
;;

let message result = Result.map_error result ~f:(fun (e : P.Error.t) -> e.message)
let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let handler_failure = function
  | Chat_response.Moderator_invocation.Unhandled ->
    fail "invocation.unhandled" "The moderator did not resolve this tool invocation."
  | Duplicate_resolution ->
    fail
      "invocation.duplicate_resolution"
      "The moderator attempted to resolve this tool invocation more than once."
  | Wrong_id ->
    fail "invocation.wrong_id" "The resolution referenced a different tool invocation."
  | Invalid_output ->
    fail "invocation.invalid_output" "The moderator returned an invalid tool outcome."
  | Invalid_state ->
    fail "invocation.invalid_state" "The moderator returned invalid persistent state."
  | Suspended ->
    fail "invocation.suspended" "A moderator tool cannot retain a legacy UI continuation."
  | Handler_failed ->
    fail "invocation.handler_failed" "The moderator tool handler failed."
;;

let create
      ~definition
      ~manager
      ~input
      ~(capabilities : Operation_worker.Capabilities.t)
      ~available_tools
      ~session_meta
      ~now
      ~validate_work
      ~admit
      ~prepare_outcome
      (request : Stream.Tool_dispatch.request)
      ~authorize
  =
  match
    List.find (EC.prepared_tools definition) ~f:(fun t ->
      String.equal (EC.declaration t).name request.name)
  with
  | None -> None
  | Some prepared ->
    if Option.is_some request.source || Option.is_some request.parent_call_id
    then
      raise
        (Dispatch_error
           (P.Error.create
              Permission_denied
              ~message:"moderator tool requires its owning persisted session"
              ~retryable:false
              ()));
    (match (EC.declaration prepared).implementation with
     | Standalone _ ->
       raise
         (Dispatch_error
            (P.Error.create
               Invalid_state
               ~message:"standalone execution service is not installed"
               ~retryable:false
               ()))
     | Moderator _ -> ());
    let value =
      match request.kind with
      | Chat_response.Tool_call.Kind.Function ->
        Schema.parse_json request.payload
        |> Result.map_error ~f:(fun _ -> "invalid JSON arguments")
      | Custom -> Ok (`String request.payload)
    in
    let value =
      Result.bind value ~f:(fun value ->
        match I.validate_outcome (Complete value) with
        | Ok () -> Ok value
        | Error _ -> Error "invalid JSON arguments")
    in
    let parse_error = Result.is_error value in
    let invocation =
      I.create
        { id = P.Id.Invocation.create ()
        ; session_id = input.Operation_worker.Input.session_id
        ; generation = input.session_generation
        ; origin = Model
        ; provider_call_id =
            (match History_entry.item request.call with
             | Function_call c -> Some c.call_id
             | Custom_tool_call c -> Some c.call_id
             | _ -> None)
        ; call_entry_id = Some (History_entry.id request.call)
        ; parent_invocation = None
        ; parent_job = None
        ; tool_name = request.name
        ; implementation_revision = EC.fingerprint prepared
        ; capability_fingerprint =
            Chat_response.Tool_capability.fingerprint (EC.capabilities prepared)
        ; input = Result.ok value |> Option.value ~default:`Null
        ; created_at = now ()
        ; deadline = None
        }
      |> require
    in
    let recorded = ref None in
    let observed = ref None in
    let failure = ref None in
    let checked outcome f =
      match f () with
      | Ok _ as result -> result
      | Error _ as result ->
        failure := Some outcome;
        result
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception exn ->
        failure := Some outcome;
        raise exn
    in
    capabilities.with_moderator_invocation ~invocation (fun ~dispatched ~commit ->
      let save resolved snapshot =
        let open Result.Let_syntax in
        let%map () =
          checked
            (fail
               "invocation.commit_failed"
               "The moderator result could not be committed.")
            (fun () -> commit ~resolved ~snapshot)
        in
        recorded := Some resolved
      in
      let run () =
        if
          parse_error
          || Result.is_error
               (Schema.validate (EC.input_schema prepared) invocation.context.input)
        then (
          failure
          := Some
               (fail
                  "invocation.invalid_input"
                  "The tool arguments do not satisfy its input schema.");
          Error "invalid tool arguments")
        else
          M.handle_invocation_entries
            manager
            ~invocation:dispatched
            ~history:request.history
            ~available_tools
            ~session_meta
            ~now_ms:
              (P.Timestamp.to_time_ns (now ())
               |> Time_ns.to_int_ns_since_epoch
               |> fun n -> n / 1_000_000)
            ~validate_work
            ~on_failure:(fun kind ->
              if Option.is_none !failure then failure := Some (handler_failure kind))
            ~authorize:(fun () ->
              let open Result.Let_syntax in
              let denied =
                fail
                  "invocation.permission_denied"
                  "The tool invocation is not authorized by the current policy."
              in
              let%bind () = checked denied (fun () -> admit request) in
              checked denied (fun () ->
                authorize ();
                Ok ()))
            ~prepare_resolution:(fun ~resolved ~outcome ~snapshot ->
              let open Result.Let_syntax in
              let%bind () =
                match resolved.I.status with
                | Resolved outcome ->
                  checked
                    (fail
                       "invocation.disclosure_rejected"
                       "The tool outcome did not pass the host output policy.")
                    (fun () -> prepare_outcome outcome)
                | _ -> Error "handler did not resolve"
              in
              let%map () = save resolved snapshot |> message in
              observed := Some outcome;
              fun () -> ())
          |> Result.map ~f:(fun _ -> ())
      in
      let result =
        try run () with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | _ -> Error "handler or authorization callback failed"
      in
      match result, !recorded with
      | Ok (), Some _ -> Ok ()
      | Error _, Some _ ->
        Error
          (P.Error.create
             Invalid_state
             ~message:"moderator failed after recording its resolution"
             ~retryable:false
             ())
      | Ok (), None | Error _, None ->
        let open Result.Let_syntax in
        let%bind snapshot =
          M.identity_snapshot manager
          |> Result.map_error ~f:(fun _ ->
            P.Error.create
              Invalid_state
              ~message:"cannot snapshot failed moderator invocation"
              ~retryable:false
              ())
        in
        let%bind resolved =
          I.resolve
            dispatched
            ~session_id:input.session_id
            ~generation:input.session_generation
            (Option.value !failure ~default:(handler_failure Handler_failed))
        in
        save resolved snapshot)
    |> require;
    let resolved = Option.value_exn !recorded in
    let outcome =
      match resolved.status with
      | Resolved outcome -> outcome
      | _ -> assert false
    in
    Some
      Stream.Tool_dispatch.
        { output = Text (Jsonaf.to_string (I.outcome_to_json outcome))
        ; runtime_requests =
            Option.value_map !observed ~default:[] ~f:(fun outcome ->
              outcome.Chat_response.Moderation.Outcome.runtime_requests)
        ; commit_output =
            Some
              (fun entry ->
                capabilities.publish_invocation_output
                  ~invocation_id:invocation.context.id
                  entry
                |> require)
        }
;;
