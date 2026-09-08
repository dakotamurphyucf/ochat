open Core
module I = Agent_protocol.Invocation
module C = Chat_response.Tool_capability
module EC = Chat_response.Extension_compiler
module M = Chat_response.Moderation.Capabilities

type t =
  { registry : unit -> C.t
  ; moderator_names : String.Set.t
  ; now : unit -> Agent_protocol.Timestamp.t
  ; is_halted : unit -> bool
  ; requires_active_moderator : C.reference -> bool
  ; authorize : I.t -> C.binding -> (unit, Agent_protocol.Error.t) result
  ; prepare_output :
      Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, Agent_protocol.Error.t) result
  ; defer_observation : I.t -> (unit, Agent_protocol.Error.t) result
  }

let create
      ~registry
      ~moderator_names
      ~now
      ~is_halted
      ~requires_active_moderator
      ~authorize
      ~prepare_output
      ~defer_observation
  =
  { registry
  ; moderator_names
  ; now
  ; is_halted
  ; requires_active_moderator
  ; authorize
  ; prepare_output
  ; defer_observation
  }
;;

let tool_error code = Ok (M.Tool_error code)
let fail code message = I.Fail { code; message; retryable = false; details = `Null }

let checked error f =
  match f () with
  | Ok value -> Ok value
  | Error _ -> Error error
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception _ -> Error error
;;

let with_invocation t ~prepared ~capabilities ~(parent : I.t) f =
  let active = Atomic.make true in
  let attempts = Atomic.make 0 in
  let selected = EC.capabilities prepared in
  let limits = EC.execution_limits prepared in
  let validate_value value =
    let open Result.Let_syntax in
    let%bind () = I.validate_outcome (Complete value) in
    Chat_response.Moderator_invocation.snapshot_state
      ~limits
      (Chatml.Chatml_value_codec.jsonaf_to_value value)
    |> Result.map ~f:(fun _ -> ())
    |> Result.map_error ~f:(fun _ ->
      Agent_protocol.Error.invalid_request "nested value exceeds script limits")
  in
  let prepare_output output =
    let open Result.Let_syntax in
    let%bind value = t.prepare_output output in
    let%map () = validate_value value in
    value
  in
  let references = C.references selected in
  let names = List.map references ~f:(fun reference -> reference.C.name) in
  let registry () =
    match C.select (t.registry ()) ~names with
    | Ok selected -> selected
    | Error _ -> failwith "captured tool subset is no longer available"
  in
  let valid_parent =
    match parent.status with
    | Dispatching ->
      String.equal parent.context.tool_name (EC.declaration prepared).name
      && String.equal parent.context.implementation_revision (EC.fingerprint prepared)
      && String.equal parent.context.capability_fingerprint (C.fingerprint selected)
    | _ -> false
  in
  let call ~name ~args =
    if (not (Atomic.get active)) || not valid_parent
    then tool_error "invocation.inactive_scope"
    else if
      Atomic.fetch_and_add attempts 1
      >= Chat_response.Moderator_invocation.max_nested_calls
    then tool_error "invocation.nested_call_limit"
    else if
      String.equal name (EC.declaration prepared).name || Set.mem t.moderator_names name
    then tool_error "moderator_reentrancy"
    else (
      match
        List.find references ~f:(fun reference -> String.equal reference.C.name name)
      with
      | None -> tool_error "invocation.unselected_tool"
      | Some reference ->
        let execute () =
          let open Result.Let_syntax in
          let%bind () = checked `Input (fun () -> validate_value args) in
          let%bind invocation =
            checked `Admission (fun () ->
              I.create
                { id = Agent_protocol.Id.Invocation.create ()
                ; session_id = parent.context.session_id
                ; generation = parent.context.generation
                ; origin = Moderator
                ; provider_call_id = None
                ; call_entry_id = None
                ; parent_invocation = Some parent.context.id
                ; parent_job = None
                ; tool_name = name
                ; implementation_revision = reference.implementation_revision
                ; capability_fingerprint = C.fingerprint selected
                ; input = args
                ; created_at = t.now ()
                ; deadline = parent.context.deadline
                })
          in
          let%bind resolved =
            checked `Admission (fun () ->
              match t.requires_active_moderator reference with
              | true ->
                capabilities.Operation_worker.Capabilities.with_invocation
                  ~invocation
                  (fun ~dispatched:_ ->
                     Ok
                       (fail
                          "moderator_reentrancy"
                          "Tool execution requires a decision from the active moderator."))
              | false ->
                Native_tool_invocation.run
                  ~capabilities
                  ~registry
                  ~reference
                  ~invocation
                  ~is_halted:t.is_halted
                  ~authorize:t.authorize
                  ~prepare_output)
          in
          let%map () = checked `Observation (fun () -> t.defer_observation resolved) in
          match resolved.status with
          | Resolved (Complete value) -> M.Tool_ok value
          | Resolved (Fail error) -> Tool_error error.code
          | Resolved (Cancelled _) -> Tool_error "invocation.cancelled"
          | _ -> Tool_error "invocation.invalid_outcome"
        in
        (match execute () with
         | Ok result -> Ok result
         | Error `Observation -> tool_error "invocation.observation_failed"
         | Error `Input -> tool_error "invocation.invalid_input"
         | Error `Admission -> tool_error "invocation.admission_failed"
         | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
         | exception _ -> tool_error "invocation.host_failed"))
  in
  Exn.protect ~f:(fun () -> f call) ~finally:(fun () -> Atomic.set active false)
;;
