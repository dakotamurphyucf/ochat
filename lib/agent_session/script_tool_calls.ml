open Core
module I = Agent_protocol.Invocation
module C = Chat_response.Tool_capability
module EC = Chat_response.Extension_compiler
module M = Chat_response.Moderation.Capabilities
module E = Agent_protocol.Moderator_execution

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

type parent =
  | Invocation of I.t
  | Event of E.t

let with_scope t ~selected ~script ~valid_parent ~execute ~parent f =
  let session_id, generation, parent_invocation, parent_event, deadline =
    match parent with
    | Invocation parent ->
      ( parent.context.session_id
      , parent.context.generation
      , Some parent.context.id
      , None
      , parent.context.deadline )
    | Event parent ->
      ( parent.context.session_id
      , parent.context.generation
      , None
      , Some parent.context.id
      , None )
  in
  let active = Atomic.make true in
  let attempts = Atomic.make 0 in
  let limits = script.Chatmd_shell_spec.Extension_spec.limits in
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
  let call ~name ~args =
    if (not (Atomic.get active)) || not valid_parent
    then tool_error "invocation.inactive_scope"
    else if
      Atomic.fetch_and_add attempts 1
      >= Chat_response.Moderator_invocation.max_nested_calls
    then tool_error "invocation.nested_call_limit"
    else if Set.mem t.moderator_names name
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
                ~observer:{ script_id = script.id; source_sha256 = script.source_sha256 }
                ?parent_event
                { id = Agent_protocol.Id.Invocation.create ()
                ; session_id
                ; generation
                ; origin = Moderator
                ; provider_call_id = None
                ; call_entry_id = None
                ; parent_invocation
                ; parent_job = None
                ; tool_name = name
                ; implementation_revision = reference.implementation_revision
                ; capability_fingerprint = C.fingerprint selected
                ; input = args
                ; created_at = t.now ()
                ; deadline
                })
          in
          let%bind resolved =
            checked `Admission (fun () ->
              match t.requires_active_moderator reference with
              | true ->
                execute ~invocation (fun ~dispatched:_ ->
                  Ok
                    (fail
                       "moderator_reentrancy"
                       "Tool execution requires a decision from the active moderator."))
              | false ->
                Native_tool_invocation.run_scoped
                  ~execute
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

let with_invocation t ~prepared ~capabilities ~(parent : I.t) f =
  let selected = EC.capabilities prepared in
  let valid_parent =
    match parent.status with
    | Dispatching ->
      String.equal parent.context.tool_name (EC.declaration prepared).name
      && String.equal parent.context.implementation_revision (EC.fingerprint prepared)
      && String.equal parent.context.capability_fingerprint (C.fingerprint selected)
    | _ -> false
  in
  let t =
    { t with moderator_names = Set.add t.moderator_names (EC.declaration prepared).name }
  in
  with_scope
    t
    ~selected
    ~script:(EC.script prepared)
    ~valid_parent
    ~execute:capabilities.Operation_worker.Capabilities.with_invocation
    ~parent:(Invocation parent)
    f
;;

let with_moderator_scope t ~definition ~execute ~parent ~valid_parent f =
  match
    List.find_map (EC.compiled_scripts definition) ~f:(fun (script, _) ->
      match script.Chatmd_shell_spec.Extension_spec.kind with
      | Moderator_script -> Some script
      | Tool_script -> None)
  with
  | None -> f (fun ~name:_ ~args:_ -> tool_error "invocation.inactive_scope")
  | Some script ->
    let observer : I.observer =
      { script_id = script.id; source_sha256 = script.source_sha256 }
    in
    let moderator_names =
      List.fold
        (EC.prepared_tools definition)
        ~init:t.moderator_names
        ~f:(fun names prepared ->
          match (EC.declaration prepared).implementation with
          | Moderator _ -> Set.add names (EC.declaration prepared).name
          | Standalone _ -> names)
    in
    with_scope
      { t with moderator_names }
      ~selected:(EC.definition_capabilities definition)
      ~script
      ~valid_parent:(valid_parent observer)
      ~execute
      ~parent
      f
;;

let with_observation t ~definition ~execute ~(observing : I.t) f =
  with_moderator_scope
    t
    ~definition
    ~execute
    ~parent:(Invocation observing)
    ~valid_parent:(fun observer ->
      match observing.status, observing.observation with
      | (Resolved _ | Published _), Some { observer = owner; status = Observing; _ } ->
        I.equal_observer owner observer
      | _ -> false)
    f
;;

let with_event t ~definition ~execute ~(executing : E.t) f =
  with_moderator_scope
    t
    ~definition
    ~execute
    ~parent:(Event executing)
    ~valid_parent:(fun observer ->
      match executing.status with
      | Running ->
        Result.is_ok (E.validate executing)
        && I.equal_observer executing.context.source observer
      | Completed _ | Failed _ | Interrupted _ -> false)
    f
;;
