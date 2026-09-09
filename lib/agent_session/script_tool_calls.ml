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

let native_dispatch t ~input ~capabilities =
  Native_tool_dispatch.create
    ~input
    ~capabilities
    ~registry:t.registry
    ~now:t.now
    ~is_halted:t.is_halted
    ~admit:t.authorize
    ~prepare_output:t.prepare_output
;;

let is_halted t = t.is_halted ()

let validate_definition t definition =
  let captured = EC.definition_capabilities definition in
  let names = List.map (C.references captured) ~f:(fun reference -> reference.C.name) in
  let open Result.Let_syntax in
  let%bind selected =
    C.select (t.registry ()) ~names
    |> Result.map_error ~f:(fun _ -> "captured native capability is no longer available")
  in
  match String.equal (C.fingerprint selected) (C.fingerprint captured) with
  | true -> Ok ()
  | false -> Error "captured native capability bindings changed"
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

type prepared_call =
  { reference : C.reference
  ; input : Jsonaf.t
  ; routing : I.routing option
  ; rejection : I.outcome option
  }

let with_scope
      ?prepare
      t
      ~selected
      ~script
      ~origin
      ~observer
      ~valid_parent
      ~execute
      ~parent
      f
  =
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
          let id = Agent_protocol.Id.Invocation.create () in
          let%bind prepared =
            checked `Admission (fun () ->
              match prepare with
              | None -> Ok { reference; input = args; routing = None; rejection = None }
              | Some prepare -> prepare ~id reference args)
          in
          let reference = prepared.reference in
          let%bind () = checked `Input (fun () -> validate_value prepared.input) in
          let%bind invocation =
            checked `Admission (fun () ->
              I.create
                ?observer
                ?parent_event
                ?routing:prepared.routing
                { id
                ; session_id
                ; generation
                ; origin
                ; provider_call_id = None
                ; call_entry_id = None
                ; parent_invocation
                ; parent_job = None
                ; tool_name = reference.name
                ; implementation_revision = reference.implementation_revision
                ; capability_fingerprint = C.fingerprint selected
                ; input = prepared.input
                ; created_at = t.now ()
                ; deadline
                })
          in
          let%bind resolved =
            checked `Admission (fun () ->
              match prepared.rejection with
              | Some outcome -> execute ~invocation (fun ~dispatched:_ -> Ok outcome)
              | None when Option.is_none prepare && t.requires_active_moderator reference
                ->
                execute ~invocation (fun ~dispatched:_ ->
                  Ok
                    (fail
                       "moderator_reentrancy"
                       "Tool execution requires a decision from the active moderator."))
              | None ->
                Native_tool_invocation.run_scoped
                  ~execute
                  ~registry
                  ~reference
                  ~invocation
                  ~is_halted:t.is_halted
                  ~authorize:t.authorize
                  ~prepare_output)
          in
          let%map () =
            match resolved.observation with
            | None -> Ok ()
            | Some _ -> checked `Observation (fun () -> t.defer_observation resolved)
          in
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
    ~origin:Moderator
    ~observer:
      (Some
         { script_id = (EC.script prepared).id
         ; source_sha256 = (EC.script prepared).source_sha256
         })
    ~valid_parent
    ~execute:capabilities.Operation_worker.Capabilities.with_invocation
    ~parent:(Invocation parent)
    f
;;

let with_standalone ?observer t ~prepared ~capabilities ~(parent : I.t) ~moderate f =
  let selected = EC.capabilities prepared in
  let kind (reference : C.reference) =
    match C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint with
    | Ok binding when String.equal (C.implementation binding).info.type_ "custom" ->
      Chat_response.Moderation.Tool_call.Custom
    | _ -> Function
  in
  let payload reference input =
    match kind reference, input with
    | Custom, `String text -> text
    | Function, _ | Custom, _ -> Jsonaf.to_string input
  in
  let fingerprint payload =
    I.
      { sha256 = Chatmd_shell_spec.Source_ref.digest payload
      ; byte_length = String.length payload
      }
  in
  let prepare ~id (original : C.reference) input =
    let original_payload = payload original input in
    let route reference input preparation rejection =
      let routing : I.routing =
        { kind =
            (match kind reference with
             | Custom -> Custom
             | Function -> Function)
        ; original_name = original.name
        ; original_payload = fingerprint original_payload
        ; final_payload = fingerprint (payload reference input)
        ; canonical_payload = None
        ; preparation
        }
      in
      Ok { reference; input; routing = Some routing; rejection }
    in
    let reject preparation code message =
      route original input preparation (Some (fail code message))
    in
    let valid_input =
      let open Result.Let_syntax in
      let%bind schema = Chatmd_shell_spec.Tool_schema.compile original.input_schema in
      Chatmd_shell_spec.Tool_schema.validate schema input
    in
    match valid_input with
    | Error _ ->
      reject Invalid_input "invocation.invalid_input" "The tool arguments are invalid."
    | Ok () ->
      let call : Chat_response.Moderation.Tool_call.t =
        { id = Agent_protocol.Id.Invocation.to_string id
        ; name = original.name
        ; args = input
        ; kind = kind original
        ; payload_text = original_payload
        ; meta =
            `Object
              [ "origin", `String "script"
              ; ( "parent_invocation"
                , `String (Agent_protocol.Id.Invocation.to_string parent.context.id) )
              ]
        }
      in
      let decision = checked () (fun () -> moderate call) in
      (match decision with
       | Error () ->
         reject Pre_tool_failed "invocation.pre_tool_failed" "Pre-tool moderation failed."
       | Ok (Some (Chat_response.Moderation.Tool_moderation.Reject _)) ->
         reject
           Pre_tool_rejected
           "invocation.pre_tool_rejected"
           "Pre-tool moderation rejected the call."
       | Ok action ->
         let name, args =
           match action with
           | None | Some Approve -> original.name, input
           | Some (Rewrite_args args) -> original.name, args
           | Some (Redirect (name, args)) -> name, args
           | Some (Reject _) -> assert false
         in
         (match
            List.find (C.references selected) ~f:(fun reference ->
              String.equal reference.name name)
          with
          | None ->
            reject
              Pre_tool_rejected
              "invocation.unselected_tool"
              "The redirected tool is not selected."
          | Some reference -> route reference args Passed None))
  in
  let valid_parent =
    match parent.status, (EC.declaration prepared).implementation with
    | Dispatching, Standalone _ ->
      String.equal parent.context.tool_name (EC.declaration prepared).name
      && String.equal parent.context.implementation_revision (EC.fingerprint prepared)
      && String.equal parent.context.capability_fingerprint (C.fingerprint selected)
    | _ -> false
  in
  with_scope
    ~prepare
    t
    ~selected
    ~script:(EC.script prepared)
    ~origin:Script
    ~observer
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
      ~origin:Moderator
      ~observer:(Some observer)
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
