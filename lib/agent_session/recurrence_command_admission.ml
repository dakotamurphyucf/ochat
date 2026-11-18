open! Core
module P = Agent_protocol
module F = Recurrence_definition
module C = Recurrence_command
module G = Recurrence_goal_capture

type native_selection = unit -> (Goal_work_attribution.t option, P.Error.t) result

type goal =
  | Native_new of native_selection
  | Native_edit of native_selection
  | New of C.Goal_owner_request.t
  | Edit of C.Goal_owner_request.t
  | Enable

type goal_basis =
  { action : goal
  ; binding : F.Goal_binding.Capture.t
  ; source : P.Run_source.t
  }

type t =
  { receipt : Recurrence_command_receipt.t
  ; delta : Session_delta.t option
  ; current : Recurrence_controller.observation option
  ; goal : goal_basis option
  }

let receipt t = t.receipt
let delta t = t.delta
let checks_goal t = Option.is_some t.goal

let check actor ~scope ~authorize state =
  let current () =
    Operator_authorization.is_current actor
    && P.Principal.has_scope (Operator_authorization.principal actor) scope
  in
  let denied () =
    Error
      (P.Error.create
         Permission_denied
         ~message:"recurrence original authority expired, was revoked or lacks scope"
         ~retryable:false
         ())
  in
  if current ()
  then
    let open Result.Let_syntax in
    let%bind () = authorize state in
    if current () then Ok () else denied ()
  else denied ()
;;

let check_write actor ~authorize state = check actor ~scope:Send_messages ~authorize state

let check_read actor ~authorize state =
  check actor ~scope:View_session_transcript ~authorize state
;;

let check_current t =
  if Option.for_all t.current ~f:Recurrence_controller.is_current
  then Ok ()
  else
    Error
      (P.Error.create
         Conflict
         ~message:"recurrence lifecycle observation changed"
         ~retryable:false
         ())
;;

let check_goal_basis { action; binding; source } state ~now =
  let owner = F.Goal_binding.Capture.owner binding in
  let open Result.Let_syntax in
  let check_capture selection =
    let%bind actual = G.capture state ~owner ~source ~selection ~now in
    if F.Goal_binding.Capture.equal actual binding
    then Ok ()
    else
      Error
        (P.Error.create
           Conflict
           ~message:"recurrence Goal capture changed before commit"
           ~retryable:false
           ())
  in
  match action with
  | Enable -> G.check_current binding state ~owner ~source ~now
  | Edit request ->
    let%map _ = G.retain binding state ~owner ~source ~request ~now in
    ()
  | New request -> check_capture (G.Operator request)
  | Native_new observe ->
    let%bind actual = observe () in
    (match actual with
     | Some attribution -> check_capture (G.Native attribution)
     | None -> Error (P.Error.invalid_request "recurrence native Goal capture was lost"))
  | Native_edit observe ->
    let%bind actual = observe () in
    if
      Option.exists actual ~f:(fun attribution ->
        P.Id.Goal.equal
          (Goal_work_attribution.goal_id attribution)
          (F.Goal_binding.Capture.goal_id binding))
    then G.check_current binding state ~owner ~source ~now
    else
      Error
        (P.Error.invalid_request "recurrence native Edit must preserve its original Goal")
;;

let check_goal t state ~now =
  Option.value_map t.goal ~default:(Ok ()) ~f:(fun basis ->
    check_goal_basis basis state ~now)
;;

let prepare_goal state ~(request : C.t) ~catalog ~owner ~source ~now ~native =
  let open Result.Let_syntax in
  match request.mutation, source with
  | (C.Mutation.Pause _ | Retire _), _ | _, None -> Ok None
  | (Create _ | Edit _ | Enable _), Some source ->
    let existing =
      Option.value_map
        (Recurrence_catalog.find catalog (C.Mutation.id request.mutation))
        ~default:F.Goal_binding.Unbound
        ~f:(fun entry -> F.goal_binding (Recurrence_entry.current entry))
    in
    let retain action binding =
      let basis = { action; binding; source } in
      let%map () = check_goal_basis basis state ~now in
      Some basis
    in
    (match request.mutation, request.goal_owner, native, existing with
     | Create (_, capture), None, Some observe, _ ->
       (match Recurrence_target.view capture.target with
        | Existing_session _ -> Ok None
        | Fresh_child _ ->
          let%bind actual = observe () in
          (match actual with
           | None -> Ok None
           | Some attribution ->
             let%map binding =
               G.capture state ~owner ~source ~selection:(Native attribution) ~now
             in
             Some { action = Native_new observe; binding; source }))
     | Create _, Some requested, None, _ ->
       let%map binding =
         G.capture state ~owner ~source ~selection:(Operator requested) ~now
       in
       Some { action = New requested; binding; source }
     | Edit _, Some requested, None, Controller_goal binding ->
       retain (Edit requested) binding
     | Edit _, None, Some observe, Controller_goal binding ->
       retain (Native_edit observe) binding
     | Enable _, None, _, Controller_goal binding -> retain Enable binding
     | Edit _, (Some _ | None), _, _
       when Option.is_some request.goal_owner
            || not (F.Goal_binding.equal existing Unbound) ->
       Error
         (P.Error.invalid_request
            "recurrence Edit must preserve its original Goal binding")
     | _ -> Ok None)
;;

let retained_native_digest catalog ~principal_id (request : C.t) =
  let open Result.Let_syntax in
  let%bind binding =
    match
      ( request.mutation
      , Recurrence_catalog.find_receipt catalog ~principal_id ~key:request.key )
    with
    | (Create _ | Edit _), Some receipt ->
      let definition =
        Option.bind
          (Recurrence_catalog.find catalog receipt.recurrence_id)
          ~f:(fun entry ->
            List.find (Recurrence_entry.definitions entry) ~f:(fun definition ->
              Int64.equal
                receipt.definition_revision
                (Recurrence_identity.Definition.revision (F.key definition))))
      in
      let%map definition =
        Result.of_option
          definition
          ~error:(P.Error.invalid_request "acknowledged recurrence definition is missing")
      in
      F.goal_binding definition
    | _ -> Ok F.Goal_binding.Unbound
  in
  C.digest_native request ~binding
;;

let prepare
      ?native
      (state : Session_state.t)
      ~controller
      ~actor
      ~authorize
      ~(request : Recurrence_command.t)
      ~now
  =
  let open Result.Let_syntax in
  let%bind () = check_write actor ~authorize state in
  let session = Recurrence_controller.session controller in
  let%bind () =
    if
      P.Id.Session.equal state.identity.session_id (P.Session_ref.session_id session)
      && P.Id.Session.equal request.session_id state.identity.session_id
    then Ok ()
    else Error (P.Error.invalid_request "recurrence command targets another actor")
  in
  let catalog = Option.value state.recurrence_catalog ~default:Recurrence_catalog.empty in
  let principal_id = (Operator_authorization.principal actor).id in
  let%bind request_sha256 =
    match native with
    | None -> C.digest request
    | Some _ -> retained_native_digest catalog ~principal_id request
  in
  let%bind retained =
    Recurrence_catalog.receipt catalog ~principal_id ~key:request.key ~request_sha256
  in
  match retained with
  | Some receipt -> Ok { receipt; delta = None; current = None; goal = None }
  | None ->
    let%bind source, source_delta, current =
      if Recurrence_command.Mutation.needs_source request.mutation
      then (
        let%bind current = Recurrence_controller.observe controller state in
        let%bind () =
          if Recurrence_controller.automatic current
          then Ok ()
          else
            Error
              (P.Error.create
                 Invalid_state
                 ~message:"recurrence capture requires active automatic lifecycle"
                 ~retryable:false
                 ())
        in
        let%map index, source = Goal_catalog_source.capture state in
        let changed =
          match state.run_state with
          | None -> true
          | Some previous ->
            not
              (Run_source_installation.equal
                 (Run_state.installation previous)
                 (Run_state.installation index))
        in
        ( Some source
        , (if changed then [ Session_delta.Run_state_changed index ] else [])
        , Some current ))
      else Ok (None, [], None)
    in
    let%bind goal =
      prepare_goal state ~request ~catalog ~owner:session ~source ~now ~native
    in
    let goal_binding =
      Option.bind goal ~f:(fun { action; binding; _ } ->
        match action with
        | Enable -> None
        | New _ | Edit _ | Native_new _ | Native_edit _ -> Some binding)
    in
    let producer : Recurrence_command_transition.producer =
      if Option.is_some native then Native else Operator
    in
    let%map decision =
      Recurrence_command_transition.prepare
        ~producer
        ?goal_binding
        catalog
        ~request
        ~principal_id
        ~session
        ~generation:state.identity.generation
        ~current_source:source
        ~current_lifecycle_revision:(Option.map current ~f:Recurrence_controller.revision)
        ~now
    in
    (match decision with
     | Retained receipt -> { receipt; delta = None; current; goal = None }
     | Commit { catalog; receipt } ->
       { receipt
       ; delta =
           Some
             (Session_delta.Batch (source_delta @ [ Recurrence_catalog_changed catalog ]))
       ; current
       ; goal
       })
;;

let read (state : Session_state.t) ~controller ~actor ~authorize =
  let open Result.Let_syntax in
  let%bind () = check_read actor ~authorize state in
  let%bind current = Recurrence_controller.observe controller state in
  let%bind entries =
    List.map
      (Option.value_map
         state.recurrence_catalog
         ~default:[]
         ~f:Recurrence_catalog.entries)
      ~f:(fun entry ->
        let%map control =
          Recurrence_entry.effective_control
            entry
            ~current_revision:(Recurrence_controller.revision current)
            ~archived:(Recurrence_controller.archived current)
        in
        Recurrence_view.of_entry entry ~effective_control:control)
    |> Result.all
  in
  let%bind () = check_read actor ~authorize state in
  if Recurrence_controller.is_current current
  then Ok entries
  else
    Error
      (P.Error.create
         Conflict
         ~message:"recurrence read lifecycle observation changed"
         ~retryable:false
         ())
;;
