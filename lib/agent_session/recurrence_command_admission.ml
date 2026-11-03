open! Core
module P = Agent_protocol

type t =
  { receipt : Recurrence_command_receipt.t
  ; delta : Session_delta.t option
  ; current : Recurrence_controller.observation option
  }

let check actor ~scope ~authorize state =
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope (Operator_authorization.principal actor) scope
  then authorize state
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"recurrence original authority expired, was revoked or lacks scope"
         ~retryable:false
         ())
;;

let check_write actor ~authorize state = check actor ~scope:Send_messages ~authorize state

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

let prepare
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
  let%bind request_sha256 = Recurrence_command.digest request in
  let%bind retained =
    Recurrence_catalog.receipt catalog ~principal_id ~key:request.key ~request_sha256
  in
  match retained with
  | Some receipt -> Ok { receipt; delta = None; current = None }
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
    let%map decision =
      Recurrence_command_transition.prepare
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
     | Retained receipt -> { receipt; delta = None; current }
     | Commit { catalog; receipt } ->
       { receipt
       ; delta =
           Some
             (Session_delta.Batch (source_delta @ [ Recurrence_catalog_changed catalog ]))
       ; current
       })
;;

let read (state : Session_state.t) ~controller ~actor ~authorize =
  let open Result.Let_syntax in
  let%bind () = check actor ~scope:View_session_transcript ~authorize state in
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
