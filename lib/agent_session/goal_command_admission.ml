open! Core
module P = Agent_protocol

type t =
  { receipt : Goal_command_receipt.t
  ; delta : Session_delta.t option
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
         ~message:"goal command original authority expired, was revoked or lacks scope"
         ~retryable:false
         ())
;;

let check_write actor ~authorize state = check actor ~scope:Send_messages ~authorize state

let read (state : Session_state.t) ~actor ~authorize =
  let%map.Result () = check actor ~scope:View_session_transcript ~authorize state in
  Option.value_map state.goal_catalog ~default:[] ~f:Goal_catalog.goals
;;

let prepare
      (state : Session_state.t)
      ~actor
      ~authorize
      ~session
      ~(request : Goal_command.t)
      ~now
  =
  let open Result.Let_syntax in
  let%bind () = check_write actor ~authorize state in
  let%bind () =
    if
      P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
      && P.Id.Session.equal request.session_id state.identity.session_id
    then Ok ()
    else Error (P.Error.invalid_request "goal command targets another actor")
  in
  let catalog = Option.value state.goal_catalog ~default:Goal_catalog.empty in
  let principal_id = (Operator_authorization.principal actor).id in
  let%bind request_sha256 = Goal_command.digest request in
  let%bind retained =
    Goal_catalog.receipt catalog ~principal_id ~key:request.key ~request_sha256
  in
  match retained with
  | Some receipt -> Ok { receipt; delta = None }
  | None ->
    let%bind source, source_delta =
      if Goal_command.Mutation.needs_source request.mutation
      then (
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
        Some source, if changed then [ Session_delta.Run_state_changed index ] else [])
      else Ok (None, [])
    in
    let%map decision =
      Goal_command_transition.prepare
        catalog
        ~request
        ~principal_id
        ~session
        ~generation:state.identity.generation
        ~current_source:source
        ~now
    in
    (match decision with
     | Retained receipt -> { receipt; delta = None }
     | Commit { catalog; receipt } ->
       { receipt
       ; delta =
           Some (Session_delta.Batch (source_delta @ [ Goal_catalog_changed catalog ]))
       })
;;
