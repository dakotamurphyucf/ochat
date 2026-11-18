open! Core
module P = Agent_protocol
module W = P.Recurrence_control
module A = Agent_session
module Target = Recurrence_operator_target
module Actor = A.Session_actor
module Catalog = A.Recurrence_catalog

type t =
  { factory : Session_factory.t
  ; registry : Session_registry.t
  ; server_id : P.Id.Server.t
  ; now : unit -> P.Timestamp.t
  ; authorize_write :
      Operator_authorization.t -> A.Session_state.t -> (unit, P.Error.t) result
  }

let create ~registry ~factory ~server_id ~now ~authorize_write =
  { registry; factory; server_id; now; authorize_write }
;;

let denied () =
  Error
    (P.Error.create
       Permission_denied
       ~message:"recurrence writer is unavailable"
       ~retryable:false
       ())
;;

let authorize t actor state =
  let p = Operator_authorization.principal actor in
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope p Send_messages
    && P.Principal.has_scope p View_session_transcript
    && Authorization.session_visible_to p (A.Session_state.summary state)
  then t.authorize_write actor state
  else denied ()
;;

let select_target t ~entry ~session ~actor ~authorize ~catalog (action : W.Action.t) =
  let open Result.Let_syntax in
  match action with
  | Create { capture; _ } | Edit { capture; _ } ->
    let%map selected =
      Target.capture
        t.factory
        ~controller:entry
        ~session
        ~actor
        ~authorize
        capture.selector
    in
    Some (Target.value selected), Some selected
  | Enable { id; _ } ->
    let%bind row =
      Result.of_option
        (Catalog.find catalog id)
        ~error:
          (P.Error.create
             Conflict
             ~message:"recurrence definition is missing"
             ~retryable:false
             ())
    in
    let frozen = A.Recurrence_definition.target (A.Recurrence_entry.current row) in
    let%map selected =
      Target.check_frozen t.factory ~controller:entry ~session ~actor ~authorize frozen
    in
    None, Some selected
  | Pause _ | Retire _ -> Ok (None, None)
;;

let writer t ~actor ~context (request : W.Request.t) =
  let open Result.Let_syntax in
  let%bind actor = Connection_context.request_actor context (Some actor) in
  let session = request.session in
  let session_id = P.Session_ref.session_id session in
  let writer = request.attachment_id in
  let owns () =
    Connection_context.owns_attachment context ~session_id ~attachment_id:writer
  in
  let%bind () =
    if P.Id.Server.equal t.server_id (P.Session_ref.server_id session) && owns ()
    then Ok ()
    else denied ()
  in
  let%bind entry =
    match Session_registry.find t.registry session_id with
    | Some e -> Ok e
    | None -> denied ()
  in
  let check state =
    let%bind () = Session_factory.check_registered_owner t.factory entry ~session in
    let%bind _ = A.Writer_attachment.require state ~attachment_id:writer ~now:t.now in
    let%bind () = authorize t actor state in
    if owns () && Operator_authorization.is_current actor then Ok () else denied ()
  in
  Ok (actor, entry, check)
;;

let check_disclosure t ~actor ~context request =
  let result =
    let open Result.Let_syntax in
    let%bind _, entry, check = writer t ~actor ~context request in
    let%bind state = Actor.state entry.actor in
    check state
  in
  result
  |> Result.map_error ~f:(fun (failure : P.Error.t) ->
    P.Error.create
      failure.code
      ~message:"Recurrence receipt requires a current writer"
      ~retryable:failure.retryable
      ())
;;

let execute_inner t ~actor ~context (request : W.Request.t) =
  let open Result.Let_syntax in
  let%bind actor, entry, check = writer t ~actor ~context request in
  let session = request.session in
  let intent = request.intent in
  let principal_id = (Operator_authorization.principal actor).id in
  let%bind state = Actor.state entry.actor in
  let%bind () = check state in
  let catalog = Option.value state.recurrence_catalog ~default:Catalog.empty in
  let%bind request_sha256 = W.Intent.digest intent ~session in
  let%bind retained =
    Catalog.receipt catalog ~principal_id ~key:intent.key ~request_sha256
  in
  let%bind receipt =
    match retained with
    | Some receipt -> Ok receipt
    | None ->
      let%bind () =
        if
          Int.equal state.identity.generation intent.generation
          && Int64.equal (Catalog.revision catalog) intent.expected_catalog_revision
          && Option.equal
               Int64.equal
               (W.Action.expected_control_revision intent.action)
               (Option.map
                  (Catalog.find catalog (W.Action.id intent.action))
                  ~f:A.Recurrence_entry.control_revision)
        then Ok ()
        else
          Error
            (P.Error.create
               Conflict
               ~message:"recurrence basis changed"
               ~retryable:false
               ())
      in
      let%bind target, selected =
        match intent.action with
        | Pause _ | Retire _ -> Ok (None, None)
        | Create { basis; _ } | Edit { basis; _ } | Enable { basis; _ } ->
          let%bind controller = Actor.observe_resource_state entry.actor in
          (* Reject a stale original before acquiring an indexed target. The
             actor repeats source, lifecycle and revision checks for the write. *)
          let%bind query =
            Actor.query_recurrences entry.actor ~actor ~authorize:check ~request:List
          in
          let observed = A.Recurrence_query.basis query in
          let%bind () =
            if
              observed.automatic
              && Int.equal intent.generation observed.generation
              && Int64.equal intent.expected_catalog_revision observed.catalog_revision
              && Option.equal P.Run_source.equal (Some basis.source) observed.source
              && P.Session_lifecycle.Revision.equal
                   basis.expected_lifecycle_revision
                   observed.lifecycle_revision
            then Ok ()
            else
              Error
                (P.Error.create
                   Conflict
                   ~message:"recurrence capture basis changed"
                   ~retryable:false
                   ())
          in
          let authorize target_state =
            let%bind current = Actor.Resource_observation.check_current controller in
            let%bind () = check current in
            let%bind () = A.Recurrence_query.check_current query in
            authorize t actor target_state
          in
          select_target t ~entry ~session ~actor ~authorize ~catalog intent.action
      in
      let%bind command = A.Recurrence_command.of_shared_intent ~session intent ~target in
      let check_at_writer state =
        let%bind () =
          Option.value_map selected ~default:(Ok ()) ~f:Target.check_current
        in
        check state
      in
      Actor.recurrence_command
        entry.actor
        ~actor
        ~authorize:check_at_writer
        ~request:command
  in
  (* The journal write can yield. Recheck the actual current writer before
     disclosing the receipt; an exact reconnect can recover it afterwards. *)
  let%bind current = Actor.state entry.actor in
  let%bind () = check current in
  let verb : W.Verb.t =
    match receipt.kind with
    | Create -> Create
    | Edit -> Edit
    | Enable -> Enable
    | Pause -> Pause
    | Retire -> Retire
  in
  let%bind response =
    W.Receipt.create
      ~session
      ~generation:intent.generation
      ~principal_id:receipt.principal_id
      ~key:receipt.key
      ~intent_sha256:receipt.request_sha256
      ~recurrence_id:receipt.recurrence_id
      ~verb
      ~definition_revision:receipt.definition_revision
      ~control_revision:receipt.control_revision
      ~catalog_revision:receipt.catalog_revision
      ~accepted_at:receipt.accepted_at
  in
  let%map () = W.Receipt.check_intent response ~session ~principal_id ~intent in
  response
;;

let execute t ~actor ~context request =
  execute_inner t ~actor ~context request
  |> Result.map_error ~f:(fun (e : P.Error.t) ->
    let message =
      match e.code with
      | Permission_denied | Lease_stale ->
        "Recurrence control requires a current session writer"
      | Conflict | Idempotency_conflict ->
        "Recurrence command conflicts with its retained key or the current selection"
      | Persistence_error -> "Recurrence command acknowledgement is unavailable"
      | _ -> "Recurrence control is unavailable for this request"
    in
    P.Error.create e.code ~message ~retryable:e.retryable ())
;;
