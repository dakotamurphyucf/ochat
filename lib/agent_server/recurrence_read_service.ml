open! Core
module A = Agent_session
module P = Agent_protocol
module Q = P.Recurrence_query
module Registry = Session_registry
module Read = Registry.Recurrence_read
module Catalog = A.Recurrence_catalog
module Entry = A.Recurrence_entry
module Query = A.Recurrence_query

type t =
  { registry : Registry.t
  ; factory : Session_factory.t
  ; server_id : P.Id.Server.t
  ; execution_enabled : bool
  ; authorize_write :
      Operator_authorization.t -> A.Session_state.t -> (unit, P.Error.t) result
  }

let create ~registry ~factory ~server_id ~execution_enabled ~authorize_write =
  { registry; factory; server_id; execution_enabled; authorize_write }
;;

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())
let conflict message = Error (P.Error.create Conflict ~message ~retryable:true ())

let authorize_actor actor =
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope
         (Operator_authorization.principal actor)
         View_session_transcript
  then Ok ()
  else denied "recurrence inspection requires current transcript visibility"
;;

let authorize_session actor summary =
  let open Result.Let_syntax in
  let%bind () = authorize_actor actor in
  if Authorization.session_visible_to (Operator_authorization.principal actor) summary
  then Ok ()
  else denied "recurrence session is not visible to this principal"
;;

let local_session t session =
  if P.Id.Server.equal (P.Session_ref.server_id session) t.server_id
  then Ok (P.Session_ref.session_id session)
  else Error (P.Error.invalid_request "recurrence reference belongs to another host")
;;

let selector : Q.Request.selector -> Query.request = function
  | List -> List
  | Get id -> Get id
  | Status id -> Status id
  | History id -> History id
;;

let unavailable (request : Q.Request.t) : Q.Result.t =
  match request.selector with
  | List -> List (Unavailable request.session)
  | Get _ -> Get (Unavailable request.session)
  | Status _ -> Status (Unavailable request.session)
  | History _ -> History (Unavailable request.session)
;;

let availability (state : A.Session_state.t) ~selected : Q.Availability.t =
  match state.lifecycle.observed with
  | Stopped -> Stopped
  | Failed _ -> Unavailable
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping -> if selected then Active else Unavailable
;;

let can_write t ~actor ~context ~at (request : Q.Request.t) read =
  let principal = Operator_authorization.principal actor in
  let state = Read.state read in
  let session_id = P.Session_ref.session_id request.session in
  match Read.origin read with
  | Indexed -> false
  | Selected | Loaded_unselected ->
    t.execution_enabled
    && P.Principal.has_scope principal Send_messages
    && (not (Registry.lifecycle_reserved t.registry session_id))
    && Option.exists request.attachment_id ~f:(fun attachment_id ->
      Connection_context.owns_attachment context ~session_id ~attachment_id
      && Result.is_ok
           (A.Writer_attachment.require state ~now:(fun () -> at) ~attachment_id)
      && Result.is_ok (t.authorize_write actor state))
;;

let actions catalog (basis : Query.Basis.t) ~can_capture (row : Q.Definition.t) =
  let open P.Recurrence_control.Verb in
  match Catalog.find catalog row.id with
  | None -> []
  | Some _ when Int64.equal row.control_revision Int64.max_value -> []
  | Some entry ->
    let edit =
      if
        can_capture
        && Int64.(row.definition_revision < max_value)
        && Entry.has_definition_slot entry
      then [ Edit ]
      else []
    in
    let retirement = [ Retire ] in
    (match row.control with
     | Retired _ -> []
     | Enabled -> edit @ (Pause :: retirement)
     | Paused Target_review -> edit @ retirement
     | Paused (Operator_pause | Controller_stop | Source_review | Recovery_review) ->
       let enable =
         if
           can_capture
           && Option.exists basis.source ~f:(P.Run_source.equal row.source)
           && Option.exists
                (Result.ok (Entry.enable entry))
                ~f:(fun enabled -> Entry.equal_control (Entry.control enabled) Enabled)
         then [ Enable ]
         else []
       in
       edit @ enable @ retirement)
;;

let advise read result =
  let state = Read.state read in
  let basis = Query.snapshot_basis (Read.snapshot read) in
  let catalog = Option.value state.recurrence_catalog ~default:Catalog.empty in
  if not (Catalog.has_command_slot catalog)
  then result
  else (
    let capture_current entry =
      match Entry.control entry with
      | Retired _ -> true
      | Enabled | Paused _ ->
        P.Session_lifecycle.Revision.equal
          (A.Recurrence_definition.birth_lifecycle_revision (Entry.current entry))
          basis.lifecycle_revision
    in
    let can_capture =
      basis.automatic
      && Option.is_some basis.source
      && List.for_all (Catalog.entries catalog) ~f:capture_current
    in
    let can_create = can_capture && Catalog.has_entry_slot catalog in
    let definition (row : Q.Definition.t) =
      { row with actions = actions catalog basis ~can_capture row }
    in
    let view f = function
      | Q.Result.Available view ->
        Q.Result.Available { view with can_create; value = f view.value }
      | Unavailable session -> Unavailable session
    in
    match result with
    | Q.Result.List rows -> Q.Result.List (view (List.map ~f:definition) rows)
    | Get row -> Get (view definition row)
    | Status status ->
      Status
        (view
           (fun (status : Q.Status.t) -> { status with entry = definition status.entry })
           status)
    | History history ->
      History
        (view
           (fun (history : Q.History.t) ->
              { history with entry = definition history.entry })
           history))
;;

let inspect t ~actor ~context ~now (request : Q.Request.t) =
  let open Result.Let_syntax in
  let%bind actor = Connection_context.request_actor context (Some actor) in
  let%bind () = authorize_actor actor in
  let%bind session_id = local_session t request.session in
  let controller_authorize = authorize_session actor in
  let read () =
    let request = selector request.selector in
    Registry.read_recurrences
      t.registry
      ~actor
      ~authorize:controller_authorize
      ~read_indexed:
        (Session_factory.read_recurrence_snapshot
           t.factory
           ~actor
           ~authorize:(fun state -> controller_authorize (A.Session_state.summary state))
           ~request)
      ~request
      session_id
  in
  let check_original () =
    let%bind actor = Connection_context.request_actor context (Some actor) in
    authorize_actor actor
  in
  match read () with
  | Error { code = Session_not_found; _ } ->
    let%map () = check_original () in
    unavailable request
  | Error failure -> Error failure
  | Ok observed ->
    let%bind () = Registry.check_recurrence_read t.registry observed in
    let selected =
      match Read.origin observed with
      | Selected -> t.execution_enabled
      | Loaded_unselected | Indexed -> false
    in
    let read_target target =
      let%bind target_id = local_session t target in
      let%bind () = check_original () in
      Registry.read_state t.registry ~authorize:(authorize_session actor) target_id
    in
    let principal = Operator_authorization.principal actor in
    let%bind result =
      Query.render_public
        (Read.snapshot observed)
        ~availability:(availability (Read.state observed) ~selected)
        ~selected
        ~read_target
        ~can_view_run:(Run_read_service.can_view principal)
    in
    let%bind current =
      match read () with
      | Error { code = Session_not_found; _ } ->
        conflict "recurrence controller disappeared during target read"
      | result -> result
    in
    let at = now () in
    let%bind () = check_original () in
    let%bind () = controller_authorize (A.Session_state.summary (Read.state current)) in
    let%bind () =
      if Read.same_basis observed current
      then Registry.check_recurrence_read t.registry current
      else conflict "recurrence controller changed during target read"
    in
    let result =
      if can_write t ~actor ~context ~at request current
      then advise current result
      else result
    in
    let%bind () = check_original () in
    let%bind () = controller_authorize (A.Session_state.summary (Read.state current)) in
    let%map () = Registry.check_recurrence_read t.registry current in
    result
;;

let query t ~actor ~context ~now request =
  inspect t ~actor ~context ~now request
  |> Result.map_error ~f:(fun (failure : P.Error.t) ->
    let message =
      match failure.code with
      | Permission_denied -> "Recurrence inspection requires current session visibility"
      | Conflict -> "Recurrence state changed during inspection"
      | Resource_limit -> "Recurrence inspection exceeds a host limit"
      | _ -> "Recurrence inspection is unavailable for this request"
    in
    P.Error.create failure.code ~message ~retryable:failure.retryable ())
;;
