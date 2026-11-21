open! Core
module P = Agent_protocol
module Q = P.Recovery_query
module A = Agent_session
module Actor = A.Session_actor
module Ledger = A.Recovery_window_ledger

type t =
  { factory : Session_factory.t
  ; context : Connection_context.t
  ; actor : Operator_authorization.t
  ; negotiation : Connection_context.Authoring_negotiation.t
  ; registry : Session_registry.t
  ; server_id : P.Id.Server.t
  }

type release = unit -> (unit, P.Error.t) result

let error code message = P.Error.create code ~message ~retryable:false ()

let original_local_authority context actor =
  (not (Operator_authorization.is_durable_local actor))
  || Option.exists (Connection_context.original_actor context) ~f:(fun original ->
    Int.equal
      (Operator_authorization.origin original)
      (Operator_authorization.origin actor)
    && Operator_authorization.is_current original)
;;

let create factory ~context ~actor ~registry ~server_id =
  match factory, Connection_context.authoring_negotiation context with
  | Some factory, Some lease ->
    let actor =
      Operator_authorization.restrict actor ~is_current:(fun () ->
        Connection_context.current_authoring_negotiation context lease
        && original_local_authority context actor)
    in
    Ok { factory; context; actor; negotiation = lease; registry; server_id }
  | _ -> Error (error Method_not_found "this connection has no recovery window feature")
;;

let require_feature t feature =
  if
    Connection_context.Authoring_negotiation.selected t.negotiation feature
    && Operator_authorization.is_current t.actor
  then Ok ()
  else
    Error
      (error Method_not_found "this connection has no current requested recovery feature")
;;

let enable t (entry : Session_registry.entry) ~on_release (request : Q.Enable.t) =
  let open Result.Let_syntax in
  let%bind () = require_feature t Q.Features.windows in
  let principal = Operator_authorization.principal t.actor in
  let session_id = P.Session_ref.session_id request.session in
  let attachment_id = request.attachment_id in
  let authorize (state : A.Session_state.t) =
    let%bind () = Session_registry.check_native_metadata_publication t.registry entry in
    if
      not
        (Operator_authorization.is_current t.actor
         && P.Id.Session.equal state.identity.session_id session_id
         && Connection_context.owns_attachment t.context ~session_id ~attachment_id
         && P.Principal.has_scope principal View_session_transcript
         && Authorization.session_creator_visible_to
              principal
              ~creator:state.identity.creating_principal)
    then Error (error Permission_denied "recovery activation authority expired")
    else if state.identity.generation <> request.expected_generation
    then Error (error Conflict "recovery activation generation changed")
    else Ok ()
  in
  if not (P.Id.Server.equal (P.Session_ref.server_id request.session) t.server_id)
  then Error (error Invalid_request "recovery activation belongs to another host")
  else (
    let%bind scope =
      Actor.enable_recovery_windows entry.actor ~actor:t.actor ~attachment_id ~authorize
    in
    let%bind observation = Actor.observe_resource_state entry.actor in
    let check () =
      let%bind state =
        Actor.Resource_observation.check_recovery_window_writer
          observation
          ~actor:t.actor
          ~attachment_id
      in
      let%bind () = authorize state in
      if
        Option.exists state.recovery_windows ~f:(fun ledger ->
          P.Recovery_window.Scope.equal scope (Ledger.Window.scope (Ledger.active ledger)))
      then Ok ()
      else Error (error Conflict "recovery activation has changed")
    in
    let%bind () = check () in
    (* Optional source adoption cannot undo the already committed window.
       Keep the actual negotiated connection and writer live through this
       bounded host transfer and recheck them for the eventual response. *)
    let state = Actor.Resource_observation.state observation in
    Option.iter state.recovery_installed_originals ~f:(fun _ ->
      Eio.Switch.run (fun sw ->
        ignore
          (Session_factory.Recovery_assets.adopt_installed
             t.factory
             ~sw
             ~actor:t.actor
             ~attachment_id
             ~connection_current:(fun () ->
               Connection_context.owns_attachment t.context ~session_id ~attachment_id)
             scope
           : (unit, A.Recovery_source_reader.error) result)));
    let%map () = check () in
    on_release check;
    scope)
;;

let list t ~on_release (request : Q.List_windows.t) =
  let%bind.Result () = require_feature t Q.Features.windows in
  Session_factory.Recovery_history.list_windows
    t.factory
    ~on_release
    ~actor:t.actor
    ~session:request.session
    ?generation:request.generation
    ?state:request.state
    request.page
;;

let query t ~on_release (request : P.Recovery_history_request.t) =
  let open Result.Let_syntax in
  let%bind () = require_feature t P.Recovery_history_request.feature in
  let%bind query = A.Recovery_history_query.of_request request in
  let%bind json =
    Session_factory.Recovery_history.query
      t.factory
      ~access:(Actor.Recovery_history_access.operator t.actor)
      ?cursor:request.page.cursor
      ~on_release:(fun check ->
        on_release (fun () ->
          if check ()
          then Ok ()
          else Error (error Permission_denied "recovery history authority expired")))
      query
  in
  let%bind page = P.Recovery_history_page.of_json json in
  let%map () = P.Recovery_history_page.validate page ~request in
  page
;;

let read_text t ~on_release (request : P.Recovery_text_range.t) =
  let module Range = P.Recovery_text_range in
  let module History = Session_factory.Recovery_history in
  let open Result.Let_syntax in
  let%bind () = require_feature t Range.feature in
  let principal = Operator_authorization.principal t.actor in
  let selection =
    match request.preparation_revision with
    | None -> History.Text.Canonical request.citation
    | Some source_revision -> Prepared { citation = request.citation; source_revision }
  in
  let authorize _selection _view (state : A.Session_state.t) =
    let%bind () = require_feature t Range.feature in
    if P.Principal.has_scope principal View_session_transcript
       && Authorization.session_creator_visible_to principal ~creator:state.identity.creating_principal
    then Ok ()
    else Error (error Permission_denied "recovery text is unavailable")
  in
  History.read_text t.factory ~on_release ~authorize selection ~view:Ordinary
    ~part:request.part ~start:request.start ~stop:request.stop ~max_bytes:request.max_bytes ()
  |> Result.map ~f:Range.Reply.of_slice
  |> Result.map_error ~f:(function
    | History.Range error -> error
    | Source Denied -> error Permission_denied "recovery text is unavailable"
    | Source (Limit | Budget_exhausted) -> error Resource_limit "recovery text examination limit"
    | Source (Stale | Expired | Withdrawn) -> error Conflict "recovery text revision is no longer visible"
    | Source (Missing | Unavailable | Unsupported | Corrupt) -> error Interrupted "recovery text source is unavailable")
;;

let read_asset t ~on_release request =
  let module Wire = P.Recovery_asset_read in
  let module Assets = Session_factory.Recovery_assets in
  let open Result.Let_syntax in
  let%bind () = require_feature t Wire.feature in
  let principal = Operator_authorization.principal t.actor in
  let authorize (state : A.Session_state.t) =
    let%bind () = require_feature t Wire.feature in
    if
      P.Principal.has_scope principal View_session_transcript
      && Authorization.session_creator_visible_to
           principal
           ~creator:state.identity.creating_principal
    then Ok ()
    else Error (error Permission_denied "recovery asset is unavailable")
  in
  Eio.Switch.run (fun sw ->
    Assets.read_chunk t.factory ~sw ~authorize ~on_release request)
  |> Result.map_error ~f:(function
    | Assets.Request error -> error
    | Source Denied -> error Permission_denied "recovery asset is unavailable"
    | Source (Limit | Budget_exhausted) ->
      error Resource_limit "recovery asset examination limit"
    | Source (Stale | Expired | Withdrawn) ->
      error Conflict "recovery asset revision is no longer visible"
    | Source (Missing | Unavailable) ->
      error Blob_unavailable "recovery asset is unavailable"
    | Source Unsupported ->
      error Invalid_request "this part has no supported local original"
    | Source Corrupt -> error Interrupted "recovery asset could not be verified")
;;

module Control = P.Recovery_rollover_control

let authorize_control t session (state : A.Session_state.t) =
  let open Result.Let_syntax in
  let%bind () = require_feature t Control.feature in
  let principal = Operator_authorization.principal t.actor in
  if
    P.Id.Session.equal state.identity.session_id (P.Session_ref.session_id session)
    && P.Principal.has_scope principal View_session_transcript
    && Authorization.session_creator_visible_to
         principal
         ~creator:state.identity.creating_principal
  then Ok ()
  else Error (error Permission_denied "recovery status is unavailable")
;;

let authorize_control_writer t session attachment state =
  let open Result.Let_syntax in
  let%bind () = authorize_control t session state in
  let session_id = P.Session_ref.session_id session in
  if
    Option.exists attachment ~f:(fun attachment_id ->
      Connection_context.owns_attachment t.context ~session_id ~attachment_id)
  then Ok ()
  else Error (error Permission_denied "rollover requires the current connection writer")
;;

let authorize_control_force t _state =
  let principal = Operator_authorization.principal t.actor in
  if
    Operator_authorization.is_current t.actor
    && P.Principal.has_scope principal Administer_configuration
    && P.Principal.has_scope principal Send_messages
  then Ok ()
  else
    Error
      (error Permission_denied "rollover force requires current configuration authority")
;;

let inspect_rollover t ~on_release (request : Control.Inspect.t) =
  let open Result.Let_syntax in
  let%bind () = require_feature t Control.feature in
  let%bind () =
    if P.Id.Server.equal t.server_id (P.Session_ref.server_id request.session)
    then Ok ()
    else Error (error Invalid_request "recovery status belongs to another host")
  in
  Session_factory.Recovery_history.inspect_rollover
    t.factory
    ~on_release
    ~actor:t.actor
    ~authorize:(authorize_control t request.session)
    ~authorize_writer:(authorize_control_writer t request.session request.attachment_id)
    ~authorize_force:(authorize_control_force t)
    request
;;

let rollover t (entry : Session_registry.entry) ~on_release (request : Control.Request.t) =
  let open Result.Let_syntax in
  let%bind () = require_feature t Control.feature in
  let session = Control.Request.session request in
  let%bind () =
    if P.Id.Server.equal t.server_id (P.Session_ref.server_id session)
    then Ok ()
    else Error (error Invalid_request "rollover belongs to another host")
  in
  let authorize state =
    let%bind () = Session_registry.check_native_metadata_publication t.registry entry in
    authorize_control_writer t session (Some request.attachment_id) state
  in
  let authorize_force = authorize_control_force t in
  let%bind receipt =
    Actor.publish_context_rollover
      entry.actor
      ~actor:t.actor
      ~authorize
      ~authorize_force
      request
  in
  let%bind observation = Actor.observe_resource_state entry.actor in
  let check () =
    let%bind state =
      Actor.Resource_observation.check_recovery_window_writer
        observation
        ~actor:t.actor
        ~attachment_id:request.attachment_id
    in
    let%bind () = authorize state in
    if request.force then authorize_force state else Ok ()
  in
  let%map () = check () in
  on_release check;
  receipt
;;
