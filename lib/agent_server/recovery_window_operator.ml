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
  ; registry : Session_registry.t
  ; server_id : P.Id.Server.t
  ; now : unit -> P.Timestamp.t
  }

type release = unit -> (unit, P.Error.t) result

let error code message = P.Error.create code ~message ~retryable:false ()

let create factory ~context ~actor ~registry ~server_id ~now =
  match factory, Connection_context.authoring_negotiation context with
  | Some factory, Some lease
    when Connection_context.Authoring_negotiation.selected lease Q.Features.windows ->
    let actor =
      Operator_authorization.restrict actor ~is_current:(fun () ->
        Connection_context.current_authoring_negotiation context lease)
    in
    Ok { factory; context; actor; registry; server_id; now }
  | _ -> Error (error Method_not_found "this connection has no recovery window feature")
;;

let enable t (entry : Session_registry.entry) ~on_release (request : Q.Enable.t) =
  let open Result.Let_syntax in
  let principal = Operator_authorization.principal t.actor in
  let session_id = P.Session_ref.session_id request.session in
  let attachment_id = request.attachment_id in
  let authorize (state : A.Session_state.t) =
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
      let%bind () = Session_registry.check_native_metadata_publication t.registry entry in
      let%bind state = Actor.Resource_observation.check_current observation in
      let%bind () = authorize state in
      let%bind () =
        A.Writer_attachment.require_recovery_owner
          state
          ~principal:principal.id
          ~attachment_id
          ~now:t.now
      in
      if
        Option.exists state.recovery_windows ~f:(fun ledger ->
          P.Recovery_window.Scope.equal scope (Ledger.Window.scope (Ledger.active ledger)))
      then Ok ()
      else Error (error Conflict "recovery activation has changed")
    in
    let%map () = check () in
    on_release check;
    scope)
;;

let list t ~on_release (request : Q.List_windows.t) =
  Session_factory.Recovery_history.list_windows
    t.factory
    ~on_release
    ~actor:t.actor
    ~session:request.session
    ?generation:request.generation
    ?state:request.state
    request.page
;;
