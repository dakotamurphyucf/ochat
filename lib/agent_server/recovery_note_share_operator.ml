open! Core
module P = Agent_protocol
module W = P.Recovery_note_share_control
module S = Agent_session.Recovery_note_share
module Catalog = Recovery_notes.Catalog
module Shares = Session_factory.Recovery_note_shares

let unavailable () =
  P.Error.create
    Permission_denied
    ~message:"recovery note source management is unavailable"
    ~retryable:false
    ()
;;

let no_feature () =
  P.Error.create
    Method_not_found
    ~message:"this connection has no current recovery note share management feature"
    ~retryable:false
    ()
;;

let dispatch factory ~context ~actor ~registry ~server_id ~on_release request =
  let open Result.Let_syntax in
  let%bind factory, original, negotiation =
    match
      ( factory
      , Connection_context.original_actor context
      , Connection_context.authoring_negotiation context )
    with
    | Some factory, Some original, Some negotiation
      when Connection_context.Authoring_negotiation.selected negotiation W.feature ->
      Ok (factory, original, negotiation)
    | _ -> Error (no_feature ())
  in
  let actor =
    Operator_authorization.restrict actor ~is_current:(fun () ->
      Operator_authorization.is_current original
      && Connection_context.current_authoring_negotiation context negotiation)
  in
  let { W.Source.session; attachment_id; expected_generation } =
    W.Request.source request
  in
  let principal = Operator_authorization.principal actor in
  let session_id = P.Session_ref.session_id session in
  let current () =
    Operator_authorization.is_current actor
    && P.Principal.has_scope principal View_session_transcript
    && P.Principal.has_scope principal Manage_grants
    && Connection_context.owns_attachment context ~session_id ~attachment_id
  in
  let%bind entry =
    if P.Id.Server.equal (P.Session_ref.server_id session) server_id && current ()
    then
      Result.of_option (Session_registry.find registry session_id) ~error:(unavailable ())
    else Error (unavailable ())
  in
  let authorize (state : Agent_session.Session_state.t) =
    if
      P.Id.Session.equal session_id state.identity.session_id
      && current ()
      && Authorization.session_creator_visible_to
           principal
           ~creator:state.identity.creating_principal
    then Ok ()
    else Error (unavailable ())
  in
  let access =
    Agent_session.Recovery_note_access.operator
      actor
      ~owner:session
      ~generation:expected_generation
      ~attachment_id:(Some attachment_id)
      ~authorize
  in
  let check () = Shares.check_management_now factory ~source:entry ~access in
  let%bind () = check () in
  let receipt value =
    W.Receipt.create
      (S.Receipt.reference value)
      ~share_revision:(S.Revision.to_int64 (S.Receipt.revision value))
  in
  let%bind response =
    match request with
    | Inspect { path; _ } ->
      let%map metadata = Shares.inspect_management factory ~source:entry ~access path in
      W.Response.Inspected metadata
    | Share { expected; selection; recipient; expires_at; key; _ } ->
      let%bind expected_revision = S.Revision.of_int64 expected.share_revision in
      let%bind expected_catalog_revision =
        Catalog.Catalog_revision.of_int64 expected.catalog_revision
      in
      let%bind id = Catalog.Note_id.of_int64 selection.id in
      let%bind revision = Catalog.Note_revision.of_int64 selection.revision in
      let%bind value =
        Shares.create
          factory
          ~source:entry
          ~access
          ~recipient
          ~key
          { expected_revision
          ; expected_catalog_revision
          ; selection = { id; revision }
          ; expires_at
          }
      in
      let%map receipt = receipt value in
      W.Response.Shared receipt
    | Revoke { expected_share_revision; reference; key; _ } ->
      let%bind expected_revision = S.Revision.of_int64 expected_share_revision in
      if P.Session_ref.equal session (W.Reference.source reference)
      then (
        let%bind value =
          Shares.revoke
            factory
            ~source:entry
            ~access
            ~key
            { expected_revision; reference }
        in
        let%map receipt = receipt value in
        W.Response.Revoked receipt)
      else Error (unavailable ())
  in
  let%bind () = W.Response.validate response ~request in
  let%map () = check () in
  on_release check;
  response
;;
