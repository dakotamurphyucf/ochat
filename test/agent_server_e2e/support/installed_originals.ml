open Core
module P = Agent_protocol
module S = Agent_store
module Catalog = Agent_session.Prompt_catalog
module Revision = Agent_session.Prompt_revision
module Daemon = Agent_server.Daemon
module Content = Chatmd_prompt_content
module Originals = S.Installed_document_originals
module CM = Prompt.Chat_markdown

let require condition message = if not condition then failwith message
let sha256 bytes = Digestif.SHA256.(digest_string bytes |> to_hex)

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "installed original authority failure", (error : P.Error.t)]
;;

let ok = function
  | Ok value -> value
  | Error _ -> failwith "installed original fixture rejected"
;;

let first = "<p>same rendered</p><!-- unique private original A -->"
let second = "<p>same rendered</p><!-- unique private original B -->"
let limit = 4096

let fixture env environment =
  let port =
    Eio.Switch.run (fun sw ->
      let reservation = Port_reservation.create ~sw ~env in
      let port = Port_reservation.port reservation in
      Port_reservation.release reservation;
      port)
  in
  Config_fixture.create environment ~name:"installed-originals" ~http_port:port
;;

let save env physical_path bytes =
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / physical_path)
    bytes
;;

let configure env fixture ~retain =
  let config = Config_fixture.configuration fixture () in
  let config =
    if retain
    then
      String.substr_replace_all
        config
        ~pattern:"(permission_profile unattended)"
        ~with_:"(permission_profile unattended) (document_originals retain)"
    else config
  in
  save env (Config_fixture.config_path fixture) config
;;

let selected catalog =
  match Catalog.entries catalog with
  | [ { availability = Ready revision; _ } ] -> revision
  | [ { availability = Unavailable diagnostics; _ } ] ->
    raise_s
      [%sexp
        "installed prompt unavailable"
      , (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
  | _ -> failwith "expected one installed prompt"
;;

let rendered revision =
  List.concat_map (Revision.elements revision) ~f:(function
    | CM.Developer { content = Some (Items items); _ } ->
      List.map items ~f:(function
        | Basic { text = Some text; document_url = None; _ } -> text
        | _ -> failwith "expected captured literal")
    | _ -> [])
;;

let keys revision ~limits =
  let parsed = Option.value_exn (Revision.parser_context revision) in
  let content = Option.value_exn (Revision.content revision) in
  List.map (CM.Parsed.document_occurrences parsed) ~f:(fun occurrence ->
    Content.original_key content ~parsed ~occurrence ~limits ~checkpoint:ignore |> ok)
;;

let listing ~sw env fixture =
  let token = Config_fixture.admin_token fixture in
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some token)
    |> ok
  in
  let _ = Http_driver.initialize client |> protocol_ok in
  let page = P.Page.Request.create ~limit:10 () |> protocol_ok in
  let response =
    Http_driver.request
      client
      (Prompt_list { page; enabled = Some true; available = Some true })
    |> protocol_ok
  in
  let body = P.Public.Result.to_json response.result |> Jsonaf.to_string in
  require
    (not
       (List.exists
          [ "unique private original"; "originals_sha256"; "document_originals" ]
          ~f:(fun substring -> String.is_substring body ~substring)))
    "public prompt response exposed private custody";
  let revision =
    match Public_view.non_history response.result with
    | Prompt_list { items = [ prompt ]; _ } -> Option.value_exn prompt.current_revision
    | _ -> failwith "expected one public prompt"
  in
  let response =
    Http_driver.request_raw
      client
      ~headers:[ "Authorization", "Bearer " ^ token ]
      ~meth:`GET
      ~path:("/v1/blobs/blb_" ^ sha256 first)
      ()
    |> ok
  in
  require
    (response.status >= 400 && not (String.is_substring response.body ~substring:first))
    "generic blob read exposed a private installed original";
  Http_driver.shutdown client;
  revision
;;

let authorize daemon fixture =
  let actor =
    Daemon.authenticate_http_actor
      daemon
      { client_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0); headers = [] }
      (Some (Config_fixture.admin_token fixture))
    |> protocol_ok
  in
  fun () ->
    let module A = Operator_authorization in
    if
      A.is_current actor
      && P.Principal.has_scope (A.principal actor) Administer_configuration
    then Ok ()
    else
      Error
        (P.Error.create
           Permission_denied
           ~message:"fixture authority expired"
           ~retryable:false
           ())
;;

let with_original ?admit catalog revision occurrence ~authorize ~f =
  Catalog.with_installed_original
    ?admit
    catalog
    ~definition:(Revision.definition revision).id
    ~revision:(Revision.id revision)
    ~occurrence
    ~max_bytes:limit
    ~authorize
    ~checkpoint:ignore
    ~f
;;

let artifact_root env fixture =
  let root =
    S.Data_root.open_existing ~env ~path:(Config_fixture.data_dir fixture) |> ok
  in
  S.Data_root.prompt_artifacts_path root
;;

let installed_path env fixture revision =
  Eio.Path.(
    Eio.Stdenv.fs env
    / artifact_root env fixture
    / P.Id.Prompt_revision.to_string revision)
;;

let assert_absent catalog revision occurrence ~authorize =
  require
    (Result.is_error
       (with_original catalog revision occurrence ~authorize ~f:(fun ~check:_ _ _ ->
          failwith "unavailable original reached borrower")))
    "unavailable original was accepted"
;;

let run env environment =
  let fixture = fixture env environment in
  let prompt = Config_fixture.prompt_path fixture in
  let local name = Filename.concat (Filename.dirname prompt) name in
  save
    env
    prompt
    {|<developer><doc src="a.html" local markdown capture/><doc src="b.html" local markdown capture/></developer>|};
  save env (local "a.html") first;
  save env (local "b.html") second;
  let options = Daemon_host.with_offline_inference Daemon.default_options in
  let limits = Option.value_exn options.content_limits in
  let ordinary_result = ref None in
  Daemon_host.with_ env fixture ~options (fun _ daemon ->
    let catalog = Daemon.prompts daemon in
    let revision = selected catalog in
    require
      (Option.is_none (Revision.artifact revision).originals_sha256)
      "default capture retained originals";
    assert_absent
      catalog
      revision
      (List.hd_exn (keys revision ~limits))
      ~authorize:(authorize daemon fixture);
    let text = rendered revision in
    require
      (match text with
       | [ a; b ] -> String.equal a b && String.is_substring a ~substring:"same rendered"
       | _ -> false)
      "fixture originals did not produce identical rendered parts";
    ordinary_result := Some (Revision.id revision, text));
  let ordinary_id, ordinary_text = Option.value_exn !ordinary_result in
  configure env fixture ~retain:true;
  let installed =
    Eio.Switch.run (fun sw ->
      let process =
        Daemon_process.start
          ~sw
          ~env
          ~fixture
          ~config_path:(Config_fixture.config_path fixture)
      in
      let _ = Daemon_process.wait_ready process ~env ~timeout_seconds:15. |> ok in
      let revision = listing ~sw env fixture in
      let stopped = Daemon_process.stop process ~env ~grace_seconds:5. in
      require (not stopped.forced) "installation daemon did not stop cleanly";
      revision)
  in
  require
    (not (P.Id.Prompt_revision.equal ordinary_id installed))
    "opted-in custody reused the rendered-only revision";
  let installed_dir = installed_path env fixture installed in
  let payload_a = Eio.Path.(installed_dir / "originals" / sha256 first) in
  Daemon_host.with_ env fixture ~options (fun _ daemon ->
    let catalog = Daemon.prompts daemon in
    let revision = selected catalog in
    require
      (P.Id.Prompt_revision.equal (Revision.id revision) installed)
      "restart selected a different immutable publication";
    require
      (List.equal String.equal (rendered revision) ordinary_text)
      "opt-in changed rendered content";
    let left, right =
      match keys revision ~limits with
      | [ left; right ] -> left, right
      | _ -> failwith "expected two occurrence keys"
    in
    let authorize = authorize daemon fixture in
    save env (local "a.html") "unrelated later local bytes";
    let verify occurrence expected =
      let retired_check =
        with_original
          catalog
          revision
          occurrence
          ~authorize
          ~f:(fun ~check entry contents ->
            require
              (String.equal contents expected)
              "original came from another occurrence or live path";
            require
              (String.equal (Originals.Entry.media_type entry) "text/html")
              "original media type was lost";
            require
              (Int.equal (Originals.Entry.byte_length entry) (String.length expected))
              "original length differs";
            check () |> protocol_ok;
            Ok check)
        |> protocol_ok
      in
      require
        (Result.is_error (retired_check ()))
        "borrowed writer check survived lexical custody"
    in
    verify left first;
    verify right second;
    Eio.Path.unlink payload_a;
    let bounded =
      with_original
        ~admit:(fun entry ->
          require
            (Int.equal (Originals.Entry.byte_length entry) (String.length first))
            "the allowance did not get the admitted entry";
          false)
        catalog
        revision
        left
        ~authorize
        ~f:(fun ~check:_ _ _ -> failwith "body loaded after aggregate denial")
    in
    require
      (match bounded with
       | Error { P.Error.code = Resource_limit; _ } -> true
       | _ -> false)
      "aggregate allowance was checked after opening the missing body";
    assert_absent catalog revision left ~authorize;
    Eio.Path.save
      payload_a
      ~create:(`Exclusive 0o600)
      (String.make (String.length first) 'x');
    assert_absent catalog revision left ~authorize;
    verify right second;
    let store =
      S.Prompt_artifact_store.open_existing ~env ~root:(artifact_root env fixture) |> ok
    in
    let restored =
      Agent_session.Prompt_revision_builder.restore
        ~content_limits:limits
        ~artifact_store:store
        (Revision.definition revision)
        installed
      |> ok
    in
    require
      (List.equal String.equal (rendered restored) ordinary_text)
      "optional corrupt original prevented rendered restore";
    save env (local "a.html") first;
    let revoked =
      with_original catalog revision right ~authorize ~f:(fun ~check _ _ ->
        configure env fixture ~retain:false;
        ignore (Daemon.reload_config daemon |> ok : Agent_server.Config_diff.t);
        check ())
    in
    require
      (Result.is_error revoked)
      "removed opt-in retained current disclosure authority";
    assert_absent catalog revision right ~authorize;
    require
      (List.equal String.equal (rendered (selected catalog)) ordinary_text)
      "revocation changed rendered content");
  Daemon_host.with_ env fixture ~options (fun _ daemon ->
    let catalog = Daemon.prompts daemon in
    let _ = Catalog.prune_unreferenced_artifacts catalog ~additional:[] |> ok in
    require
      (match Eio.Path.kind ~follow:false installed_dir with
       | `Not_found -> true
       | _ -> false)
      "unreferenced private original pool survived pruning";
    require
      (P.Id.Prompt_revision.equal (Revision.id (selected catalog)) ordinary_id)
      "pruning affected the rendered revision")
;;

module Handoff = struct
  module A = Agent_session
  module Actor = A.Session_actor
  module Receipt = A.Recovery_installed_originals
  module Factory = Agent_server.Session_factory.Recovery_assets
  module Source = A.Recovery_source_reader
  module Http = Data_integrity_http

  let key text = P.Idempotency_key.of_string text |> protocol_ok

  let connect ~sw env fixture token =
    let client =
      Http_driver.create
        ~sw
        ~env
        ~port:(Config_fixture.http_port fixture)
        ~token:(Some token)
      |> Result.ok_or_failwith
    in
    let initialized, _ =
      Http_driver.initialize ~features:[ P.Recovery_query.Features.windows ] client
      |> protocol_ok
    in
    require
      (List.mem
         initialized.enabled_features
         P.Recovery_query.Features.windows
         ~equal:String.equal)
      "the daemon did not negotiate recovery controls";
    client
  ;;

  let proof daemon token =
    Daemon.authenticate_http_actor
      daemon
      { client_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0); headers = [] }
      (Some token)
    |> protocol_ok
  ;;

  let actor_ok = function
    | Ok value -> value
    | Error error -> raise_s [%sexp "installed original handoff", (error : Source.error)]
  ;;

  let authorize actor (state : A.Session_state.t) =
    let principal = Operator_authorization.principal actor in
    if
      Operator_authorization.is_current actor
      && Option.exists
           state.identity.creating_principal
           ~f:(P.Id.Principal.equal principal.id)
    then Ok ()
    else Error (P.Error.invalid_request "fixture writer is unavailable")
  ;;

  let entry daemon id =
    Agent_server.Session_registry.find (Daemon.registry daemon) id |> Option.value_exn
  ;;

  let initial_row state =
    match A.Session_state.(state.conversation.canonical_history) with
    | [ _; row ] when A.Session_state.(state.conversation.initial_prompt_entry_count) = 2
      -> row
    | _ -> failwith "expected two actual allocated initial rows"
  ;;

  let text row part =
    let module Payload = History_entry.Payload in
    let entry = A.History_codec.of_protocol row |> protocol_ok in
    match History_entry.payload entry |> Payload.semantic |> Payload.Semantic.view with
    | Message { content; _ } ->
      (match List.nth_exn content part with
       | Payload.Content.Text { text; _ } -> text
       | _ -> failwith "expected rendered text at the exact part")
    | _ -> failwith "expected canonical message"
  ;;

  let citation scope (row : P.History.entry) =
    P.Recovery_window.Citation.create
      scope
      ~occurrence:row.id
      ~version:(P.Recovery_item_version.canonical row.content_revision)
    |> protocol_ok
  ;;

  let assert_receipt (state : A.Session_state.t) =
    let row = initial_row state in
    let receipt = Option.value_exn state.recovery_installed_originals in
    require
      (match Receipt.bindings receipt with
       | [ a; b ] ->
         P.History.Id.equal a.row row.id
         && P.History.Id.equal b.row row.id
         && Int.equal a.part 1
         && Int.equal b.part 3
         && not
              (String.equal
                 (Originals.Occurrence.digest a.original)
                 (Originals.Occurrence.digest b.original))
       | _ -> false)
      "receipt did not preserve exact allocated row and two distinct multipart positions";
    let left = text row 1
    and right = text row 3 in
    require
      (String.equal left right && String.is_substring left ~substring:"same rendered")
      "canonical parts did not retain identical rendered text";
    let public = A.Session_state.summary state |> P.Session.to_json |> Jsonaf.to_string in
    require
      (not (String.is_substring public ~substring:"recovery_installed_originals"))
      "public snapshot exposed the private receipt";
    row
  ;;

  let wait_idle env (entry : Agent_server.Session_registry.entry) =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 8. (fun () ->
      let rec loop () =
        let state = Actor.state entry.actor |> protocol_ok in
        match state.lifecycle.observed, state.active_operation with
        | Idle, None -> state
        | Failed error, _ ->
          raise_s [%sexp "actual initialization failed", (error : P.Error.t)]
        | _ ->
          Eio.Fiber.yield ();
          loop ()
      in
      loop ())
  ;;

  let start env daemon client label =
    let session, attachment =
      Http.create ~mode:Owner_read_write client (label ^ "-create")
    in
    Http_driver.request
      client
      (Session_start
         { session_id = session.id
         ; attachment_id = attachment.id
         ; queue_if_limited = false
         ; idempotency_key = key (label ^ "-start")
         })
    |> protocol_ok
    |> ignore;
    let entry = entry daemon session.id in
    session.id, attachment.id, wait_idle env entry
  ;;

  let attach client session_id name =
    let result =
      Http_driver.request
        client
        (Session_attach
           { session_id
           ; requested_mode = Owner_read_write
           ; subscribe = false
           ; after_sequence = None
           ; reclaim_token = None
           ; idempotency_key = key name
           })
      |> protocol_ok
    in
    match result.result with
    | Session_attach attached -> attached.attachment.id
    | _ -> failwith "expected the real reattached writer"
  ;;

  let enable entry actor attachment_id =
    Actor.enable_recovery_windows
      entry.Agent_server.Session_registry.actor
      ~actor
      ~attachment_id
      ~authorize:(authorize actor)
    |> protocol_ok
  ;;

  let enable_remote client daemon ~session_id ~attachment_id ~generation =
    let session =
      P.Session_ref.create
        ~server_id:(S.Session_store.server_id (Daemon.store daemon))
        ~session_id
    in
    match
      Http_driver.request_without_history
        client
        (Recovery_enable { session; attachment_id; expected_generation = generation })
      |> protocol_ok
    with
    | Recovery_enable scope -> scope
    | _ -> failwith "expected the negotiated recovery activation result"
  ;;

  let stop client ~session_id ~attachment_id name =
    Http_driver.request
      client
      (Session_stop
         { session_id; attachment_id; mode = Graceful; idempotency_key = key name })
    |> protocol_ok
    |> ignore
  ;;

  let read factory ~sw actor citation part =
    Factory.read factory ~sw ~authorize:(authorize actor) citation ~part ~max_bytes:limit
  ;;

  let assert_original factory ~sw actor citation part expected =
    let reference, actual = read factory ~sw actor citation part |> actor_ok in
    require
      (String.equal actual expected)
      "protected read returned bytes from another occurrence";
    Agent_store.Recovery_asset_reference.blob reference
  ;;

  let with_host env fixture ~options f =
    let observed = ref None in
    let host =
      Or_error.try_with (fun () ->
        try
          Daemon_host.with_ env fixture ~options (fun sw daemon ->
            observed := Some (Or_error.try_with (fun () -> f sw daemon)))
        with
        | Agent_server.Session_registry.Cleanup_failed failure ->
          let error = Agent_server.Session_registry.Cleanup_failure.error failure in
          raise_s [%sexp "daemon shutdown after installed handoff", (error : P.Error.t)])
    in
    match host, !observed with
    | Error cleanup, Some (Error failure) ->
      Error.raise (Error.of_list [ failure; cleanup ])
    | Error error, _ -> Error.raise error
    | Ok (), Some result -> Or_error.ok_exn result
    | Ok (), None -> failwith "daemon handoff fixture did not run"
  ;;

  let run env environment =
    let fixture = fixture env environment in
    let prompt = Config_fixture.prompt_path fixture in
    let local name = Filename.concat (Filename.dirname prompt) name in
    save
      env
      prompt
      {|<system>Setup</system><developer><text>before</text><doc src="a.html" local markdown capture/><text>between</text><doc src="b.html" local markdown capture/></developer>|};
    save env (local "a.html") first;
    save env (local "b.html") second;
    configure env fixture ~retain:true;
    let token = Config_fixture.admin_token fixture in
    let options = Daemon_host.with_offline_inference Daemon.default_options in
    let installed =
      Eio.Switch.run (fun sw ->
        let process =
          Daemon_process.start
            ~sw
            ~env
            ~fixture
            ~config_path:(Config_fixture.config_path fixture)
        in
        let _ = Daemon_process.wait_ready process ~env ~timeout_seconds:15. |> ok in
        let revision = listing ~sw env fixture in
        let stopped = Daemon_process.stop process ~env ~grace_seconds:5. in
        require (not stopped.forced) "installation daemon did not stop cleanly";
        revision)
    in
    let session_id, row =
      with_host env fixture ~options (fun sw daemon ->
        let client = connect ~sw env fixture token in
        let session_id, attachment_id, state =
          start env daemon client "installed-handoff"
        in
        require
          (P.Id.Prompt_revision.equal state.spec.prompt_revision_id installed)
          "the real session started from a different revision";
        require
          (Option.is_none state.recovery_windows)
          "installed custody silently enabled a session";
        let row = assert_receipt state in
        stop client ~session_id ~attachment_id "installed-handoff-stop";
        Http_driver.shutdown client;
        session_id, row)
    in
    let scope, adopted =
      with_host env fixture ~options (fun sw daemon ->
        let client = connect ~sw env fixture token in
        let attachment_id = attach client session_id "installed-handoff-attach" in
        let target = entry daemon session_id in
        let replay = Actor.state target.actor |> protocol_ok |> assert_receipt in
        require
          (P.History.equal_entry replay row)
          "the receipt did not survive cold replay";
        let actor = proof daemon token in
        let scope = enable target actor attachment_id in
        let selected = citation scope row in
        let factory = Daemon.factory daemon in
        require
          (Result.is_error (read factory ~sw actor selected 1))
          "the owner enablement had not invoked the optional handoff";
        let adopt attachment_id connection_current =
          Factory.adopt_installed
            factory
            ~sw
            ~actor
            ~attachment_id
            ~connection_current
            scope
        in
        require
          (Result.is_error (adopt (P.Id.Attachment.create ()) (fun () -> true)))
          "an unrelated attachment was accepted";
        let handle = Option.value_exn target.store_handle in
        let directory =
          Filename.concat
            (S.Session_store.Handle.directory handle)
            "recovery-asset-commits"
        in
        let cut = ref false in
        let connection_current () =
          let pending_rename =
            try
              Sys_unix.ls_dir directory
              |> List.exists ~f:(fun filename ->
                String.is_substring filename ~substring:".frame.tmp-")
            with
            | Sys_error _ -> false
          in
          if pending_rename then cut := true;
          not !cut
        in
        require
          (Result.is_error (adopt attachment_id connection_current) && !cut)
          "the original connection was not invalidated at physical commit rename";
        require
          (Result.is_error (read factory ~sw actor selected 1))
          "the interrupted physical writer exposed an uncommitted reference";
        let enable_now () =
          enable_remote
            client
            daemon
            ~session_id
            ~attachment_id
            ~generation:(P.Recovery_window.Scope.generation scope)
        in
        let retry_scope = enable_now () in
        require
          (P.Recovery_window.Scope.equal scope retry_scope)
          "retry replaced the exact window";
        let a = assert_original factory ~sw actor selected 1 first in
        let b = assert_original factory ~sw actor selected 3 second in
        require (not (P.Id.Blob.equal a.id b.id)) "distinct originals cross-bound";
        ignore (enable_now () : P.Recovery_window.Scope.t);
        let retry = assert_original factory ~sw actor selected 1 first in
        require
          (P.Id.Blob.equal a.id retry.id)
          "handoff retry allocated a different result";
        require
          (Result.is_error (read factory ~sw actor selected 0))
          "plain sibling text acquired original custody";
        let fresh_id, fresh_attachment, fresh_state =
          start env daemon client "installed-fresh"
        in
        let fresh = assert_receipt fresh_state in
        let activate_fresh () =
          enable_remote
            client
            daemon
            ~session_id:fresh_id
            ~attachment_id:fresh_attachment
            ~generation:fresh_state.identity.generation
        in
        let pool = Eio.Path.(installed_path env fixture installed / "originals") in
        let body = Eio.Path.(pool / sha256 first) in
        let offline = Eio.Path.(pool / "temporarily-unavailable") in
        Eio.Path.rename body offline;
        let fresh_scope, independently_adopted =
          Exn.protect
            ~finally:(fun () -> Eio.Path.rename offline body)
            ~f:(fun () ->
              let fresh_scope = activate_fresh () in
              let fresh_citation = citation fresh_scope fresh in
              require
                (Result.is_error (read factory ~sw actor fresh_citation 1))
                "the missing body acquired a committed file";
              fresh_scope, assert_original factory ~sw actor fresh_citation 3 second)
        in
        require
          (P.Recovery_window.Scope.equal fresh_scope (activate_fresh ()))
          "optional file failure changed activation";
        let fresh_citation = citation fresh_scope fresh in
        ignore
          (assert_original factory ~sw actor fresh_citation 1 first : P.Blob.Metadata.t);
        let retried = assert_original factory ~sw actor fresh_citation 3 second in
        require
          (P.Id.Blob.equal independently_adopted.id retried.id)
          "retry replaced the independently committed file";
        stop
          client
          ~session_id:fresh_id
          ~attachment_id:fresh_attachment
          "installed-fresh-stop";
        let rejected_id, rejected_attachment, state =
          start env daemon client "installed-revoked"
        in
        let rejected_row = assert_receipt state in
        configure env fixture ~retain:false;
        ignore (Daemon.reload_config daemon |> ok : Agent_server.Config_diff.t);
        let actor = proof daemon token in
        let rejected_scope =
          enable_remote
            client
            daemon
            ~session_id:rejected_id
            ~attachment_id:rejected_attachment
            ~generation:state.identity.generation
        in
        require
          (Result.is_error
             (Factory.adopt_installed
                factory
                ~sw
                ~actor
                ~attachment_id:rejected_attachment
                ~connection_current:(fun () -> true)
                rejected_scope))
          "a retired installed revision allowed a new adoption";
        require
          (Result.is_error
             (read factory ~sw actor (citation rejected_scope rejected_row) 1))
          "source policy revocation left a visible original";
        let current = Actor.state (entry daemon rejected_id).actor |> protocol_ok in
        require
          (P.History.equal_entry rejected_row (initial_row current))
          "revoked original authority damaged canonical rendered text";
        stop
          client
          ~session_id:rejected_id
          ~attachment_id:rejected_attachment
          "installed-revoked-stop";
        Http_driver.shutdown client;
        scope, a)
    in
    Daemon_host.with_ env fixture ~options (fun sw daemon ->
      let client = connect ~sw env fixture token in
      let _ = attach client session_id "installed-handoff-final-attach" in
      let actor = proof daemon token in
      let retained =
        assert_original (Daemon.factory daemon) ~sw actor (citation scope row) 1 first
      in
      require
        (P.Id.Blob.equal adopted.id retained.id)
        "the independently adopted asset did not retain its committed identity";
      Http_driver.shutdown client)
  ;;
end

let run_handoff = Handoff.run
