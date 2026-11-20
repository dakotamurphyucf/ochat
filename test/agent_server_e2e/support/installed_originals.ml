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

let with_original catalog revision occurrence ~authorize ~f =
  Catalog.with_installed_original
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
