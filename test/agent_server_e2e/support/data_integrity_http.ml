open Core

let require condition message = if not condition then failwith message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "data integrity protocol failure", (error : Agent_protocol.Error.t)]
;;

let string_ok = function
  | Ok value -> value
  | Error message -> failwith message
;;

let request_public client command =
  (Http_driver.request client command |> protocol_ok).result
;;

let request client command =
  Http_driver.request_without_history client command |> protocol_ok
;;

let digest data = Digestif.SHA256.(digest_string data |> to_hex)
let key text = Agent_protocol.Idempotency_key.of_string text |> protocol_ok

let fixture env environment name =
  let port =
    Eio.Switch.run (fun sw ->
      let reservation = Port_reservation.create ~sw ~env in
      let port = Port_reservation.port reservation in
      Port_reservation.release reservation;
      port)
  in
  Config_fixture.create environment ~name ~http_port:port
;;

let token_path fixture =
  let native =
    Filename.concat (Filename.dirname (Config_fixture.config_path fixture)) "tokens.sexp"
  in
  Temporary_environment.path (Config_fixture.environment fixture) native
;;

let restrict_public fixture =
  Config_fixture.grant_public_all_scopes fixture;
  let path = token_path fixture in
  let rec remove_admin = function
    | Sexp.List values ->
      Sexp.List
        (List.filter_map values ~f:(function
           | Sexp.Atom "configuration.admin" -> None
           | value -> Some (remove_admin value)))
    | atom -> atom
  in
  let tokens = Eio.Path.load path |> Sexp.of_string in
  let tokens =
    match tokens with
    | Sexp.List [ admin; public ] -> Sexp.List [ admin; remove_admin public ]
    | _ -> failwith "unexpected credential fixture"
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) path (Sexp.to_string tokens)
;;

let configured_principal fixture token =
  let field fields key =
    List.find_map_exn fields ~f:(function
      | Sexp.List [ Atom name; Atom value ] when String.equal name key -> Some value
      | _ -> None)
  in
  match Eio.Path.load (token_path fixture) |> Sexp.of_string with
  | Sexp.List tokens ->
    List.find_map_exn tokens ~f:(function
      | Sexp.List fields when String.equal (field fields "token_sha256") (digest token) ->
        Some
          (Agent_protocol.Id.Principal.of_string (field fields "principal_id")
           |> protocol_ok)
      | _ -> None)
  | _ -> failwith "invalid credential fixture"
;;

let connect ?features ~sw env fixture token =
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some token)
    |> string_ok
  in
  let initialized, _ = Http_driver.initialize ?features client |> protocol_ok in
  require
    (Agent_protocol.Id.Principal.compare
       initialized.principal.id
       (configured_principal fixture token)
     = 0)
    "HTTP authentication principal differs from the bearer credential fixture";
  client, initialized.principal.id
;;

let connection client =
  Agent_client.Transport.create
    ~request:(fun command ->
      Http_driver.request client command |> Result.map ~f:(fun rpc -> rpc.result))
    ~next_notification:(fun () -> None)
    ~close:(fun () -> ())
  |> Agent_client.Connection.create
;;

let catalog_ids client =
  let page = Agent_protocol.Page.Request.create ~limit:100 () |> protocol_ok in
  let prompt =
    match
      request client (Prompt_list { page; enabled = Some true; available = Some true })
    with
    | Prompt_list page -> (List.hd_exn page.items).id
    | _ -> failwith "expected prompt list"
  in
  let workspace =
    match
      request
        client
        (Workspace_list { page; kind = None; access = None; available = Some true })
    with
    | Workspace_list page ->
      (List.find_exn page.items ~f:(fun item -> String.equal item.name "physical")).id
    | _ -> failwith "expected workspace list"
  in
  prompt, workspace
;;

let spec client =
  let prompt, workspace = catalog_ids client in
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt)
    ~workspace:(Configured workspace)
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately:false
    ~labels:[ "suite", "data-integrity" ]
    ()
  |> protocol_ok
;;

let create ?(mode = Agent_protocol.Session.Read_write) client name =
  match
    request_public
      client
      (Session_create
         { spec = spec client
         ; requested_mode = Some mode
         ; subscribe = false
         ; idempotency_key = key name
         })
  with
  | Session_create created ->
    created.session, (Option.value_exn created.attachment).attachment
  | _ -> failwith "expected session create"
;;

let export
      client
      (session : Agent_protocol.Session.t)
      (attachment : Agent_protocol.Session.Attachment.t)
  =
  match
    request
      client
      (Session_export
         { session_id = session.id
         ; attachment_id = Some attachment.id
         ; format = Json
         ; revision = None
         ; history = None
         })
  with
  | Session_export export -> export.blob
  | _ -> failwith "expected export"
;;

let audit_request session_id principal_id cursor limit =
  Agent_protocol.Command.Audit_read
    { page = Agent_protocol.Page.Request.create ~limit ?cursor () |> protocol_ok
    ; session_id = Some session_id
    ; principal_id = Some principal_id
    ; minimum_level = None
    ; name_prefix = Some "protocol.command."
    }
;;

let audit client session principal cursor limit =
  match request client (audit_request session principal cursor limit) with
  | Audit_read page -> page
  | _ -> failwith "expected audit page"
;;

let assert_attribution (record : Agent_protocol.Audit.t) session principal =
  require
    (Option.equal
       (fun a b -> Agent_protocol.Id.Session.compare a b = 0)
       record.session_id
       (Some session))
    "audit session attribution differs";
  require
    (Option.equal
       (fun a b -> Agent_protocol.Id.Principal.compare a b = 0)
       record.principal_id
       (Some principal))
    "audit principal attribution differs"
;;

let assert_record (record : Agent_protocol.Audit.t) session principal method_ outcome =
  assert_attribution record session principal;
  let successful = String.equal outcome "success" in
  require
    (String.equal
       record.name
       (if successful then "protocol.command.succeeded" else "protocol.command.failed"))
    "audit action status differs";
  require
    (Agent_protocol.Audit.equal_level record.level (if successful then Info else Warning))
    "audit severity differs";
  require record.redacted "command audit must be marked redacted";
  let expected = `Object [ "method", `String method_; "outcome", `String outcome ] in
  require
    (String.equal (Jsonaf.to_string record.payload) (Jsonaf.to_string expected))
    "audit method/outcome payload differs"
;;

let tampered cursor =
  let text = Agent_protocol.Page.Cursor.to_string cursor in
  let last = if Char.equal text.[String.length text - 1] '0' then "1" else "0" in
  Agent_protocol.Page.Cursor.of_string (String.drop_suffix text 1 ^ last) |> protocol_ok
;;

let assert_tamper client session principal cursor =
  match
    Http_driver.request
      client
      (audit_request session principal (Some (tampered cursor)) 1)
  with
  | Error error ->
    require
      (String.is_substring error.message ~substring:"signature")
      "cursor failure was not signature validation"
  | Ok _ -> failwith "signed audit cursor tampering was accepted"
;;

let exercise_actions client session attachment =
  let blob = export client session attachment in
  let bad_read =
    Agent_protocol.Command.Blob_read
      { session_id = session.id
      ; attachment_id = Some attachment.id
      ; blob_id = blob.id
      ; offset = Int64.succ blob.byte_length
      ; max_bytes = 1
      }
  in
  let failure =
    match Http_driver.request client bad_read with
    | Error error ->
      require
        (Agent_protocol.Error.equal_code error.code Invalid_request)
        "bad blob offset failed for an unrelated reason";
      Agent_protocol.Error.code_to_string error.code
    | Ok _ -> failwith "out-of-range blob read succeeded"
  in
  ignore
    (request_public client (Session_get { session_id = session.id; history = None })
     : Agent_protocol.Public.Result.t);
  failure
;;

let assert_audit_page page session principal failure =
  require
    (List.length page.Agent_protocol.Page.items = 3)
    "audit has missing, duplicate or unrelated commands";
  List.iter2_exn
    page.items
    [ "session.export", "success"; "blob.read", failure; "session.get", "success" ]
    ~f:(fun record (method_, outcome) ->
      assert_record record session principal method_ outcome);
  let sequences =
    List.map page.items ~f:(fun item -> item.Agent_protocol.Audit.sequence)
  in
  require
    (List.is_sorted_strictly sequences ~compare:Int64.compare)
    "audit action ordering is not strictly increasing";
  require (Option.is_none page.next_cursor) "unexpected trailing audit page"
;;

let audit_before_restart env fixture =
  let saved = ref None in
  Daemon_host.with_
    env
    fixture
    ~options:(Daemon_host.with_offline_inference Agent_server.Daemon.default_options)
    (fun sw _ ->
       let actor, principal =
         connect ~sw env fixture (Config_fixture.public_token fixture)
       in
       let reader, _ = connect ~sw env fixture (Config_fixture.admin_token fixture) in
       let session, attachment = create actor "data-audit-create" in
       let failure = exercise_actions actor session attachment in
       let all = audit reader session.id principal None 20 in
       assert_audit_page all session.id principal failure;
       let first = audit reader session.id principal None 1 in
       require (List.length first.items = 1) "audit page limit was not enforced";
       let cursor = Option.value_exn first.next_cursor in
       assert_tamper reader session.id principal cursor;
       saved := Some (session.id, principal, failure, all, first, cursor);
       Http_driver.shutdown actor;
       Http_driver.shutdown reader);
  Option.value_exn !saved
;;

let audit_after_restart env fixture (session, principal, failure, all, first, cursor) =
  Daemon_host.with_ env fixture ~options:Agent_server.Daemon.default_options (fun sw _ ->
    let reader, _ = connect ~sw env fixture (Config_fixture.admin_token fixture) in
    let second = audit reader session principal (Some cursor) 1 in
    require (List.length second.items = 1) "second audit page limit differs";
    require (Option.is_some second.next_cursor) "second audit page lost its continuation";
    let third = audit reader session principal second.next_cursor 1 in
    let combined =
      Agent_protocol.Page.
        { items = first.items @ second.items @ third.items
        ; next_cursor = third.next_cursor
        }
    in
    assert_audit_page combined session principal failure;
    let encode page =
      Agent_protocol.Page.to_json Agent_protocol.Audit.to_json page |> Jsonaf.to_string
    in
    require
      (String.equal (encode all) (encode combined))
      "audit records or signed cursor changed across restart";
    assert_tamper reader session principal cursor;
    Http_driver.shutdown reader)
;;

let test_audit env environment =
  let fixture = fixture env environment "data-audit" in
  restrict_public fixture;
  let before = audit_before_restart env fixture in
  audit_after_restart env fixture before
;;

let headers token = [ "authorization", "Bearer " ^ token ]

let upload client token ?(extra = []) body =
  Http_driver.request_raw
    client
    ~headers:(headers token @ [ "content-type", "application/octet-stream" ] @ extra)
    ~body
    ~meth:`POST
    ~path:"/v1/blobs"
    ()
  |> string_ok
;;

let metadata response =
  match Jsonaf.of_string response.Http_driver.body with
  | `Object fields ->
    List.Assoc.find_exn fields ~equal:String.equal "blob"
    |> Agent_protocol.Blob.Metadata.of_json
    |> protocol_ok
  | _ -> failwith "invalid upload response"
;;

let fetch client token blob =
  Http_driver.request_raw
    client
    ~headers:(headers token)
    ~meth:`GET
    ~path:
      ("/v1/blobs/"
       ^ Agent_protocol.Id.Blob.to_string blob.Agent_protocol.Blob.Metadata.id)
    ()
  |> string_ok
;;

let assert_http_status response status =
  require
    (response.Http_driver.status = status)
    (sprintf "expected HTTP %d, got %d: %s" status response.status response.body)
;;

let assert_denied response =
  assert_http_status response 403;
  let error =
    Jsonaf.of_string response.body |> Agent_protocol.Error.of_json |> protocol_ok
  in
  require
    (Agent_protocol.Error.equal_code error.code Permission_denied)
    "foreign blob rejection was not ownership enforcement";
  require
    (String.equal error.message "blob is not visible")
    "foreign blob rejection came from the wrong authorization check"
;;

let assert_upload_roundtrip owner owner_token body =
  let uploaded = upload owner owner_token ~extra:[ "ochat-sha256", digest body ] body in
  assert_http_status uploaded 201;
  let blob = metadata uploaded in
  require
    (Int64.equal blob.byte_length 4096L && String.equal blob.digest (digest body))
    "upload metadata length/digest differs";
  let downloaded = fetch owner owner_token blob in
  assert_http_status downloaded 200;
  require (String.equal downloaded.body body) "HTTP blob round trip differs";
  blob
;;

let temporary_files environment daemon =
  Agent_server.Daemon.store daemon
  |> Agent_store.Session_store.data_root
  |> Agent_store.Data_root.temporary_blobs_path
  |> Temporary_environment.path environment
  |> Eio.Path.read_dir
  |> List.sort ~compare:String.compare
;;

let assert_upload_rejection response message =
  require
    (response.Http_driver.status >= 400 && response.status < 500)
    "invalid HTTP upload was not rejected";
  let error =
    Jsonaf.of_string response.body |> Agent_protocol.Error.of_json |> protocol_ok
  in
  require (String.equal error.message message) "upload failed for an unrelated reason"
;;

let rejected_uploads environment daemon client token body =
  let before = temporary_files environment daemon in
  let forged =
    upload client token ~extra:[ "ochat-allowed-use", "recovery_asset:future" ] body
  in
  assert_upload_rejection forged "reserved blob use";
  let wrong = upload client token ~extra:[ "ochat-sha256", digest "wrong" ] body in
  assert_upload_rejection wrong "blob digest differs from the expected digest";
  let large = upload client token (body ^ "x") in
  assert_upload_rejection large "blob upload exceeds configured maximum";
  require
    (List.equal String.equal before (temporary_files environment daemon))
    "rejected HTTP upload leaked partial or metadata files"
;;

let assert_recovery_blob_is_private ~sw fixture daemon owner token body =
  let module P = Agent_protocol in
  let module B = Agent_store.Blob_store in
  let store_ok = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
  in
  let session, attachment = create owner "asset-privacy" in
  let registry = Agent_server.Daemon.registry daemon in
  let entry =
    Agent_server.Session_registry.find registry session.id |> Option.value_exn
  in
  let handle = Option.value_exn entry.store_handle in
  let blobs = Agent_server.Daemon.blob_store daemon in
  let store = Agent_server.Daemon.store daemon in
  let session_ref =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id store)
      ~session_id:session.id
  in
  let scope =
    P.Recovery_window.Scope.create
      ~session:session_ref
      ~generation:0
      ~window:(P.Id.Context_window.create ())
    |> protocol_ok
  in
  let citation =
    P.Recovery_window.Citation.create
      scope
      ~occurrence:(History_entry.Id.create ~namespace:"asset" ~sequence:1 |> string_ok)
      ~version:(P.Recovery_item_version.canonical P.History.Content_revision.zero)
    |> protocol_ok
  in
  let allowed_use =
    Agent_store.Blob_metadata.Recovery_use.for_part citation ~part:0 |> protocol_ok
  in
  let upload =
    B.begin_upload
      blobs
      ~sw
      ~id:(P.Id.Blob.create ())
      ~creating_principal:(configured_principal fixture token)
      ~target_session:(Some session.id)
      ~kind:File
      ~media_type:"text/plain"
      ~display_name:None
      ~allowed_use
      ~created_at:(P.Timestamp.now ())
      ~expires_at:None
    |> store_ok
  in
  B.write_string upload body |> store_ok;
  let adopted =
    B.finish upload ~expected_digest:(Some (digest body))
    |> store_ok
    |> B.adopt blobs handle
    |> store_ok
  in
  let blob = (B.Handle.metadata_checked adopted |> store_ok).blob in
  assert_denied (fetch owner token blob);
  match
    Http_driver.request
      owner
      (Blob_read
         { session_id = session.id
         ; attachment_id = Some attachment.id
         ; blob_id = blob.id
         ; offset = 0L
         ; max_bytes = 1
         })
  with
  | Error error when P.Error.equal_code error.code Permission_denied -> ()
  | Error _ | Ok _ ->
    failwith "authorized session client could bypass the recovery reader"
;;

let test_blobs env environment =
  let fixture = fixture env environment "data-blobs" in
  restrict_public fixture;
  let options = Daemon_host.with_offline_inference Agent_server.Daemon.default_options in
  let protocol_limits = { options.protocol_limits with max_request_bytes = 4096 } in
  let authoring =
    Agent_server.Authoring_installation.Host_policy.basic_supplied
      ~initialize_limits:protocol_limits
    |> protocol_ok
  in
  let options = { options with protocol_limits; authoring = Some authoring } in
  Daemon_host.with_ env fixture ~options (fun sw daemon ->
    let owner_token = Config_fixture.admin_token fixture in
    let foreign_token = Config_fixture.public_token fixture in
    let owner, _ = connect ~sw env fixture owner_token in
    let foreign, _ = connect ~sw env fixture foreign_token in
    let body = String.init 4096 ~f:(fun index -> Char.of_int_exn (index mod 256)) in
    let blob = assert_upload_roundtrip owner owner_token body in
    assert_denied (fetch foreign foreign_token blob);
    let own_upload = upload foreign foreign_token "foreign-owned" in
    assert_http_status own_upload 201;
    assert_http_status (fetch foreign foreign_token (metadata own_upload)) 200;
    rejected_uploads environment daemon owner owner_token body;
    assert_recovery_blob_is_private ~sw fixture daemon owner owner_token body;
    Http_driver.shutdown owner;
    Http_driver.shutdown foreign)
;;

let test_recovery_assets env environment =
  let module P = Agent_protocol in
  let module A = Agent_session in
  let module Actor = A.Session_actor in
  let module Reference = Agent_store.Recovery_asset_reference in
  let module S = Agent_store.Session_store in
  let module B = Agent_store.Blob_store in
  let module Source = A.Recovery_source_reader in
  let module Factory = Agent_server.Session_factory.Recovery_assets in
  let options = Daemon_host.with_offline_inference Agent_server.Daemon.default_options in
  let fixture = fixture env environment "data-recovery-assets" in
  let token = Config_fixture.admin_token fixture in
  let actor_auth daemon =
    Agent_server.Daemon.authenticate_http_actor
      daemon
      { client_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0); headers = [] }
      (Some token)
    |> protocol_ok
  in
  let asset_ok = function
    | Ok value -> value
    | Error error -> raise_s [%sexp "asset operation failed", (error : Source.error)]
  in
  let store_ok = function
    | Ok value -> value
    | Error error ->
      raise_s [%sexp "asset store failed", (error : Agent_store.Store_error.t)]
  in
  let expect_error expected = function
    | Error actual when Source.equal_error expected actual -> ()
    | Error actual ->
      raise_s
        [%sexp
          "unexpected asset error", (expected : Source.error), (actual : Source.error)]
    | Ok _ -> failwith "unexpected asset disclosure"
  in
  let authorize proof (state : A.Session_state.t) =
    if
      Operator_authorization.is_current proof
      && Option.equal
           P.Id.Principal.equal
           state.identity.creating_principal
           (Some (Operator_authorization.principal proof).id)
    then Ok ()
    else Error (P.Error.invalid_request "fixture source authority expired")
  in
  let orphan_name = String.make 64 '0' ^ ".frame.tmp-1-1" in
  let orphan handle =
    let dir = Filename.concat (S.Handle.directory handle) "recovery-asset-intents" in
    let path = Filename.concat dir orphan_name in
    Eio.Path.(Eio.Stdenv.fs env / path)
  in
  let original = "private original file bytes; rendered text was separate" in
  let max_bytes = String.length original in
  let retained = ref None in
  Daemon_host.with_ env fixture ~options (fun sw daemon ->
    let client, principal = connect ~sw env fixture token in
    let session, attachment = create ~mode:Owner_read_write client "asset-publication" in
    let registry = Agent_server.Daemon.registry daemon in
    let entry =
      Agent_server.Session_registry.find registry session.id |> Option.value_exn
    in
    let handle = Option.value_exn entry.store_handle in
    let sessions = Agent_server.Daemon.store daemon in
    let proof = actor_auth daemon in
    let authorize = authorize proof in
    let attachment_id = attachment.id in
    ignore (Actor.start entry.actor ~attachment_id |> protocol_ok : P.Session.t);
    let row sequence =
      let id = History_entry.Id.create ~namespace:"asset-e2e" ~sequence |> string_ok in
      A.History_codec.user_text ~id "identical rendered text"
      |> A.History_codec.to_protocol
    in
    let first = row 0
    and second = row 1 in
    ignore
      (Actor.append_history entry.actor ~attachment_id [ first; second ] |> protocol_ok
       : P.Session.t);
    let scope =
      Actor.enable_recovery_windows entry.actor ~actor:proof ~attachment_id ~authorize
      |> protocol_ok
    in
    let citation (row : P.History.entry) =
      P.Recovery_window.Citation.create
        scope
        ~occurrence:row.id
        ~version:(P.Recovery_item_version.canonical row.content_revision)
      |> protocol_ok
    in
    let old = citation first in
    let pending = citation second in
    let capture citation row =
      A.Recovery_asset.Attribution.create
        citation
        ~part:0
        ~row
        ~media_type:"text/plain"
        ~original
    in
    expect_error Stale (capture old second);
    let factory = Agent_server.Daemon.factory daemon in
    let module History = Agent_server.Session_factory.Recovery_history in
    let read ~authorize =
      History.read_text
        factory
        ~authorize
        (Canonical old)
        ~view:Ordinary
        ~part:0
        ~start:0
        ~stop:9
        ~max_bytes:5
        ()
    in
    let exact selection view state =
      match selection, view with
      | A.Recovery_item_text.Canonical selected, A.Recovery_item_text.Ordinary
        when P.Recovery_window.Citation.equal selected old -> authorize state
      | _ -> Error (P.Error.invalid_request "unexpected selection")
    in
    (match read ~authorize:exact with
     | Ok slice ->
       require
         (String.equal slice.text "ident" && Option.equal Int.equal slice.next_offset (Some 5))
         "exact text read lost its original byte position"
     | Error error -> raise_s [%sexp "exact text read", (error : History.error)]);
    (match read ~authorize:(fun _ _ _ -> Error (P.Error.invalid_request "no source access")) with
     | Error (Source Denied) -> ()
     | _ -> failwith "unavailable source authority admitted a text read");
    let publish ~authorize attribution =
      Factory.publish_converted_file
        factory
        ~sw
        ~authorize
        ~principal
        ~now:(P.Timestamp.now ())
        ~max_bytes
        attribution
    in
    let committed = publish ~authorize (capture old first |> asset_ok) |> asset_ok in
    let id = (Reference.blob committed).id in
    let get citation = Factory.read factory ~sw ~authorize citation ~part:0 ~max_bytes in
    let restored, bytes = get old |> asset_ok in
    require
      (Reference.equal restored committed && String.equal bytes original)
      "committed exact asset differs from the original";
    require
      (Reference.equal
         committed
         (publish ~authorize (capture old first |> asset_ok) |> asset_ok))
      "exact retry created another asset";
    let wrong =
      A.Recovery_asset.Attribution.create
        old
        ~part:0
        ~row:first
        ~media_type:"text/plain"
        ~original:"another original"
      |> asset_ok
    in
    expect_error Stale (publish ~authorize wrong);
    expect_error
      Limit
      (Factory.read factory ~sw ~authorize old ~part:0 ~max_bytes:(max_bytes - 1));
    expect_error Unavailable (Factory.read factory ~sw ~authorize old ~part:1 ~max_bytes);
    let wrong_authority _ = Error (P.Error.invalid_request "source scope denied") in
    expect_error
      Denied
      (Factory.read factory ~sw ~authorize:wrong_authority old ~part:0 ~max_bytes);
    let dir = Filename.concat (S.Handle.directory handle) "recovery-asset-blobs" in
    let root =
      B.Result_root.retained_conversation
        (Agent_server.Daemon.blob_store daemon)
        ~sessions
        ~handle
      |> store_ok
    in
    let blobs = Agent_server.Daemon.blob_store daemon in
    let facade =
      B.with_max_upload_bytes blobs ~max_upload_bytes:(Int64.of_int max_bytes) |> store_ok
    in
    ignore (B.open_result facade root id |> store_ok : B.Result_read.t);
    let data_root = S.data_root sessions in
    let other =
      B.create
        ~env
        ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
        ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
        ~max_upload_bytes:(Int64.of_int max_bytes)
      |> store_ok
    in
    require
      (Result.is_error (B.open_result other root id))
      "independent Blob coordinator accepted the retained root";
    let public_blob = fetch client token (Reference.blob committed) in
    assert_http_status public_blob 400;
    require
      (String.is_substring public_blob.body ~substring:"blob_unavailable")
      "private asset was visible to the generic HTTP blob reader";
    (match
       Http_driver.request
         client
         (Blob_read
            { session_id = session.id
            ; attachment_id = Some attachment.id
            ; blob_id = id
            ; offset = 0L
            ; max_bytes = 1
            })
     with
     | Error error when P.Error.equal_code error.code Persistence_error -> ()
     | Error _ | Ok _ -> failwith "private asset was visible to the generic blob command");
    let blob_path = Filename.concat dir (P.Id.Blob.to_string id ^ ".blob") in
    let eio_blob = Eio.Path.(Eio.Stdenv.fs env / blob_path) in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      eio_blob
      (String.map original ~f:(fun _ -> 'X'));
    expect_error Corrupt (get old);
    Eio.Path.unlink eio_blob;
    expect_error Missing (get old);
    Eio.Path.save ~create:(`Exclusive 0o600) eio_blob original;
    let collector =
      Agent_store.Recovery_asset_store.create blobs ~sessions ~handle
      |> Result.ok
      |> Option.value_exn
    in
    let inventory_reader () =
      S.retained_reader sessions ~handle ~max_entries:100 ~max_bytes:32768 |> store_ok
    in
    let collect () =
      Agent_store.Recovery_asset_store.collect
        collector
        ~reader:(inventory_reader ())
        ~retired:(fun _ -> false)
      |> store_ok
    in
    (* The factory checks authority before, during and after the physical read.
       Exercise the real collector at those boundaries without assuming a poll count. *)
    let collection_before = ref false in
    let collection_blocked = ref false in
    let collection_after = ref false in
    let authorize_read state =
      let open Result.Let_syntax in
      let%map () = authorize state in
      match collect () with
      | None ->
        require !collection_before "collector was busy before the protected read";
        collection_blocked := true
      | Some _ when !collection_blocked -> collection_after := true
      | Some _ -> collection_before := true
    in
    let read_reference, read_bytes =
      Factory.read factory ~sw ~authorize:authorize_read old ~part:0 ~max_bytes
      |> asset_ok
    in
    require
      (!collection_blocked && !collection_after)
      "collector exclusion was not held and released during a protected asset read";
    require
      (Reference.equal read_reference committed && String.equal read_bytes original)
      "a collector probe changed the exact protected read";
    let reached_cut = ref false in
    let after_cut state =
      let open Result.Let_syntax in
      let%bind () = authorize state in
      let children = Array.to_list (Stdlib.Sys.readdir dir) in
      if List.count children ~f:(String.is_suffix ~suffix:".sexp") = 1
      then Ok ()
      else (
        if not !reached_cut
        then (
          reached_cut := true;
          require
            (Option.is_none (collect ()))
            "collection entered an in-flight publication");
        Error (P.Error.invalid_request "fixture withdraws authority after real adoption"))
    in
    expect_error
      Denied
      (publish ~authorize:after_cut (capture pending second |> asset_ok));
    expect_error Unavailable (get pending);
    let pending_blob =
      Array.to_list (Stdlib.Sys.readdir dir)
      |> List.find_map_exn ~f:(fun name ->
        match String.chop_suffix name ~suffix:".blob" with
        | Some blob when not (String.equal blob (P.Id.Blob.to_string id)) -> Some blob
        | _ -> None)
    in
    let intents = Filename.concat (S.Handle.directory handle) "recovery-asset-intents" in
    let journal = Filename.concat (S.Handle.directory handle) "recovery-asset-commits" in
    let names directory = Array.to_list (Stdlib.Sys.readdir directory) in
    require
      (List.length (names intents) = 2 && List.length (names journal) = 1)
      "the cutpoint exposed a commit or lost its preparation";
    let read_file path = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
    List.iter [ intents; journal ] ~f:(fun directory ->
      List.iter (names directory) ~f:(fun name ->
        let frame = read_file (Filename.concat directory name) in
        require
          (not (String.is_substring frame ~substring:original))
          "a private journal frame contained original bytes"));
    Eio.Path.save ~create:(`Exclusive 0o600) (orphan handle) "OCHAT";
    ignore
      (Actor.stop entry.actor ~attachment_id ~mode:Graceful |> protocol_ok : P.Session.t);
    Agent_server.Runtime_owner.unload_and_wait entry.runtime |> protocol_ok;
    Http_driver.shutdown client;
    retained := Some (session.id, old, committed, pending, pending_blob, bytes));
  let session_id, citation, committed, pending, pending_blob, earlier_bytes =
    Option.value_exn !retained
  in
  Daemon_host.with_ env fixture ~options (fun sw daemon ->
    let module Registry = Agent_server.Session_registry in
    let factory = Agent_server.Daemon.factory daemon in
    let authorize = authorize (actor_auth daemon) in
    let read citation = Factory.read factory ~sw ~authorize citation ~part:0 ~max_bytes in
    let restored, bytes = read citation |> asset_ok in
    require
      (Reference.equal restored committed && String.equal bytes earlier_bytes)
      "restarted Factory lost the committed descriptor or bytes";
    expect_error Unavailable (read pending);
    let entry =
      Registry.find (Agent_server.Daemon.registry daemon) session_id |> Option.value_exn
    in
    let handle = Option.value_exn entry.store_handle in
    require
      (Eio.Path.is_file (orphan handle))
      "fixture lost the interrupted atomic intent";
    require
      (Option.is_some (entry.collect_results () |> protocol_ok))
      "quiescent Factory collector did not run";
    require
      (not (Eio.Path.is_file (orphan handle)))
      "collector retained an orphan intent temporary";
    expect_error Unavailable (read pending);
    let resumed =
      Factory.resume factory ~sw ~authorize pending ~part:0 ~max_bytes |> asset_ok
    in
    require
      (String.equal pending_blob (P.Id.Blob.to_string (Reference.blob resumed).id))
      "reconciliation rebound the originally selected blob identity";
    let restored, bytes = read pending |> asset_ok in
    require
      (Reference.equal resumed restored && String.equal bytes original)
      "restart did not reconcile the adopted pending asset";
    ignore (read pending |> asset_ok : Reference.t * string))
;;

let export_parent environment =
  let parent =
    Temporary_environment.path
      environment
      (Filename.concat (Temporary_environment.roots environment).temporary "export")
  in
  Eio.Path.mkdir ~perm:0o700 parent;
  parent
;;

let assert_installed parent path (blob : Agent_protocol.Blob.Metadata.t) =
  let installed = Eio.Path.load path in
  require
    (String.equal (digest installed) blob.digest)
    "atomic export success digest differs";
  require
    (Int64.equal (Int64.of_int (String.length installed)) blob.byte_length)
    "atomic export success length differs";
  require
    (List.equal String.equal (Eio.Path.read_dir parent) [ "session.json" ])
    "successful export left a partial file"
;;

let test_export env environment =
  let fixture = fixture env environment "data-export" in
  Daemon_host.with_
    env
    fixture
    ~options:(Daemon_host.with_offline_inference Agent_server.Daemon.default_options)
    (fun sw _ ->
       let client, _ = connect ~sw env fixture (Config_fixture.admin_token fixture) in
       let session, attachment = create client "data-export-create" in
       let blob = export client session attachment in
       let parent = export_parent environment in
       let path = Eio.Path.(parent / "session.json") in
       Eio.Path.save ~create:(`Exclusive 0o600) path "existing export";
       let download output =
         Agent_client.Blob_download.download
           ~connection:(connection client)
           ~session_id:session.id
           ~attachment_id:(Some attachment.id)
           ~blob
           ~output
       in
       Agent_client.Blob_download.install_atomic ~path ~download |> Or_error.ok_exn;
       assert_installed parent path blob;
       Http_driver.shutdown client)
;;
