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

let request client command = (Http_driver.request client command |> protocol_ok).result
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

let connect ~sw env fixture token =
  let client =
    Http_driver.create
      ~sw
      ~env
      ~port:(Config_fixture.http_port fixture)
      ~token:(Some token)
    |> string_ok
  in
  let initialized, _ = Http_driver.initialize client |> protocol_ok in
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

let create client name =
  match
    request
      client
      (Session_create
         { spec = spec client
         ; requested_mode = Some Read_write
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
         ; attachment_id = attachment.id
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
      ; attachment_id = attachment.id
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
    (request client (Session_get { session_id = session.id; history = None })
     : Agent_protocol.Method_result.t);
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
  Daemon_host.with_ env fixture ~options:Agent_server.Daemon.default_options (fun sw _ ->
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
  let wrong = upload client token ~extra:[ "ochat-sha256", digest "wrong" ] body in
  assert_upload_rejection wrong "blob digest differs from the expected digest";
  let large = upload client token (body ^ "x") in
  assert_upload_rejection large "blob upload exceeds configured maximum";
  require
    (List.equal String.equal before (temporary_files environment daemon))
    "rejected HTTP upload leaked partial or metadata files"
;;

let test_blobs env environment =
  let fixture = fixture env environment "data-blobs" in
  restrict_public fixture;
  let options = Agent_server.Daemon.default_options in
  let options =
    { options with
      protocol_limits = { options.protocol_limits with max_request_bytes = 4096 }
    }
  in
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
    Http_driver.shutdown owner;
    Http_driver.shutdown foreign)
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
  Daemon_host.with_ env fixture ~options:Agent_server.Daemon.default_options (fun sw _ ->
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
        ~attachment_id:attachment.id
        ~blob
        ~output
    in
    Agent_client.Blob_download.install_atomic ~path ~download |> Or_error.ok_exn;
    assert_installed parent path blob;
    Http_driver.shutdown client)
;;
