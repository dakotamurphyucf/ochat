open! Core
module P = Agent_protocol
module A = Agent_session
module Actor = A.Session_actor
module Source = A.Recovery_source_reader
module Asset = Agent_server.Session_factory.Recovery_assets
module F = Data_integrity_http

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "asset converter fixture failure", (error : P.Error.t)]
;;

let key value = P.Idempotency_key.of_string value |> protocol_ok
let require condition message = if not condition then failwith message

let await env f =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    let rec loop () =
      match f () with
      | Some value -> value
      | None ->
        Eio.Fiber.yield ();
        loop ()
    in
    loop ())
;;

let state (entry : Agent_server.Session_registry.entry) =
  Actor.state entry.actor |> protocol_ok
;;

let await_idle env entry =
  await env (fun () ->
    let current = state entry in
    match current.lifecycle.observed, current.active_operation with
    | Idle, None -> Some current
    | _ -> None)
;;

let send
      client
      ~session_id
      ~attachment_id
      ?(timing = P.Pending_input.Timing.Safe_boundary)
      ~name
      kind
      text
  =
  match
    (Http_driver.request
       client
       (Session_send_message
          { session_id
          ; attachment_id
          ; timing
          ; content = { kind; text; attachments = [] }
          ; idempotency_key = key name
          })
     |> protocol_ok)
      .result
    |> Public_view.non_history
  with
  | Session_send_message receipt -> receipt
  | _ -> failwith "expected a real daemon send receipt"
;;

let row state id =
  List.find_exn state.A.Session_state.conversation.canonical_history ~f:(fun row ->
    P.History.Id.equal row.id id)
;;

let citation scope (row : P.History.entry) =
  P.Recovery_window.Citation.create
    scope
    ~occurrence:row.id
    ~version:(P.Recovery_item_version.canonical row.content_revision)
  |> protocol_ok
;;

let text (row : P.History.entry) ~part =
  let open History_entry.Payload in
  let payload = A.History_codec.of_protocol row |> protocol_ok |> History_entry.payload in
  match Semantic.view (semantic payload) with
  | Message { content; _ } ->
    (match List.nth_exn content part with
     | Content.Text { text; _ } -> text
     | _ -> failwith "expected real converted text")
  | _ -> failwith "expected a real canonical message"
;;

let expect_error error = function
  | Error actual when Source.equal_error error actual -> ()
  | Error actual ->
    raise_s
      [%sexp "wrong recovery error", (actual : Source.error), (error : Source.error)]
  | Ok _ -> failwith "unexpected original disclosure"
;;

let image = Base64.decode_exn "R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw=="

let check_history_query factory entry ~attachment_id ~authenticate ~typed ~observer scope =
  let module History = Agent_server.Session_factory.Recovery_history in
  let module Query = A.Recovery_history_query in
  let module Access = Actor.Recovery_history_access in
  let append sequence =
    let id =
      History_entry.Id.create ~namespace:"history-query-fixture" ~sequence
      |> Result.ok_or_failwith
    in
    let text = sprintf "history page fixture row %d" sequence in
    let item = A.History_codec.user_text ~id text |> A.History_codec.to_protocol in
    Actor.append_history entry.Agent_server.Session_registry.actor ~attachment_id [ item ]
    |> protocol_ok
    |> ignore;
    text
  in
  let first = append 0 in
  let second = append 1 in
  let query ?(view = A.Recovery_item_text.Ordinary) term =
    Query.create
      ~source:(P.Recovery_window.Scope.session scope)
      ~scope:(Window scope)
      ~family:Canonical
      ~view
      ~limit:1
      (Search (P.Search_term.create term |> protocol_ok))
    |> protocol_ok
  in
  let selected = query "history page fixture" in
  let wire ?cursor action =
    let page = P.Page.Request.create ~limit:1 ?cursor () |> protocol_ok in
    P.Recovery_history_request.create ~session:(P.Recovery_window.Scope.session scope)
      ~window:scope ~family:Canonical ~page action |> protocol_ok
  in
  let remote ?cursor term =
    Agent_client.Recovery_windows.query typed
      (wire ?cursor (Search (P.Search_term.create term |> protocol_ok)))
  in
  let remote_first = remote "history page fixture" |> protocol_ok in
  let match_text (page : P.Recovery_history_page.t) =
    match page.rows with
    | [ { projection = Match hit; _ } ] -> P.Search_snippet.text hit.snippet
    | _ -> failwith "unexpected recovery search projection"
  in
  let remote_cursor = Option.value_exn remote_first.next in
  require (String.equal first (match_text remote_first)) "wrong remote search result";
  (match Http_driver.request_without_history observer (Recovery_history (wire List_items)) with
   | Error failure when P.Error.equal_code failure.code Method_not_found -> ()
   | _ -> failwith "window negotiation granted history query access");
  (match (Agent_client.Recovery_windows.query typed (wire List_items) |> protocol_ok).rows with
   | [ { projection = Text_parts _; _ } ] -> ()
   | _ -> failwith "list did not return safe part metadata");

  let run ?cursor ?on_release ?authorize_view actor query =
    History.query factory ~access:(Access.operator actor) ?authorize_view ?cursor ?on_release query
  in
  let field json name =
    P.Json_codec.fields json
    |> protocol_ok
    |> fun fields -> P.Json_codec.required fields name |> protocol_ok
  in
  let snippets page =
    P.Json_codec.list Result.return (field page "rows")
    |> protocol_ok
    |> List.map ~f:(fun row ->
      P.Search_snippet.of_json (field row "snippet")
      |> protocol_ok
      |> P.Search_snippet.text)
  in
  let token page = P.Page.Cursor.of_json (field page "next") |> protocol_ok in
  let live = ref true in
  let actor =
    Operator_authorization.restrict (authenticate ()) ~is_current:(fun () -> !live)
  in
  let released = ref None in
  let page =
    run actor ~on_release:(fun check -> released := Some check) selected |> protocol_ok
  in
  let check = Option.value_exn !released in
  require (List.equal String.equal (snippets page) [ first ]) "wrong first history page";
  let cursor = token page in
  let repeated = run actor selected |> protocol_ok |> token in
  require (not (P.Page.Cursor.equal cursor repeated)) "history cursor reused its nonce";
  let expect code = function
    | Error actual when P.Error.equal_code code actual.P.Error.code -> ()
    | _ -> failwith "history query admitted an invalid continuation or view"
  in
  let foreign =
    Operator_authorization.trusted_local (Operator_authorization.principal actor)
  in
  expect Cursor_expired (remote ~cursor:remote_cursor "different literal");
  expect Cursor_expired (run ~cursor foreign selected);
  expect Cursor_expired (run ~cursor actor (query "irrelevant"));
  expect
    Permission_denied
    (run actor (query ~view:Selected_developer "history page fixture"));
  let blocked = ref None in
  let authorize_view _principal _state _view ~selection =
    match selection, !blocked with
    | Some (A.Recovery_item_text.Canonical actual), Some expected
      when P.Recovery_window.Citation.equal actual expected ->
      Error (P.Error.create Permission_denied ~message:"item permission revoked" ~retryable:false ())
    | _ -> Ok ()
  in
  let item_release = ref None in
  let guarded =
    run actor selected ~authorize_view ~on_release:(fun check -> item_release := Some check)
    |> protocol_ok
  in
  let row = P.Json_codec.list Result.return (field guarded "rows") |> protocol_ok |> List.hd_exn in
  blocked := Some (P.Recovery_window.Citation.of_json (field row "citation") |> protocol_ok);
  let item_check = Option.value_exn !item_release in
  require (not (item_check ())) "delivery ignored revocation for the selected item";
  expect Permission_denied (run ~authorize_view actor selected);
  let tampered =
    let text = P.Page.Cursor.to_string cursor in
    let first = if Char.equal text.[0] 'A' then "B" else "A" in
    P.Page.Cursor.of_string (first ^ String.drop_prefix text 1) |> protocol_ok
  in
  expect Cursor_expired (run ~cursor:tampered actor selected);
  ignore (append 2 : string);
  require (check ()) "an ordinary append invalidated released history";
  let remote_next = remote ~cursor:remote_cursor "history page fixture" |> protocol_ok in
  require (String.equal second (match_text remote_next) && Option.is_none remote_next.next)
    "remote continuation drifted across append";
  let resumed = run ~cursor (authenticate ()) selected |> protocol_ok in
  require
    (List.equal String.equal (snippets resumed) [ second ]
     && Document_schema.Json.equal (field resumed "next") `Null)
    "history cursor did not preserve the exact append-stable highwater";
  live := false;
  require
    (not (check ()))
    "the delivery proof ignored revocation of its original credential"
;;

let archive_only entry proof attachment_id authorize scope =
  let module R = A.Recovery_rollover in
  let host =
    A.Session_context_capacity.Host.create
      ~mode_for_session:(fun session ->
        Option.some_if
          (P.Session_ref.equal session (P.Recovery_window.Scope.session scope))
          A.Session_context_capacity.Mode.Manual)
      ~targets:[]
    |> Result.map_error ~f:(fun _ -> "invalid retention test host")
    |> Result.ok_or_failwith
  in
  Actor.set_context_capacity_host entry.Agent_server.Session_registry.actor host
  |> protocol_ok;
  let expected () =
    R.Expected.create scope ~revision:(state entry).counters.revision |> protocol_ok
  in
  let cut =
    Actor.request_context_rollover
      entry.actor
      ~actor:proof
      ~attachment_id
      ~authorize
      ~authorize_force:authorize
      ~key:(key "actual-converter-rollover")
      (expected ())
    |> protocol_ok
  in
  Actor.settle_context_rollover
    entry.actor
    ~actor:proof
    ~attachment_id
    ~authorize
    ~authorize_force:authorize
    ~id:cut.intent.id
    (expected ())
    (Publish { checkpoint = None; retain = [] })
  |> protocol_ok
  |> ignore
;;

let run env environment =
  let fixture = F.fixture env environment "data-recovery-asset-converter" in
  let workspace = Config_fixture.physical_workspace fixture in
  let save name bytes =
    let native = Filename.concat workspace name in
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      (Temporary_environment.path environment native)
      bytes;
    native
  in
  let first = "<p>Equivalent</p><!-- first private original -->"
  and second = "<p>Equivalent</p><!-- second private original -->" in
  let first_path = save "first.html" first
  and second_path = save "second.html" second
  and image_path = save "original.gif" image
  and invalid_path = save "invalid.gif" "bytes that are not a GIF" in
  let doc path = sprintf "<doc local markdown src=%S/>" path in
  let img path = sprintf "<img local src=%S/>" path in
  let user parts = "<user>" ^ String.concat parts ^ "</user>" in
  let token = Config_fixture.admin_token fixture in
  let options = Daemon_host.with_offline_inference Agent_server.Daemon.default_options in
  Daemon_host.with_ env fixture ~options (fun sw daemon ->
    let client =
      Http_driver.create
        ~sw
        ~env
        ~port:(Config_fixture.http_port fixture)
        ~token:(Some token)
      |> Result.ok_or_failwith
    in
    let typed = F.connection client in
    let negotiated =
      Agent_client.Session_handle.initialize
        typed
        ~implementation_name:"recovery-integration"
        ~implementation_version:"dev"
      |> protocol_ok
    in
    let principal = negotiated.principal.id in
    let session, attachment = F.create ~mode:Owner_read_write client "real-converter" in
    let attachment_id = attachment.id in
    let entry =
      Agent_server.Session_registry.find (Agent_server.Daemon.registry daemon) session.id
      |> Option.value_exn
    in
    let authenticate () =
      Agent_server.Daemon.authenticate_http_actor
        daemon
        { client_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0); headers = [] }
        (Some token)
      |> protocol_ok
    in
    let proof = authenticate () in
    let authorize (state : A.Session_state.t) =
      if
        Operator_authorization.is_current proof
        && Option.exists
             state.identity.creating_principal
             ~f:(P.Id.Principal.equal principal)
      then Ok ()
      else Error (P.Error.invalid_request "authentic source owner expired")
    in
    let factory = Agent_server.Daemon.factory daemon in
    let read citation ~part =
      Asset.read factory ~sw ~authorize citation ~part ~max_bytes:1024
    in
    let send = send client ~session_id:session.id ~attachment_id in
    Http_driver.request
      client
      (Session_start
         { session_id = session.id
         ; attachment_id
         ; queue_if_limited = false
         ; idempotency_key = key "actual-converter-start"
         })
    |> protocol_ok
    |> ignore;
    ignore (await_idle env entry : A.Session_state.t);
    let next_pause = ref None in
    let hold_next () =
      let entered, notify = Eio.Promise.create () in
      let released, release = Eio.Promise.create () in
      next_pause := Some (notify, released);
      entered, release
    in
    let worker =
      A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
        let pause = !next_pause in
        next_pause := None;
        Option.iter pause ~f:(fun (notify, released) ->
          Eio.Promise.resolve notify ();
          Eio.Promise.await released);
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = None
          })
    in
    Actor.set_operation_worker entry.actor (Some worker) |> protocol_ok;
    let ordinary = send ~name:"converter-disabled" Chatmd (user [ doc first_path ]) in
    let idle = await_idle env entry in
    let legacy = row idle ordinary.history_id in
    require
      (String.is_substring (text legacy ~part:0) ~substring:"Equivalent")
      "ordinary converted text was lost";
    let owner =
      P.Session_ref.create
        ~server_id:
          (Agent_store.Session_store.server_id (Agent_server.Daemon.store daemon))
        ~session_id:session.id
    in
    let enable connection expected_generation =
      Http_driver.request_without_history
        connection
        (Recovery_enable { session = owner; attachment_id; expected_generation })
    in
    let enabled () =
      Agent_client.Recovery_windows.enable
        typed
        { session = owner; attachment_id; expected_generation = idle.identity.generation }
      |> protocol_ok
    in
    let scope = enabled () in
    require
      (P.Recovery_window.Scope.equal scope (enabled ()))
      "activation retry created another window";
    (match enable client (idle.identity.generation + 1) with
     | Error failure when P.Error.equal_code failure.code Conflict -> ()
     | _ -> failwith "stale writer generation was accepted");
    let observer, _ = F.connect ~sw env fixture token in
    let request =
      P.Recovery_query.List_windows.
        { session = owner
        ; generation = None
        ; state = None
        ; page = P.Page.Request.create ~limit:1 () |> protocol_ok
        }
    in
    (match Http_driver.request_without_history observer (Recovery_windows request) with
     | Error failure when P.Error.equal_code failure.code Method_not_found -> ()
     | _ -> failwith "recovery was available without negotiating it");
    Http_driver.shutdown observer;
    let observer, _ =
      F.connect ~sw ~features:[ P.Recovery_query.Features.windows ] env fixture token
    in
    (match enable observer idle.identity.generation with
     | Error _ -> ()
     | Ok _ -> failwith "another connection reused the owner's attachment");
    expect_error Unavailable (read (citation scope legacy) ~part:0);
    let entered, release = hold_next () in
    ignore (send ~name:"converter-hold" Plain_text "hold before adoption" : _);
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Promise.await entered);
    let pending =
      send
        ~timing:After_current_operation
        ~name:"converter-deferred"
        Chatmd
        (user [ doc first_path; doc second_path; img image_path; img invalid_path ])
    in
    let deferred = state entry in
    require
      ((not
          (List.exists deferred.conversation.canonical_history ~f:(fun row ->
             P.History.Id.equal row.id pending.history_id)))
       && List.exists deferred.conversation.deferred_user_entries ~f:(fun value ->
         P.History.Id.equal (A.Pending_input_document.entry value).id pending.history_id)
      )
      "the actual converted send did not enter the durable pending queue";
    ignore (save "first.html" "<p>Replacement after the converter returned</p>" : string);
    Eio.Promise.resolve release ();
    let adopted =
      await env (fun () ->
        let current = state entry in
        List.find current.conversation.canonical_history ~f:(fun row ->
          P.History.Id.equal row.id pending.history_id))
    in
    require
      (String.equal (text adopted ~part:0) (text adopted ~part:1))
      "distinct original documents did not emit identical Markdown";
    let retained = citation scope adopted in
    let recovered part expected =
      let _, found =
        await env (fun () ->
          match read retained ~part with
          | Ok result -> Some result
          | Error (Source.Unavailable | Denied) -> None
          | Error error ->
            raise_s [%sexp "unable to read exact original", (error : Source.error)])
      in
      require
        (String.equal found expected)
        "original bytes crossed parts or were reloaded"
    in
    recovered 0 first;
    recovered 1 second;
    recovered 2 image;
    expect_error Corrupt (read retained ~part:3);
    ignore (await_idle env entry : A.Session_state.t);
    let entered, release = hold_next () in
    ignore
      (send ~name:"converter-revocation-hold" Plain_text "hold before revocation" : _);
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Promise.await entered);
    let revoked =
      send
        ~timing:After_current_operation
        ~name:"converter-revoked"
        Chatmd
        (user [ doc second_path ])
    in
    Http_driver.request
      client
      (Session_detach
         { session_id = session.id
         ; attachment_id
         ; idempotency_key = key "converter-detach"
         })
    |> protocol_ok
    |> ignore;
    Eio.Promise.resolve release ();
    let settled =
      await env (fun () ->
        let current = state entry in
        match current.lifecycle.observed, current.active_operation with
        | Idle, None ->
          List.find current.conversation.canonical_history ~f:(fun row ->
            P.History.Id.equal row.id revoked.history_id)
        | _ -> None)
    in
    require
      (String.equal (text settled ~part:0) (text adopted ~part:1))
      "detaching the original writer changed the deferred canonical text";
    expect_error Unavailable (read (citation scope settled) ~part:0);
    let attachment_id =
      match
        (Http_driver.request
           client
           (Session_attach
              { session_id = session.id
              ; requested_mode = Owner_read_write
              ; subscribe = false
              ; after_sequence = None
              ; reclaim_token = None
              ; idempotency_key = key "converter-reattach"
              })
         |> protocol_ok)
          .result
      with
      | Session_attach attached -> attached.attachment.id
      | _ -> failwith "expected a real replacement attachment"
    in
    archive_only entry proof attachment_id authorize scope;
    let windows ?cursor actor =
      Agent_server.Session_factory.Recovery_history.list_windows
        factory
        ~actor
        ~session:(P.Recovery_window.Scope.session scope)
        (P.Page.Request.create ~limit:1 ?cursor () |> protocol_ok)
    in
    let page = windows proof |> protocol_ok in
    let predecessor = List.hd_exn page.items in
    require
      (P.Recovery_window.Scope.equal predecessor.scope scope
       && A.Recovery_window_record.equal_state predecessor.state Sealed)
      "the first page must name the sealed predecessor";
    let remote ?cursor () =
      let page = P.Page.Request.create ~limit:1 ?cursor () |> protocol_ok in
      Agent_client.Recovery_windows.list typed { request with page } |> protocol_ok
    in
    let remote_first = remote () in
    require
      (P.Recovery_window.Scope.equal (List.hd_exn remote_first.items).scope scope)
      "remote predecessor mismatch";
    let remote_cursor = Option.value_exn remote_first.next_cursor in
    let cursor = Option.value_exn page.next_cursor in
    let independent =
      Operator_authorization.trusted_local (Operator_authorization.principal proof)
    in
    let expect_expired = function
      | Error error when P.Error.equal_code error.P.Error.code Cursor_expired -> ()
      | _ -> failwith "a changed window query basis accepted the old cursor"
    in
    expect_expired (windows ~cursor independent);
    let append_id =
      History_entry.Id.create ~namespace:"window-page-test" ~sequence:0
      |> Result.ok_or_failwith
    in
    let appended =
      A.History_codec.user_text ~id:append_id "append between pages"
      |> A.History_codec.to_protocol
    in
    Actor.append_history entry.actor ~attachment_id [ appended ] |> protocol_ok |> ignore;
    let remote_next = remote ~cursor:remote_cursor () in
    require
      (Option.is_none remote_next.next_cursor && List.length remote_next.items = 1)
      "remote continuation missed successor";
    let next = windows ~cursor (authenticate ()) |> protocol_ok in
    require
      (Option.is_none next.next_cursor
       && List.length next.items = 1
       && A.Recovery_window_record.equal_state (List.hd_exn next.items).state Open)
      "a new authentication of the same credential must resume after an append";
    check_history_query factory entry ~attachment_id ~authenticate ~typed ~observer (List.hd_exn next.items).scope;
    Http_driver.shutdown observer;
    let archived = state entry in
    require
      (Option.is_none
         (List.find archived.conversation.canonical_history ~f:(fun row ->
            P.History.Id.equal row.id adopted.id)))
      "the real rollover did not prune the converted occurrence";
    recovered 0 first;
    recovered 1 second;
    recovered 2 image;
    expect_error
      Denied
      (Asset.read
         factory
         ~sw
         ~authorize:(fun _ -> Error (P.Error.invalid_request "withdrawn"))
         retained
         ~part:0
         ~max_bytes:1024);
    (* Hold a fully dispatched response as a slow stdio writer or HTTP batch
       would. Withdraw the original source before handing any bytes to it. *)
    let module Server = Agent_server in
    let module D = Server.Dispatcher in
    let context =
      Server.Connection_context.create_authenticated
        ~actor:proof
        ~connection_id:"held-recovery-result"
        ~transport:Stdio
        ~publish_notification:ignore
        ~max_attachments:1
    in
    let dispatcher = Server.Daemon.dispatcher daemon in
    let local =
      Agent_client.Transport.create
        ~request:(D.dispatch_command dispatcher ~actor:proof ~context)
        ~next_notification:(fun () -> None)
        ~close:ignore
      |> Agent_client.Connection.create
    in
    ignore
      (Agent_client.Session_handle.initialize
         local
         ~implementation_name:"held-writer"
         ~implementation_version:"dev"
       |> protocol_ok
       : P.Initialize.Response.t);
    let id = P.Envelope.Request_id.of_json (`String "held-window-page") |> protocol_ok in
    let hold command =
      D.dispatch_output
        dispatcher
        ~actor:proof
        ~context
        (P.Envelope.request
           ~id
           ~method_:(P.Command.method_name command)
           ~params:(P.Command.params command)
           ())
      |> protocol_ok
      |> Option.value_exn
    in
    let query =
      P.Recovery_history_request.create ~session:owner ~window:scope ~family:Canonical
        ~page:request.page List_items |> protocol_ok
    in
    let held = [ hold (P.Command.Recovery_windows request); hold (Recovery_history query) ] in
    List.iter held ~f:(fun output ->
      require (D.Output.requires_check output) "queued recovery output lost its release guard");
    Actor.expire_recovery_window entry.actor ~actor:proof ~attachment_id ~authorize scope
    |> protocol_ok;
    List.iter held ~f:(fun output ->
      match D.Output.materialize output with
      | Response { id = actual; outcome = Error failure }
        when P.Envelope.Request_id.compare id actual = 0
             && P.Error.equal_code failure.code Permission_denied -> ()
      | _ -> failwith "queued response disclosed an expired window");
    D.close_connection_and_wait dispatcher context |> protocol_ok;
    expect_expired (windows ~cursor (authenticate ()));
    expect_error Expired (read retained ~part:0);
    Http_driver.shutdown client)
;;
