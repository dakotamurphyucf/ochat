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
    let client, principal = F.connect ~sw env fixture token in
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
    let scope =
      Actor.enable_recovery_windows entry.actor ~actor:proof ~attachment_id ~authorize
      |> protocol_ok
    in
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
    let next = windows ~cursor (authenticate ()) |> protocol_ok in
    require
      (Option.is_none next.next_cursor
       && List.length next.items = 1
       && A.Recovery_window_record.equal_state (List.hd_exn next.items).state Open)
      "a new authentication of the same credential must resume after an append";
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
    Actor.expire_recovery_window entry.actor ~actor:proof ~attachment_id ~authorize scope
    |> protocol_ok;
    expect_expired (windows ~cursor (authenticate ()));
    expect_error Expired (read retained ~part:0);
    Http_driver.shutdown client)
;;
