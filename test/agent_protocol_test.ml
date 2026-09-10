open Core
open Agent_protocol

let print_result result ~ok =
  match result with
  | Ok value -> print_s [%sexp (ok value : string)]
  | Error (error : Error.t) -> print_s [%sexp (Error.code_to_string error.code : string)]
;;

let ok_or_fail = function
  | Ok value -> value
  | Error (error : Error.t) -> failwith error.message
;;

let deterministic_generator () =
  Id.Generator.create ~bytes:(fun length -> String.make length '\000')
;;

let%expect_test "opaque identifiers are typed and deterministic generators are injectable"
  =
  let generator = deterministic_generator () in
  let session = Id.Session.create_with generator in
  print_endline (Id.Session.to_string session);
  print_result
    (Id.Session.of_string (Id.Session.to_string session))
    ~ok:Id.Session.to_string;
  print_result
    (Id.Operation.of_string (Id.Session.to_string session))
    ~ok:Id.Operation.to_string;
  [%expect
    {|
    ses_AAAAAAAAAAAAAAAAAAAAAAAA
    ses_AAAAAAAAAAAAAAAAAAAAAAAA
    invalid_request |}]
;;

let%expect_test "version negotiation selects the highest compatible minor" =
  let version major minor =
    match Version.create ~major ~minor with
    | Ok version -> version
    | Error error -> failwith error.message
  in
  let result =
    Version.negotiate
      ~client_min:(version 1 0)
      ~client_max:(version 1 3)
      ~supported:[ version 1 0; version 1 2; version 2 0 ]
  in
  (match result with
   | Ok version -> print_s [%sexp (version : Version.t)]
   | Error error -> print_endline (Error.code_to_string error.code));
  print_result
    (Version.negotiate
       ~client_min:(version 2 0)
       ~client_max:(version 2 1)
       ~supported:[ version 1 0 ])
    ~ok:(fun version -> Sexp.to_string ([%sexp_of: Version.t] version));
  [%expect
    {|
    ((major 1) (minor 2))
    incompatible_protocol |}]
;;

let%expect_test "strict JSON helpers reject duplicates and canonicalize object order" =
  let duplicate = `Object [ "name", `String "first"; "name", `String "second" ] in
  print_result (Json_codec.fields duplicate) ~ok:(fun _ -> "accepted");
  let json =
    `Object [ "z", `Number "1"; "a", `Object [ "d", `True; "b", `Array [ `String "x" ] ] ]
  in
  (match Json_codec.canonical_string json with
   | Ok encoded -> print_endline encoded
   | Error error -> print_endline (Error.code_to_string error.code));
  List.iter [ "1"; "1.0"; "1e0" ] ~f:(fun encoded_number ->
    match Json_codec.canonical_string (`Number encoded_number) with
    | Ok encoded -> print_endline encoded
    | Error error -> print_endline (Error.code_to_string error.code));
  [%expect
    {|
    invalid_request
    {"a":{"b":["x"],"d":true},"z":1}
    1
    1.0
    1e0 |}]
;;

let request_id value =
  match Envelope.Request_id.of_json value with
  | Ok id -> id
  | Error error -> failwith error.message
;;

let%expect_test "request and error response envelopes round trip" =
  let request =
    Envelope.request
      ~id:(request_id (`String "cmd-42"))
      ~method_:"session.send_message"
      ~params:(`Object [ "text", `String "Run the tests" ])
      ()
  in
  let encoded = Envelope.to_json request in
  print_endline (Jsonaf.to_string encoded);
  let decoded =
    match Envelope.of_json encoded with
    | Ok decoded -> decoded
    | Error error -> failwith error.message
  in
  print_endline (Jsonaf.to_string (Envelope.to_json decoded));
  let error = Error.invalid_request "bad input" in
  let response = Envelope.failure ~id:(request_id (`Number "7")) error in
  print_endline (Jsonaf.to_string (Envelope.to_json response));
  [%expect
    {|
    {"jsonrpc":"2.0","id":"cmd-42","method":"session.send_message","params":{"text":"Run the tests"}}
    {"jsonrpc":"2.0","id":"cmd-42","method":"session.send_message","params":{"text":"Run the tests"}}
    {"jsonrpc":"2.0","id":7,"error":{"code":"invalid_request","message":"bad input","retryable":false,"data":{}}} |}]
;;

let%expect_test "envelope decoding rejects ambiguous responses" =
  let json =
    `Object
      [ "jsonrpc", `String "2.0"
      ; "id", `String "cmd-1"
      ; "result", `Object []
      ; "error", Error.to_json (Error.invalid_request "bad")
      ]
  in
  print_result (Envelope.of_json json) ~ok:(fun _ -> "accepted");
  [%expect {| invalid_request |}]
;;

let%expect_test "timestamps use RFC 3339 UTC and reject offsets" =
  let epoch = Timestamp.of_time_ns Time_ns.epoch in
  print_endline (Timestamp.to_string epoch);
  print_result
    (Timestamp.of_string "2026-08-15T12:34:56.123456789Z")
    ~ok:Timestamp.to_string;
  print_result (Timestamp.of_string "2026-08-15T12:34:56-05:00") ~ok:Timestamp.to_string;
  [%expect
    {|
    1970-01-01T00:00:00.000000000Z
    2026-08-15T12:34:56.123456789Z
    invalid_request |}]
;;

let%expect_test "principal scopes and attributes have stable strict codecs" =
  let generator = deterministic_generator () in
  let principal =
    Principal.create
      ~id:(Id.Principal.create_with generator)
      ~authentication_kind:"local.peer"
      ~scopes:(Scope.Set.of_list [ Scope.Send_messages; Scope.View_session_transcript ])
      ~attributes:[ "uid", "501"; "display_name", "Dakota" ]
    |> ok_or_fail
  in
  let encoded = Principal.to_json principal in
  print_endline (Jsonaf.to_string encoded);
  let decoded = Principal.of_json encoded |> ok_or_fail in
  print_s
    [%sexp
      { can_send = (Principal.has_scope decoded Scope.Send_messages : bool)
      ; can_delete = (Principal.has_scope decoded Scope.Delete_sessions : bool)
      }];
  let duplicate_scopes =
    `Array [ Scope.to_json Scope.Send_messages; Scope.to_json Scope.Send_messages ]
  in
  print_result (Scope.set_of_json duplicate_scopes) ~ok:(fun _ -> "accepted");
  [%expect
    {|
    {"id":"pri_AAAAAAAAAAAAAAAAAAAAAAAA","authentication_kind":"local.peer","scopes":["session.transcript.read","session.message.send"],"attributes":{"display_name":"Dakota","uid":"501"}}
    ((can_send true) (can_delete false))
    invalid_request |}]
;;

let%expect_test "page requests validate limits and page responses round trip" =
  let cursor = Page.Cursor.of_string "cursor.v1_abc" |> ok_or_fail in
  let request = Page.Request.create ~limit:25 ~cursor () |> ok_or_fail in
  print_endline (Jsonaf.to_string (Page.Request.to_json request));
  print_result (Page.Request.create ~limit:0 ()) ~ok:(fun _ -> "accepted");
  print_result
    (Page.Cursor.of_string (String.of_char_list [ Char.of_int_exn 0xC3 ]))
    ~ok:Page.Cursor.to_string;
  let page : string Page.t = { items = [ "a"; "b" ]; next_cursor = Some cursor } in
  let encoded = Page.to_json (fun value -> `String value) page in
  print_endline (Jsonaf.to_string encoded);
  let decoded = Page.of_json Json_codec.string encoded |> ok_or_fail in
  print_s [%sexp (decoded : string Page.t)];
  [%expect
    {|
    {"limit":25,"cursor":"cursor.v1_abc"}
    invalid_request
    invalid_request
    {"items":["a","b"],"next_cursor":"cursor.v1_abc"}
    ((items (a b)) (next_cursor (cursor.v1_abc))) |}]
;;

let%expect_test
    "prompt summaries redact paths and list requests ignore optional additions"
  =
  let generator = deterministic_generator () in
  let prompt : Prompt.t =
    { id = Id.Prompt_definition.create_with generator
    ; name = "coding-agent"
    ; description = Some "Repository coding agent"
    ; enabled = true
    ; availability = Available
    ; current_revision = Some (Id.Prompt_revision.create_with generator)
    ; allowed_workspaces = [ Id.Workspace_definition.create_with generator ]
    ; permission_profile = "interactive"
    ; runtime_policy = None
    }
  in
  let encoded = Prompt.to_json prompt in
  print_endline (Jsonaf.to_string encoded);
  ignore (Prompt.of_json encoded |> ok_or_fail : Prompt.t);
  let request_json =
    `Object
      [ "limit", `Number "10"; "enabled", `True; "future_filter", `String "ignored" ]
  in
  let request = Prompt.List_request.of_json request_json |> ok_or_fail in
  print_endline (Jsonaf.to_string (Prompt.List_request.to_json request));
  [%expect
    {|
    {"id":"prd_AAAAAAAAAAAAAAAAAAAAAAAA","name":"coding-agent","description":"Repository coding agent","enabled":true,"current_revision":"prv_AAAAAAAAAAAAAAAAAAAAAAAA","allowed_workspaces":["wsd_AAAAAAAAAAAAAAAAAAAAAAAA"],"permission_profile":"interactive","availability":"available"}
    {"limit":10,"enabled":true} |}]
;;

let%expect_test "workspace summaries expose policy without native roots" =
  let generator = deterministic_generator () in
  let prompt_id = Id.Prompt_definition.create_with generator in
  let workspace : Workspace.t =
    { id = Id.Workspace_definition.create_with generator
    ; name = "scratch"
    ; kind = Temporary
    ; temporary_location = Some Session_dir
    ; cleanup = Some On_session_delete
    ; access = Exclusive
    ; conflict_domain = Some "scratch-agents"
    ; prompt_limits = [ { prompt_id; max_root_agents = 8; overflow = Reject } ]
    ; availability = Available
    }
  in
  let encoded = Workspace.to_json workspace in
  print_endline (Jsonaf.to_string encoded);
  ignore (Workspace.of_json encoded |> ok_or_fail : Workspace.t);
  let invalid =
    `Object
      [ "id", Id.Workspace_definition.to_json workspace.id
      ; "name", `String "scratch"
      ; "kind", `String "physical"
      ; "temporary_location", `String "session_dir"
      ; "cleanup", `String "retain"
      ; "access", `String "shared_write"
      ; "prompt_limits", `Array []
      ; "availability", `String "available"
      ]
  in
  print_result (Workspace.of_json invalid) ~ok:(fun _ -> "accepted");
  [%expect
    {|
    {"id":"wsd_AAAAAAAAAAAAAAAAAAAAAAAA","name":"scratch","kind":"temporary","temporary_location":"session_dir","cleanup":"on_session_delete","access":"exclusive","conflict_domain":"scratch-agents","prompt_limits":[{"prompt_id":"prd_AAAAAAAAAAAAAAAAAAAAAAAA","max_root_agents":8,"overflow":"reject"}],"availability":"available"}
    invalid_request |}]
;;

let%expect_test "operation summaries preserve reason, state, and generation" =
  let generator = deterministic_generator () in
  let operation : Operation.t =
    { id = Id.Operation.create_with generator
    ; generation = 3
    ; kind = Turn Recovery_retry
    ; state = Interrupted { reason = "daemon restarted"; retryable = true }
    ; started_at = Timestamp.of_time_ns Time_ns.epoch
    ; updated_at = Timestamp.of_time_ns Time_ns.epoch
    }
  in
  let encoded = Operation.to_json operation in
  print_endline (Jsonaf.to_string encoded);
  ignore (Operation.of_json encoded |> ok_or_fail : Operation.t);
  [%expect
    {|
    {"id":"op_AAAAAAAAAAAAAAAAAAAAAAAA","generation":3,"kind":{"type":"turn","reason":"recovery_retry"},"state":{"type":"interrupted","reason":"daemon restarted","retryable":true},"started_at":"1970-01-01T00:00:00.000000000Z","updated_at":"1970-01-01T00:00:00.000000000Z"} |}]
;;

let daemon_spec () =
  let generator = deterministic_generator () in
  Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog (Id.Prompt_definition.create_with generator))
    ~workspace:(Configured (Id.Workspace_definition.create_with generator))
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"interactive"
    ~start_immediately:true
    ~display_name:"repository agent"
    ~labels:[ "team", "compiler"; "environment", "test" ]
    ()
  |> ok_or_fail
;;

let session_summary () : Session.t =
  let generator = deterministic_generator () in
  let timestamp = Timestamp.of_time_ns Time_ns.epoch in
  { id = Id.Session.create_with generator
  ; creator = Some (Id.Principal.create_with generator)
  ; created_at = timestamp
  ; updated_at = timestamp
  ; generation = 0
  ; spec = daemon_spec ()
  ; desired_state = Running
  ; observed_state = Idle
  ; prompt_revision = Some (Id.Prompt_revision.create_with generator)
  ; workspace_instance = Some (Id.Workspace_instance.create_with generator)
  ; active_operation = None
  ; revision = 4L
  ; latest_event_sequence = 9L
  }
;;

let%expect_test "session specifications separate host, liveness, and persistence" =
  let spec = daemon_spec () in
  let encoded = Session.Spec.to_json spec in
  print_endline (Jsonaf.to_string encoded);
  ignore (Session.Spec.of_json encoded |> ok_or_fail : Session.Spec.t);
  let generator = deterministic_generator () in
  print_result
    (Session.Spec.create
       ~execution_host:Daemon
       ~prompt:(Catalog (Id.Prompt_definition.create_with generator))
       ~workspace:Current
       ~liveness:Detached
       ~persistence:Transient
       ~start_immediately:false
       ~labels:[]
       ())
    ~ok:(fun _ -> "accepted");
  [%expect
    {|
    {"execution_host":"daemon","prompt":{"type":"catalog","prompt_id":"prd_AAAAAAAAAAAAAAAAAAAAAAAA"},"workspace":{"type":"configured","workspace_id":"wsd_AAAAAAAAAAAAAAAAAAAAAAAA"},"liveness":{"type":"detached"},"persistence":"durable","permission_profile":"interactive","start_immediately":true,"display_name":"repository agent","labels":{"environment":"test","team":"compiler"}}
    invalid_request |}]
;;

let%expect_test "owner leases belong only to owner attachments" =
  let generator = deterministic_generator () in
  let lease : Session.Owner_lease.t =
    { generation = 7L
    ; expires_at = Timestamp.of_time_ns Time_ns.epoch
    ; disconnect_grace_until = None
    ; principal_id = None
    ; reclaim_token_sha256 = None
    }
  in
  let owner : Session.Attachment.t =
    { id = Id.Attachment.create_with generator
    ; session_id = Id.Session.create_with generator
    ; mode = Owner_read_write
    ; owner_lease = Some lease
    }
  in
  let encoded = Session.Attachment.to_json owner in
  print_endline (Jsonaf.to_string encoded);
  ignore (Session.Attachment.of_json encoded |> ok_or_fail : Session.Attachment.t);
  let invalid =
    `Object
      [ "id", Id.Attachment.to_json owner.id
      ; "session_id", Id.Session.to_json owner.session_id
      ; "mode", `String "read_only"
      ; "owner_lease", Session.Owner_lease.to_json lease
      ]
  in
  print_result (Session.Attachment.of_json invalid) ~ok:(fun _ -> "accepted");
  [%expect
    {|
    {"id":"att_AAAAAAAAAAAAAAAAAAAAAAAA","session_id":"ses_AAAAAAAAAAAAAAAAAAAAAAAA","mode":"owner_read_write","owner_lease":{"generation":7,"expires_at":"1970-01-01T00:00:00.000000000Z"}}
    invalid_request |}]
;;

let%expect_test "owner lease state decodes records written before reclaim tokens" =
  let lease : Session.Owner_lease.t =
    { generation = 7L
    ; expires_at = Timestamp.of_time_ns Time_ns.epoch
    ; disconnect_grace_until = None
    ; principal_id = None
    ; reclaim_token_sha256 = None
    }
  in
  let legacy =
    match Session.Owner_lease.sexp_of_t lease with
    | Sexp.List fields ->
      Sexp.List
        (List.filter fields ~f:(function
           | Sexp.List (Sexp.Atom name :: _) ->
             not
               (List.mem
                  [ "principal_id"; "reclaim_token_sha256" ]
                  name
                  ~equal:String.equal)
           | Sexp.Atom _ | Sexp.List [] | Sexp.List (Sexp.List _ :: _) -> true))
    | Sexp.Atom _ -> assert false
  in
  let decoded = Session.Owner_lease.t_of_sexp legacy in
  print_s
    [%sexp
      { generation = (decoded.generation : int64)
      ; principal_id_present = (Option.is_some decoded.principal_id : bool)
      ; reclaim_digest_present = (Option.is_some decoded.reclaim_token_sha256 : bool)
      }];
  [%expect
    {|
    ((generation 7) (principal_id_present false) (reclaim_digest_present false))
    |}]
;;

let%expect_test "session message commands accept legacy text and dispatch once" =
  let generator = deterministic_generator () in
  let params =
    `Object
      [ "session_id", Id.Session.to_json (Id.Session.create_with generator)
      ; "attachment_id", Id.Attachment.to_json (Id.Attachment.create_with generator)
      ; "text", `String "Run the tests"
      ; "idempotency_key", `String "client-a:938"
      ; "future_option", `True
      ]
  in
  let command =
    Command.of_method_and_params ~method_:"session.send_message" ~params |> ok_or_fail
  in
  print_endline (Command.method_name command);
  print_endline (Jsonaf.to_string (Command.params command));
  let empty =
    match params with
    | `Object fields ->
      `Object (List.Assoc.add fields ~equal:String.equal "text" (`String " "))
    | _ -> assert false
  in
  print_result
    (Command.of_method_and_params ~method_:"session.send_message" ~params:empty)
    ~ok:(fun _ -> "accepted");
  print_result
    (Command.of_method_and_params ~method_:"session.future" ~params:(`Object []))
    ~ok:(fun _ -> "accepted");
  [%expect
    {|
    session.send_message
    {"session_id":"ses_AAAAAAAAAAAAAAAAAAAAAAAA","attachment_id":"att_AAAAAAAAAAAAAAAAAAAAAAAA","idempotency_key":"client-a:938","content":{"kind":"plain_text","text":"Run the tests","attachments":[]}}
    invalid_request
    method_not_found |}]
;;

let%expect_test "durable and recoverable events use independent ordering" =
  let generator = deterministic_generator () in
  let session_id = Id.Session.create_with generator in
  let timestamp = Timestamp.of_time_ns Time_ns.epoch in
  let durable : Event.Durable.t =
    { session_id
    ; sequence = 184L
    ; revision = 27L
    ; timestamp
    ; kind = History_appended
    ; visibility = Hidden
    ; payload = `Object []
    }
  in
  let live : Event.Recoverable.t =
    { session_id
    ; operation_id = Id.Operation.create_with generator
    ; operation_sequence = 9L
    ; anchor_sequence = 184L
    ; timestamp
    ; kind = Tool_progress
    ; payload = `Object [ "message", `String "building" ]
    }
  in
  print_endline
    (Jsonaf.to_string (Envelope.to_json (Event.Durable.to_notification durable)));
  print_endline
    (Jsonaf.to_string (Envelope.to_json (Event.Recoverable.to_notification live)));
  ignore
    (Event.Durable.of_json (Event.Durable.to_json durable) |> ok_or_fail
     : Event.Durable.t);
  ignore
    (Event.Recoverable.of_json (Event.Recoverable.to_json live) |> ok_or_fail
     : Event.Recoverable.t);
  [%expect
    {|
    {"jsonrpc":"2.0","method":"session.event","params":{"session_id":"ses_AAAAAAAAAAAAAAAAAAAAAAAA","sequence":184,"revision":27,"timestamp":"1970-01-01T00:00:00.000000000Z","kind":"history.appended","payload":{},"visibility":"hidden"}}
    {"jsonrpc":"2.0","method":"session.live_event","params":{"session_id":"ses_AAAAAAAAAAAAAAAAAAAAAAAA","operation_id":"op_AAAAAAAAAAAAAAAAAAAAAAAA","operation_sequence":9,"anchor_sequence":184,"timestamp":"1970-01-01T00:00:00.000000000Z","kind":"tool.progress","payload":{"message":"building"}}} |}]
;;

let%expect_test "protocol initialization canonicalizes features and dispatches" =
  let implementation =
    Initialize.Implementation.create ~name:"ochat-test" ~version:"1.2.3" |> ok_or_fail
  in
  let request =
    Initialize.Request.create
      ~implementation
      ~protocol_min:Version.initial
      ~protocol_max:Version.initial
      ~features:[ "session.replay"; "event.live" ]
      ~event_encodings:[ Json; Ndjson ]
      ~max_inbound_event_bytes:65536
      ~client_instance_id:"test-client"
      ()
    |> ok_or_fail
  in
  let command =
    Command.of_method_and_params
      ~method_:"protocol.initialize"
      ~params:(Initialize.Request.to_json request)
    |> ok_or_fail
  in
  print_endline (Command.method_name command);
  print_endline (Jsonaf.to_string (Command.params command));
  [%expect
    {|
    protocol.initialize
    {"implementation":{"name":"ochat-test","version":"1.2.3"},"protocol_min":{"major":1,"minor":0},"protocol_max":{"major":1,"minor":0},"features":["event.live","session.replay"],"event_encodings":["json","ndjson"],"max_inbound_event_bytes":65536,"client_instance_id":"test-client"} |}]
;;

let%expect_test "history windows use canonical occurrence IDs" =
  let id = History.Id.of_string "4:test:7" |> ok_or_fail in
  let request : History.Window_request.t =
    { position = After id; limit = 50; effective = true }
  in
  let encoded = History.Window_request.to_json request in
  print_endline (Jsonaf.to_string encoded);
  ignore (History.Window_request.of_json encoded |> ok_or_fail : History.Window_request.t);
  print_result (History.Id.of_string "test:7") ~ok:History.Id.to_string;
  [%expect
    {|
    {"position":"after","history_id":"4:test:7","limit":50,"effective":true}
    invalid_request |}]
;;

let%expect_test "permission codecs preserve legacy owners and reject ambiguous ownership" =
  let generator = deterministic_generator () in
  let timestamp = Timestamp.of_time_ns Time_ns.epoch in
  let permission : Permission.t =
    { id = Id.Permission.create_with generator
    ; session_id = Id.Session.create_with generator
    ; generation = 0
    ; owner = Operation (Id.Operation.create_with generator)
    ; call_id = "call-1"
    ; tool_name = "shell"
    ; runtime_identity = Some "runtime-1"
    ; invocation_display = "dune runtest"
    ; rationale = None
    ; effects = [ "process.execute" ]
    ; choices = [ Approve_once; Deny ]
    ; created_at = timestamp
    ; expires_at = None
    ; state = Pending
    ; resolution = None
    }
  in
  let encoded = Permission.to_json permission in
  ignore (Permission.of_json encoded |> ok_or_fail : Permission.t);
  let legacy =
    match Permission.sexp_of_t permission with
    | Sexp.List fields ->
      Sexp.List
        (List.map fields ~f:(function
           | Sexp.List [ Atom "owner"; List [ Atom "Operation"; id ] ] ->
             Sexp.List [ Atom "operation_id"; id ]
           | field -> field))
    | _ -> assert false
  in
  assert (Jsonaf.exactly_equal encoded (Permission.to_json (Permission.t_of_sexp legacy)));
  let invocation =
    { permission with owner = Invocation (Id.Invocation.create_with generator) }
  in
  let invocation_json = Permission.to_json invocation in
  assert (
    Permission.equal_owner
      invocation.owner
      (Permission.of_json invocation_json |> ok_or_fail).owner);
  (match invocation_json with
   | `Object fields ->
     let operation_id =
       match permission.owner with
       | Operation id -> id
       | Invocation _ -> assert false
     in
     assert (
       Result.is_error
         (Permission.of_json
            (`Object (("operation_id", Id.Operation.to_json operation_id) :: fields))));
     assert (
       Result.is_error
         (Permission.of_json
            (`Object (List.Assoc.remove fields "invocation_id" ~equal:String.equal))));
     assert (
       Result.is_error
         (Permission.of_json
            (`Object (List.Assoc.add fields "invocation_id" `Null ~equal:String.equal))))
   | _ -> assert false);
  let invalid =
    match encoded with
    | `Object fields ->
      `Object
        (fields
         @ [ ( "resolution"
             , Permission.Resolution.to_json
                 { choice = Deny
                 ; principal_id = None
                 ; resolved_at = timestamp
                 ; reason = None
                 } )
           ])
    | _ -> assert false
  in
  print_result (Permission.of_json invalid) ~ok:(fun _ -> "accepted");
  [%expect {| invalid_request |}]
;;

let architecture_methods =
  [ "protocol.initialize"
  ; "protocol.ping"
  ; "server.info"
  ; "server.health"
  ; "prompt.list"
  ; "prompt.get"
  ; "workspace.list"
  ; "workspace.get"
  ; "blob.read"
  ; "session.create"
  ; "session.list"
  ; "session.get"
  ; "session.attach"
  ; "session.detach"
  ; "session.renew_owner"
  ; "session.start"
  ; "session.stop"
  ; "session.cancel_operation"
  ; "session.send_message"
  ; "session.compact"
  ; "session.delete_history"
  ; "session.export"
  ; "session.reset"
  ; "session.rebuild"
  ; "session.upgrade_prompt"
  ; "session.delete"
  ; "permission.list"
  ; "permission.respond"
  ; "grant.list"
  ; "grant.revoke"
  ; "audit.read"
  ; "job.list"
  ; "job.get"
  ; "job.cancel"
  ; "schedule.list"
  ; "schedule.get"
  ; "schedule.create"
  ; "schedule.cancel"
  ; "ingress.submit"
  ]
;;

let%expect_test "every architecture method has request and result dispatch" =
  let normalize = List.sort ~compare:String.compare in
  let expected = normalize architecture_methods in
  print_s
    [%sexp
      { method_count = (List.length expected : int)
      ; requests =
          (List.equal String.equal expected (normalize Command.supported_methods) : bool)
      ; results =
          (List.equal String.equal expected (normalize Method_result.supported_methods)
           : bool)
      }];
  [%expect {| ((method_count 39) (requests true) (results true)) |}]
;;

let%expect_test "history deletion requires stable ID, revision and idempotency" =
  let generator = deterministic_generator () in
  let request : Session.Delete_history_request.t =
    { session_id = Id.Session.create_with generator
    ; attachment_id = Id.Attachment.create_with generator
    ; history_id =
        History_entry.Id.create ~namespace:"wire" ~sequence:0 |> Result.ok_or_failwith
    ; expected_revision = 7L
    ; idempotency_key = Idempotency_key.of_string "delete:test" |> ok_or_fail
    }
  in
  let json = Session.Delete_history_request.to_json request in
  print_result
    (Command.of_method_and_params ~method_:"session.delete_history" ~params:json)
    ~ok:Command.method_name;
  let fields =
    match json with
    | `Object fields -> fields
    | _ -> assert false
  in
  List.iter [ "history_id"; "expected_revision"; "idempotency_key" ] ~f:(fun key ->
    print_result
      (Session.Delete_history_request.of_json
         (`Object (List.filter fields ~f:(fun (name, _) -> not (String.equal key name)))))
      ~ok:(fun _ -> "accepted"));
  print_result
    (Session.Delete_history_request.of_json
       (Session.Delete_history_request.to_json { request with expected_revision = -1L }))
    ~ok:(fun _ -> "accepted");
  [%expect
    {|
    session.delete_history
    invalid_request
    invalid_request
    invalid_request
    invalid_request
    |}]
;;

let%expect_test "blob reads are bounded and chunks validate exact cursors" =
  let generator = deterministic_generator () in
  let request : Blob.Read_request.t =
    { session_id = Id.Session.create_with generator
    ; attachment_id = Id.Attachment.create_with generator
    ; blob_id = Id.Blob.create_with generator
    ; offset = 4L
    ; max_bytes = 32
    }
  in
  let decoded = Blob.Read_request.of_json (Blob.Read_request.to_json request) in
  let invalid_request =
    Blob.Read_request.of_json
      (`Object
          [ "session_id", Id.Session.to_json request.session_id
          ; "attachment_id", Id.Attachment.to_json request.attachment_id
          ; "blob_id", Id.Blob.to_json request.blob_id
          ; "offset", `Number "0"
          ; "max_bytes", `Number "0"
          ])
  in
  let data = "chunk" in
  let blob =
    Blob.Metadata.create
      ~id:request.blob_id
      ~kind:File
      ~media_type:"text/plain"
      ~byte_length:(Int64.of_int (String.length data))
      ~digest:(Digestif.SHA256.digest_string data |> Digestif.SHA256.to_hex)
      ()
    |> ok_or_fail
  in
  let chunk : Blob.Chunk.t =
    { blob
    ; offset = 0L
    ; next_offset = Int64.of_int (String.length data)
    ; data_base64 = Base64.encode_exn data
    ; eof = true
    }
  in
  let decoded_chunk = Blob.Chunk.of_json (Blob.Chunk.to_json chunk) in
  print_s
    [%sexp
      { request_valid = (Result.is_ok decoded : bool)
      ; zero_chunk_rejected = (Result.is_error invalid_request : bool)
      ; chunk_valid = (Result.is_ok decoded_chunk : bool)
      }];
  [%expect
    {|
    ((request_valid true) (zero_chunk_rejected true) (chunk_valid true))
    |}]
;;

let%expect_test "typed durable payload derives and validates its event kind" =
  let generator = deterministic_generator () in
  let timestamp = Timestamp.of_time_ns Time_ns.epoch in
  let operation : Operation.t =
    { id = Id.Operation.create_with generator
    ; generation = 1
    ; kind = Compaction
    ; state = Completed
    ; started_at = timestamp
    ; updated_at = timestamp
    }
  in
  let payload = Event.Durable.Payload.Operation_completed operation in
  let event =
    Event.Durable.of_payload
      ~session_id:(Id.Session.create_with generator)
      ~sequence:2L
      ~revision:3L
      ~timestamp
      payload
  in
  let decoded =
    Event.Durable.Payload.of_json ~kind:event.kind (Event.Durable.Payload.to_json payload)
    |> ok_or_fail
  in
  print_s
    [%sexp
      { kind_matches =
          (Event.Durable.equal_kind event.kind (Event.Durable.Payload.kind decoded)
           : bool)
      ; wire_kind =
          ((match Event.Durable.to_json event with
            | `Object fields ->
              List.Assoc.find_exn fields "kind" ~equal:String.equal |> Jsonaf.to_string
            | _ -> assert false)
           : string)
      }];
  [%expect {| ((kind_matches true) (wire_kind "\"operation.completed\"")) |}]
;;

let%expect_test "job retry and one-shot schedule requests round trip" =
  let generator = deterministic_generator () in
  let schedule : Schedule.Create_request.t =
    { session_id = Id.Session.create_with generator
    ; attachment_id = Id.Attachment.create_with generator
    ; payload = `Object [ "event", `String "wake" ]
    ; due = After_ms 250
    ; misfire = Deliver_once_immediately
    ; idempotency_key = Idempotency_key.of_string "schedule:test:1" |> ok_or_fail
    }
  in
  let encoded = Schedule.Create_request.to_json schedule in
  print_endline (Jsonaf.to_string encoded);
  ignore
    (Schedule.Create_request.of_json encoded |> ok_or_fail : Schedule.Create_request.t);
  let retry : Job.retry_policy =
    Idempotent
      { key = Idempotency_key.of_string "job:test:1" |> ok_or_fail
      ; max_attempts = 3
      ; backoff_ms = 1000
      }
  in
  print_s [%sexp (retry : Job.retry_policy)];
  [%expect
    {|
    {"session_id":"ses_AAAAAAAAAAAAAAAAAAAAAAAA","attachment_id":"att_AAAAAAAAAAAAAAAAAAAAAAAA","payload":{"event":"wake"},"due":{"type":"after_ms","delay_ms":250},"misfire":"deliver_once_immediately","idempotency_key":"schedule:test:1"}
    (Idempotent (key job:test:1) (max_attempts 3) (backoff_ms 1000))
    |}]
;;

let empty_history_window : History.Window.t =
  { entries = []
  ; previous_cursor = None
  ; next_cursor = None
  ; reached_start = true
  ; reached_end = true
  ; structurally_complete = true
  }
;;

let%expect_test "client snapshots and typed method results round trip" =
  let session = session_summary () in
  let snapshot : Snapshot.t =
    { session
    ; canonical_history = empty_history_window
    ; archived_revisions = []
    ; effective_history = None
    ; deferred_entries = []
    ; permissions = []
    ; grants = []
    ; jobs = []
    ; extension_status = []
    ; schedules = []
    ; active_tool_calls = []
    ; active_agent_calls = []
    ; halted = false
    ; halt_reason = None
    ; failure = None
    ; revision = session.revision
    ; latest_event_sequence = session.latest_event_sequence
    }
  in
  let result = Method_result.Session_get snapshot in
  let encoded = Method_result.to_json result in
  let decoded = Method_result.of_json ~method_:"session.get" encoded |> ok_or_fail in
  print_s
    [%sexp
      { method_ = (Method_result.method_name decoded : string)
      ; revision =
          ((match decoded with
            | Session_get snapshot -> snapshot.revision
            | _ -> assert false)
           : int64)
      }];
  [%expect {| ((method_ session.get) (revision 4)) |}]
;;

let%expect_test "typed inline attachments validate their declared length" =
  let attachment : Blob.Input.t =
    { kind = File
    ; media_type = "text/plain"
    ; byte_length = 2L
    ; digest = "sha256:placeholder"
    ; display_name = Some "note.txt"
    ; source = Inline_base64 "SGk="
    }
  in
  let encoded = Blob.Input.to_json attachment in
  ignore (Blob.Input.of_json encoded |> ok_or_fail : Blob.Input.t);
  let invalid =
    match encoded with
    | `Object fields ->
      `Object (List.Assoc.add fields ~equal:String.equal "byte_length" (`Number "3"))
    | _ -> assert false
  in
  print_result (Blob.Input.of_json invalid) ~ok:(fun _ -> "accepted");
  [%expect {| invalid_request |}]
;;
