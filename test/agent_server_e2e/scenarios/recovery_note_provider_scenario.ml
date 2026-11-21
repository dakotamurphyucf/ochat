open Core
open Agent_server_test_support
module A = Agent_session.Session_actor
module C = Recovery_notes.Catalog
module D = Agent_server.Daemon
module H = Agent_client.Session_handle
module P = Agent_protocol
module R = Agent_server.Session_registry
module Provider = Support.Tui_stream_provider
module Auth = Openai.Responses_driver.Auth

let body = "loopback-note-β-private"
let field name json = Jsonaf.member_exn name json
let str value = `String value

let function_call request ~id ~name args =
  let open Openai.Responses.Response_stream in
  let call arguments status =
    Item.Function_call
      { name; arguments; call_id = id; _type = "function_call"; id = Some id; status }
  in
  let arguments = Jsonaf.to_string args in
  Provider.emit
    request
    [ Provider.added (call "" None)
    ; Function_call_arguments_done
        { arguments
        ; item_id = id
        ; output_index = 0
        ; type_ = "response.function_call_arguments.done"
        }
    ; Provider.done_ (call arguments (Some "completed"))
    ];
  Provider.finish request
;;

let output request ~id =
  field "input" (Provider.body request)
  |> Jsonaf.list_exn
  |> List.find_exn ~f:(fun item ->
    Option.equal
      String.equal
      (Jsonaf.member "type" item |> Option.map ~f:Jsonaf.string_exn)
      (Some "function_call_output")
    && String.equal (field "call_id" item |> Jsonaf.string_exn) id)
  |> field "output"
  |> Jsonaf.string_exn
  |> Jsonaf.of_string
  |> P.Invocation.outcome_of_json
  |> protocol_ok
;;

let read_request =
  `Object
    [ "selection", `Object [ "kind", str "local"; "id", str "1"; "revision", str "1" ]
    ; ( "range"
      , `Object
          [ "start", `Number "0"; "stop", `Number (Int.to_string (String.length body)) ] )
    ]
;;

let default_prompt =
  {|<config model="gpt-4.1"/><developer>Recovery note fixture.</developer>
<tool name="recovery_note_write"/><tool name="recovery_note_read"/>|}
;;

let with_fixture
      ?root
      ?port
      ?(prompt_xml = default_prompt)
      ?on_preparation
      ?recovery_capacity
      ?(extra_features = [])
      ?(restrict = Fn.id)
      env
      environment
      ~auth
      ~max_sessions
      f
  =
  let root =
    Option.value root ~default:(Support.Temporary_environment.roots environment).temporary
  in
  let prompt = Filename.concat root "recovery-note.chatmd" in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / prompt)
    prompt_xml;
  Eio.Switch.run (fun sw ->
    let port =
      Option.value_or_thunk port ~default:(fun () ->
        let reservation = Support.Port_reservation.create ~sw ~env in
        let port = Support.Port_reservation.port reservation in
        Support.Port_reservation.release reservation;
        port)
    in
    let provider = Provider.start ~sw ~env ~port in
    let endpoint = sprintf "http://127.0.0.1:%d/v1/responses" port in
    let config = config root root prompt in
    let config =
      Option.value_map recovery_capacity ~default:config ~f:(fun capacity ->
        { config with server = { config.server with recovery_capacity = Some capacity } })
    in
    let workspaces =
      List.map config.workspaces ~f:(fun workspace ->
        let open Agent_server.Config.Workspace in
        let prompt_limits =
          List.map workspace.prompt_limits ~f:(fun limit ->
            { limit with max_root_agents = max_sessions })
        in
        { workspace with prompt_limits })
    in
    let daemon =
      D.start
        ~sw
        ~env
        ~config:{ config with workspaces }
        ~tool_dir:root
        ~home:root
        ~process_start_identity:None
        ~options:
          { D.default_options with
            inference_policy =
              Generated_provider_scenario.inference_policy
                ?on_preparation
                ~extra_features
                env
                ~endpoint
                ~auth
          }
        ()
      |> protocol_ok
    in
    let proof = restrict (Operator_authorization.trusted_local (principal ())) in
    let notifications = Eio.Stream.create 256 in
    let context =
      Agent_server.Connection_context.create_authenticated
        ~actor:proof
        ~connection_id:"recovery-note-fixture"
        ~transport:In_memory
        ~publish_notification:(Eio.Stream.add notifications)
        ~max_attachments:4
    in
    let client =
      Agent_client.In_memory.create
        ~request:(Agent_server.Dispatcher.dispatch_command (D.dispatcher daemon) ~context)
        ~notifications
        ~close:(fun () -> D.close_connection daemon context)
    in
    Exn.protect
      ~finally:(fun () ->
        Agent_client.Connection.close client;
        D.shutdown daemon |> protocol_ok)
      ~f:(fun () ->
        initialize client;
        f ~sw ~provider ~daemon ~proof ~client))
;;

let open_session sw env client ~mode =
  H.create
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~connection:client
    ~spec:(session_spec ())
    ~mode
    ~subscribe:false
    ()
  |> protocol_ok
;;

let test env environment =
  let auth_attempt = ref 0 in
  let can_reserve_source = ref (fun () -> false) in
  let reservations = ref [] in
  let stale_attempt, materialized = Eio.Promise.create () in
  let resume, resume_write = Eio.Promise.create () in
  let auth ~sw:_ _ =
    let ordinal = !auth_attempt in
    Int.incr auth_attempt;
    if Int.equal ordinal 4
    then (
      Eio.Promise.resolve materialized ();
      Eio.Promise.await resume);
    let open Result.Let_syntax in
    let%bind lease = Auth.bearer "recovery-note-loopback-fixture" in
    Auth.with_identity
      lease
      ~owner:"recovery-note-fixture"
      ~generation:0L
      ~check_current:(fun () ->
        if Int.equal ordinal 2
        then reservations := !can_reserve_source () :: !reservations;
        Ok ())
  in
  with_fixture
    env
    environment
    ~auth
    ~max_sessions:1
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let handle = open_session sw env client ~mode:Read_write in
       ignore (H.start handle ~queue_if_limited:false |> protocol_ok : P.Session.t);
       let session_id = H.session_id handle in
       let owner =
         P.Session_ref.create
           ~server_id:(Agent_store.Session_store.server_id (D.store daemon))
           ~session_id
       in
       let registry = D.registry daemon in
       let entry = R.find registry session_id |> Option.value_exn in
       let state () = A.state entry.actor |> protocol_ok in
       let check_owner () =
         let open Result.Let_syntax in
         let%bind () =
           Agent_server.Session_factory.check_registered_owner
             (D.factory daemon)
             entry
             ~session:owner
         in
         R.check_native_metadata_publication registry entry
       in
       (can_reserve_source
        := fun () ->
             A.Recovery_note_shares.with_source_custody
               entry.actor
               ~check_owner
               ~f:(fun current -> current ())
             |> Result.ok
             |> Option.value ~default:false);
       let send text =
         let submitted =
           H.send_message handle { kind = Plain_text; text; attachments = [] }
           |> protocol_ok
         in
         assert (Option.is_some submitted.operation_id)
       in
       let request index = Provider.await_request provider env index in
       let await_idle () =
         Support.Tui_fixture.await env (fun () ->
           let current = state () in
           if Option.is_none current.active_operation then Some current else None)
       in
       send "Write and read the test note";
       let first = request 0 in
       let tool_names =
         field "tools" (Provider.body first)
         |> Jsonaf.list_exn
         |> List.map ~f:(fun tool -> field "name" tool |> Jsonaf.string_exn)
       in
       [%test_eq: String.Set.t]
         (String.Set.of_list tool_names)
         (String.Set.of_list [ "recovery_note_write"; "recovery_note_read" ]);
       function_call
         first
         ~id:"note-write"
         ~name:"recovery_note_write"
         (`Object
             [ "kind", str "create"
             ; "expected", `Null
             ; "path", str "handoff/test"
             ; "expected_catalog_revision", str "0"
             ; "content", str body
             ]);
       let second = request 1 in
       (match output second ~id:"note-write" with
        | Complete `Null -> ()
        | outcome ->
          raise_s
            [%sexp "note write did not acknowledge", (outcome : P.Invocation.outcome)]);
       function_call second ~id:"note-read" ~name:"recovery_note_read" read_request;
       let third = request 2 in
       (match output third ~id:"note-read" with
        | Complete value ->
          [%test_eq: string] (field "text" value |> Jsonaf.string_exn) body
        | outcome ->
          raise_s
            [%sexp "protected note output was missing", (outcome : P.Invocation.outcome)]);
       [%test_eq: bool list] (List.rev !reservations) [ true; false ];
       Provider.reply_summary third "Complete";
       let current = await_idle () in
       assert (Option.is_none current.recovery_windows);
       List.iter current.invocations ~f:(fun invocation ->
         assert (Document_schema.Json.equal invocation.context.input `Null);
         assert (
           not
             (String.is_substring
                (P.Invocation.to_json invocation |> Jsonaf.to_string)
                ~substring:body)));
       List.iter current.conversation.canonical_history ~f:(fun entry ->
         assert (
           not
             (String.is_substring
                (P.History.sexp_of_entry entry |> Sexp.to_string)
                ~substring:body)));
       send "Attempt to read the pinned test note again";
       function_call
         (request 3)
         ~id:"old-note-read"
         ~name:"recovery_note_read"
         read_request;
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
         Eio.Promise.await stale_attempt);
       (* The provider adapter already received the materialized output. The
           writer can still remove it until the actual submission lease opens. *)
       let access =
         Agent_session.Recovery_note_access.operator
           proof
           ~owner
           ~generation:current.identity.generation
           ~attachment_id:(Some (H.attachment handle).id)
           ~authorize:(fun _ -> check_owner ())
       in
       let selection =
         C.
           { id = Note_id.of_int64 1L |> protocol_ok
           ; revision = Note_revision.of_int64 1L |> protocol_ok
           }
       in
       A.mutate_recovery_note_operator
         entry.actor
         ~access
         ~key:(P.Idempotency_key.of_string "provider-cutpoint" |> protocol_ok)
         (Delete selection)
       |> protocol_ok
       |> ignore;
       Eio.Promise.resolve resume_write ();
       let final = await_idle () in
       [%test_eq: int] (Provider.request_count provider) 4;
       assert (List.length final.invocations > List.length current.invocations);
       H.close handle)
;;
