open! Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module W = P.Instruction_control
module C = Agent_client.Instruction_controls
module R = Instruction_runtime_tests
module Selection = Agent_session.Instruction_selection

(* Serialization crosses no authority boundary: the actual context below still
   retains the host's original authenticated proof and connection attachments. *)
let connect ?(after_dispatch = fun _ result -> result) ~actor daemon =
  let notifications = Eio.Stream.create 256 in
  let context =
    Agent_server.Connection_context.create_authenticated
      ~actor
      ~connection_id:(P.Id.Attachment.create () |> P.Id.Attachment.to_string)
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:64
  in
  Agent_client.In_memory.create
    ~request:(fun original ->
      let command =
        P.Command.of_method_and_params
          ~method_:(P.Command.method_name original)
          ~params:(P.Command.params original)
        |> protocol_ok
      in
      let result =
        Agent_server.Dispatcher.dispatch_command
          (Agent_server.Daemon.dispatcher daemon)
          ~context
          command
        |> Result.bind ~f:(fun result ->
          P.Public.Result.of_json
            ~method_:(P.Public.Result.method_name result)
            (P.Public.Result.to_json result))
      in
      after_dispatch command result)
    ~notifications
    ~close:(fun () -> Agent_server.Daemon.close_connection daemon context)
;;

let original ?(is_current = fun () -> true) principal =
  Operator_authorization.guarded ~principal ~is_current
;;

let key text = P.Idempotency_key.of_string text |> protocol_ok
let state entry = A.state entry.Agent_server.Session_registry.actor |> protocol_ok

let selection state =
  Option.value_exn state.Agent_session.Session_state.instruction_selection
;;

let denied result =
  match result with
  | Error { P.Error.code = Permission_denied; _ } -> ()
  | Error error -> raise_s [%sexp "unexpected denial", (error : P.Error.t)]
  | Ok _ -> failwith "unauthorized instruction operation succeeded"
;;

let failed code result =
  match result with
  | Error error -> [%test_eq: P.Error.code] code error.P.Error.code
  | Ok _ -> failwith "invalid instruction operation succeeded"
;;

let equal_selection first second =
  assert (P.Instruction_status.equal (Selection.summary first) (Selection.summary second));
  assert (
    Jsonaf.exactly_equal
      (Selection.to_jsonaf first |> protocol_ok)
      (Selection.to_jsonaf second |> protocol_ok))
;;

let project_manifest source =
  match W.Source_request.selection source with
  | W.Selection.Project { root; scope } ->
    ( W.Root_id.to_string root
    , Option.map scope ~f:Source.Portable_path.to_string
    , List.map (W.Source_request.members source) ~f:(fun member ->
        ( W.Root_id.to_string member.W.Member.root
        , Source.Portable_path.to_string member.member
        , Chatml_source.Digest.to_hex member.digest )) )
  | Skills _ -> failwith "project view contained skills"
;;

let check_public_summary view =
  let fields = P.Instruction_status.to_json view.W.View.summary |> R.fields in
  [%test_eq: string list]
    [ "revision"
    ; "project_selected"
    ; "project_files"
    ; "skills"
    ; "retained_captures"
    ; "retained_receipts"
    ]
    (List.map fields ~f:fst)
;;

let%expect_test
    "actual headless instruction controls retain exact receipts and captured bytes"
  =
  let current = Atomic.make true in
  let old_actor = original ~is_current:(fun () -> Atomic.get current) (principal ()) in
  let scope = ref None in
  let host = ref None in
  let workspace = ref None in
  let provider_calls = ref 0 in
  let expected_guidance = ref None in
  let prepared_text = ref None in
  let saved_id = ref None in
  let saved_view = ref None in
  let saved_selection = ref None in
  let saved_retry = ref None in
  let replacement = "ROOT-AGENTS-REPLACEMENT\nexact trailing bytes \t\n" in
  with_daemon
    ~request_guidance:true
    ~config_file:"server.sexp"
    ~sources:[ "agent.chatmd", R.agent; "server.sexp", R.configuration ]
    ~workspace_files:[ "AGENTS.md", R.root_guidance; "sub/AGENTS.md", R.leaf_guidance ]
    ~calls:[]
    ~initial_requests:1
    ~expected_requests:3
    ~connect:(fun ~sw ~env ~root daemon ->
      scope := Some sw;
      host := Some daemon;
      workspace := Some Eio.Path.(Eio.Stdenv.fs env / root / "workspace");
      connect ~actor:(original (principal ())) daemon)
    ~inspect_preparation:(fun ~selection:_ ~request ->
      prepared_text
      := Option.map
           (Inference.Request.guidance request)
           ~f:Inference.Request.Guidance.text)
    ~inspect_request:(fun _ inputs ->
      incr provider_calls;
      let guidance =
        List.concat_map inputs ~f:(function
          | Openai.Responses.Item.Input_message { role = User; content; _ } ->
            List.filter_map content ~f:(function
              | Text { text; _ }
                when String.is_prefix
                       text
                       ~prefix:"OChat session guidance from explicitly selected files." ->
                Some text
              | Text _ | Image _ -> None)
          | _ -> [])
      in
      (match !expected_guidance with
       | None -> [%test_eq: string list] [] guidance
       | Some bytes ->
         let text = List.hd_exn guidance in
         [%test_eq: int] 1 (List.length guidance);
         [%test_eq: string option] (Some text) !prepared_text;
         assert (String.is_substring text ~substring:bytes);
         assert (String.is_substring text ~substring:R.leaf_guidance));
      assert (
        String.is_substring (R.wire_text inputs) ~substring:"AUTHORED-HIGHER-PRIORITY"))
    ~after_turn:(fun env initial_handle entry ->
      let daemon = Option.value_exn !host in
      let sw = Option.value_exn !scope in
      let clients = ref [] in
      let handles = ref [] in
      let client ?after_dispatch actor =
        let connection = connect ?after_dispatch ~actor daemon in
        clients := connection :: !clients;
        initialize connection;
        connection
      in
      let attach ?(mode = P.Session.Read_write) connection =
        let handle =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection
            ~session_id:(H.session_id initial_handle)
            ~mode
            ~subscribe:false
            ()
          |> protocol_ok
        in
        handles := handle :: !handles;
        handle
      in
      Exn.protect
        ~finally:(fun () ->
          List.iter !handles ~f:H.close;
          List.iter !clients ~f:Agent_client.Connection.close)
        ~f:(fun () ->
          let dropped = ref None in
          let drop_reply = ref true in
          let first =
            client old_actor ~after_dispatch:(fun command result ->
              match command, result with
              | P.Command.Instructions (W.Request.Activate _), Ok result when !drop_reply
                ->
                drop_reply := false;
                let receipt =
                  match result with
                  | P.Public.Result.Non_history value ->
                    (match P.Public.Result.Non_history.value value with
                     | P.Method_result.Instructions (W.Result.Activate receipt) -> receipt
                     | _ -> failwith "wrong committed activation response")
                  | _ -> failwith "wrong committed activation container"
                in
                dropped := Some receipt;
                Error
                  (P.Error.create
                     Interrupted
                     ~message:"controlled reply lost after real activation ACK"
                     ~retryable:true
                     ())
              | _ -> result)
          in
          let first_handle = attach first in
          let controls = H.instructions first_handle in
          let empty = C.inspect controls |> protocol_ok in
          [%test_eq: int64] 0L empty.summary.revision;
          let roots =
            C.refresh controls ~selection:None |> protocol_ok |> W.Discovery.roots
          in
          let root = List.hd_exn roots in
          let project =
            W.Selection.Project
              { root
              ; scope = Some (Source.Portable_path.create "sub" |> Or_error.ok_exn)
              }
          in
          let discovery = C.refresh controls ~selection:(Some project) |> protocol_ok in
          [%test_eq: string list]
            [ "AGENTS.md"; "sub/AGENTS.md" ]
            (List.map (W.Discovery.members discovery) ~f:(fun member ->
               Source.Portable_path.to_string member.W.Member.member));
          let source =
            W.Source_request.create
              ~selection:project
              ~members:(W.Discovery.members discovery)
            |> protocol_ok
          in
          let captured = C.capture controls source |> protocol_ok in
          [%test_eq: int] 1 !provider_calls;
          let activation_key = key "operator-lost-reply" in
          let activate controls =
            C.activate
              controls
              ~expected_generation:empty.generation
              ~expected_revision:0L
              ~key:activation_key
              captured
          in
          failed P.Error.Interrupted (activate controls);
          let accepted = Option.value_exn !dropped in
          let after_ack = state entry in
          [%test_eq: int64] 1L (Selection.revision (selection after_ack));
          [%test_eq: int] 0 (List.length (Agent_client.Connection.pending_commands first));
          Atomic.set current false;
          denied (activate controls);
          H.close first_handle;
          Agent_client.Connection.close first;
          let next = client (original (principal ())) in
          let next_handle = attach next in
          assert (
            not
              (P.Id.Attachment.equal
                 (H.attachment first_handle).id
                 (H.attachment next_handle).id));
          let controls = H.instructions next_handle in
          let before_retry = state entry in
          let retry = activate controls |> protocol_ok in
          assert (W.Receipt.equal accepted retry);
          let after_retry = state entry in
          [%test_eq: int64]
            before_retry.counters.event_sequence
            after_retry.counters.event_sequence;
          [%test_eq: int64] before_retry.counters.revision after_retry.counters.revision;
          equal_selection (selection after_ack) (selection after_retry);
          let inspected = C.inspect controls |> protocol_ok in
          check_public_summary inspected;
          [%test_eq: int64] 1L inspected.summary.revision;
          assert (
            List.mem inspected.permitted_actions W.Action.Activate ~equal:W.Action.equal);
          let foreign_connection = client (original (principal ())) in
          denied
            (Agent_client.Connection.request_without_history
               foreign_connection
               (P.Command.Instructions
                  (W.Request.Activate
                     { session_id = H.session_id next_handle
                     ; attachment_id = (H.attachment next_handle).id
                     ; expected_generation = empty.generation
                     ; expected_revision = 0L
                     ; source = captured.source
                     ; capture_id = captured.capture_id
                     ; key = activation_key
                     })));
          let principal_only =
            Agent_server_test_support.connection daemon (principal ())
          in
          clients := principal_only :: !clients;
          initialize principal_only;
          denied
            (Agent_client.Connection.request_without_history
               principal_only
               (P.Command.Instructions
                  (W.Request.Inspect
                     { session_id = H.session_id next_handle; attachment_id = None })));
          let readonly =
            client (original (principal ())) |> attach ~mode:Read_only |> H.instructions
          in
          let readonly_view = C.inspect readonly |> protocol_ok in
          assert (
            not
              (List.mem
                 readonly_view.permitted_actions
                 W.Action.Activate
                 ~equal:W.Action.equal));
          denied
            (C.remove
               readonly
               ~expected_generation:empty.generation
               ~expected_revision:1L
               ~key:(key "readonly-clear")
               W.Request.Remove.All);
          let other =
            principal_with_scopes
              "pri_no_session_visibility"
              (P.Scope.Set.of_list [ View_session_transcript; Send_messages ])
          in
          let other_client = client (original other) in
          denied
            (Agent_client.Connection.request_without_history
               other_client
               (P.Command.Instructions
                  (W.Request.Inspect
                     { session_id = H.session_id next_handle; attachment_id = None })));
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(Option.value_exn !workspace / "AGENTS.md")
            replacement;
          let changed = C.refresh controls ~selection:(Some project) |> protocol_ok in
          [%test_eq: int] 1 !provider_calls;
          equal_selection (selection after_ack) (selection (state entry));
          expected_guidance := Some R.root_guidance;
          ignore
            (Instruction_example_recovery.send
               env
               next_handle
               entry
               ~terminal:Completed
               "Use the retained original bytes.");
          let replacement_source =
            W.Source_request.create
              ~selection:project
              ~members:(W.Discovery.members changed)
            |> protocol_ok
          in
          let replacement_capture =
            C.capture controls replacement_source |> protocol_ok
          in
          let replacement_key = key "operator-replace" in
          failed
            P.Error.Conflict
            (C.activate
               controls
               ~expected_generation:empty.generation
               ~expected_revision:0L
               ~key:(key "operator-stale")
               replacement_capture);
          failed
            P.Error.Idempotency_conflict
            (C.activate
               controls
               ~expected_generation:empty.generation
               ~expected_revision:1L
               ~key:activation_key
               replacement_capture);
          let replaced =
            C.activate
              controls
              ~expected_generation:empty.generation
              ~expected_revision:1L
              ~key:replacement_key
              replacement_capture
            |> protocol_ok
          in
          [%test_eq: int64] 2L replaced.revision;
          expected_guidance := Some replacement;
          ignore
            (Instruction_example_recovery.send
               env
               next_handle
               entry
               ~terminal:Completed
               "Use the explicitly replaced bytes.");
          let after = state entry in
          [%test_eq: string list]
            [ replacement; R.leaf_guidance ]
            (List.map
               (Selection.project (selection after))
               ~f:Instruction_content.Loaded.contents);
          [%test_eq: int] 2 (List.length (Selection.receipts (selection after)));
          saved_id := Some after.identity.session_id;
          saved_view := Some (C.inspect controls |> protocol_ok);
          saved_selection := Some (selection after);
          saved_retry := Some (empty.generation, replacement_capture);
          H.stop next_handle ~mode:Graceful |> protocol_ok |> ignore;
          unload_idle_runtime env entry.runtime;
          assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime))))
    ~after_shutdown:(fun ~sw ~env ~root ~configuration ->
      List.iter [ "AGENTS.md"; "sub/AGENTS.md" ] ~f:(fun member ->
        Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / "workspace" / member));
      let daemon =
        Agent_server.Daemon.start
          ~sw
          ~env
          ~config:configuration
          ~tool_dir:root
          ~home:root
          ~process_start_identity:None
          ~options:
            { Agent_server.Daemon.default_options with
              inference_policy =
                inference_policy
                  ~default_model:"fixture-model"
                  ~post_stream:(fun ~sw:_ ~inputs:_ ->
                    failwith "instruction inspection/removal started a provider")
            }
          ()
        |> protocol_ok
      in
      Exn.protect
        ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> protocol_ok)
        ~f:(fun () ->
          let id = Option.value_exn !saved_id in
          let entry =
            Agent_server.Session_registry.load (Agent_server.Daemon.registry daemon) id
            |> protocol_ok
          in
          let retained = state entry in
          equal_selection (Option.value_exn !saved_selection) (selection retained);
          assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime));
          let live = Atomic.make true in
          let actor = original ~is_current:(fun () -> Atomic.get live) (principal ()) in
          let client = connect ~actor daemon in
          initialize client;
          let handle =
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client
              ~session_id:id
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () ->
              H.close handle;
              Agent_client.Connection.close client)
            ~f:(fun () ->
              let reopened = C.inspect (H.instructions handle) |> protocol_ok in
              let before = Option.value_exn !saved_view in
              assert (P.Instruction_status.equal before.summary reopened.summary);
              [%test_eq:
                (string * string option * (string * string * string) list) option]
                (Option.map before.project ~f:project_manifest)
                (Option.map reopened.project ~f:project_manifest);
              Atomic.set live false;
              denied (C.inspect (H.instructions handle)));
          let replacement_principal =
            P.Principal.create
              ~id:(principal ()).id
              ~authentication_kind:"replacement"
              ~scopes:(principal ()).scopes
              ~attributes:[]
            |> protocol_ok
          in
          let client = connect ~actor:(original replacement_principal) daemon in
          initialize client;
          let handle =
            H.attach
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~connection:client
              ~session_id:id
              ~mode:Read_write
              ~subscribe:false
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () ->
              H.close handle;
              Agent_client.Connection.close client)
            ~f:(fun () ->
              let controls = H.instructions handle in
              denied (C.inspect controls);
              let generation, capture = Option.value_exn !saved_retry in
              denied
                (C.activate
                   controls
                   ~expected_generation:generation
                   ~expected_revision:1L
                   ~key:(key "operator-replace")
                   capture);
              let before = state entry in
              equal_selection (Option.value_exn !saved_selection) (selection before);
              let cleared =
                C.remove
                  controls
                  ~expected_generation:before.identity.generation
                  ~expected_revision:2L
                  ~key:(key "operator-authorized-clear")
                  W.Request.Remove.All
                |> protocol_ok
              in
              [%test_eq: int64] 3L cleared.revision;
              let after = state entry in
              [%test_eq: int] 0 (List.length (Selection.project (selection after)));
              [%test_eq: int] 3 (List.length (Selection.receipts (selection after)));
              assert (not (Agent_server.Runtime_owner.is_loaded entry.runtime)))))
    (fun _ -> ());
  print_endline
    "actual operator wire/client: lost ACK reply, new-attachment exact receipt, current \
     proof/root denial, explicit captured-byte replacement, reopened inert identities \
     and authorized Clear";
  [%expect
    {| actual operator wire/client: lost ACK reply, new-attachment exact receipt, current proof/root denial, explicit captured-byte replacement, reopened inert identities and authorized Clear |}]
;;
