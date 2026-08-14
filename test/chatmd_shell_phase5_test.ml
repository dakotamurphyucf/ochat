open! Core
module A = Shell_access.Approval
module CM = Prompt.Chat_markdown
module F = Chatmd_shell_spec.Feature
module MC = Chatmd_shell_spec.Manifest_compiler
module S = Chatmd_shell_spec.Shell_spec

let approval_ok = function
  | Ok value -> value
  | Error (error : Shell_runtime.Approval_store.error) -> failwith error.message
;;

let authorizer_ok = function
  | Ok value -> value
  | Error (error : Shell_runtime.Manifest_authorizer.error) -> failwith error.message
;;

let audit_append_ok = function
  | Ok value -> value
  | Error (error : Shell_runtime.Audit_sink.error) -> failwith error.message
;;

let audit_load_ok = function
  | Ok value -> value
  | Error errors ->
    errors
    |> List.map ~f:(fun (error : Shell_runtime.Audit_replay.error) -> error.message)
    |> String.concat ~sep:"; "
    |> failwith
;;

let parse source =
  Eio_main.run (fun env ->
    CM.parse_chat_inputs ~source:"phase5.chatmd" ~dir:(Eio.Stdenv.cwd env) source)
;;

let compile_exn source =
  let runtimes, tools =
    List.fold (parse source) ~init:([], []) ~f:(fun (runtimes, tools) -> function
      | CM.Shell_runtime runtime -> runtime :: runtimes, tools
      | CM.Tool (CM.Shell tool) -> runtimes, tool :: tools
      | _ -> runtimes, tools)
  in
  match
    MC.compile
      { runtimes = List.rev runtimes
      ; tools = List.rev tools
      ; scripts = []
      ; legacy_tools = []
      ; moderator_runtime = None
      ; platform = S.Macos
      ; supported_features = F.phase4
      }
  with
  | Ok manifest -> manifest
  | Error diagnostics ->
    diagnostics
    |> List.map ~f:Chatmd_shell_spec.Diagnostic.to_string
    |> String.concat ~sep:"\n"
    |> failwith
;;

let basic_manifest suffix =
  compile_exn
    (sprintf
       {|<shell_access id="runtime-%s" extends="builtin:yolo@1"/>
         <tool name="run-%s" type="shell" mode="structured" runtime="runtime-%s"/>|}
       suffix
       suffix
       suffix)
;;

let sample_approval_grant =
  Session.Shell_state.Approval_grant.
    { grant_id = "grant"
    ; manifest_sha256 = "manifest"
    ; runtime_id = "runtime"
    ; request_kind = Structured
    ; command_sha256 = "command"
    ; executable_sha256 = "executable"
    ; argv = [ "echo"; "hello" ]
    ; argv_prefix = None
    ; cwd_sha256 = "cwd"
    ; environment_sha256 = "environment"
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; scope = Exact_session
    ; session_id = Some "session"
    ; user_id = Some "user"
    ; host_id = Some "host"
    ; created_at_ns = 1L
    ; expires_at_ns = None
    ; last_used_at_ns = None
    ; reviewer = { source = "ui"; reviewer_id = Some "reviewer" }
    ; revoked_at_ns = None
    ; revocation_reason = None
    }
;;

let equal_shell_state left right =
  Sexp.equal (Session.Shell_state.sexp_of_t left) (Session.Shell_state.sexp_of_t right)
;;

let%test_unit "Session V5 round-trips shell state and legacy/reset paths add no trust" =
  Eio_main.run (fun env ->
    let shell_state =
      { Session.Shell_state.empty with approval_grants = [ sample_approval_grant ] }
    in
    let session =
      Session.create ~id:"session" ~prompt_file:"prompt.chatmd" ~shell_state ()
    in
    let path_string =
      sprintf
        "/tmp/ochat-phase5-session-%d-%d.bin"
        (Core_unix.getpid () |> Pid.to_int)
        (Random.bits ())
    in
    let path = Eio.Path.(Eio.Stdenv.fs env / path_string) in
    Fun.protect
      ~finally:(fun () -> if Eio.Path.is_file path then Eio.Path.unlink path)
      (fun () ->
         Session.V5.Io.File.write path (Session.to_v5 session);
         let restored = Session.V5.Io.File.read path |> Session.of_v5 in
         assert (equal_shell_state restored.shell_state shell_state));
    let migrated = Session.to_v4 session |> Session.V5.of_v4 |> Session.of_v5 in
    assert (equal_shell_state migrated.shell_state Session.Shell_state.empty);
    assert (
      equal_shell_state (Session.reset session).shell_state Session.Shell_state.empty);
    assert (
      equal_shell_state
        (Session.reset_keep_history session).shell_state
        Session.Shell_state.empty))
;;

let identity ?(argv = [ "echo"; "hello" ]) ?(command_hash = "command") () =
  A.
    { manifest_sha256 = "manifest"
    ; runtime_id = "runtime"
    ; request_kind = Shell_access.Context.Structured
    ; command_hash
    ; executable_sha256 = "executable"
    ; argv
    ; cwd_sha256 = "cwd"
    ; environment_sha256 = "environment"
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    }
;;

let at seconds = Time_ns.of_span_since_epoch (Time_ns.Span.of_sec seconds)

let%test_unit "approval stores preserve exact identity, prefix, expiry, and revocation" =
  Eio_main.run (fun _env ->
    let bindings =
      Shell_runtime.Approval_store.{ user_id = Some "user"; host_id = Some "host" }
    in
    let store = Shell_runtime.Approval_store.memory ~bindings () in
    let exact = identity () in
    Shell_runtime.Approval_store.remember
      store
      ~now:(at 100.)
      ~session_id:(Some "session")
      exact
      (A.Exact_session { expires_at = Some 101. })
      None
    |> approval_ok;
    assert (
      Option.is_some
        (Shell_runtime.Approval_store.lookup
           store
           ~now:(at 100.5)
           ~session_id:(Some "session")
           exact
         |> approval_ok));
    assert (
      Option.is_none
        (Shell_runtime.Approval_store.lookup
           store
           ~now:(at 102.)
           ~session_id:(Some "session")
           exact
         |> approval_ok));
    Shell_runtime.Approval_store.remember
      store
      ~now:(at 200.)
      ~session_id:(Some "session")
      (identity ~argv:[ "git"; "status" ] ~command_hash:"git-status" ())
      (A.Prefix_session { prefix = [ "git" ]; expires_at = None })
      None
    |> approval_ok;
    let prefix_match = identity ~argv:[ "git"; "diff" ] ~command_hash:"different" () in
    assert (
      Option.is_some
        (Shell_runtime.Approval_store.lookup
           store
           ~now:(at 201.)
           ~session_id:(Some "session")
           prefix_match
         |> approval_ok));
    let grant =
      Shell_runtime.Approval_store.list store
      |> approval_ok
      |> List.find_exn ~f:(fun grant ->
        match grant.Session.Shell_state.Approval_grant.scope with
        | Prefix_session _ -> true
        | Exact_session | Durable_exact -> false)
    in
    Shell_runtime.Approval_store.revoke
      store
      ~now:(at 202.)
      ~grant_id:grant.grant_id
      ~reason:(Some "test")
    |> approval_ok;
    assert (
      Option.is_none
        (Shell_runtime.Approval_store.lookup
           store
           ~now:(at 203.)
           ~session_id:(Some "session")
           prefix_match
         |> approval_ok)))
;;

let%test_unit "durable approval file survives reopen, is 0600, and rejects tampering" =
  Eio_main.run (fun env ->
    let path =
      sprintf
        "/tmp/ochat-phase5-grants-%d-%d.bin"
        (Core_unix.getpid () |> Pid.to_int)
        (Random.bits ())
    in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let bindings =
      Shell_runtime.Approval_store.{ user_id = Some "user"; host_id = Some "host" }
    in
    Fun.protect
      ~finally:(fun () -> if Eio.Path.is_file file then Eio.Path.unlink file)
      (fun () ->
         let store =
           Shell_runtime.Approval_store.durable_file
             ~env
             ~path
             ~integrity_key:"test-key"
             ~bindings
             ()
           |> approval_ok
         in
         Shell_runtime.Approval_store.remember
           store
           ~now:(at 100.)
           ~session_id:(Some "session")
           (identity ())
           (A.Durable_exact { expires_at = None })
           None
         |> approval_ok;
         assert ((Eio.Path.stat ~follow:true file).perm land 0o077 = 0);
         let reopened =
           Shell_runtime.Approval_store.durable_file
             ~env
             ~path
             ~integrity_key:"test-key"
             ~bindings
             ()
           |> approval_ok
         in
         assert (
           Option.is_some
             (Shell_runtime.Approval_store.lookup
                reopened
                ~now:(at 101.)
                ~session_id:(Some "other-session")
                (identity ())
              |> approval_ok));
         Eio.Path.save ~create:(`Or_truncate 0o600) file "tampered";
         assert (
           Result.is_error
             (Shell_runtime.Approval_store.durable_file
                ~env
                ~path
                ~integrity_key:"test-key"
                ~bindings
                ()))))
;;

let%test_unit "failed durable approval mutation preserves the last committed file" =
  Eio_main.run (fun env ->
    let root_name =
      sprintf
        "/tmp/ochat-phase5-grant-failure-%d-%d"
        (Core_unix.getpid () |> Pid.to_int)
        (Random.bits ())
    in
    let fs = Eio.Stdenv.fs env in
    let root = Eio.Path.(fs / root_name) in
    let path = Filename.concat root_name "grants.bin" in
    let bindings = Shell_runtime.Approval_store.{ user_id = None; host_id = None } in
    Eio.Path.mkdirs ~perm:0o700 root;
    Fun.protect
      ~finally:(fun () ->
        Core_unix.chmod root_name ~perm:0o700;
        Eio.Path.rmtree ~missing_ok:true root)
      (fun () ->
         let store =
           Shell_runtime.Approval_store.durable_file
             ~env
             ~path
             ~integrity_key:"test-key"
             ~bindings
             ()
           |> approval_ok
         in
         Shell_runtime.Approval_store.remember
           store
           ~now:(at 100.)
           ~session_id:(Some "session")
           (identity ~command_hash:"committed" ())
           (A.Durable_exact { expires_at = None })
           None
         |> approval_ok;
         Core_unix.chmod root_name ~perm:0o500;
         assert (
           Result.is_error
             (Shell_runtime.Approval_store.remember
                store
                ~now:(at 101.)
                ~session_id:(Some "session")
                (identity ~command_hash:"uncommitted" ~argv:[ "echo"; "new" ] ())
                (A.Durable_exact { expires_at = None })
                None));
         Core_unix.chmod root_name ~perm:0o700;
         let reopened =
           Shell_runtime.Approval_store.durable_file
             ~env
             ~path
             ~integrity_key:"test-key"
             ~bindings
             ()
           |> approval_ok
         in
         match Shell_runtime.Approval_store.list reopened |> approval_ok with
         | [ grant ] -> assert (String.equal grant.command_sha256 "committed")
         | _ -> failwith "failed mutation changed the committed grant set"))
;;

let%test_unit
    "persisted manifest grants bind source, imports, versions, session, user, and host"
  =
  let manifest = basic_manifest "one" in
  let session = ref (Session.create ~id:"session" ~prompt_file:"prompt.chatmd" ()) in
  let persist updated =
    session := updated;
    Ok ()
  in
  let source =
    Shell_runtime.Manifest_grant_store.
      { canonical_source_root = "/workspace"
      ; source_sha256 = "source-digest"
      ; repository_identity = Some "repository"
      }
  in
  let bindings =
    Shell_runtime.Manifest_grant_store.{ user_id = Some "user"; host_id = Some "host" }
  in
  let authorizer =
    Shell_runtime.Manifest_grant_store.session_authorizer
      ~session
      ~persist
      ~source
      ~bindings
      ~fallback:Shell_runtime.Manifest_authorizer.assume_authorized
  in
  ignore
    (Shell_runtime.Manifest_authorizer.authorize authorizer manifest |> authorizer_ok
     : Shell_runtime.Manifest_authorizer.grant);
  assert (List.length !session.shell_state.manifest_grants = 1);
  let restart =
    Shell_runtime.Manifest_grant_store.session_authorizer
      ~session
      ~persist
      ~source
      ~bindings
      ~fallback:Shell_runtime.Manifest_authorizer.deny
  in
  assert (Result.is_ok (Shell_runtime.Manifest_authorizer.authorize restart manifest));
  assert (
    Result.is_error
      (Shell_runtime.Manifest_authorizer.authorize restart (basic_manifest "changed")));
  let wrong_source =
    Shell_runtime.Manifest_grant_store.session_authorizer
      ~session
      ~persist
      ~source:{ source with source_sha256 = "changed-source" }
      ~bindings
      ~fallback:Shell_runtime.Manifest_authorizer.deny
  in
  assert (
    Result.is_error (Shell_runtime.Manifest_authorizer.authorize wrong_source manifest))
;;

let octets_of_hex hex =
  let digit = function
    | '0' .. '9' as value -> Char.to_int value - Char.to_int '0'
    | 'a' .. 'f' as value -> 10 + Char.to_int value - Char.to_int 'a'
    | 'A' .. 'F' as value -> 10 + Char.to_int value - Char.to_int 'A'
    | value -> failwithf "invalid hex digit: %c" value ()
  in
  String.init
    (String.length hex / 2)
    ~f:(fun index ->
      Char.of_int_exn ((digit hex.[index * 2] * 16) + digit hex.[(index * 2) + 1]))
;;

let%test_unit "Ed25519 manifest signatures detect canonical-manifest tampering" =
  let module E = Mirage_crypto_ec.Ed25519 in
  let private_key =
    match
      E.priv_of_octets
        (octets_of_hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
    with
    | Ok key -> key
    | Error _ -> failwith "invalid fixed Ed25519 private key"
  in
  let manifest = basic_manifest "signed" in
  let signed_payload =
    Shell_runtime.Manifest_signature.payload
      ~manifest
      ~issuer:"example.org"
      ~audience:[ "ochat" ]
      ~issued_at_unix:10L
      ~expires_at_unix:(Some 100L)
  in
  let signature =
    Shell_runtime.Manifest_signature.
      { key_id = "test"
      ; algorithm = "ed25519"
      ; payload = signed_payload
      ; signature_base64 =
          E.sign ~key:private_key (canonical_payload signed_payload) |> Base64.encode_exn
      }
  in
  let public_keys =
    Shell_runtime.Manifest_signature.
      [ { key_id = "test"
        ; public_key_base64 =
            E.pub_of_priv private_key |> E.pub_to_octets |> Base64.encode_exn
        }
      ]
  in
  assert (
    Result.is_ok
      (Shell_runtime.Manifest_signature.verify
         ~now_unix:50L
         ~audience:"ochat"
         ~public_keys
         ~manifest
         signature));
  assert (
    Result.is_error
      (Shell_runtime.Manifest_signature.verify
         ~now_unix:50L
         ~audience:"ochat"
         ~public_keys
         ~manifest:(basic_manifest "tampered")
         signature))
;;

let%test_unit "Ed25519 signatures cover every authority-bearing payload field" =
  let module E = Mirage_crypto_ec.Ed25519 in
  let private_key =
    match
      E.priv_of_octets
        (octets_of_hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
    with
    | Ok key -> key
    | Error _ -> failwith "invalid fixed Ed25519 private key"
  in
  let manifest = basic_manifest "covered" in
  let signed_payload =
    Shell_runtime.Manifest_signature.payload
      ~manifest
      ~issuer:"example.org"
      ~audience:[ "ochat" ]
      ~issued_at_unix:10L
      ~expires_at_unix:(Some 100L)
  in
  let signature =
    Shell_runtime.Manifest_signature.
      { key_id = "test"
      ; algorithm = "ed25519"
      ; payload = signed_payload
      ; signature_base64 =
          E.sign ~key:private_key (canonical_payload signed_payload) |> Base64.encode_exn
      }
  in
  let public_keys =
    Shell_runtime.Manifest_signature.
      [ { key_id = "test"
        ; public_key_base64 =
            E.pub_of_priv private_key |> E.pub_to_octets |> Base64.encode_exn
        }
      ]
  in
  let is_rejected candidate =
    Shell_runtime.Manifest_signature.verify
      ~now_unix:50L
      ~audience:"ochat"
      ~public_keys
      ~manifest
      { signature with payload = candidate }
    |> Result.is_error
  in
  let payloads =
    Shell_runtime.Manifest_signature.
      [ { signed_payload with canonical_manifest_sha256 = "changed" }
      ; { signed_payload with encoding_version = "changed" }
      ; { signed_payload with builtin_versions = [ "runtime", "changed" ] }
      ; { signed_payload with issuer = "changed" }
      ; { signed_payload with audience = [ "ochat"; "changed" ] }
      ; { signed_payload with issued_at_unix = 11L }
      ; { signed_payload with expires_at_unix = Some 99L }
      ; { signed_payload with imported_source_sha256 = [ "import.chatmd", "changed" ] }
      ]
  in
  assert (List.for_all payloads ~f:is_rejected)
;;

let%test_unit "administrative policy forbids raw-shell prefix grants" =
  let manifest =
    compile_exn
      {|<shell_access id="raw" extends="builtin:yolo@1">
          <approvals provider="ui" unavailable="deny"
            scopes="once,prefix_session" durable="false"/>
        </shell_access>
        <tool name="raw-tool" type="shell" mode="raw" runtime="raw"
          executable="/bin/zsh"/>|}
  in
  let policy =
    { Shell_runtime.Admin_policy.permissive with
      source = "test-policy"
    ; allow_raw_prefix_grants = false
    }
  in
  match
    Shell_runtime.Admin_policy.evaluate
      policy
      ~manifest
      ~runtimes:manifest.payload.runtimes
  with
  | Ok () -> failwith "expected administrative rejection"
  | Error violations ->
    assert (
      List.exists violations ~f:(fun violation ->
        String.equal violation.code "shell.admin_raw_prefix_grant_denied"))
;;

let%test_unit "administrative denial and manifest denial both preempt reviewers" =
  Eio_main.run (fun env ->
    let reviewer_calls = ref 0 in
    let reviewer _ =
      Int.incr reviewer_calls;
      A.Approve
    in
    let capabilities =
      Shell_access.Capabilities.development
        ~workspace:(Eio.Path.native_exn (Eio.Stdenv.cwd env))
    in
    let run ~policy ~administrative_check =
      let config =
        Shell_access.Executor.config
          ~env
          ~runtime_id:"precedence"
          ~manifest_sha256:"manifest"
          ~policy
          ~capabilities
          ~reviewer
          ~administrative_check
          ~backends:[ Shell_access.Backend.direct ]
          ()
      in
      Shell_access.Executor.run
        config
        { request =
            Shell_access.Request.command
              (Shell_access.Command.create "/bin/echo" [ "safe" ])
        ; input = Shell_access.Input.Empty
        ; rationale = None
        ; origin = Shell_access.Context.Host "phase5-test"
        }
    in
    let ask = Shell_access.Policy.create ~default:Ask [] in
    assert (
      match
        run ~policy:ask ~administrative_check:(fun _ -> Error "organization deny")
      with
      | Error (Denied "organization deny") -> true
      | Ok _ | Error _ -> false);
    assert (Int.equal !reviewer_calls 0);
    let deny = Shell_access.Policy.create ~default:Deny [] in
    assert (
      match run ~policy:deny ~administrative_check:(fun _ -> Ok ()) with
      | Error (Denied _) -> true
      | Ok _ | Error _ -> false);
    assert (Int.equal !reviewer_calls 0))
;;

let%test_unit "chained audit resumes after reopen and replay detects tampering" =
  Eio_main.run (fun env ->
    let path =
      sprintf
        "/tmp/ochat-phase5-audit-%d-%d.jsonl"
        (Core_unix.getpid () |> Pid.to_int)
        (Random.bits ())
    in
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    Fun.protect
      ~finally:(fun () ->
        if Eio.Path.is_file file then Eio.Path.unlink file;
        let lock = Eio.Path.(Eio.Stdenv.fs env / (path ^ ".lock")) in
        if Eio.Path.is_file lock then Eio.Path.unlink lock)
      (fun () ->
         let append event request_id =
           Shell_runtime.Audit_sink.append_management_event
             ~env
             ~path
             ~session_id:(Some "session")
             ~runtime_id:"runtime"
             ~manifest_sha256:"manifest"
             ~request_id
             ~event
             ~fields:[ "reason", `String "redacted" ]
           |> audit_append_ok
         in
         assert (Int64.equal (append "grant_revoked" "one") 0L);
         assert (Int64.equal (append "manifest_grant_revoked" "two") 1L);
         let events =
           Shell_runtime.Audit_replay.load ~fs:(Eio.Stdenv.fs env) ~path |> audit_load_ok
         in
         assert (Result.is_ok (Shell_runtime.Audit_replay.validate events));
         let tampered =
           Eio.Path.load file
           |> String.substr_replace_first
                ~pattern:"manifest_grant_revoked"
                ~with_:"manifest_grant_changed"
         in
         Eio.Path.save ~create:(`Or_truncate 0o600) file tampered;
         let events =
           Shell_runtime.Audit_replay.load ~fs:(Eio.Stdenv.fs env) ~path |> audit_load_ok
         in
         assert (Result.is_error (Shell_runtime.Audit_replay.validate events))))
;;

let context () =
  let command = Shell_access.Command.create "rm" [ "-rf"; "build" ] in
  let executable =
    Shell_access.Executable.
      { requested = "rm"
      ; path = "/bin/rm"
      ; canonical_path = "/bin/rm"
      ; trusted = true
      ; fingerprint =
          { device = 1
          ; inode = 2
          ; mode = 0o755
          ; uid = 0
          ; gid = 0
          ; size = 1L
          ; mtime = 1.
          ; sha256 = "executable"
          }
      }
  in
  Shell_access.Context.
    { request_id = "request"
    ; runtime_id = "runtime"
    ; manifest_sha256 = "manifest"
    ; command
    ; executable
    ; cwd = "/workspace"
    ; environment = [| "PATH=/bin" |]
    ; request_kind = Structured
    ; stdin_kind = Empty
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; script_preview = None
    ; origin = Tool
    ; effects = [ Write_path "/workspace/build"; Child_processes ]
    ; capabilities = Shell_access.Capabilities.development ~workspace:"/workspace"
    ; policy_action = Some "ask"
    ; policy_matches = [ "destructive" ]
    ; session_id = Some "session"
    }
;;

let approval_ui_request () =
  let context = context () in
  let identity =
    identity
      ~argv:(Shell_access.Command.to_argv context.command)
      ~command_hash:"command"
      ()
  in
  let approval_request =
    A.
      { context
      ; policy = { action = Ask; matches = []; reason = "project policy" }
      ; identity
      ; display_command = "rm -rf build"
      ; rationale = Some "clean build output"
      }
  in
  Shell_runtime.Approval_broker.
    { id = "approval"
    ; request = approval_request
    ; runtime_id = "runtime"
    ; manifest_sha256 = "manifest"
    ; scopes = [ S.Once; Exact_session; Prefix_session; Durable_exact ]
    }
;;

let make_model () =
  Chat_tui.Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:"multiline\ndraft"
    ~auto_follow:false
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos:4
    ~selection_anchor:(Some 1)
    ~mode:Chat_tui.Model.Insert
    ~draft_mode:Raw_xml
    ~selected_msg:None
    ~undo_stack:[ "older", 2 ]
    ~redo_stack:[ "newer", 3 ]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let%test_unit "shell page and modal preserve draft; broad grants require confirmation" =
  let model = make_model () in
  let draft =
    ( Chat_tui.Model.input_line model
    , Chat_tui.Model.cursor_pos model
    , Chat_tui.Model.selection_anchor model )
  in
  ignore
    (Chat_tui.Controller_cmdline.execute_command model "shell"
     : Chat_tui.Controller_types.reaction);
  (match Chat_tui.Model.active_page model with
   | Shell_security -> ()
   | Chat | Agent -> failwith "shell command did not open Shell Security");
  assert (
    [%equal: string * int * int option]
      draft
      ( Chat_tui.Model.input_line model
      , Chat_tui.Model.cursor_pos model
      , Chat_tui.Model.selection_anchor model ));
  Chat_tui.Model.open_shell_approval_modal
    model
    ~request:(approval_ui_request ())
    ~queue_count:2;
  let term : Notty_eio.Term.t = Obj.magic 0 in
  ignore
    (Chat_tui.Controller.handle_key ~model ~term (`Key (`ASCII 'm', []))
     : Chat_tui.Controller.reaction);
  ignore
    (Chat_tui.Controller.handle_key ~model ~term (`Key (`ASCII '3', []))
     : Chat_tui.Controller.reaction);
  (match Chat_tui.Controller.handle_key ~model ~term (`Key (`Enter, [])) with
   | Redraw -> ()
   | _ -> failwith "prefix approval skipped confirmation");
  (match (Chat_tui.Model.shell_approval_modal model |> Option.value_exn).stage with
   | Confirm_prefix [ "rm"; "-rf"; "build" ] -> ()
   | _ -> failwith "wrong prefix confirmation");
  (match Chat_tui.Controller.handle_key ~model ~term (`Key (`Enter, [])) with
   | Shell_approval_response ("approval", Approve_prefix_session [ "rm"; "-rf"; "build" ])
     -> ()
   | _ -> failwith "wrong confirmed prefix response");
  assert (
    [%equal: string * int * int option]
      draft
      ( Chat_tui.Model.input_line model
      , Chat_tui.Model.cursor_pos model
      , Chat_tui.Model.selection_anchor model ));
  let image, _ = Chat_tui.Renderer.render_full ~size:(80, 28) ~model in
  assert (Notty.I.width image <= 80 && Notty.I.height image <= 28)
;;

let%test_unit "grant revocation uses stable selection and a non-optimistic modal" =
  let model = make_model () in
  let draft = Chat_tui.Model.input_line model, Chat_tui.Model.cursor_pos model in
  Chat_tui.Model.set_active_page model Shell_security;
  Chat_tui.Model.set_shell_security_tab model Grants;
  Chat_tui.Model.set_shell_security_snapshot
    model
    { Chat_tui.Model.Shell_security_page_state.empty_snapshot with
      grants = [ sample_approval_grant ]
    };
  let term : Notty_eio.Term.t = Obj.magic 0 in
  ignore
    (Chat_tui.Controller.handle_key ~model ~term (`Key (`ASCII 'x', []))
     : Chat_tui.Controller.reaction);
  let modal = Chat_tui.Model.shell_grant_revoke_modal model |> Option.value_exn in
  assert (String.equal modal.grant_id "grant");
  (match Chat_tui.Controller.handle_key ~model ~term (`Key (`Enter, [])) with
   | Shell_grant_revoke_requested (generation, "grant") ->
     Chat_tui.Model.mark_shell_grant_revoking model ~generation ~grant_id:"grant";
     (match (Chat_tui.Model.shell_grant_revoke_modal model |> Option.value_exn).stage with
      | Revoking -> ()
      | Confirm_revoke | Revoke_failed _ -> failwith "grant was removed optimistically")
   | _ -> failwith "revocation confirmation did not emit a mutation request");
  assert (
    [%equal: string * int]
      draft
      (Chat_tui.Model.input_line model, Chat_tui.Model.cursor_pos model));
  assert (List.length (Chat_tui.Model.shell_security_snapshot model).grants = 1)
;;

let remove_file fs path =
  let file = Eio.Path.(fs / path) in
  if Eio.Path.is_file file then Eio.Path.unlink file
;;

let%test_unit "concurrent durable approval writers preserve both grants" =
  Eio_main.run (fun env ->
    let path =
      sprintf
        "/tmp/ochat-phase5-concurrent-%d-%d.bin"
        (Core_unix.getpid () |> Pid.to_int)
        (Random.bits ())
    in
    let fs = Eio.Stdenv.fs env in
    let bindings =
      Shell_runtime.Approval_store.{ user_id = Some "user"; host_id = Some "host" }
    in
    Fun.protect
      ~finally:(fun () ->
        remove_file fs path;
        remove_file fs (path ^ ".lock"))
      (fun () ->
         let store () =
           Shell_runtime.Approval_store.durable_file
             ~env
             ~path
             ~integrity_key:"test-key"
             ~bindings
             ()
           |> approval_ok
         in
         let left = store () in
         let right = store () in
         let remember store identity =
           Shell_runtime.Approval_store.remember
             store
             ~now:(at 100.)
             ~session_id:(Some "session")
             identity
             (A.Durable_exact { expires_at = None })
             None
           |> approval_ok
         in
         Eio.Fiber.both
           (fun () ->
              remember left (identity ~command_hash:"left" ~argv:[ "echo"; "left" ] ()))
           (fun () ->
              remember right (identity ~command_hash:"right" ~argv:[ "echo"; "right" ] ()))
         |> ignore;
         let reopened = store () in
         assert (
           List.length (Shell_runtime.Approval_store.list reopened |> approval_ok) = 2)))
;;

let audit_envelope sequence event =
  let ctx = context () in
  Shell_access.Audit.
    { sequence
    ; timestamp = Int64.to_float sequence
    ; session_id = ctx.session_id
    ; runtime_id = ctx.runtime_id
    ; manifest_sha256 = ctx.manifest_sha256
    ; request_id = ctx.request_id
    ; plan_id = None
    ; event
    ; dropped_fields = String.Set.empty
    ; replacement_fields = String.Map.empty
    }
;;

let%test_unit "rotating audit replay loads the complete retained integrity chain" =
  Eio_main.run (fun env ->
    let path =
      sprintf
        "/tmp/ochat-phase5-rotation-%d-%d.jsonl"
        (Core_unix.getpid () |> Pid.to_int)
        (Random.bits ())
    in
    let fs = Eio.Stdenv.fs env in
    Fun.protect
      ~finally:(fun () ->
        remove_file fs path;
        remove_file fs (path ^ ".lock");
        List.iter (List.range 1 21) ~f:(fun index ->
          remove_file fs (sprintf "%s.%d" path index)))
      (fun () ->
         let sink =
           Shell_runtime.Audit_sink.create_rotating_jsonl
             ~env
             ~path
             ~max_bytes:2_400L
             ~max_files:20
             ~content:Chatmd_shell_spec.Shell_spec.Metadata
             ~failure_policy:Shell_access.Audit.Terminate_runtime
             ~secret_filter:Shell_access.Secret_filter.empty
             ~integrity_chaining:true
           |> audit_append_ok
         in
         List.iter (List.range 0 18) ~f:(fun sequence ->
           let event = Shell_access.Audit.Resolved (context ()) in
           Shell_access.Audit.write sink (audit_envelope (Int64.of_int sequence) event)
           |> Result.ok_or_failwith);
         assert (Eio.Path.is_file Eio.Path.(fs / (path ^ ".1")));
         let events =
           Shell_runtime.Audit_replay.load_rotated ~fs ~path |> audit_load_ok
         in
         assert (List.length events = 18);
         assert (Result.is_ok (Shell_runtime.Audit_replay.validate events))))
;;

let%test_unit "audit fan-out writes every sink and propagates the strongest failure" =
  let writes = ref [] in
  let sink name failure_policy result =
    Shell_access.Audit.create ~failure_policy (fun _ ->
      writes := name :: !writes;
      result)
  in
  let fan_out =
    Shell_runtime.Audit_sink.fan_out
      [ sink "session" Shell_access.Audit.Ignore_failure (Ok ())
      ; sink "organization" Deny_start (Error "collector unavailable")
      ]
  in
  assert (
    match Shell_access.Audit.failure_policy fan_out with
    | Deny_start -> true
    | Ignore_failure | Terminate_runtime -> false);
  assert (
    Result.is_error
      (Shell_access.Audit.write fan_out (audit_envelope 0L (Resolved (context ())))));
  assert ([%equal: string list] (List.rev !writes) [ "session"; "organization" ])
;;

let%test_unit "executor timeout is a terminal replay event, not an interrupted request" =
  Eio_main.run (fun env ->
    let path =
      sprintf
        "/tmp/ochat-phase5-timeout-%d-%d.jsonl"
        (Core_unix.getpid () |> Pid.to_int)
        (Random.bits ())
    in
    let fs = Eio.Stdenv.fs env in
    Fun.protect
      ~finally:(fun () ->
        remove_file fs path;
        remove_file fs (path ^ ".lock"))
      (fun () ->
         Eio.Switch.run (fun sw ->
           let audit =
             Shell_runtime.Audit_sink.create_chained_jsonl
               ~env
               ~sw
               ~path:Eio.Path.(fs / path)
               ~content:Chatmd_shell_spec.Shell_spec.Metadata
               ~failure_policy:Shell_access.Audit.Terminate_runtime
               ~secret_filter:Shell_access.Secret_filter.empty
             |> audit_append_ok
           in
           let capabilities =
             Shell_access.Capabilities.
               { (development ~workspace:(Eio.Path.native_exn (Eio.Stdenv.cwd env))) with
                 sandbox = Direct_unsafe
               }
           in
           let limits =
             Shell_access.Limits.
               { default with wall_time_seconds = 0.03; idle_time_seconds = None }
           in
           let config =
             Shell_access.Executor.config
               ~env
               ~runtime_id:"timeout"
               ~manifest_sha256:"manifest"
               ~policy:(Shell_access.Policy.create ~default:Allow [])
               ~capabilities
               ~limits
               ~audit
               ~backends:[ Shell_access.Backend.direct ]
               ()
           in
           let invocation =
             Shell_access.Executor.
               { request =
                   Shell_access.Request.command
                     (Shell_access.Command.create "/bin/sleep" [ "1" ])
               ; input = Shell_access.Input.Empty
               ; rationale = Some "timeout test"
               ; origin = Shell_access.Context.Host "test"
               }
           in
           assert (
             match Shell_access.Executor.run config invocation with
             | Error (Timed_out _) -> true
             | Ok _ | Error _ -> false));
         let events = Shell_runtime.Audit_replay.load ~fs ~path |> audit_load_ok in
         assert (Result.is_ok (Shell_runtime.Audit_replay.validate events));
         let request = List.hd_exn (Shell_runtime.Audit_replay.requests events) in
         assert request.completed;
         assert (
           Option.value_map request.exit_kind ~default:false ~f:(String.equal "timed_out"))))
;;

let%test_unit "interrupted reconstruction never marks a request retryable" =
  Eio_main.run (fun env ->
    let session_id =
      sprintf
        "phase5-interrupted-%d-%d"
        (Core_unix.getpid () |> Pid.to_int)
        (Random.bits ())
    in
    let session_dir = Session_store.ensure_dir ~env session_id in
    let path =
      Filename.concat (Session_store.rel_path session_id) ".chatmd/shell-audit.jsonl"
    in
    Fun.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true session_dir)
      (fun () ->
         Shell_runtime.Audit_sink.append_management_event
           ~env
           ~path
           ~session_id:(Some session_id)
           ~runtime_id:"runtime"
           ~manifest_sha256:"manifest"
           ~request_id:"unfinished"
           ~event:"started"
           ~fields:
             [ "request_kind", `String "structured"
             ; "command_sha256", `String "command"
             ; "cwd_sha256", `String "cwd"
             ; "effects", `Array [ `String "child_processes" ]
             ]
         |> audit_append_ok
         |> ignore;
         let session = Session.create ~id:session_id ~prompt_file:"prompt.chatmd" () in
         let refreshed =
           Shell_runtime.Interrupted_store.refresh ~env ~session |> Result.ok_or_failwith
         in
         match refreshed.shell_state.interrupted_requests with
         | [ request ] ->
           assert (String.equal request.request_id "unfinished");
           assert (not request.retryable)
         | _ -> failwith "expected one interrupted request"))
;;

let%test_unit "moderator interaction survives shell preemption without touching the draft"
  =
  let model = make_model () in
  let draft = Chat_tui.Model.input_line model, Chat_tui.Model.cursor_pos model in
  Chat_tui.Model.open_moderator_modal
    model
    (Chat_response.In_memory_stream.Ask_text { prompt = "Why continue?" });
  let moderator = Chat_tui.Model.moderator_modal model |> Option.value_exn in
  moderator.response <- "because";
  moderator.cursor <- String.length moderator.response;
  Chat_tui.Model.open_shell_approval_modal
    model
    ~request:(approval_ui_request ())
    ~queue_count:1;
  assert (
    String.equal
      (Chat_tui.Model.moderator_modal model |> Option.value_exn).response
      "because");
  Chat_tui.Model.close_shell_approval_modal model;
  assert (
    String.equal
      (Chat_tui.Model.shell_interaction_id model |> Option.value_exn)
      "moderator-text");
  assert (
    [%equal: string * int]
      draft
      (Chat_tui.Model.input_line model, Chat_tui.Model.cursor_pos model))
;;

let%test_unit "stale audit loads cannot replace a newer Shell Security refresh" =
  let model = make_model () in
  let first = Chat_tui.Model.begin_shell_management_load model in
  let second = Chat_tui.Model.begin_shell_management_load model in
  let page =
    Chat_tui.Model.Shell_security_page_state.
      { path = "audit.jsonl"
      ; integrity = "verified"
      ; total_requests = 0
      ; requests = []
      ; last_sequence = Some 4L
      }
  in
  assert (not (Chat_tui.Model.finish_shell_management_load model ~generation:first page));
  assert (Chat_tui.Model.finish_shell_management_load model ~generation:second page)
;;

let extension_source_ref source =
  let position = Chatmd_shell_spec.Source_ref.{ offset = 0; line = 1; column = 0 } in
  Chatmd_shell_spec.Source_ref.create
    ~file:"phase5-extension.chatmd"
    ~source_dir:"."
    ~prompt_dir:"."
    ~namespace:None
    ~start_pos:position
    ~end_pos:position
    ~source
;;

let stateful_matcher_source =
  {|
    type state = { count : int }
    let initial_state = { count = 0 }
    let match_command : shell_context -> state -> state task =
      fun ctx st ->
        Task.bind(Match.yes(to_string(st.count)), fun ignored ->
        Task.pure({ count = st.count + 1 }))
  |}
;;

let stateful_matcher_script () =
  Chatmd_shell_spec.Chatmd_script_spec.
    { id = "persisted-matcher"
    ; language = "chatml"
    ; kind = Shell_matcher
    ; source = Inline stateful_matcher_source
    ; source_ref = extension_source_ref stateful_matcher_source
    ; source_sha256 = Chatmd_shell_spec.Source_ref.digest stateful_matcher_source
    ; limits = default_limits
    }
;;

let extension_context () =
  Shell_runtime.Chatml_context_value.
    { phase = "shell_matcher"
    ; request_id = "request"
    ; runtime_id = "runtime"
    ; manifest_sha256 = "manifest"
    ; argv = [ "echo" ]
    ; executable =
        { requested = "echo"
        ; path = "/bin/echo"
        ; canonical_path = "/bin/echo"
        ; trusted = true
        ; sha256 = "executable"
        }
    ; cwd = "/workspace"
    ; origin = "tool"
    ; request_kind = "structured"
    ; stdin_kind = "empty"
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; script_preview = None
    ; effects = []
    ; capabilities =
        { read_roots = [ "/workspace" ]
        ; write_roots = []
        ; network = false
        ; child_processes = false
        ; arbitrary_code = false
        ; privilege_change = false
        ; sandbox = "required"
        }
    ; session_id = Some "session"
    ; policy = None
    }
  |> Shell_runtime.Chatml_context_value.encode
;;

let extension_ok = function
  | Ok value -> value
  | Error diagnostic -> failwith (Chatmd_shell_spec.Diagnostic.to_string diagnostic)
;;

let%test_unit
    "stateful ChatML shell extensions checkpoint, restore, and roll back failed \
     persistence"
  =
  Eio_main.run (fun env ->
    let module Extension = Shell_runtime.Chatml_extension in
    let compiled =
      Extension.compile ~script:(stateful_matcher_script ())
      |> Result.map_error ~f:(fun diagnostics ->
        diagnostics
        |> List.map ~f:Chatmd_shell_spec.Diagnostic.to_string
        |> String.concat ~sep:"; ")
      |> Result.ok_or_failwith
    in
    let create () =
      Extension.instantiate ~env ~lifecycle:Chatmd_shell_spec.Shell_spec.Session compiled
      |> extension_ok
    in
    let call instance =
      let value = extension_context () in
      Extension.call instance ~context:value ~event:value
    in
    let saved = ref None in
    let first = create () in
    Extension.set_snapshot_handler first (fun snapshot ->
      saved := Some snapshot;
      Ok ());
    ignore (call first |> extension_ok : Extension.action);
    ignore (call first |> extension_ok : Extension.action);
    let restored = create () in
    Extension.restore restored (Option.value_exn !saved) |> extension_ok;
    (match call restored |> extension_ok with
     | Extension.Match (true, "2") -> ()
     | action -> failwith (Sexp.to_string_hum (Extension.sexp_of_action action)));
    let rollback = create () in
    Extension.set_snapshot_handler rollback (fun _ -> Error "session save failed");
    assert (Result.is_error (call rollback));
    Extension.set_snapshot_handler rollback (fun _ -> Ok ());
    match call rollback |> extension_ok with
    | Extension.Match (true, "0") -> ()
    | action -> failwith (Sexp.to_string_hum (Extension.sexp_of_action action)))
;;
