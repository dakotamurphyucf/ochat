open Core

type t =
  { environment : Temporary_environment.t
  ; directory : string
  ; config_path : string
  ; data_dir : string
  ; unix_socket : string
  ; http_port : int
  ; admin_token : string
  ; public_token : string
  ; token_path : string
  ; prompt_path : string
  ; physical_workspace : string
  }

let path t value = Temporary_environment.path t.environment value
let atom value = Sexp.to_string_mach (Sexp.Atom value)
let generate_token () = Mirage_crypto_rng.generate 32 |> Base64.encode_exn

let all_scope_names =
  Agent_protocol.Scope.
    [ List_prompts
    ; List_workspaces
    ; Create_sessions
    ; View_session_transcript
    ; Send_messages
    ; Own_sessions
    ; Answer_approvals
    ; View_security_state
    ; Manage_grants
    ; Read_audit
    ; Stop_sessions
    ; Delete_sessions
    ; Administer_configuration
    ; Diagnostics
    ; Submit_ingress
    ]
  |> List.map ~f:Agent_protocol.Scope.to_string
;;

let token_record token principal scopes =
  let digest = Digestif.SHA256.digest_string token |> Digestif.SHA256.to_hex in
  sprintf
    "((token_sha256 %s) (principal_id %s) (scopes (%s)) (attributes ()) (expires_at \
     none))"
    digest
    (Agent_protocol.Id.Principal.to_string principal)
    (String.concat ~sep:" " scopes)
;;

let write_tokens_with_public_scopes t public_scopes =
  let admin = Agent_protocol.Id.Principal.create () in
  let public = Agent_protocol.Id.Principal.create () in
  let contents =
    sprintf
      "(%s %s)"
      (token_record t.admin_token admin all_scope_names)
      (token_record t.public_token public public_scopes)
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) (path t t.token_path) contents
;;

let write_tokens t = write_tokens_with_public_scopes t []
let grant_public_all_scopes t = write_tokens_with_public_scopes t all_scope_names

let configuration t ?data_dir ?unix_socket ?http_port () =
  let data_dir = Option.value data_dir ~default:t.data_dir |> atom in
  let unix_socket = Option.value unix_socket ~default:t.unix_socket |> atom in
  let http_port = Option.value http_port ~default:t.http_port in
  sprintf
    {|
(version 1)
(server
 ((data_dir %s)
  (unix_socket %s)
  (shutdown_grace_ms 1000)
  (max_attachments_per_session 16)
  (subscriber_queue_capacity 16)
  (http
   ((enabled true)
    (address 127.0.0.1)
    (port %d)
    (require_auth true)
    (static_tokens_file %s)
    (max_connections 32)
    (idle_connection_timeout_ms 5000)))
  (event_retention
   ((completed_stream_ms 60000)
    (response_artifact_ms 60000)
    (max_events_per_session 1000)))
  (durability
   ((journal_flush each)
    (journal_flush_ms 10)
    (snapshot_every_events 10)
    (snapshot_every_ms 1000)))
  (unsafe_allow_unauthenticated_remote_http false)))
(workspaces
 (((id physical)
   (source (physical %s))
   (access shared_write)
   (prompt_limits (((prompt smoke) (max_root_agents 2) (overflow queue)))))
  ((id temporary)
   (source (temporary ((location session_dir) (cleanup on_session_delete))))
   (access exclusive)
   (prompt_limits (((prompt smoke) (max_root_agents 1) (overflow reject)))))))
(prompts
 (((id smoke)
   (path %s)
   (description "E2E smoke prompt")
   (allowed_workspaces (physical temporary))
   (permission_profile unattended)
   (enabled true))))
(permission_profiles
 (((id unattended)
   (tool_default deny)
   (approval_timeout none)
   (approval_fallback deny)
   (manifest_authorization deny))))
(manifest_grants ())
|}
    data_dir
    unix_socket
    http_port
    (atom t.token_path)
    (atom t.physical_workspace)
    (atom t.prompt_path)
;;

let write_configuration t ~name contents =
  let config_path = Filename.concat t.directory name in
  Eio.Path.save ~create:(`Exclusive 0o600) (path t config_path) contents;
  config_path
;;

let create environment ~name ~http_port =
  let roots : Temporary_environment.roots = Temporary_environment.roots environment in
  let directory = Filename.concat roots.config name in
  let make value = Filename.concat directory value in
  Eio.Path.mkdir ~perm:0o700 (Temporary_environment.path environment directory);
  let physical_workspace = make "workspace" in
  Eio.Path.mkdir ~perm:0o700 (Temporary_environment.path environment physical_workspace);
  let t =
    { environment
    ; directory
    ; config_path = make "server.sexp"
    ; data_dir = make "data"
    ; unix_socket = Filename.concat roots.sockets (name ^ ".sock")
    ; http_port
    ; admin_token = generate_token ()
    ; public_token = generate_token ()
    ; token_path = make "tokens.sexp"
    ; prompt_path = make "smoke.chatmd"
    ; physical_workspace
    }
  in
  Temporary_environment.register_secret environment t.admin_token;
  Temporary_environment.register_secret environment t.public_token;
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    (path t t.prompt_path)
    "<developer>You are a deterministic E2E smoke agent.</developer>";
  write_tokens t;
  Eio.Path.save ~create:(`Exclusive 0o600) (path t t.config_path) (configuration t ());
  t
;;

let config_path t = t.config_path
let data_dir t = t.data_dir
let unix_socket t = t.unix_socket
let http_port t = t.http_port
let admin_token t = t.admin_token
let public_token t = t.public_token
let prompt_path t = t.prompt_path
let physical_workspace t = t.physical_workspace
let environment t = t.environment
