open! Core

let protocol_exn = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let path env root name = Eio.Path.(Eio.Stdenv.fs env / root / name)

let write env root name data =
  Eio.Path.save ~create:(`Exclusive 0o600) (path env root name) data
;;

let atom value = Sexp.to_string_mach (Sexp.Atom value)

let token () =
  Mirage_crypto_rng.generate 32 |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
;;

let scopes =
  [ "prompt.list"
  ; "workspace.list"
  ; "session.create"
  ; "session.transcript.read"
  ; "session.message.send"
  ; "session.own"
  ; "permission.respond"
  ; "security.read"
  ; "grant.manage"
  ; "audit.read"
  ; "session.stop"
  ; "session.delete"
  ; "configuration.admin"
  ; "diagnostics.read"
  ]
;;

let record ~principal raw scopes =
  let digest = Digestif.SHA256.digest_string raw |> Digestif.SHA256.to_hex in
  sprintf
    "((token_sha256 %s)(principal_id %s)(scopes (%s))(attributes ())(expires_at none))"
    digest
    principal
    (String.concat ~sep:" " scopes)
;;

let write_credentials env root =
  let principal =
    Agent_protocol.Id.Principal.create () |> Agent_protocol.Id.Principal.to_string
  in
  let admin = token ()
  and observer = token () in
  write env root "admin.token" admin;
  write env root "observer.token" observer;
  write env root "admin.curl" (sprintf "header = \"Authorization: Bearer %s\"\n" admin);
  write
    env
    root
    "observer.curl"
    (sprintf "header = \"Authorization: Bearer %s\"\n" observer);
  write
    env
    root
    "tokens.sexp"
    (sprintf
       "(%s\n%s)"
       (record ~principal admin scopes)
       (record ~principal observer [ "session.transcript.read" ]))
;;

let config root http =
  sprintf
    {|(version 1)
(server ((data_dir %s)(unix_socket %s)
 (http ((enabled %b)(address 127.0.0.1)(port 8787)(require_auth true)(static_tokens_file %s)))))
(workspaces (((id project)(source (physical %s))(access shared_write)
 (prompt_limits (((prompt hello)(max_root_agents 4)(overflow reject)))))))
(prompts (((id hello)(path %s)(allowed_workspaces (project))(permission_profile interactive)(enabled true))))
(permission_profiles (((id interactive)(tool_default ask)(approval_timeout none)
 (approval_fallback deny)(manifest_authorization require_grant))))
(manifest_grants ())
|}
    (atom (Filename.concat root "store"))
    (atom (Filename.concat root "agent.sock"))
    http
    (atom (Filename.concat root "tokens.sexp"))
    (atom (Filename.concat root "workspace"))
    (atom (Filename.concat root "hello.chatmd"))
;;

let validate env file =
  let result =
    Agent_server.Config_parser.load ~env ~path:file
    |> Result.bind ~f:(Agent_server.Config_validator.validate ~env)
  in
  match result with
  | Ok _ -> ()
  | Error errors -> raise_s [%sexp (errors : Agent_server.Config.Diagnostic.t list)]
;;

let setup env root model =
  if not (Filename.is_absolute root)
  then failwith "root must be an absolute, private empty directory";
  if not (List.is_empty (Eio.Path.read_dir (path env root ".")))
  then failwith "root must be empty";
  if
    not
      (String.for_all model ~f:(function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
         | _ -> false))
  then failwith "invalid model name";
  Eio.Path.mkdir ~perm:0o700 (path env root "workspace");
  write
    env
    root
    "hello.chatmd"
    (sprintf
       "<config model=\"%s\"/>\n\
        <developer>You are a concise assistant. No tools are available.</developer>\n"
       model);
  write_credentials env root;
  List.iter
    [ "unix.sexp", false; "http.sexp", true ]
    ~f:(fun (name, http) ->
      write env root name (config root http);
      validate env (Filename.concat root name));
  Eio.Flow.copy_string
    ("Prepared private tutorial directory: " ^ root ^ "\n")
    (Eio.Stdenv.stdout env)
;;

let initialize () =
  Agent_protocol.Initialize.Request.
    { implementation = { name = "ochat-docs"; version = "1" }
    ; protocol_min = Agent_protocol.Version.initial
    ; protocol_max = Agent_protocol.Version.initial
    ; features = []
    ; event_encodings = [ Json ]
    ; max_inbound_event_bytes = 16 * 1024 * 1024
    ; client_instance_id = None
    }
;;

let client env endpoint token_file =
  Eio.Switch.run (fun sw ->
    let bearer_token =
      Option.map token_file ~f:(fun path ->
        Agent_transport_client.Endpoint.load_bearer_token ~env ~path |> protocol_exn)
    in
    let target =
      Agent_transport_client.Endpoint.create
        ~home:(Sys.getenv "HOME")
        ~bearer_token
        endpoint
      |> protocol_exn
    in
    let connection =
      Agent_transport_client.Endpoint.connect target ~sw ~env ~notification_capacity:1024
      |> protocol_exn
    in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close connection)
      (fun () ->
         Agent_transport_stdio.Gateway.run
           ~sw
           ~connection
           ~input:(Eio.Stdenv.stdin env)
           ~output:(Eio.Stdenv.stdout env)
           ~max_line_length:(16 * 1024 * 1024)
           ~outgoing_capacity:1024
           ~on_error:(fun error ->
             Eio.Flow.copy_string
               (Sexp.to_string_hum ([%sexp_of: Agent_protocol.Error.t] error) ^ "\n")
               (Eio.Stdenv.stderr env))))
;;

let check_envelope line =
  match Agent_protocol.Envelope.of_json (Jsonaf.of_string line) |> protocol_exn with
  | Request request ->
    let command =
      Agent_protocol.Command.of_method_and_params
        ~method_:request.method_
        ~params:request.params
      |> protocol_exn
    in
    let encoded = Agent_protocol.Command.params command in
    ignore
      (Agent_protocol.Command.of_method_and_params
         ~method_:request.method_
         ~params:encoded
       |> protocol_exn
       : Agent_protocol.Command.t)
  | Notification _ | Response _ -> failwith "request fixture expected"
;;

let check env file =
  Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / file)
  |> String.split_lines
  |> List.filter ~f:(fun line -> not (String.is_empty (String.strip line)))
  |> List.iter ~f:check_envelope;
  Eio.Flow.copy_string "Request codec validation passed\n" (Eio.Stdenv.stdout env)
;;

let () =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    match Array.to_list (Sys.get_argv ()) with
    | [ _; "setup"; root; model ] ->
      setup env root model;
      let id =
        Agent_protocol.Envelope.Request_id.of_json (`String "initialize") |> protocol_exn
      in
      let request =
        Agent_protocol.Envelope.request
          ~id
          ~method_:"protocol.initialize"
          ~params:(Agent_protocol.Initialize.Request.to_json (initialize ()))
          ()
      in
      write
        env
        root
        "initialize.json"
        (Agent_protocol.Envelope.to_json request |> Jsonaf.to_string)
    | [ _; "validate-config"; file ] -> validate env file
    | [ _; "check-requests"; file ] -> check env file
    | [ _; "client"; endpoint ] -> client env endpoint None
    | [ _; "client"; endpoint; token_file ] -> client env endpoint (Some token_file)
    | _ ->
      failwith
        "usage: docs_example setup ROOT MODEL | validate-config FILE | check-requests \
         FILE | client URI [TOKEN_FILE]")
;;
