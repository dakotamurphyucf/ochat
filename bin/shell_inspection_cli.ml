open Core
open Command.Let_syntax

let print env format =
  ksprintf (fun text -> Eio.Flow.copy_string text (Eio.Stdenv.stdout env)) format
;;

let print_error env format =
  ksprintf (fun text -> Eio.Flow.copy_string text (Eio.Stdenv.stderr env)) format
;;

module Selection = struct
  type t =
    | Legacy of string
    | Configured of
        { config_path : string
        ; prompt_name : string
        ; selection : Agent_server.Daemon.Shell_inspection_selection.t
        }
end

let invocation_arg_type =
  Command.Arg_type.create (function
    | "ui" -> Agent_session.Runtime_script_preparation.Invocation.Ui
    | "headless" -> Headless
    | "delegated-ui" -> Delegated_ui
    | _ -> failwith "-invocation must be ui, headless or delegated-ui")
;;

let revision_arg_type =
  Command.Arg_type.create (fun value ->
    Agent_protocol.Id.Prompt_revision.of_string value
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum ([%sexp_of: Agent_protocol.Error.t] error))
    |> Result.ok_or_failwith)
;;

let select ~path ~config_path ~prompt_name ~revision_id ~invocation =
  match path, config_path, prompt_name, revision_id, invocation with
  | Some path, None, None, None, None when not (String.is_empty path) ->
    Ok (Selection.Legacy path)
  | None, Some config_path, Some prompt_name, revision_id, invocation ->
    if String.is_empty config_path || String.is_empty prompt_name
    then Or_error.error_string "-config and -prompt must be nonempty"
    else
      let open Or_error.Let_syntax in
      let%map selection =
        match revision_id, invocation with
        | None, None -> Ok Agent_server.Daemon.Shell_inspection_selection.Current
        | Some revision_id, Some invocation -> Ok (Retained { revision_id; invocation })
        | Some _, None ->
          Or_error.error_string "retained inspection requires explicit -invocation"
        | None, Some _ ->
          Or_error.error_string "-invocation requires an explicit retained -revision"
      in
      Selection.Configured { config_path; prompt_name; selection }
  | _ ->
    Or_error.error_string
      "select raw CHATMD OR -config FILE -prompt NAME; retained options require \
       configured inspection"
;;

let validate_signing ~signature_payload ~issuer ~audience ~issued_at ~expires_at =
  if signature_payload
  then (
    match issuer with
    | Some issuer when not (String.is_empty issuer) -> Ok ()
    | None | Some _ ->
      Or_error.error_string "-signature-payload requires nonempty -issuer")
  else if
    Option.is_some issuer
    || (not (List.is_empty audience))
    || Option.is_some issued_at
    || Option.is_some expires_at
  then Or_error.error_string "signing options require -signature-payload"
  else Ok ()
;;

let signature_summary status =
  match status.Shell_runtime.Manifest_security.signature_key_id with
  | None -> "unsigned; accepted by administrative policy"
  | Some key_id ->
    sprintf
      "verified; key=%s%s"
      key_id
      (Option.value_map status.signature_issuer ~default:"" ~f:(fun issuer ->
         "; issuer=" ^ issuer))
;;

let print_requested_shell
      env
      (inspection : Chat_response.Agent_runtime.prepared_shell_inspection)
      ~canonical
      ~signature_payload
      ~issuer
      ~audience
      ~issued_at
      ~expires_at
      ~legacy
  =
  let module Manifest = Chatmd_shell_spec.Manifest in
  let manifest = inspection.manifest in
  if legacy
  then (
    print env "requested manifest: %s\n" manifest.sha256;
    print env "live manifest:      %s\n" manifest.sha256)
  else (
    print env "execution manifest: %s\n" manifest.sha256;
    print env "resource manifest:  %s\n" (Manifest.resource_sha256 manifest);
    print env "encoding:           %s\n" (Manifest.encoding_version manifest));
  print env "administrative:     %s\n" inspection.administrative_policy.source;
  (match inspection.security_check with
   | Ok status ->
     print env "signature:          %s\n" (signature_summary status);
     print env "trusted sources:    %d\n" (List.length status.trusted_sources)
   | Error errors ->
     print env "security:           rejected (requested identity remains inspectable)\n";
     List.iter errors ~f:(fun error ->
       print env "  [%s] %s\n" error.Shell_runtime.Manifest_security.code error.message));
  print env "live runtimes:      %d\n" (List.length inspection.live_runtimes);
  List.iter inspection.live_runtimes ~f:(fun runtime ->
    print
      env
      "  %s  profile=%s\n"
      (Chatmd_shell_spec.Shell_spec.Runtime_id.to_string runtime.id)
      (Option.value runtime.resolved_profile ~default:"custom"));
  if canonical then print env "\n%s\n" manifest.canonical_json;
  if signature_payload
  then (
    let issuer = Option.value_exn issuer in
    let audience =
      if List.is_empty audience
      then [ Option.value (Sys.getenv "OCHAT_SHELL_SIGNATURE_AUDIENCE") ~default:"ochat" ]
      else audience
    in
    let issued_at_unix =
      Option.value
        issued_at
        ~default:(Int64.of_float (Eio.Time.now (Eio.Stdenv.clock env)))
    in
    let payload =
      Shell_runtime.Manifest_signature.payload
        ~manifest
        ~issuer
        ~audience
        ~issued_at_unix
        ~expires_at_unix:expires_at
    in
    print
      env
      "\nsignature payload:\n%s\n"
      (Shell_runtime.Manifest_signature.canonical_payload payload))
;;

let inspect_configured_shell env ~config_path ~prompt_name ~selection =
  let module Daemon = Agent_server.Daemon in
  let protocol_error error =
    Sexp.to_string_hum ([%sexp_of: Agent_protocol.Error.t] error)
  in
  let open Result.Let_syntax in
  let%bind config =
    Agent_server.Config_parser.load ~env ~path:config_path
    |> Result.bind ~f:(Agent_server.Config_validator.validate ~env)
    |> Result.map_error
         ~f:
           (List.map ~f:(fun diagnostic ->
              Sexp.to_string_hum ([%sexp_of: Agent_server.Config.Diagnostic.t] diagnostic)))
  in
  Eio.Switch.run (fun sw ->
    Mirage_crypto_rng_unix.use_default ();
    let%bind daemon =
      Daemon.start
        ~sw
        ~env
        ~config
        ~tool_dir:(Eio.Path.native_exn (Eio.Stdenv.cwd env))
        ~process_start_identity:None
        ~options:{ Daemon.default_options with startup_mode = Operator_only }
        ()
      |> Result.map_error ~f:(fun error -> [ protocol_error error ])
    in
    Eio.Switch.on_release sw (fun () ->
      match Daemon.shutdown daemon with
      | Ok () -> ()
      | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]);
    let%bind entry =
      Agent_session.Prompt_catalog.find_by_name (Daemon.prompts daemon) prompt_name
      |> Result.of_option ~error:[ "configured prompt name is not admitted" ]
    in
    Daemon.inspect_prompt_shell
      daemon
      ~definition_id:entry.definition.id
      ~selection
      ~checkpoint:Eio.Fiber.check
    |> Result.map_error ~f:(fun error -> [ protocol_error error ]))
;;

let command =
  Command.basic
    ~summary:
      "Inspect legacy text or a configured captured shell revision without activation."
    (let%map_open path = anon (maybe ("CHATMD" %: string))
     and canonical =
       flag "-canonical" no_arg ~doc:" Print canonical requested manifest JSON"
     and config_path =
       flag
         "-config"
         (optional string)
         ~doc:"FILE use the explicit Operator_only daemon configuration/captured catalog"
     and prompt_name =
       flag "-prompt" (optional string) ~doc:"NAME select a configured prompt"
     and revision_id =
       flag
         "-revision"
         (optional revision_arg_type)
         ~doc:"ID inspect exact retained revision without current-source fallback"
     and invocation =
       flag
         "-invocation"
         (optional invocation_arg_type)
         ~doc:"ROLE retained contract: ui, headless, delegated-ui"
     and signature_payload =
       flag
         "-signature-payload"
         no_arg
         ~doc:
           " Print canonical Ed25519 payload for external signing (even if security \
            rejects)"
     and issuer =
       flag "-issuer" (optional string) ~doc:"NAME issuer for signature payload"
     and audience =
       flag
         "-audience"
         (listed string)
         ~doc:"NAME signature audience; defaults to existing host audience"
     and issued_at =
       flag
         "-issued-at"
         (optional (Command.Arg_type.create Int64.of_string))
         ~doc:"UNIX_SECONDS payload issue time; defaults to current clock"
     and expires_at =
       flag
         "-expires-at"
         (optional (Command.Arg_type.create Int64.of_string))
         ~doc:"UNIX_SECONDS optional payload expiry"
     in
     fun () ->
       let request =
         let open Or_error.Let_syntax in
         let%bind () =
           validate_signing ~signature_payload ~issuer ~audience ~issued_at ~expires_at
         in
         select ~path ~config_path ~prompt_name ~revision_id ~invocation
       in
       let success =
         Io.run_main (fun env ->
           let requested =
             let open Result.Let_syntax in
             let%bind selection =
               request |> Result.map_error ~f:(fun error -> [ Error.to_string_hum error ])
             in
             match selection with
             | Configured { config_path; prompt_name; selection } ->
               print
                 env
                 "Configured operator startup performs normal catalog/source/artifact \
                  admission; inspection does not activate runtimes.\n";
               inspect_configured_shell env ~config_path ~prompt_name ~selection
               |> Result.map ~f:(fun inspection -> inspection, false)
             | Legacy path ->
               let fs = Eio.Stdenv.fs env in
               let prompt = Eio.Path.(fs / path) |> Eio.Path.load in
               let dir = Eio.Path.(fs / Filename.dirname path) in
               let elements =
                 Prompt.Chat_markdown.parse_chat_inputs ~source:path ~dir prompt
               in
               Chat_response.Agent_runtime.inspect_shell
                 ~env
                 ~platform:(Chat_response.Agent_runtime.platform ())
                 ~prompt_elements:elements
               |> Result.map ~f:(fun inspection ->
                 ( ({ manifest = inspection.manifest
                    ; live_runtimes = inspection.live_runtimes
                    ; administrative_policy = inspection.administrative_policy
                    ; security_check = Ok inspection.security_status
                    }
                    : Chat_response.Agent_runtime.prepared_shell_inspection)
                 , true ))
               |> Result.map_error
                    ~f:(List.map ~f:Chat_response.Agent_runtime.diagnostic_to_string)
           in
           match requested with
           | Ok (inspection, legacy) ->
             print_requested_shell
               env
               inspection
               ~canonical
               ~signature_payload
               ~issuer
               ~audience
               ~issued_at
               ~expires_at
               ~legacy;
             true
           | Error diagnostics ->
             List.iter diagnostics ~f:(fun message -> print_error env "%s\n" message);
             false)
       in
       if not success then Core.exit 1)
;;

let%test_unit "malformed inspection selections fail before constructing a host owner" =
  let revision_id =
    Agent_protocol.Id.Prompt_revision.of_string "prv_option_validation"
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum ([%sexp_of: Agent_protocol.Error.t] error))
    |> Result.ok_or_failwith
  in
  let configured revision_id invocation =
    select
      ~path:None
      ~config_path:(Some "not-opened.sexp")
      ~prompt_name:(Some "prompt")
      ~revision_id
      ~invocation
  in
  assert (Result.is_ok (configured None None));
  assert (Result.is_ok (configured (Some revision_id) (Some Headless)));
  assert (Result.is_error (configured (Some revision_id) None));
  assert (Result.is_error (configured None (Some Headless)));
  assert (
    Result.is_error
      (select
         ~path:(Some "not-opened.chatmd")
         ~config_path:(Some "not-opened.sexp")
         ~prompt_name:(Some "prompt")
         ~revision_id:None
         ~invocation:None));
  assert (
    Result.is_error
      (select
         ~path:(Some "not-opened.chatmd")
         ~config_path:None
         ~prompt_name:None
         ~revision_id:(Some revision_id)
         ~invocation:(Some Headless)));
  let sign signature_payload issuer =
    validate_signing
      ~signature_payload
      ~issuer
      ~audience:[]
      ~issued_at:None
      ~expires_at:None
  in
  assert (Result.is_ok (sign false None));
  assert (Result.is_ok (sign true (Some "operator")));
  assert (Result.is_error (sign true None));
  assert (Result.is_error (sign true (Some "")));
  assert (Result.is_error (sign false (Some "operator")))
;;
