open! Core
module P = Agent_protocol
module C = Agent_client
module E = Agent_server.Embedded
module M = Managed_completion_cli

let invalid message = P.Error.invalid_request message
let failure code message = P.Error.create code ~message ~retryable:false ()
let key () = P.Idempotency_key.of_string P.Id.Transaction.(create () |> to_string)

module Options = struct
  type t =
    { definition_file : string
    ; output_file : string
    ; storage : Agent_server.Local_storage.t
    ; session_id : P.Id.Session.t option
    ; run_id : P.Id.Run.t option
    ; rebuild : bool
    ; import_transcript : string option
    ; submission : string option
    ; mode : P.Run.Mode.t
    }

  let create
        ~definition_file
        ~output_file
        ~storage
        ~session_id
        ~run_id
        ~rebuild
        ~import_transcript
        ~submission
        ~mode
    =
    if String.is_empty definition_file || String.is_empty output_file
    then Error (invalid "definition and output paths must be nonempty")
    else if String.equal definition_file output_file
    then Error (invalid "transcript export must not replace its authored definition")
    else if Option.is_some run_id && Option.is_none session_id
    then Error (invalid "-run-id requires -session-id")
    else if rebuild && (Option.is_none session_id || Option.is_some run_id)
    then Error (invalid "-rebuild requires -session-id and excludes -run-id")
    else if
      Option.is_some submission
      && (Option.is_none session_id
          || Option.is_some run_id
          || Option.is_some import_transcript)
    then Error (invalid "-message requires -session-id and excludes -run-id/import")
    else if Option.exists submission ~f:(fun text -> String.is_empty (String.strip text))
    then Error (invalid "-message must be nonempty")
    else if Option.is_some import_transcript && Option.is_some session_id
    then Error (invalid "transcript import creates a new conversation; it cannot resume")
    else if
      Option.exists import_transcript ~f:(fun path ->
        String.is_empty path
        || String.equal path output_file
        || String.equal path definition_file)
    then Error (invalid "transcript import, definition and export must be separate files")
    else
      Ok
        { definition_file
        ; output_file
        ; storage
        ; session_id
        ; run_id
        ; rebuild
        ; import_transcript
        ; submission
        ; mode
        }
  ;;
end

let path env name =
  let root =
    if Filename.is_absolute name then Eio.Stdenv.fs env else Eio.Stdenv.cwd env
  in
  Eio.Path.(root / name)
;;

let snapshot connection id =
  C.Admin.get_session connection id |> Result.map ~f:P.Public.Snapshot.fields
;;

let lifecycle fields =
  Result.of_option
    fields.P.Public.Snapshot.Fields.lifecycle
    ~error:(invalid "selected session lacks authoritative lifecycle evidence")
  |> Result.map ~f:P.Session_lifecycle.Observation.expected
;;

let recover_created connection ~method_name ~idempotency_key error =
  match C.Connection.pending_commands connection with
  | [] -> Error error
  | [ pending ] ->
    (match C.Connection.reconcile connection pending with
     | Ok (Committed (Created_session id)) -> Ok id
     | Ok (Failed error) -> Error error
     | Ok (Missing | Unavailable | Pending _ | Committed _) | Error _ ->
       Error
         (failure
            Interrupted
            (sprintf
               "Unknown %s outcome for original key %s; do not repeat admission with a \
                new key."
               method_name
               (P.Idempotency_key.to_string idempotency_key))))
  | _ ->
    Error (failure Interrupted "unexpected unresolved commands during session admission")
;;

let create connection spec =
  let open Result.Let_syntax in
  let%bind idempotency_key = key () in
  match
    C.Connection.request
      connection
      (Session_create { spec; requested_mode = None; subscribe = false; idempotency_key })
  with
  | Ok (Session_create result) -> Ok result.session.id
  | Ok _ -> Error (invalid "unexpected session.create response")
  | Error error ->
    recover_created connection ~method_name:"session.create" ~idempotency_key error
;;

let seed connection spec transcript =
  let open Result.Let_syntax in
  let%bind definition_id =
    match spec.P.Session.Spec.prompt with
    | Catalog id -> Ok id
    | Generated _ | Local_path _ ->
      Error (invalid "transcript seed requires authored code")
  in
  let%bind definition = C.Catalog.get_prompt connection definition_id in
  let%bind expected_revision =
    Result.of_option
      definition.current_revision
      ~error:(invalid "authored code is unavailable")
  in
  let%bind idempotency_key = key () in
  let%bind request =
    P.Session_seed.Request.create
      spec
      ~code:(Catalog { definition_id; expected_revision })
      ~transcript
      ~idempotency_key
  in
  match C.Session_handle.seed connection request with
  | Ok result -> Ok result.session_id
  | Error error ->
    recover_created connection ~method_name:"session.seed" ~idempotency_key error
;;

let retained_run connection session_id run_id mode =
  let open Result.Let_syntax in
  let%bind initialized =
    Result.of_option
      (C.Connection.initialization connection)
      ~error:(invalid "connection is not initialized")
  in
  let views = C.Run_views.create connection ~server_id:initialized.server_id in
  let%bind outcome =
    C.Run_views.lookup
      views
      { session = P.Session_ref.create ~server_id:initialized.server_id ~session_id
      ; run_id
      }
  in
  match outcome with
  | Unavailable _ -> Error (failure Invalid_state "original run evidence is unavailable")
  | Available view ->
    if not (P.Run.Mode.equal view.run.mode mode)
    then Error (failure Conflict "requested mode differs from original run mode")
    else
      Result.of_option
        view.admission_receipt
        ~error:(failure Invalid_state "original run admission receipt is unavailable")
      |> Result.map ~f:(fun receipt -> M.Request.Observe_run receipt)
;;

let source_request selection mode submission =
  let module Role = Chat_response.Script_contract_plan.Program.Role in
  match selection.E.Script_selection.moderator with
  | Some (Role.Standard (Moderator_v1 | Delegated_moderator_v1)) ->
    (match submission with
     | None -> Ok (M.Request.Authored_start mode)
     | Some text ->
       Ok
         (M.Request.User_submission
            { mode
            ; content = { kind = Plain_text; text; attachments = [] }
            ; activation = Start_stopped
            }))
  | None | Some (Legacy_moderator _) ->
    (match submission, mode with
     | Some _, _ ->
       Error
         (failure
            Invalid_state
            "Explicit -message requires a versioned moderator; ordinary input uses the \
             authored/imported transcript")
     | None, Single_turn -> Ok (M.Request.Foreground_continue Start_stopped)
     | None, Workflow ->
       Error (failure Invalid_state "Workflow requires an actual versioned moderator"))
  | Some (Standard (Tool_v1 | One_off_v1) | Native_hook _) ->
    Error (failure Invalid_state "selected root has a child-only moderator role")
;;

let complete host connection options ~env ~session =
  let open Result.Let_syntax in
  let%bind rendering_limits =
    History_chatmd.Rendering.Limits.create
      ~max_bytes:(64 * 1024 * 1024)
      ~max_work_steps:(256 * 1024 * 1024)
      ~json_limits:Document_schema.Limits.default
    |> Result.map_error ~f:(fun error ->
      invalid (Sexp.to_string_hum (History_chatmd.Rendering.Error.sexp_of_t error)))
  in
  let%bind presentation_limits =
    History_chatmd.Rendering.Limits.create
      ~max_bytes:(1024 * 1024)
      ~max_work_steps:(8 * 1024 * 1024)
      ~json_limits:Document_schema.Limits.default
    |> Result.map_error ~f:(fun error ->
      invalid (Sexp.to_string_hum (History_chatmd.Rendering.Error.sexp_of_t error)))
  in
  let presentation = Managed_completion_presentation.create presentation_limits in
  let omitted = ref false in
  let present projection =
    match
      Managed_completion_presentation.observe
        presentation
        projection
        ~checkpoint:Eio.Fiber.check
    with
    | Ok None -> ()
    | Ok (Some frame) ->
      omitted := false;
      Eio.Flow.copy_string frame (Eio.Stdenv.stderr env)
    | Error _ ->
      if not !omitted
      then (
        omitted := true;
        Eio.Flow.copy_string
          "[live observation omitted: presentation limit]\n"
          (Eio.Stdenv.stderr env))
  in
  let prepare handle initial =
    let open Result.Let_syntax in
    let protocol result =
      Result.map_error result ~f:(fun error -> M.Error.Protocol error)
    in
    let%bind selected =
      if options.Options.rebuild
      then
        C.Session_handle.rebuild
          handle
          ~expected_revision:initial.P.Session.revision
          ~prompt_choice:Current_catalog
        |> protocol
      else Ok initial
    in
    let%bind request =
      match options.run_id with
      | Some run_id -> retained_run connection selected.id run_id options.mode |> protocol
      | None ->
        let%bind fields = snapshot connection selected.id |> protocol in
        let%bind expected = lifecycle fields |> protocol in
        let%bind selection =
          E.session_script_selection host ~expected ~checkpoint:Eio.Fiber.check
          |> protocol
        in
        source_request selection options.mode options.submission |> protocol
    in
    let%bind request, selected =
      match request with
      | M.Request.User_submission submission ->
        (match selected.desired_state, selected.observed_state with
         | Stopped, Stopped ->
           let current =
             C.Session_handle.projection handle
             |> C.Projection.snapshot
             |> P.Public.Snapshot.fields
           in
           let%bind () =
             if
               P.Id.Session.equal current.session.id selected.id
               && Int.equal current.session.generation selected.generation
               && Int64.equal current.session.revision selected.revision
             then Ok ()
             else
               Error (M.Error.Protocol (failure Conflict "activation selection changed"))
           in
           let%bind started =
             C.Session_handle.start handle ~queue_if_limited:false |> protocol
           in
           let%map activated =
             E.complete_script_activation
               host
               ~started
               ~attachment_id:(C.Session_handle.attachment handle).id
               ~checkpoint:Eio.Fiber.check
             |> protocol
           in
           ( M.Request.User_submission { submission with activation = Require_running }
           , activated )
         | Running, Idle ->
           Ok
             ( M.Request.User_submission { submission with activation = Require_running }
             , selected )
         | _ ->
           Error
             (M.Error.Protocol
                (failure Conflict "selected session is not ready for explicit input")))
      | Foreground_continue _ | Authored_start _ | Observe_run _ -> Ok (request, selected)
    in
    Ok (request, selected)
  in
  M.run
    connection
    ~clock:(Eio.Stdenv.clock env)
    ~session_id:session.P.Session.id
    ~expected_generation:session.generation
    ~expected_revision:session.revision
    ~request:(Foreground_continue Require_running)
    ~prepare:(Some prepare)
    ~document_limits:Document_schema.Limits.default
    ~rendering_limits
    ~checkpoint:Eio.Fiber.check
    ~on_view:present
    ~on_admission:(fun admission ->
      Eio.Flow.copy_string
        (sprintf "[managed run %s]\n" (P.Id.Run.to_string admission.run_id))
        (Eio.Stdenv.stderr env))
  |> Result.map_error ~f:(function
    | Protocol error -> error
    | Continuation_unavailable reason ->
      failure
        Invalid_state
        (Sexp.to_string_hum (P.History_edit.Continuation.sexp_of_unavailable reason))
    | Rendering error ->
      invalid (Sexp.to_string_hum (History_chatmd.Rendering.Error.sexp_of_t error)))
;;

let write env file bytes =
  if String.equal file "/dev/stdout"
  then (
    Eio.Flow.copy_string bytes (Eio.Stdenv.stdout env);
    Ok ())
  else
    C.Blob_download.install_atomic ~path:(path env file) ~download:(fun output ->
      Eio.Flow.copy_string bytes output;
      Ok ())
    |> Result.map_error ~f:(fun error ->
      failure Persistence_error (Error.to_string_hum error))
;;

let successful completed =
  match completed.M.Completed.evidence with
  | Foreground { operation; _ } ->
    (match operation.P.Operation.state with
     | Completed -> Ok ()
     | Failed error -> Error error
     | Cancelled -> Error (failure Interrupted "foreground operation was cancelled")
     | Interrupted { reason; retryable } ->
       Error (P.Error.create Interrupted ~message:reason ~retryable ())
     | Starting | Running | Cancelling ->
       Error (failure Invalid_state "foreground operation has no successful terminal"))
  | Managed_run { terminal; _ } ->
    (match terminal.run.lifecycle with
     | Terminal (Completed _) -> Ok ()
     | Terminal (Failed code) ->
       Error (failure (Option.value code ~default:Internal_error) "managed run failed")
     | Terminal Cancelled -> Error (failure Interrupted "managed run was cancelled")
     | Terminal Limited -> Error (failure Resource_limit "managed run reached its limits")
     | Terminal Interrupted -> Error (failure Interrupted "managed run was interrupted")
     | Admitted | Active | Waiting _ ->
       Error (failure Invalid_state "managed run has no successful terminal"))
;;

let with_pending_diagnostics connection ~env ~session_id ~storage f =
  let previous = C.Connection.pending_commands connection in
  let storage =
    match Agent_server.Local_storage.durable_root storage ~home:(Sys.getenv "HOME") with
    | Ok (Some root) -> Agent_server.Local_storage.Root.path root
    | Ok None -> "transient"
    | Error _ -> "default-home-unavailable"
  in
  let report () =
    C.Connection.pending_commands connection
    |> List.filter ~f:(fun pending ->
      not (List.exists previous ~f:(fun original -> phys_equal original pending)))
    |> List.iter ~f:(fun pending ->
      let identity = C.Connection.pending_identity pending in
      let original_key =
        Option.value_map
          identity.idempotency_key
          ~default:"unavailable"
          ~f:P.Idempotency_key.to_string
      in
      Eio.Flow.copy_string
        (sprintf
           "[original command %s key %S; session %s; storage %S]\n"
           identity.method_name
           original_key
           (P.Id.Session.to_string session_id)
           storage)
        (Eio.Stdenv.stderr env);
      match C.Connection.reconcile connection pending with
      | Ok (Committed (Accepted_run { session_id = admitted_session; receipt }))
        when String.equal identity.method_name "session.run.start"
             && P.Id.Session.equal session_id admitted_session
             && P.Run_receipt.Kind.equal receipt.kind Admission
             && Option.value_map
                  identity.idempotency_key
                  ~default:false
                  ~f:(P.Idempotency_key.equal receipt.key)
             && Option.value_map
                  (C.Connection.initialization connection)
                  ~default:false
                  ~f:(fun initialized ->
                    P.Id.Principal.equal initialized.principal.id receipt.principal_id) ->
        Eio.Flow.copy_string
          (sprintf
             "[original managed run %s; use -session-id %s -run-id %s with its original \
              mode]\n"
             (P.Id.Run.to_string receipt.run_id)
             (P.Id.Session.to_string session_id)
             (P.Id.Run.to_string receipt.run_id))
          (Eio.Stdenv.stderr env)
      | Ok (Failed _) ->
        Eio.Flow.copy_string
          "[original command has a failed receipt]\n"
          (Eio.Stdenv.stderr env)
      | Ok (Committed (Accepted_run _)) ->
        Eio.Flow.copy_string
          "[accepted-run receipt conflicts with original recovery coordinates; no usable \
           run ID]\n"
          (Eio.Stdenv.stderr env)
      | Ok (Committed _) ->
        Eio.Flow.copy_string
          "[original command has committed receipt evidence]\n"
          (Eio.Stdenv.stderr env)
      | Ok (Missing | Unavailable | Pending _) | Error _ ->
        Eio.Flow.copy_string
          "[original command outcome unresolved; do not submit it with a new key]\n"
          (Eio.Stdenv.stderr env))
  in
  let result =
    try Ok (f ()) with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  (match result with
   | Ok (Ok _) -> ()
   | Ok (Error _) | Error _ ->
     Eio.Cancel.protect (fun () ->
       Eio.Time.with_timeout (Eio.Stdenv.clock env) 1. (fun () ->
         (try report () with
          | secondary ->
            Eio.traceln "Original-command reporting failed: %s" (Exn.to_string secondary));
         Ok ())
       |> ignore));
  match result with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let run options ~env ~daemon_options ~project_sources ~transcript_context =
  let open Result.Let_syntax in
  if
    Frontend_transcript.Destination.conflicts_with
      ~env
      ~source_file:options.Options.definition_file
      ~output_file:options.output_file
    || Option.exists options.import_transcript ~f:(fun file ->
      Frontend_transcript.Destination.conflicts_with
        ~env
        ~source_file:file
        ~output_file:options.output_file)
  then
    Error (invalid "transcript export must not replace definition or imported transcript")
  else (
    let%bind transcript =
      match options.import_transcript with
      | None -> Ok None
      | Some file ->
        Frontend_transcript.load transcript_context ~file ~checkpoint:Eio.Fiber.check
        |> Result.map ~f:Option.some
    in
    let workspace =
      Eio_posix.Low_level.realpath (Eio.Path.native_exn (Eio.Stdenv.cwd env))
    in
    let definition_file =
      if Filename.is_absolute options.definition_file
      then options.definition_file
      else Filename.concat workspace options.definition_file
    in
    let embedded_options : E.options =
      { prompt_file = definition_file
      ; workspace
      ; tool_dir = workspace
      ; home = Sys.getenv "HOME"
      ; storage = options.storage
      ; start_immediately = false
      ; permission_profile = E.default_permission_profile
      ; attachment_mode = Read_write
      ; event_capacity = 4096
      }
    in
    Eio.Switch.run (fun sw ->
      E.with_local_host
        ~sw
        ~env
        ~daemon_options
        ~root_invocation:Headless
        ?project_sources
        embedded_options
        ~f:(fun host ->
          let connection = E.host_connection host in
          let%bind session_id =
            match options.session_id with
            | Some id ->
              let%bind fields = snapshot connection id in
              let%bind expected = lifecycle fields in
              let%map () = E.select_session host expected in
              id
            | None ->
              let%bind spec = E.session_spec embedded_options in
              (match transcript with
               | None -> create connection spec
               | Some transcript -> seed connection spec transcript)
          in
          Eio.Flow.copy_string
            (sprintf "[managed conversation %s]\n" (P.Id.Session.to_string session_id))
            (Eio.Stdenv.stderr env);
          let%bind fields = snapshot connection session_id in
          let%bind completed =
            with_pending_diagnostics
              connection
              ~env
              ~session_id
              ~storage:options.storage
              (fun () -> complete host connection options ~env ~session:fields.session)
          in
          let%bind () = write env options.output_file completed.chatmd in
          let%map () = successful completed in
          completed)))
;;

let mode_arg =
  Command.Arg_type.create (function
    | "single-turn" -> P.Run.Mode.Single_turn
    | "workflow" -> Workflow
    | _ -> failwith "-run-mode must be single-turn or workflow")
;;

let id_arg decode =
  Command.Arg_type.create (fun value ->
    decode value
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (P.Error.sexp_of_t error))
    |> Result.ok_or_failwith)
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Run a managed conversation from a separate ChatMD definition."
    (let%map_open definition_file =
       flag
         "-prompt-file"
         (required string)
         ~doc:
           "FILE Authored ChatMD definition (retained resume does not recapture its \
            code)."
     and output_file =
       flag
         "-output-file"
         (optional_with_default "/dev/stdout" string)
         ~doc:"FILE Complete canonical transcript export; default stdout."
     and session_id =
       flag
         "-session-id"
         (optional (id_arg P.Id.Session.of_string))
         ~doc:
           "ID Retained conversation; use -message for new versioned input or -run-id \
            for original evidence."
     and run_id =
       flag
         "-run-id"
         (optional (id_arg P.Id.Run.of_string))
         ~doc:"ID Observe original managed run; requires -session-id."
     and rebuild =
       flag
         "-rebuild"
         no_arg
         ~doc:" Adopt current catalog code in stopped retained conversation."
     and import_transcript =
       flag
         "-import-transcript"
         (optional string)
         ~doc:"FILE Edited transcript to admit as a new conversation."
     and submission =
       flag
         "-message"
         (optional string)
         ~doc:
           "TEXT Submit genuine new input to a retained versioned moderator; requires \
            -session-id."
     and project_roots =
       flag
         "-project-root"
         (listed string)
         ~doc:
           "DIR Explicit external project-source root grant (repeatable); absent means \
            local-only."
     and storage_root =
       flag
         "-storage-root"
         (optional string)
         ~doc:"DIR Explicit native local store; default HOME/.ochat/agent-store."
     and mode =
       flag
         "-run-mode"
         (optional_with_default P.Run.Mode.Single_turn mode_arg)
         ~doc:"MODE single-turn or workflow; never implicitly coerced."
     and transport =
       flag
         "inference-transport"
         (optional_with_default "sse" string)
         ~doc:"POLICY sse, prefer-websocket or require-websocket."
     in
     fun () ->
       let transport_policy =
         Provider_runtime_host.Profile_policy.Transport_policy.of_string transport
         |> Or_error.ok_exn
       in
       let storage =
         match storage_root with
         | None -> Agent_server.Local_storage.Default
         | Some path ->
           Agent_server.Local_storage.Root.create ~path ()
           |> Result.map_error ~f:(fun error ->
             Sexp.to_string_hum (P.Error.sexp_of_t error))
           |> Result.ok_or_failwith
           |> fun root -> Agent_server.Local_storage.Durable root
       in
       let options =
         Options.create
           ~definition_file
           ~output_file
           ~storage
           ~session_id
           ~run_id
           ~rebuild
           ~import_transcript
           ~submission
           ~mode
         |> Result.map_error ~f:(fun error ->
           Sexp.to_string_hum (P.Error.sexp_of_t error))
         |> Result.ok_or_failwith
       in
       let raw_project_sources : Agent_server.Project_source_configuration.t =
         { external_roots = project_roots; git = None }
       in
       Agent_server.Project_source_configuration.validate_raw raw_project_sources
       |> Or_error.ok_exn;
       Io.run_main (fun env ->
         let project_sources =
           if List.is_empty project_roots
           then None
           else (
             let cwd = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
             Some
               { raw_project_sources with
                 external_roots =
                   List.map project_roots ~f:(fun path ->
                     if Filename.is_absolute path then path else Filename.concat cwd path)
               })
         in
         Eio.Switch.run (fun sw ->
           let inference_host =
             Inference_composition.create_with_policy
               ~transport_policy
               ~sw
               ~env
               ~default_model:"gpt-4.5-preview"
           in
           let ctx =
             Chat_response.Ctx.create
               ~env
               ~dir:(Eio.Stdenv.cwd env)
               ~tool_dir:(Eio.Stdenv.cwd env)
               ~cache:(Chat_response.Cache.create ~max_size:1 ())
               ~inference_context:
                 (Inference_composition.context
                    inference_host
                    Chat_response.Config.default)
               ~inference_identity:(Inference_host.identity inference_host)
               ~on_inference_attempt:ignore
               ~on_inference_completion:ignore
               ()
           in
           run
             options
             ~env
             ~daemon_options:(Inference_composition.daemon_options inference_host)
             ~project_sources
             ~transcript_context:ctx
           |> Result.map_error ~f:(fun error ->
             Sexp.to_string_hum (P.Error.sexp_of_t error))
           |> Result.ok_or_failwith
           |> ignore)))
;;
