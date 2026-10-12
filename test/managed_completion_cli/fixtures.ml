open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module E = Agent_server.Embedded
module M = Managed_completion_cli

let debug_lines = ref []

let with_host ~remove_source ~prompt ~requests ~f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    debug_lines := [];
    Chatml.Chatml_debug_log.set_sink (fun line ->
      if List.length !debug_lines < 300 then debug_lines := line :: !debug_lines);
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Chatml.Chatml_debug_log.clear_sink ();
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt_file = Filename.concat root "definition.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          prompt;
        let options : E.options =
          { prompt_file
          ; workspace
          ; tool_dir = root
          ; home = Some root
          ; storage = Transient
          ; start_immediately = false
          ; permission_profile = E.default_permission_profile
          ; attachment_mode = Read_write
          ; event_capacity = 256
          }
        in
        let daemon_options =
          { Agent_server.Daemon.default_options with
            inference_policy =
              inference_policy
                ~default_model:"fixture-model"
                ~post_stream:(fun ~sw:_ ~inputs:_ ->
                  Int.incr requests;
                  Stdlib.Seq.empty)
          }
        in
        Eio.Switch.run (fun sw ->
          E.with_local_host
            ~sw
            ~env
            ~daemon_options
            ~root_invocation:Headless
            options
            ~f:(fun host ->
              if remove_source
              then Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / prompt_file);
              f ~env ~host (E.host_connection host);
              Ok ())
          |> protocol_ok)))
;;

let create connection ~key =
  let spec =
    P.Session.Spec.create
      ~execution_host:Embedded
      ~prompt:
        (Catalog (Agent_server.Catalog_identity.prompt_definition "embedded.prompt"))
      ~workspace:
        (Configured
           (Agent_server.Catalog_identity.workspace_definition "embedded.workspace"))
      ~liveness:Process_bound
      ~persistence:Transient
      ~start_immediately:false
      ~labels:[]
      ()
    |> protocol_ok
  in
  let idempotency_key = P.Idempotency_key.of_string key |> protocol_ok in
  match
    C.Connection.request
      connection
      (Session_create { spec; requested_mode = None; subscribe = false; idempotency_key })
    |> protocol_ok
  with
  | P.Public.Result.Session_create result ->
    assert (Option.is_none result.attachment);
    result.session
  | _ -> failwith "actual session.create returned another result"
;;

let snapshot connection id =
  C.Admin.get_session connection id |> protocol_ok |> P.Public.Snapshot.fields
;;

let run
      connection
      ~env
      ~session
      ~request
      ?prepare
      ?(checkpoint = ignore)
      ?(on_view = ignore)
      ?(on_admission = ignore)
      ()
  =
  let json = Document_schema.Limits.default in
  let rendering_limits =
    History_chatmd.Rendering.Limits.create
      ~max_bytes:(8 * 1024 * 1024)
      ~max_work_steps:10_000_000
      ~json_limits:json
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (History_chatmd.Rendering.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  try
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      M.run
        connection
        ~clock:(Eio.Stdenv.clock env)
        ~session_id:session.P.Session.id
        ~expected_generation:session.generation
        ~expected_revision:session.revision
        ~request
        ~prepare
        ~document_limits:json
        ~rendering_limits
        ~checkpoint
        ~on_view
        ~on_admission)
  with
  | Eio.Time.Timeout ->
    let state = snapshot connection session.id in
    let initialized = C.Connection.initialization connection |> Option.value_exn in
    let query =
      P.Run_query.Request.create
        ~session:
          (P.Session_ref.create ~server_id:initialized.server_id ~session_id:session.id)
        ~page:(P.Page.Request.create ~limit:10 () |> protocol_ok)
      |> protocol_ok
    in
    let runs =
      C.Run_views.create connection ~server_id:initialized.server_id
      |> fun views -> C.Run_views.page views query |> protocol_ok
    in
    raise_s
      [%sexp
        "managed presentation stalled"
      , (!debug_lines : string list)
      , (state : P.Public.Snapshot.Fields.t)
      , (runs : P.Run_query.View.t P.Page.t)]
;;

let checked = function
  | Ok value -> value
  | Error (M.Error.Protocol error) ->
    raise_s [%sexp (error : P.Error.t), (!debug_lines : string list)]
  | Error (Continuation_unavailable reason) ->
    raise_s [%sexp (reason : P.History_edit.Continuation.unavailable)]
  | Error (Rendering error) -> raise_s [%sexp (error : History_chatmd.Rendering.Error.t)]
;;
