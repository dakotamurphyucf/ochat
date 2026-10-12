open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module E = Agent_server.Embedded

type t =
  { env : Eio_unix.Stdenv.base
  ; connection : C.Connection.t
  ; handle : C.Session_handle.t
  ; views : C.Run_views.t
  ; query : P.Run_query.Lookup_request.t
  ; admission : P.Run_receipt.t
  ; requests : int ref
  ; changed : Eio.Condition.t
  ; latest : P.Run_query.Outcome.t option ref
  ; failure : P.Error.t option ref
  }

let admission t = t.admission
let requests t = !(t.requests)

let snapshot t =
  C.Admin.get_session t.connection (C.Session_handle.session_id t.handle)
  |> protocol_ok
  |> P.Public.Snapshot.fields
;;

let query t =
  match C.Run_views.lookup t.views t.query |> protocol_ok with
  | Available view -> view
  | Unavailable _ -> failwith "admitted run unavailable"
;;

let await t ~f =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock t.env) 10. (fun () ->
    Eio.Fiber.first
      (fun () ->
         Eio.Condition.loop_no_mutex t.changed (fun () ->
           Option.iter !(t.failure) ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
           match !(t.latest) with
           | Some (Available view) -> if f view then Some view else None
           | Some (Unavailable _) -> failwith "admitted run unavailable"
           | None -> None))
      (fun () ->
         C.Session_handle.await_closed t.handle;
         match C.Session_handle.last_error t.handle with
         | Some error -> raise_s [%sexp (error : P.Error.t)]
         | None -> failwith "run reader closed before requested evidence"))
;;

let with_run ~mode ~startup ~f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt_file = Filename.concat root "definition.chatmd" in
        let prompt =
          {|<developer>Real authored startup.</developer>
<script id="startup" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event ctx state event = match event with
| `Session_start -> |}
          ^ startup
          ^ {|
| _ -> Task.pure(state)
</script>|}
        in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          prompt;
        let requests = ref 0 in
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
              let connection = E.host_connection host in
              let spec =
                P.Session.Spec.create
                  ~execution_host:Embedded
                  ~prompt:
                    (Catalog
                       (Agent_server.Catalog_identity.prompt_definition "embedded.prompt"))
                  ~workspace:
                    (Configured
                       (Agent_server.Catalog_identity.workspace_definition
                          "embedded.workspace"))
                  ~liveness:Process_bound
                  ~persistence:Transient
                  ~start_immediately:false
                  ~labels:[]
                  ()
                |> protocol_ok
              in
              let session =
                match
                  C.Connection.request
                    connection
                    (Session_create
                       { spec
                       ; requested_mode = None
                       ; subscribe = false
                       ; idempotency_key =
                           P.Idempotency_key.of_string "authored-start-fixture"
                           |> protocol_ok
                       })
                  |> protocol_ok
                with
                | P.Public.Result.Session_create result ->
                  assert (Option.is_none result.attachment);
                  result.session
                | _ -> failwith "actual session.create returned another result"
              in
              let observer = ref None in
              let handle =
                C.Session_handle.attach
                  ~sw
                  ~clock:(Eio.Stdenv.clock env)
                  ~connection
                  ~session_id:session.id
                  ~mode:Read_write
                  ~on_update:(fun projection ->
                    Option.iter !observer ~f:(fun observe -> observe projection))
                  ()
                |> protocol_ok
              in
              Exn.protect
                ~finally:(fun () -> C.Session_handle.close handle)
                ~f:(fun () ->
                  let current =
                    C.Session_handle.projection handle
                    |> C.Projection.snapshot
                    |> P.Public.Snapshot.fields
                  in
                  let admission =
                    C.Session_handle.start_run
                      handle
                      ~generation:current.session.generation
                      ~expected_revision:current.session.revision
                      ~mode
                      ~input:Authored_start
                    |> protocol_ok
                  in
                  let initialized =
                    C.Connection.initialization connection |> Option.value_exn
                  in
                  let views =
                    C.Run_views.create connection ~server_id:initialized.server_id
                  in
                  let query : P.Run_query.Lookup_request.t =
                    { session =
                        P.Session_ref.create
                          ~server_id:initialized.server_id
                          ~session_id:session.id
                    ; run_id = admission.run_id
                    }
                  in
                  let changed = Eio.Condition.create () in
                  let latest = ref None in
                  let failure = ref None in
                  let observe =
                    C.Run_views.watch
                      views
                      ~sw
                      ~request:query
                      ~on_result:(fun result ->
                        latest := Some result;
                        Eio.Condition.broadcast changed)
                      ~on_error:(fun error ->
                        failure := Some error;
                        Eio.Condition.broadcast changed)
                  in
                  observer
                  := Some
                       (fun projection ->
                         observe projection;
                         Eio.Condition.broadcast changed);
                  observe (C.Session_handle.projection handle);
                  f
                    { env
                    ; connection
                    ; handle
                    ; views
                    ; query
                    ; admission
                    ; requests
                    ; changed
                    ; latest
                    ; failure
                    });
              Ok ())
          |> protocol_ok)))
;;
