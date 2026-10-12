open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module E = Agent_server.Embedded

let%expect_test "actual failing moderator produces one correlated run terminal receipt" =
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
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          {|<developer>Failed managed callback.</developer>
<script id="failure" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event ctx state event = Task.fail("actual callback failure")
</script>|};
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
          let embedded =
            E.start ~sw ~env ~daemon_options ~root_invocation:Headless options
            |> protocol_ok
          in
          E.with_session embedded ~f:(fun embedded ->
            (* The embedded compatibility owner already has its own subscribed
               bootstrap attachment. This reader borrows a distinct host-owned
               connection so it receives only its actual attachment's stream. *)
            let connection = E.connect_owned embedded |> protocol_ok in
            C.Session_handle.initialize
              connection
              ~implementation_name:"run-callback-failure-fixture"
              ~implementation_version:"1"
            |> protocol_ok
            |> ignore;
            let changed = Eio.Condition.create () in
            let observer = ref None in
            let handle =
              C.Session_handle.attach
                ~sw
                ~clock:(Eio.Stdenv.clock env)
                ~connection
                ~session_id:(E.session_id embedded)
                ~mode:Read_write
                ~on_update:(fun projection ->
                  Option.iter !observer ~f:(fun observe -> observe projection))
                ()
              |> protocol_ok
            in
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
                ~mode:Workflow
                ~input:Authored_start
              |> protocol_ok
            in
            let initialized =
              C.Connection.initialization connection |> Option.value_exn
            in
            let views = C.Run_views.create connection ~server_id:initialized.server_id in
            let query : P.Run_query.Lookup_request.t =
              { session =
                  P.Session_ref.create
                    ~server_id:initialized.server_id
                    ~session_id:(E.session_id embedded)
              ; run_id = admission.run_id
              }
            in
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
            observer := Some observe;
            observe (C.Session_handle.projection handle);
            let terminal =
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                Eio.Fiber.first
                  (fun () ->
                     Eio.Condition.loop_no_mutex changed (fun () ->
                       Option.iter !failure ~f:(fun error ->
                         raise_s [%sexp (error : P.Error.t)]);
                       match !latest with
                       | Some (Available view) ->
                         (match view.run.lifecycle with
                          | Terminal _ -> Some view
                          | Admitted | Active | Waiting _ -> None)
                       | Some (Unavailable _) -> failwith "admitted run unavailable"
                       | None -> None))
                  (fun () ->
                     C.Session_handle.await_closed handle;
                     match C.Session_handle.last_error handle with
                     | Some error -> raise_s [%sexp (error : P.Error.t)]
                     | None -> failwith "run reader closed before terminal"))
            in
            (match terminal.run.lifecycle with
             | Terminal (Failed None) -> ()
             | _ -> failwith "failed callback did not settle run");
            let receipt = Option.value_exn terminal.terminal_receipt in
            assert (P.Id.Run.equal receipt.run_id admission.run_id);
            assert (P.Id.Principal.equal receipt.principal_id admission.principal_id);
            assert (P.Run_source.equal receipt.source admission.source);
            assert (
              Option.exists terminal.admission_receipt ~f:(P.Run_receipt.equal admission));
            let current =
              C.Admin.get_session connection (E.session_id embedded)
              |> protocol_ok
              |> P.Public.Snapshot.fields
            in
            assert (
              List.exists current.extension_status ~f:(fun status ->
                P.Extension_status.equal_kind status.kind Moderator_execution
                && String.equal status.state "failed"));
            let page =
              P.Run_query.Request.create
                ~session:query.session
                ~page:(P.Page.Request.create ~limit:10 () |> protocol_ok)
              |> protocol_ok
              |> C.Run_views.page views
              |> protocol_ok
            in
            [%test_eq: int] (List.length page.items) 1;
            [%test_eq: int] !requests 0;
            C.Session_handle.close handle;
            print_endline
              "actual callback failed; original admission retained; one run terminal \
               receipt; no provider";
            Ok ())
          |> protocol_ok)));
  [%expect
    {| actual callback failed; original admission retained; one run terminal receipt; no provider |}]
;;
