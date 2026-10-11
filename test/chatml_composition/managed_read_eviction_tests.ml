open Core
open Agent_server_test_support
module Team = Documentation_agent_team_tests
module P = Agent_protocol
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module Owner = Agent_server.Runtime_owner
module H = Agent_client.Session_handle

let%expect_test "managed child read overlaps a stopped-session eviction" =
  Team.with_team_runtime (fun ~env ~daemon create invoke state _ ->
    let parent = create "generated" in
    Exn.protect
      ~finally:(fun () -> H.close parent)
      ~f:(fun () ->
        let call name fields = invoke parent name (`Object fields) |> Team.complete in
        let candidate = Jsonaf.of_string (Team.source "generated/create.json") in
        let child = invoke parent "agent_create" candidate |> Team.complete in
        let child_json = Team.field child "session_id" in
        let child_id = P.Id.Session.of_json child_json |> protocol_ok in
        let query = [ "session_id", child_json ] in
        let receipt =
          call
            "agent_send"
            (query
             @ [ "message", `String "Review the recorded documentation findings."
               ; "idempotency_key", `String "eviction-read-output"
               ])
        in
        call "agent_wait" (query @ [ "receipt_id", Team.field receipt "receipt_id" ])
        |> ignore;
        call
          "agent_stop"
          (query
           @ [ "mode", `String "graceful"
             ; "idempotency_key", `String "eviction-read-stop"
             ])
        |> ignore;
        let before = state child_id in
        let registry = D.registry daemon in
        let original = R.load registry child_id |> protocol_ok in
        Owner.unload_and_wait original.runtime |> protocol_ok;
        assert (not (Owner.is_loaded original.runtime));
        Eio.Switch.run (fun sw ->
          let entered, entered_u = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let evicted, evicted_u = Eio.Promise.create () in
          let release_once () =
            if Option.is_none (Eio.Promise.peek release)
            then Eio.Promise.resolve release_u ()
          in
          let retained = R.remove registry child_id |> Option.value_exn in
          assert (phys_equal retained.actor original.actor);
          R.add
            registry
            ~session_id:child_id
            { original with
              close =
                (fun () ->
                  Eio.Promise.resolve entered_u ();
                  Eio.Promise.await release;
                  original.close ())
            }
          |> protocol_ok;
          Exn.protect ~finally:release_once ~f:(fun () ->
            Eio.Fiber.fork ~sw (fun () ->
              let indexes =
                Agent_store.Session_store.list_sessions_checked (D.store daemon)
                |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                |> protocol_ok
                |> List.filter ~f:(fun entry ->
                  P.Id.Session.equal entry.session.id child_id)
              in
              let count = R.unload_inactive registry ~index_entries:indexes in
              Eio.Promise.resolve evicted_u count);
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
              Eio.Promise.await entered;
              assert (R.lifecycle_reserved registry child_id);
              let read, read_u = Eio.Promise.create () in
              Eio.Fiber.fork ~sw (fun () ->
                Eio.Promise.resolve read_u (invoke parent "agent_read" (`Object query)));
              Background_shell_tests.wait ~timeout:5. env (fun () ->
                let parent_state = state (H.session_id parent) in
                List.exists parent_state.invocations ~f:(fun invocation ->
                  match invocation.P.Invocation.status with
                  | Dispatching -> true
                  | Admitted | Resolved _ | Published _ -> false));
              assert (Option.is_none (Eio.Promise.peek read));
              release_once ();
              [%test_eq: int] (Eio.Promise.await evicted) 1;
              let output = Eio.Promise.await read |> Team.complete in
              assert (Team.contains output "Recorded verification failure");
              let after = state child_id in
              [%test_eq: int] before.identity.generation after.identity.generation;
              let reloaded = R.load registry child_id |> protocol_ok in
              assert (not (Owner.is_loaded reloaded.runtime));
              (* The read lifetime excludes a second eviction without activating
                 execution; release restores normal inactive eviction. *)
              let indexes () =
                Agent_store.Session_store.list_sessions_checked (D.store daemon)
                |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                |> protocol_ok
                |> List.filter ~f:(fun entry ->
                  P.Id.Session.equal entry.session.id child_id)
              in
              R.with_loaded_read registry child_id (fun retained ->
                assert (phys_equal retained.actor reloaded.actor);
                [%test_eq: int] (R.unload_inactive registry ~index_entries:(indexes ())) 0;
                Ok ())
              |> protocol_ok;
              (* Cancellation inside an admitted callback releases its exclusion
                 without closing or activating the retained session. *)
              (try
                 Eio.Switch.run (fun read_sw ->
                   let admitted, admitted_u = Eio.Promise.create () in
                   Eio.Fiber.fork ~sw:read_sw (fun () ->
                     R.with_loaded_read registry child_id (fun _ ->
                       Eio.Promise.resolve admitted_u ();
                       Eio.Fiber.await_cancel ())
                     |> protocol_ok);
                   Eio.Promise.await admitted;
                   assert (R.lifecycle_reserved registry child_id);
                   Eio.Switch.fail read_sw Exit);
                 failwith "read cancellation must propagate"
               with
               | Exit -> ());
              assert (not (R.lifecycle_reserved registry child_id));
              assert (not (Owner.is_loaded reloaded.runtime));
              [%test_eq: int] (R.unload_inactive registry ~index_entries:(indexes ())) 1;
              print_endline
                "stopped-child read survives eviction; read lifetime defers eviction")))));
  [%expect
    {|
    stopped-child read survives eviction; read lifetime defers eviction
    |}]
;;
