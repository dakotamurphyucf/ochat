open! Core
open Agent_server_test_support
open Retry_support
module Existing = Delegated_load_tests
module Warm = Delegated_warm_resource_tests
module Integration = Native_resource_integration_tests
module Store = Agent_store.Session_store

module Cache_failure = struct
  type t =
    { mutable cache : string option
    ; mutable armed : bool
    ; mutable attempts : int
    ; mutable failures : int
    ; mutable publications : int
    }

  let create () =
    { cache = None; armed = false; attempts = 0; failures = 0; publications = 0 }
  ;;

  let temporary t path =
    Option.exists t.cache ~f:(fun cache ->
      String.equal (Filename.dirname cache) (Filename.dirname path)
      && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
      && String.is_suffix path ~suffix:".tmp")
  ;;

  let wrap
        ?(fail =
          fun path ->
            raise (Core_unix.Unix_error (EIO, "635 native final cache publication", path)))
        t
        env
    =
    let observed =
      Crash_fault_io.wrap
        env
        ~boundary:After_rename
        ~matches:(temporary t)
        ~reached:(fun path ->
          [%test_eq: string] (Option.value_exn t.cache) path;
          t.publications <- t.publications + 1)
    in
    Crash_fault_io.wrap
      observed
      ~boundary:Before_rename
      ~matches:(temporary t)
      ~reached:(fun path ->
        t.attempts <- t.attempts + 1;
        if t.armed
        then (
          t.armed <- false;
          t.failures <- t.failures + 1;
          fail path))
  ;;
end

let%expect_test
    "shared native last release preserves cache failure and retries same holder"
  =
  let fault = Cache_failure.create () in
  let actual_failure = ref None in
  let fail path =
    try
      raise (Core_unix.Unix_error (EIO, "635 native final cache publication", path))
    with
    | exn ->
      let trace = Stdlib.Printexc.get_raw_backtrace () in
      actual_failure := Some (exn, trace);
      Exn.raise_with_original_backtrace exn trace
  in
  Existing.with_child
    ~wrap_env:(Cache_failure.wrap ~fail fault)
    (fun env _sw daemon parent child _gate providers ->
       let within phase f =
         try Eio.Time.with_timeout_exn env#clock 10. f with
         | Eio.Time.Timeout -> failwith ("635 native failure cut timed out: " ^ phase)
       in
       Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
       fault.cache
       <- Some
            (Filename.concat
               (Store.Handle.cache_directory
                  (Option.value_exn parent.Registry.store_handle))
               "cache.bin");
       within "close original child and parent" (fun () ->
         Owner.unload_and_wait child.runtime |> protocol_ok;
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 1 fault.publications;
       [%test_eq: int] 1 fault.attempts;
       within "retain actual native-only parent through Owned child" (fun () ->
         Owner.ensure_loaded child.runtime |> protocol_ok);
       assert (not (Owner.is_loaded parent.runtime));
       let native_before = Warm.settled parent in
       [%test_eq: int] 0 !providers;
       within "promote actual retained parent" (fun () ->
         Owner.ensure_loaded parent.runtime |> protocol_ok;
         Owner.drain_idle_moderator parent.runtime |> protocol_ok |> ignore);
       let promoted = Warm.loaded parent in
       Ordinary_warm_resource_tests.assert_live_source promoted;
       let before = Warm.settled parent in
       assert (P.Run_source.equal (Warm.source native_before) (Warm.source before));
       within "release actual child B before final Product C" (fun () ->
         Owner.unload_and_wait child.runtime |> protocol_ok);
       [%test_eq: int] 1 fault.publications;
       [%test_eq: int] 1 fault.attempts;
       let cache = Option.value_exn fault.cache in
       let load_cache () = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / cache) in
       let original_bytes = load_cache () in
       fault.armed <- true;
       within "observe original last-release cache failure" (fun () ->
         match Owner.unload_and_wait parent.runtime with
         | Ok () -> failwith "635 last release swallowed the actual cache failure"
         | Error error ->
           raise_s
             [%sexp "635 last release changed raised cache failure", (error : P.Error.t)]
         | exception Core_unix.Unix_error (EIO, operation, path) ->
           [%test_eq: string] "635 native final cache publication" operation;
           assert (Cache_failure.temporary fault path));
       [%test_eq: int] 1 fault.failures;
       [%test_eq: int] 2 fault.attempts;
       [%test_eq: int] 1 fault.publications;
       [%test_eq: string] original_bytes (load_cache ());
       assert (not (Owner.is_loaded parent.runtime));
       assert (
         Store.owns_handle (Daemon.store daemon) (Option.value_exn parent.store_handle));
       (* The failed common close has not unwound its native child switch. This
          is retained cleanup, not fresh borrowing or execution permission. *)
       (match A.Prompt_source_lease.availability promoted.source_lease with
        | Live -> ()
        | Missing_receipt | Unavailable _ | Revoked | Closed ->
          failwith "635 failed cache leg lost its actual retained source switch");
       Integration.no_lifecycle_replay before (Warm.settled parent);
       [%test_eq: int] 0 !providers;
       within "explicitly retry same retained close" (fun () ->
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 1 fault.failures;
       [%test_eq: int] 3 fault.attempts;
       [%test_eq: int] 2 fault.publications;
       (match A.Prompt_source_lease.availability promoted.source_lease with
        | Closed -> ()
        | Missing_receipt | Live | Unavailable _ | Revoked ->
          failwith "635 successful close retry did not join the original source switch");
       within "repeat joined unload without cache replay" (fun () ->
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 3 fault.attempts;
       [%test_eq: int] 2 fault.publications;
       within "fresh independent construction after same-holder retry" (fun () ->
         Owner.ensure_loaded parent.runtime |> protocol_ok);
       let fresh = Warm.loaded parent in
       assert (
         not
           (phys_equal
              (Option.value_exn promoted.native_runtime)
              (Option.value_exn fresh.native_runtime)));
       within "close fresh unit" (fun () ->
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 4 fault.attempts;
       [%test_eq: int] 3 fault.publications;
       [%test_eq: int] 0 !providers;
       let recording = Stdlib.Printexc.backtrace_status () in
       Creation_tests.protect_cleanup
         ~finally:(fun () ->
           fault.armed <- false;
           Stdlib.Printexc.record_backtrace recording)
         (fun () ->
            Stdlib.Printexc.record_backtrace true;
            let module R = Owner.Resource_retirement in
            let run_new = function
              | R.Run work ->
                let completion = R.completion work in
                R.run work;
                completion
              | Join _ -> failwith "638 expected a new external resource retirement"
            in
            let expect_failure completion (expected, expected_trace) =
              match Existing.capture_load (fun () -> R.await completion) with
              | Raised (actual, trace) ->
                assert (phys_equal actual expected);
                let prefix = Stdlib.Printexc.raw_backtrace_to_string expected_trace in
                assert (not (String.is_empty prefix));
                assert (
                  String.is_prefix (Stdlib.Printexc.raw_backtrace_to_string trace) ~prefix)
              | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
              | Returned (Ok ()) ->
                failwith "638 retained retirement lost its actual failure"
            in
            let expect_same_join completion = function
              | R.Join actual -> assert (phys_equal actual completion)
              | Run work ->
                (* Settle an unexpectedly elected producer before reporting the failed
              identity oracle; never strand a queued actual Owner attempt. *)
                R.run work;
                failwith "638 replaced the actual failed retirement instead of joining it"
            in
            let parent_before = Warm.settled parent in
            let child_before = Warm.settled child in
            (* The ledger still contains this real Running child, but its previous
          borrow has ended. Complete retirement must not permanently close that
          historical child Owner. Its next genuine load proves this distinction. *)
            within "retire parent with historical child but no current borrow" (fun () ->
              let completion =
                Owner.claim_resource_retirement parent.runtime |> protocol_ok |> run_new
              in
              R.await completion |> protocol_ok);
            [%test_eq: int] 4 fault.attempts;
            [%test_eq: int] 3 fault.publications;
            assert (not (Owner.is_loaded child.runtime));
            Integration.no_lifecycle_replay parent_before (Warm.settled parent);
            Integration.no_lifecycle_replay child_before (Warm.settled child);
            within
              "historical child remains reusable and takes an actual new native pin"
              (fun () -> Owner.ensure_loaded child.runtime |> protocol_ok);
            assert (Owner.is_loaded child.runtime);
            assert (not (Owner.is_loaded parent.runtime));
            let borrowed_parent = Warm.settled parent in
            let borrowed_child = Warm.settled child in
            fault.armed <- true;
            let failed =
              within
                "external provider retirement drains its actual registered child"
                (fun () ->
                   Owner.claim_resource_retirement parent.runtime
                   |> protocol_ok
                   |> run_new)
            in
            let original_failure = Option.value_exn !actual_failure in
            expect_failure failed original_failure;
            let original_outcome = Option.value_exn (R.peek failed) in
            assert (not (Owner.is_loaded child.runtime));
            assert (not (Owner.is_loaded parent.runtime));
            (* The inherited prompt lease belongs to the retained provider.
               Prove the child's own cleanup joined through its actual public
               closer while that provider failure remains armed. *)
            within
              "actual child cleanup is joined independently of provider failure"
              (fun () -> Owner.unload_and_wait child.runtime |> protocol_ok);
            [%test_eq: int] 2 fault.failures;
            [%test_eq: int] 5 fault.attempts;
            [%test_eq: int] 3 fault.publications;
            Owner.claim_resource_retirement parent.runtime
            |> protocol_ok
            |> expect_same_join failed;
            Owner.signal_resource_retirement parent.runtime
            |> protocol_ok
            |> expect_same_join failed;
            Integration.no_lifecycle_replay borrowed_parent (Warm.settled parent);
            Integration.no_lifecycle_replay borrowed_child (Warm.settled child);
            [%test_eq: int] 0 !providers;
            within
              "explicit retry repairs the exact retained provider after child join"
              (fun () ->
                 let repaired =
                   Owner.retry_resource_retirement parent.runtime ~failed
                   |> protocol_ok
                   |> run_new
                 in
                 R.await repaired |> protocol_ok);
            (match original_outcome, Option.value_exn (R.peek failed) with
             | Raised (old_exn, old_trace), Raised (actual, trace) ->
               assert (phys_equal old_exn actual);
               assert (phys_equal old_trace trace)
             | _ -> failwith "638 factual retry rewrote the old retirement outcome");
            expect_failure failed original_failure;
            [%test_eq: int] 2 fault.failures;
            [%test_eq: int] 6 fault.attempts;
            [%test_eq: int] 4 fault.publications;
            within
              "joined parent and child cleanup never replays physical release"
              (fun () ->
                 Owner.unload_and_wait child.runtime |> protocol_ok;
                 Owner.unload_and_wait parent.runtime |> protocol_ok);
            [%test_eq: int] 6 fault.attempts;
            [%test_eq: int] 4 fault.publications;
            within
              "both actual Owners admit fresh construction after external repair"
              (fun () ->
                 Owner.ensure_loaded child.runtime |> protocol_ok;
                 Owner.ensure_loaded parent.runtime |> protocol_ok;
                 Owner.drain_idle_moderator parent.runtime |> protocol_ok |> ignore);
            assert (Owner.is_loaded child.runtime);
            assert (Owner.is_loaded parent.runtime);
            let final_parent = Warm.loaded parent in
            Ordinary_warm_resource_tests.assert_live_source final_parent;
            let final_parent_state = Warm.settled parent in
            let final_child_state = Warm.settled child in
            (* Registry's runtime barrier is separate from reusable retirement. It
          permanently closes both Owners, so test it only after restart above. *)
            fault.armed <- true;
            let registry = Daemon.registry daemon in
            let retained =
              within
                "Registry retains all resources while a native provider close fails"
                (fun () -> Registry.shutdown_reported registry)
            in
            assert (not (Agent_server.Session_disposal_report.disposed retained));
            let registry_failure = Option.value_exn !actual_failure in
            [%test_eq: int] 3 fault.failures;
            [%test_eq: int] 7 fault.attempts;
            [%test_eq: int] 4 fault.publications;
            List.iter [ parent; child ] ~f:(fun entry ->
              Actor.state entry.Registry.actor |> protocol_ok |> ignore;
              assert (
                Store.owns_handle
                  (Daemon.store daemon)
                  (Option.value_exn entry.store_handle));
              assert (
                Result.is_ok
                  (Store.Handle.metadata_checked (Option.value_exn entry.store_handle))));
            (match A.Prompt_source_lease.availability final_parent.source_lease with
             | Live -> ()
             | Missing_receipt | Unavailable _ | Revoked | Closed ->
               failwith
                 "638 Registry canceled the retained provider switch before cleanup");
            Integration.no_lifecycle_replay final_parent_state (Warm.settled parent);
            Integration.no_lifecycle_replay final_child_state (Warm.settled child);
            (* This public claim is a read/join of the actual failed closed Owner,
          never a fabricated Registry retirement handle or a second producer. *)
            let registry_failed =
              match Owner.claim_resource_retirement parent.runtime |> protocol_ok with
              | Join completion -> completion
              | Run work ->
                R.run work;
                failwith "638 Registry did not retain its actual failed retirement"
            in
            expect_failure registry_failed registry_failure;
            Owner.claim_resource_retirement parent.runtime
            |> protocol_ok
            |> expect_same_join registry_failed;
            within
              "repair the exact closed provider before Registry resource phase"
              (fun () ->
                 let repaired =
                   Owner.retry_resource_retirement parent.runtime ~failed:registry_failed
                   |> protocol_ok
                   |> run_new
                 in
                 R.await repaired |> protocol_ok);
            [%test_eq: int] 3 fault.failures;
            [%test_eq: int] 8 fault.attempts;
            [%test_eq: int] 5 fault.publications;
            let disposed =
              within "Registry joins runtime repair before Actors and Handles" (fun () ->
                Registry.shutdown_reported registry)
            in
            assert (Agent_server.Session_disposal_report.disposed disposed);
            assert (phys_equal disposed (Registry.shutdown_reported registry));
            List.iter [ parent; child ] ~f:(fun entry ->
              assert (Result.is_error (Actor.state entry.Registry.actor));
              assert (
                Result.is_error
                  (Store.Handle.metadata_checked (Option.value_exn entry.store_handle))));
            assert (not (Agent_server.Session_disposal_report.disposed retained));
            [%test_eq: int] 8 fault.attempts;
            [%test_eq: int] 5 fault.publications;
            [%test_eq: int] 0 !providers));
  print_endline
    "actual last cache failure retained; same-holder retry publishes once; joined unload \
     never republishes";
  [%expect
    {| actual last cache failure retained; same-holder retry publishes once; joined unload never republishes |}]
;;

type native_consumer_retirement =
  | Exact_cell
  | Covering_whole
  | Joined_close_cell

let native_build_consumer_cache_failure retirement =
  let module R = Owner.Resource_retirement in
  let module Tree = Descendant_ancestry_support in
  let provider_cache = Cache_failure.create () in
  let consumer_cache = Cache_failure.create () in
  let entered, enter = Eio.Promise.create () in
  let released, release = Eio.Promise.create () in
  let release_cache () =
    if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
  in
  let actual_failure = ref None in
  let fail_consumer path =
    Eio.Promise.resolve enter ();
    Eio.Cancel.protect (fun () -> Eio.Promise.await released);
    try
      raise (Core_unix.Unix_error (EIO, "638 actual Native_build consumer cache", path))
    with
    | exn ->
      let trace = Stdlib.Printexc.get_raw_backtrace () in
      actual_failure := Some (exn, trace);
      Exn.raise_with_original_backtrace exn trace
  in
  let recording = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace true;
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
    ~f:(fun () ->
      Tree.with_tree
        ~wrap_env:(fun env ->
          Cache_failure.wrap
            ~fail:fail_consumer
            consumer_cache
            (Cache_failure.wrap provider_cache env))
        ~before_provider:(fun _tree _inputs ->
          failwith "638 native consumer cleanup unexpectedly entered a provider")
        (fun (tree : Tree.t) ->
           let parent = tree.root in
           let child = tree.intermediate.entry in
           let leaf = tree.grandchild.entry in
           let entries = [ leaf; tree.sibling.entry; child; parent ] in
           let within phase f =
             Tree.within tree.env phase (fun () ->
               let value = f () in
               Tree.raise_failure tree;
               value)
           in
           let cache_path (entry : Registry.entry) =
             Filename.concat
               (Store.Handle.cache_directory (Option.value_exn entry.store_handle))
               "cache.bin"
           in
           provider_cache.cache <- Some (cache_path parent);
           consumer_cache.cache <- Some (cache_path child);
           let current = ref None in
           let producer = ref None in
           let original_target = ref None in
           let checkpoint_entered, enter_checkpoint = Eio.Promise.create () in
           let checkpoint_release, release_checkpoint = Eio.Promise.create () in
           let checkpoint_task = ref None in
           let closing_task = ref None in
           let release_mailbox () =
             if Option.is_none (Eio.Promise.peek checkpoint_release)
             then Eio.Promise.resolve release_checkpoint ()
           in
           let check_permanently_closed () =
             match
               Owner.with_auxiliary_execution_lifetime child.runtime (fun _ ->
                 failwith "638 closed target let a new runtime lifetime enter")
             with
             | Ok () -> failwith "638 already closed target unexpectedly admitted work"
             | Error error -> [%test_eq: P.Error.code] Server_shutting_down error.code
           in
           let start_joined_close () =
             (match (Warm.settled parent).lifecycle.desired with
              | Running -> ()
              | Stopped -> failwith "638 test needs the physical native dependency walk");
             let checkpoint =
               Eio.Fiber.fork_promise ~sw:tree.sw (fun () ->
                 Actor.checkpoint child.actor ~persist:(fun _ ->
                   Eio.Promise.resolve enter_checkpoint ();
                   Eio.Promise.await checkpoint_release;
                   Ok ()))
             in
             checkpoint_task := Some checkpoint;
             Eio.Fiber.first
               (fun () -> Eio.Promise.await checkpoint_entered)
               (fun () ->
                  Eio.Promise.await_exn checkpoint |> protocol_ok;
                  failwith "638 target checkpoint returned before entering its gate");
             let closing =
               Eio.Fiber.fork_promise ~sw:tree.sw (fun () ->
                 Existing.capture_load (fun () ->
                   Owner.close_and_wait child.runtime;
                   Ok ()))
             in
             closing_task := Some closing;
             (* The fork reaches the actual target Actor RPC. Its checkpoint
                holds that mailbox until the real provider elects the Cell. *)
             (* Checking a new execution lifetime would itself require the
                blocked Actor mailbox. The observed handoff below proves that
                the permanent prelude was installed before the Cell election. *)
             assert (Option.is_none (Eio.Promise.peek closing))
           in
           let finish_work work =
             R.run work;
             R.await (R.completion work) |> protocol_ok
           in
           let repair_target failed =
             match
               Owner.retry_resource_retirement child.runtime ~failed |> protocol_ok
             with
             | R.Run work ->
               finish_work work;
               original_target := None
             | Join _ -> failwith "638 exact target failure was replaced"
           in
           let run_new = function
             | R.Run work ->
               let completion = R.completion work in
               current := Some completion;
               R.run work;
               completion
             | Join completion ->
               current := Some completion;
               failwith "638 expected new genuine external retirement work"
           in
           let expect_raw (expected, expected_trace) = function
             | Owner.Scope_outcome.Raised (actual, trace) ->
               assert (phys_equal actual expected);
               let prefix = Stdlib.Printexc.raw_backtrace_to_string expected_trace in
               assert (not (String.is_empty prefix));
               assert (
                 String.is_prefix (Stdlib.Printexc.raw_backtrace_to_string trace) ~prefix)
             | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
             | Returned (Ok ()) ->
               failwith "638 external retirement lost actual consumer G"
           in
           let expect_failure completion expected =
             expect_raw expected (Existing.capture_load (fun () -> R.await completion))
           in
           let expect_same_join expected = function
             | R.Join actual -> assert (phys_equal expected actual)
             | Run work ->
               current := Some (R.completion work);
               R.run work;
               failwith "638 repeated claim replaced retained consumer failure"
           in
           let no_replay (before : A.Session_state.t) entry =
             let after = Warm.settled entry in
             [%test_eq: int] before.identity.generation after.identity.generation;
             assert (
               P.Id.Prompt_revision.equal
                 before.spec.prompt_revision_id
                 after.spec.prompt_revision_id);
             assert (
               List.equal
                 P.Moderator_execution.equal
                 before.moderator_executions
                 after.moderator_executions);
             assert (Option.equal Jsonaf.exactly_equal before.moderator after.moderator);
             assert (Option.is_none after.active_operation);
             match after.runtime_initialization with
             | Ready -> ()
             | Pending _ -> failwith "638 cleanup changed genuine runtime initialization"
           in
           Creation_tests.protect_cleanup
             ~finally:(fun () ->
               consumer_cache.armed <- false;
               provider_cache.armed <- false;
               release_mailbox ();
               release_cache ();
               within "join producer and close real tree resources" (fun () ->
                 Option.iter !checkpoint_task ~f:(fun task ->
                   Eio.Promise.await_exn task |> protocol_ok);
                 Option.iter !closing_task ~f:(fun task ->
                   Eio.Promise.await_exn task |> ignore);
                 Option.iter !producer ~f:(fun task ->
                   Eio.Promise.await_exn task |> ignore);
                 Option.iter !original_target ~f:(fun (work, failed) ->
                   match R.peek failed with
                   | None -> finish_work work
                   | Some (Returned (Ok ())) -> ()
                   | Some (Returned (Error _) | Raised _) -> repair_target failed);
                 Option.iter !current ~f:(fun failed ->
                   match R.peek failed with
                   | Some (Returned (Ok ())) -> current := None
                   | Some (Returned (Error _) | Raised _) ->
                     let repaired =
                       Owner.retry_resource_retirement parent.runtime ~failed
                       |> protocol_ok
                       |> run_new
                     in
                     R.await repaired |> protocol_ok;
                     current := None
                   | None ->
                     failwith "638 actual tree retirement producer remained unfinished");
                 List.iter entries ~f:(fun entry ->
                   Owner.unload_and_wait entry.runtime |> protocol_ok)))
             (fun () ->
                within
                  "settle actual tree operations before full baseline disposal"
                  (fun () ->
                     List.iter entries ~f:(fun entry ->
                       Goal_native_test_support.await_quiet entry |> ignore));
                within
                  "dispose old leaf sibling intermediate and provider units"
                  (fun () ->
                     List.iter entries ~f:(fun entry ->
                       Owner.unload_and_wait entry.runtime |> protocol_ok));
                List.iter entries ~f:(fun entry ->
                  assert (not (Owner.is_loaded entry.runtime));
                  assert (Owner.source_lane_idle entry.runtime));
                List.iter [ consumer_cache; provider_cache ] ~f:(fun cache ->
                  [%test_eq: int] 1 cache.attempts;
                  [%test_eq: int] 1 cache.publications;
                  [%test_eq: int] 0 cache.failures);
                within "repeat old full cleanup without physical replay" (fun () ->
                  List.iter entries ~f:(fun entry ->
                    Owner.unload_and_wait entry.runtime |> protocol_ok));
                [%test_eq: int] 1 consumer_cache.attempts;
                [%test_eq: int] 1 provider_cache.attempts;
                within
                  "load actual provider source before cold native intermediate"
                  (fun () ->
                     Owner.ensure_loaded parent.runtime |> protocol_ok;
                     Owner.drain_idle_moderator parent.runtime |> protocol_ok |> ignore);
                assert (Owner.is_loaded parent.runtime);
                assert (not (Owner.is_loaded child.runtime));
                (* C's actual generated-source borrow prepares cold B through Native_build,
                 not restored Product policy. B's retain driver holds Resource B's A loan
                 while C owns the real native B pin. No B ordinary binding is installed. *)
                within
                  "load genuine leaf through actual cold Native_build intermediate"
                  (fun () -> Owner.ensure_loaded leaf.runtime |> protocol_ok);
                assert (Owner.is_loaded leaf.runtime);
                assert (not (Owner.is_loaded child.runtime));
                assert (Owner.is_loaded parent.runtime);
                within
                  "detach provider baseline while actual native intermediate holds it"
                  (fun () -> Owner.unload_and_wait parent.runtime |> protocol_ok);
                assert (not (Owner.is_loaded parent.runtime));
                List.iter [ consumer_cache; provider_cache ] ~f:(fun cache ->
                  [%test_eq: int] 1 cache.attempts;
                  [%test_eq: int] 1 cache.publications);
                let before =
                  List.map entries ~f:(fun entry -> entry, Warm.settled entry)
                in
                let providers_before = !(tree.counters.providers) in
                let child_file =
                  Eio.Path.(
                    Eio.Stdenv.fs tree.env / Option.value_exn consumer_cache.cache)
                in
                let original_bytes = Eio.Path.load child_file in
                consumer_cache.armed <- true;
                let covering =
                  match retirement with
                  | Exact_cell -> None
                  | Joined_close_cell ->
                    start_joined_close ();
                    None
                  | Covering_whole ->
                    (* Only the target Owner elects this covering Whole. The
                       provider must join it through its same captured consumer. *)
                    let work =
                      match
                        Owner.signal_resource_retirement child.runtime |> protocol_ok
                      with
                      | R.Run work -> work
                      | Join _ -> failwith "638 target already had a covering retirement"
                    in
                    let failed = R.completion work in
                    original_target := Some (work, failed);
                    Some failed
                in
                let work =
                  match Owner.claim_resource_retirement parent.runtime |> protocol_ok with
                  | R.Run work ->
                    current := Some (R.completion work);
                    work
                  | Join _ ->
                    failwith
                      "638 expected actual provider election with active Native_build \
                       consumer"
                in
                let failed = R.completion work in
                let task =
                  Eio.Fiber.fork_promise ~sw:tree.sw (fun () ->
                    Existing.capture_load (fun () ->
                      R.run work;
                      R.await failed))
                in
                producer := Some task;
                let handed_off =
                  Option.map !closing_task ~f:(fun closing ->
                    let rec observe () =
                      match Owner.For_testing.joined_close_cell child.runtime with
                      | Some exact -> exact
                      | None ->
                        (match Eio.Promise.peek task with
                         | Some _ ->
                           Existing.restore (Eio.Promise.await_exn task);
                           failwith "638 provider returned before an exact Cell handoff"
                         | None ->
                           assert (Option.is_none (Eio.Promise.peek closing));
                           Eio.Fiber.yield ();
                           observe ())
                    in
                    let exact =
                      within "observe joined close exact Cell handoff" observe
                    in
                    assert (Option.is_none (R.peek exact));
                    assert (Option.is_none (Eio.Promise.peek closing));
                    release_mailbox ();
                    Eio.Promise.await_exn (Option.value_exn !checkpoint_task)
                    |> protocol_ok;
                    exact)
                in
                (match
                   within
                     "actual native consumer cache gate or early retirement"
                     (fun () ->
                        Eio.Fiber.first
                          (fun () ->
                             Eio.Promise.await entered;
                             `Entered)
                          (fun () -> `Returned (Eio.Promise.await_exn task)))
                 with
                 | `Entered -> ()
                 | `Returned outcome ->
                   Existing.restore outcome;
                   failwith "638 provider returned before genuine native consumer close");
                assert (Option.is_none (R.peek failed));
                [%test_eq: int] 2 consumer_cache.attempts;
                [%test_eq: int] 1 consumer_cache.failures;
                [%test_eq: int] 1 consumer_cache.publications;
                [%test_eq: int] 1 provider_cache.attempts;
                [%test_eq: int] 1 provider_cache.publications;
                release_cache ();
                within "join original consumer-origin failure" (fun () ->
                  Eio.Promise.await_exn task |> ignore);
                let original_failure = Option.value_exn !actual_failure in
                expect_failure failed original_failure;
                Option.iter handed_off ~f:(fun exact ->
                  expect_failure exact original_failure;
                  within "joined target receives same actual Cell failure" (fun () ->
                    Eio.Promise.await_exn (Option.value_exn !closing_task)
                    |> expect_raw original_failure));
                let old_outcome = Option.value_exn (R.peek failed) in
                [%test_eq: string] original_bytes (Eio.Path.load child_file);
                assert (not (Owner.is_loaded leaf.runtime));
                assert (not (Owner.source_lane_idle parent.runtime));
                Owner.claim_resource_retirement parent.runtime
                |> protocol_ok
                |> expect_same_join failed;
                Owner.signal_resource_retirement parent.runtime
                |> protocol_ok
                |> expect_same_join failed;
                [%test_eq: int] 2 consumer_cache.attempts;
                [%test_eq: int] 1 provider_cache.attempts;
                [%test_eq: int] 1 provider_cache.publications;
                [%test_eq: int] 0 provider_cache.failures;
                List.iter before ~f:(fun (entry, saved) -> no_replay saved entry);
                [%test_eq: int] providers_before !(tree.counters.providers);
                let unrelated =
                  Option.map covering ~f:(fun target_failed ->
                    expect_failure target_failed original_failure;
                    within "target alone repairs its same covering Whole" (fun () ->
                      repair_target target_failed);
                    expect_failure target_failed original_failure;
                    (* A different Whole can now be accepted. The provider's
                       retry must use the completed authentic successor above. *)
                    match
                      Owner.signal_resource_retirement child.runtime |> protocol_ok
                    with
                    | R.Run work -> R.completion work
                    | Join _ -> failwith "638 completed target repair retained election")
                in
                within
                  "explicit actual retry repairs Native_build consumer before provider"
                  (fun () ->
                     let repaired =
                       Owner.retry_resource_retirement parent.runtime ~failed
                       |> protocol_ok
                       |> run_new
                     in
                     R.await repaired |> protocol_ok;
                     current := None);
                Option.iter unrelated ~f:(fun current ->
                  assert (Option.is_none (R.peek current)));
                (match old_outcome, Option.value_exn (R.peek failed) with
                 | Raised (old_exn, old_trace), Raised (actual, trace) ->
                   assert (phys_equal old_exn actual);
                   assert (phys_equal old_trace trace)
                 | _ -> failwith "638 native consumer repair rewrote immutable old G");
                expect_failure failed original_failure;
                [%test_eq: int] 3 consumer_cache.attempts;
                [%test_eq: int] 2 consumer_cache.publications;
                [%test_eq: int] 1 consumer_cache.failures;
                [%test_eq: int] 2 provider_cache.attempts;
                [%test_eq: int] 2 provider_cache.publications;
                [%test_eq: int] 0 provider_cache.failures;
                Option.iter handed_off ~f:(fun exact ->
                  expect_failure exact original_failure;
                  within
                    "permanent target joins remaining Whole after Cell repair"
                    (fun () ->
                       let report = Owner.close_and_wait_reported child.runtime in
                       match Owner.Disposal_report.disposition report with
                       | Joined -> ()
                       | Retained ->
                         failwith "638 target retained after exact Cell repair");
                  check_permanently_closed ());
                List.iter before ~f:(fun (entry, saved) -> no_replay saved entry);
                [%test_eq: int] providers_before !(tree.counters.providers);
                within "repeat joined native tree cleanup without replay" (fun () ->
                  List.iter entries ~f:(fun entry ->
                    Owner.unload_and_wait entry.runtime |> protocol_ok));
                [%test_eq: int] 3 consumer_cache.attempts;
                [%test_eq: int] 2 provider_cache.attempts;
                (match retirement with
                 | Joined_close_cell -> assert (not (Owner.is_loaded child.runtime))
                 | Exact_cell | Covering_whole ->
                   within "genuine fresh provider leaf and intermediate reuse" (fun () ->
                     Owner.ensure_loaded parent.runtime |> protocol_ok;
                     Owner.drain_idle_moderator parent.runtime |> protocol_ok |> ignore;
                     Owner.ensure_loaded leaf.runtime |> protocol_ok;
                     Owner.ensure_loaded child.runtime |> protocol_ok);
                   List.iter [ parent; child; leaf ] ~f:(fun entry ->
                     assert (Owner.is_loaded entry.runtime);
                     Ordinary_warm_resource_tests.assert_live_source (Warm.loaded entry));
                   within "close genuinely fresh tree units" (fun () ->
                     List.iter entries ~f:(fun entry ->
                       Owner.unload_and_wait entry.runtime |> protocol_ok));
                   [%test_eq: int] 4 consumer_cache.attempts;
                   [%test_eq: int] 3 consumer_cache.publications;
                   [%test_eq: int] 3 provider_cache.attempts;
                   [%test_eq: int] 3 provider_cache.publications);
                [%test_eq: int] providers_before !(tree.counters.providers))))
;;

let%expect_test
    "actual Native_build consumer cache failure retains provider through retry"
  =
  native_build_consumer_cache_failure Exact_cell;
  print_endline
    "actual Native_build consumer G retains provider; exact retry joins child then \
     provider; old G stays fixed";
  [%expect
    {| actual Native_build consumer G retains provider; exact retry joins child then provider; old G stays fixed |}]
;;

let%expect_test
    "native consumer joins completed Whole repair despite unrelated queued retirement"
  =
  native_build_consumer_cache_failure Covering_whole;
  print_endline
    "same failed Whole repaired by target; provider joins its completed successor";
  [%expect
    {| same failed Whole repaired by target; provider joins its completed successor |}]
;;

let%expect_test
    "joined close hands off exact Cell and provider repairs its physical failure"
  =
  native_build_consumer_cache_failure Joined_close_cell;
  print_endline
    "same Cell failure retained; exact provider retry releases once; target joins Whole";
  [%expect
    {| same Cell failure retained; exact provider retry releases once; target joins Whole |}]
;;

let%expect_test "ordinary child retries exact returned outer cleanup after cancellation" =
  let fault = Cache_failure.create () in
  let sentinel = Failure "645 actual retained parent cache cleanup" in
  let body_completed = ref false in
  let callback_failure = ref None in
  let current_phase = ref "construct actual Factory fixture" in
  let expect_actual_cancel f =
    match f () with
    | Ok () -> failwith "645 actual outer cleanup cancellation became success"
    | Error error ->
      raise_s [%sexp "645 cleanup cancellation became protocol data", (error : P.Error.t)]
    | exception Eio.Cancel.Cancelled cause -> assert (phys_equal cause sentinel)
  in
  let fixture =
    try
      Existing.with_child
        ~wrap_env:
          (Cache_failure.wrap
             ~fail:(fun _ -> raise (Eio.Cancel.Cancelled sentinel))
             fault)
        (fun env _sw _daemon parent child _gate providers ->
           let run_body () =
             let within phase f =
               current_phase := phase;
               try Eio.Time.with_timeout_exn env#clock 10. f with
               | Eio.Time.Timeout -> failwith ("645 outer cleanup timed out: " ^ phase)
             in
             Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
             fault.cache
             <- Some
                  (Filename.concat
                     (Store.Handle.cache_directory
                        (Option.value_exn parent.Registry.store_handle))
                     "cache.bin");
             (* Use the already-qualified native-first -> Product promotion
                route: its physical common owner has factual cache retry. The
                generic loaded-first runtime tracker retains an immutable failed
                close, which is a separate ordinary-resource retry contract. *)
             within "close initial child and parent before native-first setup" (fun () ->
               Owner.unload_and_wait child.runtime |> protocol_ok;
               Owner.unload_and_wait parent.runtime |> protocol_ok);
             within "retain actual native-first parent through child" (fun () ->
               Owner.ensure_loaded child.runtime |> protocol_ok);
             assert (not (Owner.is_loaded parent.runtime));
             within "promote same native parent to ordinary Product" (fun () ->
               Owner.ensure_loaded parent.runtime |> protocol_ok;
               Owner.drain_idle_moderator parent.runtime |> protocol_ok |> ignore);
             let before = Warm.settled parent in
             let attempts = fault.attempts in
             let publications = fault.publications in
             [%test_eq: int] attempts 1;
             [%test_eq: int] publications 1;
             (* install records SAME-native Product before dropping baseline A.
                Shared_native retention keeps C when parent unloads; B's pin
                then closes nonlast, and OUTER lease detachment elects C's last
                common close. The actual negative therefore is not pin-only. *)
             within "retire ordinary parent with surviving actual child" (fun () ->
               Owner.unload_and_wait parent.runtime |> protocol_ok);
             [%test_eq: int] attempts fault.attempts;
             fault.armed <- true;
             within "first actual outer failure" (fun () ->
               expect_actual_cancel (fun () -> Owner.unload_and_wait child.runtime));
             [%test_eq: int] 1 fault.failures;
             [%test_eq: int] (attempts + 1) fault.attempts;
             [%test_eq: int] publications fault.publications;
             (* The normally returned wrapper owns the exact physical cleanup
                batch. This ordinary retirement drops its temporary public slot;
                the next ordinary unload repairs the retained factual holder. *)
             within "actual child returned-outer cleanup retry" (fun () ->
               Owner.unload_and_wait child.runtime |> protocol_ok);
             [%test_eq: int] (attempts + 2) fault.attempts;
             [%test_eq: int] (publications + 1) fault.publications;
             within "repeat child and parent cleanup without physical replay" (fun () ->
               Owner.unload_and_wait child.runtime |> protocol_ok;
               Owner.unload_and_wait parent.runtime |> protocol_ok);
             [%test_eq: int] (attempts + 2) fault.attempts;
             [%test_eq: int] (publications + 1) fault.publications;
             [%test_eq: int] 1 fault.failures;
             Integration.no_lifecycle_replay before (Warm.settled parent);
             [%test_eq: int] 0 !providers;
             body_completed := true
           in
           match run_body () with
           | () -> ()
           | exception exn ->
             let backtrace = Stdlib.Printexc.get_raw_backtrace () in
             callback_failure := Some (exn, backtrace);
             (try
                Eio.traceln
                  "645 outer callback failed at %s: %s"
                  !current_phase
                  (Exn.to_string exn)
              with
              | _ -> ());
             Exn.raise_with_original_backtrace exn backtrace);
      Ok ()
    with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  if not !body_completed
  then (
    match !callback_failure, fixture with
    | Some (exn, backtrace), _ | None, Error (exn, backtrace) ->
      Exn.raise_with_original_backtrace exn backtrace
    | None, Ok () -> failwith "645 fixture did not execute its cleanup assertions");
  (* Current factual cleanup is complete; fixture disposal must succeed. *)
  (match fixture with
   | Ok () -> ()
   | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
  print_endline
    "driver success checks actual outer cleanup; exact returned holder repairs; no \
     physical replay";
  [%expect
    {| driver success checks actual outer cleanup; exact returned holder repairs; no physical replay |}]
;;

let%expect_test
    "native-first restored baseline leaves actual last pin for explicit same-pin retry"
  =
  let module Graph_fault = Inference_close_tests.Fault in
  let module Ledger = A.Inference_ledger in
  let module Observation = Inference.Observation in
  let fault = Cache_failure.create () in
  let graph_fault = Graph_fault.create () in
  let retry_armed = ref false in
  let retry_entered, enter_retry = Eio.Promise.create () in
  let retry_released, release_retry = Eio.Promise.create () in
  let release_retry_gate () =
    if Option.is_none (Eio.Promise.peek retry_released)
    then Eio.Promise.resolve release_retry ()
  in
  let hold_retry _path =
    if !retry_armed
    then (
      retry_armed := false;
      Eio.Promise.resolve enter_retry ();
      Eio.Cancel.protect (fun () -> Eio.Promise.await retry_released))
  in
  let fail path =
    (* This exact physical pin close precedes C's tracked graph finish. *)
    graph_fault.phase <- Reject_finish_once;
    raise (Core_unix.Unix_error (EIO, "635 native final cache publication", path))
  in
  Existing.with_child
    ~on_post_stream:(fun () -> graph_fault.phase <- Reject_settlement)
    ~wrap_env:(fun env ->
      let observed =
        Cache_failure.wrap ~fail fault (Graph_fault.wrap_env graph_fault env)
      in
      Crash_fault_io.wrap
        observed
        ~boundary:Before_rename
        ~matches:(Cache_failure.temporary fault)
        ~reached:hold_retry)
    (fun env sw daemon parent child _gate providers ->
       let within phase f =
         try Eio.Time.with_timeout_exn env#clock 15. f with
         | Eio.Time.Timeout -> failwith ("645 actual last pin timed out: " ^ phase)
       in
       Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
       fault.cache
       <- Some
            (Filename.concat
               (Store.Handle.cache_directory
                  (Option.value_exn parent.Registry.store_handle))
               "cache.bin");
       within "close original child and parent unit" (fun () ->
         Owner.unload_and_wait child.runtime |> protocol_ok;
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 1 fault.attempts;
       [%test_eq: int] 1 fault.publications;
       let before_native = Warm.settled parent in
       within "actual native-first provider retained by child" (fun () ->
         Owner.ensure_loaded child.runtime |> protocol_ok);
       assert (not (Owner.is_loaded parent.runtime));
       assert (Owner.is_loaded child.runtime);
       Integration.no_lifecycle_replay before_native (Warm.settled parent);
       let phase = ref "645 actual policy adoption" in
       within "genuine policy adoption without ordinary binding" (fun () ->
         Cold_policy_resource_tests.run_standalone_policy
           ~phase
           env
           daemon
           parent
           (Warm.settled parent)
           ~original
         |> ignore);
       assert (not (Owner.is_loaded parent.runtime));
       let after_policy = Warm.settled parent in
       (* This releases the restored Product anchor, not a current/retired
         ordinary binding. Native baseline was already released at adoption.
         C's real surviving pin now owns the last reference. *)
       within "drop restored baseline nonlast while child pin survives" (fun () ->
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 1 fault.attempts;
       [%test_eq: int] 1 fault.publications;
       graph_fault.marker
       <- Some
            (Filename.concat
               (Agent_store.Data_root.indexes_path
                  (Store.data_root (Daemon.store daemon)))
               "sessions.recovery-required");
       within "actual child main graph retains confirmed native row" (fun () ->
         Owner.with_loaded_runtime child.runtime (fun runtime ->
           (match
              Inference_client.Execution.complete_text
                runtime.A.Runtime_builder.inference_execution
                ~sw
                ~settings:[]
                ~messages:[ User, "645 actual child main graph cleanup" ]
                ()
            with
            | _ -> failwith "645 actual main completion ignored settlement rejection"
            | exception Agent_server.Graph_tracking.Rejected error ->
              [%test_eq: P.Error.code] Persistence_error error.code);
           Ok ())
         |> protocol_ok);
       assert (graph_fault.settlement_rejections > 0);
       [%test_eq: int] 0 graph_fault.finish_rejections;
       [%test_eq: int] 1 !providers;
       graph_fault.phase <- Quiet;
       let admitted = Warm.settled child in
       let row =
         Ledger.rows admitted.inference_ledger
         |> List.filter ~f:(fun row ->
           match Observation.Attempt_record.state (Ledger.Row.record row) with
           | Running -> true
           | Prepared | Terminal _ | Interrupted _ -> false)
         |> Goal_resume_runtime_tests.one_actual "645 actual child main graph row"
       in
       let actual_handle = Ledger.Row.handle row in
       let actual_marks = Inference_close_tests.marks admitted in
       let same_row () =
         let current = Warm.settled child in
         let row =
           Ledger.find
             current.inference_ledger
             ~ordinal:(Ledger.Handle.ordinal actual_handle)
           |> Option.value_exn
         in
         assert (Ledger.Handle.equal actual_handle (Ledger.Row.handle row));
         assert (
           A.Canonical_usage_fold.Marks.equal
             actual_marks
             (Inference_close_tests.marks current));
         row
       in
       let running () =
         match Observation.Attempt_record.state (Ledger.Row.record (same_row ())) with
         | Running -> ()
         | Prepared | Terminal _ | Interrupted _ ->
           failwith "645 failed child graph changed its actual running row"
       in
       fault.armed <- true;
       within "first actual last-pin persistence failure" (fun () ->
         match Owner.unload_and_wait child.runtime with
         | Ok () -> failwith "645 actual last pin failure was swallowed"
         | Error error ->
           raise_s
             [%sexp
               "645 last pin changed raised persistence failure", (error : P.Error.t)]
         | exception Core_unix.Unix_error (EIO, operation, path) ->
           [%test_eq: string] "635 native final cache publication" operation;
           assert (Cache_failure.temporary fault path));
       [%test_eq: int] 1 fault.failures;
       [%test_eq: int] 2 fault.attempts;
       [%test_eq: int] 1 fault.publications;
       assert (not (Owner.is_loaded child.runtime));
       Integration.no_lifecycle_replay after_policy (Warm.settled parent);
       [%test_eq: int] 1 graph_fault.finish_rejections;
       running ();
       graph_fault.phase <- Reject_finish_once;
       within "negative same child graph blocks actual pin retry" (fun () ->
         match Owner.unload_and_wait child.runtime with
         | Ok () -> failwith "645 failed child graph allowed successful pin repair"
         | Error error ->
           raise_s
             [%sexp
               "645 graph-negative retry changed original pin failure"
             , (error : P.Error.t)]
         | exception Core_unix.Unix_error (EIO, operation, path) ->
           [%test_eq: string] "635 native final cache publication" operation;
           assert (Cache_failure.temporary fault path));
       [%test_eq: int] 2 graph_fault.finish_rejections;
       [%test_eq: int] 2 fault.attempts;
       [%test_eq: int] 1 fault.publications;
       running ();
       graph_fault.phase <- Quiet;
       let first = ref None in
       let duplicate = ref None in
       protect_cleanup
         ~finally:(fun () ->
           (* Release BEFORE joining either cancellation-protected closer, even
              if the gate wait or any assertion raises. *)
           release_retry_gate ();
           Eio.Cancel.protect (fun () ->
             within "join actual concurrent public retry observers" (fun () ->
               Option.iter !first ~f:(fun task -> Eio.Promise.await_exn task |> ignore);
               Option.iter !duplicate ~f:(fun task ->
                 Eio.Promise.await_exn task |> ignore))))
         (fun () ->
            retry_armed := true;
            let attempt =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Existing.capture_load (fun () -> Owner.unload_and_wait child.runtime))
            in
            first := Some attempt;
            let reached =
              within "actual pin retry gate or premature result" (fun () ->
                Eio.Fiber.first
                  (fun () ->
                     Eio.Promise.await retry_entered;
                     `Entered)
                  (fun () -> `Returned (Eio.Promise.await_exn attempt)))
            in
            (match reached with
             | `Entered -> ()
             | `Returned outcome ->
               Existing.restore outcome;
               failwith "645 public retry returned without reaching actual cache gate");
            let started, start = Eio.Promise.create () in
            let observer =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Eio.Promise.resolve start ();
                Existing.capture_load (fun () -> Owner.unload_and_wait child.runtime))
            in
            duplicate := Some observer;
            Eio.Promise.await started;
            Eio.Fiber.yield ();
            assert (Option.is_none (Eio.Promise.peek attempt));
            assert (Option.is_none (Eio.Promise.peek observer));
            (* Outer IO gate precedes the inner cache attempt counter. Both
              public observers await the SAME real pending retirement attempt. *)
            [%test_eq: int] 2 fault.attempts;
            [%test_eq: int] 1 fault.publications;
            [%test_eq: int] 2 graph_fault.finish_rejections;
            release_retry_gate ();
            within "both public observers share successful factual retry" (fun () ->
              Eio.Promise.await_exn attempt |> Existing.restore;
              Eio.Promise.await_exn observer |> Existing.restore));
       [%test_eq: int] 2 graph_fault.finish_rejections;
       (match Observation.Attempt_record.state (Ledger.Row.record (same_row ())) with
        | Interrupted { reason = Host_interrupted; delivery = Response_started } -> ()
        | Prepared | Running | Terminal _ | Interrupted _ ->
          failwith "645 same child graph changed its actual native delivery");
       [%test_eq: int] 1 fault.failures;
       [%test_eq: int] 3 fault.attempts;
       [%test_eq: int] 2 fault.publications;
       assert (Owner.source_lane_idle parent.runtime);
       within "repeat joined cleanup without replay" (fun () ->
         Owner.unload_and_wait child.runtime |> protocol_ok;
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 3 fault.attempts;
       [%test_eq: int] 2 fault.publications;
       Integration.no_lifecycle_replay after_policy (Warm.settled parent);
       [%test_eq: int] 1 !providers);
  print_endline
    "actual last pin failed; failed child graph blocked retry; same graph and pin joined \
     once";
  [%expect
    {| actual last pin failed; failed child graph blocked retry; same graph and pin joined once |}]
;;

let%expect_test
    "failed Owned construction retains actual outer native source pin until repair"
  =
  let fault = Cache_failure.create () in
  let child_cache = ref None in
  let build_armed = ref false in
  let build_reads = ref 0 in
  let build_failure = Failure "648 actual child cache construction failure" in
  let build_trace = ref None in
  let pin_failure = Failure "648 actual outer native source pin cache failure" in
  let pin_trace = ref None in
  let entered, enter = Eio.Promise.create () in
  let released, release = Eio.Promise.create () in
  let release_build () =
    if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
  in
  let fail_pin _path =
    try raise pin_failure with
    | exn ->
      pin_trace := Some (Stdlib.Printexc.get_raw_backtrace ());
      Exn.raise_with_original_backtrace exn (Option.value_exn !pin_trace)
  in
  let hold_build _path =
    if !build_armed
    then (
      build_armed := false;
      Int.incr build_reads;
      Eio.Promise.resolve enter ();
      Eio.Cancel.protect (fun () -> Eio.Promise.await released);
      try raise build_failure with
      | exn ->
        build_trace := Some (Stdlib.Printexc.get_raw_backtrace ());
        Exn.raise_with_original_backtrace exn (Option.value_exn !build_trace))
  in
  let recording = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace true;
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
    ~f:(fun () ->
      Existing.with_child
        ~wrap_env:(fun env ->
          Crash_fault_io.wrap
            (Cache_failure.wrap ~fail:fail_pin fault env)
            ~boundary:Before_open_in
            ~matches:(fun path ->
              Option.exists !child_cache ~f:(fun cache -> String.equal cache path))
            ~reached:hold_build)
        (fun env sw daemon parent child _gate providers ->
           let within phase f =
             try Eio.Time.with_timeout_exn env#clock 15. f with
             | Eio.Time.Timeout ->
               failwith ("648 outer native source pin timed out: " ^ phase)
           in
           Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
           fault.cache
           <- Some
                (Filename.concat
                   (Store.Handle.cache_directory
                      (Option.value_exn parent.Registry.store_handle))
                   "cache.bin");
           child_cache
           := Some
                (Filename.concat
                   (Store.Handle.cache_directory
                      (Option.value_exn child.Registry.store_handle))
                   "cache.bin");
           within "close original child and parent before native preparation" (fun () ->
             Owner.unload_and_wait child.runtime |> protocol_ok;
             Owner.unload_and_wait parent.runtime |> protocol_ok);
           [%test_eq: int] 1 fault.attempts;
           [%test_eq: int] 1 fault.publications;
           assert (not (Owner.is_loaded parent.runtime));
           assert (not (Owner.is_loaded child.runtime));
           let child_file =
             Eio.Path.(Eio.Stdenv.fs env / Option.value_exn !child_cache)
           in
           assert (Eio.Path.is_file child_file);
           let original_child_cache = Eio.Path.load child_file in
           let before_native = Warm.settled parent in
           let attempted = ref None in
           protect_cleanup
             ~finally:(fun () ->
               (* Disarm and release BEFORE joining the protected constructor, even
              when it fails before reaching this exact cache boundary. Keep the
              original assertion/exception if factual teardown also refuses. *)
               build_armed := false;
               fault.armed <- false;
               release_build ();
               Eio.Cancel.protect (fun () ->
                 within "join constructor and close actual retained resources" (fun () ->
                   Option.iter !attempted ~f:(fun task ->
                     Eio.Promise.await_exn task |> ignore);
                   Owner.unload_and_wait child.runtime |> protocol_ok;
                   Owner.unload_and_wait parent.runtime |> protocol_ok)))
             (fun () ->
                build_armed := true;
                let attempt =
                  Eio.Fiber.fork_promise ~sw (fun () ->
                    Existing.capture_load (fun () -> Owner.ensure_loaded child.runtime))
                in
                attempted := Some attempt;
                (match
                   within
                     "actual child cache read or premature constructor result"
                     (fun () ->
                        Eio.Fiber.first
                          (fun () ->
                             Eio.Promise.await entered;
                             `Entered)
                          (fun () -> `Returned (Eio.Promise.await_exn attempt)))
                 with
                 | `Entered -> ()
                 | `Returned outcome ->
                   Existing.restore outcome;
                   failwith "648 child constructor returned before its actual cache gate");
                [%test_eq: int] 1 !build_reads;
                assert (Option.is_none (Eio.Promise.peek attempt));
                assert (not (Owner.is_loaded child.runtime));
                assert (not (Owner.is_loaded parent.runtime));
                Integration.no_lifecycle_replay before_native (Warm.settled parent);
                let phase = ref "648 preparing child's actual native policy adoption" in
                within
                  "adopt same native baseline while child construction holds pin"
                  (fun () ->
                     Cold_policy_resource_tests.run_standalone_policy
                       ~phase
                       env
                       daemon
                       parent
                       (Warm.settled parent)
                       ~original
                     |> ignore);
                let after_policy = Warm.settled parent in
                within "release restored baseline while preparing pin survives" (fun () ->
                  Owner.unload_and_wait parent.runtime |> protocol_ok);
                [%test_eq: int] 1 fault.attempts;
                [%test_eq: int] 1 fault.publications;
                [%test_eq: int] 0 fault.failures;
                assert (Option.is_none (Eio.Promise.peek attempt));
                [%test_eq: int] 0 !providers;
                fault.armed <- true;
                release_build ();
                (match
                   within
                     "failed original construction preserves actual child failure"
                     (fun () -> Eio.Promise.await_exn attempt)
                 with
                 | Owner.Scope_outcome.Raised (actual, trace) ->
                   assert (phys_equal actual build_failure);
                   let original_trace =
                     Stdlib.Printexc.raw_backtrace_to_string
                       (Option.value_exn !build_trace)
                   in
                   assert (not (String.is_empty original_trace));
                   assert (
                     String.is_prefix
                       (Stdlib.Printexc.raw_backtrace_to_string trace)
                       ~prefix:original_trace)
                 | Returned (Ok ()) ->
                   failwith "648 failed actual child construction returned a runtime"
                 | Returned (Error error) ->
                   raise_s
                     [%sexp
                       "648 child construction changed raised failure"
                     , (error : P.Error.t)]);
                [%test_eq: int] 1 fault.failures;
                [%test_eq: int] 2 fault.attempts;
                [%test_eq: int] 1 fault.publications;
                assert (
                  not
                    (String.is_empty
                       (Stdlib.Printexc.raw_backtrace_to_string
                          (Option.value_exn !pin_trace))));
                assert (not (Owner.is_loaded child.runtime));
                assert (not (Owner.is_loaded parent.runtime));
                [%test_eq: string] original_child_cache (Eio.Path.load child_file);
                [%test_eq: int] 0 !providers;
                Integration.no_lifecycle_replay after_policy (Warm.settled parent);
                (* The outer delegated source-selection pin spans the inner
                   Driver borrow. The inner pin closes nonlast; acknowledge its
                   positive child partial without claiming that it repaired the
                   parent's independently retained outer source loan. *)
                within
                  "acknowledge child partial without replaying native close"
                  (fun () -> Owner.unload_and_wait child.runtime |> protocol_ok);
                [%test_eq: int] 1 fault.failures;
                [%test_eq: int] 2 fault.attempts;
                [%test_eq: int] 1 fault.publications;
                within
                  "repair actual outer source pin before fresh child admission"
                  (fun () -> Owner.unload_and_wait parent.runtime |> protocol_ok);
                [%test_eq: int] 1 fault.failures;
                [%test_eq: int] 3 fault.attempts;
                [%test_eq: int] 2 fault.publications;
                within "repeat joined cleanup without physical replay" (fun () ->
                  Owner.unload_and_wait child.runtime |> protocol_ok;
                  Owner.unload_and_wait parent.runtime |> protocol_ok);
                [%test_eq: int] 3 fault.attempts;
                [%test_eq: int] 2 fault.publications;
                Integration.no_lifecycle_replay after_policy (Warm.settled parent);
                within
                  "fresh ordinary child load after actual source pin repair"
                  (fun () -> Owner.ensure_loaded child.runtime |> protocol_ok);
                assert (Owner.is_loaded child.runtime);
                assert (not (Owner.is_loaded parent.runtime));
                [%test_eq: int] 1 !build_reads;
                [%test_eq: int] 0 !providers)));
  print_endline
    "actual child build failed; outer native source loan repaired once before fresh load";
  [%expect
    {| actual child build failed; outer native source loan repaired once before fresh load |}]
;;

let%expect_test
    "actual warm preflight preserves install rejection through source cache cleanup \
     failure"
  =
  let module Ordinary = Ordinary_warm_resource_tests in
  let module R = Owner.Cleanup_recovery in
  let module CO = A.Close_observation in
  let module D = A.Cleanup_diagnostic in
  let io = Warm.Handoff_io.create () in
  let cache = Cache_failure.create () in
  let marker = ref None in
  let reject_install = ref false in
  let install_rejections = ref 0 in
  let outer_bodies = ref 0 in
  let outer_cleanup = ref None in
  let outer_report = ref None in
  let captures = ref 0 in
  let cache_failure = Failure "647 actual warm source cache cleanup" in
  let producer_trace = ref None in
  let body_completed = ref false in
  let body_failure = ref None in
  let wrap_env env =
    let env =
      Cache_failure.wrap
        ~fail:(fun _ ->
          [%test_eq: int] 1 !install_rejections;
          [%test_eq: int] 0 !outer_bodies;
          try raise cache_failure with
          | exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            producer_trace := Some backtrace;
            Exn.raise_with_original_backtrace exn backtrace)
        cache
        env
    in
    Crash_fault_io.wrap
      env
      ~boundary:Before_stat
      ~matches:(fun path ->
        !reject_install && Option.exists !marker ~f:(String.equal path))
      ~reached:(fun path ->
        reject_install := false;
        Int.incr install_rejections;
        raise (Core_unix.Unix_error (EIO, "647 warm install projection", path)))
  in
  let fixture () =
    try
      Ordinary.with_fixture
        ~wrap_env
        io
        (fun _env sw daemon entry first _initial _writer counters boundary ->
           let within name f = Ordinary.Boundary.within boundary name f in
           let loan = Ordinary.Resource_loan.create ~sw entry in
           let loading = ref None in
           let run_body () =
             protect_cleanup
               ~finally:(fun () ->
                 reject_install := false;
                 cache.armed <- false;
                 Warm.Handoff_io.release io;
                 Ordinary.Resource_loan.release loan;
                 Ordinary.Boundary.join_all
                   boundary
                   [ ( "join actual warm source selection"
                     , fun () ->
                         Option.iter !loading ~f:(fun task ->
                           match Eio.Promise.await_exn task with
                           | Owner.Scope_outcome.Returned (Error _) -> ()
                           | outcome ->
                             Existing.restore outcome;
                             failwith "647 source cleanup lost install refusal") )
                   ; ( "join original Product resource observer"
                     , fun () -> Ordinary.Resource_loan.join loan )
                   ])
               (fun () ->
                  let retained =
                    within "retain actual initial Product" (fun () ->
                      Ordinary.Resource_loan.await loan)
                  in
                  assert (phys_equal first.worker retained.worker);
                  let identity = Warm.identity first in
                  let ports = !(counters.inference_ports) in
                  within "detach binding under actual Product loan" (fun () ->
                    Owner.unload_and_wait entry.runtime |> protocol_ok);
                  assert (not (Owner.is_loaded entry.runtime));
                  let before = Warm.settled entry in
                  marker
                  := Some
                       (Filename.concat
                          (Agent_store.Data_root.indexes_path
                             (Store.data_root (Daemon.store daemon)))
                          "sessions.recovery-required");
                  cache.cache <- io.cache;
                  io.armed <- true;
                  let selected =
                    Eio.Fiber.fork_promise ~sw (fun () ->
                      Eio.Fiber.with_binding io.caller () (fun () ->
                        Existing.capture_load (fun () ->
                          let report =
                            Owner.with_delegation_resources_observed
                              entry.runtime
                              ~capture:(fun cleanup ->
                                Int.incr captures;
                                assert (Option.is_none !outer_cleanup);
                                outer_cleanup := Some cleanup)
                              (fun _ ->
                                 Int.incr outer_bodies;
                                 CO.create (Returned (Ok ())) ~diagnostics:[] ())
                          in
                          outer_report := Some report;
                          Owner.Scope_outcome.resume (Owner.Lease_outcome.outcome report))))
                  in
                  loading := Some selected;
                  within "actual Product pinned before verified read" (fun () ->
                    Ordinary.await_load io selected);
                  [%test_eq: int] 1 !captures;
                  let cleanup = Option.value_exn !outer_cleanup in
                  assert (Option.is_none (R.original cleanup |> protocol_ok));
                  assert (Option.is_none (R.snapshot cleanup |> protocol_ok));
                  Ordinary.Resource_loan.release loan;
                  within "original observer leaves source-owned Product" (fun () ->
                    Ordinary.Resource_loan.join loan);
                  assert (Option.is_none (Eio.Promise.peek selected));
                  [%test_eq: int] 1 io.reads;
                  [%test_eq: int] 0 io.cache_renames;
                  [%test_eq: int] 0 cache.attempts;
                  [%test_eq: int] 0 !install_rejections;
                  assert (not (Owner.source_lane_idle entry.runtime));
                  reject_install := true;
                  cache.armed <- true;
                  Warm.Handoff_io.release io;
                  let outcome =
                    within "install refusal joins actual source cache failure" (fun () ->
                      Eio.Promise.await_exn selected)
                  in
                  let install_error =
                    match outcome with
                    | Owner.Scope_outcome.Returned (Error error) ->
                      [%test_eq: P.Error.code] Persistence_error error.code;
                      (* Factory's original before_commit Store mapping is
                        non-retryable; the primary is preserved unchanged. *)
                      assert (not error.retryable);
                      assert (
                        String.is_substring
                          error.message
                          ~substring:"647 warm install projection");
                      assert (
                        String.is_substring
                          error.message
                          ~substring:(Option.value_exn !marker));
                      error
                    | Raised (exn, backtrace) ->
                      Exn.raise_with_original_backtrace exn backtrace
                    | Returned (Ok ()) ->
                      failwith "647 failed warm install reached outer resource body"
                  in
                  let report = Option.value_exn !outer_report in
                  assert (phys_equal cleanup (Owner.Lease_outcome.cleanup report));
                  let original = R.original cleanup |> protocol_ok |> Option.value_exn in
                  let snapshot = R.snapshot cleanup |> protocol_ok |> Option.value_exn in
                  let original_view =
                    R.original_view cleanup |> protocol_ok |> Option.value_exn
                  in
                  let cache_group =
                    match
                      Agent_server.Cleanup_original_view.diagnostics original_view
                    with
                    | [ group ] ->
                      (match D.failures group with
                       | [ Raised (actual, _) ] ->
                         assert (phys_equal actual cache_failure)
                       | _ -> failwith "warm original view lost actual cache producer");
                      group
                    | _ -> failwith "warm original view duplicated actual cache producer"
                  in
                  let paired = Owner.Lease_outcome.observation report in
                  let install_group = Option.value_exn (CO.primary paired) in
                  (match CO.exit paired with
                   | Returned (Error actual) -> assert (phys_equal actual install_error)
                   | Returned (Ok ()) | Raised _ ->
                     failwith "warm observed primary changed");
                  (match D.failures install_group with
                   | [ Protocol actual ] -> assert (phys_equal actual install_error)
                   | _ -> failwith "warm observed primary is not actual install refusal");
                  assert (not (D.equal install_group cache_group));
                  assert (
                    List.equal
                      D.equal
                      (CO.diagnostics paired)
                      [ install_group; cache_group ]);
                  assert (
                    R.Snapshot.equal_progress (R.Snapshot.progress snapshot) Retryable);
                  (match R.Snapshot.failures snapshot with
                   | [ Raised (actual, backtrace) ] ->
                     assert (phys_equal actual cache_failure);
                     let expected =
                       Stdlib.Printexc.raw_backtrace_to_string
                         (Option.value_exn !producer_trace)
                     in
                     assert (not (String.is_empty expected));
                     assert (
                       String.is_prefix
                         (Stdlib.Printexc.raw_backtrace_to_string backtrace)
                         ~prefix:expected);
                     (match Owner.Scope_outcome.Cleanup.status original with
                      | Failed (Raised (original_failure, original_backtrace), _) ->
                        assert (phys_equal original_failure cache_failure);
                        assert (phys_equal original_backtrace backtrace)
                      | Failed (Protocol _, _) | Never_acquired | Joined ->
                        failwith "outer original lost actual preflight source cleanup")
                   | _ ->
                     failwith "outer holder did not retain exact pre-Borrow cache failure");
                  assert (
                    phys_equal
                      original
                      (R.original cleanup |> protocol_ok |> Option.value_exn));
                  assert (
                    phys_equal
                      snapshot
                      (R.snapshot cleanup |> protocol_ok |> Option.value_exn));
                  [%test_eq: int] 1 !captures;
                  [%test_eq: int] 1 !install_rejections;
                  [%test_eq: int] 1 cache.failures;
                  [%test_eq: int] 1 cache.attempts;
                  [%test_eq: int] 0 cache.publications;
                  [%test_eq: int] 0 io.cache_renames;
                  [%test_eq: int] 0 !outer_bodies;
                  [%test_eq: int] ports !(counters.inference_ports);
                  [%test_eq: int] 0 !(counters.providers);
                  assert (not (Owner.is_loaded entry.runtime));
                  assert (not (Owner.source_lane_idle entry.runtime));
                  assert (Jsonaf.exactly_equal identity (Warm.identity first));
                  Source_preparation_tests.assert_same before entry;
                  assert (
                    Store.owns_handle
                      (Daemon.store daemon)
                      (Option.value_exn entry.store_handle));
                  body_completed := true)
           in
           match run_body () with
           | () -> ()
           | exception exn ->
             let backtrace = Stdlib.Printexc.get_raw_backtrace () in
             body_failure := Some (exn, backtrace);
             Exn.raise_with_original_backtrace exn backtrace);
      Ok ()
    with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let fixture =
    let previous_backtraces = Stdlib.Printexc.backtrace_status () in
    Exn.protect
      ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtraces)
      ~f:(fun () ->
        Stdlib.Printexc.record_backtrace true;
        fixture ())
  in
  if not !body_completed
  then (
    match !body_failure, fixture with
    | Some (exn, backtrace), _ | None, Error (exn, backtrace) ->
      Exn.raise_with_original_backtrace exn backtrace
    | None, Ok () -> failwith "647 genuine warm source assertions did not run");
  (* Ordinary Product close may retain the actual failed outcome. Teardown can
     reproject only that original cache failure, never an unrelated leaf. *)
  let rec only_actual exn =
    if phys_equal exn cache_failure
    then true
    else (
      match exn with
      | Eio.Cancel.Cancelled cause -> only_actual cause
      | Eio.Exn.Multiple failures ->
        (not (List.is_empty failures))
        && List.for_all failures ~f:(fun (failure, _) -> only_actual failure)
      | Exn.Finally (primary, cleanup) -> only_actual primary && only_actual cleanup
      | _ -> false)
  in
  (match fixture with
   | Error (exn, _) when only_actual exn -> ()
   | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
   | Ok () -> ());
  print_endline
    "actual Product warm install rejected; source last release failed cache; primary \
     preserved without outer work";
  [%expect
    {| actual Product warm install rejected; source last release failed cache; primary preserved without outer work |}]
;;
