open! Core
open Agent_server_test_support
open Retry_support
module Existing = Delegated_load_tests
module Warm = Delegated_warm_resource_tests
module Integration = Native_resource_integration_tests
module Store = Agent_store
module Persistence_fault = Legacy_idle_completion_fault_tests

module Reservation_fault = struct
  type t =
    { mutable selected : A.Session_state.t option
    ; mutable hits : int
    ; mutable path : string option
    ; mutable injected : exn option
    ; mutable transaction : Store.Transaction.t option
    }

  let create () =
    { selected = None; hits = 0; path = None; injected = None; transaction = None }
  ;;

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_sync
      ~matches:(fun path ->
        Option.is_some t.selected
        && String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(fun path ->
        match t.selected with
        | None -> ()
        | Some before ->
          let scan =
            Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
            |> Store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
            |> Source_preparation_tests.store_ok
          in
          (match List.last scan.entries with
           | Some frame when Int.equal (Store.Frame.flags frame.frame) 0 ->
             let transaction =
               Store.Transaction.decode (Store.Frame.payload frame.frame)
               |> Source_preparation_tests.store_ok
             in
             if P.Id.Session.equal transaction.session_id before.identity.session_id
             then (
               let delta =
                 A.Session_delta_document.decode
                   ~limits:Document_schema.Limits.default
                   transaction.delta
                 |> document_ok
                 |> A.Session_delta_document.value
               in
               match Child_usage_fault_tests.Fault.leaves delta with
               | [ A.Session_delta.History_block_reserved reserved ] ->
                 [%test_eq: int64]
                   Int64.(
                     before.conversation.next_history_sequence
                     + of_int
                         Daemon.default_options.factory_limits.moderator_reservation_size)
                   reserved;
                 [%test_eq: int64]
                   (Int64.succ before.counters.transaction_sequence)
                   transaction.transaction_sequence;
                 [%test_eq: int64]
                   (Int64.succ before.counters.revision)
                   transaction.session_revision;
                 [%test_eq: int] before.identity.generation transaction.generation;
                 assert (List.is_empty transaction.durable_events);
                 assert (Option.is_none transaction.first_event_sequence);
                 assert (Option.is_none transaction.last_event_sequence);
                 let injected =
                   Core_unix.Unix_error
                     (EIO, "635 actual promotion history reservation", path)
                 in
                 t.selected <- None;
                 t.hits <- t.hits + 1;
                 t.path <- Some path;
                 t.injected <- Some injected;
                 t.transaction <- Some transaction;
                 raise injected
               | _ -> ())
           | None | Some _ -> ()))
  ;;
end

let%expect_test "actual prepublication reservation failure retains the native borrower" =
  let fault = Reservation_fault.create () in
  let io = Warm.Handoff_io.create () in
  let selected = ref None in
  let native_preparations = ref 0 in
  Existing.with_child
    ~wrap_env:(fun env -> Reservation_fault.wrap fault (Warm.Handoff_io.wrap io env))
    ~on_inference_ports:(fun actor ->
      if Option.exists !selected ~f:(phys_equal actor) then Int.incr native_preparations)
    (fun env _sw daemon parent child _gate providers ->
       let within phase f =
         try Eio.Time.with_timeout_exn env#clock 10. f with
         | Eio.Time.Timeout -> failwith ("635 reservation failure timed out: " ^ phase)
       in
       Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
       let initial = Warm.loaded parent in
       io.cache
       <- Some
            (Filename.concat
               (Store.Session_store.Handle.cache_directory
                  (Option.value_exn parent.Registry.store_handle))
               "cache.bin");
       within "close original child and parent" (fun () ->
         Owner.unload_and_wait child.runtime |> protocol_ok;
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 1 io.cache_renames;
       (match A.Prompt_source_lease.availability initial.source_lease with
        | Closed -> ()
        | Missing_receipt | Live | Unavailable _ | Revoked ->
          failwith "original parent source did not close before native-first setup");
       selected := Some parent.actor;
       within "retain actual native-only parent through real Owned child" (fun () ->
         Owner.ensure_loaded child.runtime |> protocol_ok);
       assert (not (Owner.is_loaded parent.runtime));
       assert (Owner.is_loaded child.runtime);
       [%test_eq: int] 1 !native_preparations;
       let borrower = Warm.loaded child in
       Ordinary_warm_resource_tests.assert_live_source borrower;
       let before = Warm.settled parent in
       [%test_eq: int] 1 (List.length (Warm.completed before Session_start));
       [%test_eq: int] 0 !providers;
       Creation_tests.protect_cleanup
         ~finally:(fun () ->
           fault.selected <- None;
           if fault.hits > 0
           then (
             within "release actual B before poisoned parent final cleanup" (fun () ->
               Owner.unload_and_wait child.runtime |> protocol_ok);
             within "strict poisoned parent cleanup" (fun () ->
               Uncertain_writer_test_support.close_uncertain_entry
                 parent
                 ~env
                 ~store:(Daemon.store daemon))))
         (fun () ->
            (* select_for_product reserves M before cold preparation reaches
               this real reservation. No Manager attachment has been allocated. *)
            fault.selected <- Some before;
            let primary =
              within "actual M reservation failure before Product publication" (fun () ->
                match Owner.ensure_loaded parent.runtime with
                | Error error -> error
                | Ok () -> failwith "actual reservation failure published a Product")
            in
            [%test_eq: int] 1 fault.hits;
            let expected =
              Store.Store_error.of_exn
                ~operation:"append journal segment"
                ~path:(Option.value_exn fault.path)
                (Option.value_exn fault.injected)
              |> Persistence_fault.persistence_error
            in
            Persistence_fault.same_error expected primary;
            ignore (Option.value_exn fault.transaction : Store.Transaction.t);
            assert (not (Owner.is_loaded parent.runtime));
            assert (Owner.is_loaded child.runtime);
            [%test_eq: int] 1 !native_preparations;
            [%test_eq: int] 1 io.cache_renames;
            [%test_eq: int] 0 !providers;
            Integration.no_lifecycle_replay before (Warm.settled parent);
            let check_failed_attachment = function
              | Error (error : P.Error.t) ->
                [%test_eq: P.Error.code] Conflict error.code;
                [%test_eq: bool] true error.retryable;
                [%test_eq: string]
                  "native attachment construction or cleanup remains owned"
                  error.message
              | Ok _ -> failwith "failed promotion accepted retained resource access"
            in
            check_failed_attachment
              (Owner.with_loaded_runtime child.runtime (fun _ ->
                 failwith "failed promotion permitted a fresh guarded child borrow"));
            let sources = Option.value_exn borrower.retained_ancestor_sources in
            check_failed_attachment
              (sources.with_source before.identity.session_id ~f:(fun ~source:_ ->
                 failwith "failed promotion provided its retained Source"));
            (* This is the actual view captured while B was admitted, not a
               new grant. Its source stays live until the real B release below. *)
            Ordinary_warm_resource_tests.assert_live_source borrower;
            within "release B while actual A still retains N" (fun () ->
              Owner.unload_and_wait child.runtime |> protocol_ok);
            assert (not (Owner.is_loaded child.runtime));
            [%test_eq: int] 1 io.cache_renames);
       [%test_eq: int] 1 fault.hits;
       [%test_eq: int] 1 !native_preparations;
       [%test_eq: int] 2 io.cache_renames;
       [%test_eq: int] 0 !providers);
  print_endline
    "actual prepublication history failure preserved; B stayed live; A retained N until \
     strict poisoned-owner cleanup";
  [%expect
    {| actual prepublication history failure preserved; B stayed live; A retained N until strict poisoned-owner cleanup |}]
;;
