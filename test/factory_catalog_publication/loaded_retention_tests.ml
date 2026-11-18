open! Core
open Agent_server_test_support
module P = Agent_protocol
module Store = Agent_store
module Actor = Agent_session.Session_actor
module Daemon = Agent_server.Daemon
module Factory = Agent_server.Session_factory
module Registry = Agent_server.Session_registry
module Creation = Creation_tests

module Control = struct
  type gate =
    { entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; released : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    }

  type t =
    { native : gate option Atomic.t
    ; mutable backoff : gate option
    ; mutable gates : gate list
    ; mutable current : string option
    ; mutable fail_current : bool
    ; mutable read_failures : int
    ; active : int Atomic.t
    ; finished : int Atomic.t
    }

  let create () =
    { native = Atomic.make None
    ; backoff = None
    ; gates = []
    ; current = None
    ; fail_current = false
    ; read_failures = 0
    ; active = Atomic.make 0
    ; finished = Atomic.make 0
    }
  ;;

  let gate t =
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    let gate = { entered; enter; released; release } in
    t.gates <- gate :: t.gates;
    gate
  ;;

  let release gate =
    if not (Eio.Promise.is_resolved gate.released)
    then Eio.Promise.resolve gate.release ()
  ;;

  let hold gate =
    Eio.Promise.resolve gate.enter ();
    Eio.Promise.await gate.released
  ;;

  let arm_native t =
    assert (Option.is_none (Atomic.get t.native));
    let gate = gate t in
    Atomic.set t.native (Some gate);
    gate
  ;;

  let arm_backoff t =
    assert (Option.is_none t.backoff);
    let gate = gate t in
    t.backoff <- Some gate;
    gate
  ;;

  let release_all t =
    Atomic.set t.native None;
    t.backoff <- None;
    t.fail_current <- false;
    List.iter t.gates ~f:release
  ;;

  let domain_manager t manager =
    let run manager operation =
      let (Eio.Resource.T (data, ops)) = manager in
      let module Native = (val Eio.Resource.get ops Eio.Domain_manager.Pi.Mgr) in
      Native.run data (fun ~cancelled ->
        ignore (Atomic.fetch_and_add t.active 1 : int);
        Exn.protect
          ~finally:(fun () ->
            ignore (Atomic.fetch_and_add t.active (-1) : int);
            ignore (Atomic.fetch_and_add t.finished 1 : int))
          ~f:(fun () ->
            (* Run the actual fixed recovery operation on the actual native
               worker, retaining its permit and join while its return is held. *)
            let result = operation ~cancelled in
            (match Atomic.exchange t.native None with
             | None -> ()
             | Some gate ->
               Eio.Cancel.protect (fun () -> hold gate);
               (match Eio.Promise.peek cancelled with
                | None -> ()
                | Some cancellation -> raise cancellation));
            result))
    in
    Eio.Resource.T
      ( manager
      , Eio.Domain_manager.Pi.mgr
          (module struct
            type t = Eio.Domain_manager.ty Eio.Resource.t

            let run = run
            let run_raw = Eio.Domain_manager.run_raw
          end) )
  ;;

  let wrap_env t env =
    let real_mono = Eio.Stdenv.mono_clock env in
    let last_now = ref None in
    let module Clock = struct
      type t = unit
      type time = Mtime.t

      let now () =
        let actual = Eio.Time.Mono.now real_mono in
        last_now := Some actual;
        actual
      ;;

      let sleep_until () deadline =
        let retention_backoff =
          Option.exists !last_now ~f:(fun actual ->
            Option.exists (Mtime.add_span actual Mtime.Span.s) ~f:(fun expected ->
              Mtime.equal deadline expected))
        in
        last_now := None;
        (* Eio.Mono.sleep computes this exact deadline without yielding. In this
           one-entry idle fixture retention is the only one-second caller. *)
        if retention_backoff
        then (
          let gate = t.backoff in
          t.backoff <- None;
          Option.iter gate ~f:hold);
        Eio.Time.Mono.sleep_until real_mono deadline
      ;;
    end
    in
    let mono_clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)) in
    let domain_mgr = domain_manager t (Eio.Stdenv.domain_mgr env) in
    let observed =
      object
        method fs = env#fs
        method cwd = env#cwd
        method stdin = env#stdin
        method stdout = env#stdout
        method stderr = env#stderr
        method net = env#net
        method domain_mgr = domain_mgr
        method process_mgr = env#process_mgr
        method clock = env#clock
        method mono_clock = mono_clock
        method secure_random = env#secure_random
        method debug = env#debug
        method backend_id = env#backend_id
      end
    in
    Crash_fault_io.wrap
      observed
      ~boundary:Before_open_in
      ~matches:(fun path -> Option.exists t.current ~f:(String.equal path))
      ~reached:(fun path ->
        if t.fail_current
        then (
          t.fail_current <- false;
          t.read_failures <- t.read_failures + 1;
          raise (Core_unix.Unix_error (EIO, "631 preflight CURRENT read", path))))
  ;;
end

let state (entry : Registry.entry) = Actor.state entry.actor |> protocol_ok

let update entry attachment name =
  let before = state entry in
  let patch =
    P.Session_metadata.Patch.create ~name:(Set name) ~set_labels:[] ~remove_labels:[]
    |> protocol_ok
  in
  Actor.update_metadata
    entry.Registry.actor
    ~attachment_id:attachment
    ~expected_metadata_revision:before.identity.metadata_revision
    ~patch
    ()
  |> protocol_ok
  |> ignore;
  let after = state entry in
  [%test_eq: int64]
    (Int64.succ before.identity.metadata_revision)
    after.identity.metadata_revision;
  [%test_eq: string option] (Some name) after.identity.display_name;
  after
;;

let inventory env directory =
  Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)
  |> List.sort ~compare:String.compare
  |> List.map ~f:(fun name ->
    name, Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / directory / name))
;;

let snapshot_count files =
  List.count files ~f:(fun (name, _) ->
    String.is_prefix name ~prefix:"snapshot-" && String.is_suffix name ~suffix:".bin")
;;

let await_pruned env directory =
  let rec await () =
    let count =
      Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)
      |> List.count ~f:(fun name ->
        String.is_prefix name ~prefix:"snapshot-" && String.is_suffix name ~suffix:".bin")
    in
    if count > 2
    then (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      await ())
    else [%test_eq: int] 2 count
  in
  await ()
;;

let with_entry f =
  let control = Control.create () in
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      failwith "loaded retention dispatched a provider")
  in
  Creation.with_daemon
    ~snapshot_every_events:1
    ~wrap_env:(Control.wrap_env control)
    ~policy
    (fun env sw _root daemon ->
       (* Release inside the Switch body, before Daemon/worker/domain joins, even
         when the bounded progress oracle cancels its caller on the old code. *)
       Creation.protect_cleanup
         ~finally:(fun () -> Control.release_all control)
         (fun () ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
              let first_backoff = Control.arm_backoff control in
              let entry =
                Factory.create_session
                  (Daemon.factory daemon)
                  ~command_audit:None
                  ~principal:(principal ())
                  (create_request ~start_immediately:true ())
                |> protocol_ok
              in
              let handle =
                match entry.store_handle with
                | Some handle -> handle
                | None ->
                  Eio.Cancel.protect (fun () ->
                    Agent_server.Session_registry.close_entry entry);
                  failwith "loaded retention has no actual Store Handle"
              in
              let session_id = Store.Session_store.Handle.session_id handle in
              Registry.close_provisional
                (Daemon.registry daemon)
                ~session_id
                entry
                (fun () -> Registry.add (Daemon.registry daemon) ~session_id entry)
              |> protocol_ok;
              let attachment, _ =
                Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
              in
              let snapshots = Store.Session_store.Handle.snapshot_directory handle in
              let journal = Store.Session_store.Handle.journal_directory handle in
              control.current <- Some (Filename.concat snapshots "CURRENT");
              Registry.with_lifecycle (Daemon.registry daemon) session_id (fun _ ->
                for index = 1 to 3 do
                  ignore
                    (update entry attachment.id ("retention setup " ^ Int.to_string index)
                     : Agent_session.Session_state.t)
                done;
                assert (snapshot_count (inventory env snapshots) >= 3);
                Eio.Promise.await first_backoff.entered;
                Ok ())
              |> protocol_ok;
              f
                env
                sw
                daemon
                control
                entry
                handle
                attachment.id
                snapshots
                journal
                first_backoff;
              [%test_eq: int] 0 !providers)))
;;

let%test_unit
    "loaded retention warms outside Actor, defers a changed basis and joins before close"
  =
  with_entry
    (fun env sw _daemon control entry handle attachment snapshots journal first_backoff ->
       let native = Control.arm_native control in
       Control.release first_backoff;
       Eio.Promise.await native.entered;
       [%test_eq: int] 1 (Atomic.get control.active);
       (* These real requests must ACK before releasing the actual native worker.
         The previous implementation holds their Actor mailbox throughout. *)
       let acknowledged = update entry attachment "ACK during outside retention" in
       let snapshot_bytes = inventory env snapshots in
       let journal_bytes = inventory env journal in
       assert (snapshot_count snapshot_bytes >= 3);
       let stale_backoff = Control.arm_backoff control in
       Control.release native;
       Eio.Promise.await stale_backoff.entered;
       [%test_eq: (string * string) list] snapshot_bytes (inventory env snapshots);
       [%test_eq: (string * string) list] journal_bytes (inventory env journal);
       [%test_eq: int64]
         acknowledged.identity.metadata_revision
         (state entry).identity.metadata_revision;
       Control.release stale_backoff;
       await_pruned env snapshots;
       [%test_eq: string option]
         (Some "ACK during outside retention")
         (state entry).identity.display_name;
       let closing_native = Control.arm_native control in
       ignore
         (update entry attachment "ACK before actual close"
          : Agent_session.Session_state.t);
       Eio.Promise.await closing_native.entered;
       let workers_finished = Atomic.get control.finished in
       let close_started, start_close = Eio.Promise.create () in
       let closed, finish_close = Eio.Promise.create () in
       Eio.Fiber.fork ~sw (fun () ->
         Eio.Promise.resolve start_close ();
         let outcome =
           match Agent_server.Session_registry.close_entry entry with
           | () -> Ok ()
           | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
         in
         Eio.Promise.resolve finish_close outcome);
       Eio.Promise.await close_started;
       Eio.Fiber.yield ();
       assert (not (Eio.Promise.is_resolved closed));
       assert (Result.is_ok (Store.Session_store.Handle.metadata_checked handle));
       Control.release closing_native;
       (match Eio.Promise.await closed with
        | Ok () -> ()
        | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
       assert (Atomic.get control.finished > workers_finished);
       [%test_eq: int] 0 (Atomic.get control.active);
       assert (Result.is_error (Store.Session_store.Handle.metadata_checked handle));
       assert (Result.is_error (Actor.state entry.actor));
       Agent_server.Session_registry.close_entry entry)
;;

let%test_unit "loaded retention preserves a same-basis warm error before retry" =
  with_entry
    (fun
        env
         _sw
         _daemon
         control
         entry
         _handle
         _attachment
         snapshots
         journal
         first_backoff
       ->
       let snapshot_bytes = inventory env snapshots in
       let journal_bytes = inventory env journal in
       let unchanged = state entry in
       let failed_backoff = Control.arm_backoff control in
       control.fail_current <- true;
       Control.release first_backoff;
       Eio.Promise.await failed_backoff.entered;
       [%test_eq: int] 1 control.read_failures;
       assert (not control.fail_current);
       assert (phys_equal unchanged (state entry));
       [%test_eq: (string * string) list] snapshot_bytes (inventory env snapshots);
       [%test_eq: (string * string) list] journal_bytes (inventory env journal);
       (* The one-shot real read now succeeds. Rerunning and swallowing the first
         Error would have pruned and skipped this failed-attempt backoff. *)
       Control.release failed_backoff;
       await_pruned env snapshots;
       assert (phys_equal unchanged (state entry)))
;;
