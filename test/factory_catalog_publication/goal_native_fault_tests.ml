open! Core
open Agent_server_test_support
open Retry_support
open Goal_native_test_support
module Checks = Goal_native_resume_tests
module Fault = Goal_native_fault_support
module Intent = P.Goal_native_resume_intent
module Native = Actor.Goal_native_resume

let await_no_goal_provider counters resumed_entered =
  [%test_eq: int] 2 !(counters.providers);
  assert (Option.is_none (Eio.Promise.peek resumed_entered))
;;

let status_after_reopen recovered loaded runtime invocation =
  await_activation loaded runtime;
  let ordinary_receipt = ordinary ~key:"native-goal-recovered-status" recovered loaded in
  let rec owner () =
    let current = state loaded in
    match current.active_operation with
    | Some operation ->
      let run =
        A.Run_state.find (Option.value_exn current.run_state) ordinary_receipt.run_id
        |> Option.value_exn
      in
      assert (
        List.exists run.owned_work ~f:(fun work ->
          P.Run_work.Key.equal work.key (Operation operation.id)));
      operation
    | None ->
      Eio.Fiber.yield ();
      owner ()
  in
  Checks.status loaded runtime (owner ()) invocation.P.Invocation.context.id
;;

(* The selected writer is poisoned while an actual Operation still owns its
   terminal checkpoint. A hard child exit tests crash recovery honestly; this
   does not claim a failed graceful Owner cleanup joined successfully. *)
let%expect_test "native Complete unknown ACK cannot accept or dispatch a callback action" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      let path = Eio.Path.(Eio.Stdenv.fs env / root) in
      Creation_tests.protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree path)
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
             let report =
               Eio.Switch.run (fun child_sw ->
                 let manager = Eio.Stdenv.process_mgr env in
                 let output, sink = Eio.Process.pipe ~sw:child_sw manager in
                 let diagnostics, errors = Eio.Process.pipe ~sw:child_sw manager in
                 let child =
                   Eio.Process.spawn
                     ~sw:child_sw
                     manager
                     ~env:[| "PATH=/usr/bin:/bin" |]
                     ~stdout:sink
                     ~stderr:errors
                     [ "./goal_native_fault_child.exe"; root ]
                 in
                 Eio.Flow.close sink;
                 Eio.Flow.close errors;
                 let report, diagnostics =
                   Eio.Fiber.pair
                     (fun () ->
                        Eio.Buf_read.take_all
                          (Eio.Buf_read.of_flow ~max_size:16384 output)
                        |> Jsonaf.of_string)
                     (fun () ->
                        Eio.Buf_read.take_all
                          (Eio.Buf_read.of_flow ~max_size:8192 diagnostics))
                 in
                 (match Eio.Process.await child with
                  | `Exited 73 -> ()
                  | `Exited code ->
                    raise_s [%sexp "native fault child failed", (code : int)]
                  | `Signaled signal ->
                    raise_s [%sexp "native fault child signaled", (signal : int)]);
                 (* The poisoned writer can race its queued cleanup warning
                    against the deliberate hard exit. Only that exact warning
                    is permitted; parent recovery diagnostics remain visible. *)
                 let expected_warning =
                   "+queued moderator cleanup failed: (Corrupt\n\
                    + \"canonical projection target cannot regress or alter \
                    equal-revision hints\")"
                 in
                 let diagnostics = String.strip diagnostics in
                 if
                   not
                     (String.is_empty diagnostics
                      || String.equal diagnostics expected_warning)
                 then
                   raise_s
                     [%sexp
                       "unexpected native fault child stderr", (diagnostics : string)];
                 report)
             in
             let primary =
               Jsonaf.member_exn "primary" report |> P.Error.of_json |> protocol_ok
             in
             [%test_eq: P.Error.code] Persistence_error primary.code;
             let session_id =
               Jsonaf.member_exn "session_id" report
               |> P.Id.Session.of_json
               |> protocol_ok
             in
             let invocation_id =
               Jsonaf.member_exn "invocation_id" report
               |> P.Id.Invocation.of_json
               |> protocol_ok
             in
             let providers = ref 0 in
             let policy =
               inference_policy
                 ~default_model:"fixture-model"
                 ~post_stream:(fun ~sw:_ ~inputs:_ ->
                   incr providers;
                   completed_response "unexpected recovered native Goal provider")
             in
             let recovered =
               Daemon.start
                 ~sw
                 ~env:(env :> Eio_unix.Stdenv.base)
                 ~config:(config root root (Filename.concat root "parent.chatmd"))
                 ~tool_dir:root
                 ~home:root
                 ~process_start_identity:None
                 ~options:
                   { Daemon.default_options with
                     inference_policy = policy
                   ; qualify_chatml_extensions = true
                   }
                 ()
               |> protocol_ok
             in
             Creation_tests.protect_cleanup
               ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
               (fun () ->
                  let loaded =
                    Registry.load (Daemon.registry recovered) session_id |> protocol_ok
                  in
                  Owner.ensure_loaded_authenticated loaded.runtime ~actor:original
                  |> protocol_ok;
                  let current =
                    Actor.with_moderator_checkpoint loaded.actor (fun () ->
                      Actor.state loaded.actor)
                    |> protocol_ok
                  in
                  assert (
                    List.exists current.invocations ~f:(fun invocation ->
                      P.Id.Invocation.equal
                        invocation.P.Invocation.context.id
                        invocation_id));
                  Checks.no_accepted loaded;
                  assert (
                    Option.is_none
                      (Actor.select_native_goal_resume loaded.actor |> protocol_ok));
                  [%test_eq: int] 0 !providers)))));
  print_endline
    "actual native Complete writer fault; no Accepted receipt or recovered command \
     custody";
  [%expect
    {| actual native Complete writer fault; no Accepted receipt or recovered command custody |}]
;;

let%expect_test
    "unknown Goal writer retains claim uncertainty and never replays from recovered DATA"
  =
  let fault = Fault.create Goal_command_unknown in
  with_callback
    ~v1:true
    ~wrap_env:(Fault.wrap fault)
    (fun
        _env
         _daemon
         entry
         runtime
         counters
         _session
         _admission
         _old_live
         _held
         request
         operation
         ~release_ordinary
         ~resumed_entered
         ~release_resumed
         ~reopen
       ->
       let result, invocations =
         callback entry runtime ~operation ~mode:"resume" ~run:(fun admission ->
           Actor.stage_native_goal_resume entry.actor ~admission ~authorize ~request)
       in
       assert (result |> protocol_ok);
       let invocation = List.hd_exn invocations in
       Fault.arm fault entry request;
       release_ordinary ();
       ignore (Fault.await fault : Agent_store.Transaction.t);
       (* The real scheduler owns the domain error; this is not a returned-primary
         assertion. The actual selected transaction proves the uncertain writer. *)
       (match Intent.phase (Checks.native_intent entry invocation) with
        | Dispatch_claimed _ -> ()
        | _ -> assert false);
       await_no_goal_provider counters resumed_entered;
       reopen (fun recovered loaded actual_runtime ->
         assert (
           Option.is_none (Actor.select_native_goal_resume loaded.actor |> protocol_ok));
         await_no_goal_provider counters resumed_entered;
         let status = status_after_reopen recovered loaded actual_runtime invocation in
         (match status with
          | Native.Status.Unknown _ ->
            let receipt =
              A.Goal_resume_admission.find_receipt
                (state loaded)
                ~request
                ~principal_id:principal.id
              |> protocol_ok
            in
            assert (Option.is_none receipt)
          | Acknowledged { detail = Unavailable; receipt; _ } ->
            let retained =
              A.Goal_resume_admission.find_receipt
                (state loaded)
                ~request
                ~principal_id:principal.id
              |> protocol_ok
              |> Option.value_exn
            in
            assert (A.Goal_command_receipt.equal receipt retained)
          | Staged _
          | Accepted _
          | Interrupted_before_dispatch _
          | Dispatching _
          | Discarded _
          | Rejected _
          | Acknowledged _ ->
            failwith "recovered status invented certainty or live handoff");
         (match Intent.phase (Checks.native_intent loaded invocation) with
          | Dispatch_claimed _ -> ()
          | _ -> assert false);
         (* The status callback can run after its Operation is admitted but
            before that same ordinary input reaches the fake provider. *)
         Eio.Promise.await resumed_entered;
         [%test_eq: int] 3 !(counters.providers);
         release_resumed ();
         ignore (await_quiet loaded : A.Session_state.t)));
  print_endline
    "real uncertain command writer; recovered exact receipt or Unknown DATA; no replay \
     or false Rejected";
  [%expect
    {| real uncertain command writer; recovered exact receipt or Unknown DATA; no replay or false Rejected |}]
;;

let%expect_test
    "known Goal ACK followed by original revocation records failure without another \
     intent writer"
  =
  let revoke = ref ignore in
  let fault = Fault.create (After_goal_ack (fun () -> !revoke ())) in
  with_callback
    ~v1:true
    ~wrap_env:(Fault.wrap fault)
    (fun
        _env
         daemon
         entry
         runtime
         counters
         _session
         admission
         old_live
         held
         request
         operation
         ~release_ordinary
         ~resumed_entered
         ~release_resumed
         ~reopen:_
       ->
       let result, invocations =
         callback entry runtime ~operation ~mode:"resume" ~run:(fun admission ->
           Actor.stage_native_goal_resume entry.actor ~admission ~authorize ~request)
       in
       assert (result |> protocol_ok);
       let invocation = List.hd_exn invocations in
       (revoke := fun () -> old_live := false);
       Fault.arm fault entry request;
       release_ordinary ();
       ignore (Fault.await fault : Agent_store.Transaction.t);
       (* A fresh ordinary status reader remains authorized independently of the
         now-revoked original Goal Run. It never recaptures the resume CAS. *)
       let _reader = ordinary ~key:"native-goal-post-ack-status" daemon entry in
       Eio.Promise.await resumed_entered;
       let reader_operation = Option.value_exn (state entry).active_operation in
       [%test_eq: P.Operation.kind] (Turn User_submit) reader_operation.kind;
       (match Checks.status entry runtime reader_operation invocation.context.id with
        | Native.Status.Acknowledged
            { detail = Unavailable; failure = Some _; receipt; _ } ->
          assert (P.Idempotency_key.equal receipt.key (A.Goal_resume_request.key request))
        | _ -> failwith "known ACK and later host failure were not both preserved");
       (match Intent.phase (Checks.native_intent entry invocation) with
        | Dispatch_claimed _ -> ()
        | _ -> failwith "post-ACK negative detail wrote Rejected");
       assert (
         P.Run.equal
           (Checks.goal_run held admission)
           (Checks.goal_run (state entry) admission));
       assert (
         A.Goal_accounting.equal (Goal.accounting held) (Goal.accounting (state entry)));
       [%test_eq: int] 3 !(counters.providers);
       release_resumed ();
       ignore (await_quiet entry : A.Session_state.t));
  print_endline
    "same actual ACK remains authoritative; revoked original prevents Goal execution; \
     negative host detail is not Rejected";
  [%expect
    {| same actual ACK remains authoritative; revoked original prevents Goal execution; negative host detail is not Rejected |}]
;;
