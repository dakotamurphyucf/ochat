open! Core
open Agent_server_test_support
open Retry_support
module F = Delegated_policy_caller_support
module Warm = Delegated_warm_resource_tests
module Resource_loan = Ordinary_warm_resource_tests.Resource_loan
module E = P.Moderator_execution
module I = P.Invocation
module B = A.Runtime_builder
module Store = Agent_store

module Native_gate = struct
  type t =
    { mutable path : string option
    ; mutable cache : string option
    ; mutable armed : bool
    ; mutable reads : int
    ; mutable cache_saves : int
    ; entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; released : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    }

  let create () =
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    { path = None
    ; cache = None
    ; armed = false
    ; reads = 0
    ; cache_saves = 0
    ; entered
    ; enter
    ; released
    ; release
    }
  ;;

  let release t =
    if not (Eio.Promise.is_resolved t.released) then Eio.Promise.resolve t.release ()
  ;;

  let select t (entry : Registry.entry) =
    let current = F.state entry in
    t.path
    <- Some
         (Filename.concat
            current.spec.workspace_instance.canonical_root.native_path
            "policy.txt");
    t.cache
    <- Some
         (Filename.concat
            (Store.Session_store.Handle.cache_directory
               (Option.value_exn entry.store_handle))
            "cache.bin");
    t.armed <- true
  ;;

  let wrap t env =
    let env =
      Crash_fault_io.wrap
        env
        ~boundary:After_rename
        ~matches:(fun path ->
          Option.exists t.cache ~f:(fun cache ->
            String.equal (Filename.dirname path) (Filename.dirname cache)
            && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
            && String.is_suffix path ~suffix:".tmp"))
        ~reached:(fun path ->
          [%test_eq: string] (Option.value_exn t.cache) path;
          t.cache_saves <- t.cache_saves + 1)
    in
    Crash_fault_io.wrap
      env
      ~boundary:Before_open_in
      ~matches:(fun path -> t.armed && Option.exists t.path ~f:(String.equal path))
      ~reached:(fun _ ->
        (* The actual native implementation may open its file more than once.
           Hold only the first selected callback read, not a guessed open count. *)
        t.armed <- false;
        t.reads <- t.reads + 1;
        Eio.Promise.resolve t.enter ();
        Eio.Cancel.protect (fun () -> Eio.Promise.await t.released))
  ;;
end

let one label = function
  | [ value ] -> value
  | _ -> failwith (label ^ " was not unique")
;;

let find_receipt current id =
  List.find_exn current.A.Session_state.moderator_executions ~f:(fun receipt ->
    P.Id.Moderator_execution.equal receipt.E.context.id id)
;;

let find_invocation current id =
  List.find_exn current.A.Session_state.invocations ~f:(fun invocation ->
    P.Id.Invocation.equal invocation.I.context.id id)
;;

let%expect_test
    "parent claim ACK survives caller unwind and closes only after actual wrapper join"
  =
  let io = Native_gate.create () in
  F.with_fixture ~wrap_env:(Native_gate.wrap io) (fun t ->
    Eio.Switch.run (fun sw ->
      let loan = Resource_loan.create ~sw t.parent in
      let closing = ref None in
      let child_join = ref None in
      let cleanup () =
        Native_gate.release io;
        Resource_loan.release loan;
        let failure = ref None in
        let join phase f =
          try F.within t phase f with
          | exn ->
            if Option.is_none !failure
            then failure := Some (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        join "join independent resource loan" (fun () -> Resource_loan.join loan);
        join "join child caller cleanup" (fun () ->
          Option.iter !child_join ~f:(fun task -> Eio.Promise.await_exn task |> ignore));
        join "join exact parent strong close" (fun () ->
          Option.iter !closing ~f:(fun task -> Eio.Promise.await_exn task));
        Option.iter !failure ~f:(fun (exn, backtrace) ->
          Exn.raise_with_original_backtrace exn backtrace)
      in
      protect_cleanup ~finally:cleanup (fun () ->
        let retained =
          F.within t "actual independent resource loan" (fun () ->
            Resource_loan.await loan)
        in
        assert (phys_equal retained.worker t.parent_runtime.worker);
        assert (
          Option.equal
            phys_equal
            retained.moderator_manager
            t.parent_runtime.moderator_manager);
        let source = Warm.source (F.state t.parent) in
        let ports = !(t.counters.inference_ports) in
        let range = t.parent_initial.conversation.reserved_history_through in
        Native_gate.select io t.parent;
        (* No canonical child tool is supplied. Its real compiled first Turn_end
           is the only producer of this delegated parent Pre_tool_call. *)
        assert (Option.is_none t.provider.call);
        let _, operation =
          F.submit t.child t.child_writer ~actor:(Some t.b) "592 claim lifetime"
        in
        let joined = Eio.Fiber.fork_promise ~sw (fun () -> F.await_idle t t.child) in
        child_join := Some joined;
        F.within t "real parent native entered after claim ACK" (fun () ->
          Eio.Fiber.first
            (fun () -> Eio.Promise.await io.entered)
            (fun () ->
               Eio.Promise.await_exn joined |> ignore;
               failwith "child finished before actual parent policy native gate"));
        let parent = F.state t.parent in
        let receipt = F.policy_receipts parent |> one "actual Running parent claim" in
        assert (E.equal_status receipt.status Running);
        assert (Option.is_none receipt.decision);
        let delegation = Option.value_exn receipt.delegation in
        assert (
          P.Id.Session.equal
            delegation.child_session_id
            t.child_initial.identity.session_id);
        [%test_eq: int] t.child_initial.identity.generation delegation.child_generation;
        assert (E.equal_phase receipt.context.phase Pre_tool_call);
        assert (Option.is_none receipt.context.operation_id);
        let native =
          List.filter parent.invocations ~f:(fun invocation ->
            Option.exists invocation.I.parent_event ~f:(fun id ->
              P.Id.Moderator_execution.equal id receipt.context.id))
          |> one "actual parent native owned by acknowledged claim"
        in
        assert (I.equal_status native.status Dispatching);
        assert (String.equal native.context.tool_name "read_file");
        let child = F.state t.child in
        assert (
          Option.exists child.active_operation ~f:(fun active ->
            P.Id.Operation.equal active.id operation));
        let child_event =
          List.filter child.moderator_executions ~f:(fun event ->
            E.equal_phase event.E.context.phase Turn_end
            && E.equal_status event.status Running
            && Option.exists
                 event.context.operation_id
                 ~f:(P.Id.Operation.equal operation))
          |> one "actual child event lending its Caller"
        in
        (* Host policy runs before I.create/execute in Script_tool_calls. The
           parent's acknowledged claim owns a real prospective child ID; it is
           not evidence of an already admitted child native invocation. *)
        assert (
          not
            (List.exists child.invocations ~f:(fun invocation ->
               P.Id.Invocation.equal
                 invocation.I.context.id
                 delegation.child_invocation_id)));
        assert (Option.is_none (Eio.Promise.peek joined));
        Resource_loan.release loan;
        F.within t "actual last independent borrower departure" (fun () ->
          Resource_loan.join loan);
        [%test_eq: int] 0 io.cache_saves;
        F.within
          t
          "cancel actual child operation while parent callback executes"
          (fun () ->
             Actor.cancel_operation
               t.child.actor
               ~attachment_id:t.child_writer
               ~operation_id:operation
             |> protocol_ok
             |> ignore);
        let close =
          Eio.Fiber.fork_promise ~sw (fun () -> Owner.close_and_wait t.parent.runtime)
        in
        closing := Some close;
        (* A fork alone is not evidence that strong close entered. Actual close
           and wrapper joins below establish the owned release, after this gate. *)
        Native_gate.release io;
        F.within t "join actual cancelled child caller" (fun () ->
          Eio.Promise.await_exn joined |> ignore);
        F.within t "join parent source close after callback exit" (fun () ->
          Eio.Promise.await_exn close);
        let child_after = F.state t.child in
        let child_saved = find_receipt child_after child_event.context.id in
        (match child_saved.status with
         | Interrupted _ -> ()
         | Running | Completed _ | Failed _ ->
           raise_s
             [%sexp "child callback did not preserve cancellation", (child_saved : E.t)]);
        assert (
          not
            (List.exists child_after.invocations ~f:(fun invocation ->
               P.Id.Invocation.equal
                 invocation.I.context.id
                 delegation.child_invocation_id)));
        let after = F.state t.parent in
        let saved = find_receipt after receipt.context.id in
        (match saved.status with
         | Interrupted _ -> ()
         | Running | Completed _ | Failed _ ->
           raise_s [%sexp "claim did not retain Interrupted", (saved : E.t)]);
        assert (Option.is_none saved.decision);
        assert (Option.equal E.equal_delegation receipt.delegation saved.delegation);
        let saved_native = find_invocation after native.context.id in
        (match saved_native.status with
         | Resolved (Cancelled _) -> ()
         | Admitted | Dispatching
         | Resolved (Complete _ | Pending _ | Fail _)
         | Published _ ->
           raise_s [%sexp "native cancellation not retained", (saved_native : I.t)]);
        [%test_eq: int] 1 (List.length (F.policy_receipts after));
        [%test_eq: int] 1 (List.length after.invocations);
        assert (P.Run_source.equal source (Warm.source after));
        [%test_eq: int64] range after.conversation.reserved_history_through;
        assert (
          List.equal
            P.History.equal_entry
            t.parent_initial.conversation.canonical_history
            after.conversation.canonical_history);
        [%test_eq: int] ports !(t.counters.inference_ports);
        [%test_eq: int] 1 t.provider.child_calls;
        [%test_eq: int] 0 t.provider.ambient_calls;
        [%test_eq: int] 1 io.reads;
        [%test_eq: int] 1 io.cache_saves;
        assert (not (Owner.is_loaded t.parent.runtime));
        (match Owner.with_loaded_runtime t.parent.runtime (fun _ -> Ok ()) with
         | Error { code = Server_shutting_down; retryable = true; _ } -> ()
         | Error failure -> raise_s [%sexp (failure : P.Error.t)]
         | Ok () -> failwith "closed parent admitted another runtime borrower");
        Owner.close_and_wait t.parent.runtime;
        [%test_eq: int] 1 io.cache_saves)));
  [%expect {| |}]
;;
