open! Core
open Agent_server_test_support
open Retry_support
module Existing = Delegated_load_tests
module B = A.Runtime_builder
module M = Chat_response.Moderator_manager
module Lease = A.Prompt_source_lease
module Store = Agent_store

module Handoff_io = struct
  type t =
    { caller : unit Eio.Fiber.key
    ; mutable record : string option
    ; mutable cache : string option
    ; mutable armed : bool
    ; mutable reads : int
    ; mutable cache_renames : int
    ; entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; released : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    }

  let create () =
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    { caller = Eio.Fiber.create_key ()
    ; record = None
    ; cache = None
    ; armed = false
    ; reads = 0
    ; cache_renames = 0
    ; entered
    ; enter
    ; released
    ; release
    }
  ;;

  let release t =
    if Option.is_none (Eio.Promise.peek t.released) then Eio.Promise.resolve t.release ()
  ;;

  let select t daemon child =
    let current = state child in
    let reference = Option.value_exn current.spec.delegation in
    let filename =
      Store.Delegation_store.Key.sexp_of_t reference.key
      |> Sexp.to_string_mach
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
      |> fun hash -> hash ^ ".frame"
    in
    let root = Store.Session_store.data_root (Daemon.store daemon) in
    t.record
    <- Some
         (Filename.concat
            (Filename.concat (Store.Data_root.path root) "delegations")
            filename);
    t.cache
    <- Some
         (Filename.concat
            (Store.Session_store.Handle.cache_directory
               (Option.value_exn child.Registry.store_handle))
            "cache.bin")
  ;;

  let wrap t env =
    let observed =
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
          t.cache_renames <- t.cache_renames + 1)
    in
    Crash_fault_io.wrap
      observed
      ~boundary:Before_open_in
      ~matches:(fun path ->
        t.armed
        && Option.is_some (Eio.Fiber.get t.caller)
        && Option.exists t.record ~f:(String.equal path))
      ~reached:(fun _ ->
        (* Only the actual reload fiber's first selected-source read is held.
           The path is a locator; the private reader still validates authority. *)
        t.armed <- false;
        t.reads <- t.reads + 1;
        Eio.Promise.resolve t.enter ();
        Eio.Promise.await t.released)
  ;;
end

let settled child =
  Actor.with_moderator_checkpoint child.Registry.actor (fun () -> Actor.state child.actor)
  |> protocol_ok
;;

let loaded child =
  Owner.with_loaded_runtime child.Registry.runtime (fun runtime -> Ok runtime)
  |> protocol_ok
;;

let identity runtime =
  M.identity_snapshot (Option.value_exn runtime.B.moderator_manager)
  |> Result.ok_or_failwith
  |> Session.Moderator_state.Identity_snapshot.to_jsonaf
;;

let source current = A.Moderator_source_installation.capture current |> protocol_ok |> snd

let completed current phase =
  List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
    P.Moderator_execution.equal_phase receipt.context.phase phase)
  |> List.map ~f:(fun receipt ->
    match receipt.P.Moderator_execution.status with
    | Completed _ -> receipt
    | Running | Failed _ | Interrupted _ ->
      raise_s [%sexp (receipt : P.Moderator_execution.t)])
;;

let assert_same_resources previous current =
  assert (not (phys_equal previous.B.worker current.B.worker));
  assert (
    phys_equal
      (Option.value_exn previous.moderator_manager)
      (Option.value_exn current.moderator_manager));
  assert (
    phys_equal
      (Option.value_exn previous.native_runtime)
      (Option.value_exn current.native_runtime));
  assert (
    phys_equal
      (Option.value_exn previous.moderator_script_tools)
      (Option.value_exn current.moderator_script_tools));
  assert (
    Option.equal
      Store.Prompt_source_receipt.equal
      (Lease.receipt previous.source_lease)
      (Lease.receipt current.source_lease));
  [%test_eq: int] previous.reserved_history_through current.reserved_history_through
;;

let expect_busy = function
  | Error { P.Error.code = Conflict; retryable = true; _ } -> ()
  | Error error -> raise_s [%sexp (error : P.Error.t)]
  | Ok () -> failwith "actual resource/effect loans did not exclude administration"
;;

let await_entry entered pending =
  Eio.Fiber.first
    (fun () -> Eio.Promise.await entered)
    (fun () ->
       Eio.Promise.await_exn pending |> Existing.restore;
       failwith "actual lease returned before entering its callback")
;;

let expect_cancelled = function
  | Owner.Scope_outcome.Raised (Eio.Cancel.Cancelled _, _) -> ()
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
  | Returned (Ok ()) -> failwith "actual effect_loan loan was not cancelled by unload"
;;

let await_turn child providers before operation =
  let rec loop () =
    let current = settled child in
    let turns =
      P.Inference_query.Summary.turns
        (A.Inference_ledger.summary current.inference_ledger)
    in
    if
      Option.is_none current.active_operation
      && Int.equal !providers 1
      && Int64.equal turns.completed (Int64.succ before)
    then (
      let endings = completed current Turn_end in
      assert (
        List.exists endings ~f:(fun receipt ->
          Option.exists receipt.context.operation_id ~f:(P.Id.Operation.equal operation)));
      current)
    else (
      Option.iter current.failure ~f:(fun error -> raise_s [%sexp (error : P.Error.t)]);
      Eio.Fiber.yield ();
      loop ())
  in
  loop ()
;;

let%expect_test "actual delegated warm handoff survives the former last resource borrower"
  =
  let io = Handoff_io.create () in
  Existing.with_child
    ~wrap_env:(Handoff_io.wrap io)
    (fun env sw daemon _parent child _construction_gate providers ->
       let within phase f =
         try Eio.Time.with_timeout_exn env#clock 10. f with
         | Eio.Time.Timeout -> failwith ("591 delegated warm timed out: " ^ phase)
       in
       Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
       let before = settled child in
       [%test_eq: int] 1 (List.length (completed before Session_start));
       assert (Option.is_none before.active_operation);
       assert (
         Option.for_all before.run_state ~f:(fun index ->
           List.for_all (A.Run_state.runs index) ~f:(fun run ->
             match run.P.Run.lifecycle with
             | Terminal _ -> true
             | Admitted | Active | Waiting _ -> false)));
       assert (not (Owner.has_activation_handoff child.runtime));
       let first = loaded child in
       let first_identity = identity first in
       let first_source = source before in
       Handoff_io.select io daemon child;
       let resource_entered, resource_enter = Eio.Promise.create () in
       let release_resource, release_resource_u = Eio.Promise.create () in
       let effect_entered, effect_enter = Eio.Promise.create () in
       let release_effect, release_effect_u = Eio.Promise.create () in
       let release promise resolver =
         if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
       in
       let resource_completed = ref false in
       let effect_left = ref false in
       let reload = ref None in
       let resources =
         Eio.Fiber.fork_promise ~sw (fun () ->
           Existing.capture_load (fun () ->
             Owner.with_delegation_resources child.runtime (fun actual ->
               assert (phys_equal actual.worker first.worker);
               Eio.Promise.resolve resource_enter actual;
               Eio.Promise.await release_resource;
               resource_completed := true;
               Ok ())))
       in
       let effect_loan = ref None in
       protect_cleanup
         ~finally:(fun () ->
           Handoff_io.release io;
           release release_resource release_resource_u;
           release release_effect release_effect_u;
           let failure = ref None in
           let join phase f =
             try within phase f with
             | exn ->
               if Option.is_none !failure
               then failure := Some (exn, Stdlib.Printexc.get_raw_backtrace ())
           in
           join "cleanup retained reload" (fun () ->
             Option.iter !reload ~f:(fun pending ->
               Eio.Promise.await_exn pending |> Existing.restore));
           join "cleanup effect loan" (fun () ->
             Option.iter !effect_loan ~f:(fun pending ->
               match Eio.Promise.await_exn pending with
               | Owner.Scope_outcome.Returned (Ok ()) | Raised (Eio.Cancel.Cancelled _, _)
                 -> ()
               | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
               | Raised (exn, backtrace) ->
                 Exn.raise_with_original_backtrace exn backtrace));
           join "cleanup resource loan" (fun () ->
             Eio.Promise.await_exn resources |> Existing.restore);
           Option.iter !failure ~f:(fun (exn, backtrace) ->
             Exn.raise_with_original_backtrace exn backtrace))
         (fun () ->
            let retained =
              within "resource loan" (fun () -> await_entry resource_entered resources)
            in
            assert (phys_equal retained.worker first.worker);
            let executing =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Existing.capture_load (fun () ->
                  Owner.with_background_runtime child.runtime (fun actual ->
                    Exn.protect
                      ~finally:(fun () -> effect_left := true)
                      ~f:(fun () ->
                        assert (phys_equal actual.worker first.worker);
                        Eio.Promise.resolve effect_enter ();
                        Eio.Promise.await release_effect;
                        Ok ()))))
            in
            effect_loan := Some executing;
            within "effect loan" (fun () -> await_entry effect_entered executing);
            Owner.unload child.runtime |> expect_busy;
            let administration_ran = ref false in
            Owner.with_administration child.runtime (fun () ->
              administration_ran := true;
              Ok ())
            |> expect_busy;
            assert (not !administration_ran);
            let writer, _ =
              Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
            in
            protect_cleanup
              ~finally:(fun () -> Actor.detach child.actor writer.id |> protocol_ok)
              (fun () ->
                 Actor.stop child.actor ~attachment_id:writer.id ~mode:Cancel
                 |> protocol_ok
                 |> ignore;
                 within "Stop resource retirement" (fun () ->
                   Owner.unload_and_wait child.runtime |> protocol_ok);
                 within "actual effect cancellation" (fun () ->
                   Eio.Promise.await_exn executing |> expect_cancelled);
                 assert !effect_left;
                 assert (not !resource_completed);
                 assert (not (Owner.is_loaded child.runtime));
                 (* The generic scheduling snapshot remains busy while the actual
                    resource loan survives; only authenticated warm staging may
                    make the exact same-Product handoff exception. *)
                 assert (not (Owner.source_lane_idle child.runtime));
                 [%test_eq: int] 0 io.cache_renames;
                 io.armed <- true;
                 let loading =
                   Eio.Fiber.fork_promise ~sw (fun () ->
                     Eio.Fiber.with_binding io.caller () (fun () ->
                       Existing.capture_load (fun () ->
                         Owner.ensure_loaded_authenticated child.runtime ~actor:original)))
                 in
                 reload := Some loading;
                 within "selected-source private read" (fun () ->
                   Eio.Fiber.first
                     (fun () -> Eio.Promise.await io.entered)
                     (fun () ->
                        Eio.Promise.await_exn loading |> Existing.restore;
                        failwith "warm load returned before its real source gate"));
                 within "responsive source-stage Owner" (fun () ->
                   assert (not (Owner.is_loaded child.runtime));
                   assert (not (Owner.source_lane_idle child.runtime)));
                 assert (Option.is_none (Eio.Promise.peek loading));
                 [%test_eq: int] 1 io.reads;
                 release release_resource release_resource_u;
                 within "former last resource borrower" (fun () ->
                   Eio.Promise.await_exn resources |> Existing.restore);
                 assert !resource_completed;
                 [%test_eq: int] 0 io.cache_renames;
                 assert (Option.is_none (Eio.Promise.peek loading));
                 Handoff_io.release io;
                 within "authenticated retained reload" (fun () ->
                   Eio.Promise.await_exn loading |> Existing.restore);
                 let current = settled child in
                 let second = loaded child in
                 assert_same_resources first second;
                 assert (Jsonaf.exactly_equal first_identity (identity second));
                 assert (P.Run_source.equal first_source (source current));
                 assert (
                   Option.equal Jsonaf.exactly_equal before.moderator current.moderator);
                 assert (
                   List.equal
                     P.Moderator_execution.equal
                     before.moderator_executions
                     current.moderator_executions);
                 [%test_eq: int64]
                   before.conversation.next_history_sequence
                   current.conversation.next_history_sequence;
                 [%test_eq: int64]
                   before.conversation.reserved_history_through
                   current.conversation.reserved_history_through;
                 (match current.runtime_initialization with
                  | Ready -> ()
                  | Pending _ -> failwith "warm bind reentered initialization");
                 assert (Owner.is_loaded child.runtime);
                 assert (Owner.source_lane_idle child.runtime);
                 assert (not (Owner.has_activation_handoff child.runtime));
                 [%test_eq: int] 0 !providers;
                 [%test_eq: int] 0 io.cache_renames;
                 Actor.start child.actor ~actor:original ~attachment_id:writer.id
                 |> protocol_ok
                 |> ignore;
                 let message =
                   Owner.parse_user_content
                     child.runtime
                     ~id:(A.History_id_source.allocate child.history_ids |> protocol_ok)
                     { kind = Plain_text
                     ; text = "Actual retained delegated worker request."
                     ; attachments = []
                     }
                   |> protocol_ok
                   |> A.History_codec.to_protocol
                 in
                 let submission =
                   Actor.submit_message_authenticated
                     child.actor
                     ~actor:original
                     ~attachment_id:writer.id
                     message
                   |> protocol_ok
                 in
                 let previous_turns =
                   P.Inference_query.Summary.turns
                     (A.Inference_ledger.summary current.inference_ledger)
                 in
                 let finished =
                   within "actual new worker foreground completion" (fun () ->
                     await_turn
                       child
                       providers
                       previous_turns.completed
                       (Option.value_exn submission.operation_id))
                 in
                 assert (P.Run_source.equal first_source (source finished));
                 assert (
                   List.for_all current.conversation.canonical_history ~f:(fun prior ->
                     List.exists finished.conversation.canonical_history ~f:(fun actual ->
                       P.History.equal_entry prior actual)));
                 let added =
                   List.filter finished.conversation.canonical_history ~f:(fun entry ->
                     not
                       (List.exists
                          current.conversation.canonical_history
                          ~f:(fun prior -> P.History.Id.equal prior.id entry.id)))
                 in
                 (match
                    List.filter added ~f:(fun entry ->
                      P.History.Id.equal entry.id message.id)
                  with
                  | [ actual ] -> assert (P.History.equal_entry message actual)
                  | _ -> failwith "submitted history entry was lost or duplicated");
                 assert (
                   List.exists added ~f:(fun entry ->
                     P.History.equal_role entry.role Assistant
                     && P.History.equal_kind entry.kind Message));
                 (* Foreground and Registry allocators reserve their own blocks;
                    the pre-submit assertions above prove warm binding itself
                    did not reserve another moderator range. *)
                 assert (
                   List.for_all added ~f:(fun entry ->
                     String.equal
                       (History_entry.Id.namespace entry.id)
                       (P.Id.Session.to_string finished.identity.session_id)
                     && Int64.(
                          of_int (History_entry.Id.sequence entry.id)
                          < finished.conversation.reserved_history_through)));
                 Actor.stop child.actor ~attachment_id:writer.id ~mode:Cancel
                 |> protocol_ok
                 |> ignore;
                 within "final physical release" (fun () ->
                   Owner.unload_and_wait child.runtime |> protocol_ok);
                 [%test_eq: int] 1 !providers;
                 [%test_eq: int] 1 io.cache_renames;
                 assert (not (Owner.is_loaded child.runtime));
                 assert (Owner.source_lane_idle child.runtime))));
  [%test_eq: int] 1 io.cache_renames;
  print_endline
    "same compiled resources survived last-borrower handoff; new worker ran once; \
     physical resources closed once";
  [%expect
    {| same compiled resources survived last-borrower handoff; new worker ran once; physical resources closed once |}]
;;
