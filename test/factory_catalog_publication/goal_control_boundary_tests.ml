open! Core
open Agent_server_test_support
open Retry_support
module Native = Goal_control_completion_tests
module Service = Agent_server.Goal_run_service

(* One actual controlling parent writer boundary, using the journal adapter.
   Native Invocation Complete alone is not the controlling checkpoint ACK. *)
module Fault = struct
  type t =
    { mutable session : P.Id.Session.t option
    ; mutable hits : int
    ; mutable candidate : Agent_store.Transaction.t option
    }

  let create () = { session = None; hits = 0; candidate = None }

  let reached t env path =
    match t.session with
    | None -> ()
    | Some session ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
           |> Existing.store_ok
         in
         if P.Id.Session.equal session transaction.session_id
         then (
           let delta =
             A.Session_delta_document.decode
               ~limits:Document_schema.Limits.default
               transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           let controls =
             Goal_admission_fault_tests.atoms delta
             |> List.exists ~f:(function
               | A.Session_delta.Goal_catalog_changed catalog ->
                 List.exists (A.Goal_catalog.receipts catalog) ~f:(fun receipt ->
                   String.equal
                     Native.control_key
                     (P.Idempotency_key.to_string receipt.A.Goal_command_receipt.key))
               | _ -> false)
           in
           if controls
           then (
             t.session <- None;
             t.hits <- t.hits + 1;
             t.candidate <- Some transaction;
             raise
               (Core_unix.Unix_error (EIO, "560 unknown controlling callback ACK", path))))
       | Some _ | None -> ())
  ;;

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:After_sync
      ~matches:(fun path ->
        String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(reached t env)
  ;;
end

exception Observed_unknown_control of P.Error.t

let%expect_test "unknown actual controlling parent ACK cannot issue Controlled" =
  let fault = Fault.create () in
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "unknown controlling callback dispatched a provider")
  in
  (match
     Creation_tests.with_daemon
       ~wrap_env:(Fault.wrap fault)
       ~prompt_text:(Native.prompt Pending_cold Hold_progress)
       ~qualify_chatml_extensions:true
       ~policy
       (fun env _sw _root daemon ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
            Native.with_pending_entry daemon (fun entry session before ->
              fault.session <- Some before.identity.session_id;
              let requested = Native.request before Pending_cold in
              let result =
                Service.start_episode
                  (Goal_resume_runtime_tests.public daemon)
                  ~entry
                  ~actor:original
                  ~session
                  ~request:requested
              in
              let failure =
                match result with
                | Error error -> error
                | Ok (Service.Outcome.Attempted attempt) ->
                  (match Service.Attempt.result attempt with
                   | Error error -> error
                   | Ok (Admission_uncertain error | Activation_failed { error; _ }) ->
                     error
                   | Ok (Controlled _ | Settled _ | Activated _ | Retained _) ->
                     failwith "unknown controlling callback acquired a positive result")
                | Ok (Retained _) ->
                  failwith "unknown controlling callback retained a positive result"
              in
              [%test_eq: int] 1 fault.hits;
              assert (Option.is_some fault.candidate);
              let confirmed = state entry in
              assert (
                not
                  (List.exists
                     (A.Goal_catalog.receipts (Option.value_exn confirmed.goal_catalog))
                     ~f:(fun receipt ->
                       String.equal
                         Native.control_key
                         (P.Idempotency_key.to_string receipt.A.Goal_command_receipt.key))));
              [%test_eq: int] 0 !providers;
              assert (not (Owner.is_loaded entry.runtime));
              (* Preserve actual selected writer cause across poisoned-fixture
                 teardown, exactly as existing552 fault fixtures do. *)
              raise (Observed_unknown_control failure))))
   with
   | () -> failwith "actual controlling parent writer cut was not reached"
   | exception Observed_unknown_control error ->
     [%test_eq: P.Error.code] Persistence_error error.code);
  print_endline
    "actual controlling parent journal ACK is unknown; no receipt/fact/runtime/provider \
     returned";
  (* The actual poisoned writer also refuses its queued failure-cleanup write.
     Keep that existing secondary diagnostic visible while asserting above that
     the original uncertain ACK remains the public result. *)
  [%expect
    {|
    +queued moderator cleanup failed: (Corrupt
    + "canonical projection target cannot regress or alter equal-revision hints")
    actual controlling parent journal ACK is unknown; no receipt/fact/runtime/provider returned
    |}]
;;
