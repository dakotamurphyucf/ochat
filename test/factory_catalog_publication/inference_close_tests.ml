open! Core
open Agent_server_test_support
open Retry_support
module Ledger = A.Inference_ledger
module Observation = Inference.Observation
module Marks = A.Canonical_usage_fold.Marks
module Store = Agent_store

module Fault = struct
  type phase =
    | Quiet
    | Reject_settlement
    | Await_cache
    | Reject_finish_once
    | Finished

  type t =
    { mutable phase : phase
    ; mutable marker : string option
    ; mutable cache : string option
    ; mutable cache_renames : int
    ; mutable settlement_rejections : int
    ; mutable finish_rejections : int
    }

  let create () =
    { phase = Quiet
    ; marker = None
    ; cache = None
    ; cache_renames = 0
    ; settlement_rejections = 0
    ; finish_rejections = 0
    }
  ;;

  let reject t path =
    let fail () =
      raise (Core_unix.Unix_error (EIO, "566 prewriter projection read", path))
    in
    match t.phase with
    | Quiet | Await_cache | Finished -> ()
    | Reject_settlement ->
      t.settlement_rejections <- t.settlement_rejections + 1;
      fail ()
    | Reject_finish_once ->
      t.phase <- Finished;
      t.finish_rejections <- t.finish_rejections + 1;
      fail ()
  ;;

  let cache_renamed t path =
    [%test_eq: string] (Option.value_exn t.cache) path;
    t.cache_renames <- t.cache_renames + 1;
    match t.phase with
    | Await_cache -> t.phase <- Reject_finish_once
    | Quiet | Reject_settlement | Reject_finish_once | Finished -> ()
  ;;

  let wrap_env t env =
    let observed =
      Crash_fault_io.wrap
        env
        ~boundary:After_rename
        ~matches:(fun path ->
          Option.exists t.cache ~f:(fun cache ->
            String.equal (Filename.dirname cache) (Filename.dirname path)
            && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
            && String.is_suffix path ~suffix:".tmp"))
        ~reached:(cache_renamed t)
    in
    Crash_fault_io.wrap
      observed
      ~boundary:Before_stat
      ~matches:(fun path -> Option.exists t.marker ~f:(String.equal path))
      ~reached:(reject t)
  ;;
end

let marks state =
  Marks.of_ledger state.A.Session_state.inference_ledger
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (A.Canonical_usage_fold.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let%expect_test
    "Factory close retries actual main graph facts without repeating resource close"
  =
  let fault = Fault.create () in
  with_recoverable_fixture
    ~wrap_env:(Fault.wrap_env fault)
    ~post_stream:(fun _ ->
      (* The real native Running ACK precedes this backend call. These failures
         occur in projection preflight, before Commit_writer.commit. *)
      fault.phase <- Reject_settlement;
      completed_response "566 actual main runtime resource close")
    (fun _env sw _root daemon _policy entry _runtime _revision counters ->
       let store = Daemon.store daemon in
       let handle = Option.value_exn entry.Registry.store_handle in
       fault.marker
       <- Some
            (Filename.concat
               (Store.Data_root.indexes_path (Store.Session_store.data_root store))
               "sessions.recovery-required");
       fault.cache
       <- Some
            (Filename.concat
               (Store.Session_store.Handle.cache_directory handle)
               "cache.bin");
       let before = state entry in
       let before_marks = marks before in
       [%test_eq: int] 0 !(counters.providers);
       Owner.with_loaded_runtime entry.runtime (fun runtime ->
         (* This SAME Factory-selected execution uses its main Graph_tracking
            identity and callbacks. No input/auxiliary graph is constructed. *)
         (match
            Inference_client.Execution.complete_text
              runtime.A.Runtime_builder.inference_execution
              ~sw
              ~settings:[]
              ~messages:[ User, "Exercise actual main runtime inference." ]
              ()
          with
          | _ -> failwith "actual completion ignored the prewriter rejection"
          | exception Agent_server.Graph_tracking.Rejected error ->
            [%test_eq: P.Error.code] Persistence_error error.code);
         Ok ())
       |> protocol_ok;
       assert (fault.settlement_rejections > 0);
       let running = state entry in
       let rows = Ledger.rows running.inference_ledger in
       let saved =
         Goal_resume_runtime_tests.one_actual "actual main graph canonical attempt" rows
       in
       let native_handle = Ledger.Row.handle saved in
       (match Observation.Attempt_record.state (Ledger.Row.record saved) with
        | Running -> ()
        | Prepared | Terminal _ | Interrupted _ ->
          failwith "prewriter rejection changed the confirmed native row");
       let admitted_marks = marks running in
       [%test_eq: int64] (Int64.succ before_marks.attempt) admitted_marks.attempt;
       [%test_eq: int64] before_marks.turn admitted_marks.turn;
       [%test_eq: int] 1 !(counters.providers);
       fault.phase <- Await_cache;
       (match Result.try_with (fun () -> Owner.close_and_wait entry.runtime) with
        | Error _ -> ()
        | Ok () -> failwith "failed Graph.finish was reported as joined");
       [%test_eq: int] 1 fault.cache_renames;
       [%test_eq: int] 1 fault.finish_rejections;
       assert (not (Owner.is_loaded entry.runtime));
       let still_running =
         Ledger.find
           (state entry).inference_ledger
           ~ordinal:(Ledger.Handle.ordinal native_handle)
         |> Option.value_exn
       in
       assert (Ledger.Handle.equal native_handle (Ledger.Row.handle still_running));
       (match Observation.Attempt_record.state (Ledger.Row.record still_running) with
        | Running -> ()
        | Prepared | Terminal _ | Interrupted _ ->
          failwith "failed close changed the confirmed native row");
       (* Positive factual retry on this SAME Actor/writer proves the injected
          read failure did not authorize recovery from an uncertain journal. *)
       Owner.close_and_wait entry.runtime;
       Owner.close_and_wait entry.runtime;
       [%test_eq: int] 1 fault.cache_renames;
       [%test_eq: int] 1 fault.finish_rejections;
       [%test_eq: int] 1 !(counters.providers);
       let final = state entry in
       assert (Marks.equal admitted_marks (marks final));
       let ended =
         Ledger.find final.inference_ledger ~ordinal:(Ledger.Handle.ordinal native_handle)
         |> Option.value_exn
       in
       assert (Ledger.Handle.equal native_handle (Ledger.Row.handle ended));
       match Observation.Attempt_record.state (Ledger.Row.record ended) with
       | Interrupted { reason = Host_interrupted; delivery = Response_started } -> ()
       | Prepared | Running | Terminal _ | Interrupted _ ->
         raise_s
           [%sexp
             "actual main graph settlement changed delivery"
           , (Observation.Attempt_record.to_json (Ledger.Row.record ended) : Jsonaf.t)]);
  print_endline
    "resource cache joined once; same main graph retried factual finish; writer stayed \
     available";
  [%expect
    {| resource cache joined once; same main graph retried factual finish; writer stayed available |}]
;;
