open! Core
open Agent_server_test_support
open Retry_support
module F = Runtime_unmoderated_source_support
module N = A.Native_tool_invocation
module Store = Agent_store
module Existing_fault = Legacy_idle_completion_fault_tests
module Physical = Constructor_model_fault_tests.Physical
module Callback = Callback_retirement_fault_tests
module Frames = Delegated_policy_claim_fault_tests
module Live = Runtime_unmoderated_source_tests

module Boundary = struct
  type t =
    | Writer_unknown
    | Publication

  let mode = function
    | Writer_unknown -> "unmoderated-configuration-unknown"
    | Publication -> "unmoderated-configuration-publication"
  ;;
end

let capture = Constructor_model_fault_tests.capture
let joined = Constructor_model_fault_tests.joined

module Cut = struct
  type selected =
    { session : P.Id.Session.t
    ; journal : string
    ; metadata : string
    ; revision : int64
    ; probe : unit -> unit
    }

  type candidate =
    { transaction : Store.Transaction.t
    ; payload : string
    ; checksum : string
    ; path : string
    }

  type t =
    { boundary : Boundary.t
    ; mutable selected : selected option
    ; mutable candidate : candidate option
    ; mutable injected : exn option
    ; mutable probe : (unit, exn * Stdlib.Printexc.raw_backtrace) result option
    ; mutable writer_hits : int
    ; mutable publication_hits : int
    }

  let create boundary =
    { boundary
    ; selected = None
    ; candidate = None
    ; injected = None
    ; probe = None
    ; writer_hits = 0
    ; publication_hits = 0
    }
  ;;

  let arm t handle (before : A.Session_state.t) ~probe =
    t.selected
    <- Some
         { session = before.identity.session_id
         ; journal = Store.Session_store.Handle.journal_directory handle
         ; metadata =
             Filename.concat (Store.Session_store.Handle.directory handle) "metadata.sexp"
         ; revision = Int64.succ before.spec.configuration_revision
         ; probe
         }
  ;;

  let configuration selected (transaction : Store.Transaction.t) =
    if not (P.Id.Session.equal transaction.session_id selected.session)
    then false
    else (
      match Existing_fault.changes transaction with
      | [ Inference_target_changed target; Configuration_revision_changed revision ] ->
        Int64.equal selected.revision revision
        && String.equal "597-next-model" (Inference.Request.Target.model target)
      | _ -> false)
  ;;

  let wrap t env =
    let wrapped =
      Crash_fault_io.wrap
        env
        ~boundary:After_sync
        ~matches:(fun path ->
          Option.exists t.selected ~f:(fun selected ->
            String.equal selected.journal (Filename.dirname path)
            && String.is_suffix path ~suffix:".log"))
        ~reached:(fun path ->
          let selected = Option.value_exn t.selected in
          let scan =
            Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
            |> Store.Journal_segment.scan_contents
                 ~max_payload_length:Physical.max_payload_length
            |> Existing.store_ok
          in
          match List.last scan.entries with
          | Some frame when Int.equal (Store.Frame.flags frame.frame) 0 ->
            let payload = Store.Frame.payload frame.frame in
            let transaction = Store.Transaction.decode payload |> Existing.store_ok in
            if configuration selected transaction
            then (
              assert (Option.is_none t.candidate);
              t.candidate
              <- Some
                   { transaction
                   ; payload
                   ; checksum = Store.Frame.checksum_hex frame.frame
                   ; path
                   };
              t.writer_hits <- t.writer_hits + 1;
              match t.boundary with
              | Publication -> ()
              | Writer_unknown ->
                t.probe <- Some (capture selected.probe);
                let injected =
                  Core_unix.Unix_error (EIO, "597 configuration after sync", path)
                in
                t.injected <- Some injected;
                raise injected)
          | Some _ | None -> ())
    in
    Crash_fault_io.wrap
      wrapped
      ~boundary:After_rename
      ~matches:(fun path ->
        match t.boundary, t.selected, t.candidate with
        | Publication, Some selected, Some _ ->
          String.equal (Filename.dirname selected.metadata) (Filename.dirname path)
          && String.is_prefix (Filename.basename path) ~prefix:"metadata.sexp.tmp-"
        | Writer_unknown, _, _ | Publication, None, _ | Publication, Some _, None -> false)
      ~reached:(fun path ->
        let selected = Option.value_exn t.selected in
        [%test_eq: string] selected.metadata path;
        t.publication_hits <- t.publication_hits + 1;
        t.probe <- Some (capture selected.probe);
        (* Factory ignores metadata's returned IO Error; a genuine unexpected
           non-IO publication exception is needed to exercise its fatal path. *)
        let injected = Failure "597 configuration publication sentinel" in
        t.injected <- Some injected;
        raise injected)
  ;;
end

let stale_capture actor request =
  match Actor.Causal_governance.capture actor ~request with
  | Error { code = Conflict; message; retryable = false; _ }
    when String.equal message "delegated policy Caller lost its exact source binding" ->
    ()
  | Error failure ->
    raise_s [%sexp "unexpected old configuration capture", (failure : P.Error.t)]
  | Ok _ -> failwith "old configuration Caller captured authority after positive ACK"
;;

let fresh_target (before : A.Session_state.t) =
  let target =
    match Inference.Selection.view before.spec.inference_target with
    | Captured target -> target
    | Unresolved -> failwith "actual root had no installed inference target"
  in
  Inference.Request.Target.with_model
    target
    ~model:"597-next-model"
    ~limits:D.Limits.default
  |> Result.map_error ~f:(fun failure ->
    Sexp.to_string_hum (Inference.Request.Error.sexp_of_t failure))
  |> Result.ok_or_failwith
;;

let frames_json frames =
  `Array
    (List.map frames ~f:(fun (flags, payload, checksum) ->
       `Array [ `Number (Int.to_string flags); `String payload; `String checksum ]))
;;

let write_report t cut before anchor before_frames fields =
  let candidate = Option.value_exn cut.Cut.candidate in
  joined (Option.value_exn cut.probe);
  [%test_eq: int] 1 cut.writer_hits;
  [%test_eq: int] 1 t.F.host.reads;
  [%test_eq: int] 1 !(t.counters.providers);
  [%test_eq: int] 1 !(t.counters.inference_ports);
  [%test_eq: int] 0 t.cache.cache_renames;
  assert (List.is_empty before.A.Session_state.jobs);
  assert (List.is_empty before.moderator_executions);
  let report =
    `Object
      ([ "mode", `String (Boundary.mode cut.boundary)
       ; "before", Existing_fault.state_json before
       ; "initial_snapshot", anchor
       ; "before_frames", frames_json before_frames
       ; "transaction_payload", `String candidate.payload
       ; "transaction_sha256", `String (Store.Transaction.hash candidate.transaction)
       ; "frame_checksum", `String candidate.checksum
       ; "journal_path", `String candidate.path
       ; "injected", `String (Exn.to_string (Option.value_exn cut.injected))
       ]
       @ fields)
  in
  Eio.Flow.copy_string (Jsonaf.to_string report ^ "\n") (Eio.Stdenv.stdout t.env);
  (* Process-only crash: neither uncertain writer nor fatal mailbox is called
     a successful native finalizer or healthy Owner close. *)
  Core_unix.exit_immediately 73
;;

let run_child boundary test_root =
  let cut = Cut.create boundary in
  F.with_fixture ~test_root ~wrap_env:(Cut.wrap cut) (fun t ->
    let handle = Option.value_exn t.entry.store_handle in
    let anchor = Physical.snapshot_anchor t.env handle in
    let config_outcome, config_resolver = Eio.Promise.create () in
    let publication_before = ref None in
    let run borrowed =
      let caller = F.caller t borrowed in
      let request =
        F.request
          t
          borrowed
          ~candidate:
            (P.Id.Invocation.of_string "inv_597_physical_configuration" |> protocol_ok)
          caller
      in
      F.registered t request;
      let before = F.state t in
      let before_frames = Frames.frames t.env handle in
      let config = Live.config_request t ~key:(Boundary.mode boundary) in
      let probe () =
        match boundary with
        | Writer_unknown -> F.registered t request
        | Publication ->
          (* All these calls are pure/same-domain and use the stored real
             borrowed scope. No Actor RPC or fiber-local N.borrow here. *)
          Live.stale t request;
          stale_capture t.entry.actor request;
          let fresh = N.borrowed_policy_caller borrowed |> protocol_ok in
          protect_cleanup
            ~finally:(fun () -> A.Delegated_policy_caller.close fresh)
            (fun () -> F.registered t { request with caller = Some fresh })
      in
      Cut.arm cut handle before ~probe;
      publication_before := Some (before, before_frames);
      let outcome =
        capture (fun () ->
          F.within t "actual physical configuration caller" (fun () ->
            Actor.update_configuration t.entry.actor config))
      in
      Eio.Promise.resolve config_resolver outcome;
      match boundary with
      | Writer_unknown ->
        let result = joined outcome in
        let candidate = Option.value_exn cut.candidate in
        let expected =
          Store.Store_error.of_exn
            ~operation:"append journal segment"
            ~path:candidate.path
            (Option.value_exn cut.injected)
          |> Existing_fault.persistence_error
        in
        (match result with
         | Error actual -> Existing_fault.same_error expected actual
         | Ok _ -> failwith "entered configuration writer was acknowledged after EIO");
        [%test_eq: int] 0 cut.publication_hits;
        assert (
          Jsonaf.exactly_equal
            (Existing_fault.state_json before)
            (Existing_fault.state_json (F.state t)));
        F.registered t request;
        write_report
          t
          cut
          before
          anchor
          before_frames
          [ "primary", P.Error.to_json expected ]
      | Publication ->
        (match outcome with
         | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
         | Ok (Error failure) ->
           raise_s [%sexp "fatal configuration caller", (failure : P.Error.t)]
         | Ok (Ok _) -> failwith "fatal configuration publication returned success")
    in
    match boundary with
    | Writer_unknown ->
      ignore (F.run t ~id:"597-configuration-unknown" run : A.Session_state.t)
    | Publication ->
      (* Unlike F.run, this branch performs no post-fatal Actor.state/idle RPC.
         Only the observer is protected; the actual native/config caller keeps
         its ordinary cancellation/error path. *)
      let id = "597-configuration-publication" in
      t.next
      := Some
           { Delegated_policy_caller_support.Provider.name = "read_file"
           ; file = "value.txt"
           ; id
           };
      t.host.selected
      <- Some
           ( Filename.concat
               (F.state t).spec.workspace_instance.canonical_root.native_path
               "value.txt"
           , fun () -> run (N.borrow () |> protocol_ok) );
      Eio.Cancel.protect (fun () ->
        let (fatal, outcome), diagnostics =
          Callback.capture_diagnostics t.env (fun () ->
            let _message, _operation =
              F.within t "submit actual native for publication cut" (fun () ->
                Delegated_policy_caller_support.submit
                  t.entry
                  t.writer
                  ~actor:(Some original)
                  "597 actual fatal configuration publication")
            in
            let outcome =
              F.within t "join actual configuration caller" (fun () ->
                Eio.Promise.await config_outcome)
            in
            (match outcome with
             | Error _ | Ok (Error _) -> ()
             | Ok (Ok _) -> failwith "fatal publication caller returned a positive result");
            let rec await_fatal () =
              match Eio.Switch.get_error t.sw with
              | None ->
                Eio.Fiber.yield ();
                await_fatal ()
              | Some (Eio.Cancel.Cancelled cause) -> cause
              | Some exn -> exn
            in
            F.within t "observe actual configuration switch failure" await_fatal, outcome)
        in
        (match Callback.fatal_causes fatal with
         | [ actual ] -> assert (phys_equal actual (Option.value_exn cut.injected))
         | causes ->
           raise_s [%sexp "unexpected fatal configuration causes", (causes : exn list)]);
        [%test_eq: int] 1 cut.publication_hits;
        let caller_outcome =
          match outcome with
          | Error (exn, _) -> `Object [ "raised", `String (Exn.to_string exn) ]
          | Ok (Error failure) -> `Object [ "returned_error", P.Error.to_json failure ]
          | Ok (Ok _) -> failwith "fatal configuration caller unexpectedly succeeded"
        in
        assert (List.sum (module Int) diagnostics ~f:String.length <= 32768);
        let before, before_frames = Option.value_exn !publication_before in
        (* Only pure counters and independent IO follow the fatal owner. The
           actual caller outcome and bounded diagnostics remain reviewable. *)
        write_report
          t
          cut
          before
          anchor
          before_frames
          [ ( "fatal_causes"
            , `Array
                (List.map (Callback.fatal_causes fatal) ~f:(fun exn ->
                   `String (Exn.to_string exn))) )
          ; "caller_outcome", caller_outcome
          ; "diagnostics", `Array (List.map diagnostics ~f:(fun line -> `String line))
          ]))
;;

let expected_state before (selected : Store.Transaction.t) =
  let before : A.Session_state.t = before in
  assert (P.Id.Session.equal before.identity.session_id selected.session_id);
  [%test_eq: int] before.identity.generation selected.generation;
  [%test_eq: int64]
    (Int64.succ before.counters.transaction_sequence)
    selected.transaction_sequence;
  [%test_eq: int64] (Int64.succ before.counters.revision) selected.session_revision;
  let target = fresh_target before in
  let revision = Int64.succ before.spec.configuration_revision in
  (match Existing_fault.changes selected with
   | [ Inference_target_changed actual; Configuration_revision_changed actual_revision ]
     ->
     assert (
       Jsonaf.exactly_equal
         (Inference.Request.Target.to_json target)
         (Inference.Request.Target.to_json actual));
     [%test_eq: int64] revision actual_revision
   | _ -> failwith "physical configuration transaction changed unrelated DATA");
  let now =
    Time_ns.of_int_ns_since_epoch (Int64.to_int_exn selected.accepted_at_ns)
    |> P.Timestamp.of_time_ns
  in
  let selected_target =
    Inference.Selection.change
      before.spec.inference_target
      ~target
      ~limits:D.Limits.default
    |> Result.map_error ~f:(fun failure ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t failure))
    |> Result.ok_or_failwith
  in
  let expected =
    { before with
      identity = { before.identity with updated_at = now }
    ; spec =
        { before.spec with
          inference_target = selected_target
        ; configuration_revision = revision
        }
    ; counters =
        { before.counters with
          transaction_sequence = selected.transaction_sequence
        ; revision = selected.session_revision
        ; event_sequence = Int64.succ before.counters.event_sequence
        }
    }
  in
  A.Session_state.validate expected |> protocol_ok;
  let event =
    P.Event.Durable.of_payload
      ~session_id:selected.session_id
      ~sequence:expected.counters.event_sequence
      ~revision:selected.session_revision
      ~timestamp:now
      (Session_updated (A.Session_state.summary expected))
  in
  [%test_eq: int64 option] (Some event.sequence) selected.first_event_sequence;
  [%test_eq: int64 option] (Some event.sequence) selected.last_event_sequence;
  let actual_event =
    Existing_fault.events selected
    |> Delegated_policy_claim_tests.one "exact configuration Session_updated"
  in
  assert (
    Jsonaf.exactly_equal
      (P.Event.Durable.to_json event)
      (P.Event.Durable.to_json actual_event));
  expected
;;

let recover env sw root boundary report =
  let member name = Jsonaf.member_exn name report in
  [%test_eq: string] (Boundary.mode boundary) (member "mode" |> Jsonaf.string_exn);
  let selected =
    member "transaction_payload"
    |> Jsonaf.string_exn
    |> Store.Transaction.decode
    |> Existing.store_ok
  in
  [%test_eq: string]
    (member "transaction_sha256" |> Jsonaf.string_exn)
    (Store.Transaction.hash selected);
  let before = Callback.decode_state (member "before") in
  let expected = expected_state before selected in
  (match boundary with
   | Writer_unknown ->
     let expected_error =
       Store.Store_error.Io
         { operation = "append journal segment"
         ; path = member "journal_path" |> Jsonaf.string_exn
         ; message = member "injected" |> Jsonaf.string_exn
         }
       |> Existing_fault.persistence_error
     in
     Existing_fault.same_error
       expected_error
       (member "primary" |> P.Error.of_json |> protocol_ok)
   | Publication ->
     let sentinel = Exn.to_string (Failure "597 configuration publication sentinel") in
     [%test_eq: string] sentinel (member "injected" |> Jsonaf.string_exn);
     assert (Jsonaf.exactly_equal (`Array [ `String sentinel ]) (member "fatal_causes")));
  let store =
    Store.Session_store.open_existing
      ~env
      ~sw
      ~root:(Filename.concat root "data")
      ~process_start_identity:None
      ~lock_nonce:"597-unmoderated-configuration-recovery"
    |> Existing.store_ok
  in
  protect_cleanup
    ~finally:(fun () -> Store.Session_store.close store |> Existing.store_ok)
    (fun () ->
       let handle =
         Store.Session_store.open_session
           store
           ~sw
           ~actor_lock_nonce:"597-configuration-recovered"
           selected.session_id
         |> Existing.store_ok
       in
       protect_cleanup
         ~finally:(fun () ->
           Store.Session_store.close_session store handle |> Existing.store_ok)
         (fun () ->
            assert (
              Jsonaf.exactly_equal
                (member "initial_snapshot")
                (Physical.snapshot_anchor env handle));
            let original = Frames.frames env handle in
            let actual =
              List.filter original ~f:(fun (flags, payload, _) ->
                Int.equal flags 0
                && Int64.equal
                     (Store.Transaction.decode payload |> Existing.store_ok)
                       .transaction_sequence
                     selected.transaction_sequence)
              |> Delegated_policy_claim_tests.one "same physical configuration frame"
            in
            let flags, payload, checksum = actual in
            [%test_eq: int] 0 flags;
            [%test_eq: string] (member "transaction_payload" |> Jsonaf.string_exn) payload;
            [%test_eq: string] (member "frame_checksum" |> Jsonaf.string_exn) checksum;
            (* frames captures structural seals too. Preserve the complete
               ordered prefix; only transaction selection filters frame flags. *)
            let prefix =
              List.take_while original ~f:(fun (flags, earlier_payload, _) ->
                not (Int.equal flags 0 && String.equal earlier_payload payload))
            in
            assert (Jsonaf.exactly_equal (member "before_frames") (frames_json prefix));
            [%test_eq: int] (List.length prefix + 1) (List.length original);
            let predecessor = Frames.transactions prefix |> List.last_exn in
            [%test_eq: int64]
              before.counters.transaction_sequence
              predecessor.transaction_sequence;
            [%test_eq: string option]
              (Some (Store.Transaction.hash predecessor))
              selected.previous_transaction_hash;
            let snapshot =
              Physical.current_snapshot
                env
                ~directory:(Store.Session_store.Handle.snapshot_directory handle)
            in
            let read =
              Physical.recovery_reader
                env
                handle
                ~session_id:selected.session_id
                ~initial_snapshot:snapshot.snapshot
            in
            let recovered = read () in
            assert (not recovered.repaired_crash_tail);
            [%test_eq: int64]
              selected.transaction_sequence
              recovered.latest_transaction_sequence;
            [%test_eq: string option]
              (Some (Store.Transaction.hash selected))
              recovered.latest_transaction_hash;
            [%test_eq: int64] selected.session_revision recovered.latest_session_revision;
            let current = A.Session_persistence.Restored.state recovered.state in
            assert (
              Jsonaf.exactly_equal
                (Existing_fault.state_json expected)
                (Existing_fault.state_json current));
            assert (
              Jsonaf.exactly_equal
                (Existing_fault.state_json expected)
                (Existing_fault.state_json
                   (A.Session_persistence.Restored.state (read ()).state)));
            [%test_eq: (int * string * string) list] original (Frames.frames env handle);
            assert (
              Jsonaf.exactly_equal
                (member "initial_snapshot")
                (Physical.snapshot_anchor env handle))))
;;

let%expect_test
    "unmoderated binding distinguishes physical configuration uncertainty and installed \
     publication"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      List.iter [ Boundary.Writer_unknown; Publication ] ~f:(fun boundary ->
        let root = temporary_root env in
        protect_cleanup
          ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
          (fun () ->
             Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 120. (fun () ->
               let report =
                 Callback.child_report ~mode:(Boundary.mode boundary) env root
               in
               recover env sw root boundary report)))));
  print_endline
    "actual old binding retained before ACK; installed replacement precedes fatal \
     publication; exact configuration replay without runtime";
  [%expect
    {| actual old binding retained before ACK; installed replacement precedes fatal publication; exact configuration replay without runtime |}]
;;
