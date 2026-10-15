open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module T = R.Tracking
module O = Inference.Observation
module S = Agent_store.Session_store
module F = Developer_run_store_tests
module Selected = Developer_run_selected_tests
module External = Developer_external_model_tests

let configuration =
  O.Configuration.of_target
    External.target
    ~preparation_id:"tracking-test"
    ~transport:O.Configuration.In_process
    ~capabilities:[]
    ~limits:External.generous
  |> F.ok
;;

let installed = function
  | R.Settlement.Installed value -> R.Installed.observation value
  | Absent | Prewrite_conflict | Diverged -> failwith "expected installed document"
;;

let complete registration =
  Inference.Event.Terminal.create
    ~scope:(T.Registration.scope registration)
    ~delivery:Response_started
    ~outcome:Completed
  |> F.ok
;;

let usage_observation registration ~revision ~input =
  let actual value = O.Count.create (Actual value) |> F.ok in
  let unknown = O.Count.create (Unknown Not_reported) |> F.ok in
  let counts =
    O.Usage.
      { input = actual input
      ; output = actual 0L
      ; reported_total = unknown
      ; cached_input = unknown
      ; cache_write_input = unknown
      ; reasoning_output = unknown
      }
  in
  let usage = O.Usage.create ~counts ~inclusions:[] |> F.ok in
  O.create
    ~scope:(T.Registration.scope registration)
    ~id:(T.Registration.accounting_id registration)
    ~revision
    ~payload:(Usage usage)
    ~limits:O.Admission.observation
  |> F.ok
;;

let document_revision path =
  match Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn with
  | `Object fields ->
    (match List.Assoc.find_exn fields "payload" ~equal:String.equal with
     | `Object payload ->
       (match List.Assoc.find_exn payload "revision" ~equal:String.equal with
        | `String value -> Int64.of_string value
        | _ -> failwith "expected numeric document revision")
     | _ -> failwith "expected document payload")
  | _ -> failwith "expected document envelope"
;;

let saved_usage_revision view =
  match O.Attempt_record.observations (R.Inference_row.record (T.View.row view)) with
  | [ observation ] -> O.revision observation
  | _ -> failwith "expected one authoritative usage observation"
;;

let update t ~claim registration change =
  let previous = T.observe t registration |> F.run_ok in
  match T.prepare_change t ~claim ~registration ~previous ~change |> F.run_ok with
  | T.Unchanged _ -> failwith "expected changed inference row"
  | Prepared pending ->
    (match T.commit t pending ~admit:(fun () -> true) |> F.run_ok with
     | T.Updated view -> view
     | Admitted _ -> failwith "change created another registration")
;;

let authority env root owner =
  Eio.Path.(
    Eio.Stdenv.fs env
    / root
    / "developer-runs"
    / ("run_" ^ P.Id.Developer_run.to_string owner.P.Execution_owner.run_id ^ ".json"))
;;

let saved_record json =
  let field json name =
    match json with
    | `Object fields -> List.Assoc.find_exn fields ~equal:String.equal name
    | _ -> failwith "expected object"
  in
  match field (field json "payload") "inference_attempts" with
  | `Array [ row ] -> field row "record"
  | _ -> failwith "expected one persisted inference row"
;;

let inference_rows path =
  match Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn with
  | `Object fields ->
    (match List.Assoc.find_exn fields "payload" ~equal:String.equal with
     | `Object payload ->
       (match List.Assoc.find_exn payload "inference_attempts" ~equal:String.equal with
        | `Array rows -> rows
        | _ -> failwith "expected inference rows")
     | _ -> failwith "expected payload")
  | _ -> failwith "expected document"
;;

let interrupted_row row =
  let fields =
    match row with
    | `Object fields -> fields
    | _ -> failwith "expected inference row"
  in
  let prior =
    List.Assoc.find_exn fields "record" ~equal:String.equal
    |> fun json -> O.Attempt_record.of_json json ~limits:O.Admission.attempt |> F.ok
  in
  let delivery =
    match O.Attempt_record.state prior with
    | Prepared -> Inference.Event.Terminal.Definitely_not_submitted
    | Running -> Possibly_submitted
    | Terminal _ | Interrupted _ -> failwith "expected active inference row"
  in
  let after =
    O.Attempt_record.create
      ~scope:(O.Attempt_record.scope prior)
      ~accounting_id:(O.Attempt_record.accounting_id prior)
      ~configuration:(O.Attempt_record.configuration prior)
      ~state:(Interrupted { reason = Host_interrupted; delivery })
      ~observations:(O.Attempt_record.observations prior)
      ~omitted_diagnostics:(O.Attempt_record.omitted_diagnostics prior)
      ~limits:O.Admission.attempt
    |> F.ok
  in
  let row =
    `Object
      (List.map fields ~f:(fun (key, value) ->
         if String.equal key "record"
         then key, O.Attempt_record.to_json after
         else key, value))
  in
  row
;;

let interrupted_row_bytes row =
  Document_schema.Json.validate_and_measure
    ~limits:Document_schema.Limits.default
    (interrupted_row row)
  |> F.ok
;;

let interrupted_document_bytes path =
  let saved = Eio.Path.load path |> Jsonaf.parse |> Or_error.ok_exn in
  let envelope =
    match saved with
    | `Object fields -> fields
    | _ -> failwith "expected developer document envelope"
  in
  let payload =
    match List.Assoc.find_exn envelope "payload" ~equal:String.equal with
    | `Object fields -> fields
    | _ -> failwith "expected developer document payload"
  in
  let rows =
    match List.Assoc.find_exn payload "inference_attempts" ~equal:String.equal with
    | `Array rows -> rows
    | _ -> failwith "expected inference array"
  in
  let payload =
    `Object
      (List.map payload ~f:(fun (name, value) ->
         if String.equal name "inference_attempts"
         then name, `Array (List.map rows ~f:interrupted_row)
         else if String.equal name "revision"
         then
           ( name
           , `String
               (Int64.to_string
                  Int64.(
                    of_string
                      (match value with
                       | `String text -> text
                       | _ -> failwith "expected decimal revision")
                    + of_int (List.length rows))) )
         else name, value))
  in
  let envelope =
    `Object
      (List.map envelope ~f:(fun (name, value) ->
         if String.equal name "payload" then name, payload else name, value))
  in
  Document_schema.Document.inspect ~limits:Document_schema.Limits.default envelope
  |> F.ok
  |> Document_schema.Document.to_string
  |> String.length
;;

let record_state json =
  O.Attempt_record.of_json (saved_record json) ~limits:O.Admission.attempt
  |> F.ok
  |> O.Attempt_record.state
;;

let%test_unit "all active inference rows retain aggregate recovery capacity" =
  let exercise ~max_document_bytes ~max_inference_bytes ~second_expected =
    F.with_root
      ~max_document_bytes
      ~max_inference_bytes_per_run:max_inference_bytes
      (fun env _sw root _session store ->
         let owner = F.owner 0 in
         let capture = F.digest "inference aggregate reserve" in
         let handle, initial = F.admit store owner capture in
         let job =
           External.model_job
             owner
             capture
             ~name:"job_inference_reserve"
             ~parent:Run_root
             ~payload:(P.Developer_model_call.to_json (External.call External.request))
         in
         let saved =
           R.prepare_external_model
             handle
             ~expected:initial
             ~parent:R.External_model_parent.Run_root
             ~job
           |> F.run_ok
           |> External.save handle
         in
         let run_claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
         let recovery = R.Recovery.open_ ~claim:run_claim ~handle |> F.run_ok in
         R.Recovery.finish recovery |> F.run_ok;
         let manager = T.open_ ~claim:run_claim ~handle |> F.run_ok in
         let claim =
           R.prepare_job_claim
             handle
             ~expected:saved
             ~lookup:(R.find_job_exact saved (P.Developer_job.id job))
             ~now:Selected.timestamp
           |> F.run_ok
           |> R.commit_job_claim handle
           |> F.run_ok
           |> R.Claimed.claim
         in
         assert (
           R.Claim.try_enter
             claim
             ~handle
             ~admit:(fun () -> true)
             ~on_enter:(fun () -> ())
           |> F.run_ok);
         let admit () =
           let pending =
             T.prepare_admit manager ~claim ~relation:T.Root ~configuration |> F.run_ok
           in
           match T.commit manager pending ~admit:(fun () -> true) |> F.run_ok with
           | T.Admitted registration -> registration
           | Updated _ -> failwith "reserve admission changed an existing row"
         in
         let first = admit () in
         let second =
           match T.prepare_admit manager ~claim ~relation:T.Root ~configuration with
           | Error R.Error.Limit_exceeded when not second_expected -> None
           | Ok pending when second_expected ->
             (match T.commit manager pending ~admit:(fun () -> true) |> F.run_ok with
              | T.Admitted registration -> Some registration
              | Updated _ -> failwith "reserve admission updated an old row")
           | Ok _ | Error _ -> failwith "unexpected inference recovery reserve decision"
         in
         let required =
           match second with
           | None -> None
           | Some _ ->
             let rows = inference_rows (authority env root owner) in
             assert (Int.equal (List.length rows) 2);
             let required_document =
               interrupted_document_bytes (authority env root owner)
             in
             let tight_document =
               Document_schema.Limits.create
                 ~max_bytes:(required_document - 1)
                 ~max_depth:16
                 ~max_fields:((required_document - 1) / 16)
                 ~max_nodes:((required_document - 1) / 8)
               |> F.ok
             in
             assert (
               Result.is_ok
                 (Document_schema.Document.decode
                    ~limits:tight_document
                    (Eio.Path.load (authority env root owner))));
             Some (List.sum (module Int) rows ~f:interrupted_row_bytes, required_document)
         in
         List.iter (first :: Option.to_list second) ~f:(fun registration ->
           ignore
             (update
                manager
                ~claim
                registration
                (T.Interrupted
                   { reason = Host_interrupted; delivery = Definitely_not_submitted })
              : T.View.t));
         let current = R.observe handle |> F.run_ok in
         let state = P.Developer_job.state (R.Claim.job claim) in
         let after : P.Developer_job.State.t =
           { state with
             status = Interrupted "fixture worker stopped"
           ; completed_at = Some Selected.timestamp
           }
         in
         let terminal =
           R.prepare_claimed_transition
             handle
             ~expected:current
             ~claim
             ~after
             ~revoked_after:after
           |> F.run_ok
         in
         ignore (R.commit_claimed_transition handle terminal |> F.run_ok : R.Installed.t);
         let terminal_document_bytes =
           Eio.Path.load (authority env root owner) |> String.length
         in
         T.finish manager |> F.run_ok;
         R.Run_claim.release run_claim |> F.run_ok;
         Option.map required ~f:(fun (inference_bytes, document_bytes) ->
           inference_bytes, document_bytes, terminal_document_bytes))
  in
  let required =
    exercise ~max_document_bytes:16384 ~max_inference_bytes:16384 ~second_expected:true
    |> Option.value_exn
  in
  let aggregate_bytes, document_bytes, terminal_document_bytes = required in
  assert (aggregate_bytes > 1 && document_bytes - 1 > aggregate_bytes);
  ignore
    (exercise
       ~max_document_bytes:16384
       ~max_inference_bytes:(aggregate_bytes - 1)
       ~second_expected:false
     : (int * int * int) option);
  assert (
    [%equal: int * int * int]
      (exercise
         ~max_document_bytes:16384
         ~max_inference_bytes:aggregate_bytes
         ~second_expected:true
       |> Option.value_exn)
      required);
  ignore
    (exercise
       ~max_document_bytes:(document_bytes - 1)
       ~max_inference_bytes:(document_bytes - 1)
       ~second_expected:false
     : (int * int * int) option);
  ignore
    (exercise
       ~max_document_bytes:(Int.max document_bytes terminal_document_bytes)
       ~max_inference_bytes:(Int.max document_bytes terminal_document_bytes)
       ~second_expected:true
     : (int * int * int) option)
;;

let%test_unit
    "tracking uses exact Claim, CAS, delayed nested relation and terminal cutoff"
  =
  F.with_root ~max_document_bytes:16384 (fun env _sw root _session store ->
    let owner = F.owner 0 in
    let file = authority env root owner in
    let capture = F.digest "developer inference tracking" in
    let handle, initial = F.admit store owner capture in
    let job =
      External.model_job
        owner
        capture
        ~name:"job_tracking_model"
        ~parent:Run_root
        ~payload:(P.Developer_model_call.to_json (External.call External.request))
    in
    let original =
      R.prepare_external_model
        handle
        ~expected:initial
        ~parent:R.External_model_parent.Run_root
        ~job
      |> F.run_ok
      |> External.save handle
    in
    let run_claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
    let recovery = R.Recovery.open_ ~claim:run_claim ~handle |> F.run_ok in
    R.Recovery.finish recovery |> F.run_ok;
    let manager = T.open_ ~claim:run_claim ~handle |> F.run_ok in
    assert (Result.is_error (T.open_ ~claim:run_claim ~handle));
    let claimed =
      R.prepare_job_claim
        handle
        ~expected:original
        ~lookup:(R.find_job_exact original (P.Developer_job.id job))
        ~now:Selected.timestamp
      |> F.run_ok
      |> R.commit_job_claim handle
      |> F.run_ok
    in
    let claim = R.Claimed.claim claimed in
    assert (
      R.Claim.try_enter claim ~handle ~admit:(fun () -> true) ~on_enter:(fun () -> ())
      |> F.run_ok);
    let first =
      T.prepare_admit manager ~claim ~relation:T.Root ~configuration |> F.run_ok
    in
    let unrelated =
      External.model_job
        owner
        capture
        ~name:"job_tracking_unrelated"
        ~parent:Run_root
        ~payload:(P.Developer_model_call.to_json (External.call External.request))
    in
    let current = R.observe handle |> F.run_ok in
    let other =
      R.prepare_external_model
        handle
        ~expected:current
        ~parent:R.External_model_parent.Run_root
        ~job:unrelated
      |> F.run_ok
    in
    ignore (External.save handle other : R.Observation.t);
    (match T.commit manager first ~admit:(fun () -> true) with
     | Error R.Error.Prewrite_conflict -> ()
     | Ok _ | Error _ -> failwith "tracking did not detect prewrite conflict");
    (match T.reconcile manager first with
     | Error R.Error.Prewrite_conflict -> ()
     | Ok _ | Error _ -> failwith "tracking lost retired prewrite evidence");
    let first = T.reprepare_same manager first |> F.run_ok in
    let root_registration =
      match T.commit manager first ~admit:(fun () -> true) |> F.run_ok with
      | T.Admitted registration -> registration
      | Updated _ -> failwith "new admission returned update"
    in
    let admitted = T.Registration.row root_registration in
    assert (Int64.equal (R.Inference_row.ordinal admitted) 1L);
    ignore (update manager ~claim root_registration T.Started : T.View.t);
    let started = T.observe manager root_registration |> F.run_ok in
    (match
       T.prepare_change
         manager
         ~claim
         ~registration:root_registration
         ~previous:started
         ~change:T.Started
       |> F.run_ok
     with
     | T.Unchanged _ -> ()
     | Prepared _ -> failwith "duplicate start advanced the row");
    let first_usage = usage_observation root_registration ~revision:0L ~input:1L in
    let observed = update manager ~claim root_registration (T.Observed first_usage) in
    assert (Int64.equal (saved_usage_revision observed) 0L);
    let revision_after_add = document_revision file in
    (match
       T.prepare_change
         manager
         ~claim
         ~registration:root_registration
         ~previous:observed
         ~change:(T.Observed first_usage)
       |> F.run_ok
     with
     | T.Unchanged view ->
       assert (Int64.equal (saved_usage_revision view) 0L);
       assert (Int64.equal (document_revision file) revision_after_add)
     | Prepared _ -> failwith "duplicate observation wrote another revision");
    let newer = usage_observation root_registration ~revision:1L ~input:2L in
    let observed = update manager ~claim root_registration (T.Observed newer) in
    assert (Int64.equal (saved_usage_revision observed) 1L);
    assert (Int64.(document_revision file > revision_after_add));
    let revision_after_replace = document_revision file in
    (match
       T.prepare_change
         manager
         ~claim
         ~registration:root_registration
         ~previous:observed
         ~change:(T.Observed first_usage)
       |> F.run_ok
     with
     | T.Unchanged view ->
       assert (Int64.equal (saved_usage_revision view) 1L);
       assert (Int64.equal (document_revision file) revision_after_replace)
     | Prepared _ -> failwith "stale observation wrote another revision");
    let conflicting = usage_observation root_registration ~revision:1L ~input:3L in
    assert (
      match
        T.prepare_change
          manager
          ~claim
          ~registration:root_registration
          ~previous:observed
          ~change:(T.Observed conflicting)
      with
      | Error R.Error.Conflict -> true
      | Ok _ | Error _ -> false);
    ignore
      (update manager ~claim root_registration (T.Finished (complete root_registration))
       : T.View.t);
    let late = usage_observation root_registration ~revision:2L ~input:4L in
    let ended = update manager ~claim root_registration (T.Observed late) in
    assert (Int64.equal (saved_usage_revision ended) 2L);
    (match O.Attempt_record.state (R.Inference_row.record (T.View.row ended)) with
     | Terminal _ -> ()
     | Prepared | Running | Interrupted _ ->
       failwith "late observation rewrote terminal truth");
    (match
       T.prepare_change
         manager
         ~claim
         ~registration:root_registration
         ~previous:ended
         ~change:(T.Finished (complete root_registration))
       |> F.run_ok
     with
     | T.Unchanged _ -> ()
     | Prepared _ -> failwith "duplicate terminal advanced the row");
    assert (
      Result.is_error
        (T.prepare_change
           manager
           ~claim
           ~registration:root_registration
           ~previous:ended
           ~change:T.Started));
    let nested =
      T.prepare_admit
        manager
        ~claim
        ~relation:
          (T.Nested
             { parent = root_registration; call_entry_id = None; call_alias = None })
        ~configuration
      |> F.run_ok
    in
    let child_registration =
      match T.commit manager nested ~admit:(fun () -> true) |> F.run_ok with
      | T.Admitted registration -> registration
      | Updated _ -> failwith "nested admission returned update"
    in
    assert (
      Int64.equal (R.Inference_row.ordinal (T.Registration.row child_registration)) 2L);
    ignore (update manager ~claim child_registration T.Started : T.View.t);
    ignore
      (update manager ~claim child_registration (T.Finished (complete child_registration))
       : T.View.t);
    let current = R.observe handle |> F.run_ok in
    let state = P.Developer_job.state (R.Claim.job claim) in
    let after : P.Developer_job.State.t =
      { state with
        status = Succeeded
      ; completed_at = Some Selected.timestamp
      ; result = Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
      }
    in
    let terminal =
      R.prepare_claimed_transition
        handle
        ~expected:current
        ~claim
        ~after
        ~revoked_after:after
      |> F.run_ok
    in
    ignore (R.commit_claimed_transition handle terminal |> F.run_ok : R.Installed.t);
    assert (
      match T.prepare_admit manager ~claim ~relation:T.Root ~configuration with
      | Error R.Error.Foreign_or_stale -> true
      | Ok _ | Error _ -> false);
    T.finish manager |> F.run_ok;
    R.Run_claim.release run_claim |> F.run_ok)
;;

let%test_unit
    "restart interrupts the exact retained inference row before recovery finishes"
  =
  F.with_temp_directory "ochat-developer-inference-restart" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let owner = F.owner 0 in
    let capture = F.digest "inference recovery capture" in
    Eio.Switch.run (fun sw ->
      let session = F.create_session env sw root in
      let store = F.open_run session sw ~max_document_bytes:16384 () in
      let handle, initial = F.admit store owner capture in
      let job =
        External.model_job
          owner
          capture
          ~name:"job_inference_recovery"
          ~parent:Run_root
          ~payload:(P.Developer_model_call.to_json (External.call External.request))
      in
      let job =
        P.Developer_job.create
          ~owner
          ~capture_fingerprint:capture
          ~id:(P.Developer_job.id job)
          ~launch:(P.Developer_job.launch job)
          ~kind:(P.Developer_job.kind job)
          ~payload:(P.Developer_job.payload job)
          ~retry_policy:(P.Job.Safe_retry { max_attempts = 2; backoff_ms = 0 })
          ~created_at:(P.Developer_job.created_at job)
          ~state:(P.Developer_job.state job)
        |> F.ok
      in
      let observed =
        R.prepare_external_model
          handle
          ~expected:initial
          ~parent:R.External_model_parent.Run_root
          ~job
        |> F.run_ok
        |> External.save handle
      in
      let run_claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
      let recovery = R.Recovery.open_ ~claim:run_claim ~handle |> F.run_ok in
      R.Recovery.finish recovery |> F.run_ok;
      let manager = T.open_ ~claim:run_claim ~handle |> F.run_ok in
      let claim =
        R.prepare_job_claim
          handle
          ~expected:observed
          ~lookup:(R.find_job_exact observed (P.Developer_job.id job))
          ~now:Selected.timestamp
        |> F.run_ok
        |> R.commit_job_claim handle
        |> F.run_ok
        |> R.Claimed.claim
      in
      assert (
        R.Claim.try_enter claim ~handle ~admit:(fun () -> true) ~on_enter:(fun () -> ())
        |> F.run_ok);
      let pending =
        T.prepare_admit manager ~claim ~relation:T.Root ~configuration |> F.run_ok
      in
      let registration =
        match T.commit manager pending ~admit:(fun () -> true) |> F.run_ok with
        | T.Admitted registration -> registration
        | Updated _ -> failwith "admission did not register"
      in
      ignore (update manager ~claim registration T.Started : T.View.t);
      ignore
        (update manager ~claim registration (T.Finished (complete registration))
         : T.View.t);
      let current = R.observe handle |> F.run_ok in
      let state = P.Developer_job.state (R.Claim.job claim) in
      let after : P.Developer_job.State.t =
        { state with
          status = Succeeded
        ; completed_at = Some Selected.timestamp
        ; result =
            Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded `Null))
        }
      in
      let terminal =
        R.prepare_claimed_transition
          handle
          ~expected:current
          ~claim
          ~after
          ~revoked_after:after
        |> F.run_ok
      in
      ignore (R.commit_claimed_transition handle terminal |> F.run_ok : R.Installed.t);
      T.finish manager |> F.run_ok;
      R.Run_claim.release run_claim |> F.run_ok;
      R.close_and_wait store |> F.run_ok;
      S.close session |> F.store_ok);
    let file = authority env root owner in
    let saved = Eio.Path.load file |> Jsonaf.parse |> Or_error.ok_exn in
    assert (
      match record_state saved with
      | O.Attempt_record.Terminal _ -> true
      | Prepared | Running | Interrupted _ -> false);
    let changed =
      F.replace_json_field saved ~name:"payload" ~f:(fun payload ->
        let payload =
          F.replace_json_field payload ~name:"inference_attempts" ~f:(function
            | `Array [ row ] ->
              `Array
                [ F.replace_json_field row ~name:"record" ~f:(fun record ->
                    F.replace_json_field record ~name:"state" ~f:(fun _ ->
                      `Object [ "kind", `String "running" ]))
                ]
            | _ -> failwith "expected one saved inference row")
        in
        F.replace_json_field payload ~name:"jobs" ~f:(function
          | `Array [ encoded ] ->
            let job = P.Developer_job.of_json encoded |> F.ok in
            let state : P.Developer_job.State.t =
              { (P.Developer_job.state job) with
                status = Running
              ; attempt = 2
              ; started_at = Some Selected.timestamp
              ; completed_at = None
              ; result = None
              }
            in
            let later = P.Developer_job.with_state job state |> F.ok in
            `Array [ P.Developer_job.to_json later ]
          | _ -> failwith "expected one saved model job"))
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) file (Jsonaf.to_string changed);
    Eio.Switch.run (fun sw ->
      let session =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"inference-recovery-reopened"
        |> F.store_ok
      in
      let store = F.open_run session sw ~max_document_bytes:16384 () in
      let handle, _ = F.admit store owner capture in
      let claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
      let recovery = R.Recovery.open_ ~claim ~handle |> F.run_ok in
      let view = R.Recovery.observe recovery |> F.run_ok in
      let candidate = R.Recovery.View.inference_candidates view |> List.hd_exn in
      let pending =
        R.Recovery.prepare_inference_interruption recovery ~view ~candidate |> F.run_ok
      in
      (match R.Recovery.reconcile_inference_interruption recovery pending |> F.run_ok with
       | R.Absent -> ()
       | Installed _ | Diverged -> failwith "unwritten recovery was not absent");
      let pending = R.Recovery.reprepare_inference_same recovery pending |> F.run_ok in
      ignore
        (R.Recovery.commit_inference_interruption recovery pending |> F.run_ok
         : R.Installed.t);
      let view = R.Recovery.observe recovery |> F.run_ok in
      let job_candidate = R.Recovery.View.candidates view |> List.hd_exn in
      let job = R.Recovery.Candidate.job job_candidate in
      let after : P.Developer_job.State.t =
        { (P.Developer_job.state job) with
          status = Interrupted "recovered after restart"
        ; completed_at = Some Selected.timestamp
        ; result = None
        ; retry_failure = None
        }
      in
      let pending =
        R.Recovery.prepare_terminal recovery ~view ~candidate:job_candidate ~after
        |> F.run_ok
      in
      ignore
        (R.Recovery.commit_terminal recovery pending ~admit:(fun () -> true) |> F.run_ok
         : R.Installed.t);
      assert (
        List.is_empty
          (R.Recovery.View.inference_candidates (R.Recovery.observe recovery |> F.run_ok)));
      R.Recovery.finish recovery |> F.run_ok;
      R.Run_claim.release claim |> F.run_ok;
      R.close_and_wait store |> F.run_ok;
      S.close session |> F.store_ok);
    let recovered = Eio.Path.load file |> Jsonaf.parse |> Or_error.ok_exn in
    match record_state recovered with
    | O.Attempt_record.Interrupted
        { reason = Host_interrupted; delivery = Possibly_submitted } -> ()
    | Prepared | Running | Terminal _ | Interrupted _ ->
      failwith "recovery did not retain possibly submitted interruption")
;;

let%test_unit "uncertain inference admission retains one exact token until reconciliation"
  =
  F.with_temp_directory "ochat-developer-inference-uncertain" (fun env temporary ->
    let rename_armed = ref false in
    let sync_blocked = ref false in
    let parent_sync_blocked = ref false in
    let fault_env =
      F.fault_after_authority_rename_env
        ~after_rename:(fun () -> sync_blocked := true)
        env
        ~rename_armed
        ~sync_blocked
        ~parent_sync_blocked
    in
    Eio.Switch.run (fun sw ->
      let session = F.create_session fault_env sw (Filename.concat temporary "data") in
      let store = F.open_run session sw ~max_document_bytes:16384 () in
      let owner = F.owner 0 in
      let capture = F.digest "inference uncertain" in
      let handle, initial = F.admit store owner capture in
      let job =
        External.model_job
          owner
          capture
          ~name:"job_tracking_uncertain"
          ~parent:Run_root
          ~payload:(P.Developer_model_call.to_json (External.call External.request))
      in
      let observed =
        R.prepare_external_model
          handle
          ~expected:initial
          ~parent:R.External_model_parent.Run_root
          ~job
        |> F.run_ok
        |> External.save handle
      in
      let run_claim = R.Run_claim.acquire ~store ~handle |> F.run_ok in
      let recovery = R.Recovery.open_ ~claim:run_claim ~handle |> F.run_ok in
      R.Recovery.finish recovery |> F.run_ok;
      let manager = T.open_ ~claim:run_claim ~handle |> F.run_ok in
      let claim =
        R.prepare_job_claim
          handle
          ~expected:observed
          ~lookup:(R.find_job_exact observed (P.Developer_job.id job))
          ~now:Selected.timestamp
        |> F.run_ok
        |> R.commit_job_claim handle
        |> F.run_ok
        |> R.Claimed.claim
      in
      assert (
        R.Claim.try_enter claim ~handle ~admit:(fun () -> true) ~on_enter:(fun () -> ())
        |> F.run_ok);
      let pending =
        T.prepare_admit manager ~claim ~relation:T.Root ~configuration |> F.run_ok
      in
      rename_armed := true;
      (match T.commit manager pending ~admit:(fun () -> true) with
       | Error (R.Error.Store (Agent_store.Store_error.Io _)) -> ()
       | Ok _ | Error _ -> failwith "post-rename fault was acknowledged as durable");
      assert (Option.is_some (T.pending manager));
      assert (Result.is_error (R.Run_claim.release run_claim));
      sync_blocked := false;
      let other =
        R.prepare_checkpoint
          handle
          ~expected:(R.observe handle |> F.run_ok)
          ~phase:R.Checkpoint.Active
        |> F.run_ok
      in
      assert (Result.is_error (R.commit handle other));
      let registration =
        match T.reconcile manager pending |> F.run_ok with
        | T.Tracking_installed (T.Admitted registration) -> registration
        | Tracking_installed (Updated _) | Tracking_absent | Tracking_diverged ->
          failwith "exact installed inference admission was not recovered"
      in
      assert (Option.is_none (T.pending manager));
      let previous = T.observe manager registration |> F.run_ok in
      let interrupted =
        T.prepare_change
          manager
          ~claim
          ~registration
          ~previous
          ~change:
            (T.Interrupted
               { reason = O.Attempt_record.Cancelled
               ; delivery = Inference.Event.Terminal.Definitely_not_submitted
               })
        |> F.run_ok
      in
      (match interrupted with
       | T.Prepared pending ->
         ignore (T.commit manager pending ~admit:(fun () -> true) |> F.run_ok : T.receipt)
       | Unchanged _ -> failwith "prepared inference was not interrupted");
      let current = R.observe handle |> F.run_ok in
      let state = P.Developer_job.state (R.Claim.job claim) in
      let after : P.Developer_job.State.t =
        { state with
          status = Cancelled
        ; completed_at = Some Selected.timestamp
        ; result = None
        }
      in
      let terminal =
        R.prepare_claimed_transition
          handle
          ~expected:current
          ~claim
          ~after
          ~revoked_after:after
        |> F.run_ok
      in
      ignore (R.commit_claimed_transition handle terminal |> F.run_ok : R.Installed.t);
      T.finish manager |> F.run_ok;
      R.Run_claim.release run_claim |> F.run_ok;
      R.close_and_wait store |> F.run_ok;
      S.close session |> F.store_ok))
;;
