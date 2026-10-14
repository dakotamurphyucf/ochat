open! Core
module P = Agent_protocol
module R = Agent_store.Developer_run_store
module S = Agent_store.Session_store
module B = Agent_store.Blob_store
module Publisher = Agent_store.Developer_result_publisher
module Recovery = Agent_store.Developer_result_recovery
module F = Developer_run_store_tests
module Selected = Developer_run_selected_tests

let blobs env session =
  let root = S.data_root session in
  B.create
    ~env
    ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path root)
    ~durable_directory:(Agent_store.Data_root.durable_blobs_path root)
    ~max_upload_bytes:65536L
  |> F.store_ok
;;

let selected_job handle initial owner capture id =
  let job = Selected.job owner capture ~id ~parent:P.Developer_job.Launch.Run_root in
  let prepared =
    R.prepare_root_selected
      handle
      ~expected:initial
      ~phase:R.Checkpoint.Active
      ~selected:[ job ]
    |> F.run_ok
  in
  let observed =
    R.commit_and_settle handle prepared (Selected.handoff ()) ~admit:(fun () -> true)
    |> F.run_ok
    |> Selected.installed
  in
  let prepared =
    R.prepare_job_claim
      handle
      ~expected:observed
      ~lookup:(R.find_job_exact observed id)
      ~now:Selected.timestamp
    |> F.run_ok
  in
  R.commit_job_claim handle prepared |> F.run_ok
;;

type seeded =
  { owner : P.Execution_owner.developer
  ; capture : Chatml_source.Digest.t
  ; id : P.Id.Job.t
  ; blob : P.Id.Blob.t
  ; payload : Jsonaf.t
  ; temporary_metadata : string
  }

let seed_artifact
      ?(payload = `String "exact recovered bytes")
      ?(poison_nonterminal = false)
      ~env
      ~root
      ~installed
      ()
  =
  Eio.Switch.run (fun sw ->
    let session = F.create_session env sw root in
    let runs = F.open_run session sw () in
    let owner = F.owner 0 in
    let capture = F.digest "developer result recovery inventory" in
    let id = Selected.job_id "job_developer_result_recovery" in
    let handle, initial = F.admit runs owner capture in
    let result_root = R.Result_root.of_handle handle |> F.run_ok in
    let blobs = blobs env session in
    B.bind_developer_runs blobs ~session_store:session runs |> F.store_ok;
    let claimed = selected_job handle initial owner capture id in
    let claim = R.Claimed.claim claimed in
    assert (
      R.Claim.try_enter claim ~handle ~admit:(fun () -> true) ~on_enter:(fun () -> ())
      |> F.run_ok);
    let proposed =
      let state = P.Developer_job.state (R.Claim.job claim) in
      { state with
        status = P.Job.Succeeded
      ; completed_at = Some Selected.timestamp
      ; result =
          Some (P.Developer_job.Result_value.Inline (P.Completion.Succeeded payload))
      }
    in
    let limits =
      Publisher.Limits.create ~inline_bytes:0 ~max_result_bytes:65536 ~max_pending:1
      |> F.ok
    in
    let publisher = Publisher.create ~sw ~runs ~root:result_root ~blobs ~limits |> F.ok in
    let selected =
      Publisher.select
        publisher
        ~storage:Publisher.Storage.Artifact
        ~claim
        ~proposed
        ~selected_at:Selected.timestamp
      |> F.ok
    in
    Publisher.Selected.ensure_attempted selected |> F.ok;
    let blob =
      Publisher.Selected.reference selected
      |> Option.value_exn
      |> Agent_store.Developer_result_reference.blob
      |> fun (metadata : P.Blob.Metadata.t) -> metadata.id
    in
    let intent =
      Agent_store.Result_intent_file.developer
        ~root:result_root
        ~owner
        ~job:id
        ~attempt:(R.Claim.attempt claim)
      |> F.store_ok
    in
    let intent_bytes =
      Agent_store.Result_intent_file.read_exact intent ~max_frame_bytes:(128 * 1024)
      |> F.store_ok
      |> Option.value_exn
    in
    let document =
      Agent_store.Result_intent_file.decode
        intent_bytes
        ~limits:Agent_store.Developer_result_intent_document.limits
        ~max_frame_bytes:(128 * 1024)
      |> F.store_ok
      |> Agent_store.Developer_result_intent_document.of_document
      |> function
      | Ok document -> document
      | Error _ -> failwith "selected fixture intent did not decode"
    in
    let temporary_metadata =
      match Agent_store.Developer_result_intent_document.selection document with
      | Artifact { stage; _ } -> Agent_store.Blob_stage_documents.temporary_bytes stage
      | Inline _ -> failwith "selected fixture was not an Artifact"
    in
    if poison_nonterminal
    then (
      assert (not installed);
      let module Reference = Agent_store.Developer_result_reference in
      let module Intent = Agent_store.Developer_result_intent_document in
      let reference = Publisher.Selected.reference selected |> Option.value_exn in
      let invalid_content = "{}" in
      let blob : P.Blob.Metadata.t =
        { (Reference.blob reference) with
          byte_length = Int64.of_int (String.length invalid_content)
        ; digest = Digestif.SHA256.(digest_string invalid_content |> to_hex)
        }
      in
      let reference =
        Reference.create ~claim ~outcome:(Reference.outcome reference) ~blob |> F.store_ok
      in
      let old_stage =
        match Intent.selection document with
        | Artifact { stage; _ } -> stage
        | Inline _ -> failwith "selected fixture lost its stage"
      in
      let metadata =
        Agent_store.Blob_metadata_document.value
          (Agent_store.Blob_stage_documents.temporary old_stage)
      in
      let metadata : Agent_store.Blob_metadata.t =
        { metadata with blob; allowed_use = Reference.allowed_use reference }
      in
      let stage =
        Agent_store.Blob_metadata_document.create metadata
        |> F.ok
        |> Agent_store.Blob_stage_documents.create
        |> F.ok
      in
      let after : P.Developer_job.State.t =
        { (Publisher.Selected.after selected) with
          result =
            Some
              (P.Developer_job.Result_value.Artifact
                 { outcome = Reference.outcome reference; blob })
        }
      in
      let altered =
        Intent.create
          ~owner
          ~capture_fingerprint:capture
          ~job_id:id
          ~attempt:(R.Claim.attempt claim)
          ~selected_at:Selected.timestamp
          ~after
          ~selection:(Artifact { reference; stage })
        |> F.ok
        |> Intent.to_document
        |> F.ok
        |> fun document ->
        Agent_store.Result_intent_file.encode document ~limits:Intent.limits |> F.store_ok
      in
      let data_root = S.data_root session in
      let run = P.Id.Developer_run.to_string owner.run_id in
      let frame_path =
        Filename.concat
          (Filename.concat
             (Agent_store.Data_root.developer_result_intents_path data_root)
             ("run_" ^ run ^ "/epoch_" ^ Int.to_string owner.epoch))
          ("job_"
           ^ P.Id.Job.to_string id
           ^ "-"
           ^ Int.to_string (R.Claim.attempt claim)
           ^ ".frame")
      in
      let durable_directory =
        Filename.concat
          (Agent_store.Data_root.durable_blobs_path data_root)
          ("developer-runs/" ^ run)
      in
      let write path contents =
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(Eio.Stdenv.fs env / path)
          contents
      in
      write frame_path altered;
      write
        (Filename.concat durable_directory (P.Id.Blob.to_string blob.id ^ ".sexp"))
        (Agent_store.Blob_stage_documents.durable_bytes stage);
      write
        (Filename.concat durable_directory (P.Id.Blob.to_string blob.id ^ ".blob"))
        invalid_content);
    if installed
    then (
      let after = Publisher.Selected.after selected in
      let prepared =
        R.prepare_claimed_transition
          handle
          ~expected:(R.Claimed.observation claimed)
          ~claim
          ~after
          ~revoked_after:after
        |> F.run_ok
      in
      let installed = R.commit_claimed_transition handle prepared |> F.run_ok in
      Publisher.Selected.acknowledge_installed selected installed |> F.ok);
    R.close_and_wait runs |> F.run_ok;
    S.close session |> F.store_ok;
    { owner; capture; id; blob; payload; temporary_metadata })
;;

let with_inventory ~env ~root ~owner ~capture ~limits f =
  Eio.Switch.run (fun sw ->
    let session =
      S.open_existing
        ~env
        ~sw
        ~root
        ~process_start_identity:None
        ~lock_nonce:"developer-result-recovery-reopened"
      |> F.store_ok
    in
    let runs = F.open_run session sw () in
    let handle, _ = F.admit runs owner capture in
    let result_root = R.Result_root.of_handle handle |> F.run_ok in
    let blobs = blobs env session in
    B.bind_developer_runs blobs ~session_store:session runs |> F.store_ok;
    let claim = R.Run_claim.acquire ~store:runs ~handle |> F.run_ok in
    let window = R.Recovery.open_ ~claim ~handle |> F.run_ok in
    let view = R.Recovery.observe window |> F.run_ok in
    let recovery = Recovery.create ~sw ~runs ~root:result_root ~blobs ~limits |> F.ok in
    Exn.protect
      ~f:(fun () -> f ~env ~sw ~session ~runs ~handle ~blobs ~window ~view recovery)
      ~finally:(fun () ->
        let current = R.Recovery.observe window |> F.run_ok in
        (if List.is_empty (R.Recovery.View.candidates current)
         then R.Recovery.finish window
         else R.Recovery.abort_clean window)
        |> F.run_ok;
        R.Run_claim.release claim |> F.run_ok;
        R.close_and_wait runs |> F.run_ok;
        S.close session |> F.store_ok))
;;

let seed_other_running ~env ~root ~owner ~capture ~id =
  Eio.Switch.run (fun sw ->
    let session =
      S.open_existing
        ~env
        ~sw
        ~root
        ~process_start_identity:None
        ~lock_nonce:"developer-result-second-run"
      |> F.store_ok
    in
    let runs = F.open_run session sw () in
    let handle, initial = F.admit runs owner capture in
    ignore (selected_job handle initial owner capture id : R.Claimed.t);
    R.close_and_wait runs |> F.run_ok;
    S.close session |> F.store_ok)
;;

let limits ?(max_work = 100_000) () =
  Recovery.Limits.create
    ~max_entries:1024
    ~max_bytes:(2 * 1024 * 1024)
    ~max_work
    ~max_result_bytes:65536
  |> F.ok
;;

let blob_file data_root ~temporary run blob suffix =
  let parent =
    if temporary
    then Agent_store.Data_root.temporary_blobs_path data_root
    else Agent_store.Data_root.durable_blobs_path data_root
  in
  Filename.concat
    (Filename.concat parent ("developer-runs/" ^ P.Id.Developer_run.to_string run))
    (P.Id.Blob.to_string blob ^ suffix)
;;

let%expect_test "recovery inventories selected and markerless installed Artifact" =
  let rec nest count value =
    if count = 0 then value else nest (count - 1) (`Array [ value ])
  in
  List.iter
    [ false, false, `String "exact recovered bytes"
    ; true, false, `String "exact recovered bytes"
    ; false, true, nest 125 `Null
    ]
    ~f:(fun (installed, nested, payload) ->
      F.with_temp_directory "ochat-developer-result-recovery" (fun env temporary ->
        let root = Filename.concat temporary "data" in
        let seeded = seed_artifact ~payload ~env ~root ~installed () in
        with_inventory
          ~env
          ~root
          ~owner:seeded.owner
          ~capture:seeded.capture
          ~limits:(limits ())
          (fun
              ~env:_ ~sw:_ ~session:_ ~runs:_ ~handle:_ ~blobs:_ ~window ~view recovery ->
             let evidence =
               match Recovery.inventory recovery ~window ~view with
               | Ok evidence -> evidence
               | Error error -> raise_s [%sexp "inventory rejected", (error : P.Error.t)]
             in
             let kinds =
               List.map evidence ~f:(function
                 | Recovery.Evidence.Selected selected ->
                   assert (
                     P.Completion.equal
                       (Recovery.Restored.completion selected)
                       (P.Completion.Succeeded seeded.payload));
                   "selected"
                 | No_selection _ -> "none"
                 | Incomplete_selection _ -> "incomplete")
             in
             print_s [%sexp (installed : bool), (nested : bool), (kinds : string list)])));
  [%expect
    {|
    (false false (selected))
    (true false ())
    (false true (selected))
    |}]
;;

let%test_unit "restored selection uses original Pending and sole terminal writer" =
  F.with_temp_directory "ochat-developer-result-resume" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let seeded = seed_artifact ~env ~root ~installed:false () in
    with_inventory
      ~env
      ~root
      ~owner:seeded.owner
      ~capture:seeded.capture
      ~limits:(limits ())
      (fun ~env:_ ~sw:_ ~session:_ ~runs ~handle:_ ~blobs:_ ~window ~view recovery ->
         let selected =
           match Recovery.inventory recovery ~window ~view |> F.ok with
           | [ Recovery.Evidence.Selected selected ] -> selected
           | _ -> failwith "expected exact selected result"
         in
         let candidate = Recovery.Restored.candidate selected in
         let pending =
           R.Recovery.prepare_terminal
             window
             ~view
             ~candidate
             ~after:(Recovery.Restored.after selected)
           |> F.run_ok
         in
         Recovery.Restored.ensure_attempted selected ~pending |> F.ok;
         let installed =
           R.Recovery.commit_terminal window pending ~admit:(fun () -> true) |> F.run_ok
         in
         Recovery.Restored.acknowledge_installed selected installed |> F.ok;
         let job =
           R.find_job (R.Installed.observation installed) seeded.id |> Option.value_exn
         in
         (match (P.Developer_job.state job).status with
          | P.Job.Succeeded -> ()
          | _ -> failwith "restored job did not install selected success");
         ignore (runs : R.t)))
;;

let%test_unit "complete corrupt stage is not hidden by an extra partial" =
  F.with_temp_directory "ochat-developer-result-corrupt-partial" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let seeded = seed_artifact ~env ~root ~installed:false () in
    let data_root = Agent_store.Data_root.open_existing ~env ~path:root |> F.store_ok in
    let file suffix =
      Eio.Path.(
        Eio.Stdenv.fs env
        / blob_file data_root ~temporary:true seeded.owner.run_id seeded.blob suffix)
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) (file ".blob") "corrupt";
    Eio.Path.save ~create:(`Or_truncate 0o600) (file ".sexp") seeded.temporary_metadata;
    Eio.Path.save ~create:(`Or_truncate 0o600) (file ".part") "partial";
    with_inventory
      ~env
      ~root
      ~owner:seeded.owner
      ~capture:seeded.capture
      ~limits:(limits ())
      (fun ~env:_ ~sw:_ ~session:_ ~runs:_ ~handle:_ ~blobs:_ ~window ~view recovery ->
         assert (Result.is_error (Recovery.inventory recovery ~window ~view))))
;;

let%test_unit
    "verified durable copy wins over extra partial; missing bytes remain incomplete"
  =
  F.with_temp_directory "ochat-developer-result-stage-shape" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let seeded = seed_artifact ~env ~root ~installed:false () in
    let data_root = Agent_store.Data_root.open_existing ~env ~path:root |> F.store_ok in
    let file ~temporary suffix =
      Eio.Path.(
        Eio.Stdenv.fs env
        / blob_file data_root ~temporary seeded.owner.run_id seeded.blob suffix)
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) (file ~temporary:true ".part") "partial";
    with_inventory
      ~env
      ~root
      ~owner:seeded.owner
      ~capture:seeded.capture
      ~limits:(limits ())
      (fun ~env:_ ~sw:_ ~session:_ ~runs:_ ~handle:_ ~blobs:_ ~window ~view recovery ->
         (match Recovery.inventory recovery ~window ~view |> F.ok with
          | [ Recovery.Evidence.Selected _ ] -> ()
          | _ -> failwith "verified durable result was hidden by partial temp");
         Eio.Path.unlink (file ~temporary:false ".sexp");
         Eio.Path.unlink (file ~temporary:false ".blob");
         match Recovery.inventory recovery ~window ~view |> F.ok with
         | [ Recovery.Evidence.Incomplete_selection _ ] -> ()
         | _ -> failwith "missing selected content was not retained as incomplete"))
;;

let%test_unit "other authentic run's selected content is checked before No_selection" =
  List.iter [ false; true ] ~f:(fun poison_nonterminal ->
    F.with_temp_directory "ochat-developer-result-global" (fun env temporary ->
      let root = Filename.concat temporary "data" in
      ignore (seed_artifact ~poison_nonterminal ~env ~root ~installed:false () : seeded);
      let run_id = P.Id.Developer_run.of_string "dvr_other_result_run" |> F.ok in
      let owner = F.owner ~run_id 0 in
      let capture = F.digest "other authentic recovery run" in
      let id = Selected.job_id "job_other_result_run" in
      seed_other_running ~env ~root ~owner ~capture ~id;
      with_inventory
        ~env
        ~root
        ~owner
        ~capture
        ~limits:(limits ())
        (fun ~env:_ ~sw:_ ~session:_ ~runs:_ ~handle:_ ~blobs:_ ~window ~view recovery ->
           match Recovery.inventory recovery ~window ~view, poison_nonterminal with
           | Ok [ Recovery.Evidence.No_selection candidate ], false ->
             assert (
               P.Id.Job.equal (P.Developer_job.id (R.Recovery.Candidate.job candidate)) id)
           | Error _, true -> ()
           | Ok _, true | Ok _, false | Error _, false ->
             failwith "global inventory failed to validate another run's exact content")))
;;

let%test_unit "empty authentic intent dirs prove absence; foreign epoch and limits fail" =
  F.with_temp_directory "ochat-developer-result-absence" (fun env temporary ->
    let root = Filename.concat temporary "data" in
    let owner = F.owner 0 in
    let capture = F.digest "result absence capture" in
    let id = Selected.job_id "job_result_absence" in
    ignore
      (Developer_terminal_recovery_tests.seed_running ~env ~root ~owner ~capture ~id ()
       : string option);
    let data_root = Agent_store.Data_root.open_existing ~env ~path:root |> F.store_ok in
    let intents = Agent_store.Data_root.developer_result_intents_path data_root in
    let path = Eio.Path.(Eio.Stdenv.fs env / intents) in
    let run = "run_" ^ P.Id.Developer_run.to_string owner.run_id in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(path / run / "epoch_0");
    with_inventory
      ~env
      ~root
      ~owner
      ~capture
      ~limits:(limits ())
      (fun ~env:_ ~sw ~session:_ ~runs ~handle ~blobs ~window ~view recovery ->
         (match Recovery.inventory recovery ~window ~view |> F.ok with
          | [ Recovery.Evidence.No_selection candidate ] ->
            assert (
              P.Id.Job.equal (P.Developer_job.id (R.Recovery.Candidate.job candidate)) id)
          | _ -> failwith "empty authenticated directories did not prove absence");
         let root = R.Result_root.of_handle handle |> F.run_ok in
         let tight =
           Recovery.create ~sw ~runs ~root ~blobs ~limits:(limits ~max_work:1 ()) |> F.ok
         in
         assert (Result.is_error (Recovery.inventory tight ~window ~view));
         let callback_ran = ref false in
         let marker = Failure "cancel bounded developer inventory" in
         (match
            Eio.Cancel.sub (fun cancel ->
              B.with_developer_result_inventory
                blobs
                ~sw
                ~runs
                ~root
                ~max_entries:32
                ~max_bytes:65536
                ~checkpoint:(fun () ->
                  Eio.Cancel.cancel cancel marker;
                  Eio.Fiber.check ())
                ~f:
                  (fun
                    ~reader:_
                    ~intent_directory:_
                    ~temporary_namespace:_
                    ~durable_namespace:_ ->
                  callback_ran := true;
                  Ok ()))
          with
          | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual marker)
          | Ok _ | Error _ -> failwith "inventory cancellation was not delivered");
         assert (not !callback_ran);
         assert (Result.is_ok (Recovery.inventory recovery ~window ~view));
         let foreign_session =
           F.create_session env sw (Filename.concat temporary "foreign-data")
         in
         let foreign_runs = F.open_run foreign_session sw () in
         let foreign_handle, _ = F.admit foreign_runs owner capture in
         let foreign_claim =
           R.Run_claim.acquire ~store:foreign_runs ~handle:foreign_handle |> F.run_ok
         in
         let foreign_window =
           R.Recovery.open_ ~claim:foreign_claim ~handle:foreign_handle |> F.run_ok
         in
         let foreign_view = R.Recovery.observe foreign_window |> F.run_ok in
         assert (
           Result.is_error
             (Recovery.inventory recovery ~window:foreign_window ~view:foreign_view));
         R.Recovery.finish foreign_window |> F.run_ok;
         R.Run_claim.release foreign_claim |> F.run_ok;
         R.close_and_wait foreign_runs |> F.run_ok;
         S.close foreign_session |> F.store_ok;
         Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / run / "epoch_1");
         assert (Result.is_error (Recovery.inventory recovery ~window ~view))))
;;
