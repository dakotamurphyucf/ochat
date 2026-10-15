open! Core
module P = Agent_protocol
module S = Agent_store.Session_store
module R = Agent_store.Developer_run_store

let () = Mirage_crypto_rng_unix.use_default ()

let store_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected store error", (error : Agent_store.Store_error.t)]
;;

let with_temp_directory name f =
  Eio_main.run (fun env ->
    let path =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        (name ^ "." ^ (P.Id.Transaction.create () |> P.Id.Transaction.to_string))
    in
    let root = Eio.Path.(Eio.Stdenv.fs env / path) in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () -> f env path)
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root))
;;

let fault_after_authority_rename_env
      ?(after_rename = fun () -> ())
      ?(before_rename = fun () -> ())
      ?(clear_sync_block_on_failure = false)
      env
      ~rename_armed
      ~sync_blocked
      ~parent_sync_blocked
  =
  let rec wrap
    :  'tags.
       location:string
    -> ([> Eio.Fs.dir_ty ] as 'tags) Eio.Resource.t
    -> 'tags Eio.Resource.t
    =
    fun ~location (Eio.Resource.T (directory, handler)) ->
    let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
    let module Directory = struct
      include Original

      let open_dir directory ~sw path =
        let child =
          if Filename.is_absolute path then path else Filename.concat location path
        in
        wrap ~location:child (Original.open_dir directory ~sw path)
      ;;

      let open_in directory ~sw path =
        let parent_sync =
          (String.equal path "." && String.equal (Filename.basename location) "data")
          || String.is_suffix path ~suffix:"/data/."
        in
        if
          (!sync_blocked && (String.equal path "." || String.is_suffix path ~suffix:"/."))
          || (!parent_sync_blocked && parent_sync)
        then (
          if clear_sync_block_on_failure then sync_blocked := false;
          raise (Core_unix.Unix_error (EIO, "injected directory sync failure", path)))
        else Original.open_in directory ~sw path
      ;;

      let rename directory source destination target =
        if String.is_suffix target ~suffix:".json" then before_rename ();
        if !rename_armed && String.is_suffix target ~suffix:".json"
        then (
          rename_armed := false;
          Original.rename directory source destination target;
          after_rename ();
          raise (Core_unix.Unix_error (EIO, "injected post-rename failure", target)))
        else Original.rename directory source destination target
      ;;
    end
    in
    Eio.Resource.T
      ( directory
      , Eio.Resource.handler
          (H (Eio.Fs.Pi.Dir, (module Directory)) :: Eio.Resource.bindings handler) )
  in
  let directory, prefix = Eio.Stdenv.fs env in
  let wrapped_fs = wrap ~location:prefix directory, prefix in
  object
    method fs = wrapped_fs
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

let server_id =
  P.Id.Server.of_string "srv_agent_store_test"
  |> function
  | Ok id -> id
  | Error error -> raise_s [%sexp "invalid test server ID", (error : P.Error.t)]
;;

let ok = function
  | Ok value -> value
  | Error _ -> failwith "developer run fixture rejected"
;;

let run_id = P.Id.Developer_run.of_string "dvr_foundation_test" |> ok
let principal_id = P.Id.Principal.of_string "pri_foundation_test" |> ok
let other_principal = P.Id.Principal.of_string "pri_foundation_other" |> ok

let owner ?(run_id = run_id) ?(principal_id = principal_id) epoch =
  match
    P.Execution_owner.developer ~host_id:server_id ~run_id ~epoch ~principal_id |> ok
  with
  | P.Execution_owner.Developer owner -> owner
  | Conversation _ -> failwith "developer owner constructor returned conversation"
;;

let digest text = Chatml_source.Digest.of_contents text ~checkpoint:(fun () -> ())

let run_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "developer run operation failed", (error : R.Error.t)]
;;

let rejected_directory_sync = function
  | Error (R.Error.Store (Agent_store.Store_error.Io { operation; _ })) ->
    String.equal operation "sync directory"
  | Ok _ | Error _ -> false
;;

let replace_json_field json ~name ~f =
  match json with
  | `Object fields when List.Assoc.mem fields ~equal:String.equal name ->
    `Object
      (List.map fields ~f:(fun (field, value) ->
         if String.equal field name then field, f value else field, value))
  | _ -> failwith ("missing JSON fixture field: " ^ name)
;;

let replace_epoch_field json ~index ~name ~value =
  replace_json_field json ~name:"payload" ~f:(fun payload ->
    replace_json_field payload ~name:"epochs" ~f:(function
      | `Array epochs when List.length epochs = 2 ->
        `Array
          (List.mapi epochs ~f:(fun offset epoch ->
             if offset = index
             then replace_json_field epoch ~name ~f:(fun _ -> value)
             else epoch))
      | _ -> failwith "expected two saved epochs"))
;;

let limits
      ?(max_work_steps = 200)
      ?(max_jobs_per_run = 4)
      ?(max_document_bytes = 4096)
      ?max_inference_bytes_per_run
      ()
  =
  let document =
    Document_schema.Limits.create
      ~max_bytes:max_document_bytes
      ~max_depth:16
      ~max_fields:(max_document_bytes / 16)
      ~max_nodes:(max_document_bytes / 8)
    |> ok
  in
  R.Limits.create
    ~max_runs:3
    ~max_epochs_per_run:4
    ~max_jobs_per_run
    ~max_invocations_per_run:4
    ~max_inference_attempts_per_run:256
    ~max_inference_bytes_per_run:
      (Option.value max_inference_bytes_per_run ~default:max_document_bytes)
    ~max_selected_per_commit:2
    ~max_job_payload_bytes:1024
    ~max_document_bytes
    ~max_recovery_entries:16
    ~max_recovery_bytes:16384
    ~max_work_steps
    ~document
  |> Or_error.ok_exn
;;

let create_session env sw root =
  S.create
    ~env
    ~sw
    ~root
    ~server_id
    ~process_start_identity:None
    ~lock_nonce:"developer-foundation"
  |> store_ok
;;

let open_run
      session
      sw
      ?(checkpoint = fun () -> ())
      ?(max_work_steps = 200)
      ?(max_jobs_per_run = 4)
      ?(max_document_bytes = 4096)
      ?max_inference_bytes_per_run
      ()
  =
  match
    R.open_or_create
      session
      ~sw
      ~limits:
        (limits
           ~max_work_steps
           ~max_jobs_per_run
           ~max_document_bytes
           ?max_inference_bytes_per_run
           ())
      ~checkpoint
  with
  | Ok value -> value
  | Error error -> raise_s [%sexp "developer run open failed", (error : R.Error.t)]
;;

let with_root
      ?(checkpoint = fun () -> ())
      ?(max_document_bytes = 4096)
      ?max_inference_bytes_per_run
      f
  =
  with_temp_directory "ochat-developer-run" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let root = Filename.concat temporary "data" in
      let session = create_session env sw root in
      let run =
        open_run
          session
          sw
          ~checkpoint
          ~max_document_bytes
          ?max_inference_bytes_per_run
          ()
      in
      Exn.protect
        ~f:(fun () -> f env sw root session run)
        ~finally:(fun () ->
          ignore (R.close_and_wait run : (unit, R.Error.t) result);
          ignore (S.close session : (unit, Agent_store.Store_error.t) result))))
;;

let admit run owner capture =
  match R.admit_epoch run ~owner ~capture_fingerprint:capture ~previous:None with
  | Ok value -> value
  | Error error -> raise_s [%sexp "developer run admit failed", (error : R.Error.t)]
;;

let%test_unit
    "run claim excludes same-run admission and close without blocking other runs"
  =
  with_root (fun env _sw _root _session run ->
    let capture = digest "physical run claim" in
    let first_owner = owner 0 in
    let handle, initial = admit run first_owner capture in
    let cross_domain =
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () -> R.observe handle)
      |> run_ok
    in
    assert (
      Int64.equal
        (R.Checkpoint.revision (R.Observation.checkpoint cross_domain))
        (R.Checkpoint.revision (R.Observation.checkpoint initial)));
    let claim = R.Run_claim.acquire ~store:run ~handle |> run_ok in
    (match R.Run_claim.acquire ~store:run ~handle with
     | Error R.Error.Conflict -> ()
     | Ok _ | Error _ -> failwith "duplicate physical run claim was admitted");
    (match
       R.admit_epoch
         run
         ~owner:(owner 1)
         ~capture_fingerprint:capture
         ~previous:(Some initial)
     with
     | Error R.Error.Conflict -> ()
     | Ok _ | Error _ -> failwith "claimed run admitted a new epoch");
    (match R.close_and_wait run with
     | Error R.Error.Conflict -> ()
     | Ok () | Error _ -> failwith "shared Store closed with a live run claim");
    let unrelated_id = P.Id.Developer_run.of_string "dvr_unrelated_claim" |> ok in
    ignore
      (admit run (owner ~run_id:unrelated_id 0) (digest "unrelated run")
       : R.handle * R.Observation.t);
    R.Run_claim.release claim |> run_ok;
    R.Run_claim.release claim |> run_ok;
    let _next_handle, _next =
      R.admit_epoch
        run
        ~owner:(owner 1)
        ~capture_fingerprint:capture
        ~previous:(Some initial)
      |> run_ok
    in
    R.close_and_wait run |> run_ok)
;;

let%test_unit "post-rename failure requires admission and reconciliation durability" =
  with_temp_directory "ochat-developer-run-uncertain" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let armed = ref false in
      let sync_blocked = ref false in
      let parent_sync_blocked = ref false in
      let env =
        fault_after_authority_rename_env
          env
          ~rename_armed:armed
          ~sync_blocked
          ~parent_sync_blocked
      in
      let root = Filename.concat temporary "data" in
      let session = create_session env sw root in
      parent_sync_blocked := true;
      assert (
        rejected_directory_sync
          (R.open_or_create session ~sw ~limits:(limits ()) ~checkpoint:(fun () -> ())));
      assert (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / root / "developer-runs"));
      assert (
        rejected_directory_sync
          (R.open_or_create session ~sw ~limits:(limits ()) ~checkpoint:(fun () -> ())));
      parent_sync_blocked := false;
      let run = open_run session sw () in
      let first_owner = owner 0 in
      let capture = digest "uncertain graph" in
      armed := true;
      assert (
        Result.is_error
          (R.admit_epoch
             run
             ~owner:first_owner
             ~capture_fingerprint:capture
             ~previous:None));
      assert (not !armed);
      sync_blocked := true;
      assert (
        rejected_directory_sync
          (R.admit_epoch
             run
             ~owner:first_owner
             ~capture_fingerprint:capture
             ~previous:None));
      sync_blocked := false;
      let handle, observed = admit run first_owner capture in
      assert (Int64.equal (R.Checkpoint.revision (R.Observation.checkpoint observed)) 1L);
      let prepared =
        R.prepare_checkpoint handle ~expected:observed ~phase:R.Checkpoint.Closed
        |> run_ok
      in
      armed := true;
      assert (Result.is_error (R.commit handle prepared));
      assert (not !armed);
      sync_blocked := true;
      assert (rejected_directory_sync (R.reconcile handle prepared));
      sync_blocked := false;
      assert (
        match R.reconcile handle prepared |> run_ok with
        | Installed installed ->
          R.Checkpoint.equal_phase
            (R.Checkpoint.phase
               (R.Observation.checkpoint (R.Installed.observation installed)))
            R.Checkpoint.Closed
        | Absent | Diverged -> false);
      R.close_and_wait run |> run_ok;
      S.close session |> store_ok))
;;

let%test_unit "document observations follow exact checkpoint and epoch state" =
  with_root (fun _env _sw _root _session run ->
    let first_owner = owner 0 in
    let first_capture = digest "first graph" in
    let handle, initial = admit run first_owner first_capture in
    let same_handle, repeated = admit run first_owner first_capture in
    assert (P.Execution_owner.equal_developer (R.owner handle) (R.owner same_handle));
    assert (Int64.equal (R.Checkpoint.revision (R.Observation.checkpoint repeated)) 1L);
    assert (
      Result.is_error
        (R.admit_epoch
           run
           ~owner:first_owner
           ~capture_fingerprint:(digest "changed graph")
           ~previous:None));
    let foreign_host = P.Id.Server.of_string "srv_foreign_developer" |> ok in
    let foreign_owner =
      match
        P.Execution_owner.developer ~host_id:foreign_host ~run_id ~epoch:0 ~principal_id
        |> ok
      with
      | Developer owner -> owner
      | Conversation _ -> failwith "invalid developer owner"
    in
    assert (
      Result.is_error
        (R.admit_epoch
           run
           ~owner:foreign_owner
           ~capture_fingerprint:first_capture
           ~previous:None));
    let prepared =
      R.prepare_checkpoint handle ~expected:initial ~phase:R.Checkpoint.Interrupted
      |> run_ok
    in
    assert (
      match R.reconcile handle prepared |> ok with
      | Absent -> true
      | Installed _ | Diverged -> false);
    let installed = R.commit handle prepared |> ok in
    assert (
      match R.reconcile handle prepared |> ok with
      | Installed _ -> true
      | Absent | Diverged -> false);
    assert (Result.is_error (R.commit handle prepared));
    assert (
      R.Checkpoint.equal_phase
        (R.Checkpoint.phase
           (R.Observation.checkpoint (R.Installed.observation installed)))
        R.Checkpoint.Interrupted);
    let next_owner = owner 1 in
    let next_handle, next =
      R.admit_epoch
        run
        ~owner:next_owner
        ~capture_fingerprint:(digest "next graph")
        ~previous:(Some (R.Installed.observation installed))
      |> ok
    in
    assert (Result.is_error (R.prepare_checkpoint handle ~expected:initial ~phase:Active));
    assert (
      Result.is_error
        (R.prepare_checkpoint handle ~expected:(R.observe handle |> ok) ~phase:Active));
    assert (
      R.Checkpoint.equal_phase (R.Checkpoint.phase (R.Observation.checkpoint next)) Active);
    assert (
      Result.is_ok
        (R.prepare_checkpoint next_handle ~expected:next ~phase:R.Checkpoint.Active));
    assert (
      Result.is_error
        (R.admit_epoch
           run
           ~owner:(owner ~principal_id:other_principal 2)
           ~capture_fingerprint:(digest "foreign")
           ~previous:(Some next)));
    let _, third =
      R.admit_epoch
        run
        ~owner:(owner 2)
        ~capture_fingerprint:(digest "third graph")
        ~previous:(Some next)
      |> run_ok
    in
    let _, fourth =
      R.admit_epoch
        run
        ~owner:(owner 3)
        ~capture_fingerprint:(digest "fourth graph")
        ~previous:(Some third)
      |> run_ok
    in
    assert (
      match
        R.admit_epoch
          run
          ~owner:(owner 4)
          ~capture_fingerprint:(digest "epoch over limit")
          ~previous:(Some fourth)
      with
      | Error R.Error.Limit_exceeded -> true
      | Error _ | Ok _ -> false);
    let other_run number =
      P.Id.Developer_run.of_string ("dvr_foundation_" ^ Int.to_string number) |> ok
    in
    List.iter [ 1; 2 ] ~f:(fun number ->
      ignore
        (admit
           run
           (owner ~run_id:(other_run number) 0)
           (digest ("other run " ^ Int.to_string number))
         : R.handle * R.Observation.t));
    assert (
      match
        R.admit_epoch
          run
          ~owner:(owner ~run_id:(other_run 3) 0)
          ~capture_fingerprint:(digest "run over limit")
          ~previous:None
      with
      | Error R.Error.Limit_exceeded -> true
      | Error _ | Ok _ -> false))
;;

let%test_unit "real daemon root claim survives callback failure and rejects self-close" =
  with_root (fun _env _sw _root session run ->
    let root_owner = S.Developer_root.claim session in
    assert (Result.is_error root_owner);
    R.close_and_wait run |> ok;
    let claimed = S.Developer_root.claim session |> store_ok in
    let original = Failure "lease failed" in
    (match
       S.Developer_root.with_lease claimed ~f:(fun ~env:_ ~root:_ -> raise original)
     with
     | exception exn -> assert (phys_equal exn original)
     | Ok _ | Error _ -> failwith "lease swallowed original exception");
    let nested =
      S.Developer_root.with_lease claimed ~f:(fun ~env:_ ~root:_ ->
        let closed = S.close session in
        let reentered =
          S.Developer_root.with_lease claimed ~f:(fun ~env:_ ~root:_ -> ())
        in
        Result.is_error closed, Result.is_error reentered)
      |> store_ok
    in
    let closed, reentered = nested in
    assert (closed && reentered);
    S.Developer_root.with_lease claimed ~f:(fun ~env:_ ~root:_ -> ()) |> store_ok;
    let reason = Failure "cancel developer root callback" in
    (match
       Eio.Cancel.sub (fun cancellation ->
         S.Developer_root.with_lease claimed ~f:(fun ~env:_ ~root:_ ->
           Eio.Cancel.cancel cancellation reason;
           Eio.Fiber.check ()))
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason)
     | Ok _ | Error _ -> failwith "lease cancellation was lost");
    S.Developer_root.with_lease claimed ~f:(fun ~env:_ ~root:_ -> ()) |> store_ok;
    S.Developer_root.release claimed)
;;

let%test_unit
    "daemon close joins a live developer lease and switch release expires the facade"
  =
  with_temp_directory "ochat-developer-run-join" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let root = Filename.concat temporary "data" in
      let session = create_session env sw root in
      let handle = ref None in
      Eio.Switch.run (fun child_sw ->
        let run = open_run session child_sw () in
        let admitted, _ = admit run (owner 0) (digest "lease join") in
        handle := Some (run, admitted));
      let run, admitted = Option.value_exn !handle in
      assert (Result.is_error (R.observe admitted));
      let claimed = S.Developer_root.claim session |> store_ok in
      let entered, entered_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let finished, finished_u = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        S.Developer_root.with_lease claimed ~f:(fun ~env:_ ~root:_ ->
          Eio.Promise.resolve entered_u ();
          Eio.Promise.await release)
        |> store_ok);
      Eio.Promise.await entered;
      Eio.Fiber.fork ~sw (fun () ->
        S.close session |> store_ok;
        Eio.Promise.resolve finished_u ());
      Eio.Fiber.yield ();
      assert (not (Eio.Promise.is_resolved finished));
      Eio.Promise.resolve release_u ();
      Eio.Promise.await finished;
      assert (
        Result.is_error
          (S.Developer_root.with_lease claimed ~f:(fun ~env:_ ~root:_ -> ())));
      S.Developer_root.release claimed;
      ignore (R.close_and_wait run : (unit, R.Error.t) result)))
;;

let%test_unit "restart reads exact source and callback Exit remains Exit" =
  with_temp_directory "ochat-developer-run-restart" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let root = Filename.concat temporary "data" in
      let session = create_session env sw root in
      let run = open_run session sw () in
      let handle, initial = admit run (owner 0) (digest "persisted graph") in
      let prepared =
        R.prepare_checkpoint handle ~expected:initial ~phase:R.Checkpoint.Closed |> run_ok
      in
      ignore (R.commit handle prepared |> ok : R.Installed.t);
      R.close_and_wait run |> ok;
      S.close session |> store_ok;
      let directory = Eio.Path.(Eio.Stdenv.fs env / root / "developer-runs") in
      let interrupted_temporary =
        Eio.Path.(
          directory / ("run_" ^ P.Id.Developer_run.to_string run_id ^ ".json.tmp-1-1"))
      in
      Eio.Path.save ~create:(`Exclusive 0o600) interrupted_temporary "uncommitted";
      let reopened =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"developer-restart"
        |> store_ok
      in
      (match
         R.open_or_create reopened ~sw ~limits:(limits ()) ~checkpoint:(fun () ->
           raise Exit)
       with
       | exception Exit -> ()
       | Ok _ | Error _ -> failwith "callback Exit was classified as work limit");
      let run = open_run reopened sw () in
      let handle, observed = admit run (owner 0) (digest "persisted graph") in
      assert (
        R.Checkpoint.equal_phase
          (R.Checkpoint.phase (R.Observation.checkpoint observed))
          Closed);
      assert (
        Result.is_error (R.prepare_checkpoint handle ~expected:observed ~phase:Active));
      R.close_and_wait run |> ok;
      S.close reopened |> store_ok;
      let authority =
        Eio.Path.(directory / ("run_" ^ P.Id.Developer_run.to_string run_id ^ ".json"))
      in
      let original = Eio.Path.load authority in
      let foreign_bytes =
        String.substr_replace_all
          original
          ~pattern:(P.Id.Server.to_string server_id)
          ~with_:"srv_foreign_developer"
      in
      assert (not (String.equal original foreign_bytes));
      Eio.Path.save ~create:(`Or_truncate 0o600) authority foreign_bytes;
      let foreign_root =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"developer-foreign-recovery"
        |> store_ok
      in
      assert (
        Result.is_error
          (R.open_or_create foreign_root ~sw ~limits:(limits ()) ~checkpoint:(fun () ->
             ())));
      S.close foreign_root |> store_ok;
      Eio.Path.save ~create:(`Or_truncate 0o600) authority original;
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(directory / "unknown-authority")
        "ignored only if incorrectly admitted";
      let invalid_root =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"developer-invalid-recovery"
        |> store_ok
      in
      assert (
        Result.is_error
          (R.open_or_create invalid_root ~sw ~limits:(limits ()) ~checkpoint:(fun () ->
             ())));
      S.close invalid_root |> store_ok))
;;

let%test_unit "recovery rejects invalid epoch lineage and document identity" =
  with_temp_directory "ochat-developer-run-lineage" (fun env temporary ->
    Eio.Switch.run (fun sw ->
      let root = Filename.concat temporary "data" in
      let session = create_session env sw root in
      let run = open_run session sw () in
      let _, first = admit run (owner 0) (digest "first lineage") in
      let _, second =
        R.admit_epoch
          run
          ~owner:(owner 1)
          ~capture_fingerprint:(digest "second lineage")
          ~previous:(Some first)
        |> run_ok
      in
      assert (
        R.Checkpoint.equal_phase
          (R.Checkpoint.phase (R.Observation.checkpoint second))
          Active);
      R.close_and_wait run |> run_ok;
      S.close session |> store_ok;
      let authority =
        Eio.Path.(
          Eio.Stdenv.fs env
          / root
          / "developer-runs"
          / ("run_" ^ P.Id.Developer_run.to_string run_id ^ ".json"))
      in
      let original = Eio.Path.load authority in
      let json = Jsonaf.parse original |> Or_error.ok_exn in
      let other_run = P.Id.Developer_run.of_string "dvr_lineage_other" |> ok in
      let cases =
        [ ( "duplicate epoch"
          , replace_epoch_field json ~index:1 ~name:"number" ~value:(`String "0") )
        ; ( "skipped epoch"
          , replace_epoch_field json ~index:1 ~name:"number" ~value:(`String "3") )
        ; ( "nonlatest active"
          , replace_epoch_field json ~index:0 ~name:"phase" ~value:(`String "active") )
        ; ( "wrong run ID"
          , replace_json_field json ~name:"payload" ~f:(fun payload ->
              replace_json_field payload ~name:"run_id" ~f:(fun _ ->
                P.Id.Developer_run.to_json other_run)) )
        ; ( "unknown version"
          , replace_json_field json ~name:"schema_version" ~f:(fun _ -> `Number "2") )
        ]
      in
      List.iter cases ~f:(fun (label, mutated) ->
        Eio.Path.save ~create:(`Or_truncate 0o600) authority (Jsonaf.to_string mutated);
        let reopened =
          S.open_existing
            ~env
            ~sw
            ~root
            ~process_start_identity:None
            ~lock_nonce:("lineage-" ^ label)
          |> store_ok
        in
        if
          Result.is_ok
            (R.open_or_create reopened ~sw ~limits:(limits ()) ~checkpoint:(fun () -> ()))
        then failwith ("accepted " ^ label);
        S.close reopened |> store_ok);
      Eio.Path.save ~create:(`Or_truncate 0o600) authority original;
      let reopened =
        S.open_existing
          ~env
          ~sw
          ~root
          ~process_start_identity:None
          ~lock_nonce:"lineage-original"
        |> store_ok
      in
      let restored = open_run reopened sw () in
      let _, observed = admit restored (owner 1) (digest "second lineage") in
      assert (
        R.Checkpoint.equal_phase
          (R.Checkpoint.phase (R.Observation.checkpoint observed))
          Active);
      R.close_and_wait restored |> run_ok;
      S.close reopened |> store_ok))
;;
