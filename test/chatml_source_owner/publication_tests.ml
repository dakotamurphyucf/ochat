open Core
open Support
module Publication = Agent_server.Prompt_catalog_publication
module Sessions = Agent_store.Session_store
module Baselines = Agent_session.Prompt_project_baseline_store
module Baseline = Agent_session.Prompt_project_baseline
module Artifacts = Agent_store.Prompt_artifact_store
module Revision = Agent_session.Prompt_revision
module Fault = Crash_fault_io

let timestamp = Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok

let sources =
  Agent_session.Prompt_script_preparation.Host.
    { additional_anchors = []; git_store = None; git_grants = [] }
;;

let policy () =
  Publication.Policy.create
    ~source_preparation:(Preparation_tests.policy ())
    ~custody_limits:(Script_tests.custody_limits ())
    ~baseline_limits:(Baseline_tests.limits ())
    ~source_limits:(limits ())
    ~max_roots:8
  |> Or_error.ok_exn
;;

module Host = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; sw : Eio.Switch.t
    ; publication : Publication.t
    ; catalog : Catalog.t
    ; artifacts : Artifacts.t
    ; baselines : Baselines.t
    ; directory : [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
    }

  let with_open ?(existing = false) ?(wrap = Fn.id) env parent f =
    Eio.Switch.run (fun store_sw ->
      let root = Eio.Path.native_exn Eio.Path.(parent / "host") in
      let store =
        (if existing
         then
           Sessions.open_existing
             ~env
             ~sw:store_sw
             ~root
             ~process_start_identity:None
             ~lock_nonce:"existing"
         else
           Sessions.create
             ~env
             ~sw:store_sw
             ~root
             ~server_id:(Agent_protocol.Id.Server.create ())
             ~process_start_identity:None
             ~lock_nonce:"new")
        |> ok
      in
      (* Join source and baseline owners before releasing exclusive store ownership,
         while the outer switch still owns the actual lock descriptor. *)
      Fun.protect
        ~finally:(fun () -> Eio.Cancel.protect (fun () -> Sessions.close store |> ok))
        (fun () ->
           Eio.Switch.run (fun sw ->
             let data = Sessions.data_root store in
             let directory =
               Eio.Path.open_dir
                 ~sw
                 Eio.Path.(env#fs / Agent_store.Data_root.indexes_path data)
               |> wrap
             in
             let baselines =
               (if existing then Baselines.open_existing else Baselines.create_new)
                 directory
                 ~sw
                 ~limits:(Baseline_tests.limits ())
                 ~checkpoint:ignore
               |> ok
             in
             let artifacts =
               Artifacts.create
                 ~env
                 ~root:(Agent_store.Data_root.prompt_artifacts_path data)
               |> ok
             in
             let catalog =
               Catalog.create ~env ~artifact_store:artifacts ~content_limits:None
             in
             let publication =
               Publication.create
                 ~store
                 ~catalog
                 ~artifacts
                 ~baselines
                 ~policy:(policy ())
                 ~content_limits:None
             in
             f { env; sw; publication; catalog; artifacts; baselines; directory })))
  ;;

  let prepare ?(mode = Publication.Mode.Build) ?(checkpoint = ignore) t definitions =
    Publication.prepare
      t.publication
      ~sw:t.sw
      ~env:t.env
      ~sources
      ~mode
      ~created_at:timestamp
      ~checkpoint
      definitions
  ;;

  let install t definitions =
    let token = prepare t definitions |> ok in
    Publication.publish token ~checkpoint:ignore |> ok;
    Publication.commit token |> ok;
    Publication.retire token;
    token
  ;;

  let bytes t = Baseline_tests.read t.directory

  let document t =
    Baseline.Document.decode
      ~limits:(Baseline_tests.limits ())
      ~checkpoint:ignore
      (bytes t)
    |> ok
  ;;

  let generation t = Baseline.Document.generation (document t)
end

let pending_revision token =
  match Publication.Prepared.entries token with
  | [ { Catalog.availability = Ready revision; _ } ] -> revision
  | _ -> failwith "expected one authentic successful candidate"
;;

let busy = function
  | Error Publication.Error.Busy -> ()
  | Ok _ | Error _ -> assert false
;;

let overlap f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> assert false
;;

let previous host definition =
  let snapshot = Baselines.snapshot host.Host.baselines ~checkpoint:ignore |> ok in
  match
    Baselines.Snapshot.previous snapshot ~receipt:(receipt definition) ~checkpoint:ignore
    |> ok
  with
  | Established entry -> entry
  | First_admission | Untracked _ -> failwith "last successful history disappeared"
;;

let assert_original actual expected =
  match actual with
  | Eio.Cancel.Cancelled reason -> assert (phys_equal reason expected)
  | actual -> assert (phys_equal actual expected)
;;

let%expect_test
    "real host publication writes history before installing pointers and survives restart"
  =
  with_workspace (fun env parent root ->
    Script_tests.populate root;
    let definition = definition root in
    let saved = ref None in
    Host.with_open env parent (fun host ->
      [%test_eq: int] 0 (Host.generation host);
      let token = Host.prepare host [ definition ] |> ok in
      let pending = pending_revision token in
      assert (List.is_empty (Catalog.entries host.catalog));
      Publication.publish token ~checkpoint:ignore |> ok;
      [%test_eq: int] 1 (Host.generation host);
      assert (List.is_empty (Catalog.entries host.catalog));
      Publication.commit token |> ok;
      assert (phys_equal pending (revision host.catalog definition));
      Publication.retire token;
      saved := Some (Revision.id pending);
      print_endline "durable generation 0 -> 1; catalog changes only at commit");
    Host.with_open ~existing:true env parent (fun host ->
      [%test_eq: int] 1 (Host.generation host);
      assert (List.is_empty (Catalog.entries host.catalog));
      let entry = previous host definition in
      assert (
        Agent_protocol.Id.Prompt_revision.equal
          (Option.value_exn !saved)
          (Baseline.Entry.revision_id entry));
      let artifact =
        Artifacts.load host.artifacts (Baseline.Entry.revision_id entry) |> ok
      in
      ignore
        (Baseline.Entry.reconcile
           entry
           ~artifact
           ~custody_limits:(Script_tests.custody_limits ())
           ~limits:(Baseline_tests.limits ())
           ~checkpoint:ignore
         |> ok
         : Baseline.Entry.Prior.t);
      save root "pkg/lookup.chatml" (Script_tests.body ^ "\nlet edited = true");
      let token = Host.prepare host [ definition ] |> ok in
      let unused = pending_revision token in
      assert (
        not
          (Agent_protocol.Id.Prompt_revision.equal
             (Revision.id unused)
             (Baseline.Entry.revision_id entry)));
      assert (Publication.equal_certainty (Publication.discard token) Rejected);
      [%test_eq: int]
        1
        (Publication.prune host.publication ~additional:[] ~checkpoint:ignore |> ok);
      assert (
        Result.is_ok (Artifacts.load host.artifacts (Baseline.Entry.revision_id entry)));
      assert (Result.is_error (Artifacts.load host.artifacts (Revision.id unused)));
      ignore
        (Host.install host [ Support.definition ~enabled:false root ]
         : Publication.Prepared.t);
      let retained = previous host definition in
      assert (
        Agent_protocol.Id.Prompt_revision.equal
          (Baseline.Entry.revision_id retained)
          (Baseline.Entry.revision_id entry));
      print_endline
        "restart retains exact artifact; prune protects baseline; disabled root keeps \
         history"));
  [%expect
    {|
    durable generation 0 -> 1; catalog changes only at commit
    restart retains exact artifact; prune protects baseline; disabled root keeps history
  |}]
;;

let%test_unit
    "overlapping token work is rejected before callbacks and acknowledged cancellation \
     is uncertain"
  =
  with_workspace (fun env parent root ->
    Script_tests.populate root;
    let definition = definition root in
    Host.with_open env parent (fun host ->
      let token = Host.prepare host [ definition ] |> ok in
      let entered, enter = Eio.Promise.create () in
      let released, release = Eio.Promise.create () in
      let first = ref true in
      Eio.Fiber.both
        (fun () ->
           Publication.publish token ~checkpoint:(fun () ->
             if !first
             then (
               first := false;
               Eio.Promise.resolve enter ();
               Eio.Promise.await released))
           |> ok)
        (fun () ->
           Eio.Promise.await entered;
           busy (Publication.publish token ~checkpoint:(fun () -> assert false));
           busy (Publication.commit token);
           overlap (fun () -> Publication.discard token);
           overlap (fun () -> Publication.retire token);
           busy (Host.prepare host ~checkpoint:(fun () -> assert false) [ definition ]);
           busy
             (Publication.prune host.publication ~additional:[] ~checkpoint:(fun () ->
                assert false));
           assert (List.is_empty (Catalog.entries host.catalog));
           Eio.Promise.resolve release ());
      [%test_eq: int] 1 (Host.generation host);
      let marker = Failure "cancel after durable acknowledgement" in
      (match
         Eio.Cancel.sub (fun context ->
           Eio.Cancel.cancel context marker;
           Eio.Fiber.check ())
       with
       | exception actual -> assert_original actual marker
       | () -> assert false);
      (* The caller has not entered its protected final pointer phase. Discard
         cannot roll acknowledged bytes back or expose the old baseline. *)
      assert (Publication.equal_certainty (Publication.discard token) Uncertain);
      assert (List.is_empty (Catalog.entries host.catalog));
      (match Publication.commit token with
       | Error Publication_uncertain -> ()
       | _ -> assert false);
      (match Host.prepare host [ definition ] with
       | Error Publication_uncertain -> ()
       | _ -> assert false);
      [%test_eq: int] 1 (Host.generation host)))
;;

let%test_unit
    "observed publish interruptions preserve identity and correlate with durable \
     certainty"
  =
  let observed = ref 0 in
  with_workspace (fun env parent root ->
    Script_tests.populate root;
    let definition = definition root in
    Host.with_open env parent (fun host ->
      ignore (Host.install host [ definition ] : Publication.Prepared.t);
      let token = Host.prepare host [ definition ] |> ok in
      Publication.publish token ~checkpoint:(fun () -> incr observed) |> ok;
      Publication.commit token |> ok;
      Publication.retire token;
      assert (!observed > 0);
      let installed = revision host.catalog definition in
      for stop = 1 to !observed do
        let token = Host.prepare host [ definition ] |> ok in
        let before = Host.bytes host in
        let marker = Failure (sprintf "throw at publish checkpoint %d" stop) in
        let visited = ref 0 in
        (match
           Publication.publish token ~checkpoint:(fun () ->
             incr visited;
             if !visited = stop then raise marker)
         with
         | exception actual -> assert_original actual marker
         | Ok () | Error _ -> assert false);
        [%test_eq: string] before (Host.bytes host);
        assert (phys_equal installed (revision host.catalog definition));
        assert (Publication.equal_certainty (Publication.discard token) Rejected)
      done;
      List.iter [ `Throw; `Cancel ] ~f:(fun mode ->
        let marker = Failure "prepare checkpoint" in
        match
          Eio.Cancel.sub (fun context ->
            Host.prepare host [ definition ] ~checkpoint:(fun () ->
              match mode with
              | `Throw -> raise marker
              | `Cancel -> Eio.Cancel.cancel context marker))
        with
        | exception actual -> assert_original actual marker
        | Ok _ | Error _ -> assert false);
      let reusable = Host.prepare host [ definition ] |> ok in
      assert (Publication.equal_certainty (Publication.discard reusable) Rejected)));
  for stop = 1 to !observed do
    (* Each waiter cancellation has its own real host. The joined store worker can
       reach protected writer entry before the waiter delivers cancellation; this
       is an uncertain write, never evidence that old bytes were rolled back. *)
    with_workspace (fun env parent root ->
      Script_tests.populate root;
      let definition = definition root in
      Host.with_open env parent (fun host ->
        ignore (Host.install host [ definition ] : Publication.Prepared.t);
        ignore (Host.install host [ definition ] : Publication.Prepared.t);
        let installed = revision host.catalog definition in
        let token = Host.prepare host [ definition ] |> ok in
        let pending = pending_revision token in
        let before = Host.bytes host in
        let generation = Host.generation host in
        let marker = Failure (sprintf "cancel at publish checkpoint %d" stop) in
        let visited = ref 0 in
        (match
           Eio.Cancel.sub (fun context ->
             Publication.publish token ~checkpoint:(fun () ->
               incr visited;
               if !visited = stop then Eio.Cancel.cancel context marker))
         with
         | exception actual -> assert_original actual marker
         | Ok () | Error _ -> assert false);
        assert (phys_equal installed (revision host.catalog definition));
        (match Publication.discard token with
         | Rejected ->
           [%test_eq: string] before (Host.bytes host);
           let reusable = Host.prepare host [ definition ] |> ok in
           assert (Publication.equal_certainty (Publication.discard reusable) Rejected)
         | Uncertain ->
           let document = Host.document host in
           let after = Baseline.Document.generation document in
           if Int.equal after generation
           then [%test_eq: string] before (Host.bytes host)
           else (
             [%test_eq: int] (generation + 1) after;
             match Baseline.Document.entries document with
             | [ entry ] ->
               assert (
                 Agent_protocol.Id.Prompt_revision.equal
                   (Baseline.Entry.revision_id entry)
                   (Revision.id pending));
               [%test_eq: string]
                 (Revision.artifact pending).manifest_sha256
                 (Baseline.Entry.manifest_sha256 entry)
             | _ -> assert false);
           (match Host.prepare host [ definition ] with
            | Error Publication_uncertain -> ()
            | _ -> assert false);
           (match Publication.commit token with
            | Error Publication_uncertain -> ()
            | _ -> assert false));
        let after = Host.bytes host in
        (match Publication.publish token ~checkpoint:(fun () -> assert false) with
         | Error _ -> ()
         | Ok () -> assert false);
        [%test_eq: string] after (Host.bytes host)))
  done
;;

let%test_unit
    "real writer failure retains full old or new history and both artifacts without retry"
  =
  List.iter
    [ Fault.Before_rename, false; After_rename, true ]
    ~f:(fun (boundary, renamed) ->
      with_workspace (fun env parent root ->
        Script_tests.populate root;
        let definition = definition root in
        let active = ref false in
        let hit = ref false in
        let marker = Failure "real baseline writer interruption" in
        let wrap (resource, path) =
          ( Fault.wrap_directory
              resource
              ~boundary
              ~matches:(fun member ->
                String.is_prefix member ~prefix:(Baseline_tests.basename ^ ".tmp-"))
              ~reached:(fun _ ->
                if !active
                then (
                  hit := true;
                  raise marker))
          , path )
        in
        Host.with_open ~wrap env parent (fun host ->
          ignore (Host.install host [ definition ] : Publication.Prepared.t);
          let old = revision host.catalog definition in
          let old_bytes = Host.bytes host in
          save root "pkg/lookup.chatml" (Script_tests.body ^ "\nlet changed = true");
          let token = Host.prepare host [ definition ] |> ok in
          let pending = pending_revision token in
          active := true;
          (match Publication.publish token ~checkpoint:ignore with
           | exception actual -> assert_original actual marker
           | Ok () | Error _ -> assert false);
          assert !hit;
          assert (phys_equal old (revision host.catalog definition));
          if renamed
          then (
            [%test_eq: int] 2 (Host.generation host);
            let entry = Baseline.Document.entries (Host.document host) |> List.hd_exn in
            assert (
              Agent_protocol.Id.Prompt_revision.equal
                (Baseline.Entry.revision_id entry)
                (Revision.id pending)))
          else [%test_eq: string] old_bytes (Host.bytes host);
          assert (Publication.equal_certainty (Publication.discard token) Uncertain);
          (match Host.prepare host [ definition ] with
           | Error Publication_uncertain -> ()
           | _ -> assert false);
          (match Publication.prune host.publication ~additional:[] ~checkpoint:ignore with
           | Error Publication_uncertain -> ()
           | _ -> assert false);
          List.iter [ old; pending ] ~f:(fun retained ->
            assert (Result.is_ok (Artifacts.load host.artifacts (Revision.id retained)))))))
;;

let%expect_test "missing history in an existing root never becomes first admission" =
  with_workspace (fun env parent root ->
    Script_tests.populate root;
    let definition = definition root in
    Host.with_open env parent (fun host ->
      ignore (Host.install host [ definition ] : Publication.Prepared.t));
    let index = Eio.Path.(parent / "host" / "indexes" / Baseline_tests.basename) in
    Eio.Path.unlink index;
    Host.with_open ~existing:true env parent (fun host ->
      let token = Host.prepare host [ definition ] |> ok in
      (match Publication.Prepared.entries token with
       | [ { Catalog.availability = Unavailable diagnostics; _ } ] ->
         assert (
           List.exists diagnostics ~f:(fun diagnostic ->
             String.equal
               diagnostic.Builder.Diagnostic.code
               "prompt.project_history_untracked"))
       | _ -> assert false);
      Publication.publish token ~checkpoint:ignore |> ok;
      Publication.commit token |> ok;
      Publication.retire token;
      assert (not (Eio.Path.is_file index));
      save root "root.chatmd" "<user>implicit</user>";
      ignore (Host.install host [ definition ] : Publication.Prepared.t);
      ignore (revision host.catalog definition : Revision.t);
      [%test_eq: int]
        0
        (Publication.prune host.publication ~additional:[] ~checkpoint:ignore |> ok);
      assert (not (Eio.Path.is_file index));
      let snapshot = Baselines.snapshot host.baselines ~checkpoint:ignore |> ok in
      [%test_eq: int option] None (Baselines.Snapshot.generation snapshot);
      print_endline
        "configured project unavailable; implicit prompt ready; missing index unchanged"));
  [%expect
    {| configured project unavailable; implicit prompt ready; missing index unchanged |}]
;;
