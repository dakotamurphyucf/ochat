open Core
open Support
module Daemon = Agent_server.Daemon
module Revision = Agent_session.Prompt_revision
module Baseline = Agent_session.Prompt_project_baseline

let with_daemon env parent root f =
  Eio.Switch.run (fun sw ->
    let home = Eio.Path.native_exn parent in
    let workspace = Eio.Path.native_exn root in
    let config =
      Agent_server_test_support.config
        home
        workspace
        (Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
    in
    let daemon =
      Daemon.start
        ~sw
        ~env
        ~config
        ~tool_dir:workspace
        ~home
        ~process_start_identity:None
        ~options:{ Daemon.default_options with startup_mode = Operator_only }
        ()
      |> Agent_server_test_support.protocol_ok
    in
    Fun.protect
      ~finally:(fun () -> Daemon.shutdown daemon |> Agent_server_test_support.protocol_ok)
      (fun () -> f daemon))
;;

let ready daemon =
  match Catalog.entries (Daemon.prompts daemon) with
  | [ { availability = Ready revision; _ } ] -> revision
  | entries ->
    raise_s
      [%sexp "expected one ready configured daemon prompt", (List.length entries : int)]
;;

let%expect_test "daemon startup preserves project history and refuses a missing old index"
  =
  with_workspace (fun env parent root ->
    Script_tests.populate root;
    let index = Eio.Path.(parent / "data" / "indexes" / Baseline_tests.basename) in
    let document () =
      Eio.Path.load index
      |> Baseline.Document.decode ~limits:(Baseline_tests.limits ()) ~checkpoint:ignore
      |> ok
    in
    let check_saved revision generation =
      let saved = document () in
      [%test_eq: int] generation (Baseline.Document.generation saved);
      match Baseline.Document.entries saved with
      | [ entry ] ->
        assert (
          Agent_protocol.Id.Prompt_revision.equal
            (Revision.id revision)
            (Baseline.Entry.revision_id entry))
      | _ -> failwith "daemon did not retain its actual ready artifact"
    in
    let first =
      with_daemon env parent root (fun daemon ->
        let revision = ready daemon in
        check_saved revision 1;
        Revision.id revision)
    in
    save root "pkg/lookup.chatml" (Script_tests.body ^ "\nlet edited = true");
    with_daemon env parent root (fun daemon ->
      let revision = ready daemon in
      assert (not (Agent_protocol.Id.Prompt_revision.equal first (Revision.id revision)));
      check_saved revision 2);
    print_endline
      "new root: generation 1; reopened root: captured local edit, generation 2";
    Eio.Path.unlink index;
    with_daemon env parent root (fun daemon ->
      (match Catalog.entries (Daemon.prompts daemon) with
       | [ { availability = Unavailable diagnostics; _ } ] ->
         assert (
           List.exists diagnostics ~f:(fun diagnostic ->
             String.equal
               diagnostic.Builder.Diagnostic.code
               "prompt.project_history_untracked"))
       | _ -> failwith "existing daemon root silently recreated project history");
      assert (not (Eio.Path.is_file index)));
    print_endline "missing old index: configured prompt unavailable; no replacement index");
  [%expect
    {|
    new root: generation 1; reopened root: captured local edit, generation 2
    missing old index: configured prompt unavailable; no replacement index
    |}]
;;
