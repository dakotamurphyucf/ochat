open Core
open Support
module History = Baseline_tests
module Prepare = Agent_session.Prompt_project_preparation
module B = Agent_session.Prompt_project_baseline
module Store = Agent_session.Prompt_project_baseline_store

let%expect_test "source preparation uses authoritative history and authentic next roles" =
  History.with_success (fun env parent root definition _ artifacts _ success ->
    let directory = Eio.Path.(parent / "selection") in
    Eio.Path.mkdir ~perm:0o700 directory;
    Eio.Path.with_open_dir directory (fun directory ->
      Eio.Switch.run (fun sw ->
        let untracked =
          Store.open_existing directory ~sw ~limits:(History.limits ()) ~checkpoint:ignore
          |> ok
        in
        let missing = Store.snapshot untracked ~checkpoint:ignore |> ok in
        let store =
          Store.create_new directory ~sw ~limits:(History.limits ()) ~checkpoint:ignore
          |> ok
        in
        let first = Store.snapshot store ~checkpoint:ignore |> ok in
        let owner = open_owner sw root definition in
        let prepare ?(checkpoint = ignore) snapshot f =
          Owner.with_scope owner ~f:(fun scope ->
            let captured = capture env scope definition |> ok in
            let result =
              Prepare.capture
                (Preparation_tests.policy ())
                ~mode:Build
                ~snapshot
                ~artifact_store:artifacts
                ~baseline_limits:(History.limits ())
                ~custody_limits:(Script_tests.custody_limits ())
                ~root_role:Ui
                ~host:{ additional_anchors = []; git_store = None; git_grants = [] }
                ~scope
                ~capture:captured
                ~provided:[]
                ~env
                ~checkpoint
                ()
            in
            f captured result;
            Ok ())
          |> ok
        in
        prepare missing (fun _ result ->
          match result with
          | Ok _ -> failwith "missing historical index authorized project acquisition"
          | Error diagnostics ->
            [%test_eq: string list]
              [ "prompt.project_history_untracked" ]
              (List.map diagnostics ~f:(fun diagnostic ->
                 diagnostic.Builder.Diagnostic.code)));
        prepare first (fun _ result -> ignore (ok result : Prepare.Prepared.t));
        let candidate =
          Store.prepare store ~snapshot:first ~successes:[ success ] ~checkpoint:ignore
          |> ok
        in
        let previous = Store.publish store candidate ~checkpoint:ignore |> ok in
        let changed = Script_tests.body ^ "\nlet edited = true" in
        save root "pkg/lookup.chatml" changed;
        (* A new local slot is legitimate history, not missing history. *)
        save
          root
          "pkg/declarations.chatmd"
          (Script_tests.declarations
           ^ {|<script id="added" language="chatml" kind="tool" src="lookup.chatml" project="chatml.project.json"/>|}
          );
        prepare previous (fun captured result ->
          let prepared = ok result in
          [%test_eq: int]
            3
            (List.length
               (Chatml_prompt_programs.bindings (Prepare.Prepared.programs prepared)));
          let revision =
            Builder.build_captured
              ~capture:(Owner.Capture.value captured)
              ~source_receipt:(receipt definition)
              ~prepare_programs:(fun _ -> Ok (Prepare.Prepared.programs prepared))
              ~artifact_store:artifacts
              ~transaction_id:(Agent_protocol.Id.Transaction.create ())
              ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T13:00:00Z" |> ok)
              definition
            |> ok
          in
          let next =
            B.Success.finalize
              (Prepare.Prepared.association prepared)
              ~revision
              ~limits:(History.limits ())
              ~checkpoint:ignore
            |> ok
          in
          assert (
            not
              (Agent_protocol.Id.Prompt_revision.equal
                 (B.Entry.revision_id (B.Success.entry success))
                 (B.Entry.revision_id (B.Success.entry next))));
          let candidate =
            Store.prepare store ~snapshot:previous ~successes:[ next ] ~checkpoint:ignore
            |> ok
          in
          let next = Store.publish store candidate ~checkpoint:ignore |> ok in
          [%test_eq: int option] (Some 2) (Store.Snapshot.generation next));
        save
          root
          "pkg/declarations.chatmd"
          (sprintf
             {|<script id="implicit" language="chatml" kind="tool">%s</script>|}
             Script_tests.body);
        prepare missing (fun _ result -> ignore (ok result : Prepare.Prepared.t));
        let sentinel = Failure "host checkpoint" in
        (match prepare ~checkpoint:(fun () -> raise sentinel) missing (fun _ _ -> ()) with
         | () -> assert false
         | exception exn -> assert (phys_equal exn sentinel));
        print_endline
          "Unknown history rejects configured projects; implicit scripts remain usable.";
        print_endline
          "First admission and edited/new local slots retain authentic successful \
           associations.")));
  [%expect
    {|
    Unknown history rejects configured projects; implicit scripts remain usable.
    First admission and edited/new local slots retain authentic successful associations.
    |}]
;;
