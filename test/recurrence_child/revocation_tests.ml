open! Core
open Support
module R = A.Recurrence_child_reservation

module Publication = struct
  type t =
    | Artifact
    | Child

  let name = function
    | Artifact -> "artifact"
    | Child -> "child"
  ;;

  let lower_stage = function
    | Artifact -> Ledger.Reserved
    | Child -> Ledger.Artifact_installed
  ;;

  let staging point data_root row =
    let transaction = P.Id.Transaction.to_string (R.transaction_id row) in
    match point with
    | Artifact ->
      Filename.concat
        (Agent_store.Data_root.prompt_artifacts_path data_root)
        (".install-" ^ transaction)
    | Child ->
      Filename.concat
        (Agent_store.Data_root.sessions_path data_root)
        (".creating-" ^ transaction)
  ;;

  let matches point row path =
    let transaction = P.Id.Transaction.to_string (R.transaction_id row) in
    match point with
    | Artifact ->
      String.is_suffix path ~suffix:("/.install-" ^ transaction ^ "/root.chatmd")
    | Child ->
      String.is_substring path ~substring:("/.creating-" ^ transaction ^ "/")
      && String.is_prefix (Filename.basename path) ~prefix:"metadata.sexp.tmp-"
  ;;
end

let record daemon row =
  Ledger.find (S.delegations (Daemon.store daemon)) (R.delegation_key row |> protocol_ok)
  |> store_ok
  |> Option.value_exn
;;

let assert_missing env path =
  match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
  | `Not_found -> ()
  | _ -> failwith "revoked publication left a destination or owned staging tree"
;;

let run env point =
  let root = T.temporary_root env in
  protect_cleanup
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
    (fun () ->
       let prompt = Filename.concat root "parent.chatmd" in
       Eio.Path.save
         ~create:(`Exclusive 0o600)
         Eio.Path.(Eio.Stdenv.fs env / prompt)
         {|<developer>Captured publication revocation.</developer>
<script id="controller" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|};
       let configuration = T.config root root prompt in
       let provider_calls = ref 0 in
       let expected = ref None in
       let original_current = ref true in
       let observed = ref false in
       let wrapped =
         Crash_fault_io.wrap
           env
           ~boundary:After_sync
           ~matches:(fun path ->
             Option.exists !expected ~f:(fun (_, row) ->
               Publication.matches point row path))
           ~reached:(fun _ ->
             assert (not !observed);
             observed := true;
             (* Revoke the actual original host proof after yielding staging
                preparation. No synthetic current proof or injected exception. *)
             original_current := false)
       in
       (try
          with_daemon wrapped root configuration ~provider_calls (fun sw daemon ->
            let connection = T.connection daemon principal in
            protect_cleanup
              ~finally:(fun () -> Agent_client.Connection.close connection)
              (fun () ->
                 T.initialize connection;
                 let created, _ = T.create_session ~start_immediately:false connection in
                 let parent = loaded daemon created.id in
                 start_parent_settled wrapped sw connection parent;
                 with_planner wrapped sw daemon parent (fun () ->
                   (* This is a validating candidate copy before target admission,
                      not a claim that a new revision has been installed. *)
                   let revision_id =
                     match point with
                     | Publication.Artifact -> Some (P.Id.Prompt_revision.create ())
                     | Child -> None
                   in
                   let selected =
                     reserve
                       parent
                       (target ?revision_id wrapped daemon parent ~root_file:prompt)
                   in
                   let row = Actor.Recurrence_creation.reservation selected in
                   let original =
                     Operator_authorization.guarded ~principal ~is_current:(fun () ->
                       !original_current)
                   in
                   let permit =
                     reserve_occurrence ~actor:original parent (R.occurrence row)
                   in
                   assert (R.equal row (Actor.Recurrence_creation.reservation permit));
                   expected := Some (created.id, row);
                   let before = state parent in
                   (match
                      Factory.create_recurrence_child
                        (Daemon.factory daemon)
                        ~parent
                        ~permit
                    with
                    | Ok _ -> failwith "revoked original proof published a child"
                    | Error error ->
                      [%test_eq: P.Error.code] Permission_denied error.code;
                      [%test_eq: bool] false error.retryable;
                      [%test_eq: string]
                        "recurrence creation original authority expired, was revoked or \
                         lacks Create_sessions"
                        error.message);
                   assert !observed;
                   assert (not !original_current);
                   let lower = record daemon row in
                   assert (Ledger.equal_stage (Publication.lower_stage point) lower.stage);
                   let data_root = S.data_root (Daemon.store daemon) in
                   assert_missing env (Publication.staging point data_root row);
                   assert_missing
                     env
                     (Agent_store.Data_root.session_path
                        data_root
                        (R.child_session_id row));
                   (match point with
                    | Publication.Artifact ->
                      assert_missing
                        env
                        (Filename.concat
                           (Agent_store.Data_root.prompt_artifacts_path data_root)
                           (P.Id.Prompt_revision.to_string (R.artifact_revision_id row)))
                    | Child -> ());
                   assert (
                     Option.is_none
                       (Registry.find (Daemon.registry daemon) (R.child_session_id row)));
                   assert (
                     List.is_empty
                       (Frontier.child_associations
                          (Entry.frontier (entry parent) |> protocol_ok)));
                   let after = state parent in
                   [%test_eq: int64] before.counters.revision after.counters.revision;
                   [%test_eq: int64]
                     before.counters.transaction_sequence
                     after.counters.transaction_sequence)))
        with
        | Registry.Cleanup_failed failure ->
          (* Keep a new cleanup failure actionable without accepting or hiding it. *)
          raise_s [%sexp (Registry.Cleanup_failure.error failure : P.Error.t)]);
       (* A denied publication after projection intent does not authorize live
          retry. A genuinely new store/actor lifetime reconciles that intent. *)
       with_daemon env root configuration ~provider_calls (fun sw daemon ->
         let parent_id, row = Option.value_exn !expected in
         let parent = Registry.load (Daemon.registry daemon) parent_id |> protocol_ok in
         let connection = T.connection daemon principal in
         protect_cleanup
           ~finally:(fun () -> Agent_client.Connection.close connection)
           (fun () ->
              T.initialize connection;
              start_parent_settled env sw connection parent;
              with_planner env sw daemon parent (fun () ->
                assert (
                  Ledger.equal_stage
                    (Publication.lower_stage point)
                    (record daemon row).stage);
                let permit =
                  reserve_occurrence
                    ~actor:(Operator_authorization.trusted_local principal)
                    parent
                    (R.occurrence row)
                in
                assert (R.equal row (Actor.Recurrence_creation.reservation permit));
                let result =
                  Factory.create_recurrence_child (Daemon.factory daemon) ~parent ~permit
                  |> protocol_ok
                in
                let child_id =
                  P.Session_ref.session_id (Factory.Recurrence_child.session result)
                in
                assert (P.Id.Session.equal child_id (R.child_session_id row));
                let _, lower = assert_dormant daemon child_id in
                assert (Ledger.equal_stage Linked lower.stage);
                assert (
                  P.Id.Transaction.equal
                    lower.admission.transaction_id
                    (R.transaction_id row));
                assert (
                  P.Id.Prompt_revision.equal
                    lower.admission.revision_id
                    (R.artifact_revision_id row));
                [%test_eq: string] (R.manifest_sha256 row) lower.admission.manifest_sha256;
                assert (
                  Ledger.Reference.equal
                    (Ledger.reference lower)
                    (Factory.Recurrence_child.reference result));
                [%test_eq: int] 0 !provider_calls)));
       printf
         "%s publication revoked; fresh proof repairs the same identities\n"
         (Publication.name point))
;;

let%expect_test
    "original proof revocation fences final publication and preserves retry identity"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    List.iter [ Publication.Artifact; Child ] ~f:(run env));
  [%expect
    {|
    artifact publication revoked; fresh proof repairs the same identities
    child publication revoked; fresh proof repairs the same identities
    |}]
;;
