open Core
open Support
module Seal = Catalog.Seal
module Revision = Agent_session.Prompt_revision
module Loan = Agent_session.Prompt_source_lease

let rejected result expected =
  match result with
  | Error actual ->
    if not (Seal.Error.equal actual expected)
    then raise_s [%sexp { actual : Seal.Error.t; expected : Seal.Error.t }]
  | Ok _ -> failwith "catalog seal operation unexpectedly succeeded"
;;

let legacy catalog =
  Catalog.prepare
    catalog
    ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
    ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok)
    []
;;

let loan sw catalog definition =
  Loan.create
    (Some (receipt definition))
    ~selection:(Some (selection catalog definition))
    ~sw
    ~checkpoint:ignore
;;

let live loan =
  match Loan.availability loan with
  | Live -> ()
  | Missing_receipt | Unavailable _ | Revoked | Closed -> assert false
;;

let%expect_test "seal reserves publication; commit and native retirement are separate" =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>old</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      let old = revision catalog definition in
      let old_loan = loan sw catalog definition in
      save root "root.chatmd" "<user>new</user>";
      let candidate = prepare catalog sw [ definition ] |> ok in
      let polls = ref 0 in
      let seal =
        Catalog.seal catalog candidate ~checkpoint:(fun () -> incr polls) |> ok
      in
      assert (!polls > 0);
      assert (Catalog.Publication_state.equal (Catalog.publication_state catalog) Sealed);
      assert (Result.is_error (Catalog.install_owned catalog candidate));
      assert (Result.is_error (Catalog.install_owned catalog (legacy catalog)));
      assert (Result.is_error (prepare catalog sw [ definition ]));
      Catalog.discard_prepared candidate;
      assert (phys_equal old (revision catalog definition));
      live old_loan;
      rejected (Seal.retire seal) Pending;
      let scheduled = ref false in
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Fiber.yield ();
        scheduled := true);
      assert (not !scheduled);
      let polls_before_commit = !polls in
      Seal.commit seal |> ok;
      assert (not !scheduled);
      [%test_eq: int] !polls polls_before_commit;
      [%test_eq: string]
        (Revision.artifact (revision catalog definition)).root_chatmd
        "<user>new</user>";
      live old_loan;
      Seal.commit seal |> ok;
      rejected (Seal.abort seal) Committed;
      Seal.retire seal |> ok;
      Seal.retire seal |> ok;
      live old_loan;
      let removed = prepare catalog sw [] |> ok in
      let removal = Catalog.seal catalog removed ~checkpoint:ignore |> ok in
      live old_loan;
      Seal.commit removal |> ok;
      (match Loan.availability old_loan with
       | Revoked -> ()
       | Missing_receipt | Unavailable _ | Live | Closed -> assert false);
      Seal.retire removal |> ok;
      print_endline "reserved -> committed -> retired; removal revokes the old loan"));
  [%expect {| reserved -> committed -> retired; removal revokes the old loan |}]
;;

let%test_unit
    "abort preserves installed policy and only the owning seal disposes candidates"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>original</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    let foreign = Support.catalog env parent in
    Eio.Switch.run (fun sw ->
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      let old = loan sw catalog definition in
      let before = descriptors env in
      let candidate = prepare catalog sw [ definition ] |> ok in
      rejected (Catalog.seal foreign candidate ~checkpoint:ignore) Foreign_candidate;
      rejected
        (Catalog.seal catalog (legacy catalog) ~checkpoint:ignore)
        Unowned_candidate;
      let seal = Catalog.seal catalog candidate ~checkpoint:ignore |> ok in
      rejected (Catalog.seal catalog candidate ~checkpoint:ignore) Busy;
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
        rejected (Seal.commit seal) Foreign_domain;
        rejected (Seal.abort seal) Foreign_domain;
        rejected (Seal.mark_uncertain seal) Foreign_domain;
        rejected (Seal.retire seal) Foreign_domain;
        rejected
          (Catalog.seal catalog candidate ~checkpoint:(fun () -> assert false))
          Foreign_domain;
        assert (Result.is_error (Catalog.install_owned catalog candidate));
        match Catalog.discard_prepared candidate with
        | exception Invalid_argument _ -> ()
        | () -> assert false);
      Catalog.discard_prepared candidate;
      live old;
      Seal.abort seal |> ok;
      Seal.abort seal |> ok;
      rejected (Seal.commit seal) Aborted;
      assert (descriptors env = before);
      live old;
      assert (
        Catalog.Publication_state.equal (Catalog.publication_state catalog) Available);
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      live old))
;;

let%test_unit "restoration during a sealed write remains cached and prune protected" =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>historical</user>";
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let writer = catalog env parent in
      Catalog.install_owned writer (prepare writer sw [ definition ] |> ok) |> ok;
      let earlier = revision writer definition in
      save root "root.chatmd" "<user>second historical</user>";
      Catalog.install_owned writer (prepare writer sw [ definition ] |> ok) |> ok;
      let historical = revision writer definition in
      let target = catalog env parent in
      Catalog.install_owned
        target
        (prepare target sw [ Support.definition ~enabled:false root ] |> ok)
      |> ok;
      save root "root.chatmd" "<user>replacement</user>";
      let candidate = prepare target sw [ definition ] |> ok in
      let restored_before_admission = ref None in
      rejected
        (Catalog.seal target candidate ~checkpoint:(fun () ->
           if Option.is_none !restored_before_admission
           then
             restored_before_admission
             := Some
                  (Catalog.restore_revision
                     target
                     ~definition_id:definition.id
                     ~revision_id:(Revision.id earlier)
                   |> ok)))
        Stale_candidate;
      let seal = Catalog.seal target candidate ~checkpoint:ignore |> ok in
      assert (Option.is_none (Catalog.find_revision target (Revision.id historical)));
      let restored =
        Catalog.restore_revision
          target
          ~definition_id:definition.id
          ~revision_id:(Revision.id historical)
        |> ok
      in
      Seal.commit seal |> ok;
      Seal.retire seal |> ok;
      assert (
        phys_equal
          restored
          (Option.value_exn (Catalog.find_revision target (Revision.id historical))));
      assert (
        phys_equal
          (Option.value_exn !restored_before_admission)
          (Option.value_exn (Catalog.find_revision target (Revision.id earlier))));
      [%test_eq: int] 0 (Catalog.prune_unreferenced_artifacts target ~additional:[] |> ok)))
;;

let%test_unit
    "uncertain publication preserves old pointers and both immutable artifact sets"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>acknowledged</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      let old = revision catalog definition in
      let old_loan = loan sw catalog definition in
      save root "root.chatmd" "<user>indeterminate</user>";
      let candidate = prepare catalog sw [ definition ] |> ok in
      let pending =
        List.find_map (Catalog.prepared_entries candidate) ~f:(fun entry ->
          match entry.Catalog.availability with
          | Ready revision -> Some revision
          | Unavailable _ | Disabled -> None)
        |> Option.value_exn
      in
      let seal = Catalog.seal catalog candidate ~checkpoint:ignore |> ok in
      Seal.mark_uncertain seal |> ok;
      Seal.mark_uncertain seal |> ok;
      rejected (Seal.commit seal) Publication_uncertain;
      rejected (Seal.abort seal) Publication_uncertain;
      assert (Result.is_error (prepare catalog sw [ definition ]));
      assert (Result.is_error (Catalog.install_owned catalog candidate));
      assert (Result.is_error (Catalog.install_owned catalog (legacy catalog)));
      assert (phys_equal old (revision catalog definition));
      live old_loan;
      Catalog.discard_prepared candidate;
      Seal.retire seal |> ok;
      Seal.retire seal |> ok;
      live old_loan;
      [%test_eq: int] 0 (Catalog.prune_unreferenced_artifacts catalog ~additional:[] |> ok);
      assert (
        Result.is_ok
          (Agent_store.Prompt_artifact_store.load
             (Agent_store.Prompt_artifact_store.create
                ~env
                ~root:Eio.Path.(native_exn (parent / "artifacts"))
              |> ok)
             (Revision.id pending)))))
;;

let%test_unit
    "seal checkpoint failures keep their identity and cannot publish stale authority"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>checkpoint</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      let old = loan sw catalog definition in
      let candidate = prepare catalog sw [ definition ] |> ok in
      let polls = ref 0 in
      let seal =
        Catalog.seal catalog candidate ~checkpoint:(fun () -> incr polls) |> ok
      in
      Seal.abort seal |> ok;
      assert (!polls > 0);
      for stop = 1 to !polls do
        let candidate = prepare catalog sw [ definition ] |> ok in
        let marker = Failure "original seal callback" in
        let visited = ref 0 in
        (match
           Catalog.seal catalog candidate ~checkpoint:(fun () ->
             incr visited;
             if !visited = stop then raise marker)
         with
         | exception actual -> assert (phys_equal actual marker)
         | Ok _ | Error _ -> assert false);
        assert (
          Catalog.Publication_state.equal (Catalog.publication_state catalog) Available);
        Catalog.discard_prepared candidate;
        live old
      done;
      let stale = prepare catalog sw [ definition ] |> ok in
      let replacement = legacy catalog in
      let visited = ref 0 in
      rejected
        (Catalog.seal catalog stale ~checkpoint:(fun () ->
           incr visited;
           if !visited = !polls then Catalog.install catalog replacement))
        Stale_candidate;
      Catalog.discard_prepared stale;
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      let cancelled = prepare catalog sw [ definition ] |> ok in
      let marker = Failure "cancel original seal" in
      (match
         Eio.Cancel.sub (fun cancel ->
           Catalog.seal catalog cancelled ~checkpoint:(fun () ->
             Eio.Cancel.cancel cancel marker))
       with
       | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason marker)
       | Ok _ | Error _ -> assert false);
      Catalog.discard_prepared cancelled;
      let discarded = prepare catalog sw [ definition ] |> ok in
      let visited = ref 0 in
      rejected
        (Catalog.seal catalog discarded ~checkpoint:(fun () ->
           incr visited;
           if !visited = !polls then Catalog.discard_prepared discarded))
        Discarded_candidate;
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok))
;;

let%test_unit "same revision ID cannot replace its original admitted manifest" =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>identity</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      let original = revision catalog definition in
      let artifact = Revision.artifact original in
      let bytes =
        Agent_store.Prompt_manifest_document.Publication.bytes
          artifact.manifest_publication
        |> Jsonaf.of_string
        |> function
        | `Object fields ->
          Jsonaf.to_string (`Object (fields @ [ "future_identity", `String "retained" ]))
        | _ -> assert false
      in
      let digest = Digestif.SHA256.(digest_string bytes |> to_hex) in
      let directory =
        "artifacts/" ^ Agent_protocol.Id.Prompt_revision.to_string (Revision.id original)
      in
      (* Simulate externally replaced, independently valid manifest bytes under
         the same content-derived ID. Unknown fields remain meaningful original
         admission identity; the catalog must not choose one by arrival order. *)
      List.iter
        [ "manifest.sexp", bytes; "manifest.sha256", digest ^ "\n" ]
        ~f:(fun (member, contents) ->
          let path = directory ^ "/" ^ member in
          Eio.Path.unlink Eio.Path.(parent / path);
          save parent path contents);
      let candidate = prepare catalog sw [ definition ] |> ok in
      rejected
        (Catalog.seal catalog candidate ~checkpoint:ignore)
        (Conflicting_revision (Revision.id original));
      assert (phys_equal original (revision catalog definition));
      assert (Result.is_error (Catalog.install_owned catalog candidate));
      Catalog.discard_prepared candidate))
;;
