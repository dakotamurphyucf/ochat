open Core
open Support
module Loan = Agent_session.Prompt_source_lease
module Artifact = Agent_store.Prompt_artifact_store

let%expect_test "retained revisions keep their definition on cache hits and restoration" =
  with_workspace (fun env parent root ->
    let module Revision = Agent_session.Prompt_revision in
    save root "root.chatmd" "<user>retained</user>";
    let original = definition root in
    let other =
      Agent_session.Prompt_definition.create
        ~id:(Agent_protocol.Id.Prompt_definition.of_string "prd_other" |> ok)
        ~config_name:"other"
        ~root_file:original.root_file
        ~allowed_workspaces:[]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:false
        ~description:None
      |> ok
    in
    Eio.Switch.run (fun sw ->
      let warm = catalog env parent in
      Catalog.install_owned warm (prepare warm sw [ original; other ] |> ok) |> ok;
      let retained = revision warm original in
      let revision_id = Revision.id retained in
      let restore catalog definition_id =
        Catalog.restore_revision catalog ~definition_id ~revision_id
      in
      let rejected result =
        match result with
        | Ok _ -> failwith "revision was accepted under another definition"
        | Error diagnostics ->
          print_s
            [%sexp
              (List.map diagnostics ~f:(fun diagnostic ->
                 diagnostic.Builder.Diagnostic.code)
               : string list)]
      in
      rejected (restore warm other.id);
      let cold = catalog env parent in
      Catalog.install_owned
        cold
        (prepare cold sw [ definition ~enabled:false root; other ] |> ok)
      |> ok;
      assert (Option.is_none (Catalog.find_revision cold revision_id));
      rejected (restore cold other.id);
      assert (Option.is_none (Catalog.find_revision cold revision_id));
      let restored = restore cold original.id |> ok in
      assert (
        String.equal
          (Revision.artifact retained).manifest_sha256
          (Revision.artifact restored).manifest_sha256);
      Catalog.install_owned cold (prepare cold sw [] |> ok) |> ok;
      assert (phys_equal restored (restore cold original.id |> ok));
      [%test_eq: int] 0 (Catalog.prune_unreferenced_artifacts cold ~additional:[] |> ok);
      print_endline "restored revision survives catalog removal and pruning"));
  [%expect
    {|
    (prompt.revision_definition_mismatch)
    (prompt.revision_definition_mismatch)
    restored revision survives catalog removal and pruning
    |}]
;;

let live loan =
  match Loan.availability loan with
  | Live -> ()
  | _ -> assert false
;;

let revoked loan =
  match Loan.availability loan with
  | Revoked -> ()
  | _ -> assert false
;;

let loan sw catalog definition =
  Loan.create
    (Some (receipt definition))
    ~selection:(Some (selection catalog definition))
    ~sw
    ~checkpoint:ignore
;;

let%test_unit
    "unchanged reload shares policy, discarded candidates cannot revoke, removal reaches \
     old loans"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>old</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      let install definitions =
        let candidate = prepare catalog sw definitions |> ok in
        Catalog.install_owned catalog candidate |> ok
      in
      install [ definition ];
      let old_revision = revision catalog definition in
      let old = loan sw catalog definition in
      live old;
      let candidate = prepare catalog sw [ definition ] |> ok in
      assert (Result.is_error (prepare catalog sw [ definition ]));
      Catalog.discard_prepared candidate;
      Catalog.discard_prepared candidate;
      live old;
      assert (Result.is_error (Catalog.install_owned catalog candidate));
      save root "root.chatmd" "<user>new</user>";
      install [ definition ];
      let fresh = loan sw catalog definition in
      live old;
      live fresh;
      let borrowed = Loan.borrow old in
      Loan.close borrowed;
      live old;
      Loan.claim old |> ok;
      assert (Result.is_error (Loan.claim old));
      let captured =
        Loan.with_scope borrowed ~f:(fun scope -> capture env scope definition) |> ok
      in
      assert (String.equal (root_text captured) "<user>new</user>");
      assert (
        String.equal
          (Agent_session.Prompt_revision.artifact old_revision).root_chatmd
          "<user>old</user>");
      install [];
      revoked old;
      revoked fresh;
      assert (
        String.equal
          (Agent_session.Prompt_revision.artifact old_revision).root_chatmd
          "<user>old</user>")))
;;

let%test_unit
    "foreign and stale owned candidates cannot resurrect authority after legacy install"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>old</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    let other = Support.catalog env parent in
    Eio.Switch.run (fun sw ->
      let candidate = prepare catalog sw [ definition ] |> ok in
      assert (Result.is_error (Catalog.install_owned other candidate));
      Catalog.install_owned catalog candidate |> ok;
      let old = loan sw catalog definition in
      let stale = prepare catalog sw [ definition ] |> ok in
      let legacy =
        Catalog.prepare
          catalog
          ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
          ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok)
          []
      in
      Catalog.install catalog legacy;
      revoked old;
      assert (Result.is_error (Catalog.install_owned catalog stale));
      Catalog.discard_prepared stale;
      let current = prepare catalog sw [ definition ] |> ok in
      Catalog.install_owned catalog current |> ok;
      live (loan sw catalog definition)))
;;

let%test_unit
    "stored intent never opens paths and captured restore survives missing original roots"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>retained</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      let candidate = prepare catalog sw [ definition ] |> ok in
      Catalog.install_owned catalog candidate |> ok;
      let retained = revision catalog definition in
      let artifact = Agent_session.Prompt_revision.artifact retained in
      assert (
        Option.equal Receipt.equal artifact.source_receipt (Some (receipt definition)));
      let absent =
        Loan.create artifact.source_receipt ~selection:None ~sw ~checkpoint:(fun () ->
          assert false)
      in
      (match Loan.availability absent with
       | Unavailable _ -> ()
       | _ -> assert false);
      assert (Option.equal Receipt.equal (Loan.receipt absent) artifact.source_receipt);
      let legacy =
        Loan.create
          None
          ~selection:(Some (selection catalog definition))
          ~sw
          ~checkpoint:(fun () -> assert false)
      in
      (match Loan.availability legacy with
       | Missing_receipt -> ()
       | _ -> assert false);
      Eio.Path.rename
        Eio.Path.(parent / "workspace")
        Eio.Path.(parent / "retained-workspace");
      let missing = prepare catalog sw [ definition ] |> ok in
      Catalog.install_owned catalog missing |> ok;
      let unavailable = loan sw catalog definition in
      (match Loan.availability unavailable with
       | Unavailable _ -> ()
       | _ -> assert false);
      let restored =
        Catalog.restore_revision
          catalog
          ~definition_id:definition.id
          ~revision_id:(Agent_session.Prompt_revision.id retained)
        |> ok
      in
      assert (
        String.equal
          (Agent_session.Prompt_revision.artifact restored).root_chatmd
          "<user>retained</user>");
      let removed = prepare catalog sw [] |> ok in
      Catalog.install_owned catalog removed |> ok;
      revoked unavailable))
;;

let%test_unit "owned manifest retains exact stored unknown metadata and receipt" =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>manifest</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      let artifact =
        Agent_session.Prompt_revision.artifact (revision catalog definition)
      in
      let publication = artifact.manifest_publication in
      let json =
        match
          Jsonaf.of_string
            (Agent_store.Prompt_manifest_document.Publication.bytes publication)
        with
        | `Object fields ->
          `Object
            (fields @ [ "future_original_source", `Object [ "value", `String "keep" ] ])
        | _ -> assert false
      in
      let bytes = "\n " ^ Jsonaf.to_string json ^ " \n" in
      let expected_sha256 = Digestif.SHA256.(digest_string bytes |> to_hex) in
      let retained =
        Agent_store.Prompt_manifest_document.Publication.of_bytes
          bytes
          ~revision_id:artifact.revision_id
          ~expected_sha256
        |> ok
      in
      let manifest =
        Agent_store.Prompt_manifest_document.Publication.document retained
        |> Agent_store.Prompt_manifest_document.value
      in
      assert (Option.equal Receipt.equal manifest.source_receipt artifact.source_receipt);
      assert (
        String.equal
          (Agent_store.Prompt_manifest_document.Publication.bytes retained)
          bytes);
      let changed =
        Agent_store.Prompt_manifest_document.Publication.with_identity
          retained
          ~revision_id:(Agent_protocol.Id.Prompt_revision.create ())
          ~created_at:artifact.created_at
        |> ok
      in
      let changed_json =
        Agent_store.Prompt_manifest_document.Publication.bytes changed |> Jsonaf.of_string
      in
      match changed_json with
      | `Object fields ->
        assert (List.Assoc.mem fields "future_original_source" ~equal:String.equal)
      | _ -> assert false))
;;

let%test_unit
    "interrupted preparation closes every candidate before returning and keeps catalog \
     reusable"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>checkpoint</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      let polls = ref 0 in
      let candidate =
        prepare catalog sw [ definition ] ~checkpoint:(fun () -> incr polls) |> ok
      in
      Catalog.discard_prepared candidate;
      assert (!polls > 0);
      let before = descriptors env in
      for stop = 1 to !polls do
        let token = Failure "prepare checkpoint" in
        let visited = ref 0 in
        (match
           prepare catalog sw [ definition ] ~checkpoint:(fun () ->
             incr visited;
             if !visited = stop then raise token)
         with
         | exception exn -> assert (phys_equal exn token)
         | Ok _ | Error _ -> assert false);
        assert (descriptors env = before)
      done;
      assert (
        Result.is_error
          (Catalog.prepare_owned
             catalog
             ~sw
             ~source_limits:(limits ())
             ~max_roots:0
             ~checkpoint:ignore
             ~transaction_id:(fun _ -> assert false)
             ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok)
             [ definition ]));
      assert (descriptors env = before);
      let fresh = prepare catalog sw [ definition ] |> ok in
      Catalog.install_owned catalog fresh |> ok))
;;

let%test_unit
    "unchanged selection cannot silently keep different supplied source ceilings"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>same policy</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    Eio.Switch.run (fun sw ->
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      let old = loan sw catalog definition in
      let before = descriptors env in
      let changed =
        Catalog.prepare_owned
          catalog
          ~sw
          ~source_limits:(limits ~leases:0 ())
          ~max_roots:8
          ~checkpoint:ignore
          ~transaction_id:(fun _ -> assert false)
          ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok)
          [ definition ]
      in
      assert (Result.is_error changed);
      assert (descriptors env = before);
      live old;
      Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
      live old))
;;

let%test_unit "catalog program capture borrows its live scope and cleans up failed hooks" =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>captured</user>";
    let definition = definition root in
    let catalog = catalog env parent in
    let escaped = ref None in
    let borrow scope captured =
      assert (Result.is_ok (Owner.Scope.check scope));
      [%test_eq: string] (root_text captured) "<user>captured</user>";
      escaped := Some scope
    in
    let closed () = error (Owner.Scope.check (Option.value_exn !escaped)) Closed in
    Eio.Switch.run (fun sw ->
      let failure = Failure "original preparation callback" in
      (match
         prepare
           ~prepare_programs:(fun scope captured ->
             borrow scope captured;
             raise failure)
           catalog
           sw
           [ definition ]
       with
       | exception actual -> assert (phys_equal actual failure)
       | _ -> failwith "source callback exception was converted");
      closed ();
      assert (List.is_empty (Catalog.entries catalog));
      let candidate =
        prepare
          ~prepare_programs:(fun scope captured ->
            borrow scope captured;
            let module Custody = Agent_session.Prompt_program_custody in
            let value = Owner.Capture.value captured in
            let parsed = Builder.Capture.root value in
            let root = Prompt.Chat_markdown.Parsed.root_input parsed in
            let authored =
              Custody.authored
                ~root_file:(Chatmd_parser_input.path root)
                ~sources:
                  ((Chatmd_parser_input.path root, Chatmd_parser_input.authored root)
                   :: Builder.Capture.sources value)
              |> ok
            in
            Ok
              (Custody.prepare
                 authored
                 ~parsed_contexts:[ parsed ]
                 ~root
                 ~supplied:None
                 ~checkpoint:ignore
               |> ok))
          catalog
          sw
          [ definition ]
        |> ok
      in
      closed ();
      Catalog.install_owned catalog candidate |> ok;
      [%test_eq: string]
        (Agent_session.Prompt_revision.artifact (revision catalog definition)).root_chatmd
        "<user>captured</user>";
      (* No native document acquisition is needed for a missing dynamic import
         or no document at all. Supplying a host hook must not introduce an empty
         content bundle or change the already captured revision's identity. *)
      List.iter
        [ "<user>no documents</user>"; {|<user><doc src="missing.md" local/></user>|} ]
        ~f:(fun contents ->
          save root "root.chatmd" contents;
          Catalog.install_owned catalog (prepare catalog sw [ definition ] |> ok) |> ok;
          let baseline = revision catalog definition in
          let candidate =
            Catalog.prepare_owned
              catalog
              ~prepare_content:(fun _ _ ->
                failwith "all-dynamic acquisition callback must not run")
              ~sw
              ~source_limits:(limits ())
              ~max_roots:8
              ~checkpoint:ignore
              ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
              ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok)
              [ definition ]
            |> ok
          in
          Catalog.install_owned catalog candidate |> ok;
          let current = revision catalog definition in
          assert (
            Agent_protocol.Id.Prompt_revision.equal
              (Agent_session.Prompt_revision.id baseline)
              (Agent_session.Prompt_revision.id current));
          assert (
            Option.is_none (Agent_session.Prompt_revision.artifact current).content_bundle))))
;;
