open Core
open Agent_store_test_fixtures
module P = Agent_protocol
module D = Agent_store.Delegation_store
module S = Agent_store.Session_store
module A = Agent_store.Prompt_artifact_store
module Schema = Document_schema

let document_ok result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (Schema.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let read_document path =
  Agent_store.Document_record.decode_file
    ~limits:Agent_store.Delegation_document.limits
    ~expected_digest:None
    (Eio.Path.load path)
  |> Result.map_error ~f:Agent_store.Document_fields.record_error
  |> store_ok
  |> Agent_store.Document_record.document
;;

let replace_member json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal key name then value else old))
  | _ -> assert false
;;

let rewrite_document path json =
  let limits = Agent_store.Delegation_document.limits in
  let document = Schema.Document.inspect ~limits json |> document_ok in
  let bytes =
    Agent_store.Document_record.encode document ~limits ~flags:0
    |> Result.map_error ~f:Agent_store.Document_fields.record_error
    |> store_ok
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) path bytes
;;

let digest text = Digestif.SHA256.(digest_string text |> to_hex)
let max_records = 32
let max_bytes = 1048576

let key name =
  D.Key.
    { parent_session_id = session_id
    ; parent_generation = 3
    ; principal_id = P.Id.Principal.of_string "pri_delegation_parent" |> protocol_ok
    ; idempotency_key = P.Idempotency_key.of_string name |> protocol_ok
    }
;;

let admission () =
  D.Admission.
    { child_session_id = P.Id.Session.create ()
    ; revision_id = P.Id.Prompt_revision.create ()
    ; transaction_id = P.Id.Transaction.create ()
    ; manifest_sha256 = digest "captured generated definition"
    ; parent_revision_id = P.Id.Prompt_revision.create ()
    ; parent_stop_epoch = None
    ; authored_tool = None
    ; authority_sha256 = digest "parent policy and effective resource allowance"
    ; capability_pins = [ "read_file", digest "registered parent root" ]
    ; lifetime = Owned
    ; created_at = timestamp
    ; inference_target = Some (delegation_inference_target ())
    }
;;

let reserve ledger key admission =
  D.reserve
    ledger
    ~key
    ~request_sha256:(digest "complete creation request")
    ~admission
    ~max_records
    ~max_bytes
  |> store_ok
;;

let record = function
  | D.New record | Replay record -> record
  | Conflict _ -> failwith "unexpected conflict"
;;

let records ledger =
  D.with_records ledger ~max_records ~max_bytes ~f:(fun records -> Ok records) |> store_ok
;;

let retention ledger ?(entries = 4096) ?(bytes = max_bytes) f =
  D.with_artifact_retention
    ledger
    ~max_records
    ~max_bytes
    ~max_artifact_entries:entries
    ~max_artifact_bytes:bytes
    ~f
;;

let create env sw root =
  S.create
    ~env
    ~sw
    ~root
    ~server_id
    ~process_start_identity:None
    ~lock_nonce:"delegation-test"
  |> store_ok
;;

let reopen env sw root =
  S.open_existing
    ~env
    ~sw
    ~root
    ~process_start_identity:None
    ~lock_nonce:"delegation-reopen"
  |> store_ok
;;

let%expect_test "authored delegation identity survives restart and cannot change on retry"
  =
  with_temp_directory "ochat-authored-delegation" (fun env root ->
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      let ledger = S.delegations store in
      let generated_key = key "generated" in
      let generated = reserve ledger generated_key (admission ()) |> record in
      let generated_reference = D.reference generated in
      let authored_key = key "authored" in
      let origin : D.Admission.authored_tool =
        { name = "researcher"
        ; source_sha256 = digest "pinned authored source and declaration"
        }
      in
      let candidate = { (admission ()) with authored_tool = Some origin } in
      let authored = reserve ledger authored_key candidate |> record in
      let reference = D.reference authored in
      let conflict key candidate =
        match reserve ledger key candidate with
        | Conflict _ -> ()
        | New _ | Replay _ -> failwith "origin substitution did not conflict"
      in
      conflict generated_key { (admission ()) with authored_tool = Some origin };
      conflict authored_key (admission ());
      conflict
        authored_key
        { candidate with authored_tool = Some { origin with name = "reviewer" } };
      conflict
        authored_key
        { candidate with
          authored_tool = Some { origin with source_sha256 = digest "changed source" }
        };
      let retry = { (admission ()) with authored_tool = Some origin } in
      assert (D.equal_record authored (reserve ledger authored_key retry |> record));
      let scoped_key = key "one-off" in
      let scope : D.Admission.lifetime =
        Invocation_owned { invocation_id = P.Id.Invocation.create () }
      in
      let scoped_candidate =
        { (admission ()) with authored_tool = Some origin; lifetime = scope }
      in
      let scoped = reserve ledger scoped_key scoped_candidate |> record in
      conflict scoped_key candidate;
      conflict
        scoped_key
        { scoped_candidate with
          lifetime = Invocation_owned { invocation_id = P.Id.Invocation.create () }
        };
      let forged_reference =
        D.reference_to_jsonaf reference
        |> fun json ->
        replace_member json "admission_sha256" (`String (digest "substituted admission"))
        |> D.reference_of_jsonaf
        |> store_ok
      in
      assert (Result.is_error (D.resolve ledger forged_reference));
      let verify_version tested_key =
        let filename =
          digest (D.Key.sexp_of_t tested_key |> Sexp.to_string_mach) ^ ".frame"
        in
        let path =
          Eio.Path.(Eio.Stdenv.fs env / Filename.concat root ("delegations/" ^ filename))
        in
        let original = Eio.Path.load path in
        let document = read_document path in
        assert (Int.equal (Schema.Document.version document) 6);
        List.iter [ 5; 7 ] ~f:(fun version ->
          rewrite_document
            path
            (replace_member
               (Schema.Document.json document)
               "schema_version"
               (`Number (Int.to_string version)));
          assert (Result.is_error (D.find ledger tested_key)));
        Eio.Path.save ~create:(`Or_truncate 0o600) path original
      in
      verify_version authored_key;
      verify_version scoped_key;
      S.close store |> store_ok;
      let store = reopen env sw root in
      let ledger = S.delegations store in
      assert (D.equal_record authored (D.resolve ledger reference |> store_ok));
      assert (D.equal_record generated (D.resolve ledger generated_reference |> store_ok));
      assert (D.equal_record authored (reserve ledger authored_key retry |> record));
      assert (D.equal_record scoped (D.resolve ledger (D.reference scoped) |> store_ok));
      assert (
        D.equal_record
          scoped
          (reserve ledger scoped_key { retry with lifetime = scope } |> record));
      let revoked = D.revoke ledger authored Parent_stopped |> store_ok in
      assert (
        Option.equal
          D.Admission.equal_authored_tool
          revoked.admission.authored_tool
          (Some origin));
      assert (D.Reference.equal reference (D.reference revoked));
      List.iter
        [ { origin with name = "" }
        ; { origin with name = "bad\nname" }
        ; { origin with source_sha256 = "not-a-digest" }
        ]
        ~f:(fun invalid ->
          assert (
            Result.is_error
              (D.reserve
                 ledger
                 ~key:(key "invalid")
                 ~request_sha256:(digest "request")
                 ~admission:{ (admission ()) with authored_tool = Some invalid }
                 ~max_records
                 ~max_bytes)));
      S.close store |> store_ok));
  print_endline
    "v4 retains authored name/source and original instance across \
     retry/restart/revocation";
  print_endline
    "changed name/source, generated/authored substitution and forged references reject";
  print_endline
    "downgraded frames and malformed origins reject; generated reference unchanged";
  print_endline
    "v6 retains one-off invocation ownership; scope changes and version substitution \
     reject";
  [%expect
    {|
    v4 retains authored name/source and original instance across retry/restart/revocation
    changed name/source, generated/authored substitution and forged references reject
    downgraded frames and malformed origins reject; generated reference unchanged
    v6 retains one-off invocation ownership; scope changes and version substitution reject
    |}]
;;

let%expect_test
    "abandoned artifacts retire durably without losing protected source or retry identity"
  =
  with_temp_directory "ochat-abandoned-artifacts" (fun env root ->
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      let data = S.data_root store in
      let ledger = S.delegations store in
      let artifact_root = Agent_store.Data_root.prompt_artifacts_path data in
      let artifacts = A.create ~env ~root:artifact_root |> store_ok in
      let make ?parent_revision name =
        let artifact =
          A.Artifact.create
            ~revision_id:(P.Id.Prompt_revision.create ())
            ~root_chatmd:("<developer>" ^ name ^ "</developer>")
            ~sources:[]
            ~parser_schema_version:4
            ~runtime_schema_version:2
            ~created_at:timestamp
            ()
          |> store_ok
        in
        let base = admission () in
        let candidate =
          { base with
            revision_id = artifact.revision_id
          ; manifest_sha256 = artifact.manifest_sha256
          ; parent_revision_id =
              Option.value parent_revision ~default:base.parent_revision_id
          }
        in
        let record = reserve ledger (key name) candidate |> record in
        A.install artifacts ~transaction_id:candidate.transaction_id artifact |> store_ok;
        record, artifact
      in
      let abandoned, doomed = make "abandoned" in
      let abandoned = D.advance ledger abandoned Artifact_installed |> store_ok in
      let abandoned = D.revoke ledger abandoned Admission_failed |> store_ok in
      let abandoned_path =
        Eio.Path.(
          Eio.Stdenv.fs env
          / root
          / "delegations"
          / (digest (D.Key.sexp_of_t abandoned.key |> Sexp.to_string_mach) ^ ".frame"))
      in
      let document = read_document abandoned_path in
      let append json field =
        match json with
        | `Object fields -> `Object (fields @ [ field ])
        | _ -> assert false
      in
      let original_admission =
        match Schema.Json.field (Schema.Document.payload document) ~name:"admission" with
        | Value json -> append json ("future_admission", `Number "1e+00")
        | Null | Absent -> assert false
      in
      let payload =
        replace_member (Schema.Document.payload document) "admission" original_admission
        |> fun json -> append json ("future_disposition", `Null)
      in
      rewrite_document
        abandoned_path
        (replace_member (Schema.Document.json document) "payload" payload
         |> fun json -> append json ("future_envelope", `String "retained"));
      let abandoned = D.find ledger abandoned.key |> store_ok |> Option.value_exn in
      let active, active_artifact = make "active-intent" in
      let ambiguous, ambiguous_artifact = make "ambiguous-child-install" in
      let ambiguous = D.revoke ledger ambiguous Parent_stopped |> store_ok in
      Eio.Path.mkdir
        ~perm:0o700
        Eio.Path.(
          Eio.Stdenv.fs env
          / Agent_store.Data_root.session_path data ambiguous.admission.child_session_id);
      let staged, staged_artifact = make "staged-child" in
      let staged = D.advance ledger staged Artifact_installed |> store_ok in
      let staged = D.revoke ledger staged Admission_failed |> store_ok in
      let stage =
        Filename.concat
          (Agent_store.Data_root.sessions_path data)
          (".creating-" ^ P.Id.Transaction.to_string staged.admission.transaction_id)
      in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / stage);
      let shared, shared_artifact = make "other-session-reference" in
      let _ = D.revoke ledger shared Admission_failed |> store_ok in
      let ancestor, ancestor_artifact = make "ancestor-source" in
      let _ = D.revoke ledger ancestor Admission_failed |> store_ok in
      let _, referencing_artifact =
        make ~parent_revision:ancestor_artifact.revision_id "retained-ancestor-reference"
      in
      let prune protected =
        A.prune_unreferenced
          artifacts
          ~protected:(shared_artifact.revision_id :: protected)
      in
      let called = ref false in
      let blocked result =
        assert (Result.is_error result);
        assert (not !called)
      in
      let observe protected =
        called := true;
        prune protected
      in
      blocked (retention ledger ~entries:1 observe);
      blocked (retention ledger ~bytes:1 observe);
      let active_path =
        Eio.Path.(
          Eio.Stdenv.fs env
          / root
          / "delegations"
          / (digest (D.Key.sexp_of_t active.key |> Sexp.to_string_mach) ^ ".frame"))
      in
      let saved_record = Eio.Path.load active_path in
      let document = read_document active_path in
      let payload =
        replace_member
          (Schema.Document.payload document)
          "artifact_collection"
          (`String "prepared")
      in
      rewrite_document
        active_path
        (replace_member (Schema.Document.json document) "payload" payload);
      assert (Result.is_error (retention ledger observe));
      assert (not !called);
      Eio.Path.save ~create:(`Or_truncate 0o600) active_path saved_record;
      let original_root =
        Eio.Path.(
          Eio.Stdenv.fs env
          / artifact_root
          / P.Id.Prompt_revision.to_string doomed.revision_id
          / "root.chatmd")
      in
      Eio.Path.unlink original_root;
      Eio.Path.save ~create:(`Exclusive 0o400) original_root "changed artifact";
      blocked (retention ledger observe);
      assert (
        Option.is_none
          (D.find ledger abandoned.key |> store_ok |> Option.value_exn)
            .artifact_collection);
      Eio.Path.unlink original_root;
      Eio.Path.save ~create:(`Exclusive 0o400) original_root doomed.root_chatmd;
      let outside = Eio.Path.(Eio.Stdenv.fs env / root / "outside") in
      Eio.Path.save ~create:(`Exclusive 0o600) outside "do not delete";
      let extra =
        Eio.Path.(
          Eio.Stdenv.fs env
          / artifact_root
          / P.Id.Prompt_revision.to_string doomed.revision_id
          / "foreign")
      in
      Eio.Path.symlink ~link_to:(Eio.Path.native_exn outside) extra;
      blocked (retention ledger observe);
      Eio.Path.unlink extra;
      let armed = ref (Some true) in
      let fault_env =
        Job_store_fixtures.fault_env
          ~matches_rename:(fun path ->
            String.is_substring path ~substring:"/delegations/"
            && String.is_suffix path ~suffix:".frame")
          env
          armed
      in
      let fault_ledger = D.create ~env:fault_env ~data_root:data in
      blocked (retention fault_ledger observe);
      assert (Option.is_none !armed);
      assert (A.exists artifacts doomed.revision_id);
      (* The consumer fails after a partial deletion. Collection intent must
         already be durable, so a fresh store can finish this exact cleanup. *)
      assert (
        Result.is_error
          (retention ledger (fun _ ->
             Eio.Path.unlink original_root;
             Error (Agent_store.Store_error.Corrupt "interrupted deletion"))));
      let prepared = D.find ledger abandoned.key |> store_ok |> Option.value_exn in
      assert (Option.is_some prepared.artifact_collection);
      let prepared_document = read_document abandoned_path in
      assert (
        Jsonaf.exactly_equal
          original_admission
          (match
             Schema.Json.field
               (Schema.Document.payload prepared_document)
               ~name:"admission"
           with
           | Value json -> json
           | Null | Absent -> assert false));
      assert (
        match
          Schema.Json.field
            (Schema.Document.json prepared_document)
            ~name:"future_envelope"
        with
        | Value (`String "retained") -> true
        | Absent | Null | Value _ -> false);
      assert (
        match
          Schema.Json.field
            (Schema.Document.payload prepared_document)
            ~name:"future_disposition"
        with
        | Null -> true
        | Absent | Value _ -> false);
      assert (D.Reference.equal (D.reference abandoned) (D.reference prepared));
      S.close store |> store_ok;
      let store = reopen env sw root in
      let ledger = S.delegations store in
      [%test_eq: int] 1 (retention ledger prune |> store_ok);
      assert (not (A.exists artifacts doomed.revision_id));
      List.iter
        [ active_artifact
        ; ambiguous_artifact
        ; staged_artifact
        ; shared_artifact
        ; ancestor_artifact
        ; referencing_artifact
        ]
        ~f:(fun artifact -> assert (A.exists artifacts artifact.A.Artifact.revision_id));
      [%test_eq: string] "do not delete" (Eio.Path.load outside);
      let replayed = reserve ledger abandoned.key (admission ()) |> record in
      assert (D.equal_record prepared replayed);
      assert (Result.is_error (D.advance ledger replayed Child_installed));
      assert (
        D.equal_record active (D.find ledger active.key |> store_ok |> Option.value_exn));
      [%test_eq: int] 0 (retention ledger prune |> store_ok);
      S.close store |> store_ok;
      print_endline
        "bounded verified cleanup; active/ambiguous/staged/shared artifacts retained; \
         lost acknowledgement and partial deletion recover; revoked retry stays revoked"));
  [%expect
    {| bounded verified cleanup; active/ambiguous/staged/shared artifacts retained; lost acknowledgement and partial deletion recover; revoked retry stays revoked |}]
;;

let%expect_test
    "concurrent reservations retain one identity across reopen, linking and revocation"
  =
  with_temp_directory "ochat-delegation-ledger" (fun env root ->
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      let ledger = S.delegations store in
      let request_key = key "create-child" in
      assert (Option.is_none (D.find ledger request_key |> store_ok));
      let results =
        Eio.Fiber.List.map
          (fun _ -> reserve ledger request_key (admission ()))
          (List.init 8 ~f:Fn.id)
      in
      let first = record (List.hd_exn results) in
      assert (List.for_all results ~f:(fun result -> D.equal_record first (record result)));
      let new_count =
        List.count results ~f:(function
          | D.New _ -> true
          | _ -> false)
      in
      assert (Result.is_error (D.advance ledger first Linked));
      let conflict =
        D.reserve
          ledger
          ~key:request_key
          ~request_sha256:(digest "changed input")
          ~admission:(admission ())
          ~max_records
          ~max_bytes
        |> store_ok
      in
      assert (
        match conflict with
        | Conflict original -> D.equal_record first original
        | _ -> false);
      let other_key = key "different-request" in
      assert (Option.is_none (D.find ledger other_key |> store_ok));
      assert (
        Result.is_error
          (D.reserve
             ledger
             ~key:other_key
             ~request_sha256:(digest "other")
             ~admission:first.admission
             ~max_records
             ~max_bytes));
      let installed = D.advance ledger first Artifact_installed |> store_ok in
      let installed_reference = D.reference installed in
      S.close store |> store_ok;
      let store = reopen env sw root in
      let ledger = S.delegations store in
      assert (D.equal_record installed (D.resolve ledger installed_reference |> store_ok));
      assert (
        D.equal_record installed (reserve ledger request_key (admission ()) |> record));
      let created = D.advance ledger first Child_installed |> store_ok in
      let linked = D.advance ledger created Linked |> store_ok in
      assert (D.equal_record linked (D.advance ledger first Artifact_installed |> store_ok));
      let revoked = D.revoke ledger first Parent_stopped |> store_ok in
      assert (D.equal_record revoked (D.revoke ledger linked Authority_changed |> store_ok));
      assert (Result.is_error (D.advance ledger created Linked));
      S.close store |> store_ok;
      let store = reopen env sw root in
      let restored =
        D.find (S.delegations store) request_key |> store_ok |> Option.value_exn
      in
      assert (D.equal_record revoked restored);
      print_s
        [%sexp
          { new_count : int
          ; retained_records = (List.length (records (S.delegations store)) : int)
          ; stage = (restored.stage : D.stage)
          ; revocation = (restored.revocation : D.revocation option)
          }];
      S.close store |> store_ok));
  [%expect
    {|
    ((new_count 1) (retained_records 1) (stage Linked)
     (revocation (Parent_stopped)))
    |}]
;;

let%expect_test
    "ambiguous durable writes reconcile original identities and revoked stages"
  =
  List.iter [ false; true ] ~f:(fun after_rename ->
    with_temp_directory "ochat-delegation-ack" (fun real_env root ->
      let armed = ref None in
      let env =
        Job_store_fixtures.fault_env
          ~matches_rename:(fun path ->
            String.is_substring path ~substring:"/delegations/"
            && String.is_suffix path ~suffix:".frame")
          real_env
          armed
      in
      Eio.Switch.run (fun sw ->
        let store = create env sw root in
        let ledger = S.delegations store in
        let request_key = key "ambiguous-create" in
        let candidate = admission () in
        armed := Some after_rename;
        assert (
          Result.is_error
            (D.reserve
               ledger
               ~key:request_key
               ~request_sha256:(digest "complete creation request")
               ~admission:candidate
               ~max_records
               ~max_bytes));
        assert (Option.is_none !armed);
        S.close store |> store_ok;
        let store = reopen real_env sw root in
        let ledger = S.delegations store in
        let replacement = admission () in
        let recovered = reserve ledger request_key replacement |> record in
        let original_selected =
          P.Id.Session.equal
            recovered.admission.child_session_id
            candidate.child_session_id
        in
        assert (Bool.equal after_rename original_selected);
        (* Repeat a stage write with an acknowledgement lost after rename. *)
        let fault_ledger = D.create ~env ~data_root:(S.data_root store) in
        armed := Some true;
        assert (Result.is_error (D.advance fault_ledger recovered Artifact_installed));
        let staged = D.advance ledger recovered Artifact_installed |> store_ok in
        armed := Some true;
        assert (Result.is_error (D.revoke fault_ledger staged Authority_changed));
        assert (Result.is_error (D.advance ledger recovered Child_installed));
        let revoked = D.find ledger request_key |> store_ok |> Option.value_exn in
        print_s
          [%sexp
            { after_rename : bool
            ; original_selected : bool
            ; stage = (revoked.stage : D.stage)
            ; revocation = (revoked.revocation : D.revocation option)
            }];
        S.close store |> store_ok)));
  [%expect
    {|
    ((after_rename false) (original_selected false) (stage Artifact_installed)
     (revocation (Authority_changed)))
    ((after_rename true) (original_selected true) (stage Artifact_installed)
     (revocation (Authority_changed)))
    |}]
;;

let%expect_test
    "incomplete and revoked intents protect real artifacts; invalid scans never prune"
  =
  with_temp_directory "ochat-delegation-retention" (fun env root ->
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      let ledger = S.delegations store in
      let artifacts =
        A.create
          ~env
          ~root:(Agent_store.Data_root.prompt_artifacts_path (S.data_root store))
        |> store_ok
      in
      let install text =
        let artifact =
          A.Artifact.create
            ~revision_id:(P.Id.Prompt_revision.create ())
            ~root_chatmd:text
            ~sources:[]
            ~parser_schema_version:4
            ~runtime_schema_version:2
            ~created_at:timestamp
            ()
          |> store_ok
        in
        artifact
      in
      let protected = install "<developer>reserved child</developer>" in
      let orphan = install "<developer>unreferenced artifact</developer>" in
      let a =
        { (admission ()) with
          revision_id = protected.revision_id
        ; manifest_sha256 = protected.manifest_sha256
        }
      in
      let pending = reserve ledger (key "retained-create") a |> record in
      List.iter [ protected; orphan ] ~f:(fun artifact ->
        A.install artifacts ~transaction_id:(P.Id.Transaction.create ()) artifact
        |> store_ok);
      let prune ledger ~max_records ~max_bytes =
        D.with_records ledger ~max_records ~max_bytes ~f:(fun records ->
          A.prune_unreferenced
            artifacts
            ~protected:(List.map records ~f:(fun r -> r.D.admission.revision_id)))
      in
      let removed = prune ledger ~max_records ~max_bytes |> store_ok in
      assert (
        A.exists artifacts protected.revision_id
        && not (A.exists artifacts orphan.revision_id));
      let _ = D.revoke ledger pending Admission_failed |> store_ok in
      assert (Int.equal 0 (prune ledger ~max_records ~max_bytes |> store_ok));
      assert (Result.is_error (prune ledger ~max_records:0 ~max_bytes));
      assert (Result.is_error (prune ledger ~max_records ~max_bytes:1));
      let directory = Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "delegations") in
      let name = Eio.Path.read_dir directory |> List.hd_exn in
      let file = Eio.Path.(directory / name) in
      let bytes = Eio.Path.load file in
      Eio.Path.save ~create:(`Or_truncate 0o600) file "truncated";
      assert (Result.is_error (prune ledger ~max_records ~max_bytes));
      assert (A.exists artifacts protected.revision_id);
      Eio.Path.save ~create:(`Or_truncate 0o600) file bytes;
      let unknown = Eio.Path.(directory / "unknown") in
      Eio.Path.save ~create:(`Exclusive 0o600) unknown "unexpected";
      assert (Result.is_error (prune ledger ~max_records ~max_bytes));
      Eio.Path.unlink unknown;
      let renamed = Eio.Path.(directory / (digest "wrong scoped key" ^ ".frame")) in
      Eio.Path.rename file renamed;
      assert (Result.is_error (prune ledger ~max_records ~max_bytes));
      Eio.Path.rename renamed file;
      Eio.Path.unlink file;
      Eio.Path.symlink ~link_to:"../schema.sexp" file;
      assert (Result.is_error (prune ledger ~max_records ~max_bytes));
      Eio.Path.unlink file;
      Eio.Path.save ~create:(`Exclusive 0o600) file bytes;
      let saved_directory =
        Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "saved-delegations")
      in
      Eio.Path.rename directory saved_directory;
      Eio.Path.symlink ~link_to:"saved-delegations" directory;
      assert (Result.is_error (prune ledger ~max_records ~max_bytes));
      Eio.Path.unlink directory;
      Eio.Path.rename saved_directory directory;
      let raised =
        Result.try_with (fun () ->
          D.with_records ledger ~max_records ~max_bytes ~f:(fun _ ->
            failwith "callback fault"))
        |> Result.is_error
      in
      assert raised;
      let retained = List.length (records ledger) in
      assert (A.exists artifacts protected.revision_id);
      print_s
        [%sexp
          { removed_orphans = (removed : int)
          ; retained : int
          ; callback_exception_propagated = (raised : bool)
          ; ledger_usable_after_exception = (Int.equal 1 retained : bool)
          }];
      S.close store |> store_ok));
  [%expect
    {|
    ((removed_orphans 1) (retained 1) (callback_exception_propagated true)
     (ledger_usable_after_exception true))
    |}]
;;

let%expect_test "v6 captures independent target and binds complete original admission" =
  let module Schema = Document_schema in
  let module Target = Inference.Request.Target in
  let schema_ok result =
    Result.map_error result ~f:(fun error ->
      Sexp.to_string_hum (Schema.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let limits = Agent_store.Document_fields.limits ~max_bytes:262144 |> schema_ok in
  let inference_ok result =
    Result.map_error result ~f:(fun error ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let target =
    Target.create
      ~adapter:"fixture"
      ~profile:"explicit"
      ~profile_revision:None
      ~account:(Some "local")
      ~endpoint:"fixture://independent"
      ~model:"original-child"
      ~settings:[]
      ~limits
    |> inference_ok
  in
  with_temp_directory "ochat-delegation-target" (fun env root ->
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      let ledger = S.delegations store in
      let request_key = key "independent-selection" in
      let original =
        reserve ledger request_key { (admission ()) with inference_target = Some target }
        |> record
      in
      let original_reference = D.reference original in
      let filename =
        digest (D.Key.sexp_of_t request_key |> Sexp.to_string_mach) ^ ".frame"
      in
      let path =
        Eio.Path.(Eio.Stdenv.fs env / Filename.concat root ("delegations/" ^ filename))
      in
      let read_document () =
        Agent_store.Document_record.decode_file
          ~limits
          ~expected_digest:None
          (Eio.Path.load path)
        |> Result.map_error ~f:Agent_store.Document_fields.record_error
        |> store_ok
        |> Agent_store.Document_record.document
      in
      let document = read_document () in
      [%test_eq: int] 6 (Schema.Document.version document);
      let replace_member json name update =
        match json with
        | `Object fields ->
          `Object
            (List.map fields ~f:(fun (key, value) ->
               key, if String.equal key name then update value else value))
        | _ -> assert false
      in
      let payload =
        replace_member (Schema.Document.payload document) "admission" (function
          | `Object fields -> `Object (("future_admission", `Number "1e+00") :: fields)
          | _ -> assert false)
      in
      let document =
        Schema.Document.inspect
          ~limits
          (replace_member (Schema.Document.json document) "payload" (fun _ -> payload))
        |> schema_ok
      in
      let bytes =
        Agent_store.Document_record.encode document ~limits ~flags:0
        |> Result.map_error ~f:Agent_store.Document_fields.record_error
        |> store_ok
      in
      Eio.Path.save ~create:(`Or_truncate 0o600) path bytes;
      assert (Result.is_error (D.resolve ledger original_reference));
      assert (Result.is_error (D.advance ledger original Artifact_installed));
      let captured = D.find ledger request_key |> store_ok |> Option.value_exn in
      let reference = D.reference captured in
      assert (
        not (String.equal original_reference.admission_sha256 reference.admission_sha256));
      let exact_admission document =
        match Schema.Json.field (Schema.Document.payload document) ~name:"admission" with
        | Value value -> Jsonaf.to_string value
        | Null | Absent -> assert false
      in
      let admission_bytes = exact_admission document in
      let advanced = D.advance ledger captured Artifact_installed |> store_ok in
      let revoked = D.revoke ledger advanced Parent_stopped |> store_ok in
      assert (D.Reference.equal reference (D.reference revoked));
      assert (String.equal admission_bytes (exact_admission (read_document ())));
      S.close store |> store_ok;
      let store = reopen env sw root in
      let ledger = S.delegations store in
      let restored = D.resolve ledger reference |> store_ok in
      assert (Target.equal target (Option.value_exn restored.admission.inference_target));
      let replacement =
        Target.with_model target ~model:"changed-parent" ~limits |> inference_ok
      in
      let replay =
        reserve
          ledger
          request_key
          { (admission ()) with inference_target = Some replacement }
        |> record
      in
      assert (D.Reference.equal reference (D.reference replay));
      assert (Target.equal target (Option.value_exn replay.admission.inference_target));
      assert (String.equal admission_bytes (exact_admission (read_document ())));
      S.close store |> store_ok));
  print_endline
    "v6 target survives parent changes; complete future admission identity and bytes \
     retained";
  [%expect
    {| v6 target survives parent changes; complete future admission identity and bytes retained |}]
;;

let%expect_test "delegation frame and original scoped owner precede current admission" =
  with_temp_directory "ochat-delegation-admission-order" (fun env root ->
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      let ledger = S.delegations store in
      let request_key = key "owner-before-admission" in
      let selected = reserve ledger request_key (admission ()) |> record in
      let path =
        Eio.Path.(
          Eio.Stdenv.fs env
          / root
          / "delegations"
          / (digest (D.Key.sexp_of_t request_key |> Sexp.to_string_mach) ^ ".frame"))
      in
      let original = Eio.Path.load path in
      let document = read_document path in
      let payload = Schema.Document.payload document in
      let get json name =
        match Schema.Json.field json ~name with
        | Value value -> value
        | Null | Absent -> assert false
      in
      let bad_admission =
        replace_member (get payload "admission") "inference_target" `Null
      in
      let foreign_key =
        replace_member (get payload "key") "parent_generation" (`String "4")
      in
      let payload =
        replace_member payload "admission" bad_admission
        |> fun json -> replace_member json "key" foreign_key
      in
      rewrite_document
        path
        (replace_member (Schema.Document.json document) "payload" payload);
      (match D.find ledger request_key with
       | Error (Agent_store.Store_error.Corrupt _) -> ()
       | Ok _ | Error _ -> assert false);
      let invalid_flags =
        Agent_store.Frame.encode ~max_payload_length:262144 ~flags:1 "not json"
        |> frame_ok
      in
      Eio.Path.save ~create:(`Or_truncate 0o600) path invalid_flags;
      (match D.find ledger request_key with
       | Error (Agent_store.Store_error.Corrupt _) -> ()
       | Ok _ | Error _ -> assert false);
      Eio.Path.save ~create:(`Or_truncate 0o600) path original;
      assert (
        D.Reference.equal
          (D.reference selected)
          (D.reference (D.find ledger request_key |> store_ok |> Option.value_exn)));
      let before = List.length (records ledger) in
      assert (
        Result.is_error
          (D.reserve
             ledger
             ~key:(key "uncaptured")
             ~request_sha256:(digest "new")
             ~admission:{ (admission ()) with inference_target = None }
             ~max_records
             ~max_bytes));
      assert (Int.equal before (List.length (records ledger)));
      S.close store |> store_ok));
  print_endline
    "framing and original owner fail first; uncaptured reserve publishes nothing";
  [%expect
    {|framing and original owner fail first; uncaptured reserve publishes nothing|}]
;;

let%expect_test
    "explicit delegation cancellation preserves the usable owner and admission"
  =
  let armed = ref None in
  with_temp_directory "ochat-delegation-cancel" (fun env root ->
    let env =
      Job_store_fixtures.fault_env
        ~matches_rename:(fun path -> String.is_suffix path ~suffix:".frame")
        ~on_failure:(fun _ -> raise Eio.Time.Timeout)
        env
        armed
    in
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      let ledger = S.delegations store in
      let request_key = key "cancelled-stage" in
      let selected = reserve ledger request_key (admission ()) |> record in
      armed := Some false;
      (try
         ignore (D.advance ledger selected Artifact_installed);
         assert false
       with
       | Eio.Time.Timeout -> ());
      let retained = D.resolve ledger (D.reference selected) |> store_ok in
      assert (D.equal_stage retained.stage Reserved);
      let advanced = D.advance ledger retained Artifact_installed |> store_ok in
      S.close store |> store_ok;
      let store = reopen env sw root in
      let restored = D.resolve (S.delegations store) (D.reference advanced) |> store_ok in
      assert (D.equal_stage restored.stage Artifact_installed);
      S.close store |> store_ok));
  print_endline
    "original Timeout propagated; owner retries and exact admitted reference reopens";
  [%expect
    {|original Timeout propagated; owner retries and exact admitted reference reopens|}]
;;

let%expect_test
    "delegation setup and retained reads propagate cancellation without poisoning"
  =
  with_temp_directory "ochat-delegation-setup-cancel" (fun env root ->
    let cancel_setup = ref false in
    let cancel_read = ref false in
    let armed = ref None in
    let env =
      Job_store_fixtures.fault_env
        ~before_open_in:(fun path ->
          if !cancel_setup && String.equal path (Filename.concat root ".")
          then (
            cancel_setup := false;
            raise Eio.Time.Timeout);
          if !cancel_read && String.is_suffix path ~suffix:".frame"
          then (
            cancel_read := false;
            raise Eio.Time.Timeout))
        env
        armed
    in
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      let ledger = S.delegations store in
      let request_key = key "setup-cancellation" in
      cancel_setup := true;
      (try
         ignore (reserve ledger request_key (admission ()));
         assert false
       with
       | Eio.Time.Timeout -> ());
      assert (not !cancel_setup);
      assert (Option.is_none (D.find ledger request_key |> store_ok));
      let selected = reserve ledger request_key (admission ()) |> record in
      cancel_read := true;
      (try
         ignore (D.resolve ledger (D.reference selected));
         assert false
       with
       | Eio.Time.Timeout -> ());
      assert (not !cancel_read);
      assert (
        D.Reference.equal
          (D.reference selected)
          (D.reference (D.find ledger request_key |> store_ok |> Option.value_exn)));
      S.close store |> store_ok;
      let store = reopen env sw root in
      D.resolve (S.delegations store) (D.reference selected) |> store_ok |> ignore;
      S.close store |> store_ok));
  print_endline
    "setup and bounded-read Timeout propagate; no partial admission or poisoned owner";
  [%expect
    {|setup and bounded-read Timeout propagate; no partial admission or poisoned owner|}]
;;
