open Core
open Agent_store_test_fixtures
module P = Agent_protocol
module D = Agent_store.Delegation_store
module S = Agent_store.Session_store
module A = Agent_store.Prompt_artifact_store

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
    ; authority_sha256 = digest "parent policy and effective resource allowance"
    ; capability_pins = [ "read_file", digest "registered parent root" ]
    ; lifetime = Owned
    ; created_at = timestamp
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
      (* An actual v1 frame must retain its original admission digest when read
         and rewritten by the v2 ledger. Absent epoch fields preserve old hashes. *)
      let private_path =
        Filename.concat
          root
          ("delegations/"
           ^ digest (D.Key.sexp_of_t request_key |> Sexp.to_string_mach)
           ^ ".frame")
      in
      let legacy_frame =
        [%sexp { version = (1 : int); record = (installed : D.record) }]
        |> Sexp.to_string_mach
        |> Agent_store.Frame.encode ~max_payload_length:262144 ~flags:0
        |> frame_ok
      in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / private_path)
        legacy_frame;
      let legacy_reference = D.reference installed in
      S.close store |> store_ok;
      let store = reopen env sw root in
      let ledger = S.delegations store in
      assert (D.equal_record installed (D.resolve ledger legacy_reference |> store_ok));
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
