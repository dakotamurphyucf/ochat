open! Core
open Support

let%expect_test "real ledger retry cannot substitute captured or historical source kinds" =
  with_temp_directory "ochat-captured-delegation" (fun env root ->
    Eio.Switch.run (fun sw ->
      let owner = ref (create env sw root) in
      Exn.protect
        ~finally:(fun () -> S.close !owner |> store_ok)
        ~f:(fun () ->
          let ledger = S.delegations !owner in
          let captured = origin () in
          let alternatives =
            [ admission "generated"
            ; { (admission "authored") with authored_tool = Some authored }
            ; { (admission "captured") with captured_recurrence = Some captured }
            ]
          in
          let rows =
            List.mapi alternatives ~f:(fun i admission ->
              let key = key ("kind-" ^ Int.to_string i) in
              let original = reserve ledger key admission |> store_ok |> row in
              List.iteri alternatives ~f:(fun j candidate ->
                match reserve ledger key candidate |> store_ok with
                | Replay replay ->
                  assert (i = j);
                  assert (L.Reference.equal (L.reference original) (L.reference replay))
                | Conflict _ -> assert (i <> j)
                | New _ -> failwith "existing scoped key replaced");
              original)
          in
          let original = List.nth_exn rows 2 in
          let changed =
            O.create
              ~reservation:(O.reservation captured)
              ~definition:(append (O.definition captured) "changed" `True)
              ~initial_spec_sha256:(O.initial_spec_sha256 captured)
            |> document_ok
          in
          let candidate =
            { (admission "other") with captured_recurrence = Some changed }
          in
          assert (
            match reserve ledger original.key candidate |> store_ok with
            | Conflict _ -> true
            | New _ | Replay _ -> false);
          let reference = L.reference original in
          let installed = L.advance ledger original Artifact_installed |> store_ok in
          let installed = L.advance ledger installed Child_installed |> store_ok in
          let linked = L.advance ledger installed Linked |> store_ok in
          assert (L.Reference.equal reference (L.reference linked));
          S.close !owner |> store_ok;
          owner := reopen env sw root;
          let ledger = S.delegations !owner in
          let restored = L.resolve ledger reference |> store_ok in
          assert (L.equal_stage restored.stage Linked);
          assert (
            Option.equal O.equal restored.admission.captured_recurrence (Some captured));
          let retry =
            { (admission "fresh_ids") with captured_recurrence = Some captured }
          in
          let replay = reserve ledger original.key retry |> store_ok |> row in
          assert (L.Reference.equal reference (L.reference replay));
          assert (L.equal_stage replay.stage Linked))));
  print_endline
    "three source kinds cannot relabel; exact captured origin, immutable reference and \
     original IDs survive stage cuts and reopen";
  [%expect
    {|three source kinds cannot relabel; exact captured origin, immutable reference and original IDs survive stage cuts and reopen|}]
;;

let%expect_test
    "Goal origin replay freezes Unknown, Unrelated and associated original branch"
  =
  let module G = Agent_store.Delegation_goal_origin in
  with_temp_directory "ochat-goal-origin" (fun env root ->
    Eio.Switch.run (fun sw ->
      let owner = ref (create env sw root) in
      Exn.protect
        ~finally:(fun () -> S.close !owner |> store_ok)
        ~f:(fun () ->
          let rows =
            List.init 3 ~f:(fun index ->
              let name = "goal-origin-" ^ Int.to_string index in
              let key = key name in
              let captured_admission = admission name in
              let associated = goal_origin key captured_admission in
              let origins = [ None; Some G.unrelated; Some associated ] in
              let origin = List.nth_exn origins index in
              let ledger = S.delegations !owner in
              let original =
                reserve ledger key { captured_admission with goal_origin = origin }
                |> store_ok
                |> row
              in
              let document =
                document (C.to_record (C.of_record original |> document_ok))
              in
              let payload = D.Document.payload document in
              let raw_admission =
                append
                  (field payload "admission")
                  "future_goal_admission"
                  (`Number "1e+00")
              in
              let json =
                set
                  (D.Document.json document)
                  "payload"
                  (set payload "admission" raw_admission)
              in
              let json =
                if index = 0 then set json "schema_version" (`Number "6") else json
              in
              let document = inspect json in
              Eio.Path.save
                ~create:(`Or_truncate 0o600)
                (file env root key)
                (frame document);
              let original = L.find ledger key |> store_ok |> Option.value_exn in
              let reference = L.reference original in
              let fresh = admission (name ^ "-fresh") in
              List.iteri origins ~f:(fun other candidate ->
                match
                  reserve ledger key { fresh with goal_origin = candidate } |> store_ok
                with
                | Replay replay ->
                  assert (index = other);
                  assert (L.Reference.equal reference (L.reference replay))
                | Conflict _ -> assert (index <> other)
                | New _ -> failwith "existing origin admission replaced");
              if index = 2
              then (
                let changed = goal_origin ~epoch:2L key captured_admission in
                assert (
                  match
                    reserve ledger key { fresh with goal_origin = Some changed }
                    |> store_ok
                  with
                  | Conflict _ -> true
                  | New _ | Replay _ -> false));
              let advanced = L.advance ledger original Artifact_installed |> store_ok in
              let revoked = L.revoke ledger advanced Parent_stopped |> store_ok in
              assert (L.Reference.equal reference (L.reference revoked));
              key, origin, reference, Jsonaf.to_string raw_admission)
          in
          S.close !owner |> store_ok;
          owner := reopen env sw root;
          let ledger = S.delegations !owner in
          List.iter rows ~f:(fun (key, origin, reference, raw_admission) ->
            let restored = L.resolve ledger reference |> store_ok in
            assert (Option.equal G.equal origin restored.admission.goal_origin);
            assert (Option.is_some restored.revocation);
            let document =
              Agent_store.Document_record.decode_file
                ~limits:C.limits
                ~expected_digest:None
                (Eio.Path.load (file env root key))
              |> Result.map_error ~f:Agent_store.Document_fields.record_error
              |> store_ok
              |> Agent_store.Document_record.document
            in
            assert (
              String.equal
                raw_admission
                (Jsonaf.to_string (field (D.Document.payload document) "admission")));
            assert (
              L.Reference.equal
                reference
                (L.reference
                   (reserve
                      ledger
                      key
                      { (admission "after-reopen") with goal_origin = origin }
                    |> store_ok
                    |> row)))))));
  print_endline
    "historical omission and explicit origins retain original branch, raw hash and bytes \
     across retry/stage/revoke/reopen";
  [%expect
    {|historical omission and explicit origins retain original branch, raw hash and bytes across retry/stage/revoke/reopen|}]
;;
