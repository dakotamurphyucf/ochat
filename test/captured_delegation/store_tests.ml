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
