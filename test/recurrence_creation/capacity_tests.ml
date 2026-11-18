open! Core
open Creation_support
module Due = Due_support
module Store = Agent_store

(* Discover the accepted frontier through the actual constructor, so a future
   identifier expansion changes this test input rather than a duplicated bound. *)
let maximal of_string prefix character =
  let rec grow count previous =
    if count > 8192 then failwith "identifier contract unexpectedly unbounded";
    match of_string (prefix ^ String.make count character) with
    | Ok value -> grow (count + 1) (Some value)
    | Error _ -> Option.value_exn previous
  in
  grow 1 None
;;

let maximal_reference_envelope () =
  let parent = maximal P.Id.Session.of_string "ses_" 'p' in
  let child = maximal P.Id.Session.of_string "ses_" 'c' in
  let principal = maximal P.Id.Principal.of_string "pri_" 'p' in
  let revision = maximal P.Id.Prompt_revision.of_string "prv_" 'r' in
  let key = maximal P.Idempotency_key.of_string "" 'k' in
  (* Pure validated reference data, not evidence of a created or linked child.
     The actual store encoder determines its complete current wire shape. *)
  let reference =
    Store.Delegation_store.reference_of_jsonaf
      (`Object
          [ ( "key"
            , `Object
                [ "parent_session_id", P.Id.Session.to_json parent
                ; "parent_generation", `Number (Int.to_string Int.max_value)
                ; "principal_id", P.Id.Principal.to_json principal
                ; "idempotency_key", P.Idempotency_key.to_json key
                ] )
          ; "child_session_id", P.Id.Session.to_json child
          ; "revision_id", P.Id.Prompt_revision.to_json revision
          ; "request_sha256", `String (String.make 64 'a')
          ; "admission_sha256", `String (String.make 64 'b')
          ])
    |> store_ok
  in
  let controller =
    P.Session_ref.create
      ~server_id:(maximal P.Id.Server.of_string "srv_" 's')
      ~session_id:parent
  in
  let definition =
    I.Definition.create
      ~controller
      ~recurrence_id:(maximal P.Id.Recurrence.of_string "rec_" 'r')
      ~revision:Int64.max_value
    |> protocol_ok
  in
  let occurrence = I.Occurrence.create ~definition ~slot:Int64.max_value |> protocol_ok in
  `Object
    [ "occurrence", I.Occurrence.to_json occurrence
    ; "reservation_request_sha256", `String (String.make 64 'c')
    ; "reference", Store.Delegation_store.reference_to_jsonaf reference
    ]
;;

let%expect_test "actual maximal Reference wire fits the explicit association quota" =
  let envelope = maximal_reference_envelope () in
  Reservation.Association_capacity.validate envelope |> protocol_ok;
  (* This independent slack is not the production reservation's 4 KiB cushion.
     Future store shape/identifier growth must trigger an explicit quota review. *)
  assert (String.length (Jsonaf.to_string envelope) < 2048);
  assert (
    Result.is_error
      (Reservation.Association_capacity.validate
         (Due.append envelope "future" (`String (String.make 4096 'x')))));
  print_endline "validated store wire; quota only, no child authority";
  [%expect {| validated store wire; quota only, no child authority |}]
;;

let%expect_test "actual child link coexists with its alarm within all reserved counters" =
  with_actor_workspace (fun _ workspace ->
    let initial =
      Domain_tests.captured_definition workspace (Due.definition "association_fields")
      |> Due.enabled
    in
    let selected, _, _ =
      Due.step initial ~phase:None (Due.snapshot ~wall:(Due.at_ms 0) ~elapsed_ms:0)
    in
    let reserved, row = Domain_tests.reserve_entry selected 0L in
    let frontier = Due.frontier reserved in
    assert (Option.is_some (H.alarm frontier));
    let key = Reservation.delegation_key row |> protocol_ok in
    let reference =
      Store.Delegation_store.reference_of_jsonaf
        (`Object
            [ ( "key"
              , `Object
                  [ "parent_session_id", P.Id.Session.to_json key.parent_session_id
                  ; "parent_generation", `Number (Int.to_string key.parent_generation)
                  ; "principal_id", P.Id.Principal.to_json key.principal_id
                  ; "idempotency_key", P.Idempotency_key.to_json key.idempotency_key
                  ] )
            ; "child_session_id", P.Id.Session.to_json (Reservation.child_session_id row)
            ; ( "revision_id"
              , P.Id.Prompt_revision.to_json (Reservation.artifact_revision_id row) )
            ; "request_sha256", `String (Reservation.request_sha256 row)
            ; "admission_sha256", `String (String.make 64 'a')
            ])
      |> store_ok
    in
    let module Association = A.Recurrence_child_association in
    let association = Association.create ~reservation:row ~reference |> protocol_ok in
    (* Discover the exact maximum accepted field count through the real decoder.
       Unknowns remain literal null fields; the expected growth is measured from
       the actual before/after documents, independently of the reserve witness. *)
    let rec saturate json count =
      if count > 256 then failwith "association field contract unexpectedly expanded";
      let candidate = Due.append json ("future_" ^ Int.to_string count) `Null in
      match Association.of_json candidate with
      | Ok _ -> saturate candidate (count + 1)
      | Error _ -> Association.of_json json |> protocol_ok
    in
    let association = saturate (Association.to_json association) 0 in
    let linked = H.link_child frontier association |> protocol_ok in
    assert (Option.is_some (H.alarm linked));
    let measure frontier =
      H.to_document frontier
      |> document_ok
      |> D.Document.json
      |> A.Capacity_growth.measure ~limits:document_limits
      |> protocol_ok
    in
    let actual =
      A.Capacity_growth.positive_difference (measure linked) ~previous:(measure frontier)
    in
    let reserve = H.reserved_growth frontier |> protocol_ok in
    List.iter
      [ A.Capacity_growth.encoded_bytes
      ; A.Capacity_growth.fields
      ; A.Capacity_growth.nodes
      ]
      ~f:(fun counter -> assert (counter actual <= counter reserve));
    let restored = H.to_document linked |> document_ok |> H.of_document |> document_ok in
    assert (Association.equal association (List.hd_exn (H.child_associations restored)));
    let restored_reserved =
      H.to_document frontier |> document_ok |> H.of_document |> document_ok
    in
    (* Actor candidates retain their original carrier; persistence decodes the
       required-semantics wire. Both representations must admit the same exact
       state and the same reservation-to-link transition without losing unknowns. *)
    List.iter
      [ frontier, restored_reserved
      ; restored_reserved, frontier
      ; linked, restored
      ; restored, linked
      ; restored_reserved, linked
      ; frontier, restored
      ]
      ~f:(fun (previous, incoming) ->
        H.validate_transition
          previous
          ~incoming
          ~definitions:(E.definitions reserved)
          ~previous_cursor:(E.cursor reserved)
          ~cursor:(E.cursor reserved)
        |> protocol_ok);
    assert (Association.equal association (List.hd_exn (H.child_associations linked)));
    print_endline
      "maximum compatible association; alarm retained; bytes/fields/nodes covered");
  [%expect
    {| maximum compatible association; alarm retained; bytes/fields/nodes covered |}]
;;

let add_padding document count =
  let json = D.Document.json document in
  let payload = Due.field json "payload" in
  let catalog = Due.field payload "recurrence_catalog" in
  let catalog =
    Due.append catalog "future_capacity_padding" (`String (String.make count 'x'))
  in
  Due.set json "payload" (Due.set payload "recurrence_catalog" catalog)
  |> D.Document.inspect ~limits:document_limits
  |> document_ok
  |> A.Session_state_document.decode ~limits:document_limits
  |> document_ok
;;

let future_transaction carrier =
  let document =
    A.Session_state_document.encode carrier ~limits:document_limits |> document_ok
  in
  let recurrence =
    A.Session_state_document.recurrence_capacity carrier ~document ~limits:document_limits
    |> document_ok
  in
  let delta =
    A.Session_delta_document.project_run_retirement
      ~run:None
      ~goal:None
      ~recurrence:(A.Recurrence_control_capacity.projected_catalog recurrence)
      ~limits:document_limits
    |> document_ok
  in
  Store.Transaction.Capacity.document
    ~limits:document_limits
    ~session_id
    ~generation:(A.Session_state_document.value carrier).identity.generation
    ~delta
    ~durable_events:[]
  |> document_ok
;;

let check_future carrier maximum =
  let document =
    A.Session_state_document.encode carrier ~limits:document_limits |> document_ok
  in
  let terminal =
    A.Session_state_document.terminal_capacity carrier ~document ~limits:document_limits
    |> document_ok
  in
  let recurrence =
    A.Session_state_document.recurrence_capacity carrier ~document ~limits:document_limits
    |> document_ok
  in
  A.Session_terminal_journal.validate
    (A.Session_state_document.value carrier)
    ~terminal
    ~recurrence:(Some recurrence)
    ~limits:document_limits
    ~max_payload_length:maximum
;;

let%expect_test
    "independent association credits and near-limit actual control journal preserve \
     custody"
  =
  with_store (fun fixture ->
    let actor = create_actor fixture ~sw:fixture.sw () in
    Exn.protect
      ~finally:(fun () -> A.Session_actor.shutdown actor)
      ~f:(fun () ->
        let host = open_host fixture () in
        Exn.protect
          ~finally:(fun () -> Host.close_and_wait host)
          ~f:(fun () ->
            install actor host;
            List.iter [ "first"; "second" ] ~f:(fun name ->
              create_fresh
                actor
                name
                (interval ~first_due:(P.Timestamp.to_time_ns timestamp) ());
              sweep actor;
              reserve actor name (N.key (selected actor name)) |> protocol_ok |> ignore);
            let initial = state actor in
            A.Session_actor.shutdown actor;
            let catalog = Option.value_exn initial.recurrence_catalog in
            let credits catalog =
              C.reservation catalog
              |> protocol_ok
              |> C.Reservation.required_transition_steps
            in
            (* Two independent association effects must not share the bulk
               control credit: max(2,2) + 1 + 1 = 4. *)
            [%test_eq: int] (credits catalog) 4;
            let boundary =
              { initial with
                counters =
                  { initial.counters with
                    revision = Int64.(max_value - 4L)
                  ; transaction_sequence = Int64.(max_value - 4L)
                  }
              }
            in
            A.Session_state.validate boundary |> protocol_ok;
            assert (
              Result.is_error
                (A.Session_state.validate
                   { boundary with
                     counters =
                       { boundary.counters with revision = Int64.(max_value - 3L) }
                   }));
            let original =
              A.Session_persistence.restored fixture.durable
              |> A.Session_persistence.Restored.state_document
              |> fun carrier ->
              A.Session_state_document.encode carrier ~limits:document_limits
              |> document_ok
            in
            let empty = add_padding original 0 in
            let maximum = Store.Commit_writer.max_payload_length fixture.writer in
            let base =
              future_transaction empty
              |> D.Document.json
              |> Jsonaf.to_string
              |> String.length
            in
            let padding = maximum - base - 32 in
            assert (padding > 0);
            let carrier = add_padding original padding in
            check_future carrier maximum |> protocol_ok;
            assert (
              Result.is_error (check_future (add_padding original (padding + 64)) maximum));
            (* The fixture admits compatible unknown data as its complete owner
               basis. The first actual control delta below durably carries it;
               no claim is made that the earlier journal already contained it. *)
            let durable =
              A.Session_persistence.create
                ~archive:(fun _ _ -> failwith "unexpected archive")
                ~pending_archive:None
                ~before_commit:
                  (Some
                     (fun state ->
                       S.prepare_canonical_projection
                         fixture.store
                         fixture.handle
                         ~metadata:(metadata state)
                         ~entry:(index_entry state)
                       |> Result.map_error ~f:Store.Store_error.to_protocol_error))
                ~command_accepted:(fun _ _ -> ())
                ~writer:fixture.writer
                ~durability:Flush
                ~limits:document_limits
                ~archive_limits:document_limits
                ~retention_preflight:None
                ~restored:(A.Session_persistence.Restored.of_state_document carrier)
                ~previous_transaction_hash:
                  (A.Session_persistence.transaction_hash fixture.durable)
            in
            let commit disposition expected_credits =
              let previous =
                A.Session_persistence.restored durable
                |> A.Session_persistence.Restored.state
              in
              let catalog =
                disposition (Option.value_exn previous.recurrence_catalog)
                |> protocol_ok
                |> Option.value_exn
              in
              [%test_eq: int] (credits catalog) expected_credits;
              let transition =
                A.Session_transition.apply
                  ~now:timestamp
                  previous
                  ~delta:(Recurrence_catalog_changed catalog)
                  ~payloads:[]
                |> protocol_ok
              in
              assert (List.is_empty transition.events);
              A.Session_persistence.commit
                durable
                ~command_audit:None
                ~previous
                transition
              |> protocol_ok;
              let document =
                A.Session_persistence.restored durable
                |> A.Session_persistence.Restored.state_document
                |> fun carrier ->
                A.Session_state_document.encode carrier ~limits:document_limits
                |> document_ok
              in
              let raw = Due.field (D.Document.payload document) "recurrence_catalog" in
              assert (
                D.Json.equal
                  (Due.field raw "future_capacity_padding")
                  (`String (String.make padding 'x')));
              List.iter (C.entries catalog) ~f:(fun entry ->
                let frontier = E.frontier entry |> protocol_ok in
                [%test_eq: int] (List.length (H.protected_children frontier)) 1;
                let encoded = H.to_document frontier |> document_ok in
                assert (
                  List.mem
                    (D.Document.required_semantics encoded)
                    "recurrence.child-reservation.v1"
                    ~equal:String.equal))
            in
            commit C.pause_for_stop 3;
            commit
              (C.retire_stale_births
                 ~current_revision:P.Session_lifecycle.Revision.one
                 ~archived:false)
              2;
            let final =
              A.Session_persistence.restored durable
              |> A.Session_persistence.Restored.state
            in
            assert (equal_state final (replayed fixture));
            print_endline "4 -> 3 -> 2 credits; real pause/retire ACK and replay")));
  [%expect {| 4 -> 3 -> 2 credits; real pause/retire ACK and replay |}]
;;
