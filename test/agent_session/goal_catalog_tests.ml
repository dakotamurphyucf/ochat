open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module G = P.Goal
module D = Document_schema
module C = A.Goal_catalog
module Command = A.Goal_command

let session =
  P.Session_ref.create
    ~server_id:(P.Id.Server.of_string "srv_goal_catalog" |> protocol_ok)
    ~session_id
;;

let goal_id name = P.Id.Goal.of_string ("goal_" ^ name) |> protocol_ok
let objective text = G.Objective.of_string text |> protocol_ok
let progress text = G.Progress.of_string text |> protocol_ok
let reason text = G.Reason.of_string text |> protocol_ok

let observer : P.Invocation.observer =
  { script_id = "goal-catalog"; source_sha256 = String.make 64 'a' }
;;

let source ?(generation = 0) epoch =
  P.Run_source.create ~observer ~generation ~installation_epoch:epoch |> protocol_ok
;;

let request catalog ?(generation = 0) ?(current_source = source 1L) key mutation =
  let id = Command.Mutation.goal_id mutation in
  let expected_goal_revision = Option.map (C.find catalog id) ~f:G.revision in
  Command.create
    ~session_id
    ~generation
    ~source:(if Command.Mutation.needs_source mutation then Some current_source else None)
    ~expected_catalog_revision:(C.revision catalog)
    ~expected_goal_revision
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~mutation
  |> protocol_ok
;;

let apply catalog ?(generation = 0) ?(current_source = source 1L) request =
  A.Goal_command_transition.prepare
    catalog
    ~request
    ~principal_id
    ~session
    ~generation
    ~current_source:(Some current_source)
    ~now:timestamp
;;

let committed = function
  | Ok (A.Goal_command_transition.Commit { catalog; receipt }) -> catalog, receipt
  | Ok (Retained _) -> failwith "expected fresh goal command"
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let create catalog name =
  let id = goal_id name in
  let request =
    request
      catalog
      ("create-" ^ name)
      (Create { id; objective = objective name; budget = G.Budget.unlimited })
  in
  let catalog, receipt = apply catalog request |> committed in
  catalog, request, receipt
;;

let set json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected object"
;;

let member json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith ("missing fixture field " ^ name)
;;

let edit_goal raw id ~f =
  match member raw "goals" with
  | `Array rows ->
    set
      raw
      "goals"
      (`Array
          (List.map rows ~f:(fun row ->
             if D.Json.equal (member row "id") (P.Id.Goal.to_json id)
             then set row "document" (f (member row "document"))
             else row)))
  | _ -> failwith "expected goal array"
;;

(* Explicit fixture import; no actor command enables a goal in this slice. *)
let enable_fixture catalog id =
  let goal = C.find catalog id |> Option.value_exn |> G.resume |> protocol_ok in
  let previous = C.document catalog id |> Option.value_exn in
  let document =
    A.Goal_document.update previous goal
    |> document_ok
    |> A.Goal_document.to_document
    |> document_ok
    |> D.Document.json
  in
  C.to_jsonaf catalog
  |> protocol_ok
  |> fun raw ->
  edit_goal raw id ~f:(fun _ -> document)
  |> fun raw ->
  set raw "revision" (`String (Int64.to_string (Int64.succ (C.revision catalog))))
  |> C.of_jsonaf
  |> protocol_ok
;;

let%expect_test "compact independent metadata model and exact retained evidence" =
  let catalog, first, receipt = create C.empty "stable" in
  let id = goal_id "stable" in
  let model_revision = ref 0L
  and model_progress = ref ""
  and model_objective = ref "stable" in
  let catalog =
    List.fold (List.init 36 ~f:Fn.id) ~init:catalog ~f:(fun catalog n ->
      let mutation : Command.Mutation.t =
        if n % 2 = 0
        then (
          model_progress := Int.to_string n;
          Record_progress (id, progress !model_progress))
        else (
          model_objective := "objective-" ^ Int.to_string n;
          Revise_objective (id, objective !model_objective))
      in
      let catalog, _ =
        apply catalog (request catalog ("model-" ^ Int.to_string n) mutation) |> committed
      in
      model_revision := Int64.succ !model_revision;
      let actual = C.find catalog id |> Option.value_exn in
      assert (Int64.equal (G.revision actual) !model_revision);
      assert (String.equal (G.Progress.to_string (G.progress actual)) !model_progress);
      assert (String.equal (G.Objective.to_string (G.objective actual)) !model_objective);
      assert (P.Id.Principal.equal (G.creator actual) principal_id);
      assert (P.Session_ref.equal (G.session actual) session);
      let reopened = C.to_jsonaf catalog |> protocol_ok |> C.of_jsonaf |> protocol_ok in
      assert (
        D.Json.equal
          (C.to_jsonaf catalog |> protocol_ok)
          (C.to_jsonaf reopened |> protocol_ok));
      reopened)
  in
  (match apply catalog first |> protocol_ok with
   | Retained actual -> assert (A.Goal_command_receipt.equal actual receipt)
   | Commit _ -> assert false);
  let conflicting =
    Command.create
      ~session_id
      ~generation:0
      ~source:(Some (source 1L))
      ~expected_catalog_revision:0L
      ~expected_goal_revision:None
      ~key:first.key
      ~mutation:
        (Create { id; objective = objective "changed"; budget = G.Budget.unlimited })
    |> protocol_ok
  in
  assert (Result.is_error (apply catalog conflicting));
  let malformed_goal = P.Id.Goal.t_of_sexp (Sexp.Atom "forged") in
  assert (
    Result.is_error
      (Command.create
         ~session_id
         ~generation:0
         ~source:(Some (source 1L))
         ~expected_catalog_revision:0L
         ~expected_goal_revision:None
         ~key:first.key
         ~mutation:
           (Create
              { id = malformed_goal
              ; objective = objective "x"
              ; budget = G.Budget.unlimited
              })));
  let malformed_principal = P.Id.Principal.t_of_sexp (Sexp.Atom "forged") in
  assert (
    Result.is_error
      (A.Goal_command_receipt.create
         ~principal_id:malformed_principal
         ~key:receipt.key
         ~request_sha256:receipt.request_sha256
         ~goal_id:id
         ~goal_revision:0L
         ~catalog_revision:1L
         ~source:None
         ~accepted_at:timestamp));
  assert (
    A.Goal_command_receipt.equal
      receipt
      (A.Goal_command_receipt.to_json receipt
       |> A.Goal_command_receipt.of_json
       |> protocol_ok));
  print_endline "model, codec, nominal input guards and immutable retry agree";
  [%expect {| model, codec, nominal input guards and immutable retry agree |}]
;;

let%expect_test
    "finite retention fails fresh keys closed; internal holds need no receipt slot"
  =
  let catalog, first, receipt = create C.empty "capacity" in
  let id = goal_id "capacity" in
  let catalog =
    List.fold (List.init 255 ~f:Fn.id) ~init:catalog ~f:(fun catalog n ->
      apply
        catalog
        (request
           catalog
           ("slot-" ^ Int.to_string n)
           (Record_progress (id, progress (Int.to_string n))))
      |> committed
      |> fst)
  in
  assert (List.length (C.receipts catalog) = 256);
  assert (
    Result.is_error
      (apply catalog (request catalog "saturated" (Hold (id, Operator_pause)))));
  (match apply catalog first |> protocol_ok with
   | Retained r -> assert (A.Goal_command_receipt.equal r receipt)
   | Commit _ -> assert false);
  let enabled = enable_fixture catalog id in
  let held = C.hold_for_review enabled |> protocol_ok |> Option.value_exn in
  assert (List.length (C.receipts held) = 256);
  assert (Option.is_none (C.hold_for_review held |> protocol_ok));
  let near_max =
    C.to_jsonaf enabled
    |> protocol_ok
    |> fun raw ->
    set raw "revision" (`String (Int64.to_string (Int64.pred Int64.max_value)))
    |> C.of_jsonaf
    |> protocol_ok
  in
  let last_hold = C.hold_for_review near_max |> protocol_ok |> Option.value_exn in
  [%test_eq: int64] (C.revision last_hold) Int64.max_value;
  assert (
    Result.is_error
      (C.to_jsonaf enabled
       |> protocol_ok
       |> fun raw ->
       C.of_jsonaf (set raw "revision" (`String (Int64.to_string Int64.max_value)))));
  let sixteen =
    List.fold (List.init 16 ~f:Fn.id) ~init:C.empty ~f:(fun catalog n ->
      create catalog ("bounded" ^ Int.to_string n) |> fun (catalog, _, _) -> catalog)
  in
  let rejected =
    request
      sixteen
      "seventeenth"
      (Create
         { id = goal_id "extra"; objective = objective "x"; budget = G.Budget.unlimited })
  in
  assert (Result.is_error (apply sixteen rejected));
  let one = enable_fixture sixteen (goal_id "bounded0") in
  let second =
    C.find one (goal_id "bounded1") |> Option.value_exn |> G.resume |> protocol_ok
  in
  let second_document =
    C.document one (G.id second)
    |> Option.value_exn
    |> fun d ->
    A.Goal_document.update d second
    |> document_ok
    |> A.Goal_document.to_document
    |> document_ok
    |> D.Document.json
  in
  let invalid =
    C.to_jsonaf one
    |> protocol_ok
    |> fun raw -> edit_goal raw (G.id second) ~f:(fun _ -> second_document)
  in
  assert (Result.is_error (C.of_jsonaf invalid));
  print_endline "16 goals; 256 receipts; no eviction; recovery hold remains available";
  [%expect {| 16 goals; 256 receipts; no eviction; recovery hold remains available |}]
;;

let initial_with_source env workspace_instance =
  let manager, _, _ = handoff_definition env in
  let snapshot =
    Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
  in
  let initial =
    actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
  in
  { initial with moderator = Some (A.Moderator_checkpoint.encode snapshot) }
;;

let principal ?(id = principal_id) scopes =
  P.Principal.create
    ~id
    ~authentication_kind:"fixture"
    ~scopes:(P.Scope.Set.of_list scopes)
    ~attributes:[]
  |> protocol_ok
;;

let authorize _ = Ok ()

let%expect_test
    "actual operator command failure retries once, expires, rotates and remains \
     controllable after removal"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let initial = initial_with_source env workspace_instance in
      let fail = ref true
      and saves = ref 0 in
      let persistence : A.Session_actor.persistence =
        { archive_reference
        ; commit =
            (fun ~command_audit:_ ~previous:_ _ ->
              if !fail
              then
                Error
                  (P.Error.create
                     Persistence_error
                     ~message:"definite no-write failure"
                     ~retryable:true
                     ())
              else (
                Int.incr saves;
                Ok ()))
        }
      in
      let actor = Run_persistence_tests.create_actor env sw initial persistence in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown actor)
        ~f:(fun () ->
          let current = ref true in
          let proof =
            Operator_authorization.guarded
              ~principal:(principal [ Send_messages; View_session_transcript ])
              ~is_current:(fun () -> !current)
          in
          let _, actual_source = A.Goal_catalog_source.capture initial |> protocol_ok in
          let command =
            request
              C.empty
              ~current_source:actual_source
              "failed-then-retry"
              (Create
                 { id = goal_id "actor"
                 ; objective = objective "retained"
                 ; budget = G.Budget.unlimited
                 })
          in
          let call command =
            A.Session_actor.goal_command
              actor
              ~actor:proof
              ~authorize
              ~session
              ~request:command
          in
          assert (Result.is_error (call command));
          let failed = A.Session_actor.state actor |> protocol_ok in
          assert (Option.is_none failed.goal_catalog && Option.is_none failed.run_state);
          fail := false;
          let accepted = call command |> protocol_ok in
          let accepted_state = A.Session_actor.state actor |> protocol_ok in
          assert (
            List.is_empty (A.Run_state.runs (Option.value_exn accepted_state.run_state)));
          let before_retry = !saves in
          assert (A.Goal_command_receipt.equal accepted (call command |> protocol_ok));
          assert (!saves = before_retry);
          current := false;
          assert (Result.is_error (call command));
          assert (
            Result.is_error (A.Session_actor.read_goals actor ~actor:proof ~authorize));
          current := true;
          assert (
            List.length
              (A.Session_actor.read_goals actor ~actor:proof ~authorize |> protocol_ok)
            = 1);
          (* A host policy callback may invalidate the original proof while
             deciding access. Neither a retained receipt nor a read can escape
             the final authentication check. *)
          let revoke _ =
            current := false;
            Ok ()
          in
          let saved = !saves in
          assert (
            Result.is_error
              (A.Session_actor.read_goals actor ~actor:proof ~authorize:revoke));
          current := true;
          assert (
            Result.is_error
              (A.Session_actor.goal_command
                 actor
                 ~actor:proof
                 ~authorize:revoke
                 ~session
                 ~request:command));
          assert (!saves = saved);
          current := true;
          let no_scope = Operator_authorization.trusted_local (principal []) in
          assert (
            Result.is_error
              (A.Session_actor.goal_command
                 actor
                 ~actor:no_scope
                 ~authorize
                 ~session
                 ~request:command));
          let writer, _ =
            A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
          in
          let ids =
            A.History_id_source.create
              ~namespace:(P.Id.Session.to_string session_id)
              ~block_size:1
              ~reserve:(fun ~count -> A.Session_actor.reserve_history_block actor ~count)
            |> protocol_ok
          in
          let entry =
            A.History_codec.user_text
              ~id:(A.History_id_source.allocate ids |> protocol_ok)
              "ordinary message"
            |> A.History_codec.to_protocol
          in
          A.Session_actor.append_history actor ~attachment_id:writer.id [ entry ]
          |> protocol_ok
          |> ignore;
          let after_message = A.Session_actor.state actor |> protocol_ok in
          assert (
            List.equal
              G.equal
              (C.goals (Option.value_exn accepted_state.goal_catalog))
              (C.goals (Option.value_exn after_message.goal_catalog)));
          A.Session_actor.change_moderator actor None |> protocol_ok |> ignore;
          let removed = A.Session_actor.state actor |> protocol_ok in
          let catalog = Option.value_exn removed.goal_catalog in
          assert (A.Goal_command_receipt.equal accepted (call command |> protocol_ok));
          let stale_edit =
            request
              catalog
              ~current_source:actual_source
              "edit-old-source"
              (Record_progress (goal_id "actor", progress "no"))
          in
          assert (Result.is_error (call stale_edit));
          let safe =
            request catalog "pause-removed" (Hold (goal_id "actor", Operator_pause))
          in
          call safe |> protocol_ok |> ignore;
          let catalog =
            (A.Session_actor.state actor |> protocol_ok).goal_catalog |> Option.value_exn
          in
          let abandon =
            request
              catalog
              "abandon-removed"
              (Abandon (goal_id "actor", reason "operator disposition"))
          in
          call abandon |> protocol_ok |> ignore;
          let goals =
            A.Session_actor.read_goals actor ~actor:proof ~authorize |> protocol_ok
          in
          assert (
            match G.state (List.hd_exn goals) with
            | Closed (Abandoned _) -> true
            | Open _ | Closed (Achieved _) -> false))));
  print_endline
    "no-write retry, retained receipt, live authority and removed-source controls use \
     actual actor";
  [%expect
    {| no-write retry, retained receipt, live authority and removed-source controls use actual actor |}]
;;

let%expect_test
    "universal carrier preserves absence, nested future data and source/reset provenance"
  =
  with_actor_workspace (fun env workspace_instance ->
    let initial = initial_with_source env workspace_instance in
    let index, actual_source = A.Goal_catalog_source.capture initial |> protocol_ok in
    let id = goal_id "document" in
    let fresh =
      request
        C.empty
        ~current_source:actual_source
        "document-create"
        (Create
           { id; objective = objective "outside transcript"; budget = G.Budget.unlimited })
    in
    let catalog, receipt =
      apply C.empty ~current_source:actual_source fresh |> committed
    in
    let state = { initial with run_state = Some index; goal_catalog = Some catalog } in
    let initial_carrier =
      A.Session_state_document.decode ~limits:document_limits (state_document initial)
      |> document_ok
    in
    let require_initial_transition candidate accepted =
      let candidate_state = { state with goal_catalog = Some candidate } in
      let incoming =
        A.Session_state_document.decode
          ~limits:document_limits
          (state_document candidate_state)
        |> document_ok
      in
      (* A full snapshot may contain advanced historical goals. Introducing new
         entries through a mutation still needs initial command provenance. *)
      List.iter
        [ Result.is_ok (C.validate_transition C.empty ~incoming:candidate)
        ; Result.is_ok (A.Session_delta.apply initial (Goal_catalog_changed candidate))
        ; Result.is_ok (A.Session_delta.apply initial (Created candidate_state))
        ; Result.is_ok
            (A.Session_state_document.adopt
               initial_carrier
               ~limits:document_limits
               incoming)
        ]
        ~f:(fun actual -> assert (Bool.equal actual accepted))
    in
    require_initial_transition catalog true;
    let raw = C.to_jsonaf catalog |> protocol_ok in
    let malformed =
      [ set raw "receipts" (`Array [])
      ; edit_goal raw id ~f:(fun document ->
          set
            document
            "payload"
            (set
               (member document "payload")
               "creator"
               (P.Id.Principal.to_json
                  (P.Id.Principal.of_string "pri_other_goal_creator" |> protocol_ok))))
      ; (match member raw "receipts" with
         | `Array [ row ] ->
           set
             raw
             "receipts"
             (`Array [ set row "receipt" (set (member row "receipt") "source" `Null) ])
         | _ -> assert false)
      ]
    in
    List.iter malformed ~f:(fun raw ->
      require_initial_transition (C.of_jsonaf raw |> protocol_ok) false);
    require_initial_transition (enable_fixture catalog id) false;
    let encoded = state_document state in
    let catalog_raw =
      C.to_jsonaf catalog
      |> protocol_ok
      |> fun raw ->
      set raw "future_catalog" (`Object [ "keep", `Null ])
      |> fun raw ->
      edit_goal raw id ~f:(fun doc ->
        set
          doc
          "payload"
          (set
             (member doc "payload")
             "future_goal"
             (`Object [ "keep", `String "protected" ])))
    in
    let encoded =
      D.Document.json encoded
      |> fun raw ->
      set raw "payload" (set (member raw "payload") "goal_catalog" catalog_raw)
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let carrier =
      A.Session_state_document.decode ~limits:document_limits encoded |> document_ok
    in
    let native = A.Session_state_document.value carrier in
    let catalog = Option.value_exn native.goal_catalog in
    let next, _ =
      apply
        catalog
        ~current_source:actual_source
        (request
           catalog
           ~current_source:actual_source
           "doc-progress"
           (Record_progress (id, progress "preserved")))
      |> committed
    in
    let delta = A.Session_delta.Goal_catalog_changed next in
    let delta_document =
      A.Session_delta_document.create
        delta
        ~limits:document_limits
        ~state_document:A.Session_state_document.authored
      |> document_ok
    in
    let adopted =
      A.Session_delta_document.apply delta_document ~limits:document_limits carrier
      |> document_ok
    in
    let after =
      A.Session_state_document.encode adopted ~limits:document_limits |> document_ok
    in
    let raw = member (D.Document.payload after) "goal_catalog" in
    assert (D.Json.equal (member raw "future_catalog") (`Object [ "keep", `Null ]));
    let goal_doc =
      match member raw "goals" with
      | `Array [ row ] -> member row "document"
      | _ -> assert false
    in
    assert (
      D.Json.equal
        (member (member goal_doc "payload") "future_goal")
        (`Object [ "keep", `String "protected" ]));
    let before = A.Session_state_document.value adopted in
    let native_snapshot = A.Session_state.snapshot ~now:timestamp before in
    let snapshot_roundtrip =
      P.Snapshot.to_json native_snapshot |> P.Snapshot.of_json |> protocol_ok
    in
    assert (List.equal G.equal native_snapshot.goals snapshot_roundtrip.goals);
    let sexp_roundtrip = P.Snapshot.sexp_of_t native_snapshot |> P.Snapshot.t_of_sexp in
    assert (List.equal G.equal native_snapshot.goals sexp_roundtrip.goals);
    let view = principal [ View_session_transcript ] in
    let public =
      Agent_server.Principal_projection.snapshot view native_snapshot
      |> protocol_ok
      |> P.Public.Snapshot.fields
    in
    assert (List.length public.goals = 1);
    let hidden =
      Agent_server.Principal_projection.snapshot (principal []) native_snapshot
      |> protocol_ok
      |> P.Public.Snapshot.fields
    in
    assert (List.is_empty hidden.goals);
    let reset =
      A.Administration.reset
        before
        { keep_history = false
        ; keep_tasks = false
        ; keep_grants = false
        ; keep_labels = false
        ; workspace_instance = None
        }
      |> protocol_ok
    in
    let goal = C.find (Option.value_exn reset.goal_catalog) id |> Option.value_exn in
    assert (P.Id.Principal.equal (G.creator goal) principal_id);
    assert (P.Run_source.equal (G.source goal) actual_source);
    assert (String.equal (G.Progress.to_string (G.progress goal)) "preserved");
    assert (
      List.exists
        (C.receipts (Option.value_exn reset.goal_catalog))
        ~f:(A.Goal_command_receipt.equal receipt));
    let absent =
      D.Document.json (state_document initial)
      |> fun raw ->
      let payload = member raw "payload" in
      let payload =
        match payload with
        | `Object fields ->
          `Object (List.Assoc.remove fields ~equal:String.equal "goal_catalog")
        | _ -> assert false
      in
      set raw "payload" payload
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let absent =
      A.Session_state_document.decode ~limits:document_limits absent
      |> document_ok
      |> fun c -> A.Session_state_document.encode c ~limits:document_limits |> document_ok
    in
    assert (
      match D.Json.field (D.Document.payload absent) ~name:"goal_catalog" with
      | Absent -> true
      | Null | Value _ -> false);
    let null =
      A.Session_state_document.decode ~limits:document_limits (state_document initial)
      |> document_ok
      |> fun c -> A.Session_state_document.encode c ~limits:document_limits |> document_ok
    in
    assert (
      match D.Json.field (D.Document.payload null) ~name:"goal_catalog" with
      | Null -> true
      | Absent | Value _ -> false);
    let encoded_bytes = String.length (D.Document.to_string after) in
    let tight =
      D.Limits.create
        ~max_bytes:encoded_bytes
        ~max_depth:128
        ~max_fields:500000
        ~max_nodes:1000000
      |> document_ok
    in
    assert (Result.is_error (A.Session_state_document.encode adopted ~limits:tight)));
  print_endline
    "one carrier retains nested unknowns, absence/null, transcript projection and reset \
     evidence; host reserve is enforced";
  [%expect
    {| one carrier retains nested unknowns, absence/null, transcript projection and reset evidence; host reserve is enforced |}]
;;

let%expect_test
    "physical lost acknowledgement reopens one accepted catalog receipt without execution"
  =
  let module Store = Agent_store in
  let module Persistence = A.Session_persistence in
  List.iter [ false; true ] ~f:(fun checkpoint ->
    with_actor_workspace (fun env workspace_instance ->
      let initial = initial_with_source env workspace_instance in
      let _, actual_source = A.Goal_catalog_source.capture initial |> protocol_ok in
      let command =
        request
          C.empty
          ~current_source:actual_source
          "physical-catalog"
          (Create
             { id = goal_id "physical"
             ; objective = objective "durable"
             ; budget = G.Budget.unlimited
             })
      in
      let proof =
        Operator_authorization.trusted_local
          (principal [ Send_messages; View_session_transcript ])
      in
      let directory =
        Filename.concat workspace_instance.canonical_root.native_path "goal-journal"
      in
      let snapshots =
        Filename.concat workspace_instance.canonical_root.native_path "goal-snapshots"
      in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / snapshots);
      let expected =
        Eio.Switch.run (fun sw ->
          let journal =
            Store.Journal.create
              ~env
              ~directory
              ~max_payload_length:1048576
              ~max_segment_bytes:4194304L
              ~max_segment_frames:16
            |> store_ok
          in
          let writer =
            Store.Commit_writer.create
              ~sw
              ~journal
              ~session_id
              ~next_transaction_sequence:1L
              ~previous_transaction_hash:None
              ~queue_capacity:8
            |> store_ok
          in
          let durable =
            Run_persistence_tests.create_persistence
              writer
              (Persistence.Restored.authored initial)
              ~previous_transaction_hash:None
          in
          let persistence : A.Session_actor.persistence =
            { archive_reference
            ; commit =
                (fun ~command_audit ~previous transition ->
                  let%bind.Result () =
                    Persistence.commit durable ~command_audit ~previous transition
                  in
                  Error
                    (P.Error.create
                       Persistence_error
                       ~message:"lost ACK after actual commit"
                       ~retryable:true
                       ()))
            }
          in
          let actor = Run_persistence_tests.create_actor env sw initial persistence in
          Exn.protect
            ~finally:(fun () ->
              A.Session_actor.shutdown actor;
              Store.Commit_writer.close writer)
            ~f:(fun () ->
              assert (
                Result.is_error
                  (A.Session_actor.goal_command
                     actor
                     ~actor:proof
                     ~authorize
                     ~session
                     ~request:command));
              assert (
                Option.is_none (A.Session_actor.state actor |> protocol_ok).goal_catalog);
              let accepted = Persistence.Restored.state (Persistence.restored durable) in
              let catalog = Option.value_exn accepted.goal_catalog in
              assert (List.length (C.receipts catalog) = 1);
              if checkpoint
              then
                Persistence.install_snapshot_at
                  durable
                  ~env
                  ~directory:snapshots
                  ~max_payload_length:1048576
                  ~transaction_hash:(Persistence.transaction_hash durable)
                  accepted
                |> store_ok
                |> ignore;
              List.hd_exn (C.receipts catalog)))
      in
      Eio.Switch.run (fun sw ->
        let journal =
          Store.Journal.open_existing
            ~env
            ~directory
            ~max_payload_length:1048576
            ~max_segment_bytes:4194304L
            ~max_segment_frames:16
          |> store_ok
        in
        let recovered =
          Store.Recovery.load
            ~env
            ~journal
            ~snapshot_directory:snapshots
            ~max_snapshot_payload_length:1048576
            ~session_id
            ~initial:(Persistence.Restored.authored initial)
            ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
            ~apply:(Persistence.apply_transaction ~limits:document_limits)
            ~validate_transaction:
              (Persistence.validate_transaction ~limits:document_limits)
            ~validate:Persistence.validate
          |> store_ok
        in
        let restored = Persistence.Restored.state recovered.state in
        let writer =
          Store.Commit_writer.create
            ~sw
            ~journal
            ~session_id
            ~next_transaction_sequence:(Int64.succ recovered.latest_transaction_sequence)
            ~previous_transaction_hash:recovered.latest_transaction_hash
            ~queue_capacity:8
          |> store_ok
        in
        let durable =
          Run_persistence_tests.create_persistence
            writer
            recovered.state
            ~previous_transaction_hash:recovered.latest_transaction_hash
        in
        let actor =
          Run_persistence_tests.create_actor
            env
            sw
            restored
            (Persistence.actor_persistence durable)
        in
        Exn.protect
          ~finally:(fun () ->
            A.Session_actor.shutdown actor;
            Store.Commit_writer.close writer)
          ~f:(fun () ->
            A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
            let before = A.Session_actor.state actor |> protocol_ok in
            let actual =
              A.Session_actor.goal_command
                actor
                ~actor:proof
                ~authorize
                ~session
                ~request:command
              |> protocol_ok
            in
            assert (A.Goal_command_receipt.equal expected actual);
            let after = A.Session_actor.state actor |> protocol_ok in
            assert (Int64.equal before.counters.revision after.counters.revision);
            assert (List.length (C.receipts (Option.value_exn after.goal_catalog)) = 1);
            assert (List.is_empty (A.Run_state.runs (Option.value_exn after.run_state)));
            assert (Option.is_none after.active_operation);
            assert (
              P.Id.Principal.equal
                (G.creator (List.hd_exn (C.goals (Option.value_exn after.goal_catalog))))
                principal_id)))));
  print_endline
    "journal replay and snapshot reopen reconcile one immutable receipt; no Run, worker \
     or repeated write";
  [%expect
    {| journal replay and snapshot reopen reconcile one immutable receipt; no Run, worker or repeated write |}]
;;

let%expect_test
    "actual source, recovery and reset hold without adopting or consuming command slots"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let initial = initial_with_source env workspace_instance in
      let index, actual_source = A.Goal_catalog_source.capture initial |> protocol_ok in
      let first = goal_id "foreground"
      and paused = goal_id "paused" in
      let create_one catalog id key =
        let command =
          request
            catalog
            ~current_source:actual_source
            key
            (Create { id; objective = objective key; budget = G.Budget.unlimited })
        in
        apply catalog ~current_source:actual_source command |> committed |> fst
      in
      let catalog =
        create_one C.empty first "first"
        |> fun catalog -> create_one catalog paused "second"
      in
      let catalog =
        apply
          catalog
          ~current_source:actual_source
          (request catalog "pause-second" (Hold (paused, Operator_pause)))
        |> committed
        |> fst
      in
      let catalog = enable_fixture catalog first in
      let initial =
        { initial with run_state = Some index; goal_catalog = Some catalog }
      in
      let actor =
        Run_persistence_tests.create_actor
          env
          sw
          initial
          { archive_reference; commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
      in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown actor)
        ~f:(fun () ->
          let read () = A.Session_actor.state actor |> protocol_ok in
          let assert_held state =
            let catalog = Option.value_exn state.A.Session_state.goal_catalog in
            assert (
              match G.state (C.find catalog first |> Option.value_exn) with
              | Open (Held Source_review) -> true
              | Open Enabled | Open (Held _) | Closed _ -> false);
            assert (
              match G.state (C.find catalog paused |> Option.value_exn) with
              | Open (Held Operator_pause) -> true
              | Open Enabled | Open (Held _) | Closed _ -> false);
            assert (List.length (C.receipts catalog) = 3);
            assert (
              P.Run_source.equal
                (G.source (C.find catalog first |> Option.value_exn))
                actual_source)
          in
          A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
          let recovered = read () in
          assert_held recovered;
          A.Session_actor.reconcile_run_recovery actor |> protocol_ok;
          assert (Int64.equal recovered.counters.revision (read ()).counters.revision);
          A.Session_actor.change_moderator actor None |> protocol_ok |> ignore;
          assert_held (read ());
          A.Session_actor.change_moderator actor initial.moderator
          |> protocol_ok
          |> ignore;
          let state = read () in
          let _, next_source = A.Goal_catalog_source.capture state |> protocol_ok in
          assert (
            Int64.(next_source.installation_epoch > actual_source.installation_epoch));
          let catalog = Option.value_exn state.goal_catalog in
          let proof =
            Operator_authorization.trusted_local
              (principal [ Send_messages; View_session_transcript ])
          in
          let call request =
            A.Session_actor.goal_command actor ~actor:proof ~authorize ~session ~request
          in
          let edit =
            request
              catalog
              ~current_source:next_source
              "before-adoption"
              (Record_progress (first, progress "must reject"))
          in
          assert (Result.is_error (call edit));
          let adopt =
            request
              catalog
              ~current_source:next_source
              "adopt-current"
              (Adopt_source first)
          in
          call adopt |> protocol_ok |> ignore;
          let adopted = read () in
          let catalog = Option.value_exn adopted.goal_catalog in
          assert (
            P.Run_source.equal
              (G.source (C.find catalog first |> Option.value_exn))
              next_source);
          let writer, _ =
            A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
          in
          let before = read () in
          A.Session_actor.reset
            actor
            ~attachment_id:writer.id
            ~expected_revision:before.counters.revision
            { keep_history = false
            ; keep_tasks = false
            ; keep_grants = false
            ; keep_labels = false
            ; workspace_instance = None
            }
          |> protocol_ok
          |> ignore;
          let reset = read () in
          assert (reset.identity.generation = 1);
          let catalog_after = Option.value_exn reset.goal_catalog in
          assert (List.equal G.equal (C.goals catalog) (C.goals catalog_after));
          assert (List.length (C.receipts catalog_after) = 4);
          let stale_generation =
            request catalog_after "old-generation-control" (Hold (first, Operator_pause))
          in
          assert (Result.is_error (call stale_generation));
          let current_generation =
            request
              catalog_after
              ~generation:1
              "current-generation-control"
              (Hold (first, Operator_pause))
          in
          call current_generation |> protocol_ok |> ignore;
          assert (Option.is_none (read ()).active_operation))));
  print_endline
    "actual recovery is idempotent; source adoption is explicit; reset preserves scope \
     and checks generation";
  [%expect
    {| actual recovery is idempotent; source adoption is explicit; reset preserves scope and checks generation |}]
;;
