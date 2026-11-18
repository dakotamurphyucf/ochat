open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module G = P.Goal
module D = Document_schema
module S = A.Session_state
module SD = A.Session_state_document
module PC = A.Policy_canonical_usage
module Cause = A.Policy_cause_data
module Book = A.Goal_policy_contributions
module Totals = A.Goal_policy_totals
module L = A.Inference_ledger
module Native = A.Canonical_usage_update
module Origin = Agent_store.Delegation_goal_origin
module Bootstrap = Goal_accounting_bootstrap_tests
module Ledger = Inference_ledger_state_tests

let member = Goal_catalog_tests.member
let set = Goal_catalog_tests.set
let goal_id = Bootstrap.goal_id

let goal state =
  A.Goal_catalog.find (Option.value_exn state.S.goal_catalog) goal_id |> Option.value_exn
;;

let remove json name =
  match json with
  | `Object fields -> `Object (List.Assoc.remove fields name ~equal:String.equal)
  | _ -> failwith "expected object"
;;

let encoded state = SD.encode (SD.authored state) ~limits:document_limits |> document_ok
let book state = Option.value_exn state.S.goal_policy_contributions
let book_json state = Book.to_document (book state) |> protocol_ok |> D.Document.json

let policy_json state =
  PC.to_document (Option.value_exn state.S.policy_canonical_usage)
  |> protocol_ok
  |> D.Document.json
;;

let view state session =
  S.goal_policy_context state ~owner:session
  |> protocol_ok
  |> fun context -> Totals.capture context ~goal_id |> protocol_ok
;;

let equal_component encode left right =
  assert (
    Option.equal (fun left right -> D.Json.equal (encode left) (encode right)) left right)
;;

let metadata (state : S.t) ~revision =
  A.Session_delta_document.Transaction_metadata.create
    ~updated_at:state.identity.updated_at
    ~revision
    ~transaction_sequence:state.counters.transaction_sequence
    ~last_event_sequence:(Some state.counters.event_sequence)
  |> document_ok
;;

let replay ?(state_document = SD.authored) before delta (published : S.t) ~revision =
  let captured =
    A.Session_delta_document.create delta ~limits:document_limits ~state_document
    |> document_ok
  in
  A.Session_delta_document.apply
    captured
    ~transaction_metadata:(metadata published ~revision)
    ~limits:document_limits
    (Bootstrap.carried before)
;;

(* All three real entry points consume the same delta, including journal replay
   before metadata stamping. Opaque components are compared separately. *)
let commit before delta =
  let transition =
    A.Session_transition.apply ~now:timestamp before ~delta ~payloads:[] |> protocol_ok
  in
  let admitted =
    A.Session_document_transition.admit_encoded
      (Bootstrap.carried before)
      ~delta
      ~next:transition.state
      ~limits:document_limits
    |> document_ok
    |> SD.Admitted.state_document
  in
  let restored = SD.value admitted in
  [%test_eq: Sexp.t] (S.sexp_of_t transition.state) (S.sexp_of_t restored);
  equal_component
    (fun t -> Book.to_document t |> protocol_ok |> D.Document.json)
    transition.state.goal_policy_contributions
    restored.goal_policy_contributions;
  equal_component
    (fun t -> PC.to_document t |> protocol_ok |> D.Document.json)
    transition.state.policy_canonical_usage
    restored.policy_canonical_usage;
  assert (
    D.Json.equal
      (Ledger.ledger_json transition.state.inference_ledger)
      (Ledger.ledger_json restored.inference_ledger));
  let replayed =
    replay before delta transition.state ~revision:transition.state.counters.revision
    |> document_ok
  in
  let replayed_state = SD.value replayed in
  [%test_eq: Sexp.t] (S.sexp_of_t restored) (S.sexp_of_t replayed_state);
  assert (D.Json.equal (book_json restored) (book_json replayed_state));
  equal_component
    (fun t -> PC.to_document t |> protocol_ok |> D.Document.json)
    restored.policy_canonical_usage
    replayed_state.policy_canonical_usage;
  assert (
    D.Json.equal
      (Ledger.ledger_json restored.inference_ledger)
      (Ledger.ledger_json replayed_state.inference_ledger));
  let replayed_document = SD.encode replayed ~limits:document_limits |> document_ok in
  assert (
    D.Json.equal
      (member (D.Document.json replayed_document) "future_bootstrap_host")
      Bootstrap.sentinel);
  let roundtrip =
    SD.encode admitted ~limits:document_limits
    |> document_ok
    |> SD.decode ~limits:document_limits
    |> document_ok
  in
  assert (D.Json.equal (book_json restored) (book_json (SD.value roundtrip)));
  restored
;;

let rejected before delta =
  (match A.Session_transition.apply ~now:timestamp before ~delta ~payloads:[] with
   | Error error ->
     if not (P.Error.equal_code Invalid_request error.code)
     then
       raise_s
         [%sexp
           "unexpected native rejection"
         , (error.code : P.Error.code)
         , (error.message : string)]
   | Ok _ -> failwith "invalid ordered Goal policy candidate accepted natively");
  let delta =
    A.Session_delta_document.create
      delta
      ~limits:document_limits
      ~state_document:SD.authored
    |> document_ok
  in
  let transaction_metadata =
    A.Session_delta_document.Transaction_metadata.create
      ~updated_at:timestamp
      ~revision:(Int64.succ before.S.counters.revision)
      ~transaction_sequence:(Int64.succ before.counters.transaction_sequence)
      ~last_event_sequence:None
    |> document_ok
  in
  match
    A.Session_delta_document.apply
      delta
      ~transaction_metadata
      ~limits:document_limits
      (Bootstrap.carried before)
  with
  | Error (D.Error.Invalid_field _) -> ()
  | Error error -> raise_s [%sexp "unexpected document rejection", (error : D.Error.t)]
  | Ok _ ->
    failwith "invalid ordered Goal policy candidate accepted by document transition"
;;

let budget attempts =
  G.Budget.create
    ~admitted_attempts:(Some attempts)
    ~admitted_turns:None
    ~observed_tokens:None
    ~deadline:None
  |> protocol_ok
;;

let change_budget state session attempts key =
  let _, delta =
    Bootstrap.project state ~session ~key (Change_budget (goal_id, budget attempts))
  in
  delta
;;

let reference ~parent ~child =
  Agent_store.Delegation_store.reference_of_jsonaf
    (`Object
        [ ( "key"
          , `Object
              [ "parent_session_id", P.Id.Session.to_json parent
              ; "parent_generation", `Number "0"
              ; "principal_id", P.Id.Principal.to_json principal_id
              ; "idempotency_key", `String "policy-book-route"
              ] )
        ; "child_session_id", P.Id.Session.to_json child
        ; "revision_id", `String "prv_policy_book_route"
        ; "request_sha256", `String (String.make 64 'a')
        ; "admission_sha256", `String (String.make 64 'b')
        ])
  |> store_ok
;;

let parent_event ~callee ~source ~child ~name =
  P.Moderator_execution.create_delegated
    ~delegation:
      { child_session_id = child
      ; child_generation = 0
      ; child_invocation_id = P.Id.Invocation.of_string ("inv_" ^ name) |> protocol_ok
      ; admission_sha256 = String.make 64 'b'
      }
    { id = P.Id.Moderator_execution.of_string ("mex_" ^ name) |> protocol_ok
    ; session_id = P.Session_ref.session_id callee
    ; generation = source.P.Run_source.generation
    ; source = source.observer
    ; operation_id = None
    ; job = None
    ; phase = Pre_tool_call
    ; event = `Null
    ; checkpoint_sha256 = String.make 64 'a'
    ; created_at = timestamp
    }
  |> protocol_ok
;;

let with_goal f =
  let unpublished_initial = ref None in
  let make_persistence _env _sw initial =
    unpublished_initial := Some initial;
    { A.Session_actor.archive_reference
    ; commit = (fun ?observation:_ ~command_audit:_ ~previous:_ _ -> Ok ())
    }
  in
  let with_captured_initial = Run_admission_tests.with_startup_actor ~make_persistence in
  with_captured_initial (fun _actor _ _ request _ _ scope session _ ->
    (* The helper's real Actor attaches normally. This separate pure trajectory
       starts from its actual immutable constructor input, before attachment. *)
    let initial = Option.value_exn !unpublished_initial in
    [%test_eq: int64] 0L initial.counters.revision;
    [%test_eq: int64] 0L initial.counters.transaction_sequence;
    assert (List.is_empty (L.rows initial.inference_ledger));
    assert (Option.is_none initial.goal_catalog);
    (* Explicit new unpublished model basis, not a reconstructed historical
       inventory or a runtime ancestry/permission claim. *)
    let children =
      A.Child_contributions.create
        ~inventory:Complete_inventory
        ~max_children:4
        ~max_encoded_bytes:1048576
      |> protocol_ok
    in
    let initial = { initial with child_contributions = Some children } in
    S.validate initial |> protocol_ok;
    let _, creation = Bootstrap.create initial ~session goal_id "policy-book-create" in
    let created = commit initial creation in
    assert (not (Totals.View.historical_causal_unknown (view created session)));
    let catalog =
      Goal_catalog_tests.enable_fixture (Option.value_exn created.goal_catalog) goal_id
    in
    (* Same existing pure model enable step; it is not a controller permission. *)
    let enabled = commit created (Goal_catalog_changed catalog) in
    let template = request "policy-book-episode" in
    let run_request =
      P.Run_start.create
        ~session_id:enabled.identity.session_id
        ~attachment_id:template.attachment_id
        ~generation:enabled.identity.generation
        ~expected_revision:enabled.counters.revision
        ~mode:template.mode
        ~input:template.input
        ~key:template.key
      |> protocol_ok
    in
    let admission =
      A.Run_admission.prepare
        enabled
        ~scope
        ~request:run_request
        ~session
        ~run_id:(P.Id.Run.of_string "run_policy_book" |> protocol_ok)
        ~operation:None
        ~request_sha256:(Goal_run_tests.digest (P.Run_start.to_json run_request))
        ~now:timestamp
      |> protocol_ok
    in
    let intent =
      A.Goal_episode_admission.intent
        ~goal_id
        ~expected_goal_revision:(G.revision (goal enabled))
        ~expected_catalog_revision:(A.Goal_catalog.revision catalog)
      |> protocol_ok
    in
    let associated =
      A.Goal_episode_admission.prepare enabled ~admission ~intent |> protocol_ok
    in
    let linked =
      commit
        enabled
        (Batch
           [ Run_state_changed (A.Goal_episode_admission.run_state associated)
           ; Goal_catalog_changed (A.Goal_episode_admission.catalog associated)
           ])
    in
    f initial creation ~created linked session)
;;

let seed state session =
  let _, source = A.Goal_catalog_source.capture state |> protocol_ok in
  let child = P.Id.Session.of_string "ses_policy_book_child" |> protocol_ok in
  let caller =
    P.Session_ref.create ~session_id:child ~server_id:(P.Session_ref.server_id session)
  in
  let reference = reference ~parent:state.S.identity.session_id ~child in
  let branch =
    Origin.Anchor.create
      ~parent_session_id:state.identity.session_id
      ~parent_generation:0
      ~principal_id
      ~key:reference.key.idempotency_key
      ~child_session_id:child
      ~transaction_id:(P.Id.Transaction.of_string "txn_policy_book_branch" |> protocol_ok)
    |> protocol_ok
  in
  let episode = G.episode (goal state) |> Option.value_exn in
  let inherited =
    Origin.Scope.create ~owner:session ~goal_id ~source ~creator_episode:episode ~branch
    |> protocol_ok
  in
  let event = parent_event ~callee:session ~source ~child ~name:"policy_book" in
  let cause =
    Cause.create
      ~parent:event
      ~callee:session
      ~callee_source:source
      ~caller
      ~caller_source:(Cause.Caller_source.installed source |> protocol_ok)
      ~caller_origin:(Some Unbound)
      ~path:[ reference ]
      ~targets:[ Cause.Goal_target.inherited inherited ]
      ~max_inherited_scopes:4
    |> protocol_ok
  in
  let after =
    A.Session_delta.apply state (Moderator_execution_changed event) |> protocol_ok
  in
  let update =
    PC.register_event
      state.policy_canonical_usage
      ~before:(S.policy_context state |> protocol_ok)
      ~after:(S.policy_context after |> protocol_ok)
      ~ledger:state.inference_ledger
      ~cause
    |> protocol_ok
  in
  ( commit state (Batch [ Moderator_execution_changed event; Policy_cause_changed update ])
  , cause
  , event )
;;

let occurrence session state after handle =
  A.Goal_descendant_occurrence.Frame.create
    ~session
    ~generation:(L.generation after)
    ~candidate_session_revision:(Int64.succ state.S.counters.revision)
  |> protocol_ok
  |> fun frame ->
  A.Goal_descendant_occurrence.attempt
    frame
    ~ordinal:(L.Handle.ordinal handle)
    ~operation_id:(L.Handle.operation_id handle)
    ~invocation_id:(L.Handle.invocation_id handle)
  |> protocol_ok
;;

let canonical_delta plan =
  let ledger =
    A.Session_delta.inference_ledger_update
      ?policy_update:(Native.policy_update plan)
      (Native.ledger plan)
      ~delegated_usage:(Native.delegated_usage plan)
  in
  match Native.accounts plan with
  | Some accounts when Native.accounts_changed plan ->
    A.Session_delta.Batch [ ledger; Goal_accounts_changed accounts ]
  | None | Some _ -> ledger
;;

let selected state session cause event ~native =
  let after, handle, tracking = Ledger.admit state state.S.inference_ledger in
  let policy_admission =
    PC.Admission.create
      ~cause:(Cause.key cause)
      ~work:(Event event.P.Moderator_execution.context.id)
      ~live_prefix:(S.policy_context state |> protocol_ok)
      ~descendants_after:state.goal_descendants
      ~canonical:(Attempt (handle, tracking))
      ~occurrence:(occurrence session state after handle)
    |> protocol_ok
  in
  let admission =
    if native
    then
      Some
        ( goal_id
        , A.Goal_accounting.Admission.Attempt
            { episode = Option.value_exn (G.episode (goal state)); handle; tracking } )
    else None
  in
  let plan =
    Native.prepare
      ~policy_usage:(Option.value_exn state.policy_canonical_usage)
      ~policy_admission
      state.inference_ledger
      ~after
      ~accounts:state.goal_accounts
      ~catalog:state.goal_catalog
      ~delegated_usage:state.delegated_usage
      ~admission
    |> protocol_ok
  in
  canonical_delta plan, handle
;;

let factual state after =
  Native.prepare
    ~policy_usage:(Option.value_exn state.S.policy_canonical_usage)
    state.inference_ledger
    ~after
    ~accounts:state.goal_accounts
    ~catalog:state.goal_catalog
    ~delegated_usage:state.delegated_usage
    ~admission:None
  |> protocol_ok
  |> canonical_delta
;;

let%test_unit "causal Goal inline counts conserve actual H and exact archive coordinates" =
  with_goal (fun initial creation ~created linked session ->
    (* This is a valid initialization payload for the actual Held, pre-episode
       creation account. Replaying it cannot make an already retained Goal new. *)
    let historical_created = { created with goal_policy_contributions = None } in
    let historical_linked = { linked with goal_policy_contributions = None } in
    let initialize =
      Book.initialize_goal
        None
        ~owner:session
        ~goal:(goal created)
        ~account:
          (A.Goal_accounts.find (Option.value_exn created.goal_accounts) goal_id
           |> Option.value_exn)
        ~before_ledger:created.inference_ledger
      |> protocol_ok
    in
    rejected historical_created (Goal_policy_contributions_changed initialize);
    rejected historical_linked (Created linked);
    let created_again = commit initial creation in
    assert (not (Totals.View.historical_causal_unknown (view created_again session)));
    let limited = commit linked (change_budget linked session 2L "policy-book-limit") in
    let target = History_edit_tests.occurrence 0 "archive this exact prior occurrence" in
    let limited =
      { limited with
        conversation =
          { limited.conversation with
            canonical_history = [ target ]
          ; initial_prompt_entry_count = 0
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      }
    in
    S.validate limited |> protocol_ok;
    let seeded, cause, event = seed limited session in
    let first_delta, h1 = selected seeded session cause event ~native:true in
    let first = commit seeded first_delta in
    let unstamped =
      A.Session_delta_document.create
        first_delta
        ~limits:document_limits
        ~state_document:SD.authored
      |> document_ok
    in
    assert (
      Result.is_error
        (A.Session_delta_document.apply
           unstamped
           ~limits:document_limits
           (Bootstrap.carried seeded)));
    (* A HIGHER stamp passes the projection head's <= bound; rejection must come
       from the retained exact publication cursor, not a relaxed final decoder. *)
    (match
       replay seeded first_delta first ~revision:(Int64.succ first.counters.revision)
     with
     | Error (D.Error.Invalid_field { path = []; reason }) ->
       [%test_eq: string]
         "policy canonical fact differs from actual transaction publication"
         reason
     | Error error ->
       raise_s [%sexp "unexpected publication failure", (error : D.Error.t)]
     | Ok _ -> failwith "canonical fact accepted a different higher publication stamp");
    let captured_state state =
      let document = encoded state in
      set (D.Document.json document) "future_prefix_created" Bootstrap.sentinel
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    (* The Created image is independently full State. Its raw extension unions
       into the receiver while the later genuine canonical head stays unstamped. *)
    let created_replay =
      replay
        ~state_document:captured_state
        seeded
        (Batch [ Created seeded; first_delta ])
        first
        ~revision:first.counters.revision
      |> document_ok
    in
    [%test_eq: Sexp.t] (S.sexp_of_t first) (S.sexp_of_t (SD.value created_replay));
    assert (D.Json.equal (book_json first) (book_json (SD.value created_replay)));
    assert (D.Json.equal (policy_json first) (policy_json (SD.value created_replay)));
    let merged = SD.encode created_replay ~limits:document_limits |> document_ok in
    assert (
      D.Json.equal
        (member (D.Document.json merged) "future_prefix_created")
        Bootstrap.sentinel);
    let local = view first session in
    [%test_eq: int64] 1L (Totals.View.committed local ~dimension:Attempt);
    [%test_eq: int64] 0L (A.Goal_usage.attempts (Totals.View.causal_usage local));
    let archive =
      A.Compaction_archive.reference_for
        (Bootstrap.carried first)
        ~limits:document_limits
        ~kind:Delete
        operation_id
      |> protocol_ok
    in
    let retirement = A.Session_delta.History_deleted (target.id, archive) in
    let retired_prefix = A.Session_delta.apply first retirement |> protocol_ok in
    let second_delta, h2 = selected retired_prefix session cause event ~native:false in
    let uncommitted = A.Session_delta.apply retired_prefix second_delta |> protocol_ok in
    (* Full archives are committed State images; the new selected head names the
       upcoming publication revision. No encode/decode counter relaxation exists. *)
    assert (
      Result.is_error
        (A.Compaction_archive.reference_for
           (SD.authored uncommitted)
           ~limits:document_limits
           ~kind:Delete
           operation_id));
    let second = commit first (Batch [ retirement; second_delta ]) in
    assert (List.is_empty second.conversation.canonical_history);
    [%test_eq: int]
      (List.length first.conversation.compaction_archives + 1)
      (List.length second.conversation.compaction_archives);
    let totals = view second session in
    [%test_eq: int64] 2L (Totals.View.committed totals ~dimension:Attempt);
    [%test_eq: int64] 1L (A.Goal_usage.attempts (Totals.View.local_usage totals));
    [%test_eq: int64] 1L (A.Goal_usage.attempts (Totals.View.causal_usage totals));
    [%test_eq: int64] 2L (A.Goal_usage.attempts (Totals.View.total_usage totals));
    [%test_eq: int] 2 (List.length (L.rows second.inference_ledger));
    let book_before = book_json second in
    let observed, _ =
      L.observe second.inference_ledger h2 (Inference_tracking_tests.usage h2 0L 7L)
      |> Ledger.ledger_ok
    in
    let observed = commit second (factual second observed) in
    let corrected, _ =
      L.observe observed.inference_ledger h2 (Inference_tracking_tests.usage h2 1L 3L)
      |> Ledger.ledger_ok
    in
    let corrected = commit observed (factual observed corrected) in
    let metric =
      A.Goal_usage.component (Totals.View.causal_usage (view corrected session)) Input
    in
    assert (
      P.Inference_query.Metric.equal_sum (Tokens 3L) (A.Goal_usage.Metric.actual metric));
    assert (D.Json.equal book_before (book_json corrected));
    assert (
      L.Handle.equal
        h1
        (L.Row.handle
           (L.find corrected.inference_ledger ~ordinal:(L.Handle.ordinal h1)
            |> Option.value_exn)));
    let third, _ = selected corrected session cause event ~native:false in
    rejected corrected third;
    let raised = change_budget corrected session 3L "too-late-budget" in
    rejected corrected (Batch [ third; raised ]);
    let higher = commit corrected (change_budget corrected session 5L "later-capacity") in
    let historical = { higher with goal_policy_contributions = None } in
    S.validate historical |> protocol_ok;
    let lower_bound = view historical session in
    assert (Totals.View.historical_causal_unknown lower_bound);
    assert (
      A.Goal_usage.equal
        (Totals.View.causal_usage (view higher session))
        (Totals.View.causal_usage lower_bound));
    [%test_eq: int64] 2L (Totals.View.committed lower_bound ~dimension:Attempt);
    assert (
      A.Goal_admission_policy.Count_availability.equal
        Available
        (Totals.available
           lower_bound
           ~context:Ordinary_existing
           ~goal:(goal historical)
           ~dimension:Attempt
         |> protocol_ok));
    assert (
      A.Goal_admission_policy.Count_availability.equal
        Unknown
        (Totals.available
           lower_bound
           ~context:Delegated_policy_causal
           ~goal:(goal historical)
           ~dimension:Attempt
         |> protocol_ok)))
;;

let%test_unit "native canonical retirement folds a bound Goal beside its causal book" =
  with_goal (fun _ _ ~created:_ linked session ->
    let module Accounting = A.Goal_accounting in
    let prepare (state : S.t) ~after ~admission =
      Native.prepare
        state.inference_ledger
        ~after
        ~accounts:state.goal_accounts
        ~catalog:state.goal_catalog
        ~delegated_usage:state.delegated_usage
        ~admission
      |> protocol_ok
    in
    let after, handle, tracking = Ledger.admit linked linked.inference_ledger in
    let episode = G.episode (goal linked) |> Option.value_exn in
    let included =
      prepare
        linked
        ~after
        ~admission:(Some (goal_id, Attempt { episode; handle; tracking }))
      |> canonical_delta
      |> commit linked
    in
    let corrected, _ =
      L.observe
        included.inference_ledger
        handle
        (Inference_tracking_tests.usage handle 0L 11L)
      |> Ledger.ledger_ok
    in
    let ended =
      L.set_state
        corrected
        handle
        (Interrupted { reason = Host_interrupted; delivery = Possibly_submitted })
      |> Ledger.ledger_ok
    in
    let retained =
      prepare included ~after:ended ~admission:None |> canonical_delta |> commit included
    in
    (* The same canonical admission under its one-row test profile reaches the
       retirement edge with only one unrelated allocation. State and replay
       use the normal durable profile. *)
    let limits =
      L.Limits.create
        ~max_attempts:1
        ~max_turns:1
        ~max_retained_bytes:4_194_304
        ~document_limits
      |> Ledger.ledger_ok
    in
    let pressure =
      L.to_document retained.inference_ledger
      |> Ledger.ledger_ok
      |> L.of_document ~limits
      |> Ledger.ledger_ok
    in
    let after, unrelated, tracking = Ledger.admit retained pressure in
    assert (L.equal_tracking tracking Tracked);
    assert (Option.is_none (L.find after ~ordinal:(L.Handle.ordinal handle)));
    assert (Option.is_some (L.find after ~ordinal:(L.Handle.ordinal unrelated)));
    rejected retained (Inference_ledger_changed after);
    let retired =
      prepare retained ~after ~admission:None |> canonical_delta |> commit retained
    in
    let account =
      A.Goal_accounts.find (Option.value_exn retired.goal_accounts) goal_id
      |> Option.value_exn
      |> A.Goal_accounting_document.resolve
           ~goal:(goal retired)
           ~ledger:retired.inference_ledger
      |> protocol_ok
    in
    [%test_eq: int64] 1L (Accounting.admitted_attempts account);
    [%test_eq: int64] 1L (Accounting.settled_tracked_attempts account);
    assert (List.is_empty (Accounting.bindings account));
    let local state = Totals.View.local_usage (view state session) in
    assert (A.Goal_usage.equal (local retained) (local retired));
    let input = A.Goal_usage.component (local retired) Input in
    assert (
      P.Inference_query.Metric.equal_sum (Tokens 11L) (A.Goal_usage.Metric.actual input));
    assert (D.Json.equal (book_json retained) (book_json retired));
    let after_late, disposition =
      L.observe
        retired.inference_ledger
        handle
        (Inference_tracking_tests.usage handle 1L 99L)
      |> Ledger.ledger_ok
    in
    (match disposition with
     | Ignored_retired -> ()
     | _ -> failwith "late observation did not remain retired");
    assert (
      D.Json.equal
        (Ledger.ledger_json retired.inference_ledger)
        (Ledger.ledger_json after_late)))
;;

let remote_request state session =
  let source = G.source (goal state) in
  let callee_id = P.Id.Session.of_string "ses_policy_book_remote" |> protocol_ok in
  let callee =
    P.Session_ref.create
      ~session_id:callee_id
      ~server_id:(P.Session_ref.server_id session)
  in
  let reference = reference ~parent:callee_id ~child:state.S.identity.session_id in
  let event =
    parent_event
      ~callee
      ~source
      ~child:state.identity.session_id
      ~name:"policy_book_remote"
  in
  let target =
    Cause.Goal_target.local ~goal_id ~episode:(G.episode (goal state) |> Option.value_exn)
    |> protocol_ok
  in
  let cause =
    Cause.create
      ~parent:event
      ~callee
      ~callee_source:source
      ~caller:session
      ~caller_source:(Cause.Caller_source.installed source |> protocol_ok)
      ~caller_origin:
        (Some
           (Run
              (P.Goal_episode.admission (G.episode (goal state) |> Option.value_exn))
                .run_id))
      ~path:[ reference ]
      ~targets:[ target ]
      ~max_inherited_scopes:4
    |> protocol_ok
  in
  (* A prospective remote occurrence is DATA, not a reservation or writer seal.
     The canonical H is nevertheless issued by the real remote ledger. *)
  let ledger =
    L.create
      ~session_id:callee_id
      ~generation:0
      ~before_tracking_unknown:false
      ~limits:L.Limits.default
    |> Ledger.ledger_ok
  in
  let after, handle, _ = Ledger.admit state ledger in
  let occurrence =
    A.Goal_descendant_occurrence.Frame.create
      ~session:callee
      ~generation:0
      ~candidate_session_revision:1L
    |> protocol_ok
    |> fun frame ->
    A.Goal_descendant_occurrence.attempt
      frame
      ~ordinal:(L.Handle.ordinal handle)
      ~operation_id:(L.Handle.operation_id handle)
      ~invocation_id:(L.Handle.invocation_id handle)
    |> protocol_ok
  in
  assert (Option.is_some (L.find after ~ordinal:(L.Handle.ordinal handle)));
  Book.Request.create cause ~target ~work:(Event event.context.id) ~occurrence
  |> protocol_ok
;;

let%test_unit "new causal quota seals only its actual unassociated native edge" =
  with_goal (fun _ _ ~created:_ linked session ->
    let zero = commit linked (change_budget linked session 0L "policy-book-zero") in
    let raise_budget = change_budget zero session 2L "policy-book-seal-limit" in
    let before = commit zero raise_budget in
    let after, handle, tracking = Ledger.admit before before.inference_ledger in
    let charged =
      Native.prepare
        before.inference_ledger
        ~after
        ~accounts:before.goal_accounts
        ~catalog:before.goal_catalog
        ~delegated_usage:before.delegated_usage
        ~admission:
          (Some
             ( goal_id
             , A.Goal_accounting.Admission.Attempt
                 { episode = G.episode (goal before) |> Option.value_exn
                 ; handle
                 ; tracking
                 } ))
      |> protocol_ok
    in
    let uncharged = A.Session_delta.Inference_ledger_changed after in
    let accounts = Native.accounts charged |> Option.value_exn in
    let early_budget =
      commit zero (Batch [ raise_budget; uncharged; Goal_accounts_changed accounts ])
    in
    [%test_eq: int64]
      1L
      (Totals.View.committed (view early_budget session) ~dimension:Attempt);
    (* Exactly the same leaves and candidate, changing only their order in ONE
       transaction. A budget edit after H cannot excuse that earlier admission. *)
    rejected zero (Batch [ uncharged; raise_budget; Goal_accounts_changed accounts ]);
    let request = remote_request before session in
    let reserve, pending =
      match
        Book.reserve before.goal_policy_contributions ~owner:session ~request
        |> protocol_ok
      with
      | Reserved (update, pending) ->
        A.Session_delta.Goal_policy_contributions_changed update, pending
      | Already_pending _ | Busy _ -> failwith "fresh remote quota was not reserved"
    in
    (* Both native count and remote quota fit the original total ceiling. The
       intervening genuine fresh decision still forbids retroactive association. *)
    rejected before (Batch [ uncharged; reserve; Goal_accounts_changed accounts ]);
    let reserved = commit before (Batch [ uncharged; reserve ]) in
    [%test_eq: int64]
      0L
      (Totals.View.committed (view reserved session) ~dimension:Attempt);
    [%test_eq: int64] 1L (Totals.View.pending (view reserved session) ~dimension:Attempt);
    [%test_eq: int] 1 (List.length (L.rows reserved.inference_ledger));
    rejected reserved (Goal_accounts_changed accounts);
    let close = Book.close_before_writer (book reserved) ~pending |> protocol_ok in
    let closed = commit reserved (Goal_policy_contributions_changed close) in
    [%test_eq: int64] 0L (Totals.View.pending (view closed session) ~dimension:Attempt);
    [%test_eq: int64] 0L (Totals.View.committed (view closed session) ~dimension:Attempt);
    assert (
      D.Json.equal
        (Ledger.ledger_json reserved.inference_ledger)
        (Ledger.ledger_json closed.inference_ledger));
    (* A separate actual write can attribute its own new H. No source-wide
       coverage or permanent prohibition is inferred from the previous edge. *)
    let after, handle, tracking = Ledger.admit closed closed.inference_ledger in
    let plan =
      Native.prepare
        closed.inference_ledger
        ~after
        ~accounts:closed.goal_accounts
        ~catalog:closed.goal_catalog
        ~delegated_usage:closed.delegated_usage
        ~admission:
          (Some
             ( goal_id
             , A.Goal_accounting.Admission.Attempt
                 { episode = G.episode (goal closed) |> Option.value_exn
                 ; handle
                 ; tracking
                 } ))
      |> protocol_ok
    in
    let admitted = commit closed (canonical_delta plan) in
    [%test_eq: int64]
      1L
      (Totals.View.committed (view admitted session) ~dimension:Attempt))
;;

let inspect raw = D.Document.inspect raw ~limits:document_limits |> document_ok

let delta_document delta =
  A.Session_delta_document.create
    delta
    ~limits:document_limits
    ~state_document:SD.authored
  |> document_ok
  |> A.Session_delta_document.document
;;

let strip_marker document marker =
  let semantics = D.Document.required_semantics document in
  assert (List.mem semantics marker ~equal:String.equal);
  set
    (D.Document.json document)
    "required_semantics"
    (`Array
        (List.filter_map semantics ~f:(fun name ->
           if String.equal name marker then None else Some (`String name))))
  |> inspect
;;

let%test_unit
    "stable causal book exact versions markers raw children and historical absence"
  =
  with_goal (fun _ _ ~created linked session ->
    let raw = encoded linked in
    [%test_eq: int] 29 linked.schema_version;
    [%test_eq: int] 14 (D.Document.version raw);
    let marker = P.Session_storage_semantics.goal_policy_contributions in
    assert (Result.is_error (SD.decode (strip_marker raw marker) ~limits:document_limits));
    let old = set (D.Document.json raw) "schema_version" (`Number "13") |> inspect in
    [%test_eq: int] 13 (D.Document.version old);
    assert (Result.is_error (SD.decode old ~limits:document_limits));
    assert (Result.is_error (S.upgrade_schema { linked with schema_version = 28 }));
    (* Existing positive595/descendant data at its own introduced native version
       remain valid with the NEW component absent; no older guards move. *)
    let before_policy, cause, event = seed linked session in
    (* A real lifecycle leaf changes only its exact private selection while the
       existing positive policy carrier and retained Run ancestry survive. *)
    let finished_seed =
      P.Moderator_execution.complete
        event
        ~decision:Approve
        ~checkpoint_sha256:event.context.checkpoint_sha256
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let lifecycle_before =
      commit before_policy (Moderator_execution_changed finished_seed)
    in
    let receipt =
      P.Moderator_execution.create
        { event.context with
          id = P.Id.Moderator_execution.of_string "mex_policy_lifecycle" |> protocol_ok
        ; phase = Session_resume
        ; event = `Null
        }
      |> protocol_ok
    in
    let before_context = S.policy_context lifecycle_before |> protocol_ok in
    let capture =
      A.Moderator_lifecycle.create
        ~source:(Option.value_exn before_context.current_source)
        ~execution_id:receipt.context.id
      |> protocol_ok
    in
    let claimed =
      commit lifecycle_before (Moderator_lifecycle_claimed { receipt; capture })
    in
    let completed =
      P.Moderator_execution.complete
        receipt
        ~checkpoint_sha256:receipt.context.checkpoint_sha256
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let ended = commit claimed (Moderator_execution_changed completed) in
    assert (D.Json.equal (policy_json before_policy) (policy_json ended));
    let uncaptured =
      P.Moderator_execution.create
        { receipt.context with
          id = P.Id.Moderator_execution.of_string "mex_policy_uncaptured" |> protocol_ok
        }
      |> protocol_ok
    in
    let cleared = commit ended (Moderator_execution_changed uncaptured) in
    assert (
      Option.is_none
        (A.Run_state.moderator_lifecycle (Option.value_exn cleared.run_state)));
    let context = S.policy_context claimed |> protocol_ok in
    let unrelated =
      Cause.Context.create
        ~session_id:context.session_id
        ~generation:context.generation
        ~current_source:context.current_source
        ~events:context.events
        ~invocations:context.invocations
        ~jobs:context.jobs
        ~runs:(Some A.Run_state.empty)
        ~accounts:context.accounts
        ~catalog:context.catalog
        ~descendants:context.descendants
      |> protocol_ok
    in
    let cursor =
      PC.Ordered.begin_
        ~previous:lifecycle_before.policy_canonical_usage
        ~context:before_context
        ~ledger:lifecycle_before.inference_ledger
      |> protocol_ok
    in
    (match
       PC.Ordered.advance
         cursor
         (Lifecycle_work { context = unrelated; receipt; capture = Some capture })
     with
     | Error failure -> [%test_eq: P.Error.code] Invalid_request failure.P.Error.code
     | Ok _ -> failwith "lifecycle step replaced unrelated Run ancestry");
    let selected_delta, _ = selected before_policy session cause event ~native:false in
    let policy = commit before_policy selected_delta in
    let unmarked_policy = strip_marker (encoded policy) marker in
    assert (
      List.mem
        (D.Document.required_semantics unmarked_policy)
        P.Session_storage_semantics.policy_canonical_usage
        ~equal:String.equal);
    assert (Result.is_error (SD.decode unmarked_policy ~limits:document_limits));
    let old_native =
      { policy with schema_version = 28; goal_policy_contributions = None }
    in
    let upgraded = S.upgrade_schema old_native |> protocol_ok in
    [%test_eq: int] 29 upgraded.schema_version;
    assert (Option.is_none upgraded.goal_policy_contributions);
    assert (D.Json.equal (policy_json policy) (policy_json upgraded));
    assert (
      D.Json.equal
        (Bootstrap.descendants_json policy)
        (Bootstrap.descendants_json upgraded));
    List.iter [ false; true ] ~f:(fun explicit_null ->
      let payload = D.Document.payload raw in
      let payload =
        if explicit_null
        then set payload "goal_policy_contributions" `Null
        else remove payload "goal_policy_contributions"
      in
      let historical = set (D.Document.json raw) "payload" payload in
      List.iter [ 13; 14 ] ~f:(fun version ->
        let historical =
          set historical "schema_version" (`Number (Int.to_string version)) |> inspect
        in
        [%test_eq: int] version (D.Document.version historical);
        let decoded = SD.decode historical ~limits:document_limits |> document_ok in
        assert (Option.is_none (SD.value decoded).goal_policy_contributions);
        assert (Totals.View.historical_causal_unknown (view (SD.value decoded) session));
        let delta = A.Session_delta.Halt_changed (Some "historical unrelated write") in
        let next =
          A.Session_transition.apply ~now:timestamp (SD.value decoded) ~delta ~payloads:[]
          |> protocol_ok
        in
        let changed =
          A.Session_document_transition.admit_encoded
            decoded
            ~delta
            ~next:next.state
            ~limits:document_limits
          |> document_ok
          |> SD.Admitted.document
        in
        match
          D.Json.field (D.Document.payload changed) ~name:"goal_policy_contributions"
        with
        | Null -> assert explicit_null
        | Absent -> assert (not explicit_null)
        | Value _ -> failwith "historical unrelated write invented known coverage"));
    let account =
      A.Goal_accounts.find (Option.value_exn created.goal_accounts) goal_id
      |> Option.value_exn
    in
    let initialization =
      Book.initialize_goal
        None
        ~owner:session
        ~goal:(goal created)
        ~account
        ~before_ledger:created.inference_ledger
      |> protocol_ok
    in
    let update_document =
      delta_document (Goal_policy_contributions_changed initialization)
    in
    let created_document = delta_document (Created policy) in
    List.iter [ update_document; created_document ] ~f:(fun document ->
      [%test_eq: int] 10 (D.Document.version document);
      let old =
        set (D.Document.json document) "schema_version" (`Number "9") |> inspect
      in
      [%test_eq: int] 9 (D.Document.version old);
      assert (
        Result.is_error (A.Session_delta_document.decode old ~limits:document_limits));
      assert (
        Result.is_error
          (A.Session_delta_document.decode
             (strip_marker document marker)
             ~limits:document_limits)));
    let reader =
      D.Domain_codec.create
        ~limits:document_limits
        ~kind:(D.Document.kind raw)
        ~version:(D.Document.version raw)
        ~shape:SD.shape
        ~supported_semantics:
          (List.filter P.Session_storage_semantics.supported_children ~f:(fun name ->
             not (String.equal name marker)))
        ~decode:(fun _ -> Ok ())
        ~encode:(fun () -> Ok (`Object []))
      |> document_ok
    in
    (match D.Domain_codec.decode reader raw with
     | Error (D.Error.Required_semantics_unknown actual) ->
       [%test_eq: string] marker actual
     | Error error -> raise_s [%sexp (error : D.Error.t)]
     | Ok _ -> failwith "reader without book semantics accepted its positive carrier");
    let sentinel = `Object [ "nested", `String "retained-complete-child" ] in
    let full = D.Document.json raw in
    let child = member (D.Document.payload raw) "goal_policy_contributions" in
    let child = set child "future_book_child" sentinel in
    let full =
      set full "payload" (set (D.Document.payload raw) "goal_policy_contributions" child)
    in
    let carried = SD.decode (inspect full) ~limits:document_limits |> document_ok in
    let state = SD.value carried in
    let delta = A.Session_delta.Halt_changed (Some "preserve complete child") in
    let next =
      A.Session_transition.apply ~now:timestamp state ~delta ~payloads:[] |> protocol_ok
    in
    let preserved =
      A.Session_document_transition.admit_encoded
        carried
        ~delta
        ~next:next.state
        ~limits:document_limits
      |> document_ok
      |> SD.Admitted.document
    in
    assert (
      D.Json.equal
        sentinel
        (member
           (member (D.Document.payload preserved) "goal_policy_contributions")
           "future_book_child"));
    (* Outer document accepts this complete raw child, whose own4MiB profile
       correctly rejects it. Unknown data is not discarded before admission. *)
    let outer_limits =
      D.Limits.create
        ~max_bytes:8388608
        ~max_depth:128
        ~max_fields:600000
        ~max_nodes:1200000
      |> document_ok
    in
    let oversized_child =
      set
        child
        "oversized_unknown"
        (`String (String.make (D.Limits.max_bytes Book.limits) 'x'))
    in
    let oversized =
      set
        full
        "payload"
        (set (member full "payload") "goal_policy_contributions" oversized_child)
      |> D.Document.inspect ~limits:outer_limits
      |> document_ok
    in
    assert (Result.is_error (SD.decode oversized ~limits:outer_limits));
    let child_document =
      D.Document.inspect oversized_child ~limits:outer_limits |> document_ok
    in
    assert (Result.is_error (Book.of_document child_document)))
;;

(* The cold reference deliberately uses the complete public decoder, followed
   by the same publication check, rather than decode_after's transition checks. *)
let prefix_reference prefix ~limits document =
  let open Result.Let_syntax in
  let%bind state_document = SD.decode ~limits document in
  let%map () =
    SD.Prefix.validate_publication
      prefix
      ~revision:(SD.value state_document).counters.revision
  in
  state_document
;;

let assert_prefix_reference prefix ~limits document =
  let cold = prefix_reference prefix ~limits document in
  let reused = SD.Prefix.materialize prefix ~limits document in
  (* Check admission itself first: a later encode failure must not conceal
     a mismatched successful decode. *)
  assert (
    Result.equal
      (fun cold reused ->
         let encode value =
           SD.encode value ~limits |> Result.map ~f:D.Document.to_string
         in
         assert (Result.equal String.equal D.Error.equal (encode cold) (encode reused));
         true)
      D.Error.equal
      cold
      reused);
  Result.bind cold ~f:(fun value ->
    SD.encode value ~limits |> Result.map ~f:D.Document.to_string)
;;

let%test_unit "prefix materialization matches cold causal decoding and error order" =
  with_goal (fun _ _ ~created:_ linked session ->
    let before, cause, event = seed linked session in
    let delta, _ = selected before session cause event ~native:true in
    let next =
      A.Session_delta.apply ~limits:document_limits before delta |> protocol_ok
    in
    let prefix =
      A.Session_document_transition.admit_prefix_encoded
        (Bootstrap.carried before)
        ~delta
        ~next
        ~limits:document_limits
      |> document_ok
    in
    let document =
      D.Document.replace_payload_scalars
        (SD.Prefix.document prefix)
        ~limits:document_limits
        ~updates:
          [ ( [ "counters"; "revision" ]
            , A.Persistence_codec.int64_json (Int64.succ before.counters.revision) )
          ]
      |> document_ok
    in
    let check document =
      assert_prefix_reference prefix ~limits:document_limits document
    in
    ignore (check document |> document_ok : string);
    let edit f =
      D.Document.json document
      |> f
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    let changed =
      edit (fun json -> set json "future_prefix" (`Object [ "n", `Number "1.00" ]))
    in
    ignore (check changed |> document_ok : string);
    let reordered =
      edit (function
        | `Object fields -> `Object (List.rev fields)
        | _ -> assert false)
    in
    ignore (check reordered |> document_ok : string);
    let malformed =
      edit (fun json ->
        let payload = D.Document.payload document in
        set
          json
          "payload"
          (set (set payload "identity" `Null) "policy_canonical_usage" `True))
    in
    let first_error = SD.decode ~limits:document_limits malformed in
    assert (Result.is_error first_error);
    assert (Result.is_error (check malformed));
    let bad_policy =
      edit (fun json ->
        set
          json
          "payload"
          (set (D.Document.payload document) "policy_canonical_usage" `True))
    in
    assert (Result.is_error (check bad_policy));
    let foreign_owner =
      edit (fun json ->
        let payload = D.Document.payload document in
        let identity =
          match payload with
          | `Object fields -> List.Assoc.find_exn fields ~equal:String.equal "identity"
          | _ -> assert false
        in
        set
          json
          "payload"
          (set
             payload
             "identity"
             (set identity "session_id" (`String "ses_foreign_prefix"))))
    in
    (* Exact causal field hits never replace fresh whole-state owner validation. *)
    assert (Result.is_error (check foreign_owner));
    let changed_policy =
      edit (fun json ->
        let payload = D.Document.payload document in
        let policy = policy_json next in
        set
          json
          "payload"
          (set
             payload
             "policy_canonical_usage"
             (set policy "future_policy" (`Number "1.00"))))
    in
    ignore (check changed_policy |> document_ok : string);
    let absent_components =
      edit (fun json ->
        set
          json
          "payload"
          (remove
             (remove (D.Document.payload document) "policy_canonical_usage")
             "goal_policy_contributions"))
    in
    ignore (check absent_components : (string, D.Error.t) Result.t);
    let null_components =
      edit (fun json ->
        set
          json
          "payload"
          (set
             (set (D.Document.payload document) "policy_canonical_usage" `Null)
             "goal_policy_contributions"
             `Null))
    in
    ignore (check null_components : (string, D.Error.t) Result.t);
    let changed_limits =
      D.Limits.create
        ~max_bytes:(D.Limits.max_bytes document_limits)
        ~max_depth:(D.Limits.max_depth document_limits)
        ~max_fields:(D.Limits.max_fields document_limits + 1)
        ~max_nodes:(D.Limits.max_nodes document_limits)
      |> document_ok
    in
    ignore
      (assert_prefix_reference prefix ~limits:changed_limits document |> document_ok
       : string);
    let tight =
      D.Limits.create ~max_bytes:128 ~max_depth:8 ~max_fields:20 ~max_nodes:100
      |> document_ok
    in
    assert (Result.is_error (assert_prefix_reference prefix ~limits:tight document));
    (* Compatible Created extensions force another actual final prefix encode. *)
    let captured = SD.decode ~limits:document_limits changed |> document_ok in
    let merged =
      SD.Prefix.merge_created prefix ~captured ~limits:document_limits |> document_ok
    in
    let merged_document =
      D.Document.replace_payload_scalars
        (SD.Prefix.document merged)
        ~limits:document_limits
        ~updates:
          [ ( [ "counters"; "revision" ]
            , A.Persistence_codec.int64_json (Int64.succ before.counters.revision) )
          ]
      |> document_ok
    in
    ignore
      (assert_prefix_reference merged ~limits:document_limits merged_document
       |> document_ok
       : string);
    assert (
      Option.equal
        D.Json.equal
        (List.Assoc.find
           (match D.Document.json merged_document with
            | `Object fields -> fields
            | _ -> assert false)
           ~equal:String.equal
           "future_prefix")
        (Some (`Object [ "n", `Number "1.00" ]))))
;;

let%test_unit "causal field evidence owns only exact successful decoder generations" =
  with_goal (fun _ _ ~created:_ linked session ->
    let state, _, _ = seed linked session in
    let module E = A.Session_state_decode_evidence in
    let policy_json = policy_json state in
    let book_json = book_json state in
    let first = E.Capture.create ~limits:document_limits () in
    let policy = E.Capture.policy_canonical_usage first policy_json |> protocol_ok in
    let book = E.Capture.goal_policy_contributions first book_json |> protocol_ok in
    (* Identity here deliberately witnesses sharing of actual immutable decoder
       results, independently of their semantic/serialized equality. *)
    assert (
      phys_equal policy (E.Capture.policy_canonical_usage first policy_json |> protocol_ok));
    assert (
      phys_equal book (E.Capture.goal_policy_contributions first book_json |> protocol_ok));
    let evidence = E.Capture.finish first in
    let same = E.Capture.create ~previous:evidence ~limits:document_limits () in
    assert (
      phys_equal policy (E.Capture.policy_canonical_usage same policy_json |> protocol_ok));
    assert (
      phys_equal book (E.Capture.goal_policy_contributions same book_json |> protocol_ok));
    let absent =
      E.Capture.create ~previous:evidence ~limits:document_limits () |> E.Capture.finish
    in
    let reintroduced = E.Capture.create ~previous:absent ~limits:document_limits () in
    assert (
      not
        (phys_equal
           policy
           (E.Capture.policy_canonical_usage reintroduced policy_json |> protocol_ok)));
    assert (
      not
        (phys_equal
           book
           (E.Capture.goal_policy_contributions reintroduced book_json |> protocol_ok)));
    let changed_limits =
      D.Limits.create
        ~max_bytes:(D.Limits.max_bytes document_limits)
        ~max_depth:(D.Limits.max_depth document_limits)
        ~max_fields:(D.Limits.max_fields document_limits + 1)
        ~max_nodes:(D.Limits.max_nodes document_limits)
      |> document_ok
    in
    let changed = E.Capture.create ~previous:evidence ~limits:changed_limits () in
    assert (
      not
        (phys_equal
           policy
           (E.Capture.policy_canonical_usage changed policy_json |> protocol_ok)));
    assert (
      not
        (phys_equal
           book
           (E.Capture.goal_policy_contributions changed book_json |> protocol_ok)));
    let changed_raw = set policy_json "future_evidence" (`Number "1.00") in
    let changed_policy =
      E.Capture.policy_canonical_usage same changed_raw |> protocol_ok
    in
    assert (not (phys_equal policy changed_policy));
    [%test_eq: string]
      (PC.to_document changed_policy |> protocol_ok |> D.Document.to_string)
      (E.policy_canonical_usage_of_jsonaf changed_raw
       |> protocol_ok
       |> PC.to_document
       |> protocol_ok
       |> D.Document.to_string);
    let malformed = E.Capture.policy_canonical_usage same `True in
    assert (Result.is_error malformed);
    assert (
      Result.equal
        (fun _ _ -> false)
        (fun left right -> Sexp.equal (P.Error.sexp_of_t left) (P.Error.sexp_of_t right))
        malformed
        (E.policy_canonical_usage_of_jsonaf `True)))
;;
