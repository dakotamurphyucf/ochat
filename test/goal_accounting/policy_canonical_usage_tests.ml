open! Core
open Fixture
module PC = Agent_session.Policy_canonical_usage
module Cause = Agent_session.Policy_cause_data
module Context = Cause.Context
module T = Descendant_component_tests
module Accounts = Agent_session.Goal_accounts
module Native = Agent_session.Canonical_usage_update
module GD = Agent_session.Goal_descendants
module Occurrence = Agent_session.Goal_descendant_occurrence
module Origin = Agent_store.Delegation_goal_origin

(* Pure persisted DATA trajectories. Every H comes from the real ledger and
   native Goal candidate; no decoded descriptor is an execution/ACK proof. *)
let member = T.member
let set = T.set
let domain = T.domain
let raw t = PC.to_document t |> protocol |> D.Document.json

let restore raw =
  D.Document.inspect raw ~limits:PC.limits |> domain |> PC.of_document |> protocol
;;

let context
      ?(events = [])
      ?(current_source = Some (source ()))
      ~accounts
      ~catalog
      ~descendants
      ()
  =
  Context.create
    ~session_id:sid
    ~generation:0
    ~current_source
    ~events
    ~invocations:[]
    ~jobs:[]
    ~runs:None
    ~accounts
    ~catalog
    ~descendants
  |> protocol
;;

let project_context ?generation (t : Context.t) ~events ~current_source ~accounts =
  Context.create
    ~session_id:t.session_id
    ~generation:(Option.value generation ~default:t.generation)
    ~current_source
    ~events
    ~invocations:t.invocations
    ~jobs:t.jobs
    ~runs:t.runs
    ~accounts
    ~catalog:t.catalog
    ~descendants:t.descendants
  |> protocol
;;

let local_episode caller caller_source =
  let run =
    P.Run.create
      ~id:(P.Id.Run.of_string "run_policy_caller" |> protocol)
      ~session:caller
      ~principal_id:principal
      ~source:caller_source
      ~mode:Workflow
      ~lifecycle:Admitted
      ~revision:0L
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:timestamp
    |> protocol
  in
  let admission =
    P.Run_receipt.create
      ~run_id:run.id
      ~principal_id:principal
      ~source:caller_source
      ~key:(P.Idempotency_key.of_string "policy-caller" |> protocol)
      ~request_sha256:(String.make 64 'c')
      ~kind:Admission
      ~run_revision:0L
      ~session_revision:0L
      ~committed_at:timestamp
    |> protocol
  in
  P.Goal_episode.create ~run ~admission |> protocol
;;

let seed ?(name = "policy_seed") reference =
  let delegation : P.Moderator_execution.delegation =
    { child_session_id = reference.Agent_store.Delegation_store.Reference.child_session_id
    ; child_generation = 0
    ; child_invocation_id = P.Id.Invocation.of_string ("inv_" ^ name) |> protocol
    ; admission_sha256 = String.make 64 'b'
    }
  in
  let context : P.Moderator_execution.context =
    { id = P.Id.Moderator_execution.of_string ("mex_" ^ name) |> protocol
    ; session_id = sid
    ; generation = 0
    ; source = (source ()).observer
    ; operation_id = None
    ; job = None
    ; phase = Pre_tool_call
    ; event = `Null
    ; checkpoint_sha256 = String.make 64 'a'
    ; created_at = timestamp
    }
  in
  P.Moderator_execution.create_delegated ~delegation context |> protocol
;;

type scenario =
  { accounting : PC.t
  ; context : Context.t
  ; cause : Cause.t
  ; event : P.Moderator_execution.t
  ; goal : G.t
  ; caller_goal : P.Id.Goal.t
  ; episode : P.Goal_episode.t
  ; ledger : L.t
  }

let scenario ?(attempts = 3) () =
  let ledger = make_ledger ~attempts () in
  let descendants, _, catalog, goal, accounts =
    T.initialized ledger (T.make_children ())
  in
  let reference = T.reference "policy_caller" in
  let caller = T.child_session reference in
  let caller_source = source ~epoch:2L () in
  let caller_episode = local_episode caller caller_source in
  let caller_goal = P.Id.Goal.of_string "goal_policy_caller" |> protocol in
  let local =
    Cause.Goal_target.local ~goal_id:caller_goal ~episode:caller_episode |> protocol
  in
  let scope = T.scope reference in
  let event = seed reference in
  let cause =
    Cause.create
      ~parent:event
      ~callee:session
      ~callee_source:(source ())
      ~caller
      ~caller_source:(Cause.Caller_source.installed caller_source |> protocol)
      ~caller_origin:(Some (Run (P.Goal_episode.admission caller_episode).run_id))
      ~path:[ reference ]
      ~targets:[ Cause.Goal_target.inherited scope; local ]
      ~max_inherited_scopes:4
    |> protocol
  in
  let before =
    context
      ~accounts:(Some accounts)
      ~catalog:(Some catalog)
      ~descendants:(Some descendants)
      ()
  in
  let context =
    project_context
      before
      ~events:[ event ]
      ~current_source:before.current_source
      ~accounts:before.accounts
  in
  let update = PC.register_event None ~before ~after:context ~ledger ~cause |> protocol in
  let accounting = PC.Update.candidate update in
  let cursor = PC.Ordered.begin_ ~previous:None ~context:before ~ledger |> protocol in
  let cursor = PC.Ordered.advance cursor (Source_work context) |> protocol in
  let cursor = PC.Ordered.advance cursor (Metadata update) |> protocol in
  PC.Ordered.check cursor ~incoming:(Some accounting) ~context ~ledger |> protocol;
  { accounting
  ; context
  ; cause
  ; event
  ; goal
  ; caller_goal
  ; episode = Option.value_exn (G.episode goal)
  ; ledger
  }
;;

let occurrence ledger handle revision =
  let frame =
    Occurrence.Frame.create
      ~session
      ~generation:(L.generation ledger)
      ~candidate_session_revision:revision
    |> protocol
  in
  Occurrence.attempt
    frame
    ~ordinal:(L.Handle.ordinal handle)
    ~operation_id:(L.Handle.operation_id handle)
    ~invocation_id:(L.Handle.invocation_id handle)
  |> protocol
;;

let checked_edge state ~revision ~after ~plan =
  let update = Native.policy_update plan |> Option.value_exn in
  let accounting = PC.Update.candidate update in
  let context =
    project_context
      state.context
      ~events:state.context.events
      ~current_source:state.context.current_source
      ~accounts:(Native.accounts plan)
  in
  let cursor =
    PC.Ordered.begin_
      ~previous:(Some state.accounting)
      ~context:state.context
      ~ledger:state.ledger
    |> protocol
  in
  let cursor =
    PC.Ordered.advance cursor (Canonical { before = state.ledger; after; update })
    |> protocol
  in
  let cursor = PC.Ordered.advance cursor (Accounts context.accounts) |> protocol in
  PC.Ordered.check cursor ~incoming:(Some accounting) ~context ~ledger:after |> protocol;
  PC.Ordered.validate_publication cursor ~revision |> protocol;
  { state with accounting; context; ledger = after }
;;

let selected ?work state ~revision ~native =
  let after, handle, tracking = admit state.ledger in
  let canonical = GD.Canonical.Attempt (handle, tracking) in
  let occurrence = occurrence after handle revision in
  let policy_admission =
    PC.Admission.create
      ~cause:(Cause.key state.cause)
      ~work:
        (Option.value work ~default:(Cause.Allocation_work.Event state.event.context.id))
      ~live_prefix:state.context
      ~descendants_after:state.context.descendants
      ~canonical
      ~occurrence
    |> protocol
  in
  let admission =
    if native
    then
      Some
        ( G.id state.goal
        , A.Admission.Attempt { episode = state.episode; handle; tracking } )
    else None
  in
  let plan =
    Native.prepare
      ~policy_usage:state.accounting
      ~policy_admission
      state.ledger
      ~after
      ~accounts:state.context.accounts
      ~catalog:state.context.catalog
      ~delegated_usage:None
      ~admission
    |> protocol
  in
  checked_edge state ~revision ~after ~plan, handle, tracking, plan
;;

let factual state ~after =
  let plan =
    Native.prepare
      ~policy_usage:state.accounting
      state.ledger
      ~after
      ~accounts:state.context.accounts
      ~catalog:state.context.catalog
      ~delegated_usage:None
      ~admission:None
    |> protocol
  in
  checked_edge state ~revision:1L ~after ~plan
;;

let projection state goal_id =
  PC.projections state.accounting ~ledger:state.ledger ~session_revision:10L
  |> protocol
  |> List.find_exn ~f:(fun projection ->
    P.Id.Goal.equal
      goal_id
      (Cause.Goal_target.goal_id (PC.Stream.target (PC.Projection.stream projection))))
;;

let assert_usage usage ~attempts ~tokens =
  [%test_eq: int64] attempts (U.attempts usage);
  assert (U.Sum.equal_sum (Tokens tokens) (U.Metric.actual (U.component usage Input)))
;;

let native_usage state =
  let child =
    Accounts.find (Option.value_exn state.context.accounts) (G.id state.goal)
    |> Option.value_exn
  in
  Agent_session.Goal_accounting_document.resolve
    child
    ~goal:state.goal
    ~ledger:state.ledger
  |> protocol
  |> fun account_ -> A.usage account_ ~ledger:state.ledger |> account
;;

(* Replace the current episode through its real terminal-evidence and catalog
   transitions. The incoming policy Cause deliberately keeps its original E1
   attribution; a later native E2 admission must still cover this same Goal. *)
let next_episode state =
  let admission = P.Goal_episode.admission state.episode in
  let terminal = P.Run.Terminal.Completed None in
  let run =
    P.Run.create
      ~id:admission.run_id
      ~session:(P.Goal_episode.session state.episode)
      ~principal_id:admission.principal_id
      ~source:admission.source
      ~mode:Workflow
      ~lifecycle:(Terminal terminal)
      ~revision:Int64.(admission.run_revision + 1L)
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:timestamp
    |> protocol
  in
  let receipt =
    P.Run_receipt.create
      ~run_id:run.id
      ~principal_id:run.principal_id
      ~source:run.source
      ~key:(P.Idempotency_key.of_string "policy-first-terminal" |> protocol)
      ~request_sha256:
        (P.Run.Terminal.to_json terminal
         |> Jsonaf.to_string
         |> Digestif.SHA256.digest_string
         |> Digestif.SHA256.to_hex)
      ~kind:Terminal
      ~run_revision:run.revision
      ~session_revision:2L
      ~committed_at:timestamp
    |> protocol
  in
  let _, episode = goal ~suffix:"policy_next" ~session_revision:3L () in
  assert (not (P.Goal_episode.equal state.episode episode));
  assert (P.Run_source.equal admission.source (P.Goal_episode.admission episode).source);
  let goal =
    G.bind_episode state.goal episode ~previous:(Some (run, receipt)) |> protocol
  in
  let catalog = Option.value_exn state.context.catalog in
  let document =
    Agent_session.Goal_catalog.document catalog (G.id goal)
    |> Option.value_exn
    |> fun document -> Agent_session.Goal_document.update document goal |> domain
  in
  let catalog =
    Agent_session.Goal_catalog.advance_existing catalog ~goal:document |> protocol
  in
  let context =
    Context.create
      ~session_id:state.context.session_id
      ~generation:state.context.generation
      ~current_source:state.context.current_source
      ~events:state.context.events
      ~invocations:state.context.invocations
      ~jobs:state.context.jobs
      ~runs:None
      ~accounts:state.context.accounts
      ~catalog:(Some catalog)
      ~descendants:state.context.descendants
    |> protocol
  in
  let cursor =
    PC.Ordered.begin_
      ~previous:(Some state.accounting)
      ~context:state.context
      ~ledger:state.ledger
    |> protocol
    |> fun cursor -> PC.Ordered.advance cursor (Catalog (Some catalog)) |> protocol
  in
  PC.Ordered.check cursor ~incoming:(Some state.accounting) ~context ~ledger:state.ledger
  |> protocol;
  { state with goal; episode; context }
;;

let%expect_test
    "same source retains per-H native versus causal coverage and factual heads"
  =
  let initial = scenario () in
  let first, h1, _, _ = selected initial ~revision:1L ~native:true in
  let first = factual first ~after:(put first.ledger h1 1L (actual_views 10L)) in
  let second, h2, _, _ = selected first ~revision:2L ~native:false in
  let second = factual second ~after:(put second.ledger h2 1L (actual_views 20L)) in
  let second = next_episode second in
  let third, h3, _, _ = selected second ~revision:4L ~native:true in
  let third = factual third ~after:(put third.ledger h3 1L (actual_views 30L)) in
  let expected = [ true; false; true ] in
  List.iter2_exn (PC.bindings third.accounting) expected ~f:(fun binding native ->
    let root =
      List.find_exn (PC.Binding.choices binding) ~f:(fun choice ->
        P.Id.Goal.equal
          (G.id third.goal)
          (Cause.Goal_target.goal_id (PC.Choice.target choice)))
    in
    assert (
      Cause.Coverage.equal
        (if native then Covered_local else Causal_overlay)
        (PC.Choice.coverage root)));
  assert_usage (native_usage third) ~attempts:2L ~tokens:40L;
  assert_usage
    (PC.Projection.usage (projection third (G.id third.goal)))
    ~attempts:1L
    ~tokens:20L;
  assert_usage
    (PC.Projection.usage (projection third third.caller_goal))
    ~attempts:3L
    ~tokens:60L;
  (* End then evict the actual selected rows. Work disappearance and source
     rotation cannot erase facts, reset streams or revive the old source. *)
  let ended =
    List.fold [ h1; h2; h3 ] ~init:third ~f:(fun state handle ->
      factual state ~after:(L.set_state state.ledger handle interrupted |> ledger))
  in
  let retired =
    Fn.apply_n_times
      ~n:3
      (fun state ->
         let after, _, _ = admit state.ledger in
         factual state ~after)
      ended
  in
  [%test_eq: int] 0 (List.length (PC.bindings retired.accounting));
  let context =
    project_context
      retired.context
      ~events:[]
      ~current_source:retired.context.current_source
      ~accounts:retired.context.accounts
  in
  let cursor =
    PC.Ordered.begin_
      ~previous:(Some retired.accounting)
      ~context:retired.context
      ~ledger:retired.ledger
    |> protocol
  in
  let cursor = PC.Ordered.advance cursor (Source_work context) |> protocol in
  let rotated = source ~epoch:3L () in
  let cursor =
    PC.Ordered.advance
      cursor
      (Source_changed { current_source = Some rotated; runs = context.runs })
    |> protocol
  in
  let context =
    project_context
      context
      ~events:context.events
      ~current_source:(Some rotated)
      ~accounts:context.accounts
  in
  let pruned = PC.prune_work retired.accounting ~context |> protocol in
  let cursor = PC.Ordered.advance cursor (Metadata pruned) |> protocol in
  let retired = { retired with context; accounting = PC.Update.candidate pruned } in
  PC.Ordered.check
    cursor
    ~incoming:(Some retired.accounting)
    ~context
    ~ledger:retired.ledger
  |> protocol;
  (* The latest head keeps its immutable cause witness, even with no live Event/H. *)
  let projected = projection retired retired.caller_goal in
  let restored = PC.Projection.of_json (PC.Projection.to_json projected) |> protocol in
  assert (Option.is_some (PC.Projection.head restored));
  assert_usage (PC.Projection.usage restored) ~attempts:3L ~tokens:60L;
  let late = put retired.ledger h1 2L (actual_views 999L) in
  let retired = factual retired ~after:late in
  assert_usage
    (PC.Projection.usage (projection retired retired.caller_goal))
    ~attempts:3L
    ~tokens:60L;
  (* A new allocation cannot cite the old source, even if its old Event is restored. *)
  let stale =
    { retired with
      context =
        project_context
          context
          ~events:[ retired.event ]
          ~current_source:context.current_source
          ~accounts:context.accounts
    }
  in
  assert (
    Result.is_error
      (let after, handle, tracking = admit stale.ledger in
       let occurrence = occurrence after handle 11L in
       PC.Selection.prepare
         stale.accounting
         ~cause:(Cause.key stale.cause)
         ~work:(Event stale.event.context.id)
         ~live_prefix:stale.context
         ~coverage_after:stale.context
         ~native_admission:None
         ~before_ledger:stale.ledger
         ~after_ledger:after
         ~canonical:(Attempt (handle, tracking))
         ~occurrence));
  print_endline
    "H1/E1 and H3/E2 native, H2 overlay; caller sees all three once; retired heads and \
     old-source fences survive";
  [%expect
    {| H1/E1 and H3/E2 native, H2 overlay; caller sees all three once; retired heads and old-source fences survive |}]
;;

let%expect_test "deleting Untracked native association rejects the same real candidate" =
  let initial = scenario ~attempts:1 () in
  let first, _, _, _ = selected initial ~revision:1L ~native:true in
  let second, _, tracking, plan = selected first ~revision:2L ~native:true in
  (match tracking with
   | Untracked Attempt_count -> ()
   | Tracked | Untracked _ -> failwith "expected real live-window exhaustion");
  let update = Option.value_exn (Native.policy_update plan) in
  let update_raw = PC.Update.to_json update |> protocol in
  let reason = member update_raw "reason" in
  let selection = member reason "selection" in
  let deleted =
    set update_raw "reason" (set reason "selection" (set selection "local_attempt" `Null))
    |> PC.Update.of_json
    |> protocol
  in
  let cursor =
    PC.Ordered.begin_
      ~previous:(Some first.accounting)
      ~context:first.context
      ~ledger:first.ledger
    |> protocol
  in
  let cursor =
    PC.Ordered.advance
      cursor
      (Canonical { before = first.ledger; after = second.ledger; update = deleted })
    |> protocol
  in
  assert (Result.is_error (PC.Ordered.advance cursor (Accounts second.context.accounts)));
  let head = projection second second.caller_goal in
  [%test_eq: int64] 2L (PC.Projection.admitted_attempts head);
  [%test_eq: int64] 1L (U.untracked_attempts (PC.Projection.usage head));
  (* Actual native +1 is essential: merely retaining the same Goal in the catalog
     does not cover a later unattributed H. The first trajectory exercises that. *)
  print_endline
    "same native +1/Unknown candidate requires its exact association; decoding grants \
     nothing";
  [%expect
    {| same native +1/Unknown candidate requires its exact association; decoding grants nothing |}]
;;

let%expect_test "ordered zero-native batch and complete unknown adoption capacity" =
  let initial = scenario () in
  let first, _, _, first_plan = selected initial ~revision:4L ~native:false in
  let second, _, _, second_plan = selected first ~revision:4L ~native:false in
  let cursor =
    PC.Ordered.begin_
      ~previous:(Some initial.accounting)
      ~context:initial.context
      ~ledger:initial.ledger
    |> protocol
  in
  let cursor =
    PC.Ordered.advance
      cursor
      (Canonical
         { before = initial.ledger
         ; after = first.ledger
         ; update = Option.value_exn (Native.policy_update first_plan)
         })
    |> protocol
  in
  let cursor =
    PC.Ordered.advance
      cursor
      (Canonical
         { before = first.ledger
         ; after = second.ledger
         ; update = Option.value_exn (Native.policy_update second_plan)
         })
    |> protocol
  in
  PC.Ordered.check
    cursor
    ~incoming:(Some second.accounting)
    ~context:second.context
    ~ledger:second.ledger
  |> protocol;
  PC.Ordered.validate_publication cursor ~revision:4L |> protocol;
  assert (Result.is_error (PC.Ordered.validate_publication cursor ~revision:5L));
  [%test_eq: int64]
    2L
    (PC.Projection.admitted_attempts (projection second second.caller_goal));
  let ordinary =
    PC.Ordered.begin_
      ~previous:(Some initial.accounting)
      ~context:initial.context
      ~ledger:initial.ledger
    |> protocol
  in
  assert (
    Result.is_error
      (PC.Ordered.advance
         ordinary
         (Ordinary_canonical { before = initial.ledger; after = first.ledger })));
  (* Independently admitted complete children each fit 256KiB; their compatible
     union does not. Factual lifecycle adoption must reject the union too. *)
  let retained field =
    let document = raw initial.accounting in
    let payload = member document "payload" in
    let causes =
      match member payload "causes" with
      | `Array [ cause ] ->
        `Array [ set cause field (`String (String.make (160 * 1024) 'x')) ]
      | _ -> failwith "expected one actual cause"
    in
    set document "payload" (set payload "causes" causes) |> restore
  in
  let left = retained "future_left" in
  let right = retained "future_right" in
  assert (Result.is_error (PC.adopt left ~incoming:right));
  let cursor =
    PC.Ordered.begin_
      ~previous:(Some left)
      ~context:initial.context
      ~ledger:initial.ledger
    |> protocol
  in
  assert (
    Result.is_error
      (PC.Ordered.advance
         cursor
         (Replacement
            { context = initial.context
            ; ledger = initial.ledger
            ; accounting = Some right
            })));
  let small =
    let document = raw second.accounting in
    let payload = member document "payload" in
    let bindings =
      match member payload "bindings" with
      | `Array (binding :: rest) ->
        `Array (set binding "future_binding" (`String "must-survive") :: rest)
      | _ -> failwith "expected actual tracked bindings"
    in
    set document "payload" (set payload "bindings" bindings) |> restore
  in
  assert (Set.mem (PC.protected_ordinals small ~ledger:second.ledger |> protocol) 1L);
  assert (D.Json.equal (raw small) (raw (restore (raw small))));
  (* Public capacity queries remain separate from intrinsic admission. Charge
     the actual supplied raw child, even when its canonical carrier still fits. *)
  let module Growth = Agent_session.Capacity_growth in
  let current = raw small in
  let growth = PC.reserved_growth small |> protocol in
  assert (Growth.encoded_bytes growth > 0);
  let depth = PC.required_depth small |> protocol in
  assert (depth > 0);
  PC.validate_encoded_capacity small current |> protocol;
  let remaining = Growth.remaining_limits growth PC.limits |> protocol in
  let with_padding text =
    set
      current
      "payload"
      (set (member current "payload") "future_capacity" (`String text))
  in
  let empty = with_padding "" in
  let padding =
    D.Limits.max_bytes remaining - String.length (Jsonaf.to_string empty) + 1
  in
  assert (padding > 0);
  let supplied = with_padding (String.make padding 'x') in
  [%test_eq: int]
    (D.Limits.max_bytes remaining + 1)
    (String.length (Jsonaf.to_string supplied));
  D.Json.validate supplied ~limits:PC.limits |> domain;
  (match D.Json.validate supplied ~limits:remaining with
   | Error (Limit_exceeded "bytes") -> ()
   | Ok () | Error _ -> failwith "raw fixture did not cross only the reserved byte bound");
  let expected =
    P.Error.invalid_request
      (Sexp.to_string_hum (D.Error.sexp_of_t (Limit_exceeded "bytes")))
  in
  let expect_bytes = function
    | Error (actual : P.Error.t) ->
      [%test_eq: P.Error.code] expected.code actual.code;
      [%test_eq: string] expected.message actual.message;
      [%test_eq: bool] expected.retryable actual.retryable;
      assert (D.Json.equal expected.data actual.data)
    | Ok _ -> failwith "actual raw capacity was replaced by fitting canonical bytes"
  in
  PC.validate_encoded_capacity small supplied |> expect_bytes;
  let inspected = D.Document.inspect supplied ~limits:PC.limits |> domain in
  PC.of_document inspected |> expect_bytes;
  assert (D.Json.equal current (raw small));
  let malformed = Cause.to_json initial.cause |> fun raw -> set raw "path" `Null in
  assert (Result.is_error (Cause.of_json malformed));
  let wrong_endpoint = T.reference "another_child" in
  let malformed =
    Cause.to_json initial.cause
    |> fun raw ->
    set
      raw
      "path"
      (`Array [ Agent_store.Delegation_store.reference_to_jsonaf wrong_endpoint ])
  in
  assert (Result.is_error (Cause.of_json malformed));
  print_endline
    "two real ordered H edges seal once; positive unpaired edge and oversized unknown \
     union reject";
  [%expect
    {| two real ordered H edges seal once; positive unpaired edge and oversized unknown union reject |}]
;;

let%test_unit "later positive leaf selects capture before the first real birth" =
  let state = scenario () in
  let before =
    project_context
      state.context
      ~events:[]
      ~current_source:state.context.current_source
      ~accounts:state.context.accounts
  in
  let update =
    PC.register_event
      None
      ~before
      ~after:state.context
      ~ledger:state.ledger
      ~cause:state.cause
    |> protocol
  in
  let delta =
    Agent_session.Session_delta.Batch
      [ Moderator_execution_changed state.event; Batch [ Policy_cause_changed update ] ]
  in
  assert (Agent_session.Session_delta.requires_policy_tracking delta ~previous:None);
  let cursor =
    PC.Ordered.begin_ ~previous:None ~context:before ~ledger:state.ledger |> protocol
  in
  let cursor = PC.Ordered.advance cursor (Source_work state.context) |> protocol in
  let cursor = PC.Ordered.advance cursor (Metadata update) |> protocol in
  PC.Ordered.check
    cursor
    ~incoming:(Some state.accounting)
    ~context:state.context
    ~ledger:state.ledger
  |> protocol;
  let absent = PC.Ordered.begin_absent ~context:before ~ledger:state.ledger |> protocol in
  let absent = PC.Ordered.advance absent (Source_work state.context) |> protocol in
  assert (Result.is_error (PC.Ordered.advance absent (Metadata update)));
  assert (
    Result.is_error
      (PC.Ordered.check
         absent
         ~incoming:(Some state.accounting)
         ~context:state.context
         ~ledger:state.ledger))
;;

let%test_unit "selected subset matches independent latest-value masks through retirement" =
  List.iter [ 0; 1; 2; 3; 4 ] ~f:(fun mode ->
    let chosen index =
      match mode with
      | 0 -> false
      | 1 -> true
      | _ -> (index + mode) mod 3 <> 1
    in
    let verify state facts =
      let attempts = Int64.of_int (List.count facts ~f:fst) in
      let tokens =
        List.fold facts ~init:0L ~f:(fun sum (selected, latest) ->
          if selected then Int64.(sum + latest) else sum)
      in
      let projections =
        PC.projections state.accounting ~ledger:state.ledger ~session_revision:20L
        |> protocol
      in
      List.iter
        [ G.id state.goal; state.caller_goal ]
        ~f:(fun id ->
          match
            List.find projections ~f:(fun projection ->
              P.Id.Goal.equal
                id
                (Cause.Goal_target.goal_id
                   (PC.Stream.target (PC.Projection.stream projection))))
          with
          | None -> assert (Int64.equal attempts 0L)
          | Some projection ->
            assert_usage (PC.Projection.usage projection) ~attempts ~tokens);
      let physical = L.summary state.ledger in
      let coverage = P.Inference_query.Summary.coverage physical in
      [%test_eq: int64]
        (Int64.of_int (List.length facts))
        Int64.(
          P.Inference_query.Summary.retained_attempts physical
          + coverage.retired_attempts
          + coverage.untracked_attempts)
    in
    let state, facts, handles =
      List.fold
        (List.init 8 ~f:Fn.id)
        ~init:(scenario ~attempts:2 (), [], [])
        ~f:(fun (state, facts, handles) index ->
          let selected_ = chosen index in
          let state, handle, tracking =
            if selected_
            then (
              let state, handle, tracking, _ =
                selected state ~revision:(Int64.of_int (index + 1)) ~native:false
              in
              state, handle, tracking)
            else (
              let after, handle, tracking = admit state.ledger in
              factual state ~after, handle, tracking)
          in
          assert (L.equal_tracking tracking Tracked);
          verify state (facts @ [ selected_, 0L ]);
          let first = Int64.of_int (40 + (mode * 5) + index) in
          let state =
            factual state ~after:(put state.ledger handle 1L (actual_views first))
          in
          verify state (facts @ [ selected_, first ]);
          let corrected = Int64.of_int (mode + index + 1) in
          let state =
            factual state ~after:(put state.ledger handle 2L (actual_views corrected))
          in
          let facts = facts @ [ selected_, corrected ] in
          verify state facts;
          let state =
            factual state ~after:(L.set_state state.ledger handle interrupted |> ledger)
          in
          verify state facts;
          state, facts, handles @ [ selected_, handle ])
    in
    let _, old = Option.value (List.find handles ~f:fst) ~default:(List.hd_exn handles) in
    assert (Option.is_none (L.find state.ledger ~ordinal:(L.Handle.ordinal old)));
    let after, disposition =
      L.observe state.ledger old (usage old ~revision:3L (actual_views 999L)) |> ledger
    in
    assert (L.equal_observation_disposition disposition Ignored_retired);
    verify (factual state ~after) facts)
;;

module Forwarding_fixture = struct
  module Book = Agent_session.Goal_policy_contributions
  module Reference = Agent_store.Delegation_store
  module Forwarding = Cause.Forwarding

  type t =
    { session : P.Session_ref.t
    ; context : Context.t
    ; ledger : L.t
    ; accounting : PC.t
    }

  let array = function
    | `Array values -> values
    | _ -> failwith "expected fixture array"
  ;;

  let remove raw key =
    match raw with
    | `Object fields -> `Object (List.Assoc.remove fields key ~equal:String.equal)
    | _ -> failwith "expected fixture object"
  ;;

  let digest raw = Digestif.SHA256.(to_hex (digest_string (Jsonaf.to_string raw)))

  (* Digests use the schema's known-field order, independent of the order in
     which a raw fixture edits object members. *)
  let frame_identity raw =
    `Object
      (List.map
         [ "seed"; "work"; "member_projection_sha256"; "cause_known_sha256" ]
         ~f:(fun key -> key, member raw key))
    |> digest
  ;;

  let wire_update update =
    let encoded = PC.Update.to_json update |> protocol in
    let decoded = PC.Update.of_json encoded |> protocol in
    assert (
      D.Json.equal (raw (PC.Update.candidate update)) (raw (PC.Update.candidate decoded)));
    assert (D.Json.equal encoded (PC.Update.to_json decoded |> protocol));
    decoded
  ;;

  let wire_book_update update =
    let encoded = Book.Update.to_json update |> protocol in
    let decoded = Book.Update.of_json encoded |> protocol in
    let candidate_raw update =
      Book.Update.candidate update |> Book.to_document |> protocol |> D.Document.json
    in
    assert (D.Json.equal (candidate_raw update) (candidate_raw decoded));
    assert (D.Json.equal encoded (Book.Update.to_json decoded |> protocol));
    decoded
  ;;

  let work (event : P.Moderator_execution.t) =
    Cause.Allocation_work.Event event.context.id
  ;;

  let register
        previous
        ~(before : Context.t)
        ~ledger
        ~(event : P.Moderator_execution.t)
        ~cause
    =
    let context =
      project_context
        before
        ~events:(before.events @ [ event ])
        ~current_source:before.current_source
        ~accounts:before.accounts
    in
    let update =
      PC.register_event previous ~before ~after:context ~ledger ~cause |> protocol
    in
    let encoded = PC.Update.to_json update |> protocol in
    assert (D.Json.equal (Cause.to_json cause) (member (member encoded "reason") "cause"));
    let update = wire_update update in
    let accounting = PC.Update.candidate update in
    let cursor = PC.Ordered.begin_ ~previous ~context:before ~ledger |> protocol in
    let cursor = PC.Ordered.advance cursor (Source_work context) |> protocol in
    let cursor = PC.Ordered.advance cursor (Metadata update) |> protocol in
    PC.Ordered.check cursor ~incoming:(Some accounting) ~context ~ledger |> protocol;
    { session = Cause.callee cause; context; ledger; accounting }
  ;;

  let add_event (t : t) ~event ~cause =
    register (Some t.accounting) ~before:t.context ~ledger:t.ledger ~event ~cause
  ;;

  let cause_raw accounting cause =
    member (raw accounting) "payload"
    |> fun payload ->
    member payload "causes"
    |> array
    |> List.find_exn ~f:(fun child ->
      Cause.Key.equal
        (Cause.key cause)
        (member child "id" |> Cause.Key.of_json |> protocol))
  ;;

  let edit_cause accounting cause ~f =
    let document = raw accounting in
    let payload = member document "payload" in
    let children = member payload "causes" |> array in
    let matches child =
      Cause.Key.equal
        (Cause.key cause)
        (member child "id" |> Cause.Key.of_json |> protocol)
    in
    [%test_eq: int] 1 (List.count children ~f:matches);
    let children =
      List.map children ~f:(fun child -> if matches child then f child else child)
    in
    set document "payload" (set payload "causes" (`Array children)) |> restore
  ;;

  let reference ~parent ~child =
    let raw = T.reference "forwarding_receiver" |> Reference.reference_to_jsonaf in
    let key = member raw "key" in
    set
      raw
      "key"
      (set
         key
         "parent_session_id"
         (P.Session_ref.session_id parent |> P.Id.Session.to_json))
    |> fun raw ->
    set raw "child_session_id" (P.Session_ref.session_id child |> P.Id.Session.to_json)
    |> Reference.reference_of_jsonaf
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    |> protocol
  ;;

  let event
        ~name
        ~callee
        ~(callee_source : P.Run_source.t)
        (reference : Reference.Reference.t)
    =
    let delegation : P.Moderator_execution.delegation =
      { child_session_id = reference.child_session_id
      ; child_generation = 0
      ; child_invocation_id = P.Id.Invocation.of_string ("inv_" ^ name) |> protocol
      ; admission_sha256 = reference.admission_sha256
      }
    in
    let context : P.Moderator_execution.context =
      { id = P.Id.Moderator_execution.of_string ("mex_" ^ name) |> protocol
      ; session_id = P.Session_ref.session_id callee
      ; generation = callee_source.generation
      ; source = callee_source.observer
      ; operation_id = None
      ; job = None
      ; phase = Pre_tool_call
      ; event = `Null
      ; checkpoint_sha256 = String.make 64 'a'
      ; created_at = timestamp
      }
    in
    P.Moderator_execution.create_delegated ~delegation context |> protocol
  ;;

  let occurrence (t : t) after handle revision =
    Occurrence.Frame.create
      ~session:t.session
      ~generation:(L.generation after)
      ~candidate_session_revision:revision
    |> protocol
    |> fun frame ->
    Occurrence.attempt
      frame
      ~ordinal:(L.Handle.ordinal handle)
      ~operation_id:(L.Handle.operation_id handle)
      ~invocation_id:(L.Handle.invocation_id handle)
    |> protocol
  ;;

  let apply (t : t) ~after ~revision ~plan =
    let update = Native.policy_update plan |> Option.value_exn |> wire_update in
    let accounting = PC.Update.candidate update in
    let context =
      project_context
        t.context
        ~events:t.context.events
        ~current_source:t.context.current_source
        ~accounts:(Native.accounts plan)
    in
    let cursor =
      PC.Ordered.begin_ ~previous:(Some t.accounting) ~context:t.context ~ledger:t.ledger
      |> protocol
    in
    let cursor =
      PC.Ordered.advance cursor (Canonical { before = t.ledger; after; update })
      |> protocol
    in
    let cursor = PC.Ordered.advance cursor (Accounts context.accounts) |> protocol in
    PC.Ordered.check cursor ~incoming:(Some accounting) ~context ~ledger:after |> protocol;
    PC.Ordered.validate_publication cursor ~revision |> protocol;
    { t with accounting; context; ledger = after }
  ;;

  let commit (t : t) ~cause ~event ~revision ~after ~handle ~tracking =
    let policy_admission =
      PC.Admission.create
        ~cause:(Cause.key cause)
        ~work:(work event)
        ~live_prefix:t.context
        ~descendants_after:t.context.descendants
        ~canonical:(Attempt (handle, tracking))
        ~occurrence:(occurrence t after handle revision)
      |> protocol
    in
    let plan =
      Native.prepare
        ~policy_usage:t.accounting
        ~policy_admission
        t.ledger
        ~after
        ~accounts:t.context.accounts
        ~catalog:t.context.catalog
        ~delegated_usage:None
        ~admission:None
      |> protocol
    in
    apply t ~after ~revision ~plan
  ;;

  let observe (t : t) handle tokens ~revision =
    let after = put t.ledger handle 1L (actual_views tokens) in
    let plan =
      Native.prepare
        ~policy_usage:t.accounting
        t.ledger
        ~after
        ~accounts:t.context.accounts
        ~catalog:t.context.catalog
        ~delegated_usage:None
        ~admission:None
      |> protocol
    in
    apply t ~after ~revision ~plan
  ;;

  let projection (t : t) target revision =
    PC.projections t.accounting ~ledger:t.ledger ~session_revision:revision
    |> protocol
    |> List.find_exn ~f:(fun projection ->
      Cause.Goal_target.equal target (PC.Stream.target (PC.Projection.stream projection)))
  ;;

  let rewrite_first_frame raw ~f =
    match member raw "frames" |> array with
    | [] -> failwith "expected nonempty forwarding prefix"
    | first :: rest -> set raw "frames" (`Array (f first :: rest))
  ;;

  (* Exercise the real keyed-child conservation kernel on actual derived
     witnesses, without introducing another callee or any runtime capability. *)
  let replace_witness before after =
    let decode raw =
      Forwarding.Witness.of_json raw
      |> Result.map_error ~f:(fun (error : P.Error.t) ->
        D.Error.Invalid_field { path = []; reason = error.message })
    in
    let kind = "ochat.test-policy-forwarding-witness" in
    let codec =
      D.Domain_codec.create
        ~limits:Cause.limits
        ~kind
        ~version:1
        ~shape:Forwarding.Witness.shape
        ~supported_semantics:[]
        ~decode
        ~encode:(fun witness -> Ok (Forwarding.Witness.known_json witness))
      |> domain
    in
    let original =
      D.Document.create
        ~limits:Cause.limits
        ~kind
        ~version:1
        ~payload:(Forwarding.Witness.to_json before)
      |> domain
    in
    let carrier = D.Domain_codec.decode codec original |> domain in
    D.Domain_codec.encode codec (D.Extension_carrier.with_value carrier after)
  ;;

  let reserve previous owner cause event target occurrence =
    let request =
      Book.Request.create cause ~target ~work:(work event) ~occurrence |> protocol
    in
    match Book.reserve previous ~owner ~request |> protocol with
    | Reserved (update, pending) ->
      let encoded = Book.Update.to_json update |> protocol in
      let retained = member (member (member encoded "reason") "request") "cause" in
      assert (D.Json.equal (Cause.to_json cause) retained);
      Book.Update.candidate (wire_book_update update), pending
    | Already_pending _ | Busy _ -> failwith "fresh forwarded occurrence did not reserve"
  ;;

  let book_raw book = Book.to_document book |> protocol |> D.Document.json

  let import book projection expected =
    let update, disposition = Book.import book ~projection |> protocol in
    assert (Book.Disposition.equal expected disposition);
    let encoded = Book.Update.to_json update |> protocol in
    assert (
      D.Json.equal
        (PC.Projection.to_json projection)
        (member (member encoded "reason") "projection"));
    Book.Update.candidate (wire_book_update update)
  ;;

  let book_projection book =
    let one = function
      | [ value ] -> value
      | [] | _ :: _ -> failwith "expected exactly one causal book/stream"
    in
    let book =
      member (book_raw book) "payload" |> fun raw -> member raw "books" |> array |> one
    in
    let slot = member book "slots" |> array |> one in
    member slot "projection"
  ;;

  let book_usage book =
    book_projection book
    |> fun raw -> member raw "usage" |> U.Persistence.of_json |> protocol
  ;;
end

let%expect_test "forwarded policy preserves exact prefix and counts stable streams once" =
  let module F = Forwarding_fixture in
  let module Forwarding = Cause.Forwarding in
  let initial = scenario () in
  let c_target =
    List.find_exn (Cause.targets initial.cause) ~f:(fun target ->
      P.Id.Goal.equal initial.caller_goal (Cause.Goal_target.goal_id target))
  in
  (* Keep the existing C-local descriptor; P has its actual native Goal/account.
     Register C-only first, so P's later direct Goal is a new target. This pure
     DATA trajectory does not authenticate either caller or an ancestor route. *)
  let c_to_p =
    Cause.create
      ~parent:initial.event
      ~callee:session
      ~callee_source:(source ())
      ~caller:(Cause.caller initial.cause)
      ~caller_source:(Cause.caller_source initial.cause)
      ~caller_origin:(Cause.caller_origin initial.cause)
      ~path:(Cause.path initial.cause)
      ~targets:[ c_target ]
      ~max_inherited_scopes:4
    |> protocol
  in
  let before =
    project_context
      initial.context
      ~events:[]
      ~current_source:initial.context.current_source
      ~accounts:initial.context.accounts
  in
  let p =
    F.register None ~before ~ledger:initial.ledger ~event:initial.event ~cause:c_to_p
  in
  let target_sentinel = `Object [ "must_survive", `String "original-C-target" ] in
  let retain_target (node : F.t) cause =
    { node with
      accounting =
        F.edit_cause node.accounting cause ~f:(fun cause ->
          match member cause "targets" |> F.array with
          | [ target ] ->
            set cause "targets" (`Array [ set target "future_target" target_sentinel ])
          | [] | _ :: _ -> failwith "expected one original local target")
    }
  in
  let p = retain_target p c_to_p in
  let exported =
    PC.capture_forwarding p.accounting ~context:p.context ~work:(F.work initial.event)
    |> protocol
  in
  PC.check_forwarding_current
    p.accounting
    ~context:p.context
    ~work:(F.work initial.event)
    exported
  |> protocol;
  let direct_stream = PC.Stream.for_target c_to_p ~target:c_target |> protocol in
  assert (Option.is_none (PC.Stream.forwarding_route direct_stream));
  let direct_descriptor =
    `Object
      [ "caller", P.Session_ref.to_json (Cause.caller c_to_p)
      ; "caller_source", Cause.Caller_source.to_json (Cause.caller_source c_to_p)
      ; "callee", P.Session_ref.to_json session
      ; "callee_source", P.Run_source.to_json (source ())
      ; "path", `Array (List.map (Cause.path c_to_p) ~f:F.Reference.reference_to_jsonaf)
      ; "target", Cause.Goal_target.to_json c_target
      ]
  in
  let direct_expected =
    set direct_descriptor "id" (`String (F.digest direct_descriptor))
  in
  assert (D.Json.equal (PC.Stream.to_json direct_stream) direct_expected);
  let q_session =
    P.Session_ref.create
      ~session_id:(P.Id.Session.of_string "ses_forwarding_receiver" |> protocol)
      ~server_id:(P.Session_ref.server_id session)
  in
  let q_source = source ~epoch:3L () in
  let reference = F.reference ~parent:q_session ~child:session in
  let p_target =
    Cause.Goal_target.local ~goal_id:(G.id initial.goal) ~episode:initial.episode
    |> protocol
  in
  let q_event =
    F.event ~name:"forward_first" ~callee:q_session ~callee_source:q_source reference
  in
  let make_cause event forwarded =
    Cause.create_forwarded
      ~parent:event
      ~callee:q_session
      ~callee_source:q_source
      ~caller:session
      ~caller_source:(Cause.Caller_source.installed (source ()) |> protocol)
      ~caller_origin:(Some (Run (P.Goal_episode.admission initial.episode).run_id))
      ~path:[ reference ]
      ~direct_targets:[ p_target ]
      ~forwarded
      ~max_inherited_scopes:4
    |> protocol
  in
  let q_cause = make_cause q_event exported in
  assert (Cause.matches_event q_cause q_event);
  assert (P.Session_ref.equal session (Cause.caller q_cause));
  assert (
    Option.equal
      P.Inference_origin.equal
      (Cause.caller_origin q_cause)
      (Some (Run (P.Goal_episode.admission initial.episode).run_id)));
  assert (
    List.equal
      Cause.Goal_target.equal
      (List.sort [ c_target; p_target ] ~compare:(fun a b ->
         String.compare (Cause.Goal_target.root a) (Cause.Goal_target.root b)))
      (Cause.targets q_cause));
  let q_ledger =
    L.create
      ~session_id:(P.Session_ref.session_id q_session)
      ~generation:0
      ~before_tracking_unknown:false
      ~limits:L.Limits.default
    |> ledger
  in
  let before =
    Context.create
      ~session_id:(P.Session_ref.session_id q_session)
      ~generation:0
      ~current_source:(Some q_source)
      ~events:[]
      ~invocations:[]
      ~jobs:[]
      ~runs:None
      ~accounts:None
      ~catalog:None
      ~descendants:None
    |> protocol
  in
  let q = F.register None ~before ~ledger:q_ledger ~event:q_event ~cause:q_cause in
  let wrapper_sentinel = `Object [ "must_survive", `String "forwarding-wrapper" ] in
  let frame_sentinel = `Object [ "must_survive", `String "earlier-frame" ] in
  let retain_prefix (node : F.t) cause =
    { node with
      accounting =
        F.edit_cause node.accounting cause ~f:(fun cause ->
          let forwarded = member cause "forwarding" in
          let forwarded =
            F.rewrite_first_frame forwarded ~f:(fun frame ->
              set frame "future_frame" frame_sentinel)
          in
          set cause "forwarding" (set forwarded "future_forwarding" wrapper_sentinel))
    }
  in
  let q = retain_prefix q q_cause in
  let q_raw = F.cause_raw q.accounting q_cause in
  let source_child = Cause.Source_child.of_json q_raw |> protocol in
  assert (D.Json.equal q_raw (Cause.Source_child.raw source_child));
  assert (Cause.equal q_cause (Cause.Source_child.value source_child));
  let q_export =
    PC.capture_forwarding q.accounting ~context:q.context ~work:(F.work q_event)
    |> protocol
  in
  let export_raw = Forwarding.to_json q_export in
  [%test_eq: int] 2 (List.length (Forwarding.frames q_export));
  assert (D.Json.equal wrapper_sentinel (member export_raw "future_forwarding"));
  let first_frame = member export_raw "frames" |> F.array |> List.hd_exn in
  assert (D.Json.equal frame_sentinel (member first_frame "future_frame"));
  let c_witness = Forwarding.for_target q_export ~target:c_target |> protocol in
  assert (
    D.Json.equal
      target_sentinel
      (member (Forwarding.Witness.to_json c_witness) "target"
       |> fun target -> member target "future_target"));
  assert (
    List.equal
      Cause.Goal_target.equal
      (Cause.targets q_cause)
      (Forwarding.targets q_export));
  let introductions = member export_raw "introductions" |> F.array in
  [%test_eq: int] 2 (List.length introductions);
  List.iter introductions ~f:(fun introduction ->
    let target = member introduction "target" |> Cause.Goal_target.of_json |> protocol in
    assert (
      D.Json.equal
        (`String (if Cause.Goal_target.equal target c_target then "0" else "1"))
        (member introduction "introduced_at")));
  (* All substituted values decode. Their last frame remains authentic: current
     source membership must reject the WHOLE earlier known/raw prefix. *)
  let authentic_last = member export_raw "frames" |> F.array |> List.last_exn in
  let forged_projection = `String (String.make 64 'f') in
  let first_known =
    member (Forwarding.known_json q_export) "frames" |> F.array |> List.hd_exn
  in
  (* Changing a known frame body without updating its validated identity must
     fail construction before any current-source comparison. *)
  assert (
    Result.is_error
      (F.rewrite_first_frame export_raw ~f:(fun frame ->
         set frame "member_projection_sha256" forged_projection)
       |> Forwarding.of_json));
  let changed_known = set first_known "member_projection_sha256" forged_projection in
  let changed_id = F.frame_identity changed_known in
  let known_substitution =
    F.rewrite_first_frame export_raw ~f:(fun frame ->
      set
        (set frame "member_projection_sha256" forged_projection)
        "frame_sha256"
        (`String changed_id))
    |> Forwarding.of_json
    |> protocol
  in
  let raw_substitution =
    F.rewrite_first_frame export_raw ~f:(fun frame ->
      set frame "future_frame" (`String "rewritten"))
    |> Forwarding.of_json
    |> protocol
  in
  let target_substitution =
    let changed =
      List.map introductions ~f:(fun introduction ->
        let target =
          member introduction "target" |> Cause.Goal_target.of_json |> protocol
        in
        if Cause.Goal_target.equal target c_target
        then
          set
            introduction
            "target"
            (set
               (member introduction "target")
               "goal_id"
               (`String "goal_substituted_origin"))
        else introduction)
      |> List.sort ~compare:(fun a b ->
        let root raw =
          member raw "target"
          |> Cause.Goal_target.of_json
          |> protocol
          |> Cause.Goal_target.root
        in
        String.compare (root a) (root b))
    in
    set export_raw "introductions" (`Array changed) |> Forwarding.of_json |> protocol
  in
  assert (not (Forwarding.equal q_export known_substitution));
  assert (not (Forwarding.equal q_export target_substitution));
  assert (Forwarding.equal q_export raw_substitution);
  List.iter
    [ known_substitution; target_substitution; raw_substitution ]
    ~f:(fun supplied ->
      assert (
        D.Json.equal
          authentic_last
          (member (Forwarding.to_json supplied) "frames" |> F.array |> List.last_exn));
      assert (
        Result.is_error
          (PC.check_forwarding_current
             q.accounting
             ~context:q.context
             ~work:(F.work q_event)
             supplied)));
  PC.check_forwarding_current
    q.accounting
    ~context:q.context
    ~work:(F.work q_event)
    q_export
  |> protocol;
  let after, h1, tracking = admit q.ledger in
  let first_occurrence = F.occurrence q after h1 1L in
  let owner = Cause.Goal_target.owner c_target in
  let book, pending1 = F.reserve None owner q_cause q_event c_target first_occurrence in
  let expected_witness =
    Cause.forwarding (Cause.Source_child.value source_child)
    |> Option.value_exn
    |> fun forwarding -> Forwarding.for_target forwarding ~target:c_target |> protocol
  in
  assert (
    Option.equal
      String.equal
      (Some (Forwarding.Witness.known_sha256 expected_witness))
      (F.Book.Pending.forwarding_sha256 pending1));
  let q =
    F.commit q ~cause:q_cause ~event:q_event ~revision:1L ~after ~handle:h1 ~tracking
  in
  let q = F.observe q h1 10L ~revision:2L in
  let after_wire =
    PC.capture_forwarding q.accounting ~context:q.context ~work:(F.work q_event)
    |> protocol
  in
  assert (D.Json.equal export_raw (Forwarding.to_json after_wire));
  let projection1 = F.projection q c_target 2L in
  let stream1 = PC.Projection.stream projection1 in
  assert (Option.is_some (PC.Stream.forwarding_route stream1));
  let missing_route =
    PC.Stream.known_json stream1 |> fun raw -> F.remove raw "forwarding_route"
  in
  let missing_route =
    set missing_route "id" (`String (F.digest (F.remove missing_route "id")))
  in
  assert (Result.is_error (PC.Stream.of_json missing_route));
  let projection_raw = PC.Projection.to_json projection1 in
  let head = member projection_raw "head" in
  let witness = member head "witness" in
  let missing_witness =
    set projection_raw "head" (set head "witness" (F.remove witness "forwarding"))
  in
  assert (Result.is_error (PC.Projection.of_json missing_witness));
  let original_forwarding =
    PC.Head.Witness.forwarding
      (PC.Projection.head projection1 |> Option.value_exn |> PC.Head.witness)
    |> Option.value_exn
  in
  let first_known =
    member (Forwarding.Witness.known_json original_forwarding) "frames"
    |> F.array
    |> List.hd_exn
  in
  let changed_known =
    set first_known "member_projection_sha256" (`String (String.make 64 'e'))
  in
  let changed_id = F.frame_identity changed_known in
  let substituted =
    member witness "forwarding"
    |> fun raw ->
    F.rewrite_first_frame raw ~f:(fun frame ->
      set
        (set frame "member_projection_sha256" (`String (String.make 64 'e')))
        "frame_sha256"
        (`String changed_id))
  in
  let substituted =
    set projection_raw "head" (set head "witness" (set witness "forwarding" substituted))
    |> PC.Projection.of_json
    |> protocol
  in
  let book_before = F.book_raw book in
  assert (Result.is_error (F.Book.import book ~projection:substituted));
  assert (D.Json.equal book_before (F.book_raw book));
  let book = F.import book projection1 Added in
  assert (D.Json.equal (PC.Projection.to_json projection1) (F.book_projection book));
  assert_usage (F.book_usage book) ~attempts:1L ~tokens:10L;
  let book_before = F.book_raw book in
  let book = F.import book projection1 Duplicate in
  assert (D.Json.equal book_before (F.book_raw book));
  (* The carrier's original template predates this new actual Event/Cause row.
     Export must use its CURRENT complete document, not the historical template. *)
  let q_event2 =
    F.event ~name:"forward_second" ~callee:q_session ~callee_source:q_source reference
  in
  let retained =
    Cause.forwarding (Cause.Source_child.value source_child) |> Option.value_exn
  in
  let q_cause2 = make_cause q_event2 retained in
  let q = F.add_event q ~event:q_event2 ~cause:q_cause2 in
  let current =
    PC.capture_forwarding q.accounting ~context:q.context ~work:(F.work q_event2)
    |> protocol
  in
  assert (not (Forwarding.equal q_export current));
  assert (not (Cause.Key.equal (Cause.key q_cause) (Cause.key q_cause2)));
  let edited_raw = F.cause_raw q.accounting q_cause2 in
  let edited_child = Cause.Source_child.of_json edited_raw |> protocol in
  assert (Cause.equal q_cause2 (Cause.Source_child.value edited_child));
  let edited_export =
    PC.capture_forwarding q.accounting ~context:q.context ~work:(F.work q_event2)
    |> protocol
  in
  PC.check_forwarding_current
    q.accounting
    ~context:q.context
    ~work:(F.work q_event2)
    edited_export
  |> protocol;
  assert (
    D.Json.equal
      wrapper_sentinel
      (member (Forwarding.to_json edited_export) "future_forwarding"));
  let edited_witness = Forwarding.for_target edited_export ~target:c_target |> protocol in
  assert (
    Forwarding.Route.equal
      (Forwarding.Witness.route c_witness)
      (Forwarding.Witness.route edited_witness));
  assert (
    not
      (String.equal
         (Forwarding.Witness.known_sha256 c_witness)
         (Forwarding.Witness.known_sha256 edited_witness)));
  let carried = F.replace_witness c_witness edited_witness |> domain in
  assert (
    D.Json.equal (Forwarding.Witness.to_json edited_witness) (D.Document.payload carried));
  let changed_protected_frame =
    Forwarding.for_target known_substitution ~target:c_target |> protocol
  in
  (match F.replace_witness c_witness changed_protected_frame with
   | Error (D.Error.Extension_conflict _) -> ()
   | Error error ->
     raise_s [%sexp "unexpected protected-frame rejection", (error : D.Error.t)]
   | Ok _ -> failwith "changed protected Frame lost or rebound its unknown child");
  let after, h2, tracking = admit q.ledger in
  let book, pending2 =
    F.reserve (Some book) owner q_cause2 q_event2 c_target (F.occurrence q after h2 3L)
  in
  [%test_eq: string]
    (F.Book.Pending.stream_id pending1)
    (F.Book.Pending.stream_id pending2);
  [%test_eq: int64] 1L (F.Book.Pending.next_prefix pending1);
  [%test_eq: int64] 2L (F.Book.Pending.next_prefix pending2);
  [%test_eq: int64] 1L (F.Book.Pending.nonce pending1);
  [%test_eq: int64] 2L (F.Book.Pending.nonce pending2);
  assert (
    Option.equal
      String.equal
      (F.Book.Pending.forwarding_sha256 pending1)
      (F.Book.Pending.forwarding_sha256 pending2));
  assert (not (F.Book.Pending.equal pending1 pending2));
  let q =
    F.commit q ~cause:q_cause2 ~event:q_event2 ~revision:3L ~after ~handle:h2 ~tracking
  in
  let q = F.observe q h2 20L ~revision:4L in
  let after_wire =
    PC.capture_forwarding q.accounting ~context:q.context ~work:(F.work q_event2)
    |> protocol
  in
  assert (D.Json.equal (Forwarding.to_json edited_export) (Forwarding.to_json after_wire));
  let projection2 = F.projection q c_target 4L in
  [%test_eq: string]
    (PC.Stream.id stream1)
    (PC.Stream.id (PC.Projection.stream projection2));
  let head1 = PC.Projection.head projection1 |> Option.value_exn in
  let head2 = PC.Projection.head projection2 |> Option.value_exn in
  assert (not (Cause.Key.equal (PC.Head.cause head1) (PC.Head.cause head2)));
  assert (
    not
      (P.Id.Invocation.equal
         (PC.Head.Witness.child_invocation (PC.Head.witness head1))
         (PC.Head.Witness.child_invocation (PC.Head.witness head2))));
  [%test_eq: int]
    2
    (List.length
       (PC.projections q.accounting ~ledger:q.ledger ~session_revision:4L |> protocol));
  assert_usage (PC.Projection.usage projection2) ~attempts:2L ~tokens:30L;
  assert_usage (PC.Projection.usage (F.projection q p_target 4L)) ~attempts:2L ~tokens:30L;
  let book = F.import book projection2 Added in
  assert (D.Json.equal (PC.Projection.to_json projection2) (F.book_projection book));
  assert_usage (F.book_usage book) ~attempts:2L ~tokens:30L;
  let book_before = F.book_raw book in
  let book = F.import book projection2 Duplicate in
  assert (D.Json.equal book_before (F.book_raw book));
  let roundtrip =
    F.book_raw book
    |> fun raw ->
    D.Document.inspect raw ~limits:F.Book.limits
    |> domain
    |> F.Book.of_document
    |> protocol
  in
  assert (D.Json.equal (F.book_raw book) (F.book_raw roundtrip));
  assert (D.Json.equal (raw q.accounting) (raw (restore (raw q.accounting))));
  print_endline
    "C-local and P-local survive forwarding; exact raw prefix is current; two Hs share \
     one stream and count once";
  [%expect
    {| C-local and P-local survive forwarding; exact raw prefix is current; two Hs share one stream and count once |}]
;;

(* Pure native birth DATA and the actual ordered component codecs. Live original
   custody is qualified separately by background_transaction_tests. *)
let%expect_test "native Async and direct Job births retain one exact policy ancestry" =
  let module Source = PC.Source in
  let module Launch = Agent_session.Job_launch in
  let initial = scenario () in
  let framing (context : Context.t) =
    Source.Native_prefix.create
      ~session_id:context.session_id
      ~generation:context.generation
      ~current_source:context.current_source
      ~events:context.events
      ~invocations:context.invocations
      ~jobs:context.jobs
      ~runs:context.runs
    |> protocol
  in
  let rows (context : Context.t) ~events ~invocations ~jobs =
    Context.create
      ~session_id:context.session_id
      ~generation:context.generation
      ~current_source:context.current_source
      ~events
      ~invocations
      ~jobs
      ~runs:context.runs
      ~accounts:context.accounts
      ~catalog:context.catalog
      ~descendants:context.descendants
    |> protocol
  in
  let accept accounting before after update =
    let update = update |> Option.value_exn in
    let update = PC.Update.to_json update |> protocol |> PC.Update.of_json |> protocol in
    let cursor =
      PC.Ordered.begin_ ~previous:(Some accounting) ~context:before ~ledger:initial.ledger
      |> protocol
    in
    let cursor = PC.Ordered.advance cursor (Source_work after) |> protocol in
    let cursor = PC.Ordered.advance cursor (Metadata update) |> protocol in
    let candidate = PC.Update.candidate update in
    PC.Ordered.check
      cursor
      ~incoming:(Some candidate)
      ~context:after
      ~ledger:initial.ledger
    |> protocol;
    restore (raw candidate)
  in
  let launch =
    Launch.derive
      ~session_id:sid
      ~generation:0
      ~invocations:[]
      ~events:[ initial.event ]
      ~jobs:[]
      ~owner:(Moderator_event initial.event.context.id)
    |> protocol
  in
  let job : P.Job.t =
    { id = P.Id.Job.of_string "job_policy_async" |> protocol
    ; session_id = sid
    ; generation = 0
    ; kind = Async_tool
    ; payload = `Null
    ; status = Queued
    ; retry_policy = Never
    ; attempt = 0
    ; created_at = timestamp
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Not_required
    ; launch = Some { launch with moderator_source = Some (source ()).observer }
    ; inference_origin = Some Unbound
    ; progress = None
    }
  in
  let before = initial.context in
  let prefix = framing before in
  let forced = ref 0 in
  let update =
    Source.async_job_birth
      (Some initial.accounting)
      ~prefix
      ~before:(fun () ->
        Int.incr forced;
        Ok before)
      job
    |> protocol
  in
  [%test_eq: int] 1 !forced;
  let after = rows before ~events:before.events ~invocations:[] ~jobs:[ job ] in
  let accounting = accept initial.accounting before after update in
  let claimed = { job with status = Running; attempt = 1; started_at = Some timestamp } in
  let context = rows after ~events:after.events ~invocations:[] ~jobs:[ claimed ] in
  let member = Cause.Member.async_job claimed.id ~generation:0 |> protocol in
  let selected_cause =
    Source.cause (Some accounting) ~prefix:(framing context) ~member
    |> protocol
    |> Option.value_exn
  in
  assert (Cause.Key.equal (Cause.key selected_cause) (Cause.key initial.cause));
  (* A direct Async H is a distinct allocation family, using this actual native
     member/attempt. This branch is pure canonical DATA, not live permission. *)
  let () =
    let module Work = Cause.Allocation_work in
    let module Book = Agent_session.Goal_policy_contributions in
    let work = Work.Async_job_attempt { id = claimed.id; attempt = claimed.attempt } in
    let wire = Work.to_json work in
    assert (Work.equal work (Work.of_json wire |> protocol));
    assert (Cause.Member.equal member (Work.member work ~generation:0 |> protocol));
    List.iter
      [ `Number "0"; `Number "-1"; `Number "999999999999999999999999999" ]
      ~f:(fun attempt ->
        assert (Result.is_error (Work.of_json (set wire "attempt" attempt))));
    let baseline = { initial with accounting; context } in
    let advanced, handle, tracking, plan =
      selected ~work baseline ~revision:1L ~native:false
    in
    let update = Native.policy_update plan |> Option.value_exn in
    let encoded = PC.Update.to_json update |> protocol in
    let decoded = PC.Update.of_json encoded |> protocol in
    assert (D.Json.equal encoded (PC.Update.to_json decoded |> protocol));
    let cursor =
      PC.Ordered.begin_ ~previous:(Some accounting) ~context ~ledger:initial.ledger
      |> protocol
      |> fun cursor ->
      PC.Ordered.advance
        cursor
        (Canonical { before = initial.ledger; after = advanced.ledger; update = decoded })
      |> protocol
      |> fun cursor ->
      PC.Ordered.advance cursor (Accounts advanced.context.accounts) |> protocol
    in
    PC.Ordered.check
      cursor
      ~incoming:(Some (PC.Update.candidate decoded))
      ~context:advanced.context
      ~ledger:advanced.ledger
    |> protocol;
    PC.Ordered.validate_publication cursor ~revision:1L |> protocol;
    let restored = restore (raw advanced.accounting) in
    assert (D.Json.equal (raw advanced.accounting) (raw restored));
    let projection = projection advanced advanced.caller_goal in
    let head = PC.Projection.head projection |> Option.value_exn in
    assert (Work.equal work (PC.Head.work head));
    let projection_wire = PC.Projection.to_json projection in
    let projection' = PC.Projection.of_json projection_wire |> protocol in
    assert (D.Json.equal projection_wire (PC.Projection.to_json projection'));
    let protected_work = set wire "future_async_work" (`String "must-survive") in
    let protected_projection =
      set
        projection_wire
        "head"
        (set (T.member projection_wire "head") "work" protected_work)
    in
    assert (
      D.Json.equal
        protected_projection
        (PC.Projection.of_json protected_projection |> protocol |> PC.Projection.to_json));
    let target = PC.Stream.target (PC.Projection.stream projection) in
    let occurrence = occurrence advanced.ledger handle 1L in
    let request =
      Book.Request.create initial.cause ~target ~work ~occurrence |> protocol
    in
    let book, pending =
      match
        Book.reserve None ~owner:(Cause.Goal_target.owner target) ~request |> protocol
      with
      | Reserved (update, pending) -> Book.Update.candidate update, pending
      | Already_pending _ | Busy _ -> failwith "fresh Async occurrence did not reserve"
    in
    let pending_wire = Book.Pending.to_json pending in
    assert (Book.Pending.equal pending (Book.Pending.of_json pending_wire |> protocol));
    let _, disposition = Book.import book ~projection |> protocol in
    assert (Book.Disposition.equal Added disposition);
    let forward = PC.capture_forwarding accounting ~context ~work |> protocol in
    let forward_wire = Cause.Forwarding.to_json forward in
    let forward' = Cause.Forwarding.of_json forward_wire |> protocol in
    assert (D.Json.equal forward_wire (Cause.Forwarding.to_json forward'));
    PC.check_forwarding_current accounting ~context ~work forward' |> protocol;
    assert (Result.is_error (PC.capture_forwarding initial.accounting ~context ~work));
    let no_source =
      project_context
        context
        ~events:context.events
        ~current_source:None
        ~accounts:context.accounts
    in
    assert (Result.is_error (PC.capture_forwarding accounting ~context:no_source ~work));
    let previous_source = Option.value_exn context.current_source in
    let next_source =
      P.Run_source.create
        ~observer:previous_source.observer
        ~generation:1
        ~installation_epoch:previous_source.installation_epoch
      |> protocol
    in
    assert (
      Result.is_error
        (PC.capture_forwarding
           accounting
           ~context:
             (project_context
                ~generation:1
                context
                ~events:context.events
                ~current_source:(Some next_source)
                ~accounts:context.accounts)
           ~work));
    let wrong =
      Work.Async_job_attempt { id = claimed.id; attempt = claimed.attempt + 1 }
    in
    List.iter
      [ wrong; Work.Model_job_attempt { id = claimed.id; attempt = claimed.attempt } ]
      ~f:(fun work ->
        assert (Result.is_error (PC.capture_forwarding accounting ~context ~work));
        assert (
          Result.is_error
            (PC.Selection.prepare
               accounting
               ~cause:(Cause.key initial.cause)
               ~work
               ~live_prefix:context
               ~coverage_after:advanced.context
               ~native_admission:None
               ~before_ledger:initial.ledger
               ~after_ledger:advanced.ledger
               ~canonical:(Attempt (handle, tracking))
               ~occurrence)));
    let maximal tag =
      `Object
        [ "kind", `String tag
        ; "id", `String (String.make 96 'x')
        ; "attempt", `Number (Int.to_string Int.max_value)
        ]
    in
    [%test_eq: int]
      (String.length (Jsonaf.to_string (maximal "model_job_attempt")))
      (String.length (Jsonaf.to_string (maximal "async_job_attempt")))
  in
  let wrong_kind = Cause.Member.model_job claimed.id ~generation:0 |> protocol in
  assert (
    Result.is_error
      (Source.cause (Some accounting) ~prefix:(framing context) ~member:wrong_kind));
  let invocation_context : P.Invocation.context =
    { id = P.Id.Invocation.of_string "inv_policy_async_child" |> protocol
    ; session_id = sid
    ; generation = 0
    ; origin = Moderator
    ; provider_call_id = None
    ; call_entry_id = None
    ; parent_invocation = None
    ; parent_job = Some claimed.id
    ; tool_name = "policy_child"
    ; implementation_revision = "policy-child-v1"
    ; capability_fingerprint = String.make 64 'c'
    ; input = `Null
    ; created_at = timestamp
    ; deadline = None
    }
  in
  let invocation = P.Invocation.create invocation_context |> protocol in
  let update =
    Source.invocation_birth
      (Some accounting)
      ~prefix:(framing context)
      ~before:(fun () -> Ok context)
      invocation
    |> protocol
  in
  let after =
    rows context ~events:context.events ~invocations:[ invocation ] ~jobs:context.jobs
  in
  let accounting = accept accounting context after update in
  (* Preserve the actual Async-to-Invocation edge. The separate deferred sink
     belongs to a genuine nested Invocation, as required by its protocol. *)
  let dispatched = P.Invocation.dispatch invocation |> protocol in
  let after =
    rows after ~events:after.events ~invocations:[ dispatched ] ~jobs:after.jobs
  in
  let deferred_sink : P.Invocation.observer =
    { script_id = "deferred-policy-sink"; source_sha256 = String.make 64 'f' }
  in
  let nested_context =
    { invocation_context with
      id = P.Id.Invocation.of_string "inv_policy_deferred_child" |> protocol
    ; parent_invocation = Some invocation.context.id
    ; parent_job = None
    }
  in
  let nested = P.Invocation.create ~observer:deferred_sink nested_context |> protocol in
  let nested_update =
    Source.invocation_birth
      (Some accounting)
      ~prefix:(framing after)
      ~before:(fun () -> Ok after)
      nested
    |> protocol
  in
  let with_nested =
    rows
      after
      ~events:after.events
      ~invocations:(nested :: after.invocations)
      ~jobs:after.jobs
  in
  let accounting = accept accounting after with_nested nested_update in
  let after = with_nested in
  let invocation_member =
    Cause.Member.invocation nested.context.id ~generation:0 |> protocol
  in
  let invocation_cause =
    Source.cause (Some accounting) ~prefix:(framing after) ~member:invocation_member
    |> protocol
    |> Option.value_exn
  in
  assert (Cause.Key.equal (Cause.key invocation_cause) (Cause.key initial.cause));
  let edited_sink =
    P.Invocation.create
      ~observer:{ deferred_sink with source_sha256 = String.make 64 'e' }
      nested_context
    |> protocol
  in
  let edited_context =
    rows
      after
      ~events:after.events
      ~invocations:
        (List.map after.invocations ~f:(fun row ->
           if P.Id.Invocation.equal row.P.Invocation.context.id nested.context.id
           then edited_sink
           else row))
      ~jobs:after.jobs
  in
  assert (
    Result.is_error
      (Source.cause
         (Some accounting)
         ~prefix:(framing edited_context)
         ~member:invocation_member));
  let direct_launch =
    Launch.derive_from_job
      ~session_id:sid
      ~generation:0
      ~jobs:[ claimed ]
      ~job_id:claimed.id
      ~attempt:claimed.attempt
    |> protocol
  in
  let model =
    { job with
      id = P.Id.Job.of_string "job_policy_direct_model" |> protocol
    ; kind = Model_call
    ; launch = Some direct_launch
    }
  in
  let update =
    Source.model_job_birth
      (Some accounting)
      ~prefix:(framing after)
      ~before:(fun () -> Ok after)
      ~parent:(Some member)
      model
    |> protocol
  in
  let final =
    rows
      after
      ~events:after.events
      ~invocations:after.invocations
      ~jobs:[ model; claimed ]
  in
  let accounting = accept accounting after final update in
  let model_member = Cause.Member.model_job model.id ~generation:0 |> protocol in
  let selected =
    Source.cause (Some accounting) ~prefix:(framing final) ~member:model_member
    |> protocol
    |> Option.value_exn
  in
  assert (Cause.Key.equal (Cause.key selected) (Cause.key initial.cause));
  assert (Result.is_error (Source.cause None ~prefix:(framing final) ~member:model_member));
  (* SAME typed cause and actual terminal-index constructors; no live authority
     is claimed by this pure occurrence/ordered-codec extension. *)
  let module Index = Agent_session.Run_state in
  let module Installation = Agent_session.Run_source_installation in
  let module Delivery = Agent_session.Run_job_delivery in
  let run_id = P.Id.Run.of_string "run_policy_retained_async" |> protocol in
  let run ~lifecycle ~revision ~owned_work ~terminal_work =
    P.Run.create
      ~id:run_id
      ~session:(Cause.callee initial.cause)
      ~principal_id:principal
      ~source:(source ())
      ~mode:Workflow
      ~lifecycle
      ~revision
      ~owned_work
      ~relinquished_work:[]
      ~terminal_work
      ~created_at:timestamp
      ~updated_at:timestamp
    |> protocol
  in
  let receipt ~kind ~revision ~key =
    P.Run_receipt.create
      ~run_id
      ~principal_id:principal
      ~source:(source ())
      ~key:(P.Idempotency_key.of_string key |> protocol)
      ~request_sha256:(String.make 64 'a')
      ~kind
      ~run_revision:revision
      ~session_revision:revision
      ~committed_at:timestamp
    |> protocol
  in
  let admitted = run ~lifecycle:Admitted ~revision:0L ~owned_work:[] ~terminal_work:[] in
  let admission = receipt ~kind:Admission ~revision:0L ~key:"retained-admission" in
  let installation =
    Installation.apply Installation.initial ~change:(Replace (source ()).observer)
    |> protocol
  in
  let index =
    Index.replace_installation Index.empty ~installation ~retired_runs:[] |> protocol
  in
  let index =
    Index.commit index ~run:admitted ~receipt:admission ~intent:None |> protocol
  in
  let with_runs (context : Context.t) runs =
    Context.create
      ~session_id:context.session_id
      ~generation:context.generation
      ~current_source:context.current_source
      ~events:context.events
      ~invocations:context.invocations
      ~jobs:context.jobs
      ~runs
      ~accounts:context.accounts
      ~catalog:context.catalog
      ~descendants:context.descendants
    |> protocol
  in
  (* A work leaf cannot silently replace the retained Run ancestry index.
     Only its exact typed Source_changed leaf may advance that witness. *)
  let cursor =
    PC.Ordered.begin_ ~previous:(Some accounting) ~context:final ~ledger:initial.ledger
    |> protocol
  in
  (match PC.Ordered.advance cursor (Source_work (with_runs final (Some index))) with
   | Error { code = Invalid_request; message; _ }
     when String.equal message "policy source-work step changes native/source basis" -> ()
   | Error error ->
     raise_s [%sexp "unexpected source-work runs error", (error : P.Error.t)]
   | Ok _ -> failwith "source-work leaf silently changed retained Run ancestry");
  let queue_job =
    { job with
      id = P.Id.Job.of_string "job_policy_retained_async" |> protocol
    ; inference_origin = Some (Run run_id)
    }
  in
  let before = with_runs final (Some index) in
  let update =
    Source.async_job_birth
      (Some accounting)
      ~prefix:(framing before)
      ~before:(fun () -> Ok before)
      queue_job
    |> protocol
  in
  let born =
    rows
      before
      ~events:before.events
      ~invocations:before.invocations
      ~jobs:(queue_job :: before.jobs)
  in
  let queue_accounting = accept accounting before born update in
  let completion = P.Completion.Succeeded (`String "retained-async-result") in
  let terminal =
    { queue_job with
      status = Succeeded
    ; attempt = 1
    ; started_at = Some timestamp
    ; completed_at = Some timestamp
    ; result = Some (P.Completion.to_json completion)
    ; delivery = Delivered timestamp
    }
  in
  let work =
    P.Run_work.create
      ~key:(Retained (Job { id = terminal.id; attempt = 1 }))
      ~generation:0
    |> protocol
  in
  let proof =
    P.Run_work.Terminal.create ~work ~outcome:Succeeded ~revision:1L |> protocol
  in
  let wake =
    P.Run_wake.create
      ~run_id
      ~source:(source ())
      ~occurrence:(Job_completion { job_id = terminal.id; attempt = 1 })
    |> protocol
  in
  let waiting =
    run
      ~lifecycle:(Waiting wake)
      ~revision:1L
      ~owned_work:[ work ]
      ~terminal_work:[ proof ]
  in
  let action = receipt ~kind:Action ~revision:1L ~key:"retained-wait" in
  let intent =
    Agent_session.Run_intent.create
      ~receipt:action
      ~execution_id:initial.event.context.id
      ~action:(Wait wake)
    |> protocol
  in
  let index =
    Index.commit index ~run:waiting ~receipt:action ~intent:(Some intent) |> protocol
  in
  let frame =
    Chat_response.Background_delivery.create ~source:(source ()).observer terminal
    |> Result.ok_or_failwith
  in
  let delivery = Delivery.capture waiting ~frame |> protocol in
  let index = Index.add_job_delivery index delivery |> protocol in
  let delivery = Delivery.enqueue delivery ~at:timestamp |> protocol in
  let index = Index.replace_job_delivery index delivery |> protocol in
  let payload =
    Chat_response.Background_delivery.capture frame
    |> Session.Snapshot.of_value
    |> Result.ok_or_failwith
  in
  let queue_occurrence ~attempt =
    P.Moderator_execution.Queue_occurrence.create
      ~source:(source ())
      ~ordinal:0L
      ~origin:(Some (Run run_id))
      ~producer:
        (Captured (Some (Job_completion { id = terminal.id; generation = 0; attempt })))
    |> protocol
  in
  let queued ~name ~attempt =
    let context : P.Moderator_execution.context =
      { id = P.Id.Moderator_execution.of_string name |> protocol
      ; session_id = sid
      ; generation = 0
      ; source = (source ()).observer
      ; operation_id = None
      ; job = None
      ; phase = Internal_event
      ; event = `Object [ "snapshot", Session.Snapshot.to_jsonaf payload ]
      ; checkpoint_sha256 = String.make 64 'c'
      ; created_at = timestamp
      }
    in
    P.Moderator_execution.create_queued
      ~queue_occurrence:(queue_occurrence ~attempt)
      context
    |> protocol
  in
  let queue_event = queued ~name:"mex_policy_retained_async" ~attempt:1 in
  let successor =
    { terminal with
      status = Queued
    ; attempt = 2
    ; started_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Not_required
    }
  in
  let advanced =
    rows
      born
      ~events:born.events
      ~invocations:born.invocations
      ~jobs:
        (successor
         :: List.filter born.jobs ~f:(fun job -> not (P.Id.Job.equal job.id terminal.id))
        )
    |> fun context -> with_runs context (Some index)
  in
  let update =
    Source.queued_event_birth
      (Some queue_accounting)
      ~prefix:(framing advanced)
      ~before:(fun () -> Ok advanced)
      queue_event
    |> protocol
  in
  let advanced_after =
    rows
      advanced
      ~events:(queue_event :: advanced.events)
      ~invocations:advanced.invocations
      ~jobs:advanced.jobs
  in
  let advanced_accounting = accept queue_accounting advanced advanced_after update in
  let queued_member =
    Cause.Member.event queue_event.context.id ~generation:0 |> protocol
  in
  let chosen =
    Source.cause
      (Some advanced_accounting)
      ~prefix:(framing advanced_after)
      ~member:queued_member
    |> protocol
    |> Option.value_exn
  in
  assert (Cause.Key.equal (Cause.key chosen) (Cause.key initial.cause));
  (* Prune native rows, preserving the actual index and canonical parent chain.
     The original attempt remains1; missing current Job never becomes a live H. *)
  let missing = rows advanced ~events:initial.context.events ~invocations:[] ~jobs:[] in
  let missing_event = queued ~name:"mex_policy_pruned_async" ~attempt:1 in
  let update =
    Source.queued_event_birth
      (Some queue_accounting)
      ~prefix:(framing missing)
      ~before:(fun () -> Ok missing)
      missing_event
    |> protocol
  in
  let missing_after =
    rows missing ~events:(missing_event :: missing.events) ~invocations:[] ~jobs:[]
  in
  let missing_accounting = accept queue_accounting missing missing_after update in
  let missing_member =
    Cause.Member.event missing_event.context.id ~generation:0 |> protocol
  in
  (* An old delivered occurrence may outlive policy accounting. Its factual
     callback remains representable, without manufacturing unrelated ancestry. *)
  List.iter [ None; Some initial.accounting ] ~f:(fun accounting ->
    assert (
      Option.is_none
        (Source.queued_event_birth
           accounting
           ~prefix:(framing missing)
           ~before:(fun () -> failwith "unavailable retained history forced Context")
           missing_event
         |> protocol));
    assert (
      Result.is_error
        (Source.cause accounting ~prefix:(framing missing_after) ~member:missing_member)));
  let chosen =
    Source.cause
      (Some missing_accounting)
      ~prefix:(framing missing_after)
      ~member:missing_member
    |> protocol
    |> Option.value_exn
  in
  assert (Cause.Key.equal (Cause.key chosen) (Cause.key initial.cause));
  let old_job_member = Cause.Member.async_job terminal.id ~generation:0 |> protocol in
  assert (
    Result.is_error
      (Source.cause
         (Some missing_accounting)
         ~prefix:(framing missing_after)
         ~member:old_job_member));
  let nested_context =
    { invocation_context with
      id = P.Id.Invocation.of_string "inv_policy_after_pruned_queue" |> protocol
    ; parent_job = None
    }
  in
  let nested =
    P.Invocation.create
      ~observer:missing_event.context.source
      ~parent_event:missing_event.context.id
      nested_context
    |> protocol
  in
  let update =
    Source.invocation_birth
      (Some missing_accounting)
      ~prefix:(framing missing_after)
      ~before:(fun () -> Ok missing_after)
      nested
    |> protocol
  in
  let nested_after =
    rows missing_after ~events:missing_after.events ~invocations:[ nested ] ~jobs:[]
  in
  ignore (accept missing_accounting missing_after nested_after update : PC.t);
  let wrong_kind_context =
    rows
      advanced
      ~events:advanced.events
      ~invocations:advanced.invocations
      ~jobs:[ { successor with kind = Model_call } ]
  in
  assert (
    Result.is_error
      (Source.queued_event_birth
         (Some queue_accounting)
         ~prefix:(framing wrong_kind_context)
         ~before:(fun () -> Ok wrong_kind_context)
         queue_event));
  let no_runs = with_runs missing_after None in
  PC.validate missing_accounting ~context:no_runs ~ledger:initial.ledger |> protocol;
  let wrong = queued ~name:"mex_policy_wrong_retained_attempt" ~attempt:2 in
  let unavailable = with_runs missing None in
  assert (
    Result.is_error
      (Source.queued_event_birth
         (Some queue_accounting)
         ~prefix:(framing missing)
         ~before:(fun () -> Ok missing)
         wrong));
  assert (
    Result.is_error
      (Source.cause
         (Some missing_accounting)
         ~prefix:(framing (with_runs missing_after None))
         ~member:missing_member));
  (* Factual unavailable claim remains representable, but it cannot be a Cause. *)
  assert (
    Option.is_none
      (Source.queued_event_birth
         (Some queue_accounting)
         ~prefix:(framing unavailable)
         ~before:(fun () -> Ok unavailable)
         missing_event
       |> protocol));
  (* Even an existing terminal witness cannot fill in an unknown producer.
     A factual claim is representable; public ancestry/new native work is not. *)
  let unknown_occurrence =
    P.Moderator_execution.Queue_occurrence.create
      ~source:(source ())
      ~ordinal:0L
      ~origin:(Some (Run run_id))
      ~producer:Unknown
    |> protocol
  in
  let unknown_event =
    P.Moderator_execution.create_queued
      ~queue_occurrence:unknown_occurrence
      { queue_event.context with
        id = P.Id.Moderator_execution.of_string "mex_policy_unknown_producer" |> protocol
      }
    |> protocol
  in
  assert (
    Option.is_none
      (Source.queued_event_birth
         (Some queue_accounting)
         ~prefix:(framing missing)
         ~before:(fun () -> failwith "unavailable factual claim forced Context")
         unknown_event
       |> protocol));
  let unknown_context =
    rows missing ~events:(unknown_event :: missing.events) ~invocations:[] ~jobs:[]
  in
  let unknown_member =
    Cause.Member.event unknown_event.context.id ~generation:0 |> protocol
  in
  assert (
    Result.is_error
      (Source.cause
         (Some queue_accounting)
         ~prefix:(framing unknown_context)
         ~member:unknown_member));
  let unknown_child =
    P.Invocation.create
      ~observer:unknown_event.context.source
      ~parent_event:unknown_event.context.id
      { nested_context with
        id = P.Id.Invocation.of_string "inv_policy_unknown_parent" |> protocol
      }
    |> protocol
  in
  assert (
    Result.is_error
      (Source.invocation_birth
         (Some queue_accounting)
         ~prefix:(framing unknown_context)
         ~before:(fun () -> failwith "unknown native parent forced Context")
         unknown_child));
  let rotated =
    Context.create
      ~session_id:sid
      ~generation:0
      ~current_source:(Some (source ~epoch:2L ()))
      ~events:final.events
      ~invocations:final.invocations
      ~jobs:final.jobs
      ~runs:final.runs
      ~accounts:final.accounts
      ~catalog:final.catalog
      ~descendants:final.descendants
    |> protocol
  in
  assert (
    Result.is_error
      (Source.cause (Some accounting) ~prefix:(framing rotated) ~member:model_member));
  let ordinary =
    P.Invocation.create
      { invocation.context with
        id = P.Id.Invocation.of_string "inv_policy_unrelated" |> protocol
      ; parent_job = None
      }
    |> protocol
  in
  let ordinary_context = rows before ~events:[] ~invocations:[] ~jobs:[] in
  assert (
    Option.is_none
      (Source.invocation_birth
         None
         ~prefix:(framing ordinary_context)
         ~before:(fun () -> failwith "unrelated birth forced complete Context")
         ordinary
       |> protocol));
  let observed =
    P.Invocation.create
      ~observer:(source ()).observer
      { ordinary.context with
        id = P.Id.Invocation.of_string "inv_policy_unrelated_observed" |> protocol
      ; parent_invocation = Some ordinary.context.id
      }
    |> protocol
  in
  let observed_context =
    rows ordinary_context ~events:[] ~invocations:[ observed; ordinary ] ~jobs:[]
  in
  let ordinary_member =
    Cause.Member.invocation observed.context.id ~generation:0 |> protocol
  in
  assert (
    Option.is_none
      (Source.cause None ~prefix:(framing observed_context) ~member:ordinary_member
       |> protocol));
  let changed_source =
    P.Run_source.create
      ~observer:{ (source ()).observer with source_sha256 = String.make 64 'f' }
      ~generation:0
      ~installation_epoch:2L
    |> protocol
  in
  let changed_prefix ~invocations =
    Source.Native_prefix.create
      ~session_id:sid
      ~generation:0
      ~current_source:(Some changed_source)
      ~events:[]
      ~invocations
      ~jobs:[]
      ~runs:None
    |> protocol
  in
  (* A deferred observation sink is not the native execution source. Its exact
     value remains in the Invocation/projection, and actual notification claim
     separately requires the currently installed observer. *)
  assert (
    Option.is_none
      (Source.cause
         None
         ~prefix:(changed_prefix ~invocations:[ observed; ordinary ])
         ~member:ordinary_member
       |> protocol));
  assert (
    Option.is_none
      (Source.invocation_birth
         None
         ~prefix:(changed_prefix ~invocations:[ ordinary ])
         ~before:(fun () -> failwith "deferred sink forced complete Context")
         observed
       |> protocol));
  (* The actual retained Job launch source remains currentness evidence. Its
     genuine parent row remains present; reject the source before following it. *)
  let stale_job_prefix =
    Source.Native_prefix.create
      ~session_id:sid
      ~generation:0
      ~current_source:(Some changed_source)
      ~events:[ initial.event ]
      ~invocations:[]
      ~jobs:[ claimed ]
      ~runs:None
    |> protocol
  in
  (match Source.cause None ~prefix:stale_job_prefix ~member with
   | Error { code = Invalid_request; message; _ }
     when String.equal message "policy native observer differs from current source" -> ()
   | Ok _ | Error _ -> failwith "stale actual Job source was not rejected exactly");
  assert (
    Result.is_error
      (Source.async_job_birth
         None
         ~prefix
         ~before:(fun () -> failwith "missing seed forced complete Context")
         job));
  print_endline
    "one cause across Async, actual-kind Invocation and direct Model birth; ordered wire \
     roundtrips; missing seed/kind/source reject; unrelated birth stays lazy";
  [%expect
    {| one cause across Async, actual-kind Invocation and direct Model birth; ordered wire roundtrips; missing seed/kind/source reject; unrelated birth stays lazy |}]
;;

let%expect_test
    "unmoderated caller preserves inherited source through installed policy hops"
  =
  let module F = Forwarding_fixture in
  let module S = Cause.Caller_source in
  let initial = scenario () in
  let inherited =
    List.find_exn (Cause.targets initial.cause) ~f:(fun target ->
      match Cause.Goal_target.view target with
      | Inherited _ -> true
      | Local _ -> false)
  in
  let caller_source =
    S.Unmoderated.create
      ~generation:0
      ~prompt_revision_id:
        (P.Id.Prompt_revision.of_string ("prv_" ^ String.make 92 'u') |> protocol)
      ~configuration_revision:Int64.max_value
      ~artifact_manifest_sha256:(String.make 64 'a')
    |> protocol
    |> S.unmoderated
  in
  let create caller_source targets =
    Cause.create
      ~parent:initial.event
      ~callee:session
      ~callee_source:(source ())
      ~caller:(Cause.caller initial.cause)
      ~caller_source
      ~caller_origin:(Some Unbound)
      ~path:(Cause.path initial.cause)
      ~targets
      ~max_inherited_scopes:4
  in
  let cause = create caller_source [ inherited ] |> protocol in
  let local =
    List.find_exn (Cause.targets initial.cause) ~f:(fun target ->
      match Cause.Goal_target.view target with
      | Local _ -> true
      | Inherited _ -> false)
  in
  assert (Result.is_error (create caller_source [ local ]));
  assert (
    Result.is_error
      (Cause.of_json
         (set (Cause.to_json initial.cause) "caller_source" (S.to_json caller_source))));
  let local_stream = PC.Stream.for_target initial.cause ~target:local |> protocol in
  let invalid_local_stream =
    set (PC.Stream.known_json local_stream) "caller_source" (S.to_json caller_source)
    |> fun raw -> set raw "id" (`String (F.digest (F.remove raw "id")))
  in
  assert (Result.is_error (PC.Stream.of_json invalid_local_stream));
  let source_raw = S.to_json caller_source in
  List.iter
    [ set source_raw "generation" (`String "-1")
    ; set source_raw "configuration_revision" (`String "-1")
    ; set source_raw "configuration_revision" (`String "01")
    ; set source_raw "artifact_manifest_sha256" (`String (String.make 64 'A'))
    ; set source_raw "prompt_revision_id" (`String "ses_wrong_type")
    ; set source_raw "kind" (`String "missing")
    ; `Null
    ]
    ~f:(fun malformed -> assert (Result.is_error (S.of_json malformed)));
  List.iter
    [ set source_raw "configuration_revision" (`String "0")
    ; set source_raw "prompt_revision_id" (`String "prv_other_source")
    ; set source_raw "artifact_manifest_sha256" (`String (String.make 64 'b'))
    ]
    ~f:(fun substituted ->
      let substituted = S.of_json substituted |> protocol in
      assert (not (S.equal caller_source substituted)));
  let before =
    project_context
      initial.context
      ~events:[]
      ~current_source:initial.context.current_source
      ~accounts:initial.context.accounts
  in
  let p = F.register None ~before ~ledger:initial.ledger ~event:initial.event ~cause in
  let sentinel = `Object [ "source_extension", `String "must-survive" ] in
  let p =
    { p with
      accounting =
        F.edit_cause p.accounting cause ~f:(fun raw ->
          set raw "caller_source" (set (member raw "caller_source") "future" sentinel))
    }
  in
  let exported =
    PC.capture_forwarding p.accounting ~context:p.context ~work:(F.work initial.event)
    |> protocol
  in
  let q_session =
    P.Session_ref.create
      ~session_id:(P.Id.Session.of_string "ses_unmoderated_receiver" |> protocol)
      ~server_id:(P.Session_ref.server_id session)
  in
  let q_source = source ~epoch:3L () in
  let reference = F.reference ~parent:q_session ~child:session in
  let event =
    F.event ~name:"unmoderated_second" ~callee:q_session ~callee_source:q_source reference
  in
  let next caller_source =
    Cause.create_forwarded
      ~parent:event
      ~callee:q_session
      ~callee_source:q_source
      ~caller:session
      ~caller_source
      ~caller_origin:(Some Unbound)
      ~path:[ reference ]
      ~direct_targets:[]
      ~forwarded:exported
      ~max_inherited_scopes:4
  in
  (* A parent policy actually executed: its source cannot be replaced by the
     first caller's unmoderated source, even when generation matches. *)
  assert (Result.is_error (next caller_source));
  let q_cause = next (S.installed (source ()) |> protocol) |> protocol in
  assert (List.equal Cause.Goal_target.equal [ inherited ] (Cause.targets q_cause));
  let q_ledger =
    L.create
      ~session_id:(P.Session_ref.session_id q_session)
      ~generation:0
      ~before_tracking_unknown:false
      ~limits:L.Limits.default
    |> ledger
  in
  let before =
    Context.create
      ~session_id:(P.Session_ref.session_id q_session)
      ~generation:0
      ~current_source:(Some q_source)
      ~events:[]
      ~invocations:[]
      ~jobs:[]
      ~runs:None
      ~accounts:None
      ~catalog:None
      ~descendants:None
    |> protocol
  in
  let q = F.register None ~before ~ledger:q_ledger ~event ~cause:q_cause in
  let q_export =
    PC.capture_forwarding q.accounting ~context:q.context ~work:(F.work event) |> protocol
  in
  let first_source =
    member (Cause.Forwarding.to_json q_export) "frames"
    |> F.array
    |> List.hd_exn
    |> fun frame -> member (member frame "seed") "caller_source"
  in
  assert (S.equal caller_source (S.of_json first_source |> protocol));
  assert (D.Json.equal sentinel (member first_source "future"));
  let stale_raw =
    F.rewrite_first_frame (Cause.Forwarding.to_json q_export) ~f:(fun frame ->
      let seed = member frame "seed" in
      let source = member seed "caller_source" in
      set frame "seed" (set seed "caller_source" (set source "future" `Null)))
    |> Cause.Forwarding.of_json
    |> protocol
  in
  assert (Cause.Forwarding.equal q_export stale_raw);
  assert (
    Result.is_error
      (PC.check_forwarding_current
         q.accounting
         ~context:q.context
         ~work:(F.work event)
         stale_raw));
  let after, handle, tracking = admit q.ledger in
  let book, pending =
    F.reserve
      None
      (Cause.Goal_target.owner inherited)
      q_cause
      event
      inherited
      (F.occurrence q after handle 1L)
  in
  assert (Option.is_some (F.Book.Pending.forwarding_sha256 pending));
  (* The full existing projection profile is held for a future forwarded
     occurrence, including source envelopes. This is a capacity check, not ACK. *)
  let growth = F.Book.reserved_growth book |> protocol in
  assert (Agent_session.Capacity_growth.encoded_bytes growth > 0);
  let depth = F.Book.required_depth book |> protocol in
  [%test_eq: int] 86 depth;
  let q = F.commit q ~cause:q_cause ~event ~revision:1L ~after ~handle ~tracking in
  let q = F.observe q handle 7L ~revision:2L in
  let projection = F.projection q inherited 2L in
  let book = F.import book projection Added in
  assert_usage (F.book_usage book) ~attempts:1L ~tokens:7L;
  let before_import = F.book_raw book in
  let duplicate = F.import book projection Duplicate in
  assert (D.Json.equal before_import (F.book_raw duplicate));
  assert (D.Json.equal (raw q.accounting) (raw (restore (raw q.accounting))));
  let complete = Cause.to_json cause in
  let insufficient =
    set
      complete
      "caller_source"
      (set source_raw "future" (`String (String.make (256 * 1024) 'x')))
  in
  assert (Result.is_error (Cause.Source_child.of_json insufficient));
  let too_deep =
    List.fold (List.init 72 ~f:Fn.id) ~init:`Null ~f:(fun child _ ->
      `Object [ "nested", child ])
  in
  let insufficient_depth =
    set complete "caller_source" (set source_raw "future" too_deep)
  in
  assert (Result.is_error (Cause.Source_child.of_json insufficient_depth));
  [%test_eq: int64] (L.revision initial.ledger) (L.revision p.ledger);
  (* Maximum-length ASCII fields in both source variants pass the real framed
     admission/carrier checks. These are not JSON-escaping worst-case sizes;
     actual encoded documents are measured without increasing any budget. *)
  let maximum_installed =
    P.Run_source.create
      ~observer:{ script_id = String.make 256 's'; source_sha256 = String.make 64 'f' }
      ~generation:0
      ~installation_epoch:Int64.max_value
    |> protocol
    |> S.installed
    |> protocol
  in
  List.iter [ maximum_installed; caller_source ] ~f:(fun source ->
    let value = create source [ inherited ] |> protocol in
    let before =
      project_context
        p.context
        ~events:[]
        ~current_source:p.context.current_source
        ~accounts:p.context.accounts
    in
    let node =
      F.register None ~before ~ledger:p.ledger ~event:initial.event ~cause:value
    in
    let exported =
      PC.capture_forwarding
        node.accounting
        ~context:node.context
        ~work:(F.work initial.event)
      |> protocol
    in
    Cause.Forwarding.to_json exported |> Cause.Forwarding.of_json |> protocol |> ignore;
    PC.to_document node.accounting |> protocol |> ignore);
  let module Growth = Agent_session.Capacity_growth in
  let maximum_sources =
    [ (S.to_json maximum_installed
       |> fun raw ->
       ( set
           raw
           "source"
           (set
              (member raw "source")
              "generation"
              (`Number (Int.to_string Int.max_value)))
       , 461
       , 6
       , 7 ))
    ; set source_raw "generation" (`String (Int.to_string Int.max_value)), 318, 5, 6
    ]
  in
  List.iter maximum_sources ~f:(fun (raw, bytes, fields, nodes) ->
    let value = S.of_json raw |> protocol in
    [%test_eq: int] Int.max_value (S.generation value);
    assert (D.Json.equal raw (S.to_json value));
    let measured = Growth.measure raw ~limits:Cause.limits |> protocol in
    [%test_eq: int] bytes (Growth.encoded_bytes measured);
    [%test_eq: int] fields (Growth.fields measured);
    [%test_eq: int] nodes (Growth.nodes measured));
  print_endline
    "unmoderated source is explicit; inherited provenance and raw prefix survive; \
     capacity remains bounded";
  [%expect
    {| unmoderated source is explicit; inherited provenance and raw prefix survive; capacity remains bounded |}]
;;
