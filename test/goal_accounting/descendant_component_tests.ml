open! Core
open Fixture
module GD = Agent_session.Goal_descendants
module C = Agent_session.Child_contributions
module S = Agent_session.Child_usage_snapshot
module DU = Agent_session.Delegated_usage
module Accounts = Agent_session.Goal_accounts
module Catalog = Agent_session.Goal_catalog
module Origin = Agent_store.Delegation_goal_origin
module DS = Agent_store.Delegation_store
module Pending = Agent_session.Goal_descendant_pending
module Included = Agent_session.Goal_descendant_included
module Occurrence = Agent_session.Goal_descendant_occurrence
module Marks = Agent_session.Canonical_usage_fold.Marks
module Model = Descendant_occurrence_oracle
module Existing = Descendant_contribution_tests

(* Component DATA tests use genuine canonical admissions, not decoded Handles or
   an ancestry/writer proof. The existing branch oracle remains the independent
   set/cardinality model; these scenarios exercise component ownership edges. *)
let member = Descendant_branch_tests.member
let set = Descendant_branch_tests.set

let remove raw name =
  match raw with
  | `Object fields -> `Object (List.Assoc.remove fields ~equal:String.equal name)
  | _ -> failwith "expected object"
;;

let domain result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let profile
      ?(scopes = 4)
      ?(branches = 4)
      ?(roots = 2)
      ?(bytes = 262144)
      ?(child_bytes = 1024 * 1024)
      ()
  =
  GD.Profile.create
    ~max_scopes:scopes
    ~max_roots:roots
    ~max_branches:branches
    ~max_encoded_bytes:bytes
    ~child_max_encoded_bytes:child_bytes
  |> protocol
;;

let make_children ?(inventory = C.Inventory.Complete_inventory) ?(maximum = 4) () =
  C.create ~inventory ~max_children:maximum ~max_encoded_bytes:(1024 * 1024) |> protocol
;;

let child_ledger reference =
  let document_limits =
    Transcript.Admission.limits ~max_bytes:(4 * 1024 * 1024) |> domain
  in
  let limits =
    L.Limits.create
      ~max_attempts:1
      ~max_turns:1
      ~max_retained_bytes:(4 * 1024 * 1024)
      ~document_limits
    |> ledger
  in
  L.create
    ~session_id:reference.DS.Reference.child_session_id
    ~generation:0
    ~before_tracking_unknown:false
    ~limits
  |> ledger
;;

let child_session reference =
  P.Session_ref.create
    ~session_id:reference.DS.Reference.child_session_id
    ~server_id:(P.Session_ref.server_id session)
;;

let own_json t = GD.to_document t |> protocol |> D.Document.json

let own_restore raw =
  D.Document.inspect raw ~limits:GD.limits |> domain |> GD.of_document |> protocol
;;

let update_json update = GD.Update.to_json update |> protocol
let update_restore raw = GD.Update.of_json raw |> protocol

let updated_children previous update =
  match GD.Update.children update with
  | Preserve -> previous
  | Replace incoming -> incoming
;;

let check
      ?(creation = None)
      ?(catalog = None)
      ?(accounts = None)
      ?(revision = 0L)
      previous
      update
      ~before
      ~after
      ~children
  =
  GD.validate_transition
    previous
    ~update
    ~before_ledger:before
    ~after_ledger:after
    ~before_session_revision:revision
    ~after_session_revision:(Int64.succ revision)
    ~before_children:(Some children)
    ~creation
    ~catalog
    ~accounts
;;

let context t ~ledger ~children ~catalog ~accounts ~delegation =
  GD.validate_context
    t
    ~session_id:(L.session_id ledger)
    ~ledger
    ~children:(Some children)
    ~catalog
    ~accounts
    ~delegation
;;

let command_goal before =
  let template, episode = goal () in
  let request =
    Agent_session.Goal_command.create
      ~session_id:sid
      ~generation:0
      ~source:(Some (source ()))
      ~expected_catalog_revision:0L
      ~expected_goal_revision:None
      ~key:(P.Idempotency_key.of_string "component-create" |> protocol)
      ~mutation:
        (Create
           { id = G.id template
           ; objective = G.objective template
           ; budget = G.budget template
           })
    |> protocol
  in
  let catalog =
    match
      Agent_session.Goal_command_transition.prepare
        Catalog.empty
        ~request
        ~principal_id:principal
        ~session
        ~generation:0
        ~current_source:(Some (source ()))
        ~now:timestamp
      |> protocol
    with
    | Commit { catalog; receipt = _ } -> catalog
    | Retained _ -> failwith "fresh fixture command retained"
  in
  let held = Catalog.find catalog (G.id template) |> Option.value_exn in
  assert (Option.is_none (G.episode held));
  let accounts =
    Accounts.bootstrap Accounts.empty ~goal:held ~ledger:before |> protocol
  in
  let account = Accounts.find accounts (G.id held) |> Option.value_exn in
  catalog, held, episode, accounts, account
;;

let bind catalog held episode =
  let bound = G.bind_episode held episode ~previous:None |> protocol in
  let document = Catalog.document catalog (G.id held) |> Option.value_exn in
  let document = Agent_session.Goal_document.update document bound |> domain in
  Catalog.advance_existing catalog ~goal:document |> protocol, bound
;;

let initialized ?(host_profile = profile ()) before children =
  let catalog, held, episode, accounts, account = command_goal before in
  let update =
    GD.register_new_root
      None
      ~host_profile
      ~session
      ~children:(Some children)
      ~goal:held
      ~account
      ~before_ledger:before
    |> protocol
  in
  check
    None
    update
    ~before
    ~after:before
    ~children
    ~creation:(Some (held, account))
    ~catalog:(Some catalog)
    ~accounts:(Some accounts)
  |> protocol;
  let catalog, bound = bind catalog held episode in
  GD.Update.accounting update, updated_children children update, catalog, bound, accounts
;;

let reference = Existing.reference
let scope = Existing.scope
let origin = Existing.origin

let register t children catalog accounts scope reference =
  let update =
    GD.register_branch t ~children ~scope ~reference ~origin:(origin scope) |> protocol
  in
  check
    (Some t)
    update
    ~before:(make_ledger ())
    ~after:(make_ledger ())
    ~children
    ~catalog:(Some catalog)
    ~accounts:(Some accounts)
  |> protocol;
  GD.Update.accounting update, updated_children children update, update
;;

let occurrence reference revision dimension =
  let frame =
    Occurrence.Frame.create
      ~session:(child_session reference)
      ~generation:0
      ~candidate_session_revision:revision
    |> protocol
  in
  match dimension with
  | Agent_session.Goal_admission_policy.Attempt ->
    Occurrence.attempt frame ~ordinal:revision ~operation_id:None ~invocation_id:None
    |> protocol
  | Host_turn ->
    Occurrence.host_turn frame ~operation_id:(operation (Int64.to_int_exn revision)).id
    |> protocol
;;

let reserve t children scope occurrence =
  match
    GD.reserve t ~children ~scope ~occurrence ~local_count:0L ~ceiling:None |> protocol
  with
  | Reserved (update, pending) -> GD.Update.accounting update, pending, update
  | Already_pending _ | Blocked _ -> failwith "expected fresh root reservation"
;;

let empty_descendants = Existing.empty_descendants

let partition delegated current =
  let usage = DU.usage delegated ~ledger:current |> protocol in
  let marks =
    Marks.of_ledger current
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Agent_session.Canonical_usage_fold.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  S.Partition.create ~usage ~marks |> protocol
;;

let snapshot reference scope included delegated current revision =
  let own = partition delegated current in
  let projected =
    S.Goal_scope.create ~scope ~own ~descendants:empty_descendants ~included |> protocol
  in
  S.create_with_scopes
    ~reference
    ~revision
    ~own
    ~descendants:empty_descendants
    ~canonical_coverage:(P.Inference_query.Summary.coverage (L.summary current))
    ~origin:(origin scope)
    ~goal_scopes:[ projected ]
  |> protocol
;;

(* Existing historical parent, with actual canonical work and unknown earlier
   history. Its previous child inventory and future fields are not birth DATA. *)
let first_unrelated_fixture () =
  let before =
    L.create
      ~session_id:sid
      ~generation:0
      ~before_tracking_unknown:true
      ~limits:L.Limits.default
    |> ledger
    |> admit
    |> fun (ledger, _, _) -> ledger
  in
  let old_reference = reference "first_unrelated_old" in
  let historical =
    make_children ~inventory:Historical_children_unknown ()
    |> fun children -> C.register children ~reference:old_reference |> protocol
  in
  let raw = C.to_jsonaf historical |> protocol in
  let payload = member raw "payload" in
  let slots =
    match member payload "children" with
    | `Array [ old ] -> `Array [ set old "future_slot" (`String "retain old slot") ]
    | _ -> failwith "expected historical slot"
  in
  let historical =
    set raw "payload" (set payload "children" slots)
    |> fun raw ->
    set raw "future_children" (`String "retain inventory") |> C.of_jsonaf |> protocol
  in
  let host_profile = profile ~branches:8 ~child_bytes:(2 * 1024 * 1024) () in
  let new_reference = reference "first_unrelated_new" in
  let update =
    GD.register_first_unrelated_child
      ~host_profile
      ~session
      ~reference:new_reference
      ~children:(Some historical)
      ~ledger:before
    |> protocol
  in
  before, historical, old_reference, new_reference, host_profile, update
;;

let%expect_test
    "historical first Unrelated child preserves facts without root or origin retrofit"
  =
  let before, historical, old_reference, new_reference, host_profile, update =
    first_unrelated_fixture ()
  in
  let replay ?(after = before) ?(previous = None) candidate =
    check previous candidate ~before ~after ~children:historical
  in
  let rejects message result =
    match result with
    | Ok _ -> failwith ("expected rejection: " ^ message)
    | Error (error : P.Error.t) ->
      assert (String.is_substring error.message ~substring:message)
  in
  replay (update_restore (update_json update)) |> protocol;
  let accounting = GD.Update.accounting update in
  let children = updated_children historical update in
  assert (Option.is_none (GD.origin accounting));
  assert (List.is_empty (GD.roots accounting));
  assert (List.is_empty (GD.local_included accounting));
  [%test_eq: int] (GD.Profile.max_branches (GD.profile accounting)) 4;
  [%test_eq: int]
    (GD.Profile.child_max_encoded_bytes (GD.profile accounting))
    (1024 * 1024);
  assert (P.Inference_query.Summary.coverage (L.summary before)).before_tracking_unknown;
  [%test_eq: int] (List.length (L.rows before)) 1;
  context
    accounting
    ~ledger:before
    ~children
    ~catalog:None
    ~accounts:None
    ~delegation:None
  |> protocol;
  assert (C.Inventory.equal (C.inventory children) Historical_children_unknown);
  assert (Option.is_none (C.origin children ~reference:old_reference |> protocol));
  assert (
    Option.equal
      Origin.equal
      (Some Origin.unrelated)
      (C.origin children ~reference:new_reference |> protocol));
  let raw = C.to_jsonaf children |> protocol in
  assert (D.Json.equal (member raw "future_children") (`String "retain inventory"));
  let slots =
    match member (member raw "payload") "children" with
    | `Array slots -> slots
    | _ -> failwith "expected slots"
  in
  let old_slot =
    List.find_exn slots ~f:(fun slot ->
      D.Json.equal
        (member slot "child_session_id")
        (P.Id.Session.to_json old_reference.child_session_id))
  in
  assert (D.Json.equal (member old_slot "future_slot") (`String "retain old slot"));
  rejects
    "requires absent descendant accounting"
    (replay ~previous:(Some accounting) update);
  let after, _, _ = admit before in
  rejects "cannot change the canonical ledger" (replay ~after update);
  let register ?(session = session) ?(ledger = before) children reference =
    GD.register_first_unrelated_child ~host_profile ~session ~reference ~children ~ledger
  in
  rejects "must register a new child slot" (register (Some historical) old_reference);
  rejects "must register a new child slot" (register (Some children) new_reference);
  let later =
    L.create
      ~session_id:sid
      ~generation:1
      ~before_tracking_unknown:true
      ~limits:L.Limits.default
    |> ledger
  in
  rejects
    "actual parent session/generation"
    (register ~ledger:later (Some historical) new_reference);
  let foreign_session = child_session new_reference in
  assert (
    Result.is_error (register ~session:foreign_session (Some historical) new_reference));
  let associated =
    C.register_with_origin
      historical
      ~reference:new_reference
      ~origin:(origin (scope new_reference))
    |> protocol
  in
  let forged =
    set
      (update_json update)
      "children"
      (`Object [ "kind", `String "replace"; "value", C.to_jsonaf associated |> protocol ])
    |> update_restore
  in
  rejects "paired child candidate differs" (replay forged);
  let own = own_json accounting in
  let relabelled =
    set
      own
      "payload"
      (set (member own "payload") "origin" (Origin.to_json Origin.unrelated))
    |> own_restore
  in
  rejects
    "closed Update"
    (replay
       (set (update_json update) "accounting" (own_json relabelled) |> update_restore));
  let omitted =
    set
      (update_json update)
      "children"
      (`Object [ "kind", `String "replace"; "value", remove raw "future_children" ])
    |> update_restore
  in
  rejects "omitted preserved child data" (replay omitted);
  let without_children = register None new_reference |> protocol in
  GD.validate_transition
    None
    ~update:without_children
    ~before_ledger:before
    ~after_ledger:before
    ~before_session_revision:0L
    ~after_session_revision:1L
    ~before_children:None
    ~creation:None
    ~catalog:None
    ~accounts:None
  |> protocol;
  let missing_children =
    match GD.Update.children without_children with
    | Replace children -> children
    | Preserve -> failwith "absent children require paired replacement"
  in
  assert (C.Inventory.equal (C.inventory missing_children) Historical_children_unknown);
  print_endline
    "nonzero Unknown history and old slots retained; only new Unrelated slot admitted";
  [%expect
    {| nonzero Unknown history and old slots retained; only new Unrelated slot admitted |}]
;;

let%expect_test
    "historical Create preserves uncertainty, captured profile, and complete replay basis"
  =
  let before, _, _ = admit (make_ledger ()) in
  let historical =
    make_children ~inventory:Historical_children_unknown ()
    |> fun children ->
    C.register children ~reference:(reference "old_component") |> protocol
  in
  let historical_raw =
    C.to_jsonaf historical
    |> protocol
    |> fun raw -> set raw "future_children" (`String "retain")
  in
  let historical = C.of_jsonaf historical_raw |> protocol in
  let catalog, held, episode, accounts, account = command_goal before in
  let host_profile = profile ~branches:8 ~roots:1 ~child_bytes:(2 * 1024 * 1024) () in
  let update =
    GD.register_new_root
      None
      ~host_profile
      ~session
      ~children:(Some historical)
      ~goal:held
      ~account
      ~before_ledger:before
    |> protocol
  in
  check
    None
    (update_json update |> update_restore)
    ~before
    ~after:before
    ~children:historical
    ~creation:(Some (held, account))
    ~catalog:(Some catalog)
    ~accounts:(Some accounts)
  |> protocol;
  assert (Result.is_error (check None update ~before ~after:before ~children:historical));
  let initial = GD.Update.accounting update in
  assert (Option.is_none (GD.origin initial));
  [%test_eq: int] (GD.Profile.max_branches (GD.profile initial)) 4;
  [%test_eq: int] (GD.Profile.child_max_encoded_bytes (GD.profile initial)) (1024 * 1024);
  assert (C.Inventory.equal (C.inventory historical) Historical_children_unknown);
  assert (
    D.Json.equal
      historical_raw
      (C.to_jsonaf (updated_children historical update) |> protocol));
  let coverage =
    GD.accounting_coverage initial ~children:historical ~goal_id:(G.id held) |> protocol
  in
  assert (S.Coverage.historical_children_unknown coverage);
  assert (
    GD.Availability.equal
      Available
      (GD.available
         initial
         ~children:historical
         ~goal_id:(G.id held)
         ~local_count:0L
         ~ceiling:None
         ~dimension:Attempt
       |> protocol));
  assert (
    GD.Availability.equal
      Unknown
      (GD.available
         initial
         ~children:historical
         ~goal_id:(G.id held)
         ~local_count:0L
         ~ceiling:(Some 2L)
         ~dimension:Attempt
       |> protocol));
  assert (
    Result.is_error
      (GD.create_new
         ~profile:host_profile
         ~session
         ~origin:Origin.unrelated
         ~ledger:before));
  assert (
    Result.is_error
      (GD.register_new_root
         (Some initial)
         ~host_profile
         ~session
         ~children:(Some historical)
         ~goal:held
         ~account
         ~before_ledger:before));
  let raw = own_json initial in
  let relabelled =
    set
      raw
      "payload"
      (set (member raw "payload") "origin" (Origin.to_json Origin.unrelated))
    |> own_restore
  in
  assert (Result.is_error (GD.adopt initial ~incoming:relabelled));
  let initial = set raw "future_accounting" (`String "retain") |> own_restore in
  let catalog, _ = bind catalog held episode in
  let reference = reference "new_component" in
  let scope = scope reference in
  let update =
    GD.register_branch
      initial
      ~children:historical
      ~scope
      ~reference
      ~origin:(origin scope)
    |> protocol
  in
  let replay update =
    check
      (Some initial)
      update
      ~before
      ~after:before
      ~children:historical
      ~catalog:(Some catalog)
      ~accounts:(Some accounts)
  in
  replay (update_json update |> update_restore) |> protocol;
  let wire = update_json update in
  let dropped_accounting =
    set wire "accounting" (remove (member wire "accounting") "future_accounting")
    |> update_restore
  in
  assert (Result.is_error (replay dropped_accounting));
  let action = member wire "children" in
  let dropped_child =
    set
      wire
      "children"
      (set action "value" (remove (member action "value") "future_children"))
    |> update_restore
  in
  assert (Result.is_error (replay dropped_child));
  assert (Result.is_error (GD.adopt initial ~incoming:(GD.Update.accounting update)));
  let changed =
    G.adopt_source
      (Catalog.find catalog (G.id held) |> Option.value_exn)
      (source ~epoch:2L ())
    |> protocol
  in
  let document =
    Catalog.document catalog (G.id held)
    |> Option.value_exn
    |> fun document -> Agent_session.Goal_document.update document changed |> domain
  in
  let later_catalog = Catalog.advance_existing catalog ~goal:document |> protocol in
  (* A durable lower admission may be older than the current source/episode when
     reserve succeeded but the parent frontier ACK was lost. This is DATA replay
     of that original attribution, not execution permission after adoption. *)
  assert (
    Result.is_error
      (check
         (Some initial)
         update
         ~before
         ~after:before
         ~children:historical
         ~catalog:(Some later_catalog)
         ~accounts:(Some accounts)));
  let recovered =
    GD.register_retained_branch
      initial
      ~children:historical
      ~scope
      ~reference
      ~origin:(origin scope)
    |> protocol
    |> update_json
    |> update_restore
  in
  let replay_recovered update ~after =
    check
      (Some initial)
      update
      ~before
      ~after
      ~children:historical
      ~catalog:(Some later_catalog)
      ~accounts:(Some accounts)
  in
  replay_recovered recovered ~after:before |> protocol;
  assert (
    D.Json.equal
      (own_json (GD.Update.accounting recovered))
      (own_json (GD.Update.accounting update)));
  let recovered_children = updated_children historical recovered in
  assert (
    D.Json.equal
      (C.to_jsonaf recovered_children |> protocol)
      (C.to_jsonaf (updated_children historical update) |> protocol));
  let branch =
    GD.branches (GD.Update.accounting recovered) ~goal_id:(G.id held) |> List.hd_exn
  in
  assert (Origin.Scope.equal scope (GD.Branch.scope branch));
  assert (DS.Reference.equal reference (GD.Branch.reference branch));
  [%test_eq: int64] (GD.Branch.confirmed_prefix branch) 0L;
  [%test_eq: int64] (GD.Branch.last_nonce branch) 0L;
  assert (Option.is_none (GD.Branch.pending branch));
  assert (Option.is_none (GD.origin (GD.Update.accounting recovered)));
  let after, _, _ = admit before in
  assert (Result.is_error (replay_recovered recovered ~after));
  let unknown_slot = C.register historical ~reference |> protocol in
  assert (
    Result.is_error
      (GD.register_retained_branch
         initial
         ~children:unknown_slot
         ~scope
         ~reference
         ~origin:(origin scope)));
  let wire = update_json recovered in
  let dropped =
    set wire "accounting" (remove (member wire "accounting") "future_accounting")
    |> update_restore
  in
  assert (Result.is_error (replay_recovered dropped ~after:before));
  let module Cursor = Agent_session.Goal_descendants_ordered in
  let original : Cursor.Projection.t =
    { ledger = before
    ; accounting = Some initial
    ; children = Some historical
    ; catalog = Some later_catalog
    ; accounts = Some accounts
    ; delegation = None
    ; active_operation = None
    ; invocations = []
    ; moderator_executions = []
    }
  in
  let cursor = Cursor.begin_ ~session_id:sid ~session_revision:3L original |> protocol in
  let cursor =
    Cursor.advance cursor (Update { update = recovered; creation = None }) |> protocol
  in
  let incoming = Cursor.current cursor in
  Cursor.check cursor ~incoming |> protocol;
  assert (Result.is_error (Cursor.validate_checkpoint ~before:original ~incoming));
  let direct = Cursor.begin_ ~session_id:sid ~session_revision:3L original |> protocol in
  assert (Result.is_error (Cursor.advance direct (Replacement incoming)));
  assert (Result.is_error (Cursor.advance direct (Ordinary_children recovered_children)));
  let registered = GD.Update.accounting update in
  let actual_children = updated_children historical update in
  let retained =
    GD.register_branch
      registered
      ~children:actual_children
      ~scope
      ~reference
      ~origin:(origin scope)
    |> protocol
  in
  check
    (Some registered)
    retained
    ~before
    ~after:before
    ~children:actual_children
    ~catalog:(Some later_catalog)
    ~accounts:(Some accounts)
  |> protocol;
  let fresh_ref = Existing.reference "wrong_episode" in
  let fresh_scope = Existing.scope fresh_ref in
  let fresh =
    GD.register_branch
      registered
      ~children:actual_children
      ~scope:fresh_scope
      ~reference:fresh_ref
      ~origin:(origin fresh_scope)
    |> protocol
  in
  assert (
    Result.is_error
      (check
         (Some registered)
         fresh
         ~before
         ~after:before
         ~children:actual_children
         ~catalog:(Some later_catalog)
         ~accounts:(Some accounts)));
  print_endline
    "historical origin stays Unknown; profile caps and all prior unknown bytes survive \
     finite replay";
  [%expect
    {| historical origin stays Unknown; profile caps and all prior unknown bytes survive finite replay |}]
;;

let%expect_test
    "canonical child inclusion retires one root pending and old outcomes cannot clear \
     its successor"
  =
  let before = make_ledger () in
  let initial, children, catalog, goal, accounts =
    initialized before (make_children ())
  in
  let reference = reference "component_leaf" in
  let scope = scope reference in
  let initial, children, _ = register initial children catalog accounts scope reference in
  let zero = child_ledger reference in
  let child_children = make_children () in
  let child =
    GD.create_new
      ~profile:(profile ())
      ~session:(child_session reference)
      ~origin:(origin scope)
      ~ledger:zero
    |> protocol
  in
  let first_occurrence = occurrence reference 1L Attempt in
  let reserved, pending, reservation = reserve initial children scope first_occurrence in
  check
    (Some initial)
    reservation
    ~before
    ~after:before
    ~children
    ~catalog:(Some catalog)
    ~accounts:(Some accounts)
  |> protocol;
  let current, handle, tracking = admit zero in
  let included = Included.create pending in
  let local =
    GD.record_included
      child
      ~children:child_children
      ~before:zero
      ~after:current
      ~canonical:(Attempt (handle, tracking))
      ~occurrence:first_occurrence
      ~receipts:[ included ]
    |> protocol
  in
  check
    (Some child)
    (update_json local |> update_restore)
    ~before:zero
    ~after:current
    ~children:child_children
  |> protocol;
  let child = GD.Update.accounting local in
  context
    child
    ~ledger:current
    ~children:child_children
    ~catalog:None
    ~accounts:None
    ~delegation:(Some reference)
  |> protocol;
  let delegated =
    DU.create ~reference ~ledger:zero
    |> protocol
    |> fun delegated -> DU.reconcile delegated ~before:zero ~after:current |> protocol
  in
  let observed = snapshot reference scope (Some included) delegated current 1L in
  let imported = GD.import reserved ~children ~snapshot:observed |> protocol in
  check
    (Some reserved)
    (update_json imported |> update_restore)
    ~before
    ~after:before
    ~children
  |> protocol;
  let confirmed = GD.Update.accounting imported in
  let confirmed_children = updated_children children imported in
  context
    confirmed
    ~ledger:before
    ~children:confirmed_children
    ~catalog:(Some catalog)
    ~accounts:(Some accounts)
    ~delegation:None
  |> protocol;
  let model = Model.create ~paths:[ "leaf", [ "root" ] ] ~unknown_edges:[] in
  let semantic : Model.Occurrence.t =
    { leaf = "leaf"; generation = 0; candidate_revision = 1L; kind = Attempt 1L }
  in
  let model =
    Model.record model ~occurrence:semantic ~status:(Included { input_tokens = 0L })
  in
  let branch state = GD.branches state ~goal_id:(G.id goal) |> List.hd_exn in
  [%test_eq: int64]
    (GD.Branch.confirmed_prefix (branch confirmed))
    (Model.totals model ~root:"root" ~dimension:Attempt).committed;
  assert (Option.is_none (GD.Branch.pending (branch confirmed)));
  let second, p2, _ =
    reserve confirmed confirmed_children scope (occurrence reference 2L Attempt)
  in
  let duplicate =
    GD.import second ~children:confirmed_children ~snapshot:observed |> protocol
  in
  assert (D.Json.equal (own_json second) (own_json (GD.Update.accounting duplicate)));
  assert (
    Pending.equal
      p2
      (GD.Branch.pending (branch (GD.Update.accounting duplicate)) |> Option.value_exn));
  let closed =
    GD.close_before_writer second ~children:confirmed_children ~pending:p2 |> protocol
  in
  check (Some second) closed ~before ~after:before ~children:confirmed_children
  |> protocol;
  let third, p3, _ =
    reserve
      (GD.Update.accounting closed)
      confirmed_children
      scope
      (occurrence reference 3L Attempt)
  in
  [%test_eq: int64] (Pending.next_prefix p2) (Pending.next_prefix p3);
  assert (Int64.(Pending.nonce p3 > Pending.nonce p2));
  let old_close =
    GD.close_before_writer third ~children:confirmed_children ~pending:p2 |> protocol
  in
  assert (D.Json.equal (own_json third) (own_json (GD.Update.accounting old_close)));
  let old_import =
    GD.import third ~children:confirmed_children ~snapshot:observed |> protocol
  in
  assert (D.Json.equal (own_json third) (own_json (GD.Update.accounting old_import)));
  assert (
    Result.is_error
      (context
         reserved
         ~ledger:before
         ~children:confirmed_children
         ~catalog:(Some catalog)
         ~accounts:(Some accounts)
         ~delegation:None));
  print_endline
    "one Included set member; duplicate preserves successor; no-writer consumes nonce \
     without count";
  [%expect
    {| one Included set member; duplicate preserves successor; no-writer consumes nonce without count |}]
;;

let%expect_test
    "actual tracked and untracked handles validate both dimensions and completed context \
     bounds"
  =
  List.iter
    [ Agent_session.Goal_admission_policy.Attempt; Host_turn ]
    ~f:(fun dimension ->
      let reference =
        reference
          (match dimension with
           | Attempt -> "canonical_attempt"
           | Host_turn -> "canonical_turn")
      in
      let scope = scope reference in
      let children = make_children () in
      let zero = child_ledger reference in
      let initial =
        GD.create_new
          ~profile:(profile ())
          ~session:(child_session reference)
          ~origin:(origin scope)
          ~ledger:zero
        |> protocol
      in
      let advance state before revision =
        let after, canonical, tracking =
          match dimension with
          | Attempt ->
            let after, handle, tracking = admit before in
            after, GD.Canonical.Attempt (handle, tracking), tracking
          | Host_turn ->
            let operation = operation (Int64.to_int_exn revision) in
            let after, handle, tracking = L.admit_turn before operation |> ledger in
            after, GD.Canonical.Host_turn (operation, handle, tracking), tracking
        in
        let occurrence = occurrence reference revision dimension in
        let pending =
          Pending.create
            ~scope
            ~branch_reference:reference
            ~nonce:revision
            ~next_prefix:revision
            ~occurrence
          |> protocol
        in
        let update =
          GD.record_included
            state
            ~children
            ~before
            ~after
            ~canonical
            ~occurrence
            ~receipts:[ Included.create pending ]
          |> protocol
        in
        check
          (Some state)
          (update_json update |> update_restore)
          ~before
          ~after
          ~children
          ~revision:(Int64.pred revision)
        |> protocol;
        after, update, tracking
      in
      let first, first_update, tracking = advance initial zero 1L in
      assert (L.equal_tracking tracking Tracked);
      let one = GD.Update.accounting first_update in
      if Agent_session.Goal_admission_policy.equal_dimension dimension Attempt
      then (
        (* A second disjoint leaf cannot reuse the first leaf's receipt while
           increasing this intermediate subtree's cardinality. *)
        let base_ref = Existing.reference "deeper_leaf" in
        let raw = DS.reference_to_jsonaf base_ref in
        let deeper_ref =
          set
            raw
            "key"
            (set
               (member raw "key")
               "parent_session_id"
               (P.Id.Session.to_json reference.child_session_id))
          |> DS.reference_of_jsonaf
          |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
          |> protocol
        in
        let registered =
          GD.register_child one ~children ~reference:deeper_ref ~origin:(origin scope)
          |> protocol
        in
        let deeper_children = updated_children children registered in
        let deeper_zero = child_ledger deeper_ref in
        let deeper_one, _, _ = admit deeper_zero in
        let delegated =
          DU.create ~reference:deeper_ref ~ledger:deeper_zero
          |> protocol
          |> fun delegated ->
          DU.reconcile delegated ~before:deeper_zero ~after:deeper_one |> protocol
        in
        let reused = GD.local_included one |> List.hd_exn in
        let observed = snapshot deeper_ref scope (Some reused) delegated deeper_one 1L in
        assert (
          Result.is_error
            (GD.import
               (GD.Update.accounting registered)
               ~children:deeper_children
               ~snapshot:observed)));
      let second, second_update, tracking = advance one first 2L in
      assert (
        match tracking with
        | Tracked -> false
        | Untracked _ -> true);
      context
        (GD.Update.accounting second_update)
        ~ledger:second
        ~children
        ~catalog:None
        ~accounts:None
        ~delegation:(Some reference)
      |> protocol;
      assert (
        Result.is_error
          (context
             initial
             ~ledger:first
             ~children
             ~catalog:None
             ~accounts:None
             ~delegation:(Some reference)));
      assert (
        Result.is_error
          (context
             one
             ~ledger:second
             ~children
             ~catalog:None
             ~accounts:None
             ~delegation:(Some reference)));
      let wire = update_json second_update in
      let reason = member wire "reason" in
      let fact = member reason "fact" in
      let replace_tracking tracking =
        set wire "reason" (set reason "fact" (set fact "tracking" (`String tracking)))
      in
      let wrong_dimension =
        match dimension with
        | Attempt -> "untracked_turn_count"
        | Host_turn -> "untracked_attempt_count"
      in
      assert (Result.is_error (GD.Update.of_json (replace_tracking wrong_dimension)));
      let different =
        if
          String.equal
            (P.Json_codec.string (member fact "tracking") |> protocol)
            "untracked_retained_bytes"
        then "untracked_protected_future_data"
        else "untracked_retained_bytes"
      in
      let relabelled = replace_tracking different |> update_restore in
      assert (
        Result.is_error
          (check (Some one) relabelled ~before:first ~after:second ~children ~revision:1L));
      let wrong_ref =
        DS.reference_to_jsonaf reference
        |> fun raw ->
        set
          raw
          "revision_id"
          (P.Id.Prompt_revision.to_json
             (Existing.reference "another_reference").revision_id)
        |> DS.reference_of_jsonaf
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
        |> protocol
      in
      assert (
        Result.is_error
          (context
             one
             ~ledger:first
             ~children
             ~catalog:None
             ~accounts:None
             ~delegation:(Some wrong_ref))));
  let before = make_ledger () in
  let historical =
    make_children ~inventory:Historical_children_unknown ()
    |> fun children ->
    C.register children ~reference:(reference "historical_slot") |> protocol
  in
  let initial, children, catalog, _, accounts =
    initialized ~host_profile:(profile ~branches:1 ()) before historical
  in
  let reference = reference "bounded_branch" in
  let scope = scope reference in
  let registered, children, _ =
    register initial children catalog accounts scope reference
  in
  (* Existing unrelated physical slots do not spend the single per-root branch. *)
  let unrelated =
    GD.register_child
      registered
      ~children
      ~reference:(Existing.reference "unrelated_slot")
      ~origin:Origin.unrelated
    |> protocol
  in
  let children = updated_children children unrelated in
  let other_ref = Existing.reference "second_root_branch" in
  let other_scope = Existing.scope other_ref in
  assert (
    Result.is_error
      (GD.register_branch
         registered
         ~children
         ~scope:other_scope
         ~reference:other_ref
         ~origin:(origin other_scope)));
  assert (
    Result.is_error
      (GD.create_new
         ~profile:(profile ~bytes:1 ())
         ~session
         ~origin:Origin.unrelated
         ~ledger:before));
  let disabled =
    GD.create_new
      ~profile:(profile ~scopes:0 ())
      ~session
      ~origin:Origin.unrelated
      ~ledger:before
    |> protocol
  in
  assert (D.Json.equal (own_json disabled) (own_json (own_restore (own_json disabled))));
  assert (
    Result.is_error
      (GD.create_new
         ~profile:(profile ~scopes:0 ())
         ~session
         ~origin:(origin scope)
         ~ledger:before));
  print_endline
    "real handles in both dimensions; impossible labels, missing heads and per-root \
     overflow reject";
  [%expect
    {| real handles in both dimensions; impossible labels, missing heads and per-root overflow reject |}]
;;

let%expect_test
    "existing-slot future depth uses exact outer paths and preserved local alternatives"
  =
  let depth_limits depth =
    D.Limits.create
      ~max_bytes:(16 * 1024 * 1024)
      ~max_depth:depth
      ~max_fields:100000
      ~max_nodes:1000000
    |> domain
  in
  (* Independent boundary oracle: the actual JSON validator decides the first
     admissible depth, including scalars. No copy of the production depth walk. *)
  let minimum_depth raw =
    List.find_exn (List.range 1 257) ~f:(fun depth ->
      Result.is_ok (D.Json.validate raw ~limits:(depth_limits depth)))
  in
  let embedded name raw = `Object [ "payload", `Object [ name, raw ] ] in
  let boundary name raw =
    let required = minimum_depth raw + 2 in
    let host = embedded name raw in
    D.Json.validate host ~limits:(depth_limits required) |> domain;
    assert (Result.is_error (D.Json.validate host ~limits:(depth_limits (required - 1))));
    required
  in
  let first raw ~f =
    match raw with
    | `Array (one :: rest) -> `Array (f one :: rest)
    | _ -> failwith "expected nonempty capacity fixture"
  in
  let nested =
    List.fold (List.init 12 ~f:Fn.id) ~init:(`String "scalar-leaf") ~f:(fun raw _ ->
      `Array [ raw ])
  in
  let before = make_ledger () in
  let initial, children, catalog, _, accounts = initialized before (make_children ()) in
  let reference = reference "depth_leaf" in
  let scope = scope reference in
  let initial, children, _ = register initial children catalog accounts scope reference in
  let initial_depth = GD.required_depth initial |> protocol in
  let reserved, pending, _ =
    reserve initial children scope (occurrence reference 1L Host_turn)
  in
  let closed =
    GD.close_before_writer reserved ~children ~pending |> protocol |> GD.Update.accounting
  in
  assert (minimum_depth (own_json initial) < initial_depth);
  [%test_eq: int] initial_depth (minimum_depth (own_json closed));
  [%test_eq: int] (boundary "goal_descendants" (own_json closed)) (initial_depth + 2);
  (* Without protected extensions the Host_turn -> Attempt alternative is real. *)
  let successor, next, _ =
    reserve closed children scope (occurrence reference 2L Attempt)
  in
  let alternative =
    GD.close_before_writer successor ~children ~pending:next
    |> protocol
    |> GD.Update.accounting
  in
  assert (initial_depth >= minimum_depth (own_json alternative));
  let raw = own_json closed in
  let payload = member raw "payload" in
  let roots =
    member payload "roots"
    |> first ~f:(fun root ->
      let branches =
        member root "branches"
        |> first ~f:(fun wrapper ->
          let branch = member wrapper "branch" in
          let last = member branch "last" in
          let pending = member last "pending" in
          let occurrence = member pending "occurrence" in
          let frame =
            member occurrence "frame" |> fun frame -> set frame "future_depth" nested
          in
          set
            wrapper
            "branch"
            (set
               branch
               "last"
               (set
                  last
                  "pending"
                  (set pending "occurrence" (set occurrence "frame" frame)))))
      in
      set root "branches" branches)
  in
  let carried = set raw "payload" (set payload "roots" roots) |> own_restore in
  let required = GD.required_depth carried |> protocol in
  assert (required >= minimum_depth (own_json carried));
  assert (
    Result.is_error
      (GD.reserve
         carried
         ~children
         ~scope
         ~occurrence:(occurrence reference 2L Attempt)
         ~local_count:0L
         ~ceiling:None));
  let successor, next, _ =
    reserve carried children scope (occurrence reference 2L Host_turn)
  in
  let terminal =
    GD.close_before_writer successor ~children ~pending:next
    |> protocol
    |> GD.Update.accounting
  in
  (* Protected kind change is excluded, but same-kind terminal keeps its Frame
     extension and remains included in exact required depth. *)
  let terminal_raw = own_json terminal in
  let branches =
    match member (member terminal_raw "payload") "roots" with
    | `Array (root :: _) -> member root "branches"
    | _ -> failwith "expected descendant root"
  in
  let branch =
    match branches with
    | `Array (wrapper :: _) -> member wrapper "branch"
    | _ -> failwith "expected descendant branch"
  in
  let frame =
    member branch "last"
    |> fun last ->
    member last "pending"
    |> fun pending ->
    member pending "occurrence" |> fun occurrence -> member occurrence "frame"
  in
  assert (D.Json.equal nested (member frame "future_depth"));
  assert (required >= minimum_depth terminal_raw);
  ignore (boundary "goal_descendants" terminal_raw : int);
  let child_zero = child_ledger reference in
  let child_current, _, _ = L.admit_turn child_zero (operation 1) |> ledger in
  let delegated =
    DU.create ~reference ~ledger:child_zero
    |> protocol
    |> fun delegated ->
    DU.reconcile delegated ~before:child_zero ~after:child_current |> protocol
  in
  let observed =
    snapshot reference scope (Some (Included.create pending)) delegated child_current 1L
  in
  let required_children = C.required_depth children |> protocol in
  let observed_children, _ = C.replace children ~snapshot:observed |> protocol in
  let observed_raw = C.to_jsonaf observed_children |> protocol in
  assert (minimum_depth (C.to_jsonaf children |> protocol) < required_children);
  assert (required_children >= minimum_depth observed_raw);
  ignore (boundary "child_contributions" observed_raw : int);
  let payload = member observed_raw "payload" in
  let slots =
    member payload "children"
    |> first ~f:(fun slot ->
      let snapshot = member slot "snapshot" in
      let entries =
        member snapshot "goal_scopes"
        |> first ~f:(fun entry ->
          let pending = member entry "included" in
          let occurrence = member pending "occurrence" in
          let frame =
            member occurrence "frame" |> fun frame -> set frame "future_depth" nested
          in
          set entry "included" (set pending "occurrence" (set occurrence "frame" frame)))
      in
      set slot "snapshot" (set snapshot "goal_scopes" entries))
  in
  let observed_children =
    set observed_raw "payload" (set payload "children" slots) |> C.of_jsonaf |> protocol
  in
  let required_children = C.required_depth observed_children |> protocol in
  let observed_raw = C.to_jsonaf observed_children |> protocol in
  assert (required_children >= minimum_depth observed_raw);
  D.Json.validate
    (embedded "child_contributions" observed_raw)
    ~limits:(depth_limits (required_children + 2))
  |> domain;
  assert (
    Result.is_error
      (D.Json.validate
         (embedded "child_contributions" observed_raw)
         ~limits:(depth_limits (minimum_depth observed_raw + 1))));
  print_endline
    "current can fit while future cannot; exact two-edge outer nesting and preserved \
     alternatives remain bounded";
  [%expect
    {| current can fit while future cannot; exact two-edge outer nesting and preserved alternatives remain bounded |}]
;;
