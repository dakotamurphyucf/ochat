open! Core
open Fixture
module Book = Agent_session.Goal_policy_contributions
module Totals = Agent_session.Goal_policy_totals
module Q = Policy_canonical_usage_tests
module T = Descendant_component_tests
module PC = Agent_session.Policy_canonical_usage
module Cause = Agent_session.Policy_cause_data
module Native = Agent_session.Canonical_usage_update
module GD = Agent_session.Goal_descendants
module Occurrence = Agent_session.Goal_descendant_occurrence

let member = T.member
let set = T.set
let raw book = Book.to_document book |> protocol |> D.Document.json

let restore raw =
  D.Document.inspect raw ~limits:Book.limits |> T.domain |> Book.of_document
;;

let changed update = Book.Update.candidate update

let remote_target (state : Q.scenario) =
  List.find_exn (Cause.targets state.cause) ~f:(fun target ->
    P.Id.Goal.equal state.caller_goal (Cause.Goal_target.goal_id target))
;;

let projected (state : Q.scenario) revision goal =
  PC.projections state.accounting ~ledger:state.ledger ~session_revision:revision
  |> protocol
  |> List.find_exn ~f:(fun projection ->
    P.Id.Goal.equal
      goal
      (Cause.Goal_target.goal_id (PC.Stream.target (PC.Projection.stream projection))))
;;

let prospective (state : Q.scenario) revision =
  let after, handle, tracking = admit state.ledger in
  let occurrence = Q.occurrence after handle revision in
  let request =
    Book.Request.create
      state.cause
      ~target:(remote_target state)
      ~work:(Event state.event.context.id)
      ~occurrence
    |> protocol
  in
  after, handle, tracking, request
;;

let reserved previous owner request =
  match Book.reserve previous ~owner ~request |> protocol with
  | Reserved (update, pending) -> changed update, pending
  | Already_pending _ | Busy _ -> failwith "fresh receipt did not reserve"
;;

let commit_actual (state : Q.scenario) ~revision ~after ~handle ~tracking =
  let canonical = GD.Canonical.Attempt (handle, tracking) in
  let policy_admission =
    PC.Admission.create
      ~cause:(Cause.key state.cause)
      ~work:(Event state.event.context.id)
      ~live_prefix:state.context
      ~descendants_after:state.context.descendants
      ~canonical
      ~occurrence:(Q.occurrence after handle revision)
    |> protocol
  in
  let admission =
    Some
      (G.id state.goal, A.Admission.Attempt { episode = state.episode; handle; tracking })
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
  Q.checked_edge state ~revision ~after ~plan
;;

let only_slot book =
  member (raw book) "payload"
  |> fun payload ->
  member payload "books"
  |> function
  | `Array [ book ] ->
    (match member book "slots" with
     | `Array [ slot ] -> slot
     | _ -> failwith "one slot expected")
  | _ -> failwith "one book expected"
;;

let import book projection =
  let update, disposition = Book.import book ~projection |> protocol in
  changed update, disposition
;;

let confirmed_usage book =
  member (only_slot book) "projection"
  |> fun p -> member p "usage" |> U.Persistence.of_json |> protocol
;;

let assert_selected_latest book ledger handles =
  let expected =
    List.fold handles ~init:U.empty ~f:(fun usage handle ->
      let row = L.find ledger ~ordinal:(L.Handle.ordinal handle) |> Option.value_exn in
      U.add_record usage (L.Row.record row)
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Agent_session.Goal_accounting_counter.sexp_of_error error))
      |> Result.ok_or_failwith)
  in
  (* Selection comes from this test's explicit set of actual H values. Shared
     Usage algebra is reused; the book's cumulative import arithmetic is not. *)
  assert (U.equal expected (confirmed_usage book))
;;

let update_only_slot raw ~f =
  let payload = member raw "payload" in
  let book =
    match member payload "books" with
    | `Array [ book ] -> book
    | _ -> failwith "one book expected"
  in
  let slot =
    match member book "slots" with
    | `Array [ slot ] -> slot
    | _ -> failwith "one slot expected"
  in
  set
    raw
    "payload"
    (set payload "books" (`Array [ set book "slots" (`Array [ f slot ]) ]))
;;

let%expect_test
    "remote causal book replaces latest selected usage and retains exact pending"
  =
  let initial = Q.scenario () in
  let owner = Cause.caller initial.cause in
  let after, h1, tracking, request = prospective initial 1L in
  let book, p1 = reserved None owner request in
  assert (Int64.equal (Book.Pending.nonce p1) 1L);
  (match Book.reserve (Some book) ~owner ~request |> protocol with
   | Already_pending same -> assert (Book.Pending.equal p1 same)
   | Reserved _ | Busy _ -> failwith "same pending replay changed ownership");
  let first = commit_actual initial ~revision:1L ~after ~handle:h1 ~tracking in
  let first = Q.factual first ~after:(put first.ledger h1 1L (actual_views 10L)) in
  let p10 = projected first 10L first.caller_goal in
  (* A valid decoded projection with altered original witness is still no match
     for the exact retained pending. No decoded fact grants an ACK. *)
  let altered = PC.Projection.to_json p10 in
  let head = member altered "head" in
  let witness = member head "witness" in
  let altered =
    set
      altered
      "head"
      (set
         head
         "witness"
         (set
            witness
            "child_invocation"
            (P.Id.Invocation.to_json
               (P.Id.Invocation.of_string "inv_foreign_original" |> protocol))))
    |> PC.Projection.of_json
    |> protocol
  in
  assert (Result.is_error (Book.import book ~projection:altered));
  let book, disposition = import book p10 in
  assert (Book.Disposition.equal Added disposition);
  Q.assert_usage (confirmed_usage book) ~attempts:1L ~tokens:10L;
  let book, disposition = import book p10 in
  assert (Book.Disposition.equal Duplicate disposition);
  let corrected = Q.factual first ~after:(put first.ledger h1 2L (actual_views 4L)) in
  let p11 = projected corrected 11L corrected.caller_goal in
  let book, disposition = import book p11 in
  assert (Book.Disposition.equal Replaced disposition);
  Q.assert_usage (confirmed_usage book) ~attempts:1L ~tokens:4L;
  let _, h2, tracking, request2 = prospective corrected 13L in
  let book, p2 = reserved (Some book) owner request2 in
  let before = raw book in
  let book, disposition = import book p10 in
  assert (Book.Disposition.equal Stale disposition);
  assert (D.Json.equal before (raw book));
  let pending_before = member (only_slot book) "pending" in
  let corrected =
    Q.factual corrected ~after:(put corrected.ledger h1 3L (actual_views 2L))
  in
  let p12 = projected corrected 12L corrected.caller_goal in
  let book, disposition = import book p12 in
  assert (Book.Disposition.equal Replaced disposition);
  Q.assert_usage (confirmed_usage book) ~attempts:1L ~tokens:2L;
  assert_selected_latest book corrected.ledger [ h1 ];
  assert (D.Json.equal pending_before (member (only_slot book) "pending"));
  (match Book.reserve (Some book) ~owner ~request:request2 |> protocol with
   | Already_pending same -> assert (Book.Pending.equal p2 same)
   | Reserved _ | Busy _ -> failwith "current-prefix correction consumed newer pending");
  assert (
    D.Json.equal
      (member (PC.Projection.known_json p11) "head")
      (member (PC.Projection.known_json p12) "head"));
  let enriched_projection =
    set
      (PC.Projection.to_json p12)
      "future_terminal"
      (`String "compatible duplicate must persist")
    |> PC.Projection.of_json
    |> protocol
  in
  let before_duplicate = raw book in
  let book, disposition = import book enriched_projection in
  assert (Book.Disposition.equal Duplicate disposition);
  assert (not (D.Json.equal before_duplicate (raw book)));
  let retained_projection = member (only_slot book) "projection" in
  assert (
    D.Json.equal
      (member retained_projection "future_terminal")
      (`String "compatible duplicate must persist"));
  Q.assert_usage (confirmed_usage book) ~attempts:1L ~tokens:2L;
  assert_selected_latest book corrected.ledger [ h1 ];
  assert (
    D.Json.equal
      (member (PC.Projection.known_json p12) "head")
      (member retained_projection "head"));
  assert (D.Json.equal pending_before (member (only_slot book) "pending"));
  (match Book.reserve (Some book) ~owner ~request:request2 |> protocol with
   | Already_pending same -> assert (Book.Pending.equal p2 same)
   | Reserved _ | Busy _ -> failwith "raw Duplicate consumed newer pending");
  let before_duplicate = raw book in
  let book, disposition = import book enriched_projection in
  assert (Book.Disposition.equal Duplicate disposition);
  assert (D.Json.equal before_duplicate (raw book));
  (* Rebase only the prospective ledger after the factual H1 change. The actual
     H2 coordinates and retained request stay unchanged before canonical commit. *)
  let after, same_h2, same_tracking, _ = prospective corrected 13L in
  assert (L.Handle.equal h2 same_h2);
  assert (L.equal_tracking tracking same_tracking);
  let _, _, _, another_request = prospective corrected 14L in
  (match Book.reserve (Some book) ~owner ~request:another_request |> protocol with
   | Busy same -> assert (Book.Pending.equal same p2)
   | Reserved _ | Already_pending _ -> failwith "different occurrence replaced pending");
  (* No canonical H2 edge is committed before this DATA no-writer retirement.
     The live seal which would authorize it belongs to597, not this test. *)
  let book = Book.close_before_writer book ~pending:p2 |> protocol |> changed in
  let same = Book.close_before_writer book ~pending:p2 |> protocol |> changed in
  assert (D.Json.equal (raw book) (raw same));
  let book, p3 = reserved (Some book) owner request2 in
  assert (Int64.equal (Book.Pending.nonce p3) 3L);
  assert (Result.is_error (Book.close_before_writer book ~pending:p2));
  let second = commit_actual corrected ~revision:13L ~after ~handle:h2 ~tracking in
  let second = Q.factual second ~after:(put second.ledger h2 1L (actual_views 20L)) in
  let p13 = projected second 13L second.caller_goal in
  let book, _ = import book p13 in
  (* Independent selected-H latest-value reference: neither +old cumulative
     snapshots nor no-writer nonce contribute another actual H. *)
  let reference =
    Int64.Map.of_alist_exn [ L.Handle.ordinal h1, 2L; L.Handle.ordinal h2, 20L ]
  in
  let expected =
    Map.fold reference ~init:0L ~f:(fun ~key:_ ~data total -> Int64.(total + data))
  in
  Q.assert_usage
    (confirmed_usage book)
    ~attempts:(Int64.of_int (Map.length reference))
    ~tokens:expected;
  assert_selected_latest book second.ledger [ h1; h2 ];
  let enriched =
    update_only_slot (raw book) ~f:(fun slot ->
      set slot "future_slot" (`Object [ "opaque", `String "preserve" ]))
  in
  let book = restore enriched |> protocol in
  assert (D.Json.equal enriched (raw book));
  let corrected = Q.factual second ~after:(put second.ledger h1 4L (actual_views 1L)) in
  let p15 = projected corrected 15L corrected.caller_goal in
  let book, disposition = import book p15 in
  assert (Book.Disposition.equal Replaced disposition);
  assert (
    D.Json.equal
      (member (PC.Projection.known_json p13) "head")
      (member (PC.Projection.known_json p15) "head"));
  let reference = Map.set reference ~key:(L.Handle.ordinal h1) ~data:1L in
  let expected =
    Map.fold reference ~init:0L ~f:(fun ~key:_ ~data total -> Int64.(total + data))
  in
  Q.assert_usage (confirmed_usage book) ~attempts:2L ~tokens:expected;
  assert_selected_latest book corrected.ledger [ h1; h2 ];
  assert (
    D.Json.equal
      (member (only_slot book) "future_slot")
      (`Object [ "opaque", `String "preserve" ]));
  let after, _, _, request3 = prospective corrected 16L in
  ignore (after : L.t);
  let book, _ = reserved (Some book) owner request3 in
  let excessive =
    update_only_slot (raw book) ~f:(fun slot ->
      set
        slot
        "pending"
        (set (member slot "pending") "future_large" (`String (String.make 262144 'x'))))
  in
  assert (Result.is_error (restore excessive));
  let before = raw book in
  let book, _ = import book p11 in
  assert (D.Json.equal before (raw book));
  print_endline
    "selected latest-H values replace once; stale reads cannot consume newer pending; \
     no-writer nonce and full raw bounds preserved";
  [%expect
    {| selected latest-H values replace once; stale reads cannot consume newer pending; no-writer nonce and full raw bounds preserved |}]
;;

let%expect_test
    "inline usage remains known with absent historical book and new Goal alone \
     initializes known history"
  =
  let initial = Q.scenario () in
  let first, h1, _, _ = Q.selected initial ~revision:1L ~native:true in
  let first = Q.factual first ~after:(put first.ledger h1 1L (actual_views 10L)) in
  let second, h2, _, _ = Q.selected first ~revision:2L ~native:false in
  let second = Q.factual second ~after:(put second.ledger h2 1L (actual_views 20L)) in
  let third, h3, _, _ = Q.selected second ~revision:3L ~native:true in
  let third = Q.factual third ~after:(put third.ledger h3 1L (actual_views 30L)) in
  let children = T.make_children () in
  let context causal =
    Totals.Context.create
      ~owner:session
      ~catalog:third.context.catalog
      ~accounts:third.context.accounts
      ~ledger:third.ledger
      ~descendants:third.context.descendants
      ~children:(Some children)
      ~causal
      ~canonical:(Some third.accounting)
      ~session_revision:10L
    |> protocol
  in
  let view = Totals.capture (context None) ~goal_id:(G.id third.goal) |> protocol in
  Q.assert_usage (Totals.View.local_usage view) ~attempts:2L ~tokens:40L;
  Q.assert_usage (Totals.View.causal_usage view) ~attempts:1L ~tokens:20L;
  Q.assert_usage (Totals.View.total_usage view) ~attempts:3L ~tokens:60L;
  assert (Totals.View.historical_causal_unknown view);
  let budget =
    G.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 4L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol
  in
  let goal = G.change_budget third.goal budget |> protocol in
  let catalog = Option.value_exn third.context.catalog in
  let document =
    Agent_session.Goal_document.update
      (Agent_session.Goal_catalog.document catalog (G.id goal) |> Option.value_exn)
      goal
    |> T.domain
  in
  let catalog =
    Agent_session.Goal_catalog.advance_existing catalog ~goal:document |> protocol
  in
  let limited =
    Totals.Context.create
      ~owner:session
      ~catalog:(Some catalog)
      ~accounts:third.context.accounts
      ~ledger:third.ledger
      ~descendants:third.context.descendants
      ~children:(Some children)
      ~causal:None
      ~canonical:(Some third.accounting)
      ~session_revision:10L
    |> protocol
  in
  let view = Totals.capture limited ~goal_id:(G.id goal) |> protocol in
  assert (
    Agent_session.Goal_admission_policy.Count_availability.equal
      Available
      (Totals.available view ~context:Ordinary_existing ~goal ~dimension:Attempt
       |> protocol));
  assert (
    Agent_session.Goal_admission_policy.Count_availability.equal
      Unknown
      (Totals.available view ~context:Delegated_policy_causal ~goal ~dimension:Attempt
       |> protocol));
  (* Genuine fresh Goal/account bootstrap DATA from the existing command path.
     Later native/document integration separately proves the creation edge. *)
  let _, held, _, _, account = T.command_goal initial.ledger in
  let initialized =
    Book.initialize_goal
      None
      ~owner:session
      ~goal:held
      ~account
      ~before_ledger:initial.ledger
    |> protocol
    |> changed
  in
  let subtotal =
    Book.subtotal
      (Some initialized)
      ~owner:session
      ~goal_id:(G.id third.goal)
      ~catalog:third.context.catalog
      ~accounts:third.context.accounts
      ~canonical:(Some third.accounting)
      ~ledger:third.ledger
      ~session_revision:10L
    |> protocol
  in
  assert (Book.History.equal Known_from_creation (Book.Subtotal.history subtotal));
  Q.assert_usage (Book.Subtotal.usage subtotal) ~attempts:1L ~tokens:20L;
  assert (
    Result.is_error
      (Book.initialize_goal
         (Some initialized)
         ~owner:session
         ~goal:held
         ~account
         ~before_ledger:initial.ledger));
  assert (
    Result.is_error
      (Book.Request.create
         third.cause
         ~target:(Cause.Goal_target.inherited (T.scope (T.reference "policy_caller")))
         ~work:(Event third.event.context.id)
         ~occurrence:(Q.occurrence third.ledger h3 3L)
       |> Result.bind ~f:(fun request -> Book.reserve None ~owner:session ~request)));
  print_endline
    "H1/H3 remain native; H2 inline counted once without a book; only new-Goal bootstrap \
     initializes known history";
  [%expect
    {| H1/H3 remain native; H2 inline counted once without a book; only new-Goal bootstrap initializes known history |}]
;;

let%expect_test "remote lifetime slots cannot be recycled after no-writer retirement" =
  let initial = Q.scenario () in
  let owner = Cause.caller initial.cause in
  let after, handle, _ = admit initial.ledger in
  let occurrence = Q.occurrence after handle 1L in
  let request epoch =
    (* Validated descriptor DATA only. Each installation is a different lifetime
       stream; no decoder or this pure fixture authenticates a live source. *)
    let cause =
      Cause.create
        ~parent:initial.event
        ~callee:(Cause.callee initial.cause)
        ~callee_source:(source ~epoch ())
        ~caller:owner
        ~caller_source:(Cause.caller_source initial.cause)
        ~caller_origin:(Cause.caller_origin initial.cause)
        ~path:(Cause.path initial.cause)
        ~targets:(Cause.targets initial.cause)
        ~max_inherited_scopes:(Cause.max_inherited_scopes initial.cause)
      |> protocol
    in
    Book.Request.create
      cause
      ~target:(remote_target initial)
      ~work:(Event initial.event.context.id)
      ~occurrence
    |> protocol
  in
  let book =
    List.fold
      (List.init Book.max_streams ~f:(fun i -> Int64.of_int (i + 1)))
      ~init:None
      ~f:(fun previous epoch ->
        let book, pending = reserved previous owner (request epoch) in
        Some (Book.close_before_writer book ~pending |> protocol |> changed))
    |> Option.value_exn
  in
  let payload = member (raw book) "payload" in
  let slots =
    match member payload "books" with
    | `Array [ book ] ->
      (match member book "slots" with
       | `Array slots -> slots
       | _ -> assert false)
    | _ -> assert false
  in
  assert (Int.equal (List.length slots) Book.max_streams);
  assert (Result.is_error (Book.reserve (Some book) ~owner ~request:(request 65L)));
  let before = raw book in
  (* Replaying the first exact receipt reuses its immutable descriptor and only
     advances that slot's nonce; it does not create room for a65th source. *)
  let same, pending = reserved (Some book) owner (request 1L) in
  assert (Int64.equal (Book.Pending.nonce pending) 2L);
  let same = Book.close_before_writer same ~pending |> protocol |> changed in
  assert (not (D.Json.equal before (raw same)));
  assert (Result.is_error (Book.reserve (Some same) ~owner ~request:(request 65L)));
  let restored = restore (raw same) |> protocol in
  assert (D.Json.equal (raw same) (raw restored));
  print_endline
    "64 retired lifetime descriptors remain; replay does not free or reset a slot";
  [%expect
    {| 64 retired lifetime descriptors remain; replay does not free or reset a slot |}]
;;
