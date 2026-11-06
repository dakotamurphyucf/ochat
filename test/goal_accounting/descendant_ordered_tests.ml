open! Core
open Fixture
module T = Descendant_component_tests
module GD = Agent_session.Goal_descendants
module Cursor = Agent_session.Goal_descendants_ordered
module Occurrence = Agent_session.Goal_descendant_occurrence
module Pending = Agent_session.Goal_descendant_pending
module Included = Agent_session.Goal_descendant_included

(* These fixtures plan from real canonical Handles. The cursor checks factual
   ordered replay only; retained execution DATA never becomes writer authority. *)
let inherited_fixture name =
  let reference = T.reference name in
  let scope = T.scope reference in
  let ledger = T.child_ledger reference in
  let children = T.make_children () in
  let accounting =
    GD.create_new
      ~profile:(T.profile ())
      ~session:(T.child_session reference)
      ~origin:(T.origin scope)
      ~ledger
    |> protocol
  in
  let projection : Cursor.Projection.t =
    { ledger
    ; accounting = Some accounting
    ; children = Some children
    ; catalog = None
    ; accounts = None
    ; delegation = Some reference
    ; active_operation = None
    ; invocations = []
    ; moderator_executions = []
    }
  in
  projection, scope, reference
;;

let begin_ ?(revision = 0L) (projection : Cursor.Projection.t) =
  Cursor.begin_
    ~session_id:(L.session_id projection.ledger)
    ~session_revision:revision
    projection
  |> protocol
;;

let advance cursor step = Cursor.advance cursor step |> protocol
let update_step update = Cursor.Update { update; creation = None }
let finish cursor = Cursor.check cursor ~incoming:(Cursor.current cursor) |> protocol

let frame reference revision generation =
  Occurrence.Frame.create
    ~session:(T.child_session reference)
    ~generation
    ~candidate_session_revision:revision
  |> protocol
;;

let record
      (projection : Cursor.Projection.t)
      scope
      reference
      ~after
      ~canonical
      ~occurrence
      ~prefix
  =
  let pending =
    Pending.create
      ~scope
      ~branch_reference:reference
      ~nonce:prefix
      ~next_prefix:prefix
      ~occurrence
    |> protocol
  in
  GD.record_included
    (Option.value_exn projection.accounting)
    ~children:(Option.value_exn projection.children)
    ~before:projection.ledger
    ~after
    ~canonical
    ~occurrence
    ~receipts:[ Included.create pending ]
  |> protocol
;;

let record_attempt projection scope reference ~after ~handle ~tracking ~revision ~prefix =
  let occurrence =
    Occurrence.attempt
      (frame reference revision (L.Handle.generation handle))
      ~ordinal:(L.Handle.ordinal handle)
      ~operation_id:(L.Handle.operation_id handle)
      ~invocation_id:(L.Handle.invocation_id handle)
    |> protocol
  in
  record
    projection
    scope
    reference
    ~after
    ~canonical:(Attempt (handle, tracking))
    ~occurrence
    ~prefix
;;

let first_included initial scope reference =
  let after, handle, tracking = admit initial.Cursor.Projection.ledger in
  assert (L.equal_tracking tracking Tracked);
  let update =
    record_attempt
      initial
      scope
      reference
      ~after
      ~handle
      ~tracking
      ~revision:1L
      ~prefix:1L
  in
  let admitted = advance (begin_ initial) (Canonical after) in
  let included = advance admitted (update_step update) in
  finish included;
  included, handle, update
;;

let%expect_test
    "ordered inherited edges require one receipt and keep correction/reset distinct"
  =
  let initial, scope, reference = inherited_fixture "ordered_edges" in
  let first, handle, tracking = admit initial.ledger in
  let update =
    record_attempt
      initial
      scope
      reference
      ~after:first
      ~handle
      ~tracking
      ~revision:1L
      ~prefix:1L
  in
  let admitted = advance (begin_ initial) (Canonical first) in
  assert (Result.is_error (Cursor.check admitted ~incoming:(Cursor.current admitted)));
  let corrected = put first handle 0L (actual_views 7L) in
  assert (Result.is_error (Cursor.advance admitted (Canonical corrected)));
  let included = advance admitted (update_step update) in
  finish included;
  assert (Result.is_error (Cursor.advance included (update_step update)));
  let correction_only = advance included (Canonical corrected) in
  finish correction_only;
  let terminal_after_inclusion =
    advance included (Canonical (L.set_state first handle interrupted |> ledger))
  in
  let reset_after_inclusion = advance terminal_after_inclusion (Reset_generation 1) in
  List.iter [ included; correction_only; reset_after_inclusion ] ~f:(fun cursor ->
    Cursor.validate_publication cursor ~revision:1L |> protocol;
    List.iter [ 0L; 2L ] ~f:(fun revision ->
      assert (Result.is_error (Cursor.validate_publication cursor ~revision))));
  assert (Result.is_error (Cursor.advance correction_only (update_step update)));
  let terminal = L.set_state first handle interrupted |> ledger in
  let terminal_projection = { (Cursor.current included) with ledger = terminal } in
  let reset_only =
    advance (begin_ ~revision:1L terminal_projection) (Reset_generation 1)
  in
  finish reset_only;
  assert (Result.is_error (Cursor.advance reset_only (update_step update)));
  (* Both admissions belong to one candidate revision. The second actual edge is
     a host Turn, exercising the other dimension and its exact operation DATA. *)
  let operation = operation 42 in
  let second, turn_handle, tracking = L.admit_turn first operation |> ledger in
  let occurrence =
    Occurrence.host_turn (frame reference 1L 0) ~operation_id:operation.id |> protocol
  in
  let second_update =
    record
      (Cursor.current included)
      scope
      reference
      ~after:second
      ~canonical:(Host_turn (operation, turn_handle, tracking))
      ~occurrence
      ~prefix:2L
  in
  let missing_operation = advance included (Canonical second) in
  assert (Result.is_error (Cursor.advance missing_operation (update_step second_update)));
  let paired =
    advance included (Active_operation (Some operation))
    |> fun cursor ->
    advance cursor (Canonical second)
    |> fun cursor -> advance cursor (update_step second_update)
  in
  finish paired;
  Cursor.validate_publication paired ~revision:1L |> protocol;
  assert (Result.is_error (Cursor.validate_publication paired ~revision:2L));
  let latest =
    GD.local_included (Option.value_exn (Cursor.current paired).accounting) |> List.hd_exn
  in
  [%test_eq: int64] (Pending.next_prefix (Included.pending latest)) 2L;
  print_endline
    "unresolved, duplicate, correction, reset and intervening edges reject; paired \
     Attempt + Turn accept";
  [%expect
    {| unresolved, duplicate, correction, reset and intervening edges reject; paired Attempt + Turn accept |}]
;;

let%expect_test
    "checkpoint cannot invent inherited accounting or counts; maximum revision remains \
     readable"
  =
  let initial, scope, reference = inherited_fixture "ordered_checkpoint" in
  let maximum = begin_ ~revision:Int64.max_value initial in
  finish maximum;
  Cursor.validate_publication maximum ~revision:Int64.max_value |> protocol;
  Cursor.validate_checkpoint ~before:initial ~incoming:initial |> protocol;
  assert (Result.is_error (Cursor.expected_session_revision maximum));
  let absent = { initial with accounting = None } in
  assert (Result.is_error (Cursor.validate_checkpoint ~before:absent ~incoming:initial));
  assert (Result.is_error (Cursor.advance (begin_ absent) (Replacement initial)));
  let first, _, _ = admit initial.ledger in
  assert (
    Result.is_error
      (Cursor.validate_checkpoint
         ~before:initial
         ~incoming:{ initial with ledger = first }));
  let included, _, _ = first_included initial scope reference in
  assert (
    Result.is_error
      (Cursor.validate_checkpoint ~before:initial ~incoming:(Cursor.current included)));
  let reset = L.with_generation initial.ledger ~generation:1 |> ledger in
  Cursor.validate_checkpoint ~before:initial ~incoming:{ initial with ledger = reset }
  |> protocol;
  print_endline
    "checkpoint rejects component birth and inherited count growth; unchanged maximum \
     revision and count-free reset accept";
  [%expect
    {| checkpoint rejects component birth and inherited count growth; unchanged maximum revision and count-free reset accept |}]
;;

let moderator_receipt reference ~operation_id ~generation =
  let context : P.Moderator_execution.context =
    { id = P.Id.Moderator_execution.of_string "mex_ordered_association" |> protocol
    ; session_id = reference.Agent_store.Delegation_store.Reference.child_session_id
    ; generation
    ; source = (source ()).observer
    ; operation_id = Some operation_id
    ; job = None
    ; phase = Internal_event
    ; event = `Null
    ; checkpoint_sha256 = String.make 64 'a'
    ; created_at = timestamp
    }
  in
  P.Moderator_execution.create context |> protocol
;;

let%expect_test
    "untracked operation association uses the retained event at the actual canonical edge"
  =
  let initial, scope, reference = inherited_fixture "ordered_event" in
  let first, _, _ = first_included initial scope reference in
  let before = Cursor.current first in
  let operation = operation 51 in
  let after, handle, tracking =
    L.admit
      before.ledger
      ~source:(Transcript.Source_id.of_string "event-dispatch" |> Result.ok_or_failwith)
      ~relation:Root
      ~operation_id:(Some operation.id)
      ~invocation_id:None
      ~configuration
    |> ledger
  in
  assert (L.equal_tracking tracking (Untracked Attempt_count));
  let update =
    record_attempt before scope reference ~after ~handle ~tracking ~revision:2L ~prefix:2L
  in
  let cursor = begin_ ~revision:1L before in
  let admitted = advance cursor (Canonical after) in
  assert (Result.is_error (Cursor.advance admitted (update_step update)));
  let matching = moderator_receipt reference ~operation_id:operation.id ~generation:0 in
  let late = advance admitted (Moderator_execution matching) in
  assert (Result.is_error (Cursor.advance late (update_step update)));
  let replay receipt =
    advance cursor (Moderator_execution receipt)
    |> fun cursor ->
    advance cursor (Canonical after)
    |> fun cursor -> Cursor.advance cursor (update_step update)
  in
  let wrong_operation =
    moderator_receipt reference ~operation_id:(Fixture.operation 52).id ~generation:0
  in
  let wrong_generation =
    moderator_receipt reference ~operation_id:operation.id ~generation:1
  in
  let wrong_session =
    P.Moderator_execution.create { matching.context with session_id = sid } |> protocol
  in
  List.iter [ wrong_operation; wrong_generation; wrong_session ] ~f:(fun receipt ->
    assert (Result.is_error (replay receipt)));
  let interrupted =
    P.Moderator_execution.interrupt matching ~reason:"fixture retired" |> protocol
  in
  assert (Result.is_error (replay interrupted));
  replay matching |> protocol |> finish;
  let completed =
    P.Moderator_execution.complete
      matching
      ~checkpoint_sha256:(String.make 64 'b')
      ~requests:{ request_turn = true; request_compaction = false; end_session = None }
    |> protocol
  in
  replay completed |> protocol |> finish;
  let applied = P.Moderator_execution.apply_intent completed |> protocol in
  assert (Result.is_error (replay applied));
  let active state =
    advance cursor (Active_operation (Some { operation with state }))
    |> fun cursor ->
    advance cursor (Canonical after)
    |> fun cursor -> Cursor.advance cursor (update_step update)
  in
  active Running |> protocol |> finish;
  assert (Result.is_error (active Completed));
  print_endline
    "missing, late, wrong-operation/generation/session and retired event association \
     reject; retained Running/pending event or Running operation accept";
  [%expect
    {| missing, late, wrong-operation/generation/session and retired event association reject; retained Running/pending event or Running operation accept |}]
;;
