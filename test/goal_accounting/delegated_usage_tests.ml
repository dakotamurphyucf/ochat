open! Core
open Fixture
module F = Agent_session.Canonical_usage_fold
module Own = Agent_session.Delegated_usage
module Children = Agent_session.Child_contributions
module Snapshot = Agent_session.Child_usage_snapshot
module DS = Agent_store.Delegation_store

(* Explicit protocol DATA for these pure accounting/codec laws. No opaque Actor
   observation or delegation/execution permission is manufactured by decoding. *)
let reference =
  DS.reference_of_jsonaf
    (`Object
        [ ( "key"
          , `Object
              [ "parent_session_id", `String "ses_accounting_parent"
              ; "parent_generation", `Number "0"
              ; "principal_id", P.Id.Principal.to_json principal
              ; "idempotency_key", `String "accounting-child"
              ] )
        ; "child_session_id", P.Id.Session.to_json sid
        ; "revision_id", `String "prv_accounting_child"
        ; "request_sha256", `String (String.make 64 'a')
        ; "admission_sha256", `String (String.make 64 'b')
        ])
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let fold result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (F.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let step own before after = Own.reconcile own ~before ~after |> protocol

let input own ledger =
  Own.usage own ~ledger |> protocol |> fun usage -> U.component usage Input
;;

let input_tokens own ledger = U.Metric.actual (input own ledger)

let snapshot own current revision =
  Snapshot.create
    ~reference
    ~revision
    ~own:
      (Snapshot.Partition.create
         ~usage:(Own.usage own ~ledger:current |> protocol)
         ~marks:(F.Marks.of_ledger current |> fold)
       |> protocol)
    ~descendants:
      (Snapshot.Descendants.create
         ~partition:Snapshot.Partition.empty
         ~coverage:Snapshot.Coverage.complete)
    ~canonical_coverage:(P.Inference_query.Summary.coverage (L.summary current))
  |> protocol
;;

let%expect_test
    "whole child replaces canonical observations and shares exact retirement fold"
  =
  let l0 = make_ledger ~attempts:1 () in
  let c0 = Own.create ~reference ~ledger:l0 |> protocol in
  let l1, h1, _ = admit l0 in
  let c1 = step c0 l0 l1 in
  let l2 = put l1 h1 0L (actual_views 7L) in
  let c2 = step c1 l1 l2 in
  let l3 = put l2 h1 1L (actual_views 11L) in
  let c3 = step c2 l2 l3 in
  assert (U.Sum.equal_sum (input_tokens c3 l3) (Tokens 11L));
  let l4 = put l3 h1 2L (actual_views 3L) in
  let c4 = step c3 l3 l4 in
  assert (U.Sum.equal_sum (input_tokens c4 l4) (Tokens 3L));
  let l5 = L.set_state l4 h1 interrupted |> ledger in
  let c5 = step c4 l4 l5 in
  let l6, _, _ = admit l5 in
  let c6 = step c5 l5 l6 in
  assert (Option.is_none (L.find l6 ~ordinal:(L.Handle.ordinal h1)));
  assert (U.Sum.equal_sum (input_tokens c6 l6) (Tokens 3L));
  assert (Int64.equal (U.attempts (Own.usage c6 ~ledger:l6 |> protocol)) 2L);
  assert (Result.is_error (Own.validate c5 ~ledger:l6));
  assert (Result.is_error (Own.validate_transition c5 ~incoming:c5 ~before:l5 ~after:l6));
  Own.validate_transition c5 ~incoming:c6 ~before:l5 ~after:l6 |> protocol;
  let l7, disposition =
    L.observe l6 h1 (usage h1 ~revision:3L (actual_views 99L)) |> ledger
  in
  assert (L.equal_observation_disposition disposition Ignored_retired);
  assert (U.Sum.equal_sum (input_tokens c6 l7) (Tokens 3L));
  assert (Result.is_error (Own.create ~reference ~ledger:l7));
  let l8, _, tracking = admit l7 in
  assert (L.equal_tracking tracking (Untracked Attempt_count));
  let c8 = step c6 l7 l8 in
  let totals = Own.usage c8 ~ledger:l8 |> protocol in
  assert (Int64.equal (U.attempts totals) 3L);
  assert (Int64.equal (U.untracked_attempts totals) 1L);
  List.iter components ~f:(fun component ->
    assert (Int64.equal (U.Metric.untracked_attempts (U.component totals component)) 1L));
  let marks = F.Marks.of_ledger l8 |> fold in
  assert (Int64.equal marks.attempt 3L && Int64.equal marks.untracked_attempts 1L);
  print_endline
    "7→11→3 replacement; exact3 folded once; missing fold rejects; late retired evidence \
     adds nothing; untracked counts once";
  [%expect
    {| 7→11→3 replacement; exact3 folded once; missing fold rejects; late retired evidence adds nothing; untracked counts once |}]
;;

let%expect_test "generation changes preserve cumulative whole child accounting" =
  let l0 = make_ledger () in
  let own = Own.create ~reference ~ledger:l0 |> protocol in
  let l1, h1, _ = admit l0 in
  let own = step own l0 l1 in
  let l2 = put l1 h1 0L (actual_views 7L) in
  let own = step own l1 l2 in
  let l3 = L.set_state l2 h1 interrupted |> ledger in
  let own = step own l2 l3 in
  let l4 = L.with_generation l3 ~generation:1 |> ledger in
  let own = step own l3 l4 in
  assert (U.Sum.equal_sum (input_tokens own l4) (Tokens 7L));
  let encoded = Own.to_document own |> protocol in
  let recovered = Own.of_document encoded |> protocol in
  Own.validate recovered ~ledger:l4 |> protocol;
  assert (
    U.equal
      (Own.usage recovered ~ledger:l4 |> protocol)
      (Own.usage own ~ledger:l4 |> protocol));
  assert (Result.is_error (Own.create ~reference ~ledger:l4));
  print_endline
    "actual interrupted row survives generation+codec; historical partition cannot reset";
  [%expect
    {| actual interrupted row survives generation+codec; historical partition cannot reset |}]
;;

let%expect_test "immediate child slots replace snapshots and retain explicit uncertainty" =
  let l0 = make_ledger () in
  let own = Own.create ~reference ~ledger:l0 |> protocol in
  let l1, handle, _ = admit l0 in
  let own = step own l0 l1 in
  let l2 = put l1 handle 0L (actual_views 7L) in
  let own2 = step own l1 l2 in
  let seven = snapshot own2 l2 2L in
  let l3 = put l2 handle 1L (actual_views 11L) in
  let own3 = step own2 l2 l3 in
  let eleven = snapshot own3 l3 3L in
  let l4 = put l3 handle 2L (actual_views 3L) in
  let three = snapshot (step own3 l3 l4) l4 4L in
  let parent =
    Children.create
      ~inventory:Complete_inventory
      ~max_children:1
      ~max_encoded_bytes:(256 * 1024)
    |> protocol
  in
  let parent = Children.register parent ~reference |> protocol in
  let before = Children.aggregate parent |> protocol in
  assert (
    Int64.equal
      (Snapshot.Coverage.unobserved_children (Snapshot.Descendants.coverage before))
      1L);
  let parent, added = Children.replace parent ~snapshot:seven |> protocol in
  assert (Children.equal_disposition added Added);
  let parent, replaced = Children.replace parent ~snapshot:eleven |> protocol in
  assert (Children.equal_disposition replaced Replaced);
  let eleven_parent = Children.to_jsonaf parent |> protocol in
  let parent, duplicate = Children.replace parent ~snapshot:eleven |> protocol in
  assert (Children.equal_disposition duplicate Duplicate);
  let parent, stale = Children.replace parent ~snapshot:seven |> protocol in
  assert (Children.equal_disposition stale Stale);
  assert (D.Json.equal eleven_parent (Children.to_jsonaf parent |> protocol));
  assert (Result.is_error (Children.replace parent ~snapshot:(snapshot own2 l2 3L)));
  let parent, _ = Children.replace parent ~snapshot:three |> protocol in
  let aggregate = Children.aggregate parent |> protocol in
  let usage = Snapshot.Partition.usage (Snapshot.Descendants.partition aggregate) in
  assert (U.Sum.equal_sum (U.Metric.actual (U.component usage Input)) (Tokens 3L));
  assert (Int64.equal (U.attempts usage) 1L);
  assert (
    Int64.equal
      (Snapshot.Coverage.unobserved_children (Snapshot.Descendants.coverage aggregate))
      0L);
  let historical =
    Children.create
      ~inventory:Historical_children_unknown
      ~max_children:1
      ~max_encoded_bytes:(256 * 1024)
    |> protocol
  in
  let historical = Children.register historical ~reference |> protocol in
  let historical, _ = Children.replace historical ~snapshot:three |> protocol in
  let historical = Children.aggregate historical |> protocol in
  assert (
    Snapshot.Coverage.historical_children_unknown
      (Snapshot.Descendants.coverage historical));
  print_endline
    "unobserved is not zero; 7→11→3 replaces; duplicate/stale unchanged; conflict \
     rejects; old history stays unknown";
  [%expect
    {| unobserved is not zero; 7→11→3 replaces; duplicate/stale unchanged; conflict rejects; old history stays unknown |}]
;;

(* Pure carrier laws share the canonical DATA fixture above. No decoded value
   is used as an Actor observation, installation proof or permission. *)
module Carrier_laws = struct
  let member json name =
    match D.Json.field json ~name with
    | Value value -> value
    | Absent | Null -> failwith name
  ;;

  let set json name value =
    match json with
    | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
    | _ -> failwith "expected fixture object"
  ;;

  let first json ~f =
    match json with
    | `Array (first :: rest) -> `Array (f first :: rest)
    | _ -> failwith "expected fixture row"
  ;;

  let inspect ~limits raw =
    D.Document.inspect raw ~limits
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  ;;

  let own_json own = Own.to_document own |> protocol |> D.Document.json
  let own_of_json raw = inspect ~limits:Own.limits raw |> Own.of_document |> protocol

  let pad raw ~bytes =
    let empty =
      set raw "payload" (set (member raw "payload") "future_padding" (`String ""))
    in
    let padding = bytes - String.length (Jsonaf.to_string empty) in
    assert (padding >= 0);
    let raw =
      set
        empty
        "payload"
        (set
           (member empty "payload")
           "future_padding"
           (`String (String.make padding 'x')))
    in
    [%test_eq: int] bytes (String.length (Jsonaf.to_string raw));
    raw
  ;;

  let other_reference =
    let raw = DS.reference_to_jsonaf reference in
    set
      (set raw "child_session_id" (`String "ses_charge_other"))
      "key"
      (set (member raw "key") "idempotency_key" (`String "accounting-other-child"))
    |> DS.reference_of_jsonaf
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
    |> Result.ok_or_failwith
  ;;

  (* Rebinding only the pure snapshot DATA constructs a second disjoint algebra
     fixture; actual import still requires the separately tested Actor association. *)
  let rebind value reference =
    Snapshot.to_json value
    |> fun raw ->
    set raw "reference" (DS.reference_to_jsonaf reference) |> Snapshot.of_json |> protocol
  ;;

  let create_children ~count =
    Children.create
      ~inventory:Complete_inventory
      ~max_children:count
      ~max_encoded_bytes:(256 * 1024)
    |> protocol
  ;;

  let assert_resource_limit = function
    | Error (error : P.Error.t) ->
      [%test_eq: P.Error.code] Resource_limit error.code;
      assert (not error.retryable)
    | Ok _ -> failwith "bounded child carrier admitted another slot"
  ;;
end

let%expect_test "whole-child carriers preserve future data and reserve bounded growth" =
  let open Carrier_laws in
  let l0 = make_ledger ~attempts:1 () in
  let own0 = Own.create ~reference ~ledger:l0 |> protocol in
  let l1, handle, _ = admit l0 in
  let own1 = step own0 l0 l1 in
  let l2 = put l1 handle 0L (actual_views 7L) in
  let own2 = step own1 l1 l2 in
  let l3 = L.set_state l2 handle interrupted |> ledger in
  let own3 = step own2 l2 l3 in
  let l4, _, _ = admit l3 in
  let future = `Object [ "retained", `True; "number_spelling", `Number "7e0" ] in
  let raw = own_json own3 in
  let payload = member raw "payload" in
  let settled = member payload "settled" in
  let raw =
    set
      (set raw "future_envelope" future)
      "payload"
      (set
         (set payload "future_partition" future)
         "settled"
         (set settled "future_settled" future))
  in
  let enriched = own_of_json raw in
  assert (Own.can_retire enriched |> protocol);
  let folded = step enriched l3 l4 in
  Own.validate_transition enriched ~incoming:folded ~before:l3 ~after:l4 |> protocol;
  let encoded = own_json folded in
  assert (D.Json.equal (member encoded "future_envelope") future);
  assert (D.Json.equal (member (member encoded "payload") "future_partition") future);
  assert (
    D.Json.equal
      (member (member (member encoded "payload") "settled") "future_settled")
      future);
  let plain = step own3 l3 l4 in
  assert (
    Result.is_error
      (Own.validate_transition enriched ~incoming:plain ~before:l3 ~after:l4));
  let conflict = own_of_json (set encoded "future_envelope" `False) in
  assert (Result.is_error (Own.adopt folded ~incoming:conflict));
  let own_growth =
    Own.reserved_growth own3 |> protocol |> Agent_session.Capacity_growth.encoded_bytes
  in
  assert (own_growth > 0);
  let too_full = pad (own_json own3) ~bytes:((256 * 1024) - own_growth + 1) in
  assert (Result.is_error (Own.of_document (inspect ~limits:Own.limits too_full)));
  let funded = pad (own_json own3) ~bytes:((256 * 1024) - own_growth) |> own_of_json in
  ignore (step funded l3 l4 : Own.t);
  let seven = snapshot own2 l2 2L in
  let children =
    create_children ~count:1 |> fun t -> Children.register t ~reference |> protocol
  in
  assert_resource_limit (Children.register children ~reference:other_reference);
  let two =
    create_children ~count:2
    |> fun t ->
    Children.register t ~reference
    |> protocol
    |> fun t -> Children.register t ~reference:other_reference |> protocol
  in
  let two_raw = Children.to_jsonaf two |> protocol in
  let too_many =
    set two_raw "payload" (set (member two_raw "payload") "max_children" (`Number "1"))
  in
  assert (Result.is_error (Children.of_jsonaf too_many));
  let children, _ = Children.replace children ~snapshot:seven |> protocol in
  let raw = Children.to_jsonaf children |> protocol in
  let payload = member raw "payload" in
  List.iter
    [ "max_children", `Number "2"; "max_encoded_bytes", `Number "262145" ]
    ~f:(fun (field, value) ->
      let resized =
        Children.of_jsonaf (set raw "payload" (set payload field value)) |> protocol
      in
      assert (Result.is_error (Children.adopt children ~incoming:resized)));
  let enriched_raw =
    set
      (set raw "future_envelope" future)
      "payload"
      (set
         (set payload "future_inventory" future)
         "children"
         (first (member payload "children") ~f:(fun slot ->
            set
              (set slot "future_slot" future)
              "snapshot"
              (set (member slot "snapshot") "future_snapshot" future))))
  in
  let enriched = Children.of_jsonaf enriched_raw |> protocol in
  let l5 = put l2 handle 1L (actual_views 11L) in
  let eleven = snapshot (step own2 l2 l5) l5 3L in
  let replaced, disposition = Children.replace enriched ~snapshot:eleven |> protocol in
  assert (Children.equal_disposition Replaced disposition);
  let encoded = Children.to_jsonaf replaced |> protocol in
  let payload = member encoded "payload" in
  assert (D.Json.equal (member encoded "future_envelope") future);
  assert (D.Json.equal (member payload "future_inventory") future);
  (match member payload "children" with
   | `Array [ slot ] ->
     assert (D.Json.equal (member slot "future_slot") future);
     assert (D.Json.equal (member (member slot "snapshot") "future_snapshot") future)
   | _ -> failwith "wrong retained child inventory");
  let conflict = Children.of_jsonaf (set encoded "future_envelope" `False) |> protocol in
  assert (Result.is_error (Children.adopt replaced ~incoming:conflict));
  let growth =
    Children.reserved_growth children
    |> protocol
    |> Agent_session.Capacity_growth.encoded_bytes
  in
  assert (growth > 0);
  assert (
    Result.is_error (Children.of_jsonaf (pad raw ~bytes:((256 * 1024) - growth + 1))));
  let funded = Children.of_jsonaf (pad raw ~bytes:((256 * 1024) - growth)) |> protocol in
  ignore
    (Children.replace funded ~snapshot:eleven |> protocol
     : Children.t * Children.disposition);
  (* Aggregation through actual contribution slots retains unknown and overflow
     labels; adding an unknown child cannot be mistaken for exact zero. *)
  List.iter [ `Unknown; `Overflow ] ~f:(fun case ->
    let views =
      match case with
      | `Unknown -> List.map components ~f:(fun c -> c, O.Count.Unknown Not_reported)
      | `Overflow -> actual_views Int64.max_value
    in
    let current = put l1 handle 0L views in
    let value = snapshot (step own1 l1 current) current 4L in
    let children, _ = Children.replace two ~snapshot:value |> protocol in
    let children, _ =
      Children.replace children ~snapshot:(rebind value other_reference) |> protocol
    in
    let aggregate = Children.aggregate children |> protocol in
    let metric =
      Snapshot.Partition.usage (Snapshot.Descendants.partition aggregate)
      |> fun usage -> U.component usage Input
    in
    match case with
    | `Unknown ->
      [%test_eq: int64] 2L (U.Unknown.count (U.Metric.unknown metric) Not_reported);
      [%test_eq: int64] 0L (U.Metric.actual_attempts metric)
    | `Overflow -> assert (U.Sum.equal_sum (U.Metric.actual metric) Overflow));
  print_endline
    "future fields survive folds/replacements; counts and exact reserved bytes bound \
     admission; unknown/overflow stay labelled";
  [%expect
    {| future fields survive folds/replacements; counts and exact reserved bytes bound admission; unknown/overflow stay labelled |}]
;;

module Scoped_snapshot_boundaries = struct
  module Shared = Descendant_contribution_tests
  module Origin = Agent_store.Delegation_goal_origin
  module Occurrence = Agent_session.Goal_descendant_occurrence
  module Pending = Agent_session.Goal_descendant_pending
  module Included = Agent_session.Goal_descendant_included
  module Growth = Agent_session.Capacity_growth
  open Carrier_laws

  let frame reference revision =
    let leaf =
      P.Session_ref.create
        ~session_id:reference.DS.Reference.child_session_id
        ~server_id:(P.Session_ref.server_id session)
    in
    Occurrence.Frame.create
      ~session:leaf
      ~generation:0
      ~candidate_session_revision:revision
    |> protocol
  ;;

  let pending scope reference ~nonce ~prefix occurrence =
    Pending.create
      ~scope
      ~branch_reference:reference
      ~nonce
      ~next_prefix:prefix
      ~occurrence
    |> protocol
  ;;

  let entry scope own included =
    Snapshot.Goal_scope.create ~scope ~own ~descendants:Shared.empty_descendants ~included
    |> protocol
  ;;

  let create_snapshot scope reference ~revision ~own ~canonical_coverage included =
    Snapshot.create_with_scopes
      ~reference
      ~revision
      ~own
      ~descendants:Shared.empty_descendants
      ~canonical_coverage
      ~origin:(Shared.origin scope)
      ~goal_scopes:[ entry scope own (Some included) ]
    |> protocol
  ;;

  let scope_row snapshot_json =
    match member snapshot_json "goal_scopes" with
    | `Array [ entry ] -> entry
    | _ -> failwith "expected one scoped boundary fixture"
  ;;

  let slot_snapshot child_json =
    match member (member child_json "payload") "children" with
    | `Array [ slot ] -> member slot "snapshot"
    | _ -> failwith "expected one child boundary fixture"
  ;;

  let map_slot_snapshot child_json ~f =
    let payload = member child_json "payload" in
    set
      child_json
      "payload"
      (set
         payload
         "children"
         (first (member payload "children") ~f:(fun slot ->
            set slot "snapshot" (f (member slot "snapshot")))))
  ;;
end

let%expect_test "same scoped prefix preserves exact receipt and both count partitions" =
  let open Scoped_snapshot_boundaries in
  let reference = Shared.reference "receipt_boundary" in
  let scope = Shared.scope reference in
  let occurrence ordinal =
    Occurrence.attempt
      (frame reference ordinal)
      ~ordinal
      ~operation_id:None
      ~invocation_id:None
    |> protocol
  in
  let first = pending scope reference ~nonce:1L ~prefix:1L (occurrence 1L) in
  let included = Included.create first in
  let before = entry scope Shared.one (Some included) in
  Snapshot.Goal_scope.validate_update before ~incoming:before |> protocol;
  let two = Snapshot.Partition.add_disjoint Shared.one Shared.one |> protocol in
  let more_counts = entry scope two (Some included) in
  assert (
    Result.is_error (Snapshot.Goal_scope.validate_update before ~incoming:more_counts));
  let more_descendants =
    Snapshot.Goal_scope.create
      ~scope
      ~own:Shared.one
      ~descendants:
        (Snapshot.Descendants.create
           ~partition:Shared.one
           ~coverage:Snapshot.Coverage.complete)
      ~included:(Some included)
    |> protocol
  in
  assert (
    Result.is_error
      (Snapshot.Goal_scope.validate_update before ~incoming:more_descendants));
  let changed_nonce =
    pending scope reference ~nonce:2L ~prefix:1L (occurrence 1L)
    |> Included.create
    |> fun included -> entry scope Shared.one (Some included)
  in
  assert (
    Result.is_error (Snapshot.Goal_scope.validate_update before ~incoming:changed_nonce));
  let changed_occurrence =
    pending scope reference ~nonce:1L ~prefix:1L (occurrence 2L)
    |> Included.create
    |> fun included -> entry scope Shared.one (Some included)
  in
  assert (
    Result.is_error
      (Snapshot.Goal_scope.validate_update before ~incoming:changed_occurrence));
  let no_receipt = entry scope Shared.one None in
  assert (
    Result.is_error (Snapshot.Goal_scope.validate_update before ~incoming:no_receipt));
  let advanced =
    pending scope reference ~nonce:3L ~prefix:2L (occurrence 2L)
    |> Included.create
    |> fun included -> entry scope two (Some included)
  in
  Snapshot.Goal_scope.validate_update before ~incoming:advanced |> protocol;
  let same_nonce_before =
    pending scope reference ~nonce:3L ~prefix:1L (occurrence 1L)
    |> Included.create
    |> fun included -> entry scope Shared.one (Some included)
  in
  assert (
    Result.is_error
      (Snapshot.Goal_scope.validate_update same_nonce_before ~incoming:advanced));
  let raw = Pending.to_json first in
  assert (
    Result.is_error (Pending.of_json (Carrier_laws.set raw "next_prefix" (`String "0"))));
  print_endline
    "same prefix preserves receipt and counts; later prefix consumes a later nonce; \
     decoded zero rejects";
  [%expect
    {| same prefix preserves receipt and counts; later prefix consumes a later nonce; decoded zero rejects |}]
;;

let%expect_test
    "scoped Host_turn capacity retains nested unknowns and protects kind replacement"
  =
  let open Scoped_snapshot_boundaries in
  let open Carrier_laws in
  let reference = Shared.reference "host_capacity" in
  let scope = Shared.scope reference in
  let origin = Shared.origin scope in
  let host =
    Occurrence.host_turn
      (frame reference 1L)
      ~operation_id:(P.Id.Operation.of_string "op_host_capacity" |> protocol)
    |> protocol
    |> pending scope reference ~nonce:1L ~prefix:1L
    |> Included.create
  in
  let own =
    Snapshot.Partition.create
      ~usage:U.empty
      ~marks:
        { F.Marks.attempt = 0L; turn = 1L; untracked_attempts = 0L; untracked_turns = 1L }
    |> protocol
  in
  let coverage attempts =
    P.Inference_query.Coverage.create
      ~before_tracking_unknown:false
      ~retired_attempts:0L
      ~untracked_attempts:attempts
      ~retired_turns:0L
      ~untracked_turns:1L
      ~tracking_status:Available
    |> protocol
  in
  let before =
    create_snapshot
      scope
      reference
      ~revision:1L
      ~own
      ~canonical_coverage:(coverage 0L)
      host
  in
  let clean_extra =
    Agent_session.Child_usage_capacity.extra_growth
      ~origin
      ~snapshot:(Some before)
      ~raw_snapshot:(Some (Snapshot.to_json before))
    |> protocol
  in
  assert (Growth.encoded_bytes clean_extra > 0);
  let children =
    Children.register_with_origin (Shared.empty ()) ~reference ~origin
    |> protocol
    |> fun children -> Children.replace children ~snapshot:before |> protocol |> fst
  in
  let future = `Object [ "number_spelling", `Number "7e0"; "retained", `True ] in
  let enriched_raw =
    Children.to_jsonaf children
    |> protocol
    |> map_slot_snapshot ~f:(fun snapshot ->
      set
        snapshot
        "goal_scopes"
        (first (member snapshot "goal_scopes") ~f:(fun entry ->
           let scope = member entry "scope" in
           let episode = member scope "creator_episode" in
           let admission = member episode "admission" in
           let scope =
             set
               (set scope "source" (set (member scope "source") "future_source" future))
               "creator_episode"
               (set
                  episode
                  "admission"
                  (set
                     admission
                     "source"
                     (set (member admission "source") "future_source" future)))
           in
           let included = member entry "included" in
           set
             (set entry "scope" scope)
             "included"
             (set
                included
                "occurrence"
                (set (member included "occurrence") "future_kind" future)))))
  in
  let protected_extra =
    Agent_session.Child_usage_capacity.extra_growth
      ~origin
      ~snapshot:(Some before)
      ~raw_snapshot:(Some (slot_snapshot enriched_raw))
    |> protocol
  in
  [%test_eq: int] 0 (Growth.encoded_bytes protected_extra);
  let enriched = Children.of_jsonaf enriched_raw |> protocol in
  let correction =
    create_snapshot
      scope
      reference
      ~revision:2L
      ~own
      ~canonical_coverage:(coverage 0L)
      host
  in
  let corrected, _ = Children.replace enriched ~snapshot:correction |> protocol in
  let retained = Children.to_jsonaf corrected |> protocol |> slot_snapshot |> scope_row in
  let scope_raw = member retained "scope" in
  assert (D.Json.equal future (member (member scope_raw "source") "future_source"));
  assert (
    D.Json.equal
      future
      (member
         (member (member (member scope_raw "creator_episode") "admission") "source")
         "future_source"));
  assert (
    D.Json.equal
      future
      (member (member (member retained "included") "occurrence") "future_kind"));
  let attempt =
    Occurrence.attempt
      (frame reference 3L)
      ~ordinal:1L
      ~operation_id:None
      ~invocation_id:None
    |> protocol
    |> pending scope reference ~nonce:2L ~prefix:2L
    |> Included.create
  in
  let with_attempt = Snapshot.Partition.add_disjoint own Shared.one |> protocol in
  let next =
    create_snapshot
      scope
      reference
      ~revision:3L
      ~own:with_attempt
      ~canonical_coverage:(coverage 1L)
      attempt
  in
  assert (Result.is_error (Children.replace corrected ~snapshot:next));
  print_endline
    "clean Host_turn funds Attempt growth; protected kind remains readable and \
     correctable; nested number lexemes survive; incompatible replacement rejects";
  [%expect
    {| clean Host_turn funds Attempt growth; protected kind remains readable and correctable; nested number lexemes survive; incompatible replacement rejects |}]
;;
