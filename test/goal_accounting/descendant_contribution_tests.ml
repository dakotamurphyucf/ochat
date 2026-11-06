open! Core
open Fixture
module Children = Agent_session.Child_contributions
module Snapshot = Agent_session.Child_usage_snapshot
module Origin = Agent_store.Delegation_goal_origin
module DS = Agent_store.Delegation_store
module Marks = Agent_session.Canonical_usage_fold.Marks

(* Pure immutable accounting DATA, not Actor observations or delegated authority.
   The expected count is the cardinality of disjoint child labels below. *)
let reference name =
  DS.reference_of_jsonaf
    (`Object
        [ ( "key"
          , `Object
              [ "parent_session_id", P.Id.Session.to_json sid
              ; "parent_generation", `Number "0"
              ; "principal_id", P.Id.Principal.to_json principal
              ; "idempotency_key", `String name
              ] )
        ; "child_session_id", `String ("ses_" ^ name)
        ; "revision_id", `String ("prv_" ^ name)
        ; "request_sha256", `String (String.make 64 'a')
        ; "admission_sha256", `String (String.make 64 'b')
        ])
  |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  |> protocol
;;

let scope reference =
  let goal, creator_episode = goal () in
  let branch =
    Origin.Anchor.create
      ~parent_session_id:sid
      ~parent_generation:0
      ~principal_id:principal
      ~key:reference.DS.Reference.key.idempotency_key
      ~child_session_id:reference.child_session_id
      ~transaction_id:(P.Id.Transaction.of_string "txn_scoped" |> protocol)
    |> protocol
  in
  Origin.Scope.create
    ~owner:session
    ~goal_id:(G.id goal)
    ~source:(source ())
    ~creator_episode
    ~branch
  |> protocol
;;

let origin scope = Origin.associated ~max_scopes:1 [ scope ] |> protocol

let one =
  let usage =
    U.add_untracked U.empty
    |> Result.map_error ~f:(fun _ -> "count")
    |> Result.ok_or_failwith
  in
  Snapshot.Partition.create
    ~usage
    ~marks:
      { Marks.attempt = 1L; turn = 0L; untracked_attempts = 1L; untracked_turns = 0L }
  |> protocol
;;

let empty_descendants =
  Snapshot.Descendants.create
    ~partition:Snapshot.Partition.empty
    ~coverage:Snapshot.Coverage.complete
;;

let canonical_coverage =
  P.Inference_query.Coverage.create
    ~before_tracking_unknown:false
    ~retired_attempts:0L
    ~untracked_attempts:1L
    ~retired_turns:0L
    ~untracked_turns:0L
    ~tracking_status:Available
  |> protocol
;;

let snapshot reference scope =
  let projected =
    Snapshot.Goal_scope.create
      ~scope
      ~own:one
      ~descendants:empty_descendants
      ~included:None
    |> protocol
  in
  Snapshot.create_with_scopes
    ~reference
    ~revision:1L
    ~own:one
    ~descendants:empty_descendants
    ~canonical_coverage
    ~origin:(origin scope)
    ~goal_scopes:[ projected ]
  |> protocol
;;

let empty () =
  Children.create
    ~inventory:Complete_inventory
    ~max_children:4
    ~max_encoded_bytes:(4 * 1024 * 1024)
  |> protocol
;;

let attempts aggregate =
  Snapshot.Descendants.partition aggregate |> Snapshot.Partition.usage |> U.attempts
;;

let root_total children =
  let goal, _ = goal () in
  Children.aggregate_root children ~owner:session ~goal_id:(G.id goal) |> protocol
;;

let%expect_test
    "one Goal sums separate first-child branches and excludes unrelated unknown usage"
  =
  let labels = String.Set.of_list [ "left"; "right" ] in
  let children =
    Set.fold labels ~init:(empty ()) ~f:(fun children label ->
      let reference = reference label in
      let scope = scope reference in
      let children =
        Children.register_with_origin children ~reference ~origin:(origin scope)
        |> protocol
      in
      Children.replace children ~snapshot:(snapshot reference scope) |> protocol |> fst)
  in
  let children =
    Children.register_with_origin
      children
      ~reference:(reference "unrelated")
      ~origin:Origin.unrelated
    |> protocol
  in
  let total = root_total children in
  assert (Int64.equal (attempts total) (Int64.of_int (Set.length labels)));
  assert (
    Int64.equal
      (Snapshot.Coverage.unobserved_children (Snapshot.Descendants.coverage total))
      0L);
  assert (
    not
      (Snapshot.Coverage.historical_children_unknown
         (Snapshot.Descendants.coverage total)));
  let whole = Children.aggregate children |> protocol in
  assert (
    Int64.equal
      (Snapshot.Coverage.unobserved_children (Snapshot.Descendants.coverage whole))
      1L);
  let historical =
    Children.register children ~reference:(reference "historical") |> protocol
  in
  let total = root_total historical in
  assert (Int64.equal (attempts total) 2L);
  assert (
    Int64.equal
      (Snapshot.Coverage.unobserved_children (Snapshot.Descendants.coverage total))
      1L);
  print_endline
    "two disjoint branches count twice; unrelated unobserved child contributes no \
     uncertainty; missing historical association stays unknown";
  [%expect
    {| two disjoint branches count twice; unrelated unobserved child contributes no uncertainty; missing historical association stays unknown |}]
;;

let%expect_test
    "slot origin and scoped snapshot presence remain immutable across replacement and \
     codec"
  =
  let reference = reference "scoped" in
  let scope = scope reference in
  let origin = origin scope in
  let children =
    Children.register_with_origin (empty ()) ~reference ~origin |> protocol
  in
  assert (Result.is_error (Children.register children ~reference));
  assert (
    Result.is_error
      (Children.register_with_origin children ~reference ~origin:Origin.unrelated));
  let legacy =
    Snapshot.create
      ~reference
      ~revision:1L
      ~own:one
      ~descendants:empty_descendants
      ~canonical_coverage
    |> protocol
  in
  assert (Result.is_error (Children.replace children ~snapshot:legacy));
  let children, _ =
    Children.replace children ~snapshot:(snapshot reference scope) |> protocol
  in
  let raw = Children.to_jsonaf children |> protocol in
  let unmarked =
    match raw with
    | `Object fields ->
      `Object (List.Assoc.add fields ~equal:String.equal "required_semantics" (`Array []))
    | _ -> failwith "expected child contribution document"
  in
  assert (Result.is_error (Children.of_jsonaf unmarked));
  let restored = Children.of_jsonaf raw |> protocol in
  assert (D.Json.equal raw (Children.to_jsonaf restored |> protocol));
  assert (Int64.equal (attempts (root_total restored)) 1L);
  let historical = Children.register (empty ()) ~reference |> protocol in
  assert (
    Result.is_error (Children.replace historical ~snapshot:(snapshot reference scope)));
  let old_snapshot_digest = Snapshot.digest legacy in
  let historical, _ = Children.replace historical ~snapshot:legacy |> protocol in
  let old_document =
    match Children.to_jsonaf historical |> protocol with
    | `Object fields ->
      `Object
        (List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "1")
         |> fun fields ->
         List.Assoc.add fields ~equal:String.equal "future_number" (`Number "1e+00"))
    | _ -> failwith "expected historical child contribution document"
  in
  let recovered = Children.of_jsonaf old_document |> protocol in
  assert (Option.is_none (Children.origin recovered ~reference |> protocol));
  let preserved = Children.to_jsonaf recovered |> protocol in
  (match D.Json.field preserved ~name:"future_number" with
   | Value (`Number spelling) -> assert (String.equal spelling "1e+00")
   | _ -> failwith "historical unknown number changed");
  let field raw name =
    match D.Json.field raw ~name with
    | Value value -> value
    | Absent | Null -> failwith name
  in
  (match field (field preserved "payload") "children" with
   | `Array [ child ] ->
     assert (D.Json.equal (field child "snapshot") (Snapshot.to_json legacy));
     assert (
       String.equal
         old_snapshot_digest
         (Snapshot.of_json (field child "snapshot") |> protocol |> Snapshot.digest));
     (match D.Json.field child ~name:"goal_origin" with
      | Absent -> ()
      | Null | Value _ -> failwith "historical origin was populated")
   | _ -> failwith "expected one historical child");
  print_endline
    "origin cannot be added, erased or changed; scoped slots reject historical \
     snapshots; exact scoped state round-trips";
  [%expect
    {| origin cannot be added, erased or changed; scoped slots reject historical snapshots; exact scoped state round-trips |}]
;;
