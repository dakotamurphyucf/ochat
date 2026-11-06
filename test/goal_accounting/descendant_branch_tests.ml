open! Core
open Fixture
module B = Agent_session.Goal_descendant_branch
module Pending = Agent_session.Goal_descendant_pending
module Included = Agent_session.Goal_descendant_included
module Occurrence = Agent_session.Goal_descendant_occurrence
module Origin = Agent_store.Delegation_goal_origin
module DS = Agent_store.Delegation_store
module Model = Descendant_occurrence_oracle

(* Validated DATA only. Neither this fixture nor a decoded receipt grants child
   execution, physical writer custody or authenticated ancestry. *)
let fixture () =
  let goal, episode = goal ~suffix:"descendant_branch" () in
  let child = P.Id.Session.of_string "ses_descendant_branch" |> protocol in
  let key = P.Idempotency_key.of_string "descendant-branch" |> protocol in
  let reference =
    DS.reference_of_jsonaf
      (`Object
          [ ( "key"
            , `Object
                [ "parent_session_id", P.Id.Session.to_json sid
                ; "parent_generation", `Number "0"
                ; "principal_id", P.Id.Principal.to_json principal
                ; "idempotency_key", P.Idempotency_key.to_json key
                ] )
          ; "child_session_id", P.Id.Session.to_json child
          ; "revision_id", `String "prv_descendant_branch"
          ; "request_sha256", `String (String.make 64 'a')
          ; "admission_sha256", `String (String.make 64 'b')
          ])
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    |> protocol
  in
  let branch =
    Origin.Anchor.create
      ~parent_session_id:sid
      ~parent_generation:0
      ~principal_id:principal
      ~key
      ~child_session_id:child
      ~transaction_id:(P.Id.Transaction.of_string "txn_descendant_branch" |> protocol)
    |> protocol
  in
  let scope =
    Origin.Scope.create
      ~owner:session
      ~goal_id:(G.id goal)
      ~source:(G.source goal)
      ~creator_episode:episode
      ~branch
    |> protocol
  in
  let leaf =
    P.Session_ref.create ~session_id:child ~server_id:(P.Session_ref.server_id session)
  in
  let occurrence revision =
    Occurrence.Frame.create
      ~session:leaf
      ~generation:0
      ~candidate_session_revision:revision
    |> protocol
    |> fun frame ->
    Occurrence.attempt frame ~ordinal:revision ~operation_id:None ~invocation_id:None
    |> protocol
  in
  scope, reference, occurrence
;;

let reserve branch occurrence =
  match B.reserve branch ~occurrence |> protocol with
  | Reserved (candidate, pending) -> candidate, pending
  | Already_pending _ | Busy _ -> failwith "expected fresh reservation"
;;

let member raw key =
  match raw with
  | `Object fields -> List.Assoc.find_exn fields key ~equal:String.equal
  | _ -> failwith "expected fixture object"
;;

let set raw key value =
  match raw with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal key value)
  | _ -> failwith "expected fixture object"
;;

let%expect_test "exact branch actions conserve occurrence sets and consume closed nonces" =
  let scope, reference, occurrence = fixture () in
  let initial = B.create ~scope ~reference |> protocol in
  let model = Model.create ~paths:[ "leaf", [ "root" ] ] ~unknown_edges:[] in
  let semantic revision : Model.Occurrence.t =
    { leaf = "leaf"
    ; generation = 0
    ; candidate_revision = revision
    ; kind = Attempt revision
    }
  in
  let verify branch model =
    let total = Model.totals model ~root:"root" ~dimension:Attempt in
    [%test_eq: int64] (B.confirmed_prefix branch) total.committed;
    [%test_eq: int64] (if Option.is_some (B.pending branch) then 1L else 0L) total.pending;
    assert (B.equal branch (B.to_json branch |> B.of_json |> protocol))
  in
  verify initial model;
  let first, p1 = reserve initial (occurrence 1L) in
  let issued = Model.record_fresh_nonce Int64.Set.empty (Pending.nonce p1) in
  let model = Model.record model ~occurrence:(semantic 1L) ~status:Pending in
  verify first model;
  (match B.reserve first ~occurrence:(occurrence 1L) |> protocol with
   | Already_pending replay -> assert (Pending.equal p1 replay)
   | Reserved _ | Busy _ -> failwith "exact pending replay changed");
  (match B.reserve first ~occurrence:(occurrence 2L) |> protocol with
   | Busy current -> assert (Pending.equal p1 current)
   | Reserved _ | Already_pending _ ->
     failwith "competing occurrence escaped serialization");
  let included1 = Included.create p1 in
  let confirmed = B.confirm first ~included:included1 |> protocol in
  B.validate_update first ~incoming:confirmed ~action:(Confirm included1) |> protocol;
  assert (
    Result.is_error
      (B.validate_update first ~incoming:confirmed ~action:(Close_before_writer p1)));
  let before = Model.totals model ~root:"root" ~dimension:Attempt in
  let model =
    Model.record model ~occurrence:(semantic 1L) ~status:(Included { input_tokens = 7L })
  in
  assert (
    Int64.equal
      (Model.occupied before)
      (Model.occupied (Model.totals model ~root:"root" ~dimension:Attempt)));
  verify confirmed model;
  let second, p2 = reserve confirmed (occurrence 2L) in
  let issued = Model.record_fresh_nonce issued (Pending.nonce p2) in
  let model = Model.record model ~occurrence:(semantic 2L) ~status:Pending in
  assert (B.equal second (B.confirm second ~included:included1 |> protocol));
  assert (Result.is_error (B.close_before_writer second ~pending:p1));
  verify second model;
  let closed = B.close_before_writer second ~pending:p2 |> protocol in
  let model = Model.record model ~occurrence:(semantic 2L) ~status:No_writer in
  verify closed model;
  let third, p3 = reserve closed (occurrence 3L) in
  let issued = Model.record_fresh_nonce issued (Pending.nonce p3) in
  [%test_eq: int] (Set.length issued) 3;
  [%test_eq: int64] (Pending.next_prefix p2) (Pending.next_prefix p3);
  assert (Int64.(Pending.nonce p3 > Pending.nonce p2));
  assert (B.equal third (B.close_before_writer third ~pending:p2 |> protocol));
  assert (Result.is_error (B.confirm third ~included:(Included.create p2)));
  let model = Model.record model ~occurrence:(semantic 3L) ~status:Pending in
  verify third model;
  let final = B.confirm third ~included:(Included.create p3) |> protocol in
  let model =
    Model.record model ~occurrence:(semantic 3L) ~status:(Included { input_tokens = 0L })
  in
  verify final model;
  [%test_eq: int64] (B.confirmed_prefix final) 2L;
  [%test_eq: int64] (B.last_nonce final) 3L;
  print_endline
    "one set member per inclusion; no-writer consumes nonce; old replay keeps new pending";
  [%expect
    {| one set member per inclusion; no-writer consumes nonce; old replay keeps new pending |}]
;;

let%expect_test "branch codec validates frontier relations and rejects counter exhaustion"
  =
  let scope, reference, occurrence = fixture () in
  let initial = B.create ~scope ~reference |> protocol in
  let pending_branch, pending = reserve initial (occurrence 1L) in
  assert (
    Result.is_error
      (Pending.create
         ~scope
         ~branch_reference:reference
         ~nonce:1L
         ~next_prefix:2L
         ~occurrence:(occurrence 1L)));
  assert (
    Result.is_error
      (Pending.of_json (set (Pending.to_json pending) "next_prefix" (`String "2"))));
  let complete =
    B.confirm pending_branch ~included:(Included.create pending) |> protocol
  in
  List.iter
    [ set (B.to_json complete) "confirmed_prefix" (`String "2")
    ; set (B.to_json complete) "last_nonce" (`String "2")
    ; set (B.to_json pending_branch) "pending" `Null
    ; set (B.to_json complete) "last" `Null
    ]
    ~f:(fun invalid -> assert (Result.is_error (B.of_json invalid)));
  let maximum =
    Pending.create
      ~scope
      ~branch_reference:reference
      ~nonce:Int64.max_value
      ~next_prefix:1L
      ~occurrence:(occurrence 1L)
    |> protocol
  in
  let exhausted =
    B.to_json complete
    |> fun raw ->
    set raw "last_nonce" (`String (Int64.to_string Int64.max_value))
    |> fun raw ->
    set
      raw
      "last"
      (`Object [ "kind", `String "included"; "pending", Pending.to_json maximum ])
    |> B.of_json
    |> protocol
  in
  assert (Result.is_error (B.reserve exhausted ~occurrence:(occurrence 2L)));
  let raw = B.to_json initial in
  let raw = set raw "future" (`String (String.make (256 * 1024) 'x')) in
  assert (Result.is_error (B.of_json raw));
  print_endline
    "orphaned/mismatched frontier rejects; max nonce does not wrap; raw preflight bounds \
     unknowns";
  [%expect
    {| orphaned/mismatched frontier rejects; max nonce does not wrap; raw preflight bounds unknowns |}]
;;
