open Core
module R = Chatml_typechecker.Semantic
module B = Chatml_typechecker.Semantic_binding
module Model = Fact_model

let facts body =
  let contract = Support.contract () in
  Support.facts contract (Support.input contract [ "entry.chatml", body, [] ])
;;

let reference facts body suffix shift =
  let offset = String.substr_index_exn body ~pattern:suffix + shift in
  Model.reference_at facts ~file:"entry.chatml" ~offset
;;

let one facts name kind =
  Model.authored facts ~file:"entry.chatml" ~name ~kind |> List.hd_exn
;;

let%test_unit "independent value/type deltas preserve earlier bindings in both orders" =
  List.iter
    [ {|type chosen = int;; let old = 1;; type later = chosen;;
        let old = "new";; let use : later = 2;; let result = old|}
    ; {|let old = 1;; type chosen = int;; let old = "new";;
        type later = chosen;; let use : later = 2;; let result = old|}
    ]
    ~f:(fun body ->
      let facts = facts body in
      let chosen = one facts "chosen" Type_alias in
      let later = one facts "later" Type_alias in
      let olds = Model.authored facts ~file:"entry.chatml" ~name:"old" ~kind:Value in
      assert (Int.equal (List.length olds) 2);
      let second_old = List.last_exn olds in
      let old_use = reference facts body "result = old" 9 in
      assert (B.Symbol_id.equal (Model.target_binding old_use) (B.symbol second_old));
      List.iter
        [ B.Namespace.Value, "old", second_old
        ; Type, "chosen", chosen
        ; Type, "later", later
        ]
        ~f:(fun (namespace, name, expected) ->
          assert (
            Option.equal
              B.Symbol_id.equal
              (Model.resolve facts old_use.context ~namespace ~name)
              (Some (B.symbol expected))));
      let alias_use = reference facts body "later = chosen" 8 in
      assert (B.Symbol_id.equal (Model.target_binding alias_use) (B.symbol chosen)))
;;

let%test_unit
    "whole RHS projections share one boundary, separate recursive schemes do not"
  =
  let body =
    {|let (|first, second|) = (|fun x -> x, fun y -> y|);;
              let rec left x = x and right y = y|}
  in
  let facts = facts body in
  let first = one facts "first" Value
  and second = one facts "second" Value in
  let left = one facts "left" Value
  and right = one facts "right" Value in
  assert (Option.equal B.Group_id.equal (B.group first) (B.group second));
  assert (not (Option.equal B.Group_id.equal (B.group left) (B.group right)));
  assert (not (Option.equal B.Root_id.equal (B.scheme first) (B.scheme second)))
;;

let%test_unit
    "original recursive alias and same-spelling parameter keep distinct identities"
  =
  let body =
    {|type 'a a = [ `Leaf('a) | `Branch('a a, 'a a) ];;
              let keep : fun(int a) -> int a = fun value -> value|}
  in
  let facts = facts body in
  let alias = one facts "a" Type_alias in
  let parameter = one facts "a" Type_parameter in
  assert (not (B.Symbol_id.equal (B.symbol alias) (B.symbol parameter)));
  assert (Option.equal B.Group_id.equal (B.group alias) (B.group parameter));
  let original = List.hd_exn (R.Settled.aliases facts) in
  assert (B.Symbol_id.equal original.binding (B.symbol alias));
  assert (List.equal B.Symbol_id.equal original.parameters [ B.symbol parameter ]);
  let parameter_use = reference facts body "Leaf('a)" 5 in
  assert (B.Symbol_id.equal (Model.target_binding parameter_use) (B.symbol parameter));
  let alias_use = reference facts body "'a a," 3 in
  assert (B.Symbol_id.equal (Model.target_binding alias_use) (B.symbol alias));
  let groups =
    List.map (R.Settled.groups facts) ~f:Chatml_typechecker.Semantic.Settled.Group.input
  in
  let limits =
    Chatml_typechecker.Type_snapshot.Limits.create
      ~max_nodes:100_000
      ~max_edges:100_000
      ~max_label_bytes:1_000_000
    |> Or_error.ok_exn
  in
  assert (
    Result.is_ok
      (Chatml_typechecker.Type_snapshot.freeze
         groups
         ~limits
         ~checkpoint:Support.checkpoint))
;;

let%test_unit "nested parameters preserve type declarations without seeing later shadows" =
  let body =
    {|type chosen = int;; let outer = 1;;
              let closure : fun(chosen) -> int = fun x -> x + outer;;
              type later = chosen;; let outer = "later";; let result = closure(3)|}
  in
  let facts = facts body in
  let chosen = one facts "chosen" Type_alias in
  let first_outer =
    Model.authored facts ~file:"entry.chatml" ~name:"outer" ~kind:Value |> List.hd_exn
  in
  let use = reference facts body "x + outer" 4 in
  assert (B.Symbol_id.equal (Model.target_binding use) (B.symbol first_outer));
  assert (
    Option.equal
      B.Symbol_id.equal
      (Model.resolve facts use.context ~namespace:Type ~name:"chosen")
      (Some (B.symbol chosen)));
  let x = one facts "x" Parameter in
  assert (
    Option.equal
      B.Symbol_id.equal
      (Model.resolve facts use.context ~namespace:Value ~name:"x")
      (Some (B.symbol x)))
;;

let%test_unit "statement boundary contexts expose a nonrecursive name only after its RHS" =
  let body = "let value = 1;; let result = value" in
  let facts = facts body in
  let binding = one facts "value" Value in
  let point offset =
    List.find_exn (R.Settled.regions facts) ~f:(fun region ->
      Option.is_none region.R.Fact.inferred
      && Int.equal (Model.offset region.location) offset
      && Int.equal (Chatml_source.Location.span region.location).right.offset offset)
  in
  let before = point 0
  and after = point 13 in
  assert (
    Option.is_none (Model.resolve facts before.context ~namespace:Value ~name:"value"));
  assert (
    Option.equal
      B.Symbol_id.equal
      (Model.resolve facts after.context ~namespace:Value ~name:"value")
      (Some (B.symbol binding)))
;;
