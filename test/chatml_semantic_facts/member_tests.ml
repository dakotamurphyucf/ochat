open Core
module R = Chatml_typechecker.Semantic
module B = Chatml_typechecker.Semantic_binding
module Model = Fact_model
module S = Chatml_source

let reference facts body suffix shift =
  Model.reference_at
    facts
    ~file:"entry.chatml"
    ~offset:(String.substr_index_exn body ~pattern:suffix + shift)
;;

let%test_unit
    "configured nested opens reuse actual member identities and declaration-order \
     closure targets"
  =
  let contract = Support.contract () in
  let body =
    {|open Catalog;; open Nested;; let closure () = item;;
              let item = "shadow";; let result = closure();;
              let qualified = Catalog.Nested.item|}
  in
  let prepared =
    Support.input
      contract
      [ "entry.chatml", body, [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "module Nested = struct let item = 7 end;; let hidden = 9", []
      ]
  in
  let facts = Support.facts contract prepared in
  let imported =
    Model.authored facts ~file:"catalog.chatml" ~name:"item" ~kind:Value |> List.hd_exn
  in
  let closure_use = reference facts body "() = item" 5 in
  let qualified_use = reference facts body "Nested.item" 7 in
  assert (B.Symbol_id.equal (Model.target_binding closure_use) (B.symbol imported));
  assert (B.Symbol_id.equal (Model.target_binding qualified_use) (B.symbol imported));
  let shadow =
    Model.authored facts ~file:"entry.chatml" ~name:"item" ~kind:Value |> List.hd_exn
  in
  assert (not (B.Symbol_id.equal (B.symbol imported) (B.symbol shadow)));
  let exports =
    List.find_exn (R.Settled.exports facts) ~f:(fun exports ->
      String.equal (S.Id.relative_path exports.R.Fact.source) "entry.chatml")
  in
  let names = List.map (B.Members.values exports.members) ~f:fst in
  assert (not (List.mem names "Nested" ~equal:String.equal));
  assert (not (List.mem names "hidden" ~equal:String.equal))
;;

let%expect_test "companion selection links interface claims to implementation definitions"
  =
  let contract = Support.contract () in
  let prepared =
    Support.input
      contract
      ~interfaces:[ "catalog.chatml", "val identity : fun('a) -> 'a", [] ]
      [ ( "entry.chatml"
        , "let result = Catalog.identity(3)"
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", "let identity x = x;; let private_value = 7", []
      ]
  in
  let facts = Support.facts contract prepared in
  let actual =
    Model.authored facts ~file:"catalog.chatml" ~name:"identity" ~kind:Value
    |> List.hd_exn
  in
  let specification =
    Model.authored facts ~file:"catalog.chatmli" ~name:"identity" ~kind:Value
    |> List.hd_exn
  in
  let link = List.hd_exn (R.Settled.signature_links facts) in
  assert (B.Symbol_id.equal link.implementation (B.symbol actual));
  assert (B.Symbol_id.equal link.specification (B.symbol specification));
  let exports =
    List.find_exn (R.Settled.exports facts) ~f:(fun exports ->
      String.equal (S.Id.relative_path exports.R.Fact.source) "catalog.chatml")
  in
  let public =
    B.Members.find
      exports.members
      ~namespace:Value
      ~name:"identity"
      ~checkpoint:Support.checkpoint
    |> Option.value_exn
  in
  assert (B.Symbol_id.equal (B.symbol public) (B.symbol actual));
  assert (Option.equal B.Root_id.equal (B.scheme public) (B.scheme specification));
  assert (
    Option.is_none
      (B.Members.find
         exports.members
         ~namespace:Value
         ~name:"private_value"
         ~checkpoint:Support.checkpoint));
  print_s
    [%sexp
      { implementation = ("catalog.chatml" : string)
      ; specification = ("catalog.chatmli" : string)
      ; public_members =
          (List.map (B.Members.values exports.members) ~f:fst : string list)
      ; same_implementation_identity = (true : bool)
      }];
  [%expect
    {|
    ((implementation catalog.chatml) (specification catalog.chatmli)
     (public_members (identity)) (same_implementation_identity true))
    |}]
;;

let%test_unit
    "inline nested signatures restrict metadata, structural labels have no nominal target"
  =
  let contract = Support.contract () in
  let body =
    {|module Public : sig module Nested : sig val item : int end end = struct
                module Nested = struct let item = 3;; let hidden = 4 end
              end;; let result = Public.Nested.item;;
              let record = { value = `Tag(2) };; let copied = { record with value = `Tag(3) };;
              let result2 = match copied.value with | `Tag(x) -> x|}
  in
  let facts =
    Support.facts contract (Support.input contract [ "entry.chatml", body, [] ])
  in
  assert (Int.equal (List.length (R.Settled.signature_links facts)) 2);
  let structural =
    List.filter (R.Settled.references facts) ~f:(fun reference ->
      match reference.R.Fact.target with
      | Structural_label -> true
      | Binding _ | Configured_module _ | Unlocated_module_member | Language_type _ ->
        false)
  in
  assert (Int.equal (List.length structural) 6)
;;

let%test_unit
    "native static layered module rows retain actual host paths and shared scheme \
     quantifiers"
  =
  let module Spec = Chatml.Chatml_builtin_spec in
  let module Surface = Chatml.Chatml_builtin_surface in
  (* A real supported compile-only static contract. Native metadata is an
     assertion; this fixture does not claim its installer honors a module value. *)
  let module_scheme =
    Spec.TModule
      (TRow_extend
         ( [ "first", TArray (TVar "element") ]
         , TRow_extend ([ "second", TArray (TVar "element"); "first", TInt ], TRow_empty)
         ))
  in
  let surface =
    { Surface.empty with
      globals =
        [ { Spec.name = "Dynamic"
          ; scheme = module_scheme
          ; impl = (fun _ -> Chatml.Chatml_lang.VUnit)
          }
        ]
    }
  in
  let static =
    Chatml.Chatml_static_surface.of_runtime_surface surface ~checkpoint:Support.checkpoint
  in
  let contract =
    Chatml.Chatml_compiler_contract.for_static_surface static ~required_bindings:[]
  in
  let body = "open Dynamic;; let first_use = first;; let second_use = second" in
  let facts =
    Support.facts contract (Support.input contract [ "entry.chatml", body, [] ])
  in
  let whole =
    List.find_exn (R.Settled.bindings facts) ~f:(fun binding ->
      String.equal (B.name binding) "Dynamic")
  in
  let imported name =
    List.find_exn (R.Settled.bindings facts) ~f:(fun binding ->
      String.equal (B.name binding) name
      &&
      match B.definition binding with
      | Host { member_path; _ } -> List.equal String.equal member_path [ "Dynamic"; name ]
      | Authored _ | Implicit_specification_parameter _ -> false)
  in
  let first = imported "first"
  and second = imported "second" in
  assert (Option.is_some (B.members whole));
  let first_definitions =
    List.count (R.Settled.bindings facts) ~f:(fun binding ->
      match B.definition binding with
      | Host { member_path; _ } ->
        List.equal String.equal member_path [ "Dynamic"; "first" ]
      | Authored _ | Implicit_specification_parameter _ -> false)
  in
  assert (Int.equal first_definitions 1);
  List.iter [ first; second ] ~f:(fun binding ->
    assert (Option.equal B.Group_id.equal (B.group whole) (B.group binding)));
  let use = reference facts body "second_use = second" 13 in
  assert (B.Symbol_id.equal (Model.target_binding use) (B.symbol second));
  assert (
    Option.equal
      B.Symbol_id.equal
      (Model.resolve facts use.context ~namespace:Value ~name:"first")
      (Some (B.symbol first)));
  let module F = Chatml_typechecker.Type_snapshot in
  let groups = R.Settled.groups facts in
  let frozen =
    F.freeze
      (List.map groups ~f:R.Settled.Group.input)
      ~limits:
        (F.Limits.create ~max_nodes:100_000 ~max_edges:100_000 ~max_label_bytes:1_000_000
         |> Or_error.ok_exn)
      ~checkpoint:Support.checkpoint
    |> function
    | Ok frozen -> frozen
    | Error _ -> failwith "valid compiler-emitted graph"
  in
  let mapping =
    List.concat
      (List.map2_exn groups (F.roots frozen) ~f:(fun group roots ->
         List.zip_exn (R.Settled.Group.roots group) roots))
  in
  let child binding =
    let private_root = B.scheme binding |> Option.value_exn in
    let root = List.Assoc.find_exn mapping private_root ~equal:B.Root_id.equal in
    match F.find frozen root with
    | Some (Array child) -> child
    | _ -> failwith "actual array member projection"
  in
  match F.find frozen (child first), F.find frozen (child second) with
  | Some (Variable (Generalized first)), Some (Variable (Generalized second)) ->
    assert (F.Quantifier_id.equal first.quantifier second.quantifier)
  | _ -> failwith "actual shared scheme quantifier"
;;
