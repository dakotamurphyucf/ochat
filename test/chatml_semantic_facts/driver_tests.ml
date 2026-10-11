open Core
module R = Chatml_typechecker.Semantic
module B = Chatml_typechecker.Semantic_binding
module Contract = Chatml.Chatml_compiler_contract
module Static = Chatml.Chatml_static_surface
module Surface = Chatml.Chatml_builtin_surface
module Spec = Chatml.Chatml_builtin_spec
module Driver = Chatml_checked_graph

let empty_contract () =
  Contract.for_static_surface
    (Static.of_runtime_surface Surface.empty ~checkpoint:Support.checkpoint)
    ~required_bindings:[]
;;

let check contract prepared ?(recording = Support.limits ()) ~checkpoint () =
  Driver.check prepared ~recording ~contract ~max_dependency_depth:8 ~checkpoint
;;

let%test_unit
    "all actual recorded-driver checkpoints preserve callback exception identity"
  =
  let contract = empty_contract () in
  let prepared =
    Support.input
      contract
      [ "entry.chatml", "let identity x = x;; let result = identity(3)", [] ]
  in
  let polls = ref 0 in
  assert (Result.is_ok (check contract prepared ~checkpoint:(fun () -> incr polls) ()));
  assert (!polls > 0);
  let expected = Chatml_typechecker.Type_error "checkpoint origin, not a type failure" in
  for stop = 1 to !polls do
    let observed = ref 0 in
    match
      check
        contract
        prepared
        ~checkpoint:(fun () ->
          incr observed;
          if Int.equal !observed stop then raise expected)
        ()
    with
    | _ -> failwith "cancelled driver must not publish facts"
    | exception actual -> assert (phys_equal actual expected)
  done
;;

let%test_unit "metadata limits and invalid programs never return settled facts" =
  let contract = empty_contract () in
  let prepared = Support.input contract [ "entry.chatml", "let identity x = x", [] ] in
  List.iter
    [ Support.limits ~facts:1 ()
    ; Support.limits ~edges:1 ()
    ; Support.limits ~labels:1 ()
    ; Support.limits ~work:1 ()
    ]
    ~f:(fun recording ->
      match check contract prepared ~recording ~checkpoint:Support.checkpoint () with
      | Error failure ->
        (match Driver.Failure.kind failure with
         | Recording _ | Sites _ -> ()
         | Compiler _ -> failwith "expected metadata admission")
      | Ok _ -> failwith "bounded metadata unexpectedly admitted");
  List.iter [ "let ="; "let value = 1 + true" ] ~f:(fun body ->
    let prepared = Support.input contract [ "entry.chatml", body, [] ] in
    match check contract prepared ~checkpoint:Support.checkpoint () with
    | Error failure ->
      (match Driver.Failure.kind failure with
       | Compiler _ -> ()
       | Recording _ | Sites _ -> failwith "expected compiler error")
    | Ok _ -> failwith "invalid program cannot have settled facts")
;;

let%test_unit
    "authentic host requirements constrain selected roots without invoking native effects"
  =
  let effects = ref 0 in
  let native =
    { Spec.name = "touch"
    ; scheme = Spec.TFun ([ Spec.TInt ], Spec.TUnit)
    ; impl =
        (fun _ ->
          incr effects;
          Chatml.Chatml_lang.VUnit)
    }
  in
  let surface = { Surface.empty with globals = [ native ] } in
  let contract =
    Contract.for_static_surface
      (Static.of_runtime_surface surface ~checkpoint:Support.checkpoint)
      ~required_bindings:[ "chosen", Spec.TFun ([ Spec.TInt ], Spec.TInt) ]
  in
  let prepared =
    Support.input
      contract
      [ "entry.chatml", "let marker = touch(1);; let chosen x = x", [] ]
  in
  let facts = Support.facts contract prepared in
  assert (Int.equal !effects 0);
  let requirement = List.hd_exn (R.Settled.host_requirements facts) in
  let chosen =
    Fact_model.authored facts ~file:"entry.chatml" ~name:"chosen" ~kind:Value
    |> List.hd_exn
  in
  assert (String.equal requirement.name "chosen");
  assert (B.Symbol_id.equal requirement.binding (B.symbol chosen));
  assert (
    Chatml_source.Digest.equal
      requirement.contract
      (Contract.fingerprint contract ~checkpoint:Support.checkpoint));
  assert (not (Option.equal B.Root_id.equal (B.scheme chosen) (Some requirement.required)))
;;
