open Core
module B = Chatml_project_build
module S = Chatml_source
module Contract = Chatml.Chatml_compiler_contract

let%test_unit "all observed compose and restore checkpoints preserve callback identity" =
  (* Compact authentic native metadata keeps the all-checkpoint sweep bounded;
     the main model independently covers the actual standard target contract. *)
  let surface =
    Chatml.Chatml_static_surface.of_runtime_surface
      Chatml.Chatml_builtin_surface.empty
      ~checkpoint:ignore
  in
  let contract = Contract.for_static_surface surface ~required_bindings:[] in
  let fixture = Fixtures.make ~contract () in
  let expected = Fixtures.compose fixture |> Support.ok in
  Support.assert_checkpoint_identity ~f:(fun checkpoint ->
    let actual = Fixtures.compose ~checkpoint fixture |> Support.ok in
    Support.assert_same_source (B.lock_source expected) (B.lock_source actual));
  Support.assert_checkpoint_identity ~f:(fun checkpoint ->
    let actual =
      B.restore
        (B.lock_source expected)
        ~sources:(Support.without_lock expected)
        ~contract
        ~limits:(Support.limits ())
        ~checkpoint
      |> Support.ok
    in
    Support.assert_same_source (B.lock_source expected) (B.lock_source actual))
;;

let%test_unit "public work source metadata and document limits reject without admission" =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let bytes =
    Chatml_prepared_program.Accounting.input_bytes
      (Chatml_prepared_program.accounting (B.prepared original))
  in
  let at_limit =
    Fixtures.compose ~limits:(Support.limits ~bytes ()) fixture |> Support.ok
  in
  Support.assert_same_source (B.lock_source original) (B.lock_source at_limit);
  ignore
    (Fixtures.compose ~limits:(Support.limits ~bytes:(bytes - 1) ()) fixture
     |> Support.reject
     : B.Error.t);
  let tiny_work =
    Fixtures.compose ~limits:(Support.limits ~steps:1 ()) fixture |> Support.reject
  in
  (match B.Error.problem tiny_work with
   | Limit_exceeded -> ()
   | _ -> failwith "work limit classification");
  let document =
    Document_schema.Limits.create ~max_bytes:16 ~max_depth:10 ~max_fields:20 ~max_nodes:30
    |> function
    | Ok value -> value
    | Error _ -> failwith "valid tiny limits rejected"
  in
  ignore
    (Fixtures.compose ~limits:(Support.limits ~document ()) fixture |> Support.reject
     : B.Error.t);
  ignore
    (B.restore
       (B.lock_source original)
       ~sources:(Support.without_lock original)
       ~contract:(Fixtures.contract fixture)
       ~limits:(Support.limits ~bytes:1 ())
       ~checkpoint:ignore
     |> Support.reject
     : B.Error.t);
  let wrong = Contract.for_target Tool_v1 ~checkpoint:ignore in
  ignore
    (B.restore
       (B.lock_source original)
       ~sources:(Support.without_lock original)
       ~contract:wrong
       ~limits:(Support.limits ())
       ~checkpoint:ignore
     |> Support.reject
     : B.Error.t);
  assert (
    S.Id.equal
      (Chatml_project.Lock.root (B.lock original))
      (S.id (Fixtures.declaration fixture)))
;;
