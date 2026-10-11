open Core
module R = Chatml_typechecker.Semantic
module S = Chatml_source
module P = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Parse = Chatml.Chatml_parse
module Inferred = Chatml_typechecker

let unit recorder prepared =
  let source = P.find_source prepared (P.entry prepared) |> Option.value_exn in
  let context =
    Lookup.create
      prepared
      ~owner:(P.Module_id.of_implementation (P.entry prepared))
      ~source:(S.id source)
      ~checkpoint:Support.checkpoint
    |> Result.map_error ~f:(fun diagnostic -> diagnostic.Chatml_diagnostic.message)
    |> Result.ok_or_failwith
  in
  let limits = R.site_limits recorder ~checkpoint:Support.checkpoint |> R.take_exn in
  let parsed =
    match
      Parse.parse_captured_with_sites source ~limits ~checkpoint:Support.checkpoint
    with
    | Ok parsed -> parsed
    | Error _ -> failwith "valid fixture parse"
  in
  R.implementation recorder context parsed ~checkpoint:Support.checkpoint
;;

let%test_unit "identical source cannot borrow another admitted lookup scope" =
  let contract = Support.contract () in
  let input target =
    Support.input
      contract
      [ "entry.chatml", "let answer = 1", [ "Chosen", target ]
      ; "a.chatml", "let item = 1", []
      ; "b.chatml", "let item = 2", []
      ]
  in
  let original = input "a.chatml" in
  let foreign = input "b.chatml" in
  let recorder = R.create original ~limits:(Support.limits ()) in
  (match unit recorder foreign with
   | Error Foreign_unit -> ()
   | Ok _ | Error _ -> failwith "expected foreign Prepared affinity rejection");
  assert (Result.is_error (R.settle recorder ~checkpoint:Support.checkpoint))
;;

let%test_unit "host module metadata admits limits before deep descent" =
  let contract = Support.contract () in
  let prepared = Support.input contract [ "entry.chatml", "let answer = 1", [] ] in
  let recorder = R.create prepared ~limits:(Support.limits ~facts:40 ()) in
  let result =
    R.with_recording recorder ~checkpoint:Support.checkpoint ~f:(fun ~checkpoint ->
      let unit = unit recorder prepared |> R.take_exn in
      let frame = R.frame recorder unit ~checkpoint |> R.take_exn in
      let nested =
        List.fold (List.init 20_000 ~f:Fn.id) ~init:Inferred.TInt ~f:(fun inferred _ ->
          Inferred.Module (Row (Inferred.Env.singleton "Child" inferred, Empty_row)))
      in
      Chatml_typechecker.Semantic_host.values_exn
        frame
        (Chatml_typechecker.Value_scope.of_schemes
           (Inferred.Env.singleton "Catalog" nested))
        ~globals:[]
        ~modules:[ "Catalog", nested ]
        ~checkpoint)
  in
  (match result with
   | Error Fact_limit -> ()
   | Ok _ | Error _ -> failwith "expected bounded root admission before descent");
  assert (Result.is_error (R.settle recorder ~checkpoint:Support.checkpoint))
;;
