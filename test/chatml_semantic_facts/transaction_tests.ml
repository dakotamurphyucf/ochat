open Core
module R = Chatml_typechecker.Semantic
module B = Chatml_typechecker.Semantic_binding
module T = Chatml_typechecker
module P = Chatml_prepared_program
module S = Chatml_source
module Lookup = Chatml_configured_lookup
module Parse = Chatml.Chatml_parse

let transaction ~discard_at =
  let contract = Support.contract () in
  let prepared =
    Support.input
      contract
      ~interfaces:[ "entry.chatml", "val storage : int array", [] ]
      [ "entry.chatml", "let storage = []", [] ]
  in
  let recorder = R.create prepared ~limits:(Support.limits ()) in
  let implementation = Admission_tests.unit recorder prepared |> R.take_exn in
  let source_id = P.find_source prepared (P.entry prepared) |> Option.value_exn |> S.id in
  let interface_id =
    S.Id.create
      ~package_namespace:(S.Id.package_namespace source_id)
      ~relative_path:"entry.chatmli"
    |> Or_error.ok_exn
  in
  let interface_source = P.find_source prepared interface_id |> Option.value_exn in
  let context =
    Lookup.create
      prepared
      ~owner:(P.Module_id.of_implementation source_id)
      ~source:interface_id
      ~checkpoint:Support.checkpoint
    |> Result.map_error ~f:(fun diagnostic -> diagnostic.Chatml_diagnostic.message)
    |> Result.ok_or_failwith
  in
  let limits = R.site_limits recorder ~checkpoint:Support.checkpoint |> R.take_exn in
  let interface =
    match
      Parse.parse_captured_interface_with_sites
        interface_source
        ~limits
        ~checkpoint:Support.checkpoint
    with
    | Ok parsed -> parsed
    | Error _ -> failwith "valid interface"
  in
  let unit =
    R.interface recorder context interface ~checkpoint:Support.checkpoint |> R.take_exn
  in
  let frame = R.frame recorder unit ~checkpoint:Support.checkpoint |> R.take_exn in
  let polls = ref 0 in
  let checkpoint () =
    incr polls;
    if Option.value_map discard_at ~default:false ~f:(Int.equal !polls)
    then R.discard recorder
  in
  let state = T.create_state ~source:interface_source ~checkpoint () in
  let weak = T.new_var state 0 in
  let before =
    match weak with
    | T.Var cell -> !cell
    | _ -> assert false
  in
  let group, roots =
    R.group recorder [ T.Array weak ] ~checkpoint:Support.checkpoint |> R.take_exn
  in
  let storage_span =
    S.locate
      (R.source implementation)
      ~start_byte:4
      ~end_byte:11
      ~checkpoint:Support.checkpoint
    |> Or_error.ok_exn
    |> S.Location.span
  in
  let storage_location =
    R.authored
      recorder
      implementation
      ~role:Value_binder
      ~span:storage_span
      ~checkpoint:Support.checkpoint
    |> R.take_exn
    |> Option.value_exn
  in
  let binding =
    R.define
      recorder
      implementation
      ~name:"storage"
      ~kind:Value
      ~definition:(B.Definition.Authored storage_location)
      ~scheme:(Some (group, List.hd_exn roots))
      ~members:None
      ~checkpoint:Support.checkpoint
    |> R.take_exn
  in
  let members =
    R.members
      recorder
      ~values:[ "storage", binding ]
      ~types:[]
      ~checkpoint:Support.checkpoint
    |> R.take_exn
  in
  let state = { state with semantic = Some frame } in
  let outcome =
    R.with_recording recorder ~checkpoint ~f:(fun ~checkpoint:_ ->
      T.check_captured_module_signature
        ~semantic_values:T.Value_scope.empty
        ~semantic_members:members
        state
        interface
        ~values:(T.Env.singleton "storage" (T.Array weak))
        ~types:T.Type_alias.empty
        ~outer_types:T.Type_alias.empty)
  in
  let unchanged =
    match weak with
    | T.Var cell -> phys_equal !cell before
    | _ -> false
  in
  !polls, outcome, unchanged, recorder
;;

let%test_unit
    "discard at the final probe callback cannot commit weak cells or activate facts"
  =
  let polls, baseline, unchanged, _ = transaction ~discard_at:None in
  assert (polls > 0);
  assert (not unchanged);
  assert (Result.is_ok baseline);
  let observed, rejected, unchanged, recorder = transaction ~discard_at:(Some polls) in
  assert (Int.equal observed polls);
  (match rejected with
   | Error Failed_invocation -> ()
   | Ok _ | Error _ -> failwith "expected recording rejection before original writes");
  assert unchanged;
  assert (Result.is_error (R.settle recorder ~checkpoint:Support.checkpoint))
;;
