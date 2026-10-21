open Core
module F = Fixture
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Session = Chatml_dependency_session
module Roots = Session.Roots
module Graph = Chatml_dependency_graph
module Check = Chatml_configured_check
module Parse = Chatml.Chatml_parse

let checkpoint () = ()

let id path =
  Captured.Id.create ~package_namespace:"graph-test" ~relative_path:path
  |> Or_error.ok_exn
;;

let capture path contents =
  Captured.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let fixture () =
  F.input
    [ ( "entry.chatml"
      , "let host = Good.value"
      , [ "Good", "good.chatml"; "Broken", "broken.chatml" ] )
    ; "good.chatml", "open Dep let value = x", [ "Dep", "dep.chatml" ]
    ; "dep.chatml", "let x = 3", []
    ; "broken.chatml", "let =", []
    ]
;;

let scope input =
  let anchor = id "entry.chatml" in
  Lookup.Repl_scope.create
    input
    ~anchor_owner:(Prepared.Module_id.of_implementation anchor)
    ~anchor_implementation:anchor
    ~limits:
      (Lookup.Repl_scope.Limits.create ~max_snippets:4 ~max_identity_bytes:256
       |> Or_error.ok_exn)
    ~checkpoint
  |> function
  | Ok scope -> scope
  | Error error -> raise_s [%sexp (error : Lookup.Repl_scope.Error.t)]
;;

let selected input =
  let scope = scope input in
  let source =
    Lookup.Repl_scope.for_submission
      scope
      (capture "snippet.chatml" "open Good")
      ~checkpoint
    |> function
    | Ok source -> source
    | Error error -> raise_s [%sexp (error : Lookup.Repl_scope.Error.t)]
  in
  let reference =
    Lookup.resolve
      source
      ~name:"Good"
      ~reason:Lookup.Reason.Open
      ~span:(F.span (Lookup.source source))
      ~checkpoint
    |> F.ok
  in
  scope, source, reference
;;

let limits ?(max_checked_modules = 4) ?(max_edges = 4) () =
  Roots.Limits.create ~max_checked_modules ~max_edges ~max_dependency_depth:4
  |> Or_error.ok_exn
;;

let checker ~checkpoint =
  let engine = Check.create Chatml.Chatml_builtin_surface.core_surface ~checkpoint in
  fun unit ~require ->
    let source = Session.Unit.implementation unit in
    let open Result.Let_syntax in
    let%bind parsed = Parse.parse_captured (Lookup.source source) ~checkpoint in
    Check.check_unit
      engine
      ~source
      ~require:(fun ~source reference ->
        Result.map (require ~source reference) ~f:Check.Checked_unit.exports)
      parsed
;;

let module_path id = Prepared.Module_id.implementation id |> Captured.Id.relative_path

let%expect_test "selected roots visit only actual transitive closure, in batch order" =
  let input = fixture () in
  let scope, source, reference = selected input in
  let initial = Roots.empty input ~limits:(limits ()) in
  let next, first_value =
    Roots.require_selected
      initial
      ~scope
      ~source
      reference
      ~checkpoint
      ~check:(checker ~checkpoint)
    |> function
    | Ok result -> result
    | Error error -> raise_s [%sexp (error : Roots.Error.t)]
  in
  let selected_order =
    Roots.checked_in_order next |> List.map ~f:(fun (id, _) -> module_path id)
  in
  let selected_edges =
    Roots.edges next
    |> List.map ~f:(fun edge ->
      module_path (Graph.Edge.owner edge), module_path (Graph.Edge.dependency edge))
  in
  let reused, reused_value =
    Roots.require_selected
      next
      ~scope
      ~source
      reference
      ~checkpoint
      ~check:(fun _ ~require:_ -> failwith "cached root was checked again")
    |> function
    | Ok result -> result
    | Error error -> raise_s [%sexp (error : Roots.Error.t)]
  in
  assert (phys_equal first_value reused_value);
  assert (
    List.equal
      (fun (left_id, _) (right_id, _) -> Prepared.Module_id.equal left_id right_id)
      (Roots.checked_in_order next)
      (Roots.checked_in_order reused));
  assert (
    List.equal
      (fun left right -> Int.equal (Graph.Edge.compare left right) 0)
      (Roots.edges next)
      (Roots.edges reused));
  let batch = F.checked input |> F.ok in
  let batch_order =
    Graph.dependency_order (Session.graph batch) |> List.map ~f:module_path
  in
  print_s
    [%sexp
      { selected_order : string list
      ; selected_edges : (string * string) list
      ; batch_order : string list
      }];
  [%expect
    {|
    ((selected_order (dep.chatml good.chatml))
     (selected_edges ((good.chatml dep.chatml) (good.chatml dep.chatml)))
     (batch_order (dep.chatml good.chatml entry.chatml)))
    |}]
;;

exception Cancelled

let%test_unit "limits and checkpoint cancellation return no completed cache" =
  let input = fixture () in
  let scope, source, reference = selected input in
  let exact, _ =
    Roots.require_selected
      (Roots.empty input ~limits:(limits ~max_checked_modules:2 ~max_edges:2 ()))
      ~scope
      ~source
      reference
      ~checkpoint
      ~check:(checker ~checkpoint)
    |> function
    | Ok result -> result
    | Error error -> raise_s [%sexp (error : Roots.Error.t)]
  in
  assert (List.length (Roots.checked_in_order exact) = 2);
  assert (List.length (Roots.edges exact) = 2);
  let initial = Roots.empty input ~limits:(limits ~max_checked_modules:1 ()) in
  (match
     Roots.require_selected
       initial
       ~scope
       ~source
       reference
       ~checkpoint
       ~check:(checker ~checkpoint)
   with
   | Error (Roots.Error.Failed_check diagnostic) ->
     assert (String.is_substring diagnostic.message ~substring:"limit")
   | _ -> failwith "module cap did not reject nested dependency");
  assert (List.is_empty (Roots.checked_in_order initial));
  let initial = Roots.empty input ~limits:(limits ~max_edges:0 ()) in
  (match
     Roots.require_selected
       initial
       ~scope
       ~source
       reference
       ~checkpoint
       ~check:(checker ~checkpoint)
   with
   | Error (Roots.Error.Failed_check diagnostic) ->
     assert (String.is_substring diagnostic.message ~substring:"limit")
   | _ -> failwith "edge cap did not reject nested dependency");
  assert (List.is_empty (Roots.checked_in_order initial));
  let total = ref 0 in
  let count () = Int.incr total in
  ignore
    (Roots.require_selected
       (Roots.empty input ~limits:(limits ()))
       ~scope
       ~source
       reference
       ~checkpoint:count
       ~check:(checker ~checkpoint:count)
     |> function
     | Ok result -> result
     | Error error -> raise_s [%sexp (error : Roots.Error.t)]);
  assert (!total > 9);
  List.iter
    [ 1; !total / 2; !total ]
    ~f:(fun stop ->
      let polls = ref 0 in
      let initial = Roots.empty input ~limits:(limits ()) in
      let poll () =
        Int.incr polls;
        if !polls = stop then raise Cancelled
      in
      let interrupted =
        try
          ignore
            (Roots.require_selected
               initial
               ~scope
               ~source
               reference
               ~checkpoint:poll
               ~check:(checker ~checkpoint:poll));
          false
        with
        | Cancelled -> true
      in
      assert interrupted;
      assert (!polls = stop);
      assert (List.is_empty (Roots.checked_in_order initial)))
;;

let%test_unit "revoked selection after checking cannot publish a completed cache" =
  let input = fixture () in
  let scope, source, reference = selected input in
  let initial = Roots.empty input ~limits:(limits ()) in
  let check = checker ~checkpoint in
  let revoke_after_check unit ~require =
    let result = check unit ~require in
    if String.equal (module_path (Session.Unit.id unit)) "good.chatml"
    then Lookup.Repl_scope.revoke scope;
    result
  in
  (match
     Roots.require_selected
       initial
       ~scope
       ~source
       reference
       ~checkpoint
       ~check:revoke_after_check
   with
   | Error (Roots.Error.Invalid_reference Lookup.Repl_scope.Error.Closed) -> ()
   | _ -> failwith "revoked selection published a completed cache");
  assert (List.is_empty (Roots.checked_in_order initial))
;;

let%test_unit
    "exact ordered seed rebinding preserves edges and rejects omitted or reordered nodes"
  =
  let input = fixture () in
  let scope, source, reference = selected input in
  let initial = Roots.empty input ~limits:(limits ()) in
  let checked, _ =
    Roots.require_selected
      initial
      ~scope
      ~source
      reference
      ~checkpoint
      ~check:(checker ~checkpoint)
    |> function
    | Ok result -> result
    | Error error -> raise_s [%sexp (error : Roots.Error.t)]
  in
  let values =
    Roots.checked_in_order checked |> List.map ~f:(fun (id, _) -> id, module_path id)
  in
  let rebound = Roots.rebind_checked checked ~checked:values ~checkpoint in
  assert (Result.is_ok rebound);
  let rebound =
    match rebound with
    | Ok rebound -> rebound
    | Error error -> raise_s [%sexp (error : Roots.Error.t)]
  in
  assert (
    List.equal
      (fun (left_id, left_value) (right_id, right_value) ->
         Prepared.Module_id.equal left_id right_id && String.equal left_value right_value)
      (Roots.checked_in_order rebound)
      values);
  assert (
    Result.is_error (Roots.rebind_checked checked ~checked:(List.rev values) ~checkpoint));
  assert (
    Result.is_error
      (Roots.rebind_checked checked ~checked:(List.tl_exn values) ~checkpoint));
  let other = fixture () in
  let foreign_scope, foreign_source, foreign_reference = selected other in
  let changed =
    F.input
      [ "entry.chatml", "let host = Good.value", [ "Good", "good.chatml" ]
      ; "good.chatml", "let value = 2", []
      ]
  in
  let different = Roots.empty changed ~limits:(limits ()) in
  match
    Roots.require_selected
      different
      ~scope:foreign_scope
      ~source:foreign_source
      foreign_reference
      ~checkpoint
      ~check:(checker ~checkpoint)
  with
  | Error Roots.Error.Different_input -> ()
  | _ -> failwith "foreign input selected a cached graph"
;;
