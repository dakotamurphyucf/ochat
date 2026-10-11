open Core
module B = Chatml_project_build
module P = Chatml_project
module S = Chatml_source
module R = Chatml_prepared_program

let%expect_test "each package owns its available scope; aliases share one module" =
  let fixture = Fixtures.make () in
  let build = Fixtures.compose fixture |> Support.ok in
  Fixtures.assert_model fixture build;
  let show label source =
    let targets = Fixtures.scope_targets (B.prepared build) (S.id source) in
    printf
      "%s: %s\n"
      label
      (String.concat
         ~sep:", "
         (List.map targets ~f:(fun (name, namespace, path) ->
            sprintf "%s=%s/%s" name namespace path)))
  in
  show "root entry" (Fixtures.main fixture);
  show "root isolated helper" (Fixtures.helper fixture);
  show "dependency implementation" (Fixtures.util fixture);
  printf "tools: %s\n" (String.concat ~sep:", " (P.Lock.required_tools (B.lock build)));
  [%expect
    {| 
    root entry: Again=lib/src/util.chatml, Imported=lib/src/util.chatml, Local=root/src/main.chatml
    root isolated helper:
    dependency implementation: Local=lib/src/util.chatml
    tools: dep.read, root.write, shared.read |}]
;;

let%test_unit "canonical input permutations and exact entry overlap preserve identity" =
  let fixture = Fixtures.make () in
  let expected = Fixtures.compose fixture |> Support.ok in
  let lib = List.hd_exn (Fixtures.catalogs fixture) in
  let root = Fixtures.root fixture in
  let orders = [ [ lib; root; lib ]; [ root; lib; lib ]; [ lib; lib; root ] ] in
  [%test_eq: int] (List.length orders) 3;
  List.iter orders ~f:(fun catalogs ->
    let actual = Fixtures.compose ~catalogs fixture |> Support.ok in
    Fixtures.assert_model fixture actual;
    Support.assert_same_source (B.lock_source expected) (B.lock_source actual);
    assert (
      S.Digest.equal
        (R.fingerprint (B.prepared expected))
        (R.fingerprint (B.prepared actual))));
  let reversed = Fixtures.make ~reverse_sources:true () in
  let actual = Fixtures.compose reversed |> Support.ok in
  Support.assert_same_source (B.lock_source expected) (B.lock_source actual);
  assert (
    S.Digest.equal
      (R.fingerprint (B.prepared expected))
      (R.fingerprint (B.prepared actual)))
;;

let%test_unit
    "declared entry outside discovery and explicit captured entry own their scopes"
  =
  let outside = Fixtures.make ~outside_entry:true () in
  let declared = Fixtures.compose outside |> Support.ok in
  Fixtures.assert_model outside declared;
  assert (
    Option.is_none
      (R.find_definition
         (B.prepared declared)
         (R.Module_id.of_implementation (R.entry (B.prepared declared)))));
  let library = Fixtures.make ~library:true () in
  let host = Support.capture ~namespace:"host" "src/helper.chatml" "let main = 4" in
  let captured = Fixtures.compose ~entry:(Captured host) library |> Support.ok in
  assert (S.Id.equal (R.entry (B.prepared captured)) (S.id host));
  [%test_eq: int] (List.length (R.sources (B.prepared captured))) 8;
  [%test_eq: (string * string * string) list]
    (Fixtures.scope_targets (B.prepared captured) (S.id host))
    [ "Again", "lib", "src/util.chatml"
    ; "Imported", "lib", "src/util.chatml"
    ; "Local", "root", "src/main.chatml"
    ];
  let overlap =
    Fixtures.compose ~entry:(Captured (Fixtures.main library)) library |> Support.ok
  in
  [%test_eq: int] (List.length (R.sources (B.prepared overlap))) 7;
  let changed =
    Support.capture ~namespace:"root" "src/main.chatml" "different revision"
  in
  let error = Fixtures.compose ~entry:(Captured changed) library |> Support.reject in
  (match B.Error.problem error with
   | Invalid_entry_selection -> ()
   | _ -> failwith "entry collision");
  let owner = B.Error.declared_at error |> Option.value_exn |> P.Declared_at.source in
  Support.assert_same_source (Fixtures.declaration library) owner;
  ignore
    (Fixtures.compose ~entry:(Captured (Fixtures.helper library)) library
     |> Support.reject
     : B.Error.t)
;;
