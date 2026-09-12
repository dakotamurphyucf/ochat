open Core
module S = Authoring_sources
module C = Authoring_corpus
module V = C.Coverage

let ok = Result.ok_or_failwith

let%expect_test "declaration coverage accounts for sources and rejects missing mappings" =
  let sources = S.installed () |> ok in
  let corpus = C.runtime_foundation ~sources |> ok in
  let paths =
    V.semantic_features @ V.declaration_features
    |> List.concat_map ~f:(fun feature -> feature.implementation_paths)
    |> List.dedup_and_sort ~compare:String.compare
  in
  [%test_eq: string list]
    (S.implementation_sources sources |> List.map ~f:(fun source -> source.S.path))
    paths;
  let targets =
    V.declaration_targets
      ~sources
      ~surface_ids:[ "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    |> ok
  in
  V.audit corpus ~targets ~mappings:V.declaration_mappings
  |> ok
  |> V.require_complete
  |> ok;
  let missing =
    V.audit corpus ~targets ~mappings:(List.tl_exn V.declaration_mappings) |> ok
  in
  assert (Result.is_error (V.require_complete missing));
  assert (Result.is_error (V.declaration_targets ~sources ~surface_ids:[ "one_off_v1" ]));
  print_s
    [%sexp
      (List.length V.declaration_features : int)
    , (List.length targets : int)
    , (List.length paths : int)];
  [%expect {| (14 42 27) |}]
;;
