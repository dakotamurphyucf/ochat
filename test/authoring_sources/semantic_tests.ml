open Core
module S = Authoring_sources
module C = Authoring_corpus
module V = C.Coverage

let ok = Result.ok_or_failwith

let%expect_test
    "reviewed language semantics bind implementation and complete topic closures"
  =
  let sources = S.installed () |> ok in
  let corpus = C.runtime_foundation ~sources |> ok in
  let surfaces = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ] in
  let paths =
    List.concat_map V.semantic_features ~f:(fun feature -> feature.implementation_paths)
    |> List.dedup_and_sort ~compare:String.compare
  in
  [%test_eq: string list]
    (S.implementation_sources sources |> List.map ~f:(fun source -> source.S.path))
    paths;
  let targets = V.semantic_targets ~sources ~surface_ids:surfaces |> ok in
  let report = V.audit corpus ~targets ~mappings:V.semantic_mappings |> ok in
  V.require_complete report |> ok;
  (* Retaining binding and grammar mappings cannot hide an omitted semantic rule. *)
  let omitted = List.tl_exn V.semantic_mappings in
  let incomplete = V.audit corpus ~targets ~mappings:omitted |> ok in
  assert (Result.is_error (V.require_complete incomplete));
  print_s
    [%sexp
      (List.length V.semantic_features : int)
    , (List.length targets : int)
    , (List.length paths : int)
    , (List.map incomplete.missing ~f:(fun target -> target.V.id) : string list)];
  [%expect
    {|
    (30 120 7 (one_off_v1/semantics/lex.identifiers))
    |}]
;;
