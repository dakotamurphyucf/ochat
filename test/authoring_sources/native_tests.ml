open Core
module S = Authoring_sources
module C = Authoring_corpus
module V = C.Coverage

let%expect_test "native operation semantics require reviewed source and topic mappings" =
  let sources = S.installed () |> Result.ok_or_failwith in
  let corpus = C.runtime_foundation ~sources |> Result.ok_or_failwith in
  let targets =
    V.native_targets
      ~sources
      ~surface_ids:[ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    |> Result.ok_or_failwith
  in
  V.audit corpus ~targets ~mappings:V.native_mappings
  |> Result.bind ~f:V.require_complete
  |> Result.ok_or_failwith;
  let missing =
    V.audit corpus ~targets ~mappings:(List.tl_exn V.native_mappings)
    |> Result.ok_or_failwith
  in
  assert (Result.is_error (V.require_complete missing));
  let changed =
    List.mapi targets ~f:(fun i target ->
      match i with
      | 0 ->
        { target with
          V.contract_sha256 = Chatmd_shell_spec.Source_ref.digest "changed implementation"
        }
      | _ -> target)
  in
  assert (Result.is_error (V.audit corpus ~targets:changed ~mappings:V.native_mappings));
  let paths =
    V.native_features
    |> List.concat_map ~f:(fun feature -> feature.implementation_paths)
    |> List.dedup_and_sort ~compare:String.compare
  in
  print_s
    [%sexp
      (List.length V.native_features : int)
    , (List.length targets : int)
    , (List.length paths : int)];
  [%expect {| (17 68 35) |}]
;;
