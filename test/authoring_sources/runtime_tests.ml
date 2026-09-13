open Core
module S = Authoring_sources
module C = Authoring_corpus
module V = C.Coverage

let ok = Result.ok_or_failwith

let%expect_test "runtime coverage follows the selected surface and detects stale review" =
  let sources = S.installed () |> ok in
  let corpus = C.runtime_foundation ~sources |> ok in
  let surfaces = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ] in
  let targets = V.runtime_targets ~sources ~surface_ids:surfaces |> ok in
  V.audit corpus ~targets ~mappings:V.runtime_mappings |> ok |> V.require_complete |> ok;
  List.iter surfaces ~f:(fun surface ->
    let targets = V.runtime_targets ~sources ~surface_ids:[ surface ] |> ok in
    let mappings =
      List.filter V.runtime_mappings ~f:(fun mapping ->
        String.is_prefix mapping.V.target_id ~prefix:(surface ^ "/"))
    in
    V.audit corpus ~targets ~mappings |> ok |> V.require_complete |> ok;
    print_s [%sexp (surface : string), (List.length targets : int)]);
  (* A missing entry remains missing despite the shared job mappings. *)
  let timer = "delegated_moderator_v1/runtime/timers.lifecycle" in
  let omitted =
    List.filter V.runtime_mappings ~f:(fun mapping ->
      not (String.equal mapping.V.target_id timer))
  in
  let report = V.audit corpus ~targets ~mappings:omitted |> ok in
  assert (Result.is_error (V.require_complete report));
  print_s [%sexp (List.map report.missing ~f:(fun target -> target.V.id) : string list)];
  (* Literal reviewed pins must fail independently for source and topic drift. *)
  let changed field =
    List.map V.runtime_mappings ~f:(fun mapping ->
      match String.equal mapping.V.target_id timer with
      | false -> mapping
      | true -> field mapping)
  in
  let source_drift =
    changed (fun mapping ->
      { mapping with
        V.contract_sha256 = Chatmd_shell_spec.Source_ref.digest "old source"
      })
  in
  let topic_drift =
    changed (fun mapping ->
      { mapping with
        V.topic_closure_sha256 = Chatmd_shell_spec.Source_ref.digest "old guide"
      })
  in
  List.iter [ source_drift; topic_drift ] ~f:(fun mappings ->
    match V.audit corpus ~targets ~mappings with
    | Ok _ -> failwith "stale review accepted"
    | Error message -> print_endline message);
  (* A broad compiler surface or ambiguous selection cannot silently produce
     only the subset this inventory happens to understand. *)
  List.iter
    [ []; [ "moderator_v1"; "moderator_v1" ]; [ "core" ]; [ "unknown" ] ]
    ~f:(fun surface_ids ->
      assert (Result.is_error (V.runtime_targets ~sources ~surface_ids)));
  let paths =
    V.runtime_features
    |> List.concat_map ~f:(fun feature -> feature.implementation_paths)
    |> List.dedup_and_sort ~compare:String.compare
  in
  print_s
    [%sexp
      (List.length V.runtime_features : int)
    , (List.length targets : int)
    , (List.length paths : int)];
  [%expect
    {|
    (one_off_v1 8)
    (tool_v1 8)
    (moderator_v1 17)
    (delegated_moderator_v1 17)
    (delegated_moderator_v1/runtime/timers.lifecycle)
    delegated_moderator_v1/runtime/timers.lifecycle: compiler contract changed; review documentation coverage
    delegated_moderator_v1/runtime/timers.lifecycle: topic changed; review documentation coverage
    (17 50 42)
    |}]
;;
