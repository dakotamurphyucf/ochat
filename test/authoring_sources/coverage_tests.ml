open! Core
module C = Authoring_corpus
module V = C.Coverage

let ok = Result.ok_or_failwith

let report label = function
  | Ok _ -> print_endline (label ^ ": accepted")
  | Error error -> print_endline (label ^ ": " ^ error)
;;

let fixture () =
  let sources = Authoring_sources.installed () |> ok in
  let corpus = C.runtime_foundation ~sources |> ok in
  let targets =
    V.compiler_targets ~sources ~surface_ids:[ "one_off_v1"; "tool_v1" ] |> ok
  in
  let target =
    List.find_exn targets ~f:(fun target ->
      String.equal target.V.id "one_off_v1/entrypoint/main")
  in
  (* These pins are synthesized only for adversarial audit fixtures. Maintained
     production mappings must contain literal reviewed pins. *)
  let mapping =
    V.
      { target_id = target.id
      ; contract_sha256 = target.contract_sha256
      ; topic_id = "runtime.invocations.one-off"
      ; topic_closure_sha256 =
          V.topic_contract
            corpus
            ~surface_id:target.surface_id
            ~topic_id:"runtime.invocations.one-off"
          |> ok
      ; evidence = [ "test/agent_docs/docs_chatml_authoring.ml" ]
      }
  in
  sources, corpus, targets, target, mapping
;;

let%expect_test "compiler coverage detects new, removed and changed API contracts" =
  let _, corpus, targets, target, mapping = fixture () in
  let full = V.audit corpus ~targets ~mappings:[ mapping ] |> ok in
  assert (List.length full.missing = List.length targets - 1);
  assert (
    List.exists full.missing ~f:(fun target ->
      String.equal target.id "tool_v1/entrypoint/run"));
  print_s [%sexp (full.mapped : string list)];
  report
    "mapped subset"
    (V.audit corpus ~targets:[ target ] ~mappings:[ mapping ]
     |> Result.bind ~f:V.require_complete);
  report
    "unmapped target"
    (V.audit corpus ~targets:[ target ] ~mappings:[] |> Result.bind ~f:V.require_complete);
  report
    "changed signature"
    (V.audit
       corpus
       ~targets:[ { target with contract_sha256 = String.make 64 '0' } ]
       ~mappings:[ mapping ]);
  report "removed binding" (V.audit corpus ~targets:full.missing ~mappings:[ mapping ]);
  report "duplicate mapping" (V.audit corpus ~targets ~mappings:[ mapping; mapping ]);
  [%expect
    {|
    (one_off_v1/entrypoint/main)
    mapped subset: accepted
    unmapped target: unmapped authoring features: one_off_v1/entrypoint/main
    changed signature: one_off_v1/entrypoint/main: compiler contract changed; review documentation coverage
    removed binding: one_off_v1/entrypoint/main: mapping has no inventory target
    duplicate mapping: duplicate coverage mapping: one_off_v1/entrypoint/main
  |}]
;;

let%expect_test "coverage binds reviewed dependencies, surface and behavioral evidence" =
  let sources, corpus, _, target, mapping = fixture () in
  let audit corpus mapping = V.audit corpus ~targets:[ target ] ~mappings:[ mapping ] in
  let changed =
    C.topics corpus
    |> List.map ~f:(fun topic ->
      let spec = topic.C.specification in
      match String.equal spec.id "chatml.introduction" with
      | true -> { spec with title = spec.title ^ " revised" }
      | false -> spec)
    |> C.create ~sources
    |> ok
  in
  report "changed prerequisite" (audit changed mapping);
  report
    "wrong surface"
    (audit corpus { mapping with topic_id = "runtime.invocations.standalone" });
  report "missing topic" (audit corpus { mapping with topic_id = "missing.topic" });
  report "no evidence" (audit corpus { mapping with evidence = [] });
  let pending =
    C.topics corpus
    |> List.map ~f:(fun topic ->
      let spec = topic.C.specification in
      match String.equal spec.id "chatml.introduction" with
      | true -> { spec with review = Pending }
      | false -> spec)
    |> C.create ~sources
    |> ok
  in
  report "pending prerequisite" (audit pending mapping);
  [%expect
    {|
    changed prerequisite: one_off_v1/entrypoint/main: topic changed; review documentation coverage
    wrong surface: authoring topic unavailable on one_off_v1: runtime.invocations.standalone
    missing topic: authoring topic is not installed: missing.topic
    no evidence: one_off_v1/entrypoint/main: coverage requires example or behavioral test evidence
    pending prerequisite: coverage topic has not been audited: chatml.introduction
  |}]
;;

let%expect_test "maintained entrypoint manifest is complete without hiding other API gaps"
  =
  let sources = Authoring_sources.installed () |> ok in
  let corpus = C.runtime_foundation ~sources |> ok in
  let surfaces = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ] in
  let targets = V.compiler_targets ~sources ~surface_ids:surfaces |> ok in
  let all = V.audit corpus ~targets ~mappings:V.entrypoint_mappings |> ok in
  assert (not (List.is_empty all.missing));
  let entrypoints =
    List.filter targets ~f:(fun target ->
      String.is_substring target.V.id ~substring:"/entrypoint/")
  in
  V.audit corpus ~targets:entrypoints ~mappings:V.entrypoint_mappings
  |> ok
  |> V.require_complete
  |> ok;
  print_s [%sexp (all.mapped : string list)];
  print_endline "other compiler APIs remain explicitly unmapped";
  [%expect
    {|
    (delegated_moderator_v1/entrypoint/initial_state
     delegated_moderator_v1/entrypoint/on_event
     moderator_v1/entrypoint/initial_state moderator_v1/entrypoint/on_event
     one_off_v1/entrypoint/main tool_v1/entrypoint/run)
    other compiler APIs remain explicitly unmapped
    |}]
;;
