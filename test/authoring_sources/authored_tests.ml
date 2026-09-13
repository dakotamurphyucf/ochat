open Core
module C = Authoring_corpus
module M = Chatmd_shell_spec.Authoring_metadata

let ok = Result.ok_or_failwith

let base () =
  Authoring_sources.installed ()
  |> ok
  |> fun sources -> C.runtime_foundation ~sources |> ok
;;

let package ?(dependencies = [ "chatml.syntax.calls" ]) name text =
  let id = "custom." ^ name ^ ".conventions" in
  C.
    { help =
        M.
          { version = 1
          ; package = name
          ; tasks = [ One_off_script ]
          ; topics = [ id ]
          ; required_helpers = []
          }
    ; topics =
        [ { id
          ; title = "Authored conventions"
          ; prerequisites = dependencies
          ; surfaces = [ "one_off_v1" ]
          ; source_name = name ^ ".md"
          ; text
          }
        ]
    }
;;

let map_topic package f = { package with C.topics = List.map package.C.topics ~f }
let root package = List.hd_exn package.C.help.topics

let%expect_test
    "authored package provenance, scope and identities survive dependency assembly"
  =
  let installed = base () in
  let a =
    package "reports" "# Reporting conventions\nPreserve the original file names.\n"
  in
  let b = package "private" "PRIVATE-CONVENTIONS-SENTINEL" in
  let both = C.extend_authored installed [ a; b ] |> ok in
  let reversed = C.extend_authored installed [ b; a ] |> ok in
  let incremental =
    C.extend_authored installed [ a ]
    |> ok
    |> fun corpus -> C.extend_authored corpus [ b ] |> ok
  in
  assert (String.equal (C.identity both) (C.identity reversed));
  assert (String.equal (C.identity both) (C.identity incremental));
  let original = C.topic installed ~id:"chatml.syntax.calls" |> ok in
  let preserved = C.topic both ~id:"chatml.syntax.calls" |> ok in
  assert (String.equal original.sha256 preserved.sha256);
  assert (C.equal_origin preserved.origin Installed);
  let closure = C.assemble both ~surface_id:"one_off_v1" ~roots:[ root a ] |> ok in
  print_s
    [%sexp (List.map closure ~f:(fun topic -> topic.C.specification.id) : string list)];
  let custom = List.last_exn closure in
  (match custom.origin with
   | Authored { package; package_sha256 } ->
     assert (String.equal package "reports");
     assert (String.length package_sha256 = 64)
   | Installed -> failwith "custom conventions acquired official provenance");
  let fragment = List.hd_exn custom.fragments in
  assert (String.equal fragment.text (List.hd_exn a.topics).text);
  assert (String.equal fragment.source.path "reports.md");
  assert (
    Result.is_error
      (C.Coverage.topic_contract both ~surface_id:"one_off_v1" ~topic_id:(root a)));
  let scoped = C.scope_authored both ~packages:[ "reports" ] |> ok in
  assert (Result.is_error (C.topic scoped ~id:(root b)));
  assert (
    List.for_all (C.topics scoped) ~f:(fun topic ->
      List.for_all topic.fragments ~f:(fun fragment ->
        not (String.is_substring fragment.text ~substring:"PRIVATE-CONVENTIONS-SENTINEL"))));
  assert (List.length (C.authored_packages scoped) = 1);
  assert (
    String.equal
      (C.identity scoped)
      (C.identity (C.extend_authored installed [ a ] |> ok)));
  let empty = C.scope_authored both ~packages:[] |> ok in
  assert (String.equal (C.identity empty) (C.identity installed));
  List.iter
    [ map_topic a (fun topic -> { topic with text = topic.text ^ "changed" })
    ; map_topic a (fun topic -> { topic with source_name = "other.md" })
    ; { a with help = { a.help with required_helpers = [ Validation ] } }
    ]
    ~f:(fun changed ->
      let corpus = C.extend_authored installed [ changed; b ] |> ok in
      assert (not (String.equal (C.identity corpus) (C.identity both)));
      let topic = C.topic corpus ~id:(root a) |> ok in
      assert (not (String.equal topic.sha256 custom.sha256)));
  print_endline
    "official hashes preserved; authored origins distinct; private scope removed; \
     revisions invalidate";
  [%expect
    {|
    (chatml.introduction chatml.syntax.calls custom.reports.conventions)
    official hashes preserved; authored origins distinct; private scope removed; revisions invalidate
    |}]
;;

let%expect_test
    "custom dependency closures cannot restore omitted packages or override official \
     semantics"
  =
  let installed = base () in
  let b = package "b" "B conventions" in
  let a = package ~dependencies:[ root b ] "a" "A conventions" in
  let full = C.extend_authored installed [ a; b ] |> ok in
  let report label result =
    match result with
    | Error message -> print_endline (label ^ ": " ^ message)
    | Ok _ -> failwith (label ^ " unexpectedly accepted")
  in
  report "omitted dependency" (C.scope_authored full ~packages:[ "a" ]);
  report
    "cycle"
    (C.extend_authored
       installed
       [ a; package ~dependencies:[ root a ] "b" "B conventions" ]);
  report "duplicate" (C.extend_authored installed [ b; b ]);
  report
    "official override"
    (C.extend_authored
       installed
       [ map_topic b (fun topic -> { topic with id = "chatml.tasks" }) ]);
  report
    "surface mismatch"
    (C.extend_authored
       installed
       [ map_topic b (fun topic ->
           { topic with prerequisites = [ "runtime.invocations.moderator" ] })
       ]);
  report "missing package" (C.scope_authored full ~packages:[ "missing" ]);
  report "byte budget" (C.extend_authored ~max_bytes:1 installed [ b ]);
  report
    "invalid UTF-8"
    (C.extend_authored
       installed
       [ map_topic b (fun topic -> { topic with text = "\255" }) ]);
  assert (String.equal (C.identity installed) (C.identity (base ())));
  [%expect
    {|
    omitted dependency: authoring topic is not installed: custom.b.conventions
    cycle: authoring topic dependency cycle: custom.a.conventions -> custom.b.conventions -> custom.a.conventions
    duplicate: duplicate authored package: b
    official override: invalid authored topic: chatml.tasks
    surface mismatch: custom.b.conventions: prerequisite unavailable on a declared surface: runtime.invocations.moderator
    missing package: invalid authored package selection
    byte budget: authored reference package budget exceeded
    invalid UTF-8: invalid authored topic: custom.b.conventions
    |}]
;;
