open! Core
module C = Authoring_corpus

let sources () = Authoring_sources.installed () |> Result.ok_or_failwith

let specification ?(prerequisites = []) ?(surfaces = [ "one_off_v1"; "tool_v1" ]) id =
  C.
    { id
    ; title = id
    ; prerequisites
    ; surfaces
    ; excerpts =
        [ { path = "guide/chatml-ocaml-differences.md"
          ; heading = "## Calls have explicit arity"
          ; include_children = true
          }
        ]
    ; review = Pending
    }
;;

let report label = function
  | Ok _ -> print_endline (label ^ ": accepted")
  | Error error -> print_endline (label ^ ": " ^ error)
;;

let%expect_test "source sections preserve fences and bytes without false headings" =
  let body = "## Topic\r\nintro\r\n````ocaml\r\n## Topic\r\n```\r\n````\r\n" in
  let child = "### Child\n~~~\n## Next\n~~~\nchild\n" in
  let text = "# Intro\n" ^ body ^ child ^ "## Next\nnext\n" in
  List.iter
    [ false, body; true, body ^ child ]
    ~f:(fun (include_children, expected) ->
      let actual =
        C.section ~text ~heading:"## Topic" ~include_children |> Result.ok_or_failwith
      in
      print_s [%sexp (include_children : bool), (String.equal actual expected : bool)]);
  report
    "duplicate"
    (C.section
       ~text:(text ^ "## Topic\nagain")
       ~heading:"## Topic"
       ~include_children:true);
  report
    "unclosed"
    (C.section ~text:(text ^ "```ocaml\n") ~heading:"## Topic" ~include_children:true);
  [%expect
    {|
    (false true)
    (true true)
    duplicate: ambiguous topic heading: ## Topic
    unclosed: unclosed Markdown code fence in topic source
    |}]
;;

let%expect_test "shared prerequisite closure, surface checks and source-pinned review" =
  let sources = sources () in
  let definitions =
    [ specification "base"
    ; specification ~prerequisites:[ "base" ] "left"
    ; specification ~prerequisites:[ "base" ] "right"
    ; specification ~prerequisites:[ "left"; "right" ] "root"
    ]
  in
  let corpus = C.create ~sources definitions |> Result.ok_or_failwith in
  let assembled =
    C.assemble corpus ~surface_id:"one_off_v1" ~roots:[ "root"; "right" ]
    |> Result.ok_or_failwith
  in
  print_s [%sexp (List.map assembled ~f:(fun t -> t.C.specification.id) : string list)];
  let reordered = C.create ~sources (List.rev definitions) |> Result.ok_or_failwith in
  print_s [%sexp (String.equal (C.identity corpus) (C.identity reordered) : bool)];
  report "unavailable" (C.assemble corpus ~surface_id:"moderator_v1" ~roots:[ "root" ]);
  report
    "duplicate root"
    (C.assemble corpus ~surface_id:"one_off_v1" ~roots:[ "root"; "root" ]);
  let base = C.topic corpus ~id:"base" |> Result.ok_or_failwith in
  let reviewed =
    { base.specification with
      review =
        Audited
          { excerpt_sha256 = List.map base.fragments ~f:(fun f -> f.sha256)
          ; evidence = [ "fixture-review" ]
          }
    }
  in
  let audited =
    C.create ~sources (reviewed :: List.tl_exn definitions) |> Result.ok_or_failwith
  in
  print_s
    [%sexp
      (C.pending audited : string list)
    , (not (String.equal (C.identity corpus) (C.identity audited)) : bool)];
  let original = List.hd_exn reviewed.excerpts in
  let stale =
    { reviewed with
      excerpts = [ { original with heading = "## Mutation restricts polymorphism" } ]
    }
  in
  report "stale review" (C.create ~sources [ stale ]);
  [%expect
    {|
    (base left right root)
    true
    unavailable: authoring topic unavailable on moderator_v1: root
    duplicate root: duplicate authoring topic root
    ((left right root) true)
    stale review: base: audited excerpt hashes changed; review the topic again
    |}]
;;

let%expect_test "corpus rejects broken source/dependency manifests before assembly" =
  let sources = sources () in
  report
    "cycle"
    (C.create
       ~sources
       [ specification ~prerequisites:[ "b" ] "a"
       ; specification ~prerequisites:[ "a" ] "b"
       ]);
  report
    "missing dependency"
    (C.create ~sources [ specification ~prerequisites:[ "absent" ] "a" ]);
  report
    "incompatible dependency"
    (C.create
       ~sources
       [ specification ~surfaces:[ "one_off_v1" ] "base"
       ; specification ~prerequisites:[ "base" ] "consumer"
       ]);
  let broken = specification "broken" in
  let original = List.hd_exn broken.excerpts in
  report
    "missing section"
    (C.create
       ~sources
       [ { broken with
           excerpts = [ { original with heading = "## Not in this revision" } ]
         }
       ]);
  [%expect
    {|
    cycle: authoring topic dependency cycle: a -> b -> a
    missing dependency: authoring topic is not installed: absent
    incompatible dependency: consumer: prerequisite unavailable on a declared surface: base
    missing section: broken: topic heading not found: ## Not in this revision
    |}]
;;

let%expect_test "installed language topics preserve example context and audited sources" =
  let corpus = C.language_foundation ~sources:(sources ()) |> Result.ok_or_failwith in
  let assembled =
    C.assemble
      corpus
      ~surface_id:"delegated_moderator_v1"
      ~roots:[ "chatml.tasks"; "chatml.modules" ]
    |> Result.ok_or_failwith
  in
  print_s
    [%sexp (List.map assembled ~f:(fun topic -> topic.C.specification.id) : string list)];
  print_s [%sexp (C.pending corpus : string list)];
  let intro = List.hd_exn assembled in
  let text = (List.hd_exn intro.fragments).text in
  print_s
    [%sexp
      (String.is_substring
         text
         ~substring:
           "Every code block below is a complete candidate for the `one_off_v1` compiler"
       : bool)];
  [%expect
    {|
    (chatml.introduction chatml.syntax.calls chatml.syntax.containers
     chatml.types chatml.operators chatml.tasks chatml.modules)
    ()
    true
    |}]
;;

let%expect_test "runtime topics include execution prerequisites without changing targets" =
  let corpus = C.runtime_foundation ~sources:(sources ()) |> Result.ok_or_failwith in
  List.iter
    [ "one_off_v1", "runtime.invocations.one-off"
    ; "tool_v1", "runtime.invocations.standalone"
    ; "delegated_moderator_v1", "runtime.invocations.moderator"
    ]
    ~f:(fun (surface_id, root) ->
      let topics =
        C.assemble corpus ~surface_id ~roots:[ root ] |> Result.ok_or_failwith
      in
      let runtime =
        List.filter_map topics ~f:(fun topic ->
          let id = topic.C.specification.id in
          if
            String.is_prefix id ~prefix:"runtime."
            || String.is_prefix id ~prefix:"chatmd."
          then Some id
          else None)
      in
      print_s [%sexp (surface_id : string), (runtime : string list)]);
  report
    "wrong entrypoint"
    (C.assemble
       corpus
       ~surface_id:"one_off_v1"
       ~roots:[ "runtime.invocations.standalone" ]);
  print_s [%sexp (C.pending corpus : string list)];
  [%expect
    {|
    (one_off_v1
     (runtime.invocations.contracts runtime.authority.tool-selection
      runtime.invocations.validation runtime.invocations.one-off))
    (tool_v1
     (runtime.invocations.contracts runtime.authority.tool-selection
      chatmd.declarations.schemas runtime.invocations.validation
      runtime.invocations.standalone))
    (delegated_moderator_v1
     (runtime.invocations.contracts runtime.authority.tool-selection
      chatmd.declarations.schemas runtime.invocations.validation
      runtime.invocations.moderator))
    wrong entrypoint: authoring topic unavailable on one_off_v1: runtime.invocations.standalone
    ()
    |}]
;;
