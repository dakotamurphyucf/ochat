open! Core
open Support

let metadata text = C.Observed.metadata (observed "demo/SKILL.md" text)

let result text =
  C.observe
    (source "demo/SKILL.md" text)
    ~root:(root_id "project")
    ~kind:Skill
    ~limits
    ~checkpoint:ignore
;;

let require_reason text reason =
  match result text with
  | Error (Invalid_metadata error) ->
    assert (C.Metadata_error.equal_reason error.reason reason)
  | Error error -> raise_s [%sexp (error : C.Error.t)]
  | Ok _ -> failwith "invalid header admitted"
;;

let%expect_test "bounded string profile preserves inert metadata and exact original bytes"
  =
  let text =
    "---\r\n\
     name: 'demo'\r\n\
     description: \"Uses Unicode λ and JSON escapes\"\r\n\
     license: MIT\r\n\
     allowed-tools: Bash Read\r\n\
     metadata:\r\n\
    \  owner: 'it''s inert'\r\n\
     custom: 123\r\n\
     ---\r\n\
     Exact body.\r\n"
  in
  let value = loaded "demo/SKILL.md" text in
  let meta = C.Observed.metadata (C.Loaded.observed value) in
  print_endline (Option.value_exn (C.Metadata.name meta));
  print_endline (Option.value_exn (C.Metadata.description meta));
  List.iter (C.Metadata.annotations meta) ~f:(fun (key, value) ->
    printf "%s=%s\n" key value);
  print_s [%sexp (C.Metadata.warnings meta : C.Warning.t list)];
  [%test_eq: string] (C.Loaded.contents value) text;
  [%test_eq: string]
    (String.drop_prefix text (C.Loaded.body_start_byte value))
    "Exact body.\r\n";
  [%expect
    {|
    demo
    Uses Unicode λ and JSON escapes
    license=MIT
    allowed-tools=Bash Read
    metadata.owner=it's inert
    custom=123
    ((Unknown_field custom))
    |}]
;;

let%test_unit "header grammar failures are explicit and never repaired" =
  List.iter
    [ "---\nname: demo\n---\n", C.Metadata_error.Missing_field
    ; "---\nname: demo\ndescription: ok\nname: demo\n---\n", Duplicate_field
    ; "---\nname: alias\ndescription: ok\n---\n", Name_mismatch
    ; "---\nname: demo\ndescription: >-\n  folded\n---\n", Unsupported_syntax
    ; "---\nname: demo\ndescription: !tag text\n---\n", Unsupported_syntax
    ; "---\nname: demo\ndescription: &anchor text\n---\n", Unsupported_syntax
    ; "---\nname: demo\ndescription: *alias\n---\n", Unsupported_syntax
    ; "---\nname: demo\ndescription: [a, b]\n---\n", Unsupported_syntax
    ; "---\nname: demo\ndescription: a: b\n---\n", Unsupported_syntax
    ; "---\nname: demo\ndescription: \"\"\n---\n", Invalid_value
    ; ( "---\nname: demo\ndescription: ok\nmetadata:\n  inner:\n    deeper: no\n---\n"
      , Invalid_value )
    ]
    ~f:(fun (text, reason) -> require_reason text reason)
;;

let%expect_test "headerless skill warns and AGENTS remains entirely plain" =
  let meta = metadata "# Plain skill\n" in
  let guidance = loaded ~kind:Project_guidance "AGENTS.md" "---\nname: anything\n" in
  print_s
    [%sexp
      (C.Metadata.name meta : string option)
    , (C.Metadata.description meta : string option)
    , (C.Metadata.warnings meta : C.Warning.t list)
    , (C.Loaded.body_start_byte guidance : int)];
  [%expect {| ((demo) () (Headerless_skill) 0) |}]
;;

let%test_unit "metadata limits count Unicode scalars rather than bytes or UTF16" =
  let unicode = "😀" in
  let repeated count = String.concat (List.init count ~f:(fun _ -> unicode)) in
  ignore (result (skill ~description:(repeated 1024) "demo") |> content_ok : C.Observed.t);
  require_reason (skill ~description:(repeated 1025) "demo") Limit_exceeded;
  let compatibility count =
    "---\nname: demo\ndescription: ok\ncompatibility: " ^ repeated count ^ "\n---\n"
  in
  ignore (result (compatibility 500) |> content_ok : C.Observed.t);
  require_reason (compatibility 501) Limit_exceeded;
  require_reason
    ("---\nname: demo\ndescription: " ^ String.make 8192 'x' ^ "\n---\n")
    Limit_exceeded;
  List.iter [ "\239\187\191# BOM"; "bad\255"; "nul\000byte" ] ~f:(fun text ->
    match result text with
    | Error Invalid_encoding -> ()
    | _ -> failwith "invalid encoding admitted")
;;

exception Probe

let%test_unit "validated IDs and bounded checkpoints are independent of body syntax" =
  List.iter
    [ ""; "../root"; "bad/path"; String.make 129 'a' ]
    ~f:(fun value -> assert (Result.is_error (C.Root_id.of_string value)));
  List.iter
    [ "UPPER/SKILL.md"; "two--hyphens/SKILL.md"; "nested/demo/SKILL.md"; "demo/OTHER.md" ]
    ~f:(fun member ->
      match
        C.observe
          (source member "plain")
          ~root:(root_id "root")
          ~kind:Skill
          ~limits
          ~checkpoint:ignore
      with
      | Error Invalid_identity -> ()
      | _ -> failwith "invalid skill member admitted");
  let tiny =
    C.Limits.create ~max_files:1 ~max_file_bytes:1 ~max_total_bytes:1 ~max_work_steps:1
    |> Or_error.ok_exn
  in
  assert (
    Result.is_error
      (C.observe
         (source "demo/SKILL.md" "body")
         ~root:(root_id "root")
         ~kind:Skill
         ~limits:tiny
         ~checkpoint:ignore));
  assert (
    try
      ignore
        (C.observe
           (source "demo/SKILL.md" "body")
           ~root:(root_id "root")
           ~kind:Skill
           ~limits
           ~checkpoint:(fun () -> raise Probe)
         : (_, _) Result.t);
      false
    with
    | Probe -> true)
;;
