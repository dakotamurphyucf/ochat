open Core
module S = Authoring_sources
module C = Authoring_corpus
module V = C.Coverage

let ok = Result.ok_or_failwith

let%expect_test "compiled syntax inventory is separate from binding coverage" =
  let sources = S.installed () |> ok in
  let corpus = C.runtime_foundation ~sources |> ok in
  let productions = S.grammar sources in
  let ids = List.map productions ~f:(fun production -> production.S.id) in
  assert (Option.is_none (List.find_a_dup ids ~compare:String.compare));
  (* These forms include syntactic sugar and an explicitly rejected branch;
     none can be inferred by enumerating builtin function signatures. *)
  List.iter
    [ "expr -> LETSTAR task_let_binder EQ expr_sequence IN expr_sequence"
    ; "expr -> LETPLUS task_let_binder EQ expr_sequence IN expr_sequence"
    ; "expr -> BANGEQ expr"
    ; "type_arrow -> type_postfix ARROW type_arrow"
    ; "pattern -> LBRACE pattern_field_list RBRACE"
    ]
    ~f:(fun id -> assert (List.mem ids id ~equal:String.equal));
  List.iter
    [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    ~f:(fun surface ->
      let targets = V.grammar_targets ~sources ~surface_ids:[ surface ] |> ok in
      [%test_eq: int] (List.length productions) (List.length targets);
      let report = V.audit corpus ~targets ~mappings:[] |> ok in
      assert (List.is_empty report.mapped);
      assert (Result.is_error (V.require_complete report));
      [%test_eq: int] (List.length productions) (List.length report.missing));
  let targets =
    V.grammar_targets
      ~sources
      ~surface_ids:[ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    |> ok
  in
  V.audit corpus ~targets ~mappings:V.grammar_mappings |> ok |> V.require_complete |> ok;
  let changed =
    match targets with
    | first :: rest -> { first with contract_sha256 = String.make 64 '0' } :: rest
    | [] -> failwith "empty compiled grammar"
  in
  assert (Result.is_error (V.audit corpus ~targets:changed ~mappings:V.grammar_mappings));
  assert (Result.is_error (V.grammar_targets ~sources ~surface_ids:[]));
  assert (Result.is_error (V.grammar_targets ~sources ~surface_ids:[ "not-installed" ]));
  print_s
    [%sexp
      (List.length productions : int)
    , "compiled productions on each of four surfaces; unmapped syntax cannot pass \
       coverage"];
  [%expect
    {|
    (143
     "compiled productions on each of four surfaces; unmapped syntax cannot pass coverage")
    |}]
;;
