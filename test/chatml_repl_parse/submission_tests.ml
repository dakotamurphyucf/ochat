open Core
module Parse = Chatml.Chatml_parse
module Submission = Parse.Submission
module Sites = Chatml.Chatml_syntax_sites

let source text =
  let id =
    Chatml_source.Id.create ~package_namespace:"repl-test" ~relative_path:"input.chatml"
    |> Or_error.ok_exn
  in
  Chatml_source.create
    id
    ~contents:text
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint:(fun () -> ())
  |> Or_error.ok_exn
;;

let limits ?(max_source_bytes = 16_384) ?(max_tokens = 4096) ?(max_work_steps = 65_536) ()
  =
  let sites =
    Chatml.Chatml_syntax_sites.Limits.create ~max_sites:4096 ~max_name_bytes:16_384
    |> Or_error.ok_exn
  in
  Submission.Limits.create ~max_source_bytes ~max_tokens ~max_work_steps ~sites
  |> Or_error.ok_exn
;;

let compare_complete input parsed =
  let baseline =
    match
      Parse.parse_captured_with_sites
        input
        ~limits:
          (Sites.Limits.create ~max_sites:4096 ~max_name_bytes:16_384 |> Or_error.ok_exn)
        ~checkpoint:(fun () -> ())
    with
    | Ok unit -> unit
    | Error _ -> failwith "batch parser rejected complete submission"
  in
  let syntax unit = Chatml.Chatml_lang.sexp_of_program (Parse.Unit.program unit) in
  assert (Sexp.equal (syntax baseline) (syntax parsed));
  let project unit =
    let table = Parse.Unit.sites unit |> Option.value_exn in
    Sites.sites table
    |> List.map ~f:(fun site ->
      let location = Sites.Site.location site in
      assert (
        Chatml_source.Id.equal
          (Chatml_source.Location.source location)
          (Chatml_source.id input));
      assert (
        Chatml_source.Digest.equal
          (Chatml_source.Location.source_digest location)
          (Chatml_source.digest input));
      let name = Sites.Site.name site in
      let role = Sites.Site.role site in
      let span = Chatml_source.Location.span location in
      [%sexp (name : string), (role : Sites.Role.t), (span : Source.span)])
    |> fun rows -> [%sexp (rows : Sexp.t list)]
  in
  assert (Sexp.equal (project baseline) (project parsed))
;;

let classify text =
  match Submission.parse (source text) ~limits:(limits ()) ~checkpoint:(fun () -> ()) with
  | Submission.Complete unit ->
    compare_complete (Parse.Unit.source unit) unit;
    "complete"
  | Submission.Need_more need ->
    let at = Submission.Need_more.at need in
    let span = Chatml_source.Location.span at in
    assert (Int.equal span.left.offset (String.length text));
    "need_more"
  | Submission.Rejected rejection ->
    (match Submission.Rejection.reason rejection with
     | Submission.Rejection.Syntax _ -> "syntax"
     | Source_byte_limit -> "source_limit"
     | Token_limit -> "token_limit"
     | Work_limit -> "work_limit"
     | Sites _ -> "sites_limit")
;;

let%expect_test "complete, unfinished, and terminal syntax" =
  List.iter
    [ ""
    ; "let x = 1"
    ; "let x ="
    ; "let x = (|1, 2"
    ; "module M = struct let x = 1"
    ; "(* unfinished"
    ; "let x = \"unfinished"
    ; "let x = )"
    ; "let x = 1 )"
    ; "@"
    ; "let x = != 1"
    ; "let x = . 1"
    ; "let \"unfinished"
    ; "let x = ) (* open"
    ]
    ~f:(fun text -> printf "%S: %s\n" text (classify text));
  [%expect
    {|
    "": complete
    "let x = 1": complete
    "let x =": need_more
    "let x = (|1, 2": need_more
    "module M = struct let x = 1": need_more
    "(* unfinished": need_more
    "let x = \"unfinished": need_more
    "let x = )": syntax
    "let x = 1 )": syntax
    "@": syntax
    "let x = != 1": syntax
    "let x = . 1": syntax
    "let \"unfinished": syntax
    "let x = ) (* open": syntax
    |}]
;;

let%test_unit "valid token-boundary prefixes remain viable or complete" =
  let atoms =
    [ "let x = fun y -> y"
    ; "let x = {a = 1; b = 2}"
    ; "let x = (|1, 2|)"
    ; "module M = struct let x = 1 end"
    ; "let x = \"a\\nlong string\""
    ; "(* nested (* comment *) *) let x = 1"
    ]
  in
  let programs =
    atoms
    @ List.concat_map atoms ~f:(fun left ->
      List.map atoms ~f:(fun right -> left ^ ";; " ^ right))
  in
  List.iter programs ~f:(fun complete ->
    let lexer = Lexing.from_string complete in
    let rec token_ends acc =
      match Chatml.Chatml_lexer.token lexer with
      | EOF -> List.rev acc
      | _ -> token_ends (Lexing.lexeme_end lexer :: acc)
    in
    List.iter (0 :: token_ends []) ~f:(fun length ->
      let prefix = String.prefix complete length in
      match classify prefix with
      | "complete" | "need_more" -> ()
      | outcome -> failwithf "valid token prefix %S rejected as %s" prefix outcome ()))
;;

let%test_unit "real EOF sites own the input" =
  let text = "let x =" in
  let input = source text in
  match Submission.parse input ~limits:(limits ()) ~checkpoint:(fun () -> ()) with
  | Submission.Need_more need ->
    let at = Submission.Need_more.at need in
    assert (
      Chatml_source.Id.equal (Chatml_source.Location.source at) (Chatml_source.id input));
    assert (
      Chatml_source.Digest.equal
        (Chatml_source.Location.source_digest at)
        (Chatml_source.digest input));
    let span = Chatml_source.Location.span at in
    assert (Int.equal span.left.offset (String.length text));
    assert (Int.equal span.right.offset (String.length text))
  | Submission.Complete _ | Submission.Rejected _ -> failwith "expected Need_more"
;;

let%test_unit "limits reject and callback exceptions propagate by identity" =
  assert (String.equal (classify (String.make 17_000 'x')) "source_limit");
  (match
     Submission.parse
       (source "let x = 1")
       ~limits:(limits ~max_tokens:2 ())
       ~checkpoint:(fun () -> ())
   with
   | Submission.Rejected rejection ->
     (match Submission.Rejection.reason rejection with
      | Token_limit -> ()
      | _ -> failwith "expected token limit")
   | _ -> failwith "expected rejection");
  let exception Cancel in
  (match
     Submission.parse
       (source ("(*" ^ String.make 10_000 'x'))
       ~limits:(limits ())
       ~checkpoint:(fun () -> raise Cancel)
   with
   | exception Cancel -> ()
   | _ -> failwith "callback exception was swallowed");
  let callback_failure = Failure "caller failure" in
  match
    Submission.parse (source "let x = 1") ~limits:(limits ()) ~checkpoint:(fun () ->
      raise callback_failure)
  with
  | exception exn when phys_equal exn callback_failure -> ()
  | _ -> failwith "caller Failure was converted into syntax"
;;

let%test_unit "work budget bounds long lexical regions" =
  let input = source ("(*" ^ String.make 5000 'x') in
  match
    Submission.parse input ~limits:(limits ~max_work_steps:200 ()) ~checkpoint:(fun () ->
      ())
  with
  | Rejected rejection ->
    (match Submission.Rejection.reason rejection with
     | Work_limit -> ()
     | _ -> failwith "expected work limit")
  | Complete _ | Need_more _ -> failwith "long comment crossed work cap"
;;

let%test_unit "all observed publication checkpoints preserve cancellation" =
  let input = source "let x = 1 let y = {a = x; b = 2}" in
  let count = ref 0 in
  let run ~checkpoint = Submission.parse input ~limits:(limits ()) ~checkpoint in
  (match run ~checkpoint:(fun () -> incr count) with
   | Complete _ -> ()
   | _ -> failwith "expected complete submission");
  assert (Int.( > ) !count 0);
  let exception Cancel in
  for stop_at = 1 to !count do
    let seen = ref 0 in
    match
      run ~checkpoint:(fun () ->
        incr seen;
        if Int.equal !seen stop_at then raise Cancel)
    with
    | exception Cancel -> ()
    | _ -> failwithf "checkpoint %d lost cancellation" stop_at ()
  done
;;

let%test_unit "tight work budgets return typed outcomes throughout classification" =
  List.iter
    [ "let x ="; "let x = \"unfinished"; "let \"unfinished"; "let x = )"; "let x = . 1" ]
    ~f:(fun text ->
      let input = source text in
      let expected = classify text in
      assert (String.equal expected "need_more" || String.equal expected "syntax");
      let settled = ref false in
      for max_work_steps = 1 to 256 do
        let actual =
          match
            Submission.parse
              input
              ~limits:(limits ~max_work_steps ())
              ~checkpoint:(fun () -> ())
          with
          | Rejected rejection ->
            (match Submission.Rejection.reason rejection with
             | Work_limit -> "work_limit"
             | Syntax _ -> "syntax"
             | _ -> failwithf "unexpected rejection at budget %d" max_work_steps ())
          | Need_more _ -> "need_more"
          | Complete _ -> failwith "incomplete/invalid text classified complete"
        in
        if !settled
        then assert (String.equal actual expected)
        else if String.equal actual expected
        then settled := true
        else assert (String.equal actual "work_limit")
      done;
      assert !settled)
;;
