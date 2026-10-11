open Core
module Checker = Chatml_typechecker
module Alias = Checker.Type_alias
module Syntax = Chatml.Chatml_module_signature
module Tests = Signature_check_tests

let types source =
  let _, _, types = Tests.implementation source in
  types
;;

let declare types source =
  match Chatml.Chatml_parse.parse_interface source with
  | Ok [ { Syntax.desc = Type declaration; _ } ] ->
    Alias.declare types declaration ~checkpoint:ignore
    |> Result.map_error ~f:(fun error -> error.Alias.Error.message)
    |> Result.ok_or_failwith
  | _ -> failwith "expected one type declaration"
;;

let scheme types source =
  let state = Checker.create_state () in
  Alias.elaborate_scheme
    types
    (Signature_scheme_tests.annotation source)
    ~equality_parameters:[]
    ~fresh_name:(fun () -> Checker.gensym state)
    ~checkpoint:ignore
;;

let require types source ~fixed =
  match scheme types source, fixed with
  | Ok _, true -> ()
  | Error error, false ->
    assert (String.is_substring error.message ~substring:"fixed function arity");
    assert (String.is_substring error.message ~substring:"fun(...) -> ...")
  | Error error, true -> failwith error.message
  | Ok _, false -> failwith ("ambiguous universal function admitted: " ^ source)
;;

let%test_unit "universal specifications require arity independent of substitution" =
  List.iter [ "a"; "renamed" ] ~f:(fun name ->
    let parameter = "'" ^ name in
    List.iter
      [ parameter ^ " -> int"
      ; "int -> " ^ parameter
      ; "unit -> " ^ parameter
      ; "fun(" ^ parameter ^ ") -> (int -> " ^ parameter ^ ")"
      ; "{handler:" ^ parameter ^ " -> int}"
      ; "[ `Handler(int -> " ^ parameter ^ ") ]"
      ]
      ~f:(fun source -> require Alias.empty source ~fixed:false);
    List.iter
      [ "fun(" ^ parameter ^ ") -> int"
      ; "fun(int) -> " ^ parameter
      ; "unit -> int"
      ; "int -> bool -> string"
      ; parameter ^ " array -> int"
      ; "int -> " ^ parameter ^ " array"
      ; "unit -> fun(" ^ parameter ^ ") -> int"
      ; "fun(" ^ parameter ^ ") -> fun(int) -> " ^ parameter
      ; "{handler:fun(" ^ parameter ^ ") -> int}"
      ]
      ~f:(fun source -> require Alias.empty source ~fixed:true))
;;

let%test_unit "private and inherited aliases cannot hide variable function arity" =
  List.iter
    [ "'a -> int", "fun('a) -> int"; "int -> 'a", "fun(int) -> 'a" ]
    ~f:(fun (legacy, explicit) ->
      let lexical =
        types ("type 'a private_fn = " ^ legacy ^ " type 'a public_fn = 'a private_fn")
      in
      let public =
        Alias.exports
          lexical
          ~type_names:[ "public_fn" ]
          ~module_names:[]
          ~checkpoint:ignore
        |> Result.ok_or_failwith
      in
      let dependency = Alias.add_module Alias.empty ~name:"Dependency" ~exports:public in
      require dependency "'a Dependency.public_fn" ~fixed:false;
      let opened =
        Alias.open_module dependency ~name:"Dependency" ~checkpoint:ignore
        |> Result.ok_or_failwith
      in
      require opened "'a public_fn" ~fixed:false;
      let shadowed =
        declare (Alias.enter_scope lexical) ("type 'a private_fn = " ^ explicit)
      in
      require shadowed "'a private_fn" ~fixed:true;
      (* The public alias retains its original lexical private definition, even
         after the caller shadows that name with a fixed explicit function. *)
      require shadowed "'a public_fn" ~fixed:false;
      let fixed = types ("type 'a public_fn = " ^ explicit) in
      require fixed "'a public_fn" ~fixed:true)
;;

let%test_unit
    "ordinary aliases and concrete universal applications retain legacy normalization"
  =
  let aliases = types "type 'a callback = 'a -> int type 'a producer = int -> 'a" in
  let ordinary source =
    Alias.elaborate aliases (Signature_scheme_tests.annotation source) ~checkpoint:ignore
    |> Result.map_error ~f:(fun error -> error.Alias.Error.message)
    |> Result.ok_or_failwith
  in
  List.iter
    [ ( "unit callback"
      , function
        | Checker.Fun ([], TInt) -> true
        | _ -> false )
    ; ( "(fun(bool) -> string) producer"
      , function
        | Checker.Fun ([ TInt; Boolean ], String) -> true
        | _ -> false )
    ]
    ~f:(fun (source, expected) ->
      assert (expected (ordinary source));
      match scheme aliases source with
      | Ok declared -> assert (expected declared)
      | Error error -> failwith error.message);
  Tests.require
    (Tests.implementation "let transform value = value")
    "val transform : fun('a) -> 'a"
    true;
  let ordinary_program =
    Chatml.Chatml_parse.parse_program_exn
      "type 'a callback = 'a -> int let identity value = value"
  in
  assert (Result.is_ok (Checker.check_program ordinary_program))
;;

let%expect_test "ambiguous universal arrows suggest explicit fixed arity syntax" =
  match scheme Alias.empty "'a -> 'a" with
  | Ok _ -> failwith "ambiguous universal arrow admitted"
  | Error error ->
    print_endline error.message;
    [%expect
      {| Universal value specifications require fixed function arity; use explicit fun(...) -> ... for parameter-dependent legacy arrows |}]
;;

let%test_unit "universal arity checking preserves every checkpoint exception" =
  let aliases = types "type 'a callback = 'a -> int" in
  List.iter [ "fun('a) -> 'a"; "'a callback" ] ~f:(fun source ->
    let annotation = Signature_scheme_tests.annotation source in
    let run checkpoint =
      let state = Checker.create_state () in
      Alias.elaborate_scheme
        aliases
        annotation
        ~equality_parameters:[]
        ~fresh_name:(fun () -> Checker.gensym state)
        ~checkpoint
    in
    let observed = ref 0 in
    ignore (run (fun () -> Int.incr observed));
    for stop = 1 to !observed do
      let exception Cancelled of int in
      let original = Cancelled stop in
      let visited = ref 0 in
      match
        run (fun () ->
          Int.incr visited;
          if Int.equal !visited stop then raise original)
      with
      | exception exn -> assert (phys_equal original exn)
      | Ok _ | Error _ -> failwith "checkpoint exception was swallowed"
    done)
;;
