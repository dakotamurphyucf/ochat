open Core
module T = Chatml_typechecker
module L = Chatml.Chatml_lang
module Check = T.Signature_check

let implementation ?checkpoint body =
  let source = "module Implementation = struct " ^ body ^ " end" in
  let program = Chatml.Chatml_parse.parse_program_exn source in
  let state = T.create_state ?checkpoint () in
  let stmt = List.hd_exn program.stmts in
  let env, types = T.infer_stmt state (T.init_env ()) T.Type_alias.empty stmt.value in
  let values =
    match T.Env.find_exn env "Implementation" with
    | Module (Row (fields, Empty_row)) -> fields
    | _ -> failwith "expected inferred module exports"
  in
  let types =
    Option.value_exn (T.Type_alias.declared_module types ~name:"Implementation")
  in
  state, values, types
;;

let check (state, values, types) source =
  let signature =
    match Chatml.Chatml_parse.parse_interface source with
    | Ok signature -> signature
    | Error diagnostic ->
      failwith (Chatml.Chatml_parse.format_diagnostic source diagnostic)
  in
  let span = (List.hd_exn signature).span in
  T.check_module_signature
    state
    ~values
    ~types
    ~outer_types:T.Type_alias.empty
    ~signature
    ~span
;;

let require actual source expected =
  match check actual source, expected with
  | Ok _, true | Error _, false -> ()
  | Error error, true -> failwith error.message
  | Ok _, false -> failwith ("invalid signature accepted: " ^ source)
;;

let%test_unit "universal function specifications obey the identity and integer models" =
  List.iter [ "a"; "renamed" ] ~f:(fun parameter ->
    List.iter [ true; false ] ~f:(fun identity ->
      List.iter
        [ "int"; "bool"; "'" ^ parameter ]
        ~f:(fun input ->
          List.iter
            [ "int"; "bool"; "'" ^ parameter ]
            ~f:(fun output ->
              let body =
                if identity
                then "let transform value = value"
                else "let transform value = value + 1"
              in
              let expected =
                String.equal input output && (identity || String.equal input "int")
              in
              require
                (implementation body)
                ("val transform : fun(" ^ input ^ ") -> " ^ output)
                expected))))
;;

let%test_unit "signature equality obligations are entailed rather than silently added" =
  List.iter [ false; true ] ~f:(fun uses_equality ->
    let body =
      "let compare left right = " ^ if uses_equality then "left == right" else "true"
    in
    List.iter [ false; true ] ~f:(fun declared_equality ->
      let signature =
        "val compare : fun('a, 'a) -> bool"
        ^ if declared_equality then " where equality('a)" else ""
      in
      require (implementation body) signature ((not uses_equality) || declared_equality)))
;;

let%test_unit
    "signature failure cannot specialize shared mutable exports or captured state"
  =
  List.iter
    [ "int", "bool"; "bool", "int" ]
    ~f:(fun (first, second) ->
      let actual =
        implementation "let storage = [] let first = storage let second = storage"
      in
      require
        actual
        ("val first : " ^ first ^ " array val second : " ^ second ^ " array")
        false;
      require
        actual
        ("val first : " ^ second ^ " array val second : " ^ second ^ " array")
        true;
      require actual ("val first : " ^ first ^ " array") false);
  require
    (implementation "let storage = [] let get () = storage")
    "val get : fun() -> 'a array"
    false;
  require (implementation "let get () = []") "val get : fun() -> 'a array" true
;;

let%test_unit "transparent alias matching respects arity and parameter positions" =
  List.iter
    [ "a", "b"; "first", "second" ]
    ~f:(fun (a, b) ->
      List.iter [ false; true ] ~f:(fun swapped ->
        let signature =
          "type ('"
          ^ a
          ^ ", '"
          ^ b
          ^ ") pair = {x:'"
          ^ (if swapped then b else a)
          ^ ";y:'"
          ^ (if swapped then a else b)
          ^ "}"
        in
        require
          (implementation "type ('left, 'right) pair = {x:'left;y:'right}")
          signature
          (not swapped)));
  let actual = implementation "type 'a box = {value:'a}" in
  require actual "type box = {value:int}" false;
  require actual "type 'a absent = {value:'a}" false
;;

let%expect_test
    "checked interfaces select nested exports and preserve private alias meanings"
  =
  let actual =
    implementation
      {|type private_int = int
        type public_int = private_int
        let hidden = 1
        let identity value = value
        module Nested = struct
          type answer = public_int
          let secret = 100
          let answer = 42
        end|}
  in
  match
    check
      actual
      {|type public_int = int
        val identity : fun('a) -> 'a
        module Nested : sig
          type answer = public_int
          val answer : answer
        end|}
  with
  | Error error -> failwith error.message
  | Ok checked ->
    print_s [%sexp (Map.keys (Check.values checked) : string list)];
    print_s [%sexp (Check.visibility checked : Check.Visibility.t)];
    print_s
      [%sexp
        (T.Type_alias.declared_arity (Check.types checked) ~name:"private_int"
         : int option)];
    [%expect
      {|
      (Nested identity)
      ((Value identity) (Module Nested ((Value answer))))
      ()
      |}]
;;

let%test_unit "invalid interface members reject without running module initializers" =
  let actual = implementation "let x = 1 / 0 type t = int" in
  require actual "val x : int" true;
  List.iter
    [ "val absent : int"
    ; "val x : int val x : int"
    ; "val x : 'a where unsupported('a)"
    ; "val x : int where equality('missing)"
    ; "type t = bool"
    ; "module Missing : sig end"
    ]
    ~f:(fun signature -> require actual signature false)
;;
