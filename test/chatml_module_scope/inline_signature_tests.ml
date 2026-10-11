open Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

let execute source =
  let env = Chatml_builtin_modules.create_default_env () in
  match
    Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
  with
  | Ok () -> env
  | Error (Type_diagnostic error) -> failwith (T.format_diagnostic source error)
  | Error (Runtime_diagnostic error) -> failwith error.message
;;

let module_env env name =
  match L.find_var env name with
  | Some (VModule env) -> env
  | _ -> failwith ("missing module " ^ name)
;;

let%test_unit "every interface subset agrees with checked and runtime nested visibility" =
  for width = 1 to 4 do
    let names = List.init width ~f:(fun index -> "value" ^ Int.to_string index) in
    let body =
      List.mapi names ~f:(fun index name -> "let " ^ name ^ " = " ^ Int.to_string index)
      |> String.concat ~sep:" "
    in
    for mask = 0 to (1 lsl width) - 1 do
      let selected =
        List.filteri names ~f:(fun index _ -> mask land (1 lsl index) <> 0)
      in
      let signature =
        List.map selected ~f:(fun name -> "val " ^ name ^ " : int")
        |> String.concat ~sep:" "
      in
      List.iter [ false; true ] ~f:(fun nested ->
        let signature, body, path =
          if nested
          then
            ( "module Nested : sig " ^ signature ^ " end"
            , "module Nested = struct " ^ body ^ " end"
            , "M.Nested" )
          else signature, body, "M"
        in
        let source = "module M : sig " ^ signature ^ " end = struct " ^ body ^ " end" in
        let env = module_env (execute source) "M" in
        let env = if nested then module_env env "Nested" else env in
        [%test_eq: string list]
          (List.sort ~compare:String.compare (Hashtbl.keys env.bindings))
          selected;
        List.iteri names ~f:(fun index name ->
          let expected = List.mem selected name ~equal:String.equal in
          (match L.find_var env name with
           | Some (VInt value) ->
             assert expected;
             [%test_eq: int] value index
           | None -> assert (not expected)
           | _ -> failwith "interface changed an exported value");
          let program =
            Chatml.Chatml_parse.parse_program_exn
              (source ^ " let read = " ^ path ^ "." ^ name)
          in
          match T.check_program program, expected with
          | Ok _, true | Error { span = Some _; _ }, false -> ()
          | _ -> failwith "checker and runtime disagree about selected exports"))
    done
  done
;;

let%expect_test "signed modules hide implementation state while closures retain it" =
  Module_scope_tests.run
    {|module Counter : sig
        type total = int
        val next : fun() -> total
        val identity : fun('a) -> 'a
        module Detail : sig val answer : int end
      end = struct
        type private_int = int
        type total = private_int
        let count = ref(0)
        let next () = count := !count + 1; !count
        let identity value = value
        module Detail = struct let private_value = 100 let answer = 40 end
      end
      open Counter
      let first : total = next()
      let second : total = Counter.next()
      let result =
        if identity(true) then
          identity(first) + second + Counter.Detail.answer
          + Array.length(record_keys(Counter))
          + Array.length(record_keys(Counter.Detail))
        else 0|};
  [%expect {| 47 |}]
;;

let%test_unit "opening preserves polymorphic schemes and shared monomorphic storage" =
  List.iter [ false; true ] ~f:(fun signed ->
    let signature = if signed then " : sig val identity : fun('a) -> 'a end" else "" in
    let source =
      "module M"
      ^ signature
      ^ " = struct let identity value = value end open M "
      ^ "let first = identity(true) let second = identity(42)"
    in
    let env = execute source in
    match L.find_var env "first", L.find_var env "second" with
    | Some (VBool true), Some (VInt 42) -> ()
    | _ -> failwith "opening specialized a polymorphic export");
  List.iter
    [ "2", true; "true", false ]
    ~f:(fun (second, compatible) ->
      let source =
        {|module M = struct let store = [] end
        open M
        let first = Array.set(store, 0, 1)
        let second = Array.set(M.store, 0, |}
        ^ second
        ^ ")"
      in
      match
        T.check_program (Chatml.Chatml_parse.parse_program_exn source), compatible
      with
      | Ok _, true | Error { span = Some _; _ }, false -> ()
      | _ -> failwith "opening changed shared mutable storage's element constraint")
;;

let%test_unit "invalid signatures reject before any initializer is executed" =
  List.iter
    [ "val answer : bool", "let answer = 1"
    ; "val read : fun() -> 'a array", "let store = [] let read () = store"
    ; "val compare : fun('a, 'a) -> bool", "let compare left right = left == right"
    ; ( "val first : int array val second : bool array"
      , "let store = [] let first = store let second = store" )
    ]
    ~f:(fun (signature, body) ->
      let source =
        "module M : sig "
        ^ signature
        ^ " end = struct let failure = 1 / 0 "
        ^ body
        ^ " end"
      in
      let env = Chatml_builtin_modules.create_default_env () in
      match
        Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
      with
      | Error (Type_diagnostic { span = Some _; _ }) ->
        assert (Option.is_none (L.find_var env "M"))
      | _ ->
        failwith "invalid signature reached initialization or lost its source location")
;;
