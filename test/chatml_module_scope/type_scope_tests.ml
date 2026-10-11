open Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

let check source expected =
  let program = Chatml.Chatml_parse.parse_program_exn source in
  match T.check_program program, expected with
  | Ok _, true ->
    let env = Chatml_builtin_modules.create_default_env () in
    (match Chatml_resolver.run_program env program with
     | Ok () -> ()
     | Error _ -> failwith ("Checked module program failed at runtime:\n" ^ source))
  | Error { span = Some _; _ }, false -> ()
  | Error diagnostic, _ -> failwith (T.format_diagnostic source diagnostic)
  | Ok _, false -> failwith ("Unexpectedly accepted:\n" ^ source)
;;

let%test_unit "qualified aliases retain lexical types through nesting and renaming" =
  List.iter [ "Layer"; "Renamed" ] ~f:(fun name ->
    for depth = 1 to 4 do
      let path = String.concat ~sep:"." (List.init depth ~f:(fun _ -> name)) in
      let body =
        List.fold
          (List.init depth ~f:Fn.id)
          ~init:"type leaf = int type 'a box = {fixed:leaf;payload:'a}"
          ~f:(fun body _ -> "module " ^ name ^ " = struct " ^ body ^ " end")
      in
      List.iter
        [ "7", true; "true", false ]
        ~f:(fun (fixed, fixed_ok) ->
          List.iter
            [ "true", true; "7", false ]
            ~f:(fun (payload, payload_ok) ->
              check
                ("type leaf = bool "
                 ^ body
                 ^ " let result : bool "
                 ^ path
                 ^ ".box = {fixed="
                 ^ fixed
                 ^ ";payload="
                 ^ payload
                 ^ "}")
                (fixed_ok && payload_ok)))
    done)
;;

let%test_unit "module type exports exclude lexical and opened-only bindings" =
  List.iter [ "a"; "renamed" ] ~f:(fun name ->
    let declaration = "type " ^ name ^ " = int " in
    List.iter
      [ declaration ^ "module M = struct end"
      ; "module Earlier = struct "
        ^ declaration
        ^ " end module M = struct open Earlier end"
      ]
      ~f:(fun prefix -> check (prefix ^ " let result : M." ^ name ^ " = 1") false);
    check
      ("module M = struct " ^ declaration ^ " end open M let result : " ^ name ^ " = 1")
      true;
    check ("module M = struct " ^ declaration ^ " end " ^ declaration ^ "open M") false;
    check
      ("module M = struct "
       ^ declaration
       ^ " end module N = struct open M type "
       ^ name
       ^ " = bool end let result : N."
       ^ name
       ^ " = true")
      true)
;;

let%expect_test "qualified generic types are static and keep their defining scope" =
  Module_scope_tests.run
    {|type leaf = int
      type 'a box = {fixed:leaf;payload:'a}
      module Data = struct
        type leaf = bool
        type answer = int box
        let value : answer = {fixed=20;payload=22}
      end
      let read : fun(Data.answer) -> int = fun value -> value.fixed + value.payload
      let result = read(Data.value)|};
  [%expect {| 42 |}]
;;

let%test_unit "statement separators preserve declaration and expression sequencing" =
  let statements =
    [ "type t = int"
    ; "module Data = struct let value = 20 end"
    ; "open Data"
    ; "let count = ref(0)"
    ; "let result : t = (count := value; !count + 22)"
    ]
  in
  (* Exhaust every optional boundary, including repeated and surrounding
     separators, at both top level and inside a module. *)
  for mask = 0 to 31 do
    let source =
      ";; "
      ^ String.concat
          (List.mapi statements ~f:(fun index statement ->
             statement ^ if mask land (1 lsl index) = 0 then " " else " ;; ;; "))
      ^ " ;;"
    in
    List.iter
      [ source; "module Outer = struct " ^ source ^ " end let result = Outer.result" ]
      ~f:(fun source ->
        let env = Chatml_builtin_modules.create_default_env () in
        match
          Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
        with
        | Ok () ->
          (match L.find_var env "result" with
           | Some (VInt result) -> [%test_eq: int] result 42
           | _ -> failwith "separator changed the result binding")
        | Error _ -> failwith ("separator changed program behavior:\n" ^ source))
  done
;;

let%expect_test "separator ends a type declaration before a qualified call" =
  Module_scope_tests.run
    {|let count = ref(0)
      module Graph = struct let run () = count := !count + 1 end
      type t = int ;; Graph.run();;
      module Nested = struct
        type t = int ;; Graph.run();;
      end
      let result : t = !count|};
  [%expect {| 2 |}]
;;
