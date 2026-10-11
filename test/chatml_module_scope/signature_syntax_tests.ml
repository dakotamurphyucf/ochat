open Core
module Signature = Chatml.Chatml_module_signature
module Parse = Chatml.Chatml_parse

let parse source =
  match Parse.parse_interface source with
  | Ok signature -> signature
  | Error diagnostic -> failwith (Parse.format_diagnostic source diagnostic)
;;

let source_at source (span : Source.span) =
  String.sub source ~pos:span.left.offset ~len:(span.right.offset - span.left.offset)
;;

let%test_unit "interface nesting preserves declaration order and exact source ranges" =
  List.iter [ "Box"; "Renamed" ] ~f:(fun name ->
    List.iter [ " "; " ;; " ] ~f:(fun separator ->
      for depth = 0 to 4 do
        let leaf =
          "type 'a t = {value:'a}"
          ^ separator
          ^ "val same : fun('a, 'a) -> bool where equality('a)"
        in
        let source =
          List.fold (List.init depth ~f:Fn.id) ~init:leaf ~f:(fun body _ ->
            "module " ^ name ^ " : sig " ^ body ^ " end")
        in
        let rec inspect remaining signature =
          match signature with
          | [ { Signature.desc = Module item; span } ] when remaining > 0 ->
            [%test_eq: string] item.name name;
            let text = source_at source span in
            assert (String.is_prefix text ~prefix:("module " ^ name ^ " : sig "));
            assert (String.is_suffix text ~suffix:" end");
            inspect (remaining - 1) item.signature
          | [ { desc = Type declaration; span = type_span }
            ; { desc = Value { name = value_name; annotation; requirements }; span }
            ]
            when remaining = 0 ->
            [%test_eq: string] declaration.name "t";
            [%test_eq: string list] declaration.parameters [ "a" ];
            [%test_eq: string] (source_at source type_span) "type 'a t = {value:'a}";
            [%test_eq: string] value_name "same";
            [%test_eq: string] (source_at source annotation.span) "fun('a, 'a) -> bool";
            [%test_eq: string]
              (source_at source span)
              "val same : fun('a, 'a) -> bool where equality('a)";
            (match requirements with
             | [ requirement ] ->
               [%test_eq: string] requirement.name "equality";
               [%test_eq: string] requirement.parameter "a";
               [%test_eq: string] (source_at source requirement.span) "equality('a)"
             | _ -> failwith "constraint was lost or duplicated")
          | _ -> failwith ("incorrect interface structure:\n" ^ source)
        in
        inspect depth (parse source)
      done))
;;

let%expect_test "companion interfaces contain declarations, never initializers" =
  List.iter
    [ "type node = {id:int} val visit : fun(node) -> int"
    ; "val identity : fun('a) -> 'a"
    ; "let hidden = 1"
    ; "module M = struct let x = 1 end"
    ; "Graph.run()"
    ]
    ~f:(fun source ->
      match Parse.parse_interface source with
      | Ok items -> print_s [%sexp (List.length items : int)]
      | Error { span; _ } ->
        assert (Option.is_some span);
        print_endline "initializer rejected");
  [%expect
    {|
    2
    1
    initializer rejected
    initializer rejected
    initializer rejected
    |}]
;;
