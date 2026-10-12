open! Core
module L = Chatml.Chatml_lang

let show source =
  Support.run source |> Chatml.Chatml_builtin_spec.value_to_string |> print_endline
;;

let%expect_test "heterogeneous products, nested pairs and ordinary argument lists" =
  show {|let result = (|7, "seven", (|true, ()|)|)|};
  show
    {|let add a b = a + b
      let sum pair = match pair with | (|a, b|) -> a + b
      let result = (|add(2, 3), sum((|2, 3|))|)|};
  show {|let result = (|`Pair(2, "x"), `Pair((|2, "x"|))|)|};
  [%expect
    {|
    (|7, seven, (|true, ()|)|)
    (|5, 5|)
    (|`Pair(2, x), `Pair((|2, x|))|)
    |}]
;;

let%expect_test "tuple matching permits existing record and variant element patterns" =
  show
    {|let classify value = match value with
      | (|`Some(n), {name = label; _}, (|true, ()|)|) -> label ++ to_string(n)
      | _ -> "other"
      let result = classify((|`Some(7), {name = "sample"; extra = 0}, (|true, ()|)|))|};
  show
    {|let counter = ref(0)
      let next () = counter := !counter + 1; !counter
      let result = (|next(), next(), next()|)|};
  [%expect
    {|
    sample7
    (|1, 2, 3|)
    |}]
;;

let%test_unit "aliases/signatures and product/postfix/arrow precedence keep exact arity" =
  Support.require
    {|module P : sig
        type 'a labelled = 'a * string
        val wrap : fun('a) -> 'a labelled
      end = struct
        type 'a labelled = 'a * string
        let wrap value = (|value, "label"|)
      end
      let first pair = match pair with | (|value, _|) -> value
      let a : int P.labelled = P.wrap(7)
      let b : bool P.labelled = P.wrap(true)
      let consume : int * string -> int = fun pair -> first(pair)
      let make : int -> string * bool = fun n -> (|to_string(n), true|)
      let array_pair : int array * string = (|[1], "one"|)
      let pair_array : (int * string) array = [(|1, "one"|)]
      let exact : fun(int * string, bool) -> int = fun pair flag -> consume(pair)
      let result = exact(a, first(b))|}
    ~accepted:true
;;

let%test_unit
    "tuple value restriction preserves shared mutable cells and polymorphic leaves"
  =
  (match
     Support.run
       {|let pair = let cell = ref([]) in (|cell, cell|)
        let result = match pair with | (|a, b|) -> a := [7]; (!b)[0]|}
   with
   | L.VInt n -> [%test_eq: int] n 7
   | _ -> failwith "Expected shared reference update");
  Support.require
    {|let pair = (|fun x -> x, "identity"|)
      let first pair = match pair with | (|value, _|) -> value
      let a = first(pair)(7)
      let b = first(pair)(true)|}
    ~accepted:true;
  Support.require
    {|let pair = let cell = ref([]) in (|cell, cell|)
      let result = match pair with | (|a, b|) -> a := [7]; b := ["wrong"]|}
    ~accepted:false
;;

let%test_unit "tuple leaves cannot hide equality restrictions and errors remain located" =
  List.iter [ "ref(1)"; "[1]"; "(fun x -> x)"; "Task.pure(1)"; "M" ] ~f:(fun leaf ->
    Support.require
      ("module M = struct let x = 1 end\nlet equal a b = a == b\nlet value = (|1, (|"
       ^ leaf
       ^ ", true|)|)\nlet result = equal(value, value)")
      ~accepted:false);
  List.iter
    [ "let result : int * bool = (|1, true, 2|)"
    ; "let result : (int * bool) * string = (|1, true, \"x\"|)"
    ; "let f a b = a + b\nlet result = f((|1, 2|))"
    ; "let result = match (|1, 2|) with | (|x, x|) -> x"
    ; "let result = match (|1, true|) with | (|1, true|) -> 7"
    ; "let result = match (|{x = 1}, true|) with | (|{x = x; x = y}, _|) -> x"
    ]
    ~f:(fun source -> Support.require source ~accepted:false);
  Support.require "let result = (|1, true|) == (|1, true|)" ~accepted:true
;;

let%test_unit "malformed source tuples reject rather than coerce unit/grouping/arguments" =
  List.iter [ "(||)"; "(|1|)"; "(|1, 2,|)"; "(1, 2)" ] ~f:(fun expression ->
    assert (
      Result.is_error (Chatml.Chatml_parse.parse_program ("let result = " ^ expression))))
;;

let%test_unit
    "tuple compilation propagates every observed checkpoint exception and reuses input"
  =
  let id =
    Chatml_source.Id.create ~package_namespace:"tuples" ~relative_path:"model.chatml"
    |> Or_error.ok_exn
  in
  let source =
    Chatml_source.create
      id
      ~contents:
        "type 'a pair = 'a * string\n\
         let value : int pair = (|7, \"x\"|)\n\
         let result = match value with | (|n, _|) -> n"
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:(fun () -> ())
    |> Or_error.ok_exn
  in
  Support.assert_checkpoints ~f:(fun checkpoint ->
    assert (
      Result.is_ok
        (Chatml_host_runtime.compile_captured_script
           source
           ~checkpoint
           ~surface:Chatml.Chatml_builtin_surface.core_surface
           ())))
;;

let%test_unit "interrupted tuple evaluation never publishes a partial product" =
  let program =
    Support.parse "let result = (|7, (|true, \"x\"|), ()|)"
    |> Chatml_resolver.resolve_program
  in
  Support.assert_checkpoints ~f:(fun checkpoint ->
    let control : L.execution_control =
      { checkpoint
      ; diagnostic_checkpoint = checkpoint
      ; allocate = (fun _ -> ())
      ; allocate_array = (fun _ -> ())
      ; before_builtin = (fun ~name:_ _ -> ())
      ; check_value = (fun _ -> ())
      ; before_json_import = (fun _ -> ())
      ; before_json_export = (fun _ -> ())
      ; before_effect = (fun ~name:_ ~spawned:_ -> ())
      ; after_effect = (fun _ -> ())
      }
    in
    let env = L.create_env ~control () in
    match Chatml.Chatml_eval.eval_program env program with
    | () -> assert (Option.is_some (L.find_var env "result"))
    | exception original ->
      assert (Option.is_none (L.find_var env "result"));
      raise original)
;;
