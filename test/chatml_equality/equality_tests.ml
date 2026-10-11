open Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

(* This source-level model does not inspect inference variables, row constraints
   or the interpreter's equality function. Its finite vocabulary deliberately
   includes forbidden leaves under otherwise comparable constructors. *)
module Value = struct
  type t =
    | Integer
    | Floating
    | Boolean
    | Text
    | Unit
    | Array
    | Reference
    | Function
    | Task
    | Module
    | Record of t
    | Variant of t
  [@@deriving sexp]

  let leaves =
    [ Integer; Floating; Boolean; Text; Unit; Array; Reference; Function; Task; Module ]
  ;;

  let rec comparable = function
    | Integer | Floating | Boolean | Text | Unit -> true
    | Array | Reference | Function | Task | Module -> false
    | Record value | Variant value -> comparable value
  ;;

  let rec source = function
    | Integer -> "1"
    | Floating -> "1.5"
    | Boolean -> "true"
    | Text -> "\"text\""
    | Unit -> "()"
    | Array -> "[1]"
    | Reference -> "ref(1)"
    | Function -> "(fun x -> x)"
    | Task -> "Task.pure(1)"
    | Module -> "M"
    | Record value -> "{payload = " ^ source value ^ "; extra = true}"
    | Variant value -> "`Payload(" ^ source value ^ ")"
  ;;

  let all =
    List.concat_map leaves ~f:(fun leaf ->
      [ leaf; Record leaf; Variant leaf; Record (Variant leaf); Variant (Record leaf) ])
  ;;
end

module Context = struct
  type t =
    | Direct
    | Generic
    | Exported
    | Higher_order
    | Weak
    | Late_field
  [@@deriving sexp]

  let all = [ Direct; Generic; Exported; Higher_order; Weak; Late_field ]

  let source t ~operator value =
    let compare left right = left ^ " " ^ operator ^ " " ^ right in
    let body =
      match t with
      | Direct -> "let result = " ^ compare value value
      | Generic ->
        "let equal x y = "
        ^ compare "x" "y"
        ^ "\nlet result = equal("
        ^ value
        ^ ", "
        ^ value
        ^ ")"
      | Exported ->
        "module Equal = struct let run x y = "
        ^ compare "x" "y"
        ^ " end\nlet result = Equal.run("
        ^ value
        ^ ", "
        ^ value
        ^ ")"
      | Higher_order ->
        "let apply f x = f(x, x)\nlet equal x y = "
        ^ compare "x" "y"
        ^ "\nlet result = apply(equal, "
        ^ value
        ^ ")"
      | Weak ->
        "let equal = ref(fun x -> "
        ^ compare "x" "x"
        ^ ")\nlet result = (!equal)("
        ^ value
        ^ ")"
      | Late_field ->
        "let equal r = "
        ^ compare "r.payload" "r.payload"
        ^ "\nlet result = equal({payload = "
        ^ value
        ^ "; extra = [1]})"
    in
    "module M = struct let x = 1 end\n" ^ body
  ;;
end

let verify source ~accepted ~result =
  let program = Chatml.Chatml_parse.parse_program_exn source in
  match T.check_program program with
  | Error diagnostic ->
    if accepted
    then
      raise_s
        [%sexp "unexpected rejection", (source : string), (diagnostic.message : string)];
    [%test_eq: bool] true (Option.is_some diagnostic.span)
  | Ok _ ->
    if not accepted then raise_s [%sexp "equality obligation was lost", (source : string)];
    let env = Chatml_builtin_modules.create_default_env () in
    (match Chatml_resolver.run_program env program with
     | Ok () -> ()
     | Error (Type_diagnostic d) -> failwith d.message
     | Error (Runtime_diagnostic d) -> failwith d.message);
    (match L.find_var env "result" with
     | Some (VBool actual) -> [%test_eq: bool] result actual
     | _ -> raise_s [%sexp "expected Boolean result", (source : string)])
;;

let%test_unit "finite equality policy is invariant under abstraction" =
  List.iter Value.all ~f:(fun value ->
    let seed = "module M = struct let x = 1 end\nlet sample = " ^ Value.source value in
    (match T.check_program (Chatml.Chatml_parse.parse_program_exn seed) with
     | Ok _ -> ()
     | Error diagnostic ->
       raise_s
         [%sexp "invalid model seed", (seed : string), (diagnostic.message : string)]);
    List.iter Context.all ~f:(fun context ->
      List.iter
        [ "==", true; "!=", false ]
        ~f:(fun (operator, result) ->
          verify
            (Context.source context ~operator (Value.source value))
            ~accepted:(Value.comparable value)
            ~result)))
;;

(* Two uses may replace different fields. The independently calculated rule is:
   every field still visible in either compared copy must be comparable. *)
let%test_unit "copy equality checks visible fields and conjoins independent uses" =
  let replacements = [ []; [ "x" ]; [ "y" ]; [ "x"; "y" ] ] in
  let copy labels =
    match labels with
    | [] -> "r"
    | _ ->
      "{r with "
      ^ String.concat ~sep:"; " (List.map labels ~f:(fun label -> label ^ " = 1"))
      ^ "}"
  in
  List.iter (List.cartesian_product Value.leaves Value.leaves) ~f:(fun (x, y) ->
    List.iter (List.cartesian_product replacements replacements) ~f:(fun (left, right) ->
      let visible label value =
        Value.comparable value
        || (List.mem left label ~equal:String.equal
            && List.mem right label ~equal:String.equal)
      in
      let source =
        "module M = struct let x = 1 end\nlet compare r = let a = "
        ^ copy left
        ^ " in let first = a == a in let b = "
        ^ copy right
        ^ " in if first then b == b else false\nlet result = compare({x = "
        ^ Value.source x
        ^ "; y = "
        ^ Value.source y
        ^ "})"
      in
      verify source ~accepted:(visible "x" x && visible "y" y) ~result:true))
;;
