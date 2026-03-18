open Core
open Chatml_lang

type ty =
  | TVar of string
  | TInt
  | TFloat
  | TNumber
  | TBool
  | TString
  | TUnit
  | TArray of ty
  | TFun of ty list * ty

type builtin =
  { name : string
  ; scheme : ty
  ; impl : value list -> value
  }

let rec value_to_string (v : value) : string =
  match v with
  | VInt i -> Int.to_string i
  | VFloat f -> Float.to_string f
  | VBool b -> Bool.to_string b
  | VString s -> s
  | VArray arr ->
    let contents = Array.to_list arr |> List.map ~f:value_to_string in
    "[|" ^ String.concat ~sep:", " contents ^ "|]"
  | VRecord fields ->
    let rendered_fields =
      Map.to_alist fields |> List.map ~f:(fun (k, v') -> k ^ " = " ^ value_to_string v')
    in
    "{ " ^ String.concat ~sep:"; " rendered_fields ^ " }"
  | VRef r -> "ref(" ^ value_to_string !r ^ ")"
  | VModule _ -> "<module>"
  | VClosure _ -> "<closure>"
  | VUnit -> "()"
  | VBuiltin _ -> "<builtin>"
  | VVariant (slug, vals) ->
    if List.is_empty vals
    then Printf.sprintf "`%s" slug
    else (
      let inside = vals |> List.map ~f:value_to_string |> String.concat ~sep:", " in
      Printf.sprintf "`%s(%s)" slug inside)
;;

let rec equal_value (lhs : value) (rhs : value) : bool =
  match lhs, rhs with
  | VInt x, VInt y -> x = y
  | VFloat x, VFloat y -> Float.equal x y
  | VBool x, VBool y -> Bool.equal x y
  | VString x, VString y -> String.equal x y
  | VUnit, VUnit -> true
  | VVariant (tag_x, vals_x), VVariant (tag_y, vals_y) ->
    String.equal tag_x tag_y
    && List.length vals_x = List.length vals_y
    && List.for_all2_exn vals_x vals_y ~f:equal_value
  | VRecord fields_x, VRecord fields_y -> Map.equal equal_value fields_x fields_y
  | VArray arr_x, VArray arr_y -> phys_equal arr_x arr_y
  | VRef ref_x, VRef ref_y -> phys_equal ref_x ref_y
  | VClosure clos_x, VClosure clos_y -> phys_equal clos_x clos_y
  | VModule env_x, VModule env_y -> phys_equal env_x env_y
  | VBuiltin fn_x, VBuiltin fn_y -> phys_equal fn_x fn_y
  | _ -> false
;;

let with_unary_arg (name : string) (f : value -> value) : value list -> value = function
  | [ arg ] -> f arg
  | _ -> failwith (Printf.sprintf "%s: expected exactly one argument" name)
;;

let numeric_binop
      ~(name : string)
      ~(int_op : int -> int -> value)
      ~(float_op : float -> float -> value)
  : value list -> value
  = function
  | [ VInt x; VInt y ] -> int_op x y
  | [ VFloat x; VFloat y ] -> float_op x y
  | [ VInt x; VFloat y ] -> float_op (Float.of_int x) y
  | [ VFloat x; VInt y ] -> float_op x (Float.of_int y)
  | _ -> failwith (Printf.sprintf "Operator '%s' expects two numeric arguments" name)
;;

let numeric_cmp
      ~(name : string)
      ~(int_cmp : int -> int -> bool)
      ~(float_cmp : float -> float -> bool)
  : value list -> value
  = function
  | [ VInt x; VInt y ] -> VBool (int_cmp x y)
  | [ VFloat x; VFloat y ] -> VBool (float_cmp x y)
  | [ VInt x; VFloat y ] -> VBool (float_cmp (Float.of_int x) y)
  | [ VFloat x; VInt y ] -> VBool (float_cmp x (Float.of_int y))
  | _ ->
    failwith
      (Printf.sprintf "Operator '%s' requires two numeric (int/float) arguments" name)
;;

let builtins : builtin list =
  [ { name = "print"
    ; scheme = TFun ([ TVar "a" ], TUnit)
    ; impl =
        with_unary_arg "print" (fun v ->
          Printf.printf "%s \n" (value_to_string v);
          VUnit)
    }
  ; { name = "to_string"
    ; scheme = TFun ([ TVar "a" ], TString)
    ; impl = with_unary_arg "to_string" (fun v -> VString (value_to_string v))
    }
  ; { name = "num2str"
    ; scheme = TFun ([ TNumber ], TString)
    ; impl =
        with_unary_arg "num2str" (function
          | VInt x -> VString (Int.to_string x)
          | VFloat x -> VString (Float.to_string x)
          | _ -> failwith "num2str: expected a numeric argument")
    }
  ; { name = "bool2str"
    ; scheme = TFun ([ TBool ], TString)
    ; impl =
        with_unary_arg "bool2str" (function
          | VBool b -> VString (Bool.to_string b)
          | _ -> failwith "bool2str: expected a boolean argument")
    }
  ; { name = "length"
    ; scheme = TFun ([ TArray (TVar "a") ], TInt)
    ; impl =
        with_unary_arg "length" (function
          | VArray arr -> VInt (Array.length arr)
          | _ -> failwith "length: expected a single array argument")
    }
  ; { name = "++"
    ; scheme = TFun ([ TString; TString ], TString)
    ; impl =
        (function
          | [ VString x; VString y ] -> VString (x ^ y)
          | _ -> failwith "Operator '++' expects two string arguments")
    }
  ; { name = "+"
    ; scheme = TFun ([ TNumber; TNumber ], TNumber)
    ; impl =
        numeric_binop
          ~name:"+"
          ~int_op:(fun x y -> VInt (x + y))
          ~float_op:(fun x y -> VFloat (x +. y))
    }
  ; { name = "-"
    ; scheme = TFun ([ TNumber; TNumber ], TNumber)
    ; impl =
        numeric_binop
          ~name:"-"
          ~int_op:(fun x y -> VInt (x - y))
          ~float_op:(fun x y -> VFloat (x -. y))
    }
  ; { name = "*"
    ; scheme = TFun ([ TNumber; TNumber ], TNumber)
    ; impl =
        numeric_binop
          ~name:"*"
          ~int_op:(fun x y -> VInt (x * y))
          ~float_op:(fun x y -> VFloat (x *. y))
    }
  ; { name = "/"
    ; scheme = TFun ([ TNumber; TNumber ], TNumber)
    ; impl =
        (function
          | [ VInt x; VInt y ] ->
            if y = 0 then failwith "Division by zero" else VInt (x / y)
          | [ VFloat x; VFloat y ] ->
            if Float.equal y 0.0 then failwith "Division by zero" else VFloat (x /. y)
          | [ VInt x; VFloat y ] ->
            if Float.equal y 0.0
            then failwith "Division by zero"
            else VFloat (Float.of_int x /. y)
          | [ VFloat x; VInt y ] ->
            if y = 0 then failwith "Division by zero" else VFloat (x /. Float.of_int y)
          | _ -> failwith "Operator '/' expects two numeric arguments")
    }
  ; { name = "<"
    ; scheme = TFun ([ TNumber; TNumber ], TBool)
    ; impl = numeric_cmp ~name:"<" ~int_cmp:( < ) ~float_cmp:Float.( < )
    }
  ; { name = ">"
    ; scheme = TFun ([ TNumber; TNumber ], TBool)
    ; impl = numeric_cmp ~name:">" ~int_cmp:( > ) ~float_cmp:Float.( > )
    }
  ; { name = "<="
    ; scheme = TFun ([ TNumber; TNumber ], TBool)
    ; impl = numeric_cmp ~name:"<=" ~int_cmp:( <= ) ~float_cmp:Float.( <= )
    }
  ; { name = ">="
    ; scheme = TFun ([ TNumber; TNumber ], TBool)
    ; impl = numeric_cmp ~name:">=" ~int_cmp:( >= ) ~float_cmp:Float.( >= )
    }
  ; { name = "=="
    ; scheme = TFun ([ TVar "a"; TVar "a" ], TBool)
    ; impl =
        (function
          | [ lhs; rhs ] -> VBool (equal_value lhs rhs)
          | _ -> failwith "Operator '==' expects exactly two arguments")
    }
  ; { name = "!="
    ; scheme = TFun ([ TVar "a"; TVar "a" ], TBool)
    ; impl =
        (function
          | [ lhs; rhs ] -> VBool (not (equal_value lhs rhs))
          | _ -> failwith "Operator '!=' expects exactly two arguments")
    }
  ]
;;
