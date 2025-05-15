open Core
open Chatml.Chatml_lang (* This is where “value”, “env”, “set_var” etc. are defined. *)

(* -------------------------------------------------------------------------- *)
(* Shared helpers                                                              *)
(* -------------------------------------------------------------------------- *)

let rec value_to_string (v : value) : string =
  match v with
  | VInt i -> Int.to_string i
  | VFloat f -> Float.to_string f
  | VBool b -> Bool.to_string b
  | VString s -> s
  | VArray arr ->
    let contents = Array.to_list arr |> List.map ~f:value_to_string in
    "[|" ^ String.concat ~sep:", " contents ^ "|]"
  | VRecord tbl ->
    let fields =
      Hashtbl.to_alist tbl
      |> List.map ~f:(fun (k, v') -> k ^ " = " ^ value_to_string v')
    in
    "{ " ^ String.concat ~sep:"; " fields ^ " }"
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

module BuiltinModules = struct
  let add_global_builtins (env : env) =
    (* 1) Shared printers --------------------------------------------------- *)
    let fn_print =
      VBuiltin
        (fun args ->
          (match args with
           | [] -> failwith "print: expected at least one argument"
           | _ -> ());
          List.iter args ~f:(fun v -> Printf.printf "%s " (value_to_string v));
          Printf.printf "\n";
          VUnit)
    in
    set_var env "print" fn_print;

    let fn_to_string =
      VBuiltin
        (fun args ->
          match args with
          | [ v ] -> VString (value_to_string v)
          | _ -> failwith "to_string: expected exactly one argument")
    in
    set_var env "to_string" fn_to_string;
    (* 2) A “sum” function that sums a list of integers. *)
    let fn_sum =
      VBuiltin
        (fun args ->
          let rec sumvals vs acc =
            match vs with
            | [] -> VInt acc
            | VInt i :: tl -> sumvals tl (acc + i)
            | _ -> failwith "sum() only supports integer values"
          in
          sumvals args 0)
    in
    set_var env "sum" fn_sum;
    (* 3) Another built-in that returns array length if passed an array. *)
    let fn_length =
      VBuiltin
        (fun args ->
          match args with
          | [ VArray arr ] -> VInt (Array.length arr)
          | _ -> failwith "length() expects a single array argument")
    in
    set_var env "length" fn_length;
    (* ────────────────────────────────────────────────────────────────────────── *)
    (* 1. Arithmetic operators                                                 *)
    (* ────────────────────────────────────────────────────────────────────────── *)

    (* + operator *)
    set_var env "+"
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VInt (x + y)
           | [ VFloat x; VFloat y ] -> VFloat (x +. y)
           | [ VInt x; VFloat y ] -> VFloat (Float.of_int x +. y)
           | [ VFloat x; VInt y ] -> VFloat (x +. Float.of_int y)
           | _ -> failwith "Operator '+' expects two numeric arguments");
    (* - operator *)
    set_var env "-"
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VInt (x - y)
           | [ VFloat x; VFloat y ] -> VFloat (x -. y)
           | [ VInt x; VFloat y ] -> VFloat (Float.of_int x -. y)
           | [ VFloat x; VInt y ] -> VFloat (x -. Float.of_int y)
           | _ -> failwith "Operator '-' expects two numeric arguments");
    (* * operator *)
    set_var env "*"
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VInt (x * y)
           | [ VFloat x; VFloat y ] -> VFloat (x *. y)
           | [ VInt x; VFloat y ] -> VFloat (Float.of_int x *. y)
           | [ VFloat x; VInt y ] -> VFloat (x *. Float.of_int y)
           | _ -> failwith "Operator '*' expects two numeric arguments");
    (* / operator *)
    set_var env "/"
    @@ VBuiltin
         (function
           (* Integer division if both arguments are ints *)
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
           | _ -> failwith "Operator '/' expects two numeric arguments");
    (* ────────────────────────────────────────────────────────────────────────── *)
    (* 2. Comparison operators                                                 *)
    (* ────────────────────────────────────────────────────────────────────────── *)

    (* < operator *)
    set_var env "<"
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VBool (x < y)
           | [ VFloat x; VFloat y ] -> VBool Float.(x < y)
           | _ -> failwith "Operator '<' requires two numeric (int/float) arguments");
    (* > operator *)
    set_var env ">"
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VBool (x > y)
           | [ VFloat x; VFloat y ] -> VBool Float.(x > y)
           | _ -> failwith "Operator '>' requires two numeric (int/float) arguments");
    (* < operator *)
    set_var env "<="
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VBool (x <= y)
           | [ VFloat x; VFloat y ] -> VBool Float.(x <= y)
           | _ -> failwith "Operator '<=' requires two numeric (int/float) arguments");
    (* > operator *)
    set_var env ">="
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VBool (x >= y)
           | [ VFloat x; VFloat y ] -> VBool Float.(x >= y)
           | _ -> failwith "Operator '>=' requires two numeric (int/float) arguments");
    (* == operator *)
    set_var env "=="
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VBool (x = y)
           | [ VFloat x; VFloat y ] -> VBool (Float.equal x y)
           | [ VBool x; VBool y ] -> VBool (Bool.equal x y)
           | [ VString x; VString y ] -> VBool (String.equal x y)
           | _ -> failwith "Operator '==' type mismatch or invalid argument count");
    (* != operator *)
    set_var env "!="
    @@ VBuiltin
         (function
           | [ VInt x; VInt y ] -> VBool (x <> y)
           | [ VFloat x; VFloat y ] -> VBool (not (Float.equal x y))
           | [ VBool x; VBool y ] -> VBool (not (Bool.equal x y))
           | [ VString x; VString y ] -> VBool (not (String.equal x y))
           | _ -> failwith "Operator '!=' type mismatch or invalid argument count");
    (* ────────────────────────────────────────────────────────────────────────── *)
    ()
  ;;
  (* end of add_global_builtins body *)
end
