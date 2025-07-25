(** Built-in standard library for the {e ChatML} interpreter.

    This module registers a minimal set of run-time primitives
    ("built-ins") inside a {!Chatml.Chatml_lang.env}.  The exported
    function {!BuiltinModules.add_global_builtins} mutates the provided
    environment in place so that user programs can access:

    {ul
    {- Basic printing utilities ([print] and [to_string]).}
    {- Simple aggregation helper ([sum]).}
    {- Array helper ([length]).}
    {- Arithmetic operators ([+], [-], [*], [/]).}
    {- Comparison operators ([<], [>], [<=], [>=], [==], [!=]).}}

    The implementation is intentionally small – it exists mostly to
    exercise the dynamic value representation of ChatML and to make the
    REPL usable during development.

    {2 Usage}

    {[
      open Chatml.Chatml_lang

      let env = create_env () in
      Chatml.Chatml_builtin_modules.BuiltinModules.add_global_builtins env;

      match find_var env "sum" with
      | Some (VBuiltin f) ->
          (* sum 1 2 3 = 6 *)
          let VInt six = f [ VInt 1; VInt 2; VInt 3 ] in
          assert (six = 6)
      | _ -> assert false
    ]}

    All helpers perform dynamic run-time checks and raise [Failure] with
    a descriptive message when the arguments do not satisfy the
    expected shape.
*)

open Core
open Chatml.Chatml_lang
(* Provides [value], [env], [set_var] … *)

(* -------------------------------------------------------------------------- *)
(* Shared helpers                                                              *)
(* -------------------------------------------------------------------------- *)

(** [value_to_string v] converts runtime value [v] to a human-readable
      string.  The representation is stable and intended primarily for
      debugging and unit-testing – it is {b not} meant to be parsed
      back by the interpreter.

      - Arrays are printed using the OCaml literal syntax {[| … |]}.
      - Records are rendered as {[{ field = value; … }]}.
      - References show their dereferenced content as
        {e ref}("value").
      - Function, module and builtin closures are abstracted as
        placeholder strings.
  *)
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
      Hashtbl.to_alist tbl |> List.map ~f:(fun (k, v') -> k ^ " = " ^ value_to_string v')
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
  (** [add_global_builtins env] populates [env] with the standard
      library of ChatML.

      The function performs {b in-place} mutation: callers must pass a
      fresh environment or be prepared for the existing bindings to be
      overwritten.

      The newly added symbols are:
      {ul
      {- [print]: variadic – prints any number of arguments using
         {!value_to_string} and appends a newline.}
      {- [to_string]: single argument – converts a value to its
         textual representation.}
      {- [sum]: variadic – sums a list of [int]s (fails on
         non-integers).}
      {- [length]: single argument – returns the size of an array.}
      {- Arithmetic operators: [+], [-], [*], [/] with numeric
         overloading between [int] and [float] (mixed mode converts the
         [int] to [float]).  Division by zero raises [Failure].}
      {- Comparison operators: [<], [>], [<=], [>=], [==], [!=] with the
         obvious semantics; heterogeneous arguments raise [Failure].}}
      All operations raise [Failure] on arity or type mismatch.
  *)
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
