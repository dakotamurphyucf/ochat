open Core
open Chatml_lang

type row =
  | TRow_empty
  | TRow_var of string
  | TRow_extend of (string * ty) list * row

and ty =
  | TVar of string
  | TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TArray of ty
  | TRef of ty
  | TTuple of ty list
  | TRecord of row
  | TVariant of row
  | TFun of ty list * ty

let closed_row (fields : (string * ty) list) : row = TRow_extend (fields, TRow_empty)
let open_row (fields : (string * ty) list) (tail : string) : row = TRow_extend (fields, TRow_var tail)
let record (fields : (string * ty) list) : ty = TRecord (closed_row fields)
let record_open (fields : (string * ty) list) (tail : string) : ty = TRecord (open_row fields tail)
let variant (cases : (string * ty) list) : ty = TVariant (closed_row cases)
let variant_open (cases : (string * ty) list) (tail : string) : ty =
  TVariant (open_row cases tail)
;;

type builtin =
  { name : string
  ; scheme : ty
  ; impl : value list -> value
  }

let expect_string (name : string) : value -> string = function
  | VString s -> s
  | _ -> failwith (Printf.sprintf "%s: expected a string argument" name)
;;

let expect_array (name : string) : value -> value array = function
  | VArray arr -> arr
  | _ -> failwith (Printf.sprintf "%s: expected an array argument" name)
;;

let expect_ref (name : string) : value -> value ref = function
  | VRef cell -> cell
  | _ -> failwith (Printf.sprintf "%s: expected a ref argument" name)
;;

let expect_record_like (name : string) : value -> string list = function
  | VRecord fields -> Map.to_alist fields |> List.map ~f:fst
  | VModule menv ->
    Hashtbl.fold menv ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)
    |> List.sort ~compare:String.compare
  | _ -> failwith (Printf.sprintf "%s: expected a record or module argument" name)
;;

let expect_variant (name : string) : value -> string = function
  | VVariant (tag, _payload) -> tag
  | _ -> failwith (Printf.sprintf "%s: expected a variant argument" name)
;;

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

let with_unary_arg (name : string) (f : value -> value) : value list -> value = function
  | [ arg ] -> f arg
  | _ -> failwith (Printf.sprintf "%s: expected exactly one argument" name)
;;

let with_binary_args (name : string) (f : value -> value -> value) : value list -> value =
  function
  | [ lhs; rhs ] -> f lhs rhs
  | _ -> failwith (Printf.sprintf "%s: expected exactly two arguments" name)
;;

let make_unary_builtin (name : string) (scheme : ty) (f : value -> value) : builtin =
  { name; scheme; impl = with_unary_arg name f }
;;

let make_binary_builtin
      (name : string)
      (scheme : ty)
      (f : value -> value -> value)
  : builtin
  =
  { name; scheme; impl = with_binary_args name f }
;;

let builtins : builtin list =
  [ make_unary_builtin "print" (TFun ([ TVar "a" ], TUnit)) (fun v ->
      Printf.printf "%s \n" (value_to_string v);
      VUnit)
  ; make_unary_builtin "to_string" (TFun ([ TVar "a" ], TString)) (fun v ->
      VString (value_to_string v))
  ; make_unary_builtin "length" (TFun ([ TArray (TVar "a") ], TInt)) (fun v ->
      VInt (Array.length (expect_array "length" v)))
  ; make_unary_builtin "string_length" (TFun ([ TString ], TInt)) (fun v ->
      VInt (String.length (expect_string "string_length" v)))
  ; make_unary_builtin "string_is_empty" (TFun ([ TString ], TBool)) (fun v ->
      VBool (String.is_empty (expect_string "string_is_empty" v)))
  ; make_unary_builtin "array_copy" (TFun ([ TArray (TVar "a") ], TArray (TVar "a"))) (fun v ->
      VArray (Array.copy (expect_array "array_copy" v)))
  ; make_unary_builtin "record_keys" (TFun ([ TRecord (TRow_var "r") ], TArray TString)) (fun v ->
      let keys =
        expect_record_like "record_keys" v
        |> List.sort ~compare:String.compare
        |> List.map ~f:(fun key -> VString key)
        |> Array.of_list
      in
      VArray keys)
  ; make_unary_builtin "variant_tag" (TFun ([ TVariant (TRow_var "r") ], TString)) (fun v ->
      VString (expect_variant "variant_tag" v))
  ; make_binary_builtin
      "swap_ref"
      (TFun ([ TRef (TVar "a"); TVar "a" ], TVar "a"))
      (fun lhs rhs ->
         let cell = expect_ref "swap_ref" lhs in
         let old = !cell in
         cell := rhs;
         old)
  ; make_unary_builtin "fail" (TFun ([ TString ], TVar "a")) (fun v ->
      failwith (expect_string "fail" v))
  ]
;;
