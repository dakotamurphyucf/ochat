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
  ; { name = "length"
    ; scheme = TFun ([ TArray (TVar "a") ], TInt)
    ; impl =
        with_unary_arg "length" (function
          | VArray arr -> VInt (Array.length arr)
          | _ -> failwith "length: expected a single array argument")
    }
  ]
;;
