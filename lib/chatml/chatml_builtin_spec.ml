open Core
open Chatml_lang

type ty =
  | TVar of string
  | TInt
  | TFloat
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
