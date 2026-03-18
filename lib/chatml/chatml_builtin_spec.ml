open Core
open Chatml_lang
open Jsonaf

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
  | TMu of string * ty
  | TRec_var of string

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

type builtin_module =
  { name : string
  ; exports : builtin list
  }

let module_scheme (m : builtin_module) : ty =
  record (List.map m.exports ~f:(fun b -> b.name, b.scheme))
;;

let option_ty (a : string) : ty =
  variant [ "None", TUnit; "Some", TVar a ]
;;

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

let expect_int (name : string) : value -> int = function
  | VInt i -> i
  | _ -> failwith (Printf.sprintf "%s: expected an int argument" name)
;;

let expect_record (name : string) : value -> value String.Map.t = function
  | VRecord fields -> fields
  | _ -> failwith (Printf.sprintf "%s: expected a record argument" name)
;;

let with_nullary_args (name : string) (f : unit -> value) : value list -> value = function
  | [] -> f ()
  | _ -> failwith (Printf.sprintf "%s: expected zero arguments" name)
;;

let with_ternary_args (name : string) (f : value -> value -> value -> value)
  : value list -> value
  = function
  | [ a; b; c ] -> f a b c
  | _ -> failwith (Printf.sprintf "%s: expected exactly three arguments" name)
;;

let make_nullary_builtin (name : string) (scheme : ty) (f : unit -> value) : builtin =
  { name; scheme; impl = with_nullary_args name f }
;;

let make_ternary_builtin
      (name : string)
      (scheme : ty)
      (f : value -> value -> value -> value)
  : builtin
  =
  { name; scheme; impl = with_ternary_args name f }
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

let json_entry_ty (self : ty) : ty =
  record [ "key", TString; "value", self ]
;;
let json_ty : ty =
  TMu
    ( "__builtin_json"
    , variant
        [ "Null", TUnit
        ; "Bool", TBool
        ; "Number", TFloat
        ; "String", TString
        ; "Array", TArray (TRec_var "__builtin_json")
        ; "Object", TArray (json_entry_ty (TRec_var "__builtin_json"))
        ] )
;;
let rec jsonaf_to_value (j : Jsonaf.t) : value =
  match j with
  | `Null -> VVariant ("Null", [])
  | `True -> VVariant ("Bool", [ VBool true ])
  | `False -> VVariant ("Bool", [ VBool false ])
  | `String s -> VVariant ("String", [ VString s ])
  | `Number n ->
    let f = Float.of_string n in
    VVariant ("Number", [ VFloat f ])
  | `Array xs ->
    VVariant ("Array", [ VArray (xs |> List.map ~f:jsonaf_to_value |> Array.of_list) ])
  | `Object fields ->
    let entries =
      fields
      |> List.map ~f:(fun (k, v) ->
        VRecord
          (Map.of_alist_exn
             (module String)
             [ "key", VString k; "value", jsonaf_to_value v ]))
      |> Array.of_list
    in
    VVariant ("Object", [ VArray entries ])
;;

let rec value_to_jsonaf (name : string) (v : value) : Jsonaf.t =
  match v with
  | VVariant ("Null", []) -> `Null
  | VVariant ("Bool", [ VBool true ]) -> `True
  | VVariant ("Bool", [ VBool false ]) -> `False
  | VVariant ("String", [ VString s ]) -> `String s
  | VVariant ("Number", [ VFloat f ]) -> `Number (Float.to_string f)
  | VVariant ("Array", [ VArray arr ]) ->
    `Array (arr |> Array.to_list |> List.map ~f:(value_to_jsonaf name))
  | VVariant ("Object", [ VArray entries ]) ->
    let fields =
      entries
      |> Array.to_list
      |> List.map ~f:(fun entry ->
        let m =
          match entry with
          | VRecord m -> m
          | _ -> failwith (name ^ ": object entry must be a record")
        in
        let key =
          match Map.find m "key" with
          | Some (VString s) -> s
          | _ -> failwith (name ^ ": object entry missing string field 'key'")
        in
        let value =
          match Map.find m "value" with
          | Some v -> value_to_jsonaf name v
          | None -> failwith (name ^ ": object entry missing field 'value'")
        in
        key, value)
    in
    `Object fields
  | _ ->
    failwith
      (Printf.sprintf
         "%s: expected Json.t (`Null/`Bool/`Number/`String/`Array/`Object)"
         name)
;;

let entry_ty a = record [ "key", TString; "value", TVar a ]
let tbl_ty a = TRef (TArray (entry_ty a))

let modules : builtin_module list =
  [ { name = "String"
    ; exports =
        [ make_unary_builtin "length" (TFun ([ TString ], TInt)) (fun v ->
            VInt (String.length (expect_string "String.length" v)))
        ; make_unary_builtin "is_empty" (TFun ([ TString ], TBool)) (fun v ->
            VBool (String.is_empty (expect_string "String.is_empty" v)))
        ; make_binary_builtin "concat" (TFun ([ TString; TString ], TString)) (fun a b ->
            VString (expect_string "String.concat" a ^ expect_string "String.concat" b))
        ]
    }
  ; { name = "Array"
    ; exports =
        [ make_unary_builtin "length" (TFun ([ TArray (TVar "a") ], TInt)) (fun v ->
            VInt (Array.length (expect_array "Array.length" v)))
        ; make_unary_builtin
            "copy"
            (TFun ([ TArray (TVar "a") ], TArray (TVar "a")))
            (fun v -> VArray (Array.copy (expect_array "Array.copy" v)))
        ; make_binary_builtin "get" (TFun ([ TArray (TVar "a"); TInt ], TVar "a")) (fun arr idx ->
            let a = expect_array "Array.get" arr in
            let i = expect_int "Array.get" idx in
            if i < 0 || i >= Array.length a
            then failwith "Array.get: index out of bounds"
            else a.(i))
        ; make_ternary_builtin
            "set"
            (TFun ([ TArray (TVar "a"); TInt; TVar "a" ], TUnit))
            (fun arr idx v ->
              let a = expect_array "Array.set" arr in
              let i = expect_int "Array.set" idx in
              if i < 0 || i >= Array.length a
              then failwith "Array.set: index out of bounds"
              else (
                a.(i) <- v;
                VUnit))
        ]
    }
  ; { name = "Option"
    ; exports =
        [ make_nullary_builtin "none" (TFun ([], option_ty "a")) (fun () ->
            VVariant ("None", []))
        ; make_unary_builtin "some" (TFun ([ TVar "a" ], option_ty "a")) (fun v ->
            VVariant ("Some", [ v ]))
        ; make_unary_builtin "is_none" (TFun ([ option_ty "a" ], TBool)) (fun v ->
            match v with
            | VVariant ("None", []) -> VBool true
            | VVariant ("Some", [ _ ]) -> VBool false
            | _ -> failwith "Option.is_none: expected `None or `Some(_)")
        ; make_unary_builtin "is_some" (TFun ([ option_ty "a" ], TBool)) (fun v ->
            match v with
            | VVariant ("None", []) -> VBool false
            | VVariant ("Some", [ _ ]) -> VBool true
            | _ -> failwith "Option.is_some: expected `None or `Some(_)")
        ; make_binary_builtin "get_or" (TFun ([ option_ty "a"; TVar "a" ], TVar "a"))
            (fun opt default ->
              match opt with
              | VVariant ("None", []) -> default
              | VVariant ("Some", [ v ]) -> v
              | _ -> failwith "Option.get_or: expected `None or `Some(_)")
        ]
    }
  ; (* Hashtbl: string-keyed, stored as ref(entry array) *)

    { name = "Hashtbl"
    ; exports =
        [ make_nullary_builtin "create" (TFun ([], tbl_ty "a")) (fun () ->
            VRef (ref (VArray [||])))

        ; make_ternary_builtin "set" (TFun ([ tbl_ty "a"; TString; TVar "a" ], TUnit))
            (fun tbl k v ->
              let cell = expect_ref "Hashtbl.set" tbl in
              let arr = expect_array "Hashtbl.set" !cell in
              let key = expect_string "Hashtbl.set" k in
              let entry =
                VRecord
                  (Map.of_alist_exn (module String) [ "key", VString key; "value", v ])
              in
              let n = Array.length arr in
              let rec find i =
                if i >= n then None
                else (
                  match arr.(i) with
                  | VRecord m ->
                    (match Map.find m "key" with
                     | Some (VString k') when String.equal k' key -> Some i
                     | _ -> find (i + 1))
                  | _ -> find (i + 1))
              in
              (match find 0 with
               | Some i ->
                 let arr' = Array.copy arr in
                 arr'.(i) <- entry;
                 cell := VArray arr'
               | None ->
                 let arr' =
                   Array.init (n + 1) ~f:(fun i -> if i < n then arr.(i) else entry)
                 in
                 cell := VArray arr');
              VUnit)

        ; make_binary_builtin "get" (TFun ([ tbl_ty "a"; TString ], option_ty "a"))
            (fun tbl k ->
              let cell = expect_ref "Hashtbl.get" tbl in
              let arr = expect_array "Hashtbl.get" !cell in
              let key = expect_string "Hashtbl.get" k in
              let rec loop i =
                if i >= Array.length arr
                then VVariant ("None", [])
                else (
                  match arr.(i) with
                  | VRecord m ->
                    (match Map.find m "key", Map.find m "value" with
                     | Some (VString k'), Some v when String.equal k' key ->
                       VVariant ("Some", [ v ])
                     | _ -> loop (i + 1))
                  | _ -> loop (i + 1))
              in
              loop 0)

        ; make_binary_builtin "mem" (TFun ([ tbl_ty "a"; TString ], TBool))
            (fun tbl k ->
              let cell = expect_ref "Hashtbl.mem" tbl in
              let arr = expect_array "Hashtbl.mem" !cell in
              let key = expect_string "Hashtbl.mem" k in
              let rec loop i =
                if i >= Array.length arr then false
                else (
                  match arr.(i) with
                  | VRecord m ->
                    (match Map.find m "key" with
                     | Some (VString k') when String.equal k' key -> true
                     | _ -> loop (i + 1))
                  | _ -> loop (i + 1))
              in
              VBool (loop 0))

        ; make_binary_builtin "remove" (TFun ([ tbl_ty "a"; TString ], TUnit))
            (fun tbl k ->
              let cell = expect_ref "Hashtbl.remove" tbl in
              let arr = expect_array "Hashtbl.remove" !cell in
              let key = expect_string "Hashtbl.remove" k in
              let kept =
                arr
                |> Array.to_list
                |> List.filter ~f:(function
                  | VRecord m ->
                    (match Map.find m "key" with
                     | Some (VString k') -> not (String.equal k' key)
                     | _ -> true)
                  | _ -> true)
                |> Array.of_list
              in
              cell := VArray kept;
              VUnit)
        ]
    }
  ; { name = "Json"
    ; exports =
        [ make_unary_builtin "parse" (TFun ([ TString ], json_ty)) (fun v ->
            let s = expect_string "Json.parse" v in
            try jsonaf_to_value (Jsonaf.of_string s) with
            | exn -> failwith (Printf.sprintf "Json.parse: %s" (Exn.to_string exn)))
        ; make_unary_builtin "stringify" (TFun ([ json_ty ], TString)) (fun v ->
          VString (Jsonaf.to_string (value_to_jsonaf "Json.stringify" v)))
        ; make_unary_builtin "pretty" (TFun ([ json_ty ], TString)) (fun v ->
            VString (Jsonaf.to_string_hum (value_to_jsonaf "Json.pretty" v)))
        ]
    }
  ]
;;
