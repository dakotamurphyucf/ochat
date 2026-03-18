(** ChatML language core.

    This module defines:

    {ol
    {- The surface AST produced by the parser and consumed by the
       typechecker.}
    {- The resolved AST produced by the resolver and consumed by the
       evaluator.}
    {- Runtime value and environment types shared by the evaluator and
       builtin libraries.}
    {- Small helper functions for diagnostics, environments, and
       pattern matching.}}

    Evaluation itself lives in {!module:Chatml_eval}.  Keeping the
    evaluator out of this module makes the phase distinction explicit and
    avoids dependency cycles between the parser, typechecker, resolver,
    and interpreter.
*)

open Core

(***************************************************************************)
(* 1) AST Types                                                            *)
(***************************************************************************)

type 'a node =
  { value : 'a
  ; span : Source.span
  }
[@@deriving sexp]

type pattern =
  | PUnit
  | PWildcard
  | PVar of string
  | PInt of int
  | PBool of bool
  | PFloat of float
  | PString of string
  | PVariant of string * pattern list
  | PRecord of (string * pattern) list * bool
[@@deriving sexp, compare]

type var_loc =
  { depth : int
  ; index : int
  ; slot : Frame_env.packed_slot
  }
[@@deriving sexp_of]

type match_case =
  { pat : pattern
  ; pat_span : Source.span
  ; rhs : expr node
  }
[@@deriving sexp_of]

and match_case_slots =
  { pat : pattern
  ; pat_span : Source.span
  ; slots : Frame_env.packed_slot list
  ; rhs : expr node
  }
[@@deriving sexp_of]

and resolved_match_case =
  { pat : pattern
  ; pat_span : Source.span
  ; slots : Frame_env.packed_slot list
  ; rhs : resolved_expr node
  }
[@@deriving sexp_of]

and unary_prim =
  | UNegInt
  | UNegFloat
[@@deriving sexp_of]

and binary_prim =
  | BIntAdd
  | BIntSub
  | BIntMul
  | BIntDiv
  | BFloatAdd
  | BFloatSub
  | BFloatMul
  | BFloatDiv
  | BStringConcat
  | BIntLt
  | BIntGt
  | BIntLe
  | BIntGe
  | BFloatLt
  | BFloatGt
  | BFloatLe
  | BFloatGe
  | BEq
  | BNeq
[@@deriving sexp_of]

and expr =
  | EUnit
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EString of string
  | EVar of string
  | EVarLoc of var_loc
  | EPrim1 of unary_prim * expr node
  | EPrim2 of binary_prim * expr node * expr node
  | ELambda of string list * expr node
  | ELambdaSlots of string list * Frame_env.packed_slot list * expr node
  | EApp of expr node * expr node list
  | EIf of expr node * expr node * expr node
  | EWhile of expr node * expr node
  | ELetIn of string * expr node * expr node
  | ELetRec of (string * expr node) list * expr node
  | ELetBlock of (string * expr node) list * expr node
  | ELetBlockSlots of (string * expr node) list * Frame_env.packed_slot list * expr node
  | ELetRecSlots of (string * expr node) list * Frame_env.packed_slot list * expr node
  | EMatch of expr node * match_case list
  | EMatchSlots of expr node * match_case_slots list
  | ERecord of (string * expr node) list
  | EFieldGet of expr node * string
  | EVariant of string * expr node list
  | EArray of expr node list
  | EArrayGet of expr node * expr node
  | EArraySet of expr node * expr node * expr node
  | ERef of expr node
  | ESetRef of expr node * expr node
  | ESequence of expr node * expr node
  | EDeref of expr node
  | ERecordExtend of expr node * (string * expr node) list
[@@deriving sexp_of]

and resolved_expr =
  | REUnit
  | REInt of int
  | REBool of bool
  | REFloat of float
  | REString of string
  | REVarGlobal of string
  | REVarLoc of var_loc
  | REPrim1 of unary_prim * resolved_expr node
  | REPrim2 of binary_prim * resolved_expr node * resolved_expr node
  | RELambda of string list * Frame_env.packed_slot list * resolved_expr node
  | REApp of resolved_expr node * resolved_expr node list
  | REIf of resolved_expr node * resolved_expr node * resolved_expr node
  | REWhile of resolved_expr node * resolved_expr node
  | RELetBlock of
      (string * resolved_expr node) list
      * Frame_env.packed_slot list
      * resolved_expr node
  | RELetRec of
      (string * resolved_expr node) list
      * Frame_env.packed_slot list
      * resolved_expr node
  | REMatch of resolved_expr node * resolved_match_case list
  | RERecord of (string * resolved_expr node) list
  | REFieldGet of resolved_expr node * string
  | REVariant of string * resolved_expr node list
  | REArray of resolved_expr node list
  | REArrayGet of resolved_expr node * resolved_expr node
  | REArraySet of resolved_expr node * resolved_expr node * resolved_expr node
  | RERef of resolved_expr node
  | RESetRef of resolved_expr node * resolved_expr node
  | RESequence of resolved_expr node * resolved_expr node
  | REDeref of resolved_expr node
  | RERecordExtend of resolved_expr node * (string * resolved_expr node) list
[@@deriving sexp_of]

type stmt =
  | SLet of string * expr node
  | SLetRec of (string * expr node) list
  | SModule of string * stmt node list
  | SOpen of string
  | SExpr of expr node
[@@deriving sexp_of]

type resolved_stmt =
  | RSLet of string * resolved_expr node
  | RSLetRec of (string * resolved_expr node) list
  | RSModule of string * resolved_stmt node list
  | RSOpen of string
  | RSExpr of resolved_expr node
[@@deriving sexp_of]

type stmt_node = stmt node [@@deriving sexp_of]
type resolved_stmt_node = resolved_stmt node [@@deriving sexp_of]
type program =
  { stmts : stmt_node list
  ; source_text : string
  }
[@@deriving sexp_of]

type resolved_program =
  { stmts : resolved_stmt_node list
  ; source_text : string
  }
[@@deriving sexp_of]

(***************************************************************************)
(* Runtime diagnostics                                                     *)
(***************************************************************************)

type runtime_error =
  { message : string
  ; span : Source.span option
  }

exception Runtime_error of runtime_error

let raise_runtime_error ?span message =
  raise (Runtime_error { message; span })
;;

let format_runtime_error (source_text : string) (err : runtime_error) : string =
  match err.span with
  | None -> Printf.sprintf "Runtime error: %s" err.message
  | Some span ->
    let source = Source.read (Source.make source_text) span in
    let caret_count = Int.max 1 (span.right.column - span.left.column) in
    Printf.sprintf
      "line %i, characters %i-%i:\n%i|    %s%s\n      %s\n\nRuntime error: %s"
      span.left.line
      span.left.column
      span.right.column
      span.left.line
      source
      (String.make (span.left.column + 3) ' ')
      (String.make caret_count '^')
      err.message
;;

(***************************************************************************)
(* 2) Runtime Value Types                                                  *)
(***************************************************************************)

type value =
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VString of string
  | VVariant of string * value list
  | VRecord of value String.Map.t
  | VArray of value array
  | VRef of value ref
  | VClosure of clos
  | VModule of env
  | VUnit
  | VBuiltin of (value list -> value)

and clos =
  { params : string list
  ; body : resolved_expr node
  ; env : env
  ; frames : Frame_env.env
  ; param_slots : Frame_env.packed_slot list
  }

and cell = value ref
and env = (string, cell) Hashtbl.t

(***************************************************************************)
(* 3) Environment Helpers                                                  *)
(***************************************************************************)

let create_env () : env = Hashtbl.create (module String)

let copy_env (parent : env) : env =
  let child = Hashtbl.create (module String) in
  Hashtbl.iteri parent ~f:(fun ~key ~data -> Hashtbl.set child ~key ~data);
  child
;;

let find_var_cell (e : env) (x : string) : cell option = Hashtbl.find e x

let find_var (e : env) (x : string) : value option =
  Hashtbl.find e x |> Option.map ~f:(fun cell -> !cell)
;;

let define_var (e : env) (x : string) (v : value) : unit =
  Hashtbl.set e ~key:x ~data:(ref v)
;;

let update_var (e : env) (x : string) (v : value) : unit =
  match find_var_cell e x with
  | Some cell -> cell := v
  | None -> define_var e x v
;;

let set_var (e : env) (x : string) (v : value) : unit = define_var e x v

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

(***************************************************************************)
(* 4) Pattern Matching                                                     *)
(***************************************************************************)

let rec match_pattern (v : value) (p : pattern) : (string * value) list option =
  match p, v with
  | PUnit, VUnit -> Some []
  | PWildcard, _ -> Some []
  | PVar x, _ -> Some [ x, v ]
  | PInt i, VInt j when i = j -> Some []
  | PBool b, VBool c when Bool.equal b c -> Some []
  | PFloat f, VFloat g when Float.equal f g -> Some []
  | PString s, VString t when String.equal s t -> Some []
  | PVariant (tag, ps), VVariant (tagv, vs)
    when String.equal tag tagv && List.length ps = List.length vs ->
    let rec combine subpats subvals accum =
      match subpats, subvals, accum with
      | [], [], Some acc -> Some acc
      | p_hd :: p_tl, v_hd :: v_tl, Some acc ->
        (match match_pattern v_hd p_hd with
         | None -> None
         | Some binds -> combine p_tl v_tl (Some (acc @ binds)))
      | _ -> None
    in
    combine ps vs (Some [])
  | PRecord (fields, is_open), VRecord fields_map ->
    let rec match_fields fl acc =
      match fl with
      | [] -> Some acc
      | (lbl, pat) :: tl ->
        (match Map.find fields_map lbl with
         | None -> None
         | Some v_f ->
           (match match_pattern v_f pat with
            | None -> None
            | Some binds -> match_fields tl (acc @ binds)))
    in
    (match match_fields fields [] with
     | None -> None
     | Some binds ->
       if is_open
       then Some binds
       else if Map.length fields_map = List.length fields
       then Some binds
       else None)
  | _ -> None
;;

let collect_pattern_vars (pat : pattern) : string list =
  let rec aux acc p =
    match p with
    | PUnit -> acc
    | PVar x -> x :: acc
    | PWildcard | PInt _ | PBool _ | PFloat _ | PString _ -> acc
    | PVariant (_, ps) -> List.fold ps ~init:acc ~f:aux
    | PRecord (fields, _open) -> List.fold fields ~init:acc ~f:(fun ac (_, p) -> aux ac p)
  in
  List.rev (aux [] pat)
;;
