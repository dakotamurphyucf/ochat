(** ChatML evaluator runtime.

    This module implements the small, dynamically-typed {e ChatML} language
    used by the rest of the code-base to describe lightweight scripts in
    prompts and test-scenarios.

    The code is organised in eight conceptual layers, each introduced by a
    numbered banner in the source:

    {ol
    {- Abstract syntax tree description (section 1).}
    {- Runtime value representation (section 2).}
    {- Helpers for environments and variable lookup (section 3).}
    {- Pattern-matching engine (section 4).}
    {- Tail-call trampoline and frame helpers (sections 5 & 6).}
    {- Statement evaluation (section 7).}
    {- Program evaluation entry-point (section 8).}}

    Each public type and function is documented individually below.  Only
    the high-level entry point [eval_program] is intended for external
    consumers; everything else is provided to make the implementation
    testable.
*)

open Core

(***************************************************************************)
(* 1) AST Types                                                            *)
(***************************************************************************)

(** A syntax node annotated with its source code span.  The parser wraps
    every AST fragment in this record so that the evaluator and error
    reporter can recover position information. *)
type 'a node =
  { value : 'a
  ; span : Source.span
  }
[@@deriving sexp]

type pattern =
  | PWildcard
  | PVar of string
  | PInt of int
  | PBool of bool
  | PFloat of float
  | PString of string
  | PVariant of string * pattern list
  | PRecord of (string * pattern) list * bool (* true = open row with _ *)
[@@deriving sexp, compare]

(** Abstract patterns used in the surface syntax and by the runtime
    matcher.  The constructors mirror the value space of the language.  A
    record pattern carries a flag indicating whether a trailing [_]
    wildcard ("open row") is present – [true] means additional fields are
    allowed. *)

(** Represents the lexical address of a variable after the resolver pass.
   [depth] = how many frames to pop (0 = current frame),
   [index] = slot inside that frame.  The runtime currently stores only
   [Chatml_lang.value] values inside frames, therefore we do not need
   the full GADT-powered descriptor from [Frame_env] at this stage.  *)
type var_loc =
  { depth : int
  ; index : int
  ; slot : Frame_env.packed_slot
  }
[@@deriving sexp_of]

type expr =
  | EInt of int
  | EBool of bool
  | EFloat of float
  | EString of string
  | EVar of string
  | EVarLoc of var_loc
  | ELambda of string list * expr node
  | ELambdaSlots of string list * Frame_env.packed_slot list * expr node
  | EApp of expr node * expr node list
  | EIf of expr node * expr node * expr node
  | EWhile of expr node * expr node
  | ELetIn of string * expr node * expr node
  | ELetRec of (string * expr node) list * expr node
  (* A sequence of non-recursive let-bindings that belong to the SAME
     lexical block (i.e. were originally written as nested [let … in]
     constructs).  The resolver groups them so that the evaluator can
     allocate *one* frame whose layout hosts all the bound variables,
     thereby avoiding one frame allocation & push/pop per binding.      *)
  | ELetBlock of (string * expr node) list * expr node
  (* Same as [ELetBlock] but carries *explicit* slot descriptors chosen
     by the resolver.  The list of [packed_slot] is in one-to-one
     correspondence with the [bindings] list so that the evaluator can
     allocate a frame whose layout exactly matches the static
     selection. *)
  | ELetBlockSlots of (string * expr node) list * Frame_env.packed_slot list * expr node
  (* Mutually recursive let-bindings with their slot layout.  Mirrors
     [ELetRec] but lifts the slot list so that the runtime does not
     need to recompute it. *)
  | ELetRecSlots of (string * expr node) list * Frame_env.packed_slot list * expr node
  | EMatch of expr node * (pattern * expr node) list
  | EMatchSlots of expr node * (pattern * Frame_env.packed_slot list * expr node) list
  | ERecord of (string * expr node) list
  | EFieldGet of expr node * string
  | EFieldSet of expr node * string * expr node
  | EVariant of string * expr node list
  | EArray of expr node list
  | EArrayGet of expr node * expr node
  | EArraySet of expr node * expr node * expr node
  | ERef of expr node
  | ESetRef of expr node * expr node
  | ESequence of expr node * expr node (* e1 ; e2 *)
  | EDeref of expr node
  | ERecordExtend of expr node * (string * expr node) list
[@@deriving sexp_of]

(** Untyped core language expressions.  Variants whose names end with
    [*Slots] are produced by the resolver pass and contain pre-computed
    slot descriptors that guide frame allocation in the evaluator.  A
    regular consumer of the module should never manufacture these
    directly – use {!Chatml_resolver.resolve} instead. *)

type stmt =
  | SLet of string * expr node
  | SLetRec of (string * expr node) list
  | SModule of string * stmt node list
  | SOpen of string
  | SExpr of expr node
[@@deriving sexp_of]

(** Top–level statements accepted by the interpreter.  The concrete
    syntax provides syntactic sugar that is desugared into this
    representation by the parser. *)

(* The [stmt] type is used to represent the top-level statements in a
   module.  The [program] type is a list of statements, followed by the
   module name.  This is used to represent the entire program. *)

type stmt_node = stmt node [@@deriving sexp_of]

(* The [program] type is a list of statements, followed by the module name.
   This is used to represent the entire program. *)
type program = stmt_node list * string [@@deriving sexp_of]

(***************************************************************************)
(* 2) Runtime Value Types                                                  *)
(***************************************************************************)

type value =
  | VInt of int
  | VBool of bool
  | VFloat of float
  | VString of string
  | VVariant of string * value list
  | VRecord of (string, value) Hashtbl.t
  | VArray of value array
  | VRef of value ref
  | VClosure of clos
  | VModule of env
  | VUnit
  | VBuiltin of (value list -> value)

(** Runtime values.  Except for [VBuiltin], every constructor is produced
    by executing ChatML code.  [VBuiltin] wraps a host‐implemented OCaml
    function and is used to expose primitive operations and the standard
    library to user programs. *)

and clos =
  { params : string list
  ; body : expr node
  ; env : env
  ; frames : Frame_env.env (** captured stack of frames at the lambda creation point *)
  ; param_slots : Frame_env.packed_slot list (** static slot layout for parameters *)
  }

and env = (string, value) Hashtbl.t

(***************************************************************************)
(* 3) Environment Helpers                                                  *)
(***************************************************************************)

let create_env () : env = Hashtbl.create (module String)

let copy_env (parent : env) : env =
  let child = Hashtbl.create (module String) in
  Hashtbl.iteri parent ~f:(fun ~key ~data -> Hashtbl.set child ~key ~data);
  child
;;

let find_var (e : env) (x : string) : value option = Hashtbl.find e x
let set_var (e : env) (x : string) (v : value) : unit = Hashtbl.set e ~key:x ~data:v

(***************************************************************************)
(* 4) Pattern Matching                                                      *)
(***************************************************************************)

let rec match_pattern (v : value) (p : pattern) : (string * value) list option =
  match p, v with
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
  | PRecord (fields, is_open), VRecord tbl ->
    (* Ensure specified fields match; if closed pattern, ensure no extra fields exist. *)
    let rec match_fields fl acc =
      match fl with
      | [] -> Some acc
      | (lbl, pat) :: tl ->
        (match Hashtbl.find tbl lbl with
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
       else if
         (* closed: record must not have extra fields *)
         Hashtbl.length tbl = List.length fields
       then Some binds
       else None)
  | _ -> None
;;

(* Helper used both by the resolver and the interpreter to list variable
   names appearing in a pattern in a deterministic left-to-right order. *)
let collect_pattern_vars (pat : pattern) : string list =
  let rec aux acc p =
    match p with
    | PVar x -> x :: acc
    | PWildcard | PInt _ | PBool _ | PFloat _ | PString _ -> acc
    | PVariant (_, ps) -> List.fold ps ~init:acc ~f:aux
    | PRecord (fields, _open) -> List.fold fields ~init:acc ~f:(fun ac (_, p) -> aux ac p)
  in
  List.rev (aux [] pat)
;;

(***************************************************************************)
(* 5) Trampoline for Tail Calls                                            *)
(***************************************************************************)

type eval_result =
  | Value of value
  | TailCall of clos * value list

(* ------------------------------------------------------------------------- *)
(* Slot helpers                                                               *)
(* ------------------------------------------------------------------------- *)

(* Decide the slot kind to use for a syntactic RHS.  This must stay in sync
   with the resolver’s [slot_of_expr] logic to guarantee that the slot
   recorded inside [var_loc] matches the one used by the evaluator when
   allocating the frame and storing the value. *)

let slot_of_expr (e : expr) : Frame_env.packed_slot =
  match e with
  | EInt _ -> Frame_env.Slot Frame_env.SInt
  | EBool _ -> Frame_env.Slot Frame_env.SBool
  | EFloat _ -> Frame_env.Slot Frame_env.SFloat
  | EString _ -> Frame_env.Slot Frame_env.SString
  | _ -> Frame_env.Slot Frame_env.SObj
;;

(* Infer a slot from a *runtime* value.  This is only used in the [let-in]
   case because the frame is allocated *after* the RHS has been evaluated.
   When in doubt we fall back to [SObj] so that we never crash at run-time
   because of an unexpected constructor. *)

let slot_of_value (v : value) : Frame_env.packed_slot =
  match v with
  | VInt _ -> Frame_env.Slot Frame_env.SInt
  | VBool _ -> Frame_env.Slot Frame_env.SBool
  | VFloat _ -> Frame_env.Slot Frame_env.SFloat
  | VString _ -> Frame_env.Slot Frame_env.SString
  | _ -> Frame_env.Slot Frame_env.SObj
;;

(* Store a [value] into the [frame] at [idx] using the setter that matches
   the given slot descriptor.  If the value kind does not match the slot we
   assert at run-time that the slot descriptor chosen by the static
   resolver matches the *actual* value kind we are storing.  A mismatch is
   symptomatic of a bug in the resolver / interpreter agreement and would
   otherwise lead to silent corruption (for instance reading back an int
   through [get_obj]).

   The assertion raises a clear exception in debug builds; in production it
   can be compiled away by defining [ocamlopt -assert false]. *)

let slot_matches_value (slot : Frame_env.packed_slot) (v : value) : bool =
  match slot, v with
  | Frame_env.Slot Frame_env.SInt, VInt _ -> true
  | Frame_env.Slot Frame_env.SBool, VBool _ -> true
  | Frame_env.Slot Frame_env.SFloat, VFloat _ -> true
  | Frame_env.Slot Frame_env.SString, VString _ -> true
  | Frame_env.Slot Frame_env.SObj, _ -> true
  | _ -> false
;;

let store_with_slot
      (fr : Frame_env.frame)
      (idx : int)
      (slot : Frame_env.packed_slot)
      (v : value)
  : unit
  =
  (* Debug-time safety check – fail fast on inconsistent slot/value pair. *)
  if not (slot_matches_value slot v)
  then failwith "internal: slot/value mismatch between resolver slot and runtime value";
  match slot with
  | Frame_env.Slot Frame_env.SInt ->
    (match v with
     | VInt n -> Frame_env.set_int fr idx n
     | _ -> Frame_env.set_obj fr idx (Obj.repr v))
  | Frame_env.Slot Frame_env.SBool ->
    (match v with
     | VBool b -> Frame_env.set_bool fr idx b
     | _ -> Frame_env.set_obj fr idx (Obj.repr v))
  | Frame_env.Slot Frame_env.SFloat ->
    (match v with
     | VFloat f -> Frame_env.set_float fr idx f
     | _ -> Frame_env.set_obj fr idx (Obj.repr v))
  | Frame_env.Slot Frame_env.SString ->
    (match v with
     | VString s -> Frame_env.set_str fr idx s
     | _ -> Frame_env.set_obj fr idx (Obj.repr v))
  | Frame_env.Slot Frame_env.SObj -> Frame_env.set_obj fr idx (Obj.repr v)
;;

(* finish_eval: resolves any TailCall until we get a concrete Value. *)
(* ------------------------------------------------------------------------- *)
(*  Frame helpers                                                            *)
(* ------------------------------------------------------------------------- *)

let rec frames_nth (frames : Frame_env.env) (n : int) : Frame_env.frame option =
  match frames, n with
  | [], _ -> None
  | fr :: _, 0 -> Some fr
  | _ :: tl, n when n > 0 -> frames_nth tl (n - 1)
  | _ -> None
;;

let load_from_frames (frames : Frame_env.env) (loc : var_loc) : value =
  match frames_nth frames loc.depth with
  | None -> failwith "Frame stack underflow in EVarLoc evaluation"
  | Some fr ->
    (match loc.slot with
     | Frame_env.Slot Frame_env.SInt -> VInt (Frame_env.get_int fr loc.index)
     | Frame_env.Slot Frame_env.SBool -> VBool (Frame_env.get_bool fr loc.index)
     | Frame_env.Slot Frame_env.SFloat -> VFloat (Frame_env.get_float fr loc.index)
     | Frame_env.Slot Frame_env.SString -> VString (Frame_env.get_str fr loc.index)
     | Frame_env.Slot Frame_env.SObj ->
       let obj = Frame_env.get_obj fr loc.index in
       Obj.obj obj)
;;

(* ------------------------------------------------------------------------- *)
(*  Tail-call trampoline – now threads [frames] as an explicit parameter.    *)
(* ------------------------------------------------------------------------- *)

let rec finish_eval (initial_frames : Frame_env.env) (initial_res : eval_result) : value =
  (* [loop] is an explicit tail-recursive trampoline that rolls through the
     chain of [TailCall] results produced by the evaluator without growing
     the OCaml call-stack. *)
  let rec loop (_frames : Frame_env.env) (res : eval_result) : value =
    match res with
    | Value v -> v
    | TailCall (cl, args) ->
      (* 1. The resolver now guarantees that [param_slots] is populated and
         has the same arity as the argument list.  Any discrepancy is a bug
         in the compiler pipeline so we turn it into a hard failure. *)
      if List.length cl.param_slots <> List.length args
      then failwith "internal: closure param_slots length mismatch with call-site";
      let slots = cl.param_slots in
      (* 2. Allocate the frame and store arguments. *)
      let param_frame = Frame_env.alloc_packed slots in
      List.iteri args ~f:(fun idx v ->
        let slot = List.nth_exn slots idx in
        store_with_slot param_frame idx slot v);
      (* 3. Build the environment and updated frame stack for the callee.  We
         reuse the captured environment directly – function bodies do not
         mutate it, so no copy is necessary – avoiding an O(n) hash-table
         clone on every call. *)
      let child_env = cl.env in
      let child_frames = param_frame :: cl.frames in
      (* 4. Evaluate the function body one step; iterate. *)
      let next_res = eval_expr child_env child_frames cl.body in
      loop child_frames next_res
  in
  loop initial_frames initial_res

(***************************************************************************)
(* 6) Expression Evaluation                                                *)
(***************************************************************************)

and eval_expr (env : env) (frames : Frame_env.env) (e : expr node) : eval_result =
  match e.value with
  | EInt i -> Value (VInt i)
  | EBool b -> Value (VBool b)
  | EFloat f -> Value (VFloat f)
  | EString s -> Value (VString s)
  | EVar x ->
    (match find_var env x with
     | Some v -> Value v
     | None -> failwith (Printf.sprintf "Unbound variable %s" x))
  | EVarLoc loc -> Value (load_from_frames frames loc)
  | ELambda (params, body) ->
    (* Param slot information is not yet threaded from the resolver, so we
       default to an empty list which signals the trampoline to perform the
       old dynamic fallback.  A future pass will populate it through the
       resolver/type-checker link. *)
    Value (VClosure { params; body; env; frames; param_slots = [] })
  | ELambdaSlots (params, slots, body) ->
    Value (VClosure { params; body; env; frames; param_slots = slots })
  | ELetBlock (bindings, body) ->
    (* 1. Decide the slot layout *syntactically* so that it matches exactly
       what the resolver recorded. *)
    let slots =
      List.map bindings ~f:(fun (_nm, rhs_expr) -> slot_of_expr rhs_expr.value)
    in
    (* 2. Allocate the frame with the precise (but still heterogeneous)
       slot layout produced by the resolver heuristic.  The new helper
       hides the necessary [Obj.magic] to bypass the type-system
       restriction on heterogeneous lists. *)
    let block_frame = Frame_env.alloc_packed slots in
    let child_frames = block_frame :: frames in
    (* 3. Evaluate each RHS sequentially, storing the result with the proper
       setter.  Earlier bindings are visible to later RHSs through the frame
       we just pushed. *)
    List.iteri bindings ~f:(fun idx (_nm, rhs_expr) ->
      let v = finish_eval child_frames (eval_expr env child_frames rhs_expr) in
      let slot = List.nth_exn slots idx in
      store_with_slot block_frame idx slot v);
    (* 4. Evaluate the body with the populated frame stack. *)
    eval_expr env child_frames body
  | ELetBlockSlots (bindings, slots, body) ->
    if List.length bindings <> List.length slots
    then failwith "internal: slot list length mismatch in ELetBlockSlots";
    (* 1. Allocate frame with the provided, *typed* slot layout. *)
    let block_frame = Frame_env.alloc_packed slots in
    let child_frames = block_frame :: frames in
    (* 2. Evaluate each RHS sequentially and store using the precise setter. *)
    List.iteri bindings ~f:(fun idx (_nm, rhs_expr) ->
      let v = finish_eval child_frames (eval_expr env child_frames rhs_expr) in
      let slot = List.nth_exn slots idx in
      store_with_slot block_frame idx slot v);
    (* 3. Evaluate the body with the populated frame. *)
    eval_expr env child_frames body
  | EApp (fn_expr, arg_exprs) ->
    (* Evaluate fn first (fully, so no nested tailcall escapes). *)
    let fn_val = finish_eval frames (eval_expr env frames fn_expr) in
    (* Evaluate each argument fully. *)
    let arg_vals =
      List.map arg_exprs ~f:(fun a -> finish_eval frames (eval_expr env frames a))
    in
    (match fn_val with
     | VClosure cl ->
       (* Produce a tail call in case we are in tail position of the caller. *)
       TailCall (cl, arg_vals)
     | VBuiltin bf -> Value (bf arg_vals)
     | _ -> failwith "Trying to call a non-function value")
  | EIf (cond_expr, then_expr, else_expr) ->
    let cond_val = finish_eval frames (eval_expr env frames cond_expr) in
    (match cond_val with
     | VBool true -> eval_expr env frames then_expr
     | VBool false -> eval_expr env frames else_expr
     | _ -> failwith "If condition must be bool")
  | EWhile (cond_expr, body_expr) ->
    let rec loop () =
      let cval = finish_eval frames (eval_expr env frames cond_expr) in
      match cval with
      | VBool true ->
        ignore (finish_eval frames (eval_expr env frames body_expr));
        loop ()
      | VBool false -> Value VUnit
      | _ -> failwith "While condition must be bool"
    in
    loop ()
  | ELetIn (_x, e1, e2) ->
    (* Evaluate RHS first (variable is not in scope). *)
    let v1 = finish_eval frames (eval_expr env frames e1) in
    (* Choose slot kind from the *runtime* value we just obtained. *)
    let slot = slot_of_value v1 in
    (* Allocate the frame using the slot that matches the *runtime* value
       we just computed.  Using the dedicated helper removes the need for
       an intermediate homogeneous list. *)
    let fr = Frame_env.alloc_packed [ slot ] in
    store_with_slot fr 0 slot v1;
    (* Push the frame and evaluate the body. *)
    let child_frames = fr :: frames in
    eval_expr env child_frames e2
  | ELetRec (bindings, body) ->
    (* 1.  Pick a slot descriptor for every mutually-recursive binding based on
       the syntactic form of its RHS.  *)
    let slots =
      List.map bindings ~f:(fun (_nm, rhs_expr) -> slot_of_expr rhs_expr.value)
    in
    (* 2.  Allocate a frame with the exact slot layout. *)
    let rec_frame = Frame_env.alloc_packed slots in
    (* 3.  OCaml semantics: each name is visible to the others during the
       evaluation of their RHS.  We insert a temporary [VUnit] so that a
       function referring to itself does not fail with an unbound variable. *)
    List.iteri bindings ~f:(fun idx _binding ->
      Frame_env.set_obj rec_frame idx (Obj.repr VUnit));
    let child_frames = rec_frame :: frames in
    (* 4.  Evaluate each RHS and overwrite the placeholder with the real
       value, using the specialised setter that matches its slot. *)
    List.iteri bindings ~f:(fun idx (_nm, rhs_expr) ->
      let v = finish_eval child_frames (eval_expr env child_frames rhs_expr) in
      let slot = List.nth_exn slots idx in
      store_with_slot rec_frame idx slot v);
    (* 5.  Evaluate the body with the populated frame. *)
    eval_expr env child_frames body
  | ELetRecSlots (bindings, slots, body) ->
    if List.length bindings <> List.length slots
    then failwith "internal: slot list length mismatch in ELetRecSlots";
    (* 1. Allocate frame with exact slot layout. *)
    let rec_frame = Frame_env.alloc_packed slots in
    (* 2. Insert temporary placeholders so that recursive RHSs can see each other. *)
    List.iteri bindings ~f:(fun idx _ -> Frame_env.set_obj rec_frame idx (Obj.repr VUnit));
    let child_frames = rec_frame :: frames in
    (* 3. Evaluate RHSs and overwrite placeholders with real values. *)
    List.iteri bindings ~f:(fun idx (_nm, rhs_expr) ->
      let v = finish_eval child_frames (eval_expr env child_frames rhs_expr) in
      let slot = List.nth_exn slots idx in
      store_with_slot rec_frame idx slot v);
    (* 4. Evaluate body. *)
    eval_expr env child_frames body
  | EMatch (scrut_expr, cases) ->
    let sv = finish_eval frames (eval_expr env frames scrut_expr) in
    match_eval env frames sv cases
  | EMatchSlots (scrut_expr, cases) ->
    let sv = finish_eval frames (eval_expr env frames scrut_expr) in
    match_eval_slots env frames sv cases
  | ERecord fields ->
    let tbl = Hashtbl.create (module String) in
    List.iter fields ~f:(fun (fld, fe) ->
      let fv = finish_eval frames (eval_expr env frames fe) in
      Hashtbl.set tbl ~key:fld ~data:fv);
    Value (VRecord tbl)
  | EFieldGet (rec_expr, field) ->
    let rec_val = finish_eval frames (eval_expr env frames rec_expr) in
    (match rec_val with
     | VRecord tbl ->
       (match Hashtbl.find tbl field with
        | Some v -> Value v
        | None -> failwith (Printf.sprintf "No field '%s' in record" field))
     | VModule menv ->
       (match find_var menv field with
        | Some v -> Value v
        | None -> failwith (Printf.sprintf "No field '%s' in module" field))
     | _ -> failwith "Field access on non-record/non-module")
  | EFieldSet (rec_expr, field, new_expr) ->
    let rec_val = finish_eval frames (eval_expr env frames rec_expr) in
    let new_val = finish_eval frames (eval_expr env frames new_expr) in
    (match rec_val with
     | VRecord tbl ->
       Hashtbl.set tbl ~key:field ~data:new_val;
       Value VUnit
     | _ -> failwith "Field set on non-record")
  | EVariant (tag, exprs) ->
    let vals =
      List.map exprs ~f:(fun ex -> finish_eval frames (eval_expr env frames ex))
    in
    Value (VVariant (tag, vals))
  | EArray elts ->
    let arr_vals =
      List.map elts ~f:(fun e -> finish_eval frames (eval_expr env frames e))
    in
    Value (VArray (Array.of_list arr_vals))
  | EArrayGet (arr_expr, idx_expr) ->
    let arr_val = finish_eval frames (eval_expr env frames arr_expr) in
    let idx_val = finish_eval frames (eval_expr env frames idx_expr) in
    (match arr_val, idx_val with
     | VArray arr, VInt i ->
       if i < 0 || i >= Array.length arr
       then failwith "Array index out of bounds"
       else Value arr.(i)
     | _ -> failwith "Invalid array access")
  | EArraySet (arr_expr, idx_expr, v_expr) ->
    let arr_val = finish_eval frames (eval_expr env frames arr_expr) in
    let idx_val = finish_eval frames (eval_expr env frames idx_expr) in
    let new_val = finish_eval frames (eval_expr env frames v_expr) in
    (match arr_val, idx_val with
     | VArray arr, VInt i ->
       if i < 0 || i >= Array.length arr
       then failwith "Array index out of bounds"
       else (
         arr.(i) <- new_val;
         Value VUnit)
     | _ -> failwith "Invalid array set")
  | ERef e1 ->
    let v1 = finish_eval frames (eval_expr env frames e1) in
    Value (VRef (ref v1))
  | ESetRef (ref_expr, new_expr) ->
    let r = finish_eval frames (eval_expr env frames ref_expr) in
    let nv = finish_eval frames (eval_expr env frames new_expr) in
    (match r with
     | VRef cell ->
       cell := nv;
       Value VUnit
     | _ -> failwith "Attempting to set a non-ref value")
  | EDeref e1 ->
    let rv = finish_eval frames (eval_expr env frames e1) in
    (match rv with
     | VRef cell -> Value !cell
     | _ -> failwith "Deref on non-ref value")
  | ESequence (e1, e2) ->
    let _ = eval_expr env frames e1 in
    (* evaluate e1, discard its result *)
    eval_expr env frames e2 (* then evaluate and return e2 *)
  | ERecordExtend (base_expr, fields) ->
    (* Evaluate the base record *)
    let base_val = finish_eval frames (eval_expr env frames base_expr) in
    let base_tbl =
      match base_val with
      | VRecord tbl -> tbl
      | _ -> failwith "Record extension base is not a record"
    in
    (* Copy existing fields to a new table *)
    let new_tbl = Hashtbl.copy base_tbl in
    (* Evaluate new field expressions and replace/insert. *)
    List.iter fields ~f:(fun (fld, fe) ->
      let fv = finish_eval frames (eval_expr env frames fe) in
      Hashtbl.set new_tbl ~key:fld ~data:fv);
    Value (VRecord new_tbl)

and match_eval
      (env : env)
      (frames : Frame_env.env)
      (v : value)
      (cases : (pattern * expr node) list)
  : eval_result
  =
  match cases with
  | [] -> failwith "Non-exhaustive pattern match"
  | (pat, rhs) :: tl ->
    (match match_pattern v pat with
     | None -> match_eval env frames v tl
     | Some binds ->
       (* 1. Allocate a slot frame for the variables bound by the pattern *)
       let vars = collect_pattern_vars pat in
       (* Choose slot for each bound variable based on the actual runtime
          value now that we have evaluated the scrutinee. *)
       (* Use generic SObj slots to keep in sync with resolver which
          currently annotates pattern-bound variables as SObj.  Using a
          specialised setter here would mismatch the descriptor stored in
          [var_loc] and lead to incorrect coercions when the variable is
          read back. *)
       let slots = List.map vars ~f:(fun _ -> Frame_env.Slot Frame_env.SObj) in
       let pat_frame = Frame_env.alloc_packed slots in
       (* Build var -> index table for quick assignment. *)
       let idx_tbl = Hashtbl.create (module String) in
       List.iteri vars ~f:(fun idx vnm -> Hashtbl.set idx_tbl ~key:vnm ~data:idx);
       List.iter binds ~f:(fun (nm, vl) ->
         match Hashtbl.find idx_tbl nm with
         | Some idx ->
           let slot = List.nth_exn slots idx in
           store_with_slot pat_frame idx slot vl
         | None -> ());
       let child_frames = pat_frame :: frames in
       eval_expr env child_frames rhs)

and match_eval_slots
      (env : env)
      (frames : Frame_env.env)
      (v : value)
      (cases : (pattern * Frame_env.packed_slot list * expr node) list)
  : eval_result
  =
  match cases with
  | [] -> failwith "Non-exhaustive pattern match"
  | (pat, slots, rhs) :: tl ->
    (match match_pattern v pat with
     | None -> match_eval_slots env frames v tl
     | Some binds ->
       let vars = collect_pattern_vars pat in
       if List.length vars <> List.length slots
       then failwith "Resolver/internal error: slot/var length mismatch in EMatchSlots";
       (* All pattern-bound identifiers are resolved to [EVarLoc] whose
          addresses point into the freshly allocated [pat_frame].  We do
          not need to touch the string-keyed environment. *)
       let child_env = env in
       (* 2. Allocate frame using provided slots *)
       let pat_frame = Frame_env.alloc_packed slots in
       (* Map var name to index *)
       List.iteri vars ~f:(fun idx vnm ->
         match List.Assoc.find binds ~equal:String.equal vnm with
         | Some vl ->
           let slot = List.nth_exn slots idx in
           store_with_slot pat_frame idx slot vl
         | None -> ());
       let child_frames = pat_frame :: frames in
       eval_expr child_env child_frames rhs)

(***************************************************************************)
(* 7) Statement Evaluation                                                 *)
(***************************************************************************)

and eval_stmt (env : env) (frames : Frame_env.env) (s : stmt node) : unit =
  match s.value with
  | SLet (x, e1) ->
    let v1 = finish_eval frames (eval_expr env frames e1) in
    set_var env x v1
  | SLetRec bindings ->
    (* Step 1: Initialize each name to VUnit in the top-level env. *)
    List.iter bindings ~f:(fun (nm, _) -> set_var env nm VUnit);
    (* Step 2: Evaluate each binding in env. *)
    List.iter bindings ~f:(fun (nm, rhs_expr) ->
      let v = finish_eval frames (eval_expr env frames rhs_expr) in
      set_var env nm v)
  | SModule (mname, stmts) ->
    let menv = create_env () in
    Hashtbl.iteri env ~f:(fun ~key ~data -> set_var menv key data);
    List.iter stmts ~f:(fun st -> eval_stmt menv frames st);
    set_var env mname (VModule menv)
  | SOpen mname ->
    (match find_var env mname with
     | Some (VModule menv) ->
       Hashtbl.iteri menv ~f:(fun ~key ~data -> set_var env key data)
     | _ -> failwith (Printf.sprintf "Cannot open non-module '%s'" mname))
  | SExpr e -> ignore (finish_eval frames (eval_expr env frames e))
;;

(***************************************************************************)
(* 8) Program Evaluation                                                   *)
(***************************************************************************)

(** [eval_program env (stmts, module_name)] interprets a ChatML module.

    The function mutates [env] in-place, inserting every value declared in
    the module at top-level.  Evaluation proceeds in declaration order and
    uses an empty frame stack – the interpreter never pushes frames for
    the toplevel, therefore side-effects visible outside the call happen
    solely through [env].

    Example executing a minimal program that prints {e 42} using a
    builtin [{!Chatml_builtin_modules.print_int}]:
    {[
      let open Chatochat.Chatml in
      let env = Chatml_lang.create_env () in
      (* Register primitives *)
      Hashtbl.set env ~key:"print_int" ~data:(VBuiltin (function
        | [ VInt n ] -> print_endline (Int.to_string n); VUnit
        | _ -> failwith "invalid call"));

      let prog : Chatml_lang.program =
        ( [ { Chatml_lang.value = Chatml_lang.SExpr
                ( { value = Chatml_lang.EApp
                    ( { value = Chatml_lang.EVar "print_int"; span = Source.dummy }
                    , [ { value = Chatml_lang.EInt 42; span = Source.dummy } ] )
                ; span = Source.dummy } )
            ; span = Source.dummy }
          ]
        , "Main" )
      in
      Chatml_lang.eval_program env prog
    ]}
    After the call, the program has printed [42] and [env] contains the
    bindings introduced by the program (none in this example).
*)
let eval_program (env : env) (prog : program) : unit =
  let initial_frames : Frame_env.env = [] in
  List.iter (fst prog) ~f:(fun stmt_node -> eval_stmt env initial_frames stmt_node)
;;
