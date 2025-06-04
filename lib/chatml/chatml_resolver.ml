(* chatml_resolver.ml
   -------------------
   Lightweight lexical-address resolver for ChatML.
*)

open Core
open Chatml
module L = Chatml_lang

(* -------------------------------------------------------------------- *)
(*  Helper  – frame stack                                                *)
(* -------------------------------------------------------------------- *)

type slot_info =
  { index : int
  ; slot : Frame_env.packed_slot
  }

type frame_map = (string, slot_info) Hashtbl.t

let push_frame (stack : frame_map list ref) (m : frame_map) : unit = stack := m :: !stack

let pop_frame (stack : frame_map list ref) : unit =
  match !stack with
  | _ :: tl -> stack := tl
  | [] -> failwith "Resolver: attempt to pop empty frame stack"
;;

(* Safe wrapper: push a frame, execute [f], then pop in an ensure-clause so
   that the stack remains well-balanced even if [f] raises. *)
let with_frame (stack : frame_map list ref) (fm : frame_map) ~(f : unit -> 'a) : 'a =
  push_frame stack fm;
  try
    let res = f () in
    pop_frame stack;
    res
  with
  | exn ->
    pop_frame stack;
    raise exn
;;

let lookup (stack : frame_map list) (name : string) : L.var_loc option =
  let rec aux depth = function
    | [] -> None
    | fm :: tl ->
      (match Hashtbl.find fm name with
       | Some info -> Some { L.depth; index = info.index; slot = info.slot }
       | None -> aux (depth + 1) tl)
  in
  aux 0 stack
;;

(* ------------------------------------------------------------- *)
(*  Pattern utilities                                             *)
(* ------------------------------------------------------------- *)

(* -------------------------------------------------------------------- *)
(*  Integration with the static type-checker                             *)
(* -------------------------------------------------------------------- *)

(* The resolver needs to query the principal type of sub-expressions in
   order to choose an efficient slot descriptor.  Instead of peeking into
   the type-checker’s *global* hash-table we now capture a *pure lookup
   closure* produced by [Chatml_typechecker.type_lookup_for_program].
   Keeping it in an [option ref] allows us to thread the function through
   the mutually-recursive [resolve_*] helpers without changing every
   signature. *)

let type_lookup_ref : (Source.span -> Chatml_typechecker.typ option) option ref = ref None

let lookup_type span =
  match !type_lookup_ref with
  | None -> None
  | Some f -> f span
;;

(*************************************************************************)
(* Slot selection                                                        *)
(*************************************************************************)

(** A very small heuristic that inspects the *syntactic* form of an
   expression and picks an appropriate slot descriptor.  This is *not* a
   full type-inference algorithm: we only special-case literal constants
   (ints, bools, floats, strings).  Everything else falls back to [SObj].

   The function is intentionally kept local to the resolver – the
   interpreter re-implements the same logic on values so that it can
   decide which setter to use when storing into a frame.  The two pieces
   of logic must stay in sync but do *not* create a dependency cycle
   between modules. *)
let slot_of_typ (t : Chatml_typechecker.typ) : Frame_env.packed_slot =
  match t with
  | Chatml_typechecker.Boolean -> Frame_env.Slot Frame_env.SBool
  | Chatml_typechecker.String -> Frame_env.Slot Frame_env.SString
  | Chatml_typechecker.TInt -> Frame_env.Slot Frame_env.SInt
  | Chatml_typechecker.TFloat -> Frame_env.Slot Frame_env.SFloat
  | Chatml_typechecker.Number -> Frame_env.Slot Frame_env.SObj
  | _ -> Frame_env.Slot Frame_env.SObj
;;

let fallback_slot_of_expr (e : L.expr) : Frame_env.packed_slot =
  match e with
  | L.EInt _ -> Frame_env.Slot Frame_env.SInt
  | L.EBool _ -> Frame_env.Slot Frame_env.SBool
  | L.EFloat _ -> Frame_env.Slot Frame_env.SFloat
  | L.EString _ -> Frame_env.Slot Frame_env.SString
  | _ -> Frame_env.Slot Frame_env.SObj
;;

let choose_slot (rhs_node : L.expr L.node) : Frame_env.packed_slot =
  match lookup_type rhs_node.span with
  | Some typ -> slot_of_typ typ
  | None -> fallback_slot_of_expr rhs_node.value
;;

let collect_pattern_vars (pat : L.pattern) : string list =
  let rec aux acc p =
    match p with
    | L.PVar x -> x :: acc
    | L.PWildcard | L.PInt _ | L.PBool _ | L.PFloat _ | L.PString _ -> acc
    | L.PVariant (_, ps) -> List.fold ps ~init:acc ~f:aux
    | L.PRecord (fields, _open) ->
      List.fold fields ~init:acc ~f:(fun ac (_, p) -> aux ac p)
  in
  List.rev (aux [] pat)
;;

(* -------------------------------------------------------------------- *)
(*  Expression traversal                                                 *)
(* -------------------------------------------------------------------- *)

let rec resolve_expr (stack : frame_map list ref) (e : L.expr L.node) : L.expr =
  let wrap nexp = { e with value = nexp } in
  match e.value with
  | L.EVar x ->
    (match lookup !stack x with
     | Some loc -> L.EVarLoc loc
     | None -> L.EVar x)
  | L.EVarLoc l -> L.EVarLoc l
  | L.ELambdaSlots (params, slots, body) ->
    (* Already resolved; simply traverse body. *)
    let fm = Hashtbl.create (module String) in
    List.iteri params ~f:(fun idx param ->
      Hashtbl.set fm ~key:param ~data:{ index = idx; slot = List.nth_exn slots idx });
    push_frame stack fm;
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.ELambdaSlots (params, slots, wrap body')
  | L.ELambda (params, body) ->
    (* Derive slot layout from static types when available. *)
    let param_slots =
      match lookup_type e.span with
      | Some (Chatml_typechecker.Fun (param_tys, _))
        when List.length param_tys = List.length params ->
        List.map param_tys ~f:slot_of_typ
      | _ -> List.map params ~f:(fun _ -> Frame_env.Slot Frame_env.SObj)
    in
    let fm = Hashtbl.create (module String) in
    List.iteri params ~f:(fun idx param ->
      let slot = List.nth_exn param_slots idx in
      Hashtbl.set fm ~key:param ~data:{ index = idx; slot });
    push_frame stack fm;
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.ELambdaSlots (params, param_slots, wrap body')
  | L.EApp (fn, args) ->
    let fn' = resolve_expr stack fn in
    let args' = List.map args ~f:(fun a -> { a with value = resolve_expr stack a }) in
    L.EApp ({ fn with value = fn' }, args')
  | L.ELetIn (x, rhs, body) ->
    (* Merge a chain of consecutive [let]s into a single [ELetBlock].
       We first collect all nested bindings until we hit a non-let body. *)
    let rec collect (acc : (string * L.expr L.node) list) (expr_node : L.expr L.node)
      : (string * L.expr L.node) list * L.expr L.node
      =
      match expr_node.value with
      | L.ELetIn (y, rhs_y, body_y)
        when not (List.exists acc ~f:(fun (n, _) -> String.equal n y)) ->
        collect ((y, rhs_y) :: acc) body_y
      | _ -> List.rev acc, expr_node
    in
    let bindings_rev, tail_body = collect [ x, rhs ] body in
    let bindings = bindings_rev in
    (* 1. Create an empty frame-map that we will fill incrementally. *)
    let fm : frame_map = Hashtbl.create (module String) in
    (* 2. Push the frame before processing any RHS so that earlier
          bindings become visible to later ones. *)
    push_frame stack fm;
    (* 3. Resolve each RHS in order, adding the bound name to [fm]
          *after* its own RHS has been processed (non-rec semantics). *)
    let resolved_bindings_rev =
      List.foldi bindings ~init:[] ~f:(fun idx acc (nm, rhs_expr) ->
        (* Resolve RHS with current mapping (earlier vars visible). *)
        let rhs_resolved = resolve_expr stack rhs_expr in
        (* Choose slot using type information when available. *)
        let slot = choose_slot rhs_expr in
        (* Register variable in the frame map for subsequent RHS + body. *)
        Hashtbl.set fm ~key:nm ~data:{ index = idx; slot };
        (nm, { rhs_expr with value = rhs_resolved }) :: acc)
    in
    let resolved_bindings = List.rev resolved_bindings_rev in
    (* 4. Resolve the body with full mapping in scope. *)
    let body' = resolve_expr stack tail_body in
    (* 5. Pop the frame when leaving the block scope. *)
    pop_frame stack;
    let slots =
      List.map bindings ~f:(fun (nm, _rhs) ->
        (* fetch slot from fm *)
        (Hashtbl.find_exn fm nm).slot)
    in
    L.ELetBlockSlots (resolved_bindings, slots, wrap body')
  | L.ELetRec (binds, body) ->
    (* Mutually-recursive let-resolution, unchanged from earlier code but
       repositioned after the new [ELetBlock] arm. *)
    let fm = Hashtbl.create (module String) in
    let slots =
      List.mapi binds ~f:(fun idx (nm, rhs_expr) ->
        let slot = choose_slot rhs_expr in
        Hashtbl.set fm ~key:nm ~data:{ index = idx; slot };
        slot)
    in
    push_frame stack fm;
    let binds' =
      List.map binds ~f:(fun (nm, rhs) -> nm, { rhs with value = resolve_expr stack rhs })
    in
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.ELetRecSlots (binds', slots, wrap body')
  | L.ELetBlock (binds, body) ->
    (* Convert to [ELetBlockSlots] and traverse children. *)
    let fm = Hashtbl.create (module String) in
    let slots =
      List.mapi binds ~f:(fun idx (nm, rhs_node) ->
        let slot = choose_slot rhs_node in
        Hashtbl.set fm ~key:nm ~data:{ index = idx; slot };
        slot)
    in
    push_frame stack fm;
    let binds' =
      List.mapi binds ~f:(fun _idx (nm, rhs_node) ->
        nm, { rhs_node with value = resolve_expr stack rhs_node })
    in
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.ELetBlockSlots (binds', slots, wrap body')
  | L.ELetBlockSlots (binds, slots, body) ->
    (* Already resolved; traverse children preserving slot info. *)
    let fm = Hashtbl.create (module String) in
    List.iteri binds ~f:(fun idx (nm, _rhs) ->
      let slot = List.nth_exn slots idx in
      Hashtbl.set fm ~key:nm ~data:{ index = idx; slot });
    push_frame stack fm;
    let binds' =
      List.mapi binds ~f:(fun _ (nm, rhs_node) ->
        nm, { rhs_node with value = resolve_expr stack rhs_node })
    in
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.ELetBlockSlots (binds', slots, wrap body')
  | L.ELetRecSlots (binds, slots, body) ->
    let fm = Hashtbl.create (module String) in
    List.iteri binds ~f:(fun idx (nm, _rhs) ->
      let slot = List.nth_exn slots idx in
      Hashtbl.set fm ~key:nm ~data:{ index = idx; slot });
    push_frame stack fm;
    let binds' =
      List.mapi binds ~f:(fun _ (nm, rhs_node) ->
        nm, { rhs_node with value = resolve_expr stack rhs_node })
    in
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.ELetRecSlots (binds', slots, wrap body')
  | L.EIf (c, t, f) ->
    let c' = resolve_expr stack c in
    let t' = resolve_expr stack t in
    let f' = resolve_expr stack f in
    L.EIf (wrap c', wrap t', wrap f')
  | L.EWhile (cond, bd) ->
    let cond' = resolve_expr stack cond in
    let bd' = resolve_expr stack bd in
    L.EWhile (wrap cond', wrap bd')
  | L.ESequence (e1, e2) ->
    let e1' = resolve_expr stack e1 in
    let e2' = resolve_expr stack e2 in
    L.ESequence (wrap e1', wrap e2')
  | L.EMatch (scrut, cases) ->
    let scrut' = resolve_expr stack scrut in
    let scrut_ty_opt = lookup_type scrut.span in
    (* Helper: collect (var, slot) list in deterministic order *)
    let rec slots_of_pattern pat ty_opt =
      match pat with
      | L.PVar x ->
        let slot =
          match ty_opt with
          | Some t -> slot_of_typ t
          | None -> Frame_env.Slot Frame_env.SObj
        in
        [ x, slot ]
      | L.PWildcard | L.PInt _ | L.PBool _ | L.PFloat _ | L.PString _ -> []
      | L.PVariant (_tag, subpats) ->
        (* We do not try to refine variant arguments for now. *)
        List.concat_map subpats ~f:(fun sp -> slots_of_pattern sp None)
      | L.PRecord (fields, _open) ->
        let field_type lbl =
          let rec search = function
            | Some (Chatml_typechecker.Record row) -> lookup_field row lbl
            | Some (Chatml_typechecker.Row (fs, tail)) ->
              (match Chatml_typechecker.Env.find fs lbl with
               | Some t -> Some t
               | None -> search (Some tail))
            | Some (Chatml_typechecker.Var { contents = Chatml_typechecker.Bound t }) ->
              search (Some t)
            | _ -> None
          and lookup_field row lbl =
            match row with
            | Chatml_typechecker.Row (fs, tail) ->
              (match Chatml_typechecker.Env.find fs lbl with
               | Some t -> Some t
               | None -> lookup_field tail lbl)
            | Chatml_typechecker.Var { contents = Chatml_typechecker.Bound t } ->
              lookup_field t lbl
            | _ -> None
          in
          search ty_opt
        in
        List.concat_map fields ~f:(fun (lbl, p) ->
          let sub_ty = field_type lbl in
          slots_of_pattern p sub_ty)
    in
    let cases' =
      List.map cases ~f:(fun (pat, rhs) ->
        let var_slots = slots_of_pattern pat scrut_ty_opt in
        let vars = List.map var_slots ~f:fst in
        let slots = List.map var_slots ~f:snd in
        (* Build frame map with those slots *)
        let fm : frame_map = Hashtbl.create (module String) in
        List.iteri vars ~f:(fun idx vnm ->
          let slot = List.nth_exn slots idx in
          Hashtbl.set fm ~key:vnm ~data:{ index = idx; slot });
        push_frame stack fm;
        let rhs' = resolve_expr stack rhs in
        pop_frame stack;
        pat, slots, { rhs with value = rhs' })
    in
    L.EMatchSlots (wrap scrut', cases')
  | L.EMatchSlots (scrut, cases) ->
    (* AST already contains slot info; we only need to recursively resolve
       sub-expressions to keep idempotence. *)
    let scrut' = resolve_expr stack scrut in
    let cases' =
      List.map cases ~f:(fun (pat, slots, rhs) ->
        let vars = collect_pattern_vars pat in
        let fm = Hashtbl.create (module String) in
        List.iteri vars ~f:(fun idx vnm ->
          let slot = List.nth_exn slots idx in
          Hashtbl.set fm ~key:vnm ~data:{ index = idx; slot });
        push_frame stack fm;
        let rhs' = resolve_expr stack rhs in
        pop_frame stack;
        pat, slots, { rhs with value = rhs' })
    in
    L.EMatchSlots (wrap scrut', cases')
  | L.ERecord fields ->
    let fields' =
      List.map fields ~f:(fun (lbl, ex) -> lbl, { ex with value = resolve_expr stack ex })
    in
    L.ERecord fields'
  | L.EFieldGet (obj, lbl) ->
    let obj' = resolve_expr stack obj in
    L.EFieldGet (wrap obj', lbl)
  | L.EFieldSet (obj, lbl, v) ->
    let obj' = resolve_expr stack obj in
    let v' = resolve_expr stack v in
    L.EFieldSet (wrap obj', lbl, wrap v')
  | L.EVariant (tag, vs) ->
    let vs' = List.map vs ~f:(fun v -> { v with value = resolve_expr stack v }) in
    L.EVariant (tag, vs')
  | L.EArray elts ->
    let elts' = List.map elts ~f:(fun e -> { e with value = resolve_expr stack e }) in
    L.EArray elts'
  | L.EArrayGet (arr, idx) ->
    let arr' = resolve_expr stack arr in
    let idx' = resolve_expr stack idx in
    L.EArrayGet (wrap arr', wrap idx')
  | L.EArraySet (arr, idx, v) ->
    let arr' = resolve_expr stack arr in
    let idx' = resolve_expr stack idx in
    let v' = resolve_expr stack v in
    L.EArraySet (wrap arr', wrap idx', wrap v')
  | L.ERef e1 ->
    let e1' = resolve_expr stack e1 in
    L.ERef (wrap e1')
  | L.ESetRef (r, v) ->
    let r' = resolve_expr stack r in
    let v' = resolve_expr stack v in
    L.ESetRef (wrap r', wrap v')
  | L.EDeref e1 ->
    let e1' = resolve_expr stack e1 in
    L.EDeref (wrap e1')
  | L.ERecordExtend (base, fields) ->
    let base' = resolve_expr stack base in
    let fields' =
      List.map fields ~f:(fun (lbl, ex) -> lbl, { ex with value = resolve_expr stack ex })
    in
    L.ERecordExtend (wrap base', fields')
  (* Literals – no change ------------------------------------------------ *)
  | L.EInt _ | L.EBool _ | L.EFloat _ | L.EString _ -> e.value
;;

(* -------------------------------------------------------------------- *)
(*  Statement traversal                                                  *)
(* -------------------------------------------------------------------- *)

let rec resolve_stmt (stack : frame_map list ref) (snode : L.stmt L.node) : L.stmt =
  match snode.value with
  | L.SLet (x, rhs) ->
    let rhs' = resolve_expr stack rhs in
    L.SLet (x, { rhs with value = rhs' })
  | L.SLetRec binds ->
    let binds' =
      List.map binds ~f:(fun (nm, rhs) -> nm, { rhs with value = resolve_expr stack rhs })
    in
    L.SLetRec binds'
  | L.SExpr e -> L.SExpr { e with value = resolve_expr stack e }
  | L.SModule (mname, stmts) ->
    let stmts' =
      List.map stmts ~f:(fun st -> { st with value = resolve_stmt stack st })
    in
    L.SModule (mname, stmts')
  | L.SOpen nm -> L.SOpen nm
;;

(* -------------------------------------------------------------------- *)
(*  Public entry-point                                                   *)
(* -------------------------------------------------------------------- *)

let resolve_program (prog : L.program) : L.program =
  (* 1.  Obtain a *pure* span→type lookup closure for this very program. *)
  let lookup_fun = Chatml_typechecker.type_lookup_for_program prog in
  (* 2.  Expose it to the recursive helpers. *)
  type_lookup_ref := Some lookup_fun;
  (* 3.  Proceed with the original resolution. *)
  let stack = ref [] in
  let stmts' =
    fst prog |> List.map ~f:(fun sn -> { sn with value = resolve_stmt stack sn })
  in
  (* 4.  Clear the lookup ref to avoid accidental reuse across programs. *)
  type_lookup_ref := None;
  stmts', snd prog
;;

let eval_program (env : L.env) (prog : L.program) : unit =
  let program = resolve_program prog in
  (* print_s [%message "Resolved program" (program : L.program)]; *)
  (* 1.  Resolve the program. *)
  (* 2.  Execute it in the given environment. *)
  L.eval_program env program
;;
