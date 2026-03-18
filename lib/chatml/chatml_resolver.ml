(** ChatML resolver — lexical-address resolution & slot selection. *)

open Core
open Chatml
module L = Chatml_lang

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

let type_lookup_ref : (Source.span -> Chatml_typechecker.typ option) option ref = ref None

let lookup_type span =
  match !type_lookup_ref with
  | None -> failwith "internal: resolver used without checked type information"
  | Some f -> f span
;;

let slot_of_typ (t : Chatml_typechecker.typ) : Frame_env.packed_slot =
  match t with
  | Chatml_typechecker.Boolean -> Frame_env.Slot Frame_env.SBool
  | Chatml_typechecker.String -> Frame_env.Slot Frame_env.SString
  | Chatml_typechecker.TInt -> Frame_env.Slot Frame_env.SInt
  | Chatml_typechecker.TFloat -> Frame_env.Slot Frame_env.SFloat
  | _ -> Frame_env.Slot Frame_env.SObj
;;

let choose_slot (rhs_node : L.expr L.node) : Frame_env.packed_slot =
  Chatml_slot_layout.choose_binding_slot rhs_node ~lookup_slot:(fun span ->
    Option.map (lookup_type span) ~f:slot_of_typ)
;;

let with_value (node : L.expr L.node) value = L.{ value; span = node.span }

let with_stmt_value (node : L.stmt L.node) value = L.{ value; span = node.span }

let rec resolve_expr (stack : frame_map list ref) (e : L.expr L.node) : L.resolved_expr =
  match e.value with
  | L.EUnit -> L.REUnit
  | L.EInt i -> L.REInt i
  | L.EBool b -> L.REBool b
  | L.EFloat f -> L.REFloat f
  | L.EString s -> L.REString s
  | L.EVar x ->
    (match lookup !stack x with
     | Some loc -> L.REVarLoc loc
     | None -> L.REVarGlobal x)
  | L.EVarLoc loc -> L.REVarLoc loc
  | L.EPrim1 (prim, arg) ->
    let arg' = resolve_expr stack arg in
    L.REPrim1 (prim, with_value arg arg')
  | L.EPrim2 (prim, lhs, rhs) ->
    let lhs' = resolve_expr stack lhs in
    let rhs' = resolve_expr stack rhs in
    L.REPrim2 (prim, with_value lhs lhs', with_value rhs rhs')
  | L.ELambdaSlots (params, slots, body) ->
    let fm = Hashtbl.create (module String) in
    List.iteri params ~f:(fun idx param ->
      Hashtbl.set fm ~key:param ~data:{ index = idx; slot = List.nth_exn slots idx });
    push_frame stack fm;
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.RELambda (params, slots, with_value body body')
  | L.ELambda (params, body) ->
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
    L.RELambda (params, param_slots, with_value body body')
  | L.EApp (fn, args) ->
    let fn' = resolve_expr stack fn in
    let args' = List.map args ~f:(fun arg -> with_value arg (resolve_expr stack arg)) in
    L.REApp (with_value fn fn', args')
  | L.ELetIn (x, rhs, body) ->
    let rec collect (acc : (string * L.expr L.node) list) (expr_node : L.expr L.node)
      : (string * L.expr L.node) list * L.expr L.node
      =
      match expr_node.value with
      | L.ELetIn (y, rhs_y, body_y)
        when not (List.exists acc ~f:(fun (n, _) -> String.equal n y)) ->
        collect ((y, rhs_y) :: acc) body_y
      | _ -> List.rev acc, expr_node
    in
    let bindings, tail_body = collect [ x, rhs ] body in
    let fm : frame_map = Hashtbl.create (module String) in
    push_frame stack fm;
    let resolved_bindings_rev =
      List.foldi bindings ~init:[] ~f:(fun idx acc (nm, rhs_expr) ->
        let rhs_resolved = resolve_expr stack rhs_expr in
        let slot = choose_slot rhs_expr in
        Hashtbl.set fm ~key:nm ~data:{ index = idx; slot };
        (nm, with_value rhs_expr rhs_resolved) :: acc)
    in
    let resolved_bindings = List.rev resolved_bindings_rev in
    let body' = resolve_expr stack tail_body in
    pop_frame stack;
    let slots = List.map bindings ~f:(fun (nm, _) -> (Hashtbl.find_exn fm nm).slot) in
    L.RELetBlock (resolved_bindings, slots, with_value tail_body body')
  | L.ELetRec (binds, body) ->
    let fm = Hashtbl.create (module String) in
    let slots =
      List.mapi binds ~f:(fun idx (nm, rhs_expr) ->
        let slot = choose_slot rhs_expr in
        Hashtbl.set fm ~key:nm ~data:{ index = idx; slot };
        slot)
    in
    push_frame stack fm;
    let binds' =
      List.map binds ~f:(fun (nm, rhs) -> nm, with_value rhs (resolve_expr stack rhs))
    in
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.RELetRec (binds', slots, with_value body body')
  | L.ELetBlock (binds, body) ->
    let fm = Hashtbl.create (module String) in
    let slots =
      List.mapi binds ~f:(fun idx (nm, rhs_node) ->
        let slot = choose_slot rhs_node in
        Hashtbl.set fm ~key:nm ~data:{ index = idx; slot };
        slot)
    in
    push_frame stack fm;
    let binds' =
      List.map binds ~f:(fun (nm, rhs_node) ->
        nm, with_value rhs_node (resolve_expr stack rhs_node))
    in
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.RELetBlock (binds', slots, with_value body body')
  | L.ELetBlockSlots (binds, slots, body) ->
    let fm = Hashtbl.create (module String) in
    List.iteri binds ~f:(fun idx (nm, _rhs) ->
      let slot = List.nth_exn slots idx in
      Hashtbl.set fm ~key:nm ~data:{ index = idx; slot });
    push_frame stack fm;
    let binds' =
      List.map binds ~f:(fun (nm, rhs_node) ->
        nm, with_value rhs_node (resolve_expr stack rhs_node))
    in
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.RELetBlock (binds', slots, with_value body body')
  | L.ELetRecSlots (binds, slots, body) ->
    let fm = Hashtbl.create (module String) in
    List.iteri binds ~f:(fun idx (nm, _rhs) ->
      let slot = List.nth_exn slots idx in
      Hashtbl.set fm ~key:nm ~data:{ index = idx; slot });
    push_frame stack fm;
    let binds' =
      List.map binds ~f:(fun (nm, rhs_node) ->
        nm, with_value rhs_node (resolve_expr stack rhs_node))
    in
    let body' = resolve_expr stack body in
    pop_frame stack;
    L.RELetRec (binds', slots, with_value body body')
  | L.EIf (c, t, f) ->
    let c' = resolve_expr stack c in
    let t' = resolve_expr stack t in
    let f' = resolve_expr stack f in
    L.REIf (with_value c c', with_value t t', with_value f f')
  | L.EWhile (cond, bd) ->
    let cond' = resolve_expr stack cond in
    let bd' = resolve_expr stack bd in
    L.REWhile (with_value cond cond', with_value bd bd')
  | L.ESequence (e1, e2) ->
    let e1' = resolve_expr stack e1 in
    let e2' = resolve_expr stack e2 in
    L.RESequence (with_value e1 e1', with_value e2 e2')
  | L.EMatch (scrut, cases) ->
    let scrut' = resolve_expr stack scrut in
    let scrut_ty_opt = lookup_type scrut.span in
    let rec slots_of_pattern pat ty_opt =
      match pat with
      | L.PUnit -> []
      | L.PVar x ->
        let slot =
          match ty_opt with
          | Some t -> slot_of_typ t
          | None -> Frame_env.Slot Frame_env.SObj
        in
        [ x, slot ]
      | L.PWildcard | L.PInt _ | L.PBool _ | L.PFloat _ | L.PString _ -> []
      | L.PVariant (_tag, subpats) ->
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
        List.concat_map fields ~f:(fun (lbl, p) -> slots_of_pattern p (field_type lbl))
    in
    let cases' =
      List.map cases ~f:(fun case ->
        let var_slots = slots_of_pattern case.pat scrut_ty_opt in
        let vars = List.map var_slots ~f:fst in
        let slots = List.map var_slots ~f:snd in
        let fm : frame_map = Hashtbl.create (module String) in
        List.iteri vars ~f:(fun idx vnm ->
          let slot = List.nth_exn slots idx in
          Hashtbl.set fm ~key:vnm ~data:{ index = idx; slot });
        push_frame stack fm;
        let rhs' = resolve_expr stack case.rhs in
        pop_frame stack;
        { L.pat = case.pat
        ; pat_span = case.pat_span
        ; slots
        ; rhs = with_value case.rhs rhs'
        })
    in
    L.REMatch (with_value scrut scrut', cases')
  | L.EMatchSlots (scrut, cases) ->
    let scrut' = resolve_expr stack scrut in
    let cases' =
      List.map cases ~f:(fun case ->
        let vars = L.collect_pattern_vars case.pat in
        let fm = Hashtbl.create (module String) in
        List.iteri vars ~f:(fun idx vnm ->
          let slot = List.nth_exn case.slots idx in
          Hashtbl.set fm ~key:vnm ~data:{ index = idx; slot });
        push_frame stack fm;
        let rhs' = resolve_expr stack case.rhs in
        pop_frame stack;
        { L.pat = case.pat
        ; pat_span = case.pat_span
        ; slots = case.slots
        ; rhs = with_value case.rhs rhs'
        })
    in
    L.REMatch (with_value scrut scrut', cases')
  | L.ERecord fields ->
    let fields' =
      List.map fields ~f:(fun (lbl, ex) -> lbl, with_value ex (resolve_expr stack ex))
    in
    L.RERecord fields'
  | L.EFieldGet (obj, lbl) ->
    let obj' = resolve_expr stack obj in
    L.REFieldGet (with_value obj obj', lbl)
  | L.EVariant (tag, vs) ->
    let vs' = List.map vs ~f:(fun v -> with_value v (resolve_expr stack v)) in
    L.REVariant (tag, vs')
  | L.EArray elts ->
    let elts' = List.map elts ~f:(fun elt -> with_value elt (resolve_expr stack elt)) in
    L.REArray elts'
  | L.EArrayGet (arr, idx) ->
    let arr' = resolve_expr stack arr in
    let idx' = resolve_expr stack idx in
    L.REArrayGet (with_value arr arr', with_value idx idx')
  | L.EArraySet (arr, idx, v) ->
    let arr' = resolve_expr stack arr in
    let idx' = resolve_expr stack idx in
    let v' = resolve_expr stack v in
    L.REArraySet (with_value arr arr', with_value idx idx', with_value v v')
  | L.ERef e1 ->
    let e1' = resolve_expr stack e1 in
    L.RERef (with_value e1 e1')
  | L.ESetRef (r, v) ->
    let r' = resolve_expr stack r in
    let v' = resolve_expr stack v in
    L.RESetRef (with_value r r', with_value v v')
  | L.EDeref e1 ->
    let e1' = resolve_expr stack e1 in
    L.REDeref (with_value e1 e1')
  | L.ERecordExtend (base, fields) ->
    let base' = resolve_expr stack base in
    let fields' =
      List.map fields ~f:(fun (lbl, ex) -> lbl, with_value ex (resolve_expr stack ex))
    in
    L.RERecordExtend (with_value base base', fields')
;;

let rec resolve_stmt (stack : frame_map list ref) (snode : L.stmt L.node)
  : L.resolved_stmt
  =
  match snode.value with
  | L.SLet (x, rhs) -> L.RSLet (x, with_value rhs (resolve_expr stack rhs))
  | L.SLetRec binds ->
    let binds' =
      List.map binds ~f:(fun (nm, rhs) -> nm, with_value rhs (resolve_expr stack rhs))
    in
    L.RSLetRec binds'
  | L.SExpr e -> L.RSExpr (with_value e (resolve_expr stack e))
  | L.SModule (mname, stmts) ->
    let stmts' = List.map stmts ~f:(fun st -> with_stmt_value st (resolve_stmt stack st)) in
    L.RSModule (mname, stmts')
  | L.SOpen nm -> L.RSOpen nm
;;

let resolve_checked_program
      (checked : Chatml_typechecker.checked_program)
      (prog : L.program)
  : L.resolved_program
  =
  let lookup_fun = Chatml_typechecker.checked_lookup_span_type checked in
  type_lookup_ref := Some lookup_fun;
  let stack = ref [] in
  let stmts' =
    fst prog |> List.map ~f:(fun sn -> with_stmt_value sn (resolve_stmt stack sn))
  in
  type_lookup_ref := None;
  stmts', snd prog
;;

let resolve_program (prog : L.program) : L.resolved_program =
  match Chatml_typechecker.check_program prog with
  | Ok checked -> resolve_checked_program checked prog
  | Error diagnostic ->
    failwith (Chatml_typechecker.format_diagnostic (snd prog) diagnostic)
;;

type eval_error =
  | Type_diagnostic of Chatml_typechecker.diagnostic
  | Runtime_diagnostic of L.runtime_error

let run_program (env : L.env) (prog : L.program) : (unit, eval_error) result =
  match Chatml_typechecker.check_program prog with
  | Error diagnostic -> Error (Type_diagnostic diagnostic)
  | Ok checked ->
    let program = resolve_checked_program checked prog in
    (try
       Chatml_eval.eval_program env program;
       Ok ()
     with
     | L.Runtime_error err -> Error (Runtime_diagnostic err))
;;

let typecheck_resolve_and_eval (env : L.env) (prog : L.program)
  : (unit, Chatml_typechecker.diagnostic) result
  =
  match Chatml_typechecker.check_program prog with
  | Error diagnostic -> Error diagnostic
  | Ok checked ->
    let program = resolve_checked_program checked prog in
    Chatml_eval.eval_program env program;
    Ok ()
;;

let eval_program (env : L.env) (prog : L.program)
  : (unit, Chatml_typechecker.diagnostic) result
  =
  typecheck_resolve_and_eval env prog
;;
