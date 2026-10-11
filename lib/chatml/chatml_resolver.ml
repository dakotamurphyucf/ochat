(** ChatML resolver — lexical-address resolution & slot selection. *)

open Core
open Chatml
module L = Chatml_lang
module Binding_pattern = Chatml_binding_pattern

type slot_info =
  { index : int
  ; slot : Frame_env.packed_slot
  }

type frame_map = (string, slot_info) Hashtbl.t

type resolve_ctx =
  { lookup_type : Source.span -> Chatml_typechecker.typ option
  ; lookup_module_visibility : Source.span -> Chatml_module_visibility.t option
  ; stack : frame_map list ref
  }

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

let slot_of_typ (t : Chatml_typechecker.typ) : Frame_env.packed_slot =
  match Chatml_typechecker.resolve_bound_type t with
  | Chatml_typechecker.Boolean -> Frame_env.Slot Frame_env.SBool
  | Chatml_typechecker.String -> Frame_env.Slot Frame_env.SString
  | Chatml_typechecker.TInt -> Frame_env.Slot Frame_env.SInt
  | Chatml_typechecker.TFloat -> Frame_env.Slot Frame_env.SFloat
  | Chatml_typechecker.Con ("task", _) -> Frame_env.Slot Frame_env.SObj
  | _ -> Frame_env.Slot Frame_env.SObj
;;

let choose_slot (ctx : resolve_ctx) (rhs_node : L.expr L.node) : Frame_env.packed_slot =
  Chatml_slot_layout.choose_binding_slot rhs_node ~lookup_slot:(fun span ->
    Option.map (ctx.lookup_type span) ~f:slot_of_typ)
;;

let with_value (node : L.expr L.node) value = L.{ value; span = node.span }
let with_stmt_value (node : L.stmt L.node) value = L.{ value; span = node.span }

let rec resolve_expr (ctx : resolve_ctx) (e : L.expr L.node) : L.resolved_expr =
  match e.value with
  | L.EUnit -> L.REUnit
  | L.EInt i -> L.REInt i
  | L.EBool b -> L.REBool b
  | L.EFloat f -> L.REFloat f
  | L.EString s -> L.REString s
  | L.EVar x ->
    (match lookup !(ctx.stack) x with
     | Some loc -> L.REVarLoc loc
     | None -> L.REVarGlobal x)
  | L.EVarLoc loc -> L.REVarLoc loc
  | L.EPrim1 (prim, arg) ->
    let arg' = resolve_expr ctx arg in
    L.REPrim1 (prim, with_value arg arg')
  | L.EPrim2 (prim, lhs, rhs) ->
    let lhs' = resolve_expr ctx lhs in
    let rhs' = resolve_expr ctx rhs in
    L.REPrim2 (prim, with_value lhs lhs', with_value rhs rhs')
  | L.EAnnot (expr_node, _type_expr) -> resolve_expr ctx expr_node
  | L.ELambdaSlots (params, slots, body) -> resolve_lambda ctx params (Some slots) body
  | L.ELambda (params, body) -> resolve_lambda ctx params None body
  | L.EApp (fn, args) ->
    let fn' = resolve_expr ctx fn in
    let args' = List.map args ~f:(fun arg -> with_value arg (resolve_expr ctx arg)) in
    L.REApp (with_value fn fn', args')
  | L.ELetIn (pattern, rhs, body) ->
    let names pattern = Binding_pattern.binders pattern |> List.map ~f:fst in
    let rec collect acc seen (expr_node : L.expr L.node) =
      match expr_node.value with
      | L.ELetIn (pattern, rhs, body)
        when not (List.exists (names pattern) ~f:(Set.mem seen)) ->
        let seen = List.fold (names pattern) ~init:seen ~f:Set.add in
        collect ((pattern, rhs) :: acc) seen body
      | _ -> List.rev acc, expr_node
    in
    let bindings, tail_body =
      collect [ pattern, rhs ] (String.Set.of_list (names pattern)) body
    in
    resolve_let_block ctx bindings None tail_body
  | L.ELetRec (binds, body) ->
    let fm = Hashtbl.create (module String) in
    let slots =
      List.mapi binds ~f:(fun idx (nm, rhs_expr) ->
        let slot = choose_slot ctx rhs_expr in
        Hashtbl.set fm ~key:nm ~data:{ index = idx; slot };
        slot)
    in
    push_frame ctx.stack fm;
    let binds' =
      List.map binds ~f:(fun (nm, rhs) -> nm, with_value rhs (resolve_expr ctx rhs))
    in
    let body' = resolve_expr ctx body in
    pop_frame ctx.stack;
    L.RELetRec (binds', slots, with_value body body')
  | L.ELetBlock (binds, body) -> resolve_let_block ctx binds None body
  | L.ELetBlockSlots (binds, slots, body) -> resolve_let_block ctx binds (Some slots) body
  | L.ELetRecSlots (binds, slots, body) ->
    let fm = Hashtbl.create (module String) in
    List.iteri binds ~f:(fun idx (nm, _rhs) ->
      let slot = List.nth_exn slots idx in
      Hashtbl.set fm ~key:nm ~data:{ index = idx; slot });
    push_frame ctx.stack fm;
    let binds' =
      List.map binds ~f:(fun (nm, rhs_node) ->
        nm, with_value rhs_node (resolve_expr ctx rhs_node))
    in
    let body' = resolve_expr ctx body in
    pop_frame ctx.stack;
    L.RELetRec (binds', slots, with_value body body')
  | L.EIf (c, t, f) ->
    let c' = resolve_expr ctx c in
    let t' = resolve_expr ctx t in
    let f' = resolve_expr ctx f in
    L.REIf (with_value c c', with_value t t', with_value f f')
  | L.EWhile (cond, bd) ->
    let cond' = resolve_expr ctx cond in
    let bd' = resolve_expr ctx bd in
    L.REWhile (with_value cond cond', with_value bd bd')
  | L.ESequence (e1, e2) ->
    let e1' = resolve_expr ctx e1 in
    let e2' = resolve_expr ctx e2 in
    L.RESequence (with_value e1 e1', with_value e2 e2')
  | L.EMatch (scrut, cases) ->
    let scrut' = resolve_expr ctx scrut in
    let scrut_ty_opt = ctx.lookup_type scrut.span in
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
      | L.PTuple patterns ->
        let patterns = Chatml_tuple.to_list patterns in
        (match
           Option.bind ty_opt ~f:(fun ty -> Chatml_typechecker.tuple_component_types ty)
         with
         | Some components when List.length patterns = List.length components ->
           List.concat
             (List.map2_exn patterns components ~f:(fun pattern ty ->
                slots_of_pattern pattern (Some ty)))
         | None | Some _ ->
           List.concat_map patterns ~f:(fun pattern -> slots_of_pattern pattern None))
      | L.PRecord (fields, _open) ->
        let field_type lbl =
          Option.bind ty_opt ~f:(fun ty -> Chatml_typechecker.record_field_type ty lbl)
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
        push_frame ctx.stack fm;
        let rhs' = resolve_expr ctx case.rhs in
        pop_frame ctx.stack;
        { L.pat = case.pat
        ; pat_span = case.pat_span
        ; slots
        ; rhs = with_value case.rhs rhs'
        })
    in
    L.REMatch (with_value scrut scrut', cases')
  | L.EMatchSlots (scrut, cases) ->
    let scrut' = resolve_expr ctx scrut in
    let cases' =
      List.map cases ~f:(fun case ->
        let vars = L.collect_pattern_vars case.pat in
        let fm = Hashtbl.create (module String) in
        List.iteri vars ~f:(fun idx vnm ->
          let slot = List.nth_exn case.slots idx in
          Hashtbl.set fm ~key:vnm ~data:{ index = idx; slot });
        push_frame ctx.stack fm;
        let rhs' = resolve_expr ctx case.rhs in
        pop_frame ctx.stack;
        { L.pat = case.pat
        ; pat_span = case.pat_span
        ; slots = case.slots
        ; rhs = with_value case.rhs rhs'
        })
    in
    L.REMatch (with_value scrut scrut', cases')
  | L.ERecord fields ->
    let fields' =
      List.map fields ~f:(fun (lbl, ex) -> lbl, with_value ex (resolve_expr ctx ex))
    in
    L.RERecord fields'
  | L.EFieldGet (obj, lbl) ->
    let obj' = resolve_expr ctx obj in
    L.REFieldGet (with_value obj obj', lbl)
  | L.EVariant (tag, vs) ->
    let vs' = List.map vs ~f:(fun v -> with_value v (resolve_expr ctx v)) in
    L.REVariant (tag, vs')
  | L.EArray elts ->
    let elts' = List.map elts ~f:(fun elt -> with_value elt (resolve_expr ctx elt)) in
    L.REArray elts'
  | L.ETuple components ->
    L.RETuple
      (Chatml_tuple.map components ~f:(fun elt -> with_value elt (resolve_expr ctx elt)))
  | L.EArrayGet (arr, idx) ->
    let arr' = resolve_expr ctx arr in
    let idx' = resolve_expr ctx idx in
    L.REArrayGet (with_value arr arr', with_value idx idx')
  | L.EArraySet (arr, idx, v) ->
    let arr' = resolve_expr ctx arr in
    let idx' = resolve_expr ctx idx in
    let v' = resolve_expr ctx v in
    L.REArraySet (with_value arr arr', with_value idx idx', with_value v v')
  | L.ERef e1 ->
    let e1' = resolve_expr ctx e1 in
    L.RERef (with_value e1 e1')
  | L.ESetRef (r, v) ->
    let r' = resolve_expr ctx r in
    let v' = resolve_expr ctx v in
    L.RESetRef (with_value r r', with_value v v')
  | L.EDeref e1 ->
    let e1' = resolve_expr ctx e1 in
    L.REDeref (with_value e1 e1')
  | L.ERecordExtend (base, fields) ->
    let base' = resolve_expr ctx base in
    let fields' =
      List.map fields ~f:(fun (lbl, ex) -> lbl, with_value ex (resolve_expr ctx ex))
    in
    L.RERecordExtend (with_value base base', fields')

and leaf_slots ctx pattern =
  Binding_pattern.binders pattern
  |> List.map ~f:(fun (_, span) ->
    Option.value_map
      (ctx.lookup_type span)
      ~default:(Frame_env.Slot Frame_env.SObj)
      ~f:slot_of_typ)

and resolve_lambda ctx params explicit_slots body =
  let names =
    List.concat_map params ~f:(fun pattern -> Binding_pattern.binders pattern)
  in
  let slots =
    match explicit_slots with
    | Some slots -> slots
    | None -> List.concat_map params ~f:(leaf_slots ctx)
  in
  if List.length names <> List.length slots
  then failwith "internal: lambda slots do not match named binding leaves";
  let fm = Hashtbl.create (module String) in
  List.iteri (List.zip_exn names slots) ~f:(fun index ((name, _), slot) ->
    Hashtbl.set fm ~key:name ~data:{ index; slot });
  push_frame ctx.stack fm;
  let body' = resolve_expr ctx body in
  pop_frame ctx.stack;
  L.RELambda (params, slots, with_value body body')

and resolve_let_block ctx bindings explicit_slots body =
  let total_leaves =
    List.sum
      (module Int)
      bindings
      ~f:(fun (pattern, _) -> List.length (Binding_pattern.binders pattern))
  in
  Option.iter explicit_slots ~f:(fun slots ->
    if List.length slots <> total_leaves
    then failwith "internal: let slots do not match named binding leaves");
  let explicit_slots = Option.map explicit_slots ~f:Array.of_list in
  let fm = Hashtbl.create (module String) in
  push_frame ctx.stack fm;
  let resolved_rev, slots_rev, _ =
    List.fold
      bindings
      ~init:([], [], 0)
      ~f:(fun (resolved, slots, offset) (pattern, rhs) ->
        let resolved_rhs = resolve_expr ctx rhs in
        let names = Binding_pattern.binders pattern in
        let selected =
          match explicit_slots with
          | Some supplied -> List.mapi names ~f:(fun i _ -> supplied.(offset + i))
          | None ->
            (match pattern.value with
             | Name _ -> [ choose_slot ctx rhs ]
             | Wildcard | Tuple _ -> leaf_slots ctx pattern)
        in
        List.iteri (List.zip_exn names selected) ~f:(fun i ((name, _), slot) ->
          Hashtbl.set fm ~key:name ~data:{ index = offset + i; slot });
        ( (pattern, with_value rhs resolved_rhs) :: resolved
        , List.rev_append selected slots
        , offset + List.length names ))
  in
  let body' = resolve_expr ctx body in
  pop_frame ctx.stack;
  L.RELetBlock (List.rev resolved_rev, List.rev slots_rev, with_value body body')
;;

let rec resolve_stmt (ctx : resolve_ctx) (snode : L.stmt L.node) : L.resolved_stmt option =
  match snode.value with
  | L.SLet (x, rhs) -> Some (L.RSLet (x, with_value rhs (resolve_expr ctx rhs)))
  | L.SLetRec binds ->
    let binds' =
      List.map binds ~f:(fun (nm, rhs) -> nm, with_value rhs (resolve_expr ctx rhs))
    in
    Some (L.RSLetRec binds')
  | L.SType _ -> None
  | L.SExpr e -> Some (L.RSExpr (with_value e (resolve_expr ctx e)))
  | L.SModule { name; signature; body } ->
    let stmts' =
      List.filter_map body ~f:(fun st ->
        Option.map (resolve_stmt ctx st) ~f:(with_stmt_value st))
    in
    let visibility =
      Option.map signature ~f:(fun signature ->
        match ctx.lookup_module_visibility signature.span with
        | Some visibility -> visibility
        | None -> failwith "Resolver: missing checked module signature")
    in
    Some (L.RSModule { name; visibility; body = stmts' })
  | L.SOpen nm -> Some (L.RSOpen nm)
;;

let resolve_checked_program
      (checked : Chatml_typechecker.checked_program)
      (prog : L.program)
  : L.resolved_program
  =
  let lookup_fun = Chatml_typechecker.checked_lookup_span_type checked in
  let ctx =
    { lookup_type = lookup_fun
    ; lookup_module_visibility = Chatml_typechecker.checked_module_visibility checked
    ; stack = ref []
    }
  in
  let stmts' =
    prog.stmts
    |> List.filter_map ~f:(fun sn ->
      Option.map (resolve_stmt ctx sn) ~f:(with_stmt_value sn))
  in
  { L.stmts = stmts'; source_text = prog.source_text }
;;

let resolve_captured_program (checked : Chatml_typechecker.checked_program) unit =
  let source = Chatml.Chatml_parse.Unit.source unit in
  match checked.source with
  | Some checked_source
    when Chatml_source.Id.equal
           (Chatml_source.id source)
           (Chatml_source.id checked_source)
         && Chatml_source.Digest.equal
              (Chatml_source.digest source)
              (Chatml_source.digest checked_source) ->
    Ok (resolve_checked_program checked (Chatml.Chatml_parse.Unit.program unit))
  | Some _ | None ->
    Error
      (Chatml_diagnostic.create
         "Checked annotations do not belong to this captured source revision")
;;

let resolve_program (prog : L.program) : L.resolved_program =
  match Chatml_typechecker.check_program prog with
  | Ok checked -> resolve_checked_program checked prog
  | Error diagnostic ->
    failwith (Chatml_typechecker.format_diagnostic prog.source_text diagnostic)
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
