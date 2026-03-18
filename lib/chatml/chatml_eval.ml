open Core
open Chatml_lang

type eval_result =
  | Value of value
  | TailCall of clos * value list

let slot_matches_value = Chatml_slot_layout.matches_value

let assert_recursive_slots_are_objects =
  Chatml_slot_layout.assert_recursive_slots_are_objects
;;

let store_with_slot
      (fr : Frame_env.frame)
      (idx : int)
      (slot : Frame_env.packed_slot)
      (v : value)
  : unit
  =
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

let rec frames_nth (frames : Frame_env.env) (n : int) : Frame_env.frame option =
  match frames, n with
  | [], _ -> None
  | fr :: _, 0 -> Some fr
  | _ :: tl, n when n > 0 -> frames_nth tl (n - 1)
  | _ -> None
;;

let load_from_frames (frames : Frame_env.env) (loc : var_loc) : value =
  match frames_nth frames loc.depth with
  | None -> failwith "Frame stack underflow in REVarLoc evaluation"
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

let rec finish_eval (initial_frames : Frame_env.env) (initial_res : eval_result) : value =
  let rec loop (_frames : Frame_env.env) (res : eval_result) : value =
    match res with
    | Value v -> v
    | TailCall (cl, args) ->
      if List.length cl.param_slots <> List.length args
      then failwith "internal: closure param_slots length mismatch with call-site";
      let slots = cl.param_slots in
      let param_frame = Frame_env.alloc_packed slots in
      List.iteri args ~f:(fun idx v ->
        let slot = List.nth_exn slots idx in
        store_with_slot param_frame idx slot v);
      let child_env = cl.env in
      let child_frames = param_frame :: cl.frames in
      let next_res = eval_expr_tail ~tail:true child_env child_frames cl.body in
      loop child_frames next_res
  in
  loop initial_frames initial_res

and eval_expr_tail
      ~(tail : bool)
      (env : env)
      (frames : Frame_env.env)
      (e : resolved_expr node)
  : eval_result
  =
  match e.value with
  | REUnit -> Value VUnit
  | REInt i -> Value (VInt i)
  | REBool b -> Value (VBool b)
  | REFloat f -> Value (VFloat f)
  | REString s -> Value (VString s)
  | REVarGlobal x ->
    (match find_var env x with
     | Some v -> Value v
     | None -> raise_runtime_error ~span:e.span (Printf.sprintf "Unbound variable %s" x))
  | REVarLoc loc -> Value (load_from_frames frames loc)
  | REPrim1 (prim, arg_expr) ->
    let arg_val = finish_eval frames (eval_expr_tail ~tail:false env frames arg_expr) in
    (match prim, arg_val with
     | UNegInt, VInt n -> Value (VInt (-n))
     | UNegFloat, VFloat f -> Value (VFloat (-.f))
     | UNegInt, _ ->
       raise_runtime_error
         ~span:e.span
         "Internal type error: unary int negation on non-int"
     | UNegFloat, _ ->
       raise_runtime_error
         ~span:e.span
         "Internal type error: unary float negation on non-float")
  | REPrim2 (prim, lhs_expr, rhs_expr) ->
    let lhs_val = finish_eval frames (eval_expr_tail ~tail:false env frames lhs_expr) in
    let rhs_val = finish_eval frames (eval_expr_tail ~tail:false env frames rhs_expr) in
    (match prim, lhs_val, rhs_val with
     | BIntAdd, VInt x, VInt y -> Value (VInt (x + y))
     | BIntSub, VInt x, VInt y -> Value (VInt (x - y))
     | BIntMul, VInt x, VInt y -> Value (VInt (x * y))
     | BIntDiv, VInt _, VInt 0 -> raise_runtime_error ~span:e.span "Division by zero"
     | BIntDiv, VInt x, VInt y -> Value (VInt (x / y))
     | BFloatAdd, VFloat x, VFloat y -> Value (VFloat (x +. y))
     | BFloatSub, VFloat x, VFloat y -> Value (VFloat (x -. y))
     | BFloatMul, VFloat x, VFloat y -> Value (VFloat (x *. y))
     | BFloatDiv, VFloat _, VFloat y when Float.equal y 0.0 ->
       raise_runtime_error ~span:e.span "Division by zero"
     | BFloatDiv, VFloat x, VFloat y -> Value (VFloat (x /. y))
     | BStringConcat, VString x, VString y -> Value (VString (x ^ y))
     | BIntLt, VInt x, VInt y -> Value (VBool (x < y))
     | BIntGt, VInt x, VInt y -> Value (VBool (x > y))
     | BIntLe, VInt x, VInt y -> Value (VBool (x <= y))
     | BIntGe, VInt x, VInt y -> Value (VBool (x >= y))
     | BFloatLt, VFloat x, VFloat y -> Value (VBool Float.(x < y))
     | BFloatGt, VFloat x, VFloat y -> Value (VBool Float.(x > y))
     | BFloatLe, VFloat x, VFloat y -> Value (VBool Float.(x <= y))
     | BFloatGe, VFloat x, VFloat y -> Value (VBool Float.(x >= y))
     | BEq, _, _ -> Value (VBool (equal_value lhs_val rhs_val))
     | BNeq, _, _ -> Value (VBool (not (equal_value lhs_val rhs_val)))
     | (BIntAdd | BIntSub | BIntMul | BIntDiv | BIntLt | BIntGt | BIntLe | BIntGe), _, _
       ->
       raise_runtime_error
         ~span:e.span
         "Internal type error: int primitive on non-int operands"
     | ( ( BFloatAdd
         | BFloatSub
         | BFloatMul
         | BFloatDiv
         | BFloatLt
         | BFloatGt
         | BFloatLe
         | BFloatGe )
       , _
       , _ ) ->
       raise_runtime_error
         ~span:e.span
         "Internal type error: float primitive on non-float operands"
     | BStringConcat, _, _ ->
       raise_runtime_error
         ~span:e.span
         "Internal type error: string concatenation on non-string operands")
  | RELambda (params, slots, body) ->
    Value (VClosure { params; body; env = copy_env env; frames; param_slots = slots })
  | RELetBlock (bindings, slots, body) ->
    if List.length bindings <> List.length slots
    then failwith "internal: slot list length mismatch in RELetBlock";
    let block_frame = Frame_env.alloc_packed slots in
    let child_frames = block_frame :: frames in
    List.iteri bindings ~f:(fun idx (_nm, rhs_expr) ->
      let v =
        finish_eval child_frames (eval_expr_tail ~tail:false env child_frames rhs_expr)
      in
      let slot = List.nth_exn slots idx in
      store_with_slot block_frame idx slot v);
    eval_expr_tail ~tail env child_frames body
  | REApp (fn_expr, arg_exprs) ->
    let fn_val = finish_eval frames (eval_expr_tail ~tail:false env frames fn_expr) in
    let arg_vals =
      List.map arg_exprs ~f:(fun a ->
        finish_eval frames (eval_expr_tail ~tail:false env frames a))
    in
    (match fn_val with
     | VClosure cl ->
       if List.length cl.params <> List.length arg_vals
       then raise_runtime_error ~span:e.span "Function arity mismatch";
       if tail
       then TailCall (cl, arg_vals)
       else Value (finish_eval frames (TailCall (cl, arg_vals)))
     | VBuiltin bf ->
       (try Value (bf arg_vals) with
        | Failure msg -> raise_runtime_error ~span:e.span msg)
     | _ -> raise_runtime_error ~span:e.span "Trying to call a non-function value")
  | REIf (cond_expr, then_expr, else_expr) ->
    let cond_val = finish_eval frames (eval_expr_tail ~tail:false env frames cond_expr) in
    (match cond_val with
     | VBool true -> eval_expr_tail ~tail env frames then_expr
     | VBool false -> eval_expr_tail ~tail env frames else_expr
     | _ -> raise_runtime_error ~span:e.span "If condition must be bool")
  | REWhile (cond_expr, body_expr) ->
    let rec loop () =
      let cval = finish_eval frames (eval_expr_tail ~tail:false env frames cond_expr) in
      match cval with
      | VBool true ->
        ignore (finish_eval frames (eval_expr_tail ~tail:false env frames body_expr));
        loop ()
      | VBool false -> Value VUnit
      | _ -> raise_runtime_error ~span:e.span "While condition must be bool"
    in
    loop ()
  | RELetRec (bindings, slots, body) ->
    if List.length bindings <> List.length slots
    then failwith "internal: slot list length mismatch in RELetRec";
    assert_recursive_slots_are_objects slots;
    let rec_frame = Frame_env.alloc_packed slots in
    List.iteri bindings ~f:(fun idx _ -> Frame_env.set_obj rec_frame idx (Obj.repr VUnit));
    let child_frames = rec_frame :: frames in
    List.iteri bindings ~f:(fun idx (_nm, rhs_expr) ->
      let v =
        finish_eval child_frames (eval_expr_tail ~tail:false env child_frames rhs_expr)
      in
      let slot = List.nth_exn slots idx in
      store_with_slot rec_frame idx slot v);
    eval_expr_tail ~tail env child_frames body
  | REMatch (scrut_expr, cases) ->
    let sv = finish_eval frames (eval_expr_tail ~tail:false env frames scrut_expr) in
    match_eval ~tail env frames e.span sv cases
  | RERecord fields ->
    let record_fields =
      List.fold fields ~init:String.Map.empty ~f:(fun acc (fld, fe) ->
        let fv = finish_eval frames (eval_expr_tail ~tail:false env frames fe) in
        Map.set acc ~key:fld ~data:fv)
    in
    Value (VRecord record_fields)
  | REFieldGet (rec_expr, field) ->
    let rec_val = finish_eval frames (eval_expr_tail ~tail:false env frames rec_expr) in
    (match rec_val with
     | VRecord fields ->
       (match Map.find fields field with
        | Some v -> Value v
        | None ->
          raise_runtime_error
            ~span:e.span
            (Printf.sprintf "No field '%s' in record" field))
     | VModule menv ->
       (match find_var menv field with
        | Some v -> Value v
        | None ->
          raise_runtime_error
            ~span:e.span
            (Printf.sprintf "No field '%s' in module" field))
     | _ -> raise_runtime_error ~span:e.span "Field access on non-record/non-module")
  | REVariant (tag, exprs) ->
    let vals =
      List.map exprs ~f:(fun ex ->
        finish_eval frames (eval_expr_tail ~tail:false env frames ex))
    in
    Value (VVariant (tag, vals))
  | REArray elts ->
    let arr_vals =
      List.map elts ~f:(fun ex ->
        finish_eval frames (eval_expr_tail ~tail:false env frames ex))
    in
    Value (VArray (Array.of_list arr_vals))
  | REArrayGet (arr_expr, idx_expr) ->
    let arr_val = finish_eval frames (eval_expr_tail ~tail:false env frames arr_expr) in
    let idx_val = finish_eval frames (eval_expr_tail ~tail:false env frames idx_expr) in
    (match arr_val, idx_val with
     | VArray arr, VInt i ->
       if i < 0 || i >= Array.length arr
       then raise_runtime_error ~span:e.span "Array index out of bounds"
       else Value arr.(i)
     | _ -> raise_runtime_error ~span:e.span "Invalid array access")
  | REArraySet (arr_expr, idx_expr, v_expr) ->
    let arr_val = finish_eval frames (eval_expr_tail ~tail:false env frames arr_expr) in
    let idx_val = finish_eval frames (eval_expr_tail ~tail:false env frames idx_expr) in
    let new_val = finish_eval frames (eval_expr_tail ~tail:false env frames v_expr) in
    (match arr_val, idx_val with
     | VArray arr, VInt i ->
       if i < 0 || i >= Array.length arr
       then raise_runtime_error ~span:e.span "Array index out of bounds"
       else (
         arr.(i) <- new_val;
         Value VUnit)
     | _ -> raise_runtime_error ~span:e.span "Invalid array set")
  | RERef e1 ->
    let v1 = finish_eval frames (eval_expr_tail ~tail:false env frames e1) in
    Value (VRef (ref v1))
  | RESetRef (ref_expr, new_expr) ->
    let r = finish_eval frames (eval_expr_tail ~tail:false env frames ref_expr) in
    let nv = finish_eval frames (eval_expr_tail ~tail:false env frames new_expr) in
    (match r with
     | VRef cell ->
       cell := nv;
       Value VUnit
     | _ -> raise_runtime_error ~span:e.span "Attempting to set a non-ref value")
  | REDeref e1 ->
    let rv = finish_eval frames (eval_expr_tail ~tail:false env frames e1) in
    (match rv with
     | VRef cell -> Value !cell
     | _ -> raise_runtime_error ~span:e.span "Deref on non-ref value")
  | RESequence (e1, e2) ->
    ignore (finish_eval frames (eval_expr_tail ~tail:false env frames e1));
    eval_expr_tail ~tail env frames e2
  | RERecordExtend (base_expr, fields) ->
    let base_val = finish_eval frames (eval_expr_tail ~tail:false env frames base_expr) in
    let base_fields =
      match base_val with
      | VRecord fields -> fields
      | _ -> raise_runtime_error ~span:e.span "Record extension base is not a record"
    in
    let new_fields =
      List.fold fields ~init:base_fields ~f:(fun acc (fld, fe) ->
        let fv = finish_eval frames (eval_expr_tail ~tail:false env frames fe) in
        Map.set acc ~key:fld ~data:fv)
    in
    Value (VRecord new_fields)

and match_eval
      ~(tail : bool)
      (env : env)
      (frames : Frame_env.env)
      (match_span : Source.span)
      (v : value)
      (cases : resolved_match_case list)
  : eval_result
  =
  match cases with
  | [] -> raise_runtime_error ~span:match_span "Non-exhaustive pattern match"
  | case :: tl ->
    (match match_pattern v case.pat with
     | None -> match_eval ~tail env frames match_span v tl
     | Some binds ->
       let vars = collect_pattern_vars case.pat in
       if List.length vars <> List.length case.slots
       then failwith "internal: slot/var length mismatch in REMatch";
       let pat_frame = Frame_env.alloc_packed case.slots in
       List.iteri vars ~f:(fun idx vnm ->
         match List.Assoc.find binds ~equal:String.equal vnm with
         | Some vl ->
           let slot = List.nth_exn case.slots idx in
           store_with_slot pat_frame idx slot vl
         | None -> ());
       let child_frames = pat_frame :: frames in
       eval_expr_tail ~tail env child_frames case.rhs)

and eval_expr (env : env) (frames : Frame_env.env) (e : resolved_expr node) : eval_result =
  eval_expr_tail ~tail:true env frames e

and import_module_bindings ?span (target_env : env) (mname : string) : string list =
  match find_var target_env mname with
  | Some (VModule menv) ->
    let imports =
      Hashtbl.fold menv ~init:[] ~f:(fun ~key ~data acc -> (key, !data) :: acc)
    in
    (match List.find imports ~f:(fun (key, _value) -> Hashtbl.mem target_env key) with
     | Some (key, _value) ->
       raise_runtime_error
         ?span
         (Printf.sprintf "open %s would shadow existing binding '%s'" mname key)
     | None ->
       List.iter imports ~f:(fun (key, value) -> define_var target_env key value);
       List.map imports ~f:fst)
  | _ -> raise_runtime_error ?span (Printf.sprintf "Cannot open non-module '%s'" mname)

and eval_module_value
      (outer_env : env)
      (frames : Frame_env.env)
      (mname : string)
      (stmts : resolved_stmt node list)
  : env
  =
  let module_eval_env = copy_env outer_env in
  let module_export_env = create_env () in
  define_var module_eval_env mname (VModule module_export_env);
  let exported_names =
    List.concat_map stmts ~f:(fun st -> eval_stmt_with_exports module_eval_env frames st)
  in
  let seen = Hash_set.create (module String) in
  List.iter exported_names ~f:(fun name ->
    if not (Hash_set.mem seen name)
    then (
      Hash_set.add seen name;
      match find_var module_eval_env name with
      | Some value -> define_var module_export_env name value
      | None -> ()));
  module_export_env

and eval_stmt_with_exports (env : env) (frames : Frame_env.env) (s : resolved_stmt node)
  : string list
  =
  match s.value with
  | RSLet (x, e1) ->
    let v1 = finish_eval frames (eval_expr env frames e1) in
    define_var env x v1;
    [ x ]
  | RSLetRec bindings ->
    List.iter bindings ~f:(fun (nm, _) -> define_var env nm VUnit);
    List.iter bindings ~f:(fun (nm, rhs_expr) ->
      let v = finish_eval frames (eval_expr env frames rhs_expr) in
      update_var env nm v);
    List.map bindings ~f:fst
  | RSModule (mname, stmts) ->
    let menv = eval_module_value env frames mname stmts in
    define_var env mname (VModule menv);
    [ mname ]
  | RSOpen mname ->
    let _imported = import_module_bindings ~span:s.span env mname in
    []
  | RSExpr e ->
    ignore (finish_eval frames (eval_expr env frames e));
    []

and eval_stmt (env : env) (frames : Frame_env.env) (s : resolved_stmt node) : unit =
  ignore (eval_stmt_with_exports env frames s)
;;

let eval_program (env : env) (prog : resolved_program) : unit =
  let initial_frames : Frame_env.env = [] in
  List.iter prog.stmts ~f:(fun stmt_node -> eval_stmt env initial_frames stmt_node)
;;
