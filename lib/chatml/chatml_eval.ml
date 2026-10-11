open Core
open Chatml_lang
module Binding_pattern = Chatml_binding_pattern

type eval_result =
  | Value of value
  | TailCall of clos * value list

let diagnostic_checkpoint env () =
  Option.iter env.control ~f:(fun control -> control.diagnostic_checkpoint ())
;;

let site env span =
  Option.bind env.source ~f:(fun source ->
    Chatml_owned_source.site source span ~checkpoint:(diagnostic_checkpoint env))
;;

let raise_owned_error env ?span message =
  let site = Option.bind span ~f:(site env) in
  let span =
    match env.source, site with
    | Some _, None -> None
    | None, _ | Some _, Some _ -> span
  in
  raise_runtime_error ?span ?site message
;;

let slot_matches_value = Chatml_slot_layout.matches_value

let allocate env bytes =
  Option.iter env.control ~f:(fun control -> control.allocate bytes)
;;

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

let binding_checkpoint env () =
  Option.iter env.control ~f:(fun control -> control.checkpoint ())
;;

let extract_binding env (pattern : Binding_pattern.t) value =
  match pattern.value with
  | Name name -> [ name, value ]
  | Wildcard -> []
  | Tuple _ ->
    let count = Binding_pattern.count ~checkpoint:(binding_checkpoint env) pattern in
    allocate env (32 * count);
    (match
       Chatml_binding_value.extract ~checkpoint:(binding_checkpoint env) pattern value
     with
     | Ok bindings -> bindings
     | Error mismatch -> raise_owned_error env ~span:mismatch.span mismatch.message)
;;

let staged_slot_bindings env (pattern : Binding_pattern.t) value =
  match pattern.value with
  | Name _ -> [ pattern.span, value ]
  | Wildcard -> []
  | Tuple _ ->
    let values = extract_binding env pattern value in
    let names = Binding_pattern.binders ~checkpoint:(binding_checkpoint env) pattern in
    List.map2_exn names values ~f:(fun (_, span) (_, value) -> span, value)
;;

let rec validate_staged_slots env slots bindings =
  match bindings, slots with
  | [], _ -> slots
  | (span, value) :: bindings, slot :: slots ->
    if not (slot_matches_value slot value)
    then raise_owned_error env ~span "Binding value does not match checked slot type";
    validate_staged_slots env slots bindings
  | _ :: _, [] -> failwith "internal: insufficient slots for named binding leaves"
;;

let rec store_staged_slots frame index slots bindings =
  match bindings, slots with
  | [], _ -> index
  | (_, value) :: bindings, slot :: slots ->
    store_with_slot frame index slot value;
    store_staged_slots frame (index + 1) slots bindings
  | _ :: _, [] -> failwith "internal: insufficient validated binding slots"
;;

let rec validate_named_parameters env params slots values =
  match params, slots, values with
  | [], [], [] -> ()
  | pattern :: params, slot :: slots, value :: values ->
    if not (slot_matches_value slot value)
    then
      raise_owned_error
        env
        ~span:pattern.Binding_pattern.span
        "Binding value does not match checked slot type";
    validate_named_parameters env params slots values
  | _ -> failwith "internal: closure parameters, slots and arguments disagree"
;;

let rec store_named_parameters frame index slots values =
  match slots, values with
  | [], [] -> ()
  | slot :: slots, value :: values ->
    store_with_slot frame index slot value;
    store_named_parameters frame (index + 1) slots values
  | _ -> failwith "internal: validated closure slots and arguments disagree"
;;

let stage_parameters env params values =
  List.fold2_exn params values ~init:[] ~f:(fun result pattern value ->
    let bindings = staged_slot_bindings env pattern value in
    List.rev_append bindings result)
  |> List.rev
;;

let rec finish_eval (initial_frames : Frame_env.env) (initial_res : eval_result) : value =
  let rec loop (_frames : Frame_env.env) (res : eval_result) : value =
    match res with
    | Value v -> v
    | TailCall (cl, args) ->
      if List.length cl.params <> List.length args
      then raise_owned_error cl.env ~span:cl.body.span "Function arity mismatch";
      let slots = cl.param_slots in
      allocate cl.env (16 * List.length slots);
      let named =
        List.for_all cl.params ~f:(fun pattern ->
          match pattern.Binding_pattern.value with
          | Name _ -> true
          | Wildcard | Tuple _ -> false)
      in
      let param_frame =
        if named
        then (
          validate_named_parameters cl.env cl.params slots args;
          let frame = Frame_env.alloc_packed slots in
          store_named_parameters frame 0 slots args;
          frame)
        else (
          let bindings = stage_parameters cl.env cl.params args in
          if not (List.is_empty (validate_staged_slots cl.env slots bindings))
          then failwith "internal: excess closure slots after parameter staging";
          let frame = Frame_env.alloc_packed slots in
          ignore (store_staged_slots frame 0 slots bindings : int);
          frame)
      in
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
  Option.iter env.control ~f:(fun control -> control.checkpoint ());
  match e.value with
  | REUnit -> Value VUnit
  | REInt i -> Value (VInt i)
  | REBool b -> Value (VBool b)
  | REFloat f -> Value (VFloat f)
  | REString s -> Value (VString s)
  | REVarGlobal x ->
    (match find_var env x with
     | Some v -> Value v
     | None -> raise_owned_error env ~span:e.span (Printf.sprintf "Unbound variable %s" x))
  | REConfigured_read reference ->
    (match Configured_modules.read env.configured_modules reference with
     | Ok value -> Value value
     | Error
         ( Registry_unavailable
         | Module_unavailable _
         | Member_unavailable _
         | Invalid_public_module ) ->
       raise_owned_error env ~span:e.span "Configured module registry is unavailable")
  | REVarLoc loc -> Value (load_from_frames frames loc)
  | REPrim1 (prim, arg_expr) ->
    let arg_val = finish_eval frames (eval_expr_tail ~tail:false env frames arg_expr) in
    (match prim, arg_val with
     | UNegInt, VInt n -> Value (VInt (-n))
     | UNegFloat, VFloat f -> Value (VFloat (-.f))
     | UNegInt, _ ->
       raise_owned_error
         env
         ~span:e.span
         "Internal type error: unary int negation on non-int"
     | UNegFloat, _ ->
       raise_owned_error
         env
         ~span:e.span
         "Internal type error: unary float negation on non-float")
  | REPrim2 (prim, lhs_expr, rhs_expr) ->
    let lhs_val = finish_eval frames (eval_expr_tail ~tail:false env frames lhs_expr) in
    let rhs_val = finish_eval frames (eval_expr_tail ~tail:false env frames rhs_expr) in
    (match prim, lhs_val, rhs_val with
     | BIntAdd, VInt x, VInt y -> Value (VInt (x + y))
     | BIntSub, VInt x, VInt y -> Value (VInt (x - y))
     | BIntMul, VInt x, VInt y -> Value (VInt (x * y))
     | BIntDiv, VInt _, VInt 0 -> raise_owned_error env ~span:e.span "Division by zero"
     | BIntDiv, VInt x, VInt y -> Value (VInt (x / y))
     | BFloatAdd, VFloat x, VFloat y -> Value (VFloat (x +. y))
     | BFloatSub, VFloat x, VFloat y -> Value (VFloat (x -. y))
     | BFloatMul, VFloat x, VFloat y -> Value (VFloat (x *. y))
     | BFloatDiv, VFloat _, VFloat y when Float.equal y 0.0 ->
       raise_owned_error env ~span:e.span "Division by zero"
     | BFloatDiv, VFloat x, VFloat y -> Value (VFloat (x /. y))
     | BStringConcat, VString x, VString y ->
       Option.iter env.control ~f:(fun control ->
         control.before_builtin ~name:"String.concat" [ lhs_val; rhs_val ]);
       Value (VString (x ^ y))
     | BIntLt, VInt x, VInt y -> Value (VBool (x < y))
     | BIntGt, VInt x, VInt y -> Value (VBool (x > y))
     | BIntLe, VInt x, VInt y -> Value (VBool (x <= y))
     | BIntGe, VInt x, VInt y -> Value (VBool (x >= y))
     | BFloatLt, VFloat x, VFloat y -> Value (VBool Float.(x < y))
     | BFloatGt, VFloat x, VFloat y -> Value (VBool Float.(x > y))
     | BFloatLe, VFloat x, VFloat y -> Value (VBool Float.(x <= y))
     | BFloatGe, VFloat x, VFloat y -> Value (VBool Float.(x >= y))
     | (BEq | BNeq), _, _ ->
       Option.iter env.control ~f:(fun control ->
         control.check_value lhs_val;
         control.check_value rhs_val);
       let equal = equal_value lhs_val rhs_val in
       Value
         (VBool
            (match prim with
             | BEq -> equal
             | _ -> not equal))
     | (BIntAdd | BIntSub | BIntMul | BIntDiv | BIntLt | BIntGt | BIntLe | BIntGe), _, _
       ->
       raise_owned_error
         env
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
       raise_owned_error
         env
         ~span:e.span
         "Internal type error: float primitive on non-float operands"
     | BStringConcat, _, _ ->
       raise_owned_error
         env
         ~span:e.span
         "Internal type error: string concatenation on non-string operands")
  | RELambda (params, slots, body) ->
    Value (VClosure { params; body; env = copy_env env; frames; param_slots = slots })
  | RELetBlock (bindings, slots, body) ->
    let leaves =
      List.sum
        (module Int)
        bindings
        ~f:(fun (pattern, _) ->
          match pattern.Binding_pattern.value with
          | Name _ -> 1
          | Wildcard -> 0
          | Tuple _ -> Binding_pattern.count ~checkpoint:(binding_checkpoint env) pattern)
    in
    if leaves <> List.length slots
    then failwith "internal: slot list length mismatch in RELetBlock";
    allocate env (16 * leaves);
    let block_frame = Frame_env.alloc_packed slots in
    let child_frames = block_frame :: frames in
    let rec install offset slots = function
      | [] -> ()
      | (pattern, rhs) :: rest ->
        let value =
          finish_eval child_frames (eval_expr_tail ~tail:false env child_frames rhs)
        in
        (match pattern.Binding_pattern.value, slots with
         | Name _, slot :: tail ->
           if not (slot_matches_value slot value)
           then
             raise_owned_error
               env
               ~span:pattern.span
               "Binding value does not match checked slot type";
           store_with_slot block_frame offset slot value;
           install (offset + 1) tail rest
         | Name _, [] -> failwith "internal: missing named let slot"
         | Wildcard, _ -> install offset slots rest
         | Tuple _, _ ->
           let staged = staged_slot_bindings env pattern value in
           let remaining = validate_staged_slots env slots staged in
           let next = store_staged_slots block_frame offset slots staged in
           install next remaining rest)
    in
    install 0 slots bindings;
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
       then raise_owned_error env ~span:e.span "Function arity mismatch";
       if tail
       then TailCall (cl, arg_vals)
       else Value (finish_eval frames (TailCall (cl, arg_vals)))
     | VBuiltin bf ->
       let value =
         try bf arg_vals with
         | Runtime_error diagnostic ->
           let fallback =
             match diagnostic.site with
             | Some _ -> None
             | None -> site env e.span
           in
           raise (Runtime_error (Chatml_diagnostic.with_fallback diagnostic fallback))
         | Failure msg -> raise_owned_error env ~span:e.span msg
       in
       let value =
         match value with
         | VTask task ->
           let fallback =
             match task.site with
             | Some _ -> None
             | None -> site env e.span
           in
           VTask (Task.with_fallback task fallback)
         | value -> value
       in
       Value value
     | _ -> raise_owned_error env ~span:e.span "Trying to call a non-function value")
  | REIf (cond_expr, then_expr, else_expr) ->
    let cond_val = finish_eval frames (eval_expr_tail ~tail:false env frames cond_expr) in
    (match cond_val with
     | VBool true -> eval_expr_tail ~tail env frames then_expr
     | VBool false -> eval_expr_tail ~tail env frames else_expr
     | _ -> raise_owned_error env ~span:e.span "If condition must be bool")
  | REWhile (cond_expr, body_expr) ->
    let rec loop () =
      let cval = finish_eval frames (eval_expr_tail ~tail:false env frames cond_expr) in
      match cval with
      | VBool true ->
        ignore (finish_eval frames (eval_expr_tail ~tail:false env frames body_expr));
        loop ()
      | VBool false -> Value VUnit
      | _ -> raise_owned_error env ~span:e.span "While condition must be bool"
    in
    loop ()
  | RELetRec (bindings, slots, body) ->
    allocate env (16 * List.length slots);
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
    allocate env (64 * List.length fields);
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
          raise_owned_error
            env
            ~span:e.span
            (Printf.sprintf "No field '%s' in record" field))
     | VModule menv ->
       (match find_var menv field with
        | Some v -> Value v
        | None ->
          raise_owned_error
            env
            ~span:e.span
            (Printf.sprintf "No field '%s' in module" field))
     | _ -> raise_owned_error env ~span:e.span "Field access on non-record/non-module")
  | REVariant (tag, exprs) ->
    allocate env (16 * (List.length exprs + 1));
    let vals =
      List.map exprs ~f:(fun ex ->
        finish_eval frames (eval_expr_tail ~tail:false env frames ex))
    in
    Value (VVariant (tag, vals))
  | REArray elts ->
    Option.iter env.control ~f:(fun control ->
      control.before_builtin ~name:"Array.literal" [ VInt (List.length elts) ]);
    let arr_vals =
      List.map elts ~f:(fun ex ->
        finish_eval frames (eval_expr_tail ~tail:false env frames ex))
    in
    Value (VArray (Array.of_list arr_vals))
  | RETuple components ->
    Option.iter env.control ~f:(fun control ->
      control.before_builtin
        ~name:"Tuple.literal"
        [ VInt (Chatml_tuple.length components) ]);
    allocate env 16;
    let values =
      Chatml_tuple.map components ~f:(fun expr ->
        finish_eval frames (eval_expr_tail ~tail:false env frames expr))
    in
    Value (VTuple values)
  | REArrayGet (arr_expr, idx_expr) ->
    let arr_val = finish_eval frames (eval_expr_tail ~tail:false env frames arr_expr) in
    let idx_val = finish_eval frames (eval_expr_tail ~tail:false env frames idx_expr) in
    (match arr_val, idx_val with
     | VArray arr, VInt i ->
       if i < 0 || i >= Array.length arr
       then raise_owned_error env ~span:e.span "Array index out of bounds"
       else Value arr.(i)
     | _ -> raise_owned_error env ~span:e.span "Invalid array access")
  | REArraySet (arr_expr, idx_expr, v_expr) ->
    let arr_val = finish_eval frames (eval_expr_tail ~tail:false env frames arr_expr) in
    let idx_val = finish_eval frames (eval_expr_tail ~tail:false env frames idx_expr) in
    let new_val = finish_eval frames (eval_expr_tail ~tail:false env frames v_expr) in
    (match arr_val, idx_val with
     | VArray arr, VInt i ->
       if i < 0 || i >= Array.length arr
       then raise_owned_error env ~span:e.span "Array index out of bounds"
       else (
         arr.(i) <- new_val;
         Value VUnit)
     | _ -> raise_owned_error env ~span:e.span "Invalid array set")
  | RERef e1 ->
    allocate env 16;
    let v1 = finish_eval frames (eval_expr_tail ~tail:false env frames e1) in
    Value (VRef (ref v1))
  | RESetRef (ref_expr, new_expr) ->
    let r = finish_eval frames (eval_expr_tail ~tail:false env frames ref_expr) in
    let nv = finish_eval frames (eval_expr_tail ~tail:false env frames new_expr) in
    (match r with
     | VRef cell ->
       cell := nv;
       Value VUnit
     | _ -> raise_owned_error env ~span:e.span "Attempting to set a non-ref value")
  | REDeref e1 ->
    let rv = finish_eval frames (eval_expr_tail ~tail:false env frames e1) in
    (match rv with
     | VRef cell -> Value !cell
     | _ -> raise_owned_error env ~span:e.span "Deref on non-ref value")
  | RESequence (e1, e2) ->
    ignore (finish_eval frames (eval_expr_tail ~tail:false env frames e1));
    eval_expr_tail ~tail env frames e2
  | RERecordExtend (base_expr, fields) ->
    let base_val = finish_eval frames (eval_expr_tail ~tail:false env frames base_expr) in
    let base_fields =
      match base_val with
      | VRecord fields -> fields
      | _ -> raise_owned_error env ~span:e.span "Record extension base is not a record"
    in
    let new_fields =
      allocate env (64 * (Map.length base_fields + List.length fields));
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
  | [] -> raise_owned_error env ~span:match_span "Non-exhaustive pattern match"
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
      Hashtbl.fold menv.bindings ~init:[] ~f:(fun ~key ~data acc -> (key, !data) :: acc)
    in
    (match
       List.find imports ~f:(fun (key, _value) -> Hashtbl.mem target_env.bindings key)
     with
     | Some (key, _value) ->
       raise_owned_error
         target_env
         ?span
         (Printf.sprintf "open %s would shadow existing binding '%s'" mname key)
     | None ->
       List.iter imports ~f:(fun (key, value) -> define_var target_env key value);
       List.map imports ~f:fst)
  | _ ->
    raise_owned_error
      target_env
      ?span
      (Printf.sprintf "Cannot open non-module '%s'" mname)

and eval_module_value
      (outer_env : env)
      (frames : Frame_env.env)
      (stmts : resolved_stmt node list)
  : env
  =
  let module_eval_env = copy_env outer_env in
  let exported_names =
    List.concat_map stmts ~f:(fun st -> eval_stmt_with_exports module_eval_env frames st)
  in
  let module_export_env =
    create_env
      ?control:outer_env.control
      ?source:outer_env.source
      ~configured_modules:outer_env.configured_modules
      ()
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

and select_module_exports ~span ~(origin : env) (env : env) visibility =
  allocate env (32 * List.length visibility);
  let selected =
    create_env
      ?control:env.control
      ?source:env.source
      ~configured_modules:env.configured_modules
      ()
  in
  List.iter visibility ~f:(fun member ->
    Option.iter env.control ~f:(fun control -> control.checkpoint ());
    let name =
      match member with
      | Chatml_module_visibility.Value name | Module (name, _) -> name
    in
    if Hashtbl.mem selected.bindings name
    then raise_owned_error origin ~span ("Duplicate checked module export '" ^ name ^ "'");
    let value =
      match find_var env name with
      | None ->
        raise_owned_error origin ~span ("Missing checked module export '" ^ name ^ "'")
      | Some value -> value
    in
    let value =
      match member, value with
      | Value _, value -> value
      | Module (_, visibility), VModule nested ->
        VModule (select_module_exports ~span ~origin nested visibility)
      | Module _, _ ->
        raise_owned_error origin ~span ("Checked export is not a module: " ^ name)
    in
    define_var selected name value);
  selected

and eval_stmt_with_exports (env : env) (frames : Frame_env.env) (s : resolved_stmt node)
  : string list
  =
  match s.value with
  | RSLet (pattern, e1) ->
    let value = finish_eval frames (eval_expr env frames e1) in
    (match pattern.value with
     | Binding_pattern.Name name ->
       define_var env name value;
       [ name ]
     | Wildcard -> []
     | Tuple _ ->
       let bindings = extract_binding env pattern value in
       (* All shape/resource/cancellation checks precede cell publication. No
          callback can observe a proper subset of these new bindings. *)
       let cells = List.map bindings ~f:(fun (name, value) -> name, ref value) in
       List.iter cells ~f:(fun (name, cell) ->
         Hashtbl.set env.bindings ~key:name ~data:cell);
       List.map bindings ~f:fst)
  | RSLetRec bindings ->
    List.iter bindings ~f:(fun (nm, _) -> define_var env nm VUnit);
    List.iter bindings ~f:(fun (nm, rhs_expr) ->
      let v = finish_eval frames (eval_expr env frames rhs_expr) in
      update_var env nm v);
    List.map bindings ~f:fst
  | RSModule { name; visibility; body } ->
    let menv = eval_module_value env frames body in
    let menv =
      match visibility with
      | None -> menv
      | Some visibility -> select_module_exports ~span:s.span ~origin:env menv visibility
    in
    define_var env name (VModule menv);
    [ name ]
  | RSOpen mname ->
    let _imported = import_module_bindings ~span:s.span env mname in
    []
  | RSExpr e ->
    ignore (finish_eval frames (eval_expr env frames e));
    []

and eval_stmt (env : env) (frames : Frame_env.env) (s : resolved_stmt node) : unit =
  ignore (eval_stmt_with_exports env frames s)
;;

let apply_value_result (fn : value) (args : value list) : (value, runtime_error) result =
  match fn with
  | VClosure cl ->
    if List.length cl.params <> List.length args
    then Error (Chatml_diagnostic.create "Function arity mismatch")
    else (
      try Ok (finish_eval [] (TailCall (cl, args))) with
      | Runtime_error err -> Error err
      | Failure msg -> Error (Chatml_diagnostic.create msg))
  | VBuiltin bf ->
    (try Ok (bf args) with
     | Runtime_error err -> Error err
     | Failure msg -> Error (Chatml_diagnostic.create msg))
  | _ -> Error (Chatml_diagnostic.create "Trying to call a non-function value")
;;

let apply_value_exn (fn : value) (args : value list) : value =
  match apply_value_result fn args with
  | Ok value -> value
  | Error err -> raise (Runtime_error err)
;;

let eval_program (env : env) (prog : resolved_program) : unit =
  (match env.source with
   | Some source when not (String.equal (Chatml_source.contents source) prog.source_text)
     -> raise_runtime_error "Resolved program does not match captured source bytes"
   | None | Some _ -> ());
  let initial_frames : Frame_env.env = [] in
  List.iter prog.stmts ~f:(fun stmt_node -> eval_stmt env initial_frames stmt_node)
;;

let eval_captured_program env ~source prog =
  if not (String.equal (Chatml_source.contents source) prog.source_text)
  then
    Error
      (Chatml_diagnostic.create "Resolved program does not match captured source bytes")
  else (
    let env = { (copy_env env) with source = Some source } in
    try
      eval_program env prog;
      Ok env
    with
    | Runtime_error diagnostic -> Error diagnostic)
;;
