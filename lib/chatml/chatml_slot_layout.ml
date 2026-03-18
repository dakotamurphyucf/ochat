open Core
open Chatml_lang

let fallback_of_expr_shape (e : expr) : Frame_env.packed_slot =
  match e with
  | EInt _ -> Frame_env.Slot Frame_env.SInt
  | EBool _ -> Frame_env.Slot Frame_env.SBool
  | EFloat _ -> Frame_env.Slot Frame_env.SFloat
  | EString _ -> Frame_env.Slot Frame_env.SString
  | EPrim1 (UNegInt, _) -> Frame_env.Slot Frame_env.SInt
  | EPrim1 (UNegFloat, _) -> Frame_env.Slot Frame_env.SFloat
  | EPrim2 ((BIntAdd | BIntSub | BIntMul | BIntDiv), _, _) ->
    Frame_env.Slot Frame_env.SInt
  | EPrim2 ((BFloatAdd | BFloatSub | BFloatMul | BFloatDiv), _, _) ->
    Frame_env.Slot Frame_env.SFloat
  | EPrim2 (BStringConcat, _, _) -> Frame_env.Slot Frame_env.SString
  | EPrim2
      ( (BIntLt | BIntGt | BIntLe | BIntGe | BFloatLt | BFloatGt | BFloatLe | BFloatGe | BEq | BNeq)
      , _
      , _ ) -> Frame_env.Slot Frame_env.SBool
  | _ -> Frame_env.Slot Frame_env.SObj
;;

let choose_binding_slot
      ~(lookup_slot : Source.span -> Frame_env.packed_slot option)
      (rhs_node : expr node)
  : Frame_env.packed_slot
  =
  match lookup_slot rhs_node.span with
  | Some slot -> slot
  | None -> fallback_of_expr_shape rhs_node.value
;;

let of_value (v : value) : Frame_env.packed_slot =
  match v with
  | VInt _ -> Frame_env.Slot Frame_env.SInt
  | VBool _ -> Frame_env.Slot Frame_env.SBool
  | VFloat _ -> Frame_env.Slot Frame_env.SFloat
  | VString _ -> Frame_env.Slot Frame_env.SString
  | _ -> Frame_env.Slot Frame_env.SObj
;;

let matches_value (slot : Frame_env.packed_slot) (v : value) : bool =
  match slot, v with
  | Frame_env.Slot Frame_env.SInt, VInt _ -> true
  | Frame_env.Slot Frame_env.SBool, VBool _ -> true
  | Frame_env.Slot Frame_env.SFloat, VFloat _ -> true
  | Frame_env.Slot Frame_env.SString, VString _ -> true
  | Frame_env.Slot Frame_env.SObj, _ -> true
  | _ -> false
;;

let assert_recursive_slots_are_objects (slots : Frame_env.packed_slot list) : unit =
  List.iter slots ~f:(function
    | Frame_env.Slot Frame_env.SObj -> ()
    | _ -> failwith "internal: non-object recursive slot; typechecker should forbid this")
;;
