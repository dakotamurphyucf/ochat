open Core

(** Runtime environment for the ChatML interpreter/compiler.

    A [frame] stores both:

    - the actual runtime cells, and
    - the packed slot layout those cells were allocated with.

    Keeping the layout at runtime lets us validate that resolver- and
    evaluator-selected slot descriptors stay in sync.  That turns a class
    of silent miscompilations into immediate internal failures.
*)

type _ slot =
  | SInt : int slot
  | SBool : bool slot
  | SFloat : float slot
  | SString : string slot
  | SObj : Obj.t slot
[@@deriving sexp_of]

type packed_slot = Slot : 'a slot -> packed_slot [@@deriving sexp_of]

type frame =
  { cells : Obj.t array
  ; layout : packed_slot array
  }

type env = frame list

let equal_slot : type a b. a slot -> b slot -> bool =
  fun lhs rhs ->
  match lhs, rhs with
  | SInt, SInt -> true
  | SBool, SBool -> true
  | SFloat, SFloat -> true
  | SString, SString -> true
  | SObj, SObj -> true
  | _ -> false
;;

let equal_packed_slot (Slot lhs) (Slot rhs) = equal_slot lhs rhs

let string_of_slot : type a. a slot -> string = function
  | SInt -> "int"
  | SBool -> "bool"
  | SFloat -> "float"
  | SString -> "string"
  | SObj -> "obj"
;;

let string_of_packed_slot (Slot slot) = string_of_slot slot
let length (fr : frame) = Array.length fr.cells
let layout (fr : frame) = fr.layout

let alloc_packed (layout : packed_slot list) : frame =
  { cells = Array.create ~len:(List.length layout) (Obj.repr 0)
  ; layout = Array.of_list layout
  }
;;

let alloc (layout : _ slot list) : frame =
  alloc_packed (List.map layout ~f:(fun slot -> Slot slot))
;;

let assert_valid_index (fr : frame) (idx : int) : unit =
  if idx < 0 || idx >= Array.length fr.cells
  then
    failwith
      (Printf.sprintf
         "Frame_env: index %d out of bounds for frame length %d"
         idx
         (Array.length fr.cells))
;;

let assert_slot (fr : frame) ~(idx : int) (expected : packed_slot) : unit =
  assert_valid_index fr idx;
  let actual = fr.layout.(idx) in
  if not (equal_packed_slot actual expected)
  then
    failwith
      (Printf.sprintf
         "Frame_env: slot mismatch at index %d (expected %s, found %s)"
         idx
         (string_of_packed_slot expected)
         (string_of_packed_slot actual))
;;

let set : type a. frame -> a slot -> int -> a -> unit =
  fun fr slot idx v ->
  assert_slot fr ~idx (Slot slot);
  match slot with
  | SInt -> Array.unsafe_set fr.cells idx (Obj.repr (v : int))
  | SBool -> Array.unsafe_set fr.cells idx (Obj.repr (if v then 1 else 0))
  | SFloat -> Array.unsafe_set fr.cells idx (Obj.repr v)
  | SString -> Array.unsafe_set fr.cells idx (Obj.repr v)
  | SObj -> Array.unsafe_set fr.cells idx (Obj.repr v)
;;

let get : type a. frame -> a slot -> int -> a =
  fun fr slot idx ->
  assert_slot fr ~idx (Slot slot);
  match slot with
  | SInt ->
    let n : int = Obj.magic (Array.unsafe_get fr.cells idx) in
    n
  | SBool ->
    let n : int = Obj.magic (Array.unsafe_get fr.cells idx) in
    n <> 0
  | SFloat -> Obj.magic (Array.unsafe_get fr.cells idx)
  | SString -> Obj.magic (Array.unsafe_get fr.cells idx)
  | SObj -> Array.unsafe_get fr.cells idx
;;

let get_int fr idx = get fr SInt idx
let set_int fr idx = set fr SInt idx
let get_bool fr idx = get fr SBool idx
let set_bool fr idx = set fr SBool idx
let get_float fr idx = get fr SFloat idx
let set_float fr idx = set fr SFloat idx
let get_str fr idx = get fr SString idx
let set_str fr idx = set fr SString idx
let get_obj fr idx = get fr SObj idx
let set_obj fr idx = set fr SObj idx

type 'a location =
  { depth : int
  ; index : int
  ; slot : 'a slot
  }

let rec load : type a. a location -> env -> a =
  fun loc frames ->
  match frames, loc.depth with
  | frame :: _, 0 -> get frame loc.slot loc.index
  | _ :: outer, d when d > 0 -> load { loc with depth = d - 1 } outer
  | _ -> failwith "Frame stack underflow in Frame_env.load"
;;

let rec store : type a. a location -> env -> a -> unit =
  fun loc frames value ->
  match frames, loc.depth with
  | frame :: _, 0 -> set frame loc.slot loc.index value
  | _ :: outer, d when d > 0 -> store { loc with depth = d - 1 } outer value
  | _ -> failwith "Frame stack underflow in Frame_env.store"
;;
