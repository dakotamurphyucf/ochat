open Core

(**************************************************************************)
(*  frame_env.ml – (GADT-based) environment implementation      *)
(**************************************************************************)

(* --------------------------------------------------------------------- *)
(*  1.  Slot descriptor – a GADT that encodes the *runtime* representation *)
(*      of one location inside a frame.                                   *)
(* --------------------------------------------------------------------- *)

type _ slot =
  | SInt : int slot
  | SBool : bool slot
  | SFloat : float slot
  | SString : string slot
  | SObj : Obj.t slot
[@@deriving sexp_of]

(*  The GADT constructor is used to *tag* the slot with its concrete ML   *)
(*  type.  This allows us to safely coerce the [Obj.t] pointer back into  *)
(*  the correct type when reading from the frame.                        *)

(*  The [Obj.t] representation of each slot kind is identical, so we can  *)
(*  safely use a single [Obj.t] pointer to store all of them.            *)
(* When we need to store a slot value whose concrete ML type is not
   statically known we use this *existential* wrapper so that we can
   pass around a heterogeneous list of slots while still retaining the
   ability to pattern-match on the underlying constructor. *)

type packed_slot = Slot : 'a slot -> packed_slot [@@deriving sexp_of]

(*  When necessary you can extend the slot list with, say, [SArray] or    *)
(*  [SRecord].  Because the users *must* pattern-match on the slot when   *)
(*  accessing it, adding a constructor forces every call-site to handle   *)
(*  the new case at compile time.                                         *)

(* --------------------------------------------------------------------- *)
(*  2.  Frame & environment                                               *)
(* --------------------------------------------------------------------- *)

(*  A frame is a fixed-size OCaml array.  The choice of [Obj.t array]
    allows us to mix OCaml *immediates* (tagged integers, characters,
    booleans) and heap-allocated pointers inside the same structure
    while remaining fully compatible with the GC. *)

type frame = Obj.t array

(** innermost frame is at the head of the list *)
type env = frame list

(* --------------------------------------------------------------------- *)
(*  3.  Frame allocation                                                  *)
(* --------------------------------------------------------------------- *)

let alloc (layout : _ slot list) : frame =
  (* All cells are initialised to the immediate [0].  This is fine for    *)
  (* any slot kind because we expect the compiler/resolver to write a     *)
  (* value before it is ever read.                                        *)
  Array.create ~len:(List.length layout) (Obj.repr 0)
;;

(** [alloc_packed layout] – convenience wrapper that takes a list of
    [packed_slot] (heterogeneous) instead of a homogeneous ['a slot list]
    and returns a fresh frame.  At the moment the GC representation of
    every slot constructor is identical – an [Obj.t] pointer or
    immediate – therefore we can safely ignore the *precise* slot type
    when allocating.  The function exists purely so that higher-level
    passes (resolver / interpreter) do *not* have to coerce their
    heterogeneous [packed_slot list] into a fake homogeneous list just to
    satisfy the type checker.

    As soon as we upgrade the runtime to allocate *fully* typed frames
    (e.g. using custom C blocks or unboxed float arrays) this helper will
    need to be revisited – its implementation, not its interface. *)

let alloc_packed (layout : packed_slot list) : frame =
  Array.create ~len:(List.length layout) (Obj.repr 0)
;;

(* --------------------------------------------------------------------- *)
(*  4.  Slot accessors – the only place where [Obj.magic] is used.        *)
(* --------------------------------------------------------------------- *)

(*  [set frame slot index v] writes [v] into the given frame.             *)
let set : type a. frame -> a slot -> int -> a -> unit =
  fun fr slot idx v ->
  match slot with
  | SInt -> Array.unsafe_set fr idx (Obj.repr (v : int))
  | SBool -> Array.unsafe_set fr idx (Obj.repr (if v then 1 else 0))
  | SFloat -> Array.unsafe_set fr idx (Obj.repr v)
  | SString -> Array.unsafe_set fr idx (Obj.repr v)
  | SObj -> Array.unsafe_set fr idx (Obj.repr v)
;;

(*  [get frame slot index] retrieves a value of the correct ML type.      *)
let get : type a. frame -> a slot -> int -> a =
  fun fr slot idx ->
  match slot with
  | SInt ->
    let n : int = Obj.magic (Array.unsafe_get fr idx) in
    n
  | SBool ->
    let n : int = Obj.magic (Array.unsafe_get fr idx) in
    n <> 0
  | SFloat -> Obj.magic (Array.unsafe_get fr idx)
  | SString -> Obj.magic (Array.unsafe_get fr idx)
  | SObj -> Array.unsafe_get fr idx
;;

(* --------------------------------------------------------------------- *)
(*  5.  High-level helpers                                               *)
(* --------------------------------------------------------------------- *)

(*  The following functions are *purely* convenience wrappers that make   *)
(*  client code a little nicer to read.                                   *)

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

(* --------------------------------------------------------------------- *)
(*  6.  Variable descriptor                                               *)
(* --------------------------------------------------------------------- *)

(*  When the *resolver* pass rewrites the AST it will replace every       *)
(*  occurrence of an identifier by a record of this type.  The phantom    *)
(*  type parameter ['a] is the value’s ML *representation* after type     *)
(*  specialisation.                                                       *)

type 'a location =
  { depth : int (** how many frames to pop *)
  ; index : int (** position inside that frame *)
  ; slot : 'a slot
  }

(*  The GADT slot already carries enough information to safely coerce     *)
(*  the [Obj.t] embedded in the frame back into an ['a].  Therefore       *)
(*  [location] does *not* store or expose [Obj.t] to the outside world.   *)

(* --------------------------------------------------------------------- *)
(*  7.  Variable access along a chain of frames                            *)
(* --------------------------------------------------------------------- *)

let rec load : type a. a location -> env -> a =
  fun loc env ->
  match env, loc.depth with
  | frame :: _, 0 -> get frame loc.slot loc.index
  | _ :: outer, d when d > 0 -> load { loc with depth = d - 1 } outer
  | _ ->
    (* An out-of-bounds access indicates a bug in the resolver – not a   *)
    (* user program – so we raise immediately.                            *)
    failwith "Frame stack underflow in Frame_env.load"
;;

let rec store : type a. a location -> env -> a -> unit =
  fun loc env v ->
  match env, loc.depth with
  | frame :: _, 0 -> set frame loc.slot loc.index v
  | _ :: outer, d when d > 0 -> store { loc with depth = d - 1 } outer v
  | _ -> failwith "Frame stack underflow in Frame_env.store"
;;
