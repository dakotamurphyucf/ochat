open Core

(** Runtime environment for the ChatML interpreter/compiler.

    A [frame] is a fixed-size array of [Obj.t] slots that stores the
    run-time values of variables that are in scope simultaneously.
    A stack of such frames – the [env] – represents nested scopes
    produced by `let`, function calls and module boundaries while
    executing ChatML programs.

    The design goals are:
    {ul
    {- Constant-time random access by index (arrays).}
    {- Compact, GC-friendly representation (uniform [Obj.t]).}
    {- Extensibility – adding a new primitive type should be a
       compile-time change (GADT ensures exhaustiveness).}}

    The module exposes three layers of API:
    {1 Slot descriptor}
    A GADT [('a slot)] tags each position with the concrete OCaml type
    stored there (e.g. [SInt] for [int]).  This information is erased at
    run-time but retained at the type level so that reads/writes remain
    type-safe without dynamic checks.

    {1 Frame allocation}
    {!alloc} and {!alloc_packed} create fresh frames from a layout
    description.

    {1 Slot access}
    {!set} and {!get} are the *only* functions that use [Obj.magic].
    Every other helper (e.g. {!get_int}) is a thin wrapper that
    specialises the slot type.

    {1 Variable descriptors}
    [('a location)] describes where a variable lives at run-time: how
    many frames to pop ([depth]), the index inside that frame, and the
    slot kind.  {!load} and {!store} follow the chain of frames to read
    or write a value.

    {1 Example}
    {[
      open Chatml.Frame_env

      let () =
        (* Build a frame layout *)
        let layout : _ slot list = [ SInt; SFloat ] in
        let fr = alloc layout in
        set_int fr 0 42;
        set_float fr 1 3.14;
        assert (get_int fr 0 = 42);
        assert (Float.equal (get_float fr 1) 3.14)
    ]}
*)

(**************************************************************************)
(*  frame_env.ml – (GADT-based) environment implementation      *)
(**************************************************************************)

(* --------------------------------------------------------------------- *)
(*  1.  Slot descriptor – a GADT that encodes the *runtime* representation *)
(*      of one location inside a frame.                                   *)
(* --------------------------------------------------------------------- *)

(** Slot descriptor – compile-time tag of the OCaml type stored in a
    cell.  The constructor name also doubles as the *run-time layout*
    information of the frame hence it must stay in sync with the way we
    serialise/deserialise values in {!set} and {!get}. *)
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

(** Existential wrapper used when the precise type of the slot is not
    known statically (e.g. when assembling heterogeneous lists).
    All constructors have the same run-time representation so the
    wrapper merely hides the type parameter. *)
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

(** Mutable block of storage for one scope.  Indexing is O(1).
    The representation uses a uniform [Obj.t] array so that immediates
    and heap pointers coexist. *)
type frame = Obj.t array

(** A stack of frames – innermost scope at the head. *)
type env = frame list

(* --------------------------------------------------------------------- *)
(*  3.  Frame allocation                                                  *)
(* --------------------------------------------------------------------- *)

(** [alloc layout] returns a fresh frame whose size equals
    [List.length layout].  All cells are initialised to the immediate
    [0] which is a valid bit-pattern for every slot constructor.  The
    caller is expected to populate each cell before reading it.

    @param layout ordered list describing the *physical* layout of the
           frame.  The order must match the indexing decisions taken by
           the resolver.

    Example allocating a 3-slot frame:
    {[
      let fr = alloc [ SInt; SBool; SString ] in
      assert (Array.length fr = 3)
    ]} *)
let alloc (layout : _ slot list) : frame =
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

(** [alloc_packed layout] is the existentially-typed variant of {!alloc}
    whose [layout] comes as a heterogeneous list of [packed_slot].  The
    current implementation ignores the precise slot type because every
    constructor maps to the same run-time representation.  This may
    change in future versions once frames become more strongly typed.

    It exists purely for ergonomic reasons in passes where the layout is
    discovered dynamically (e.g. the resolver).
*)
let alloc_packed (layout : packed_slot list) : frame =
  Array.create ~len:(List.length layout) (Obj.repr 0)
;;

(* --------------------------------------------------------------------- *)
(*  4.  Slot accessors – the only place where [Obj.magic] is used.        *)
(* --------------------------------------------------------------------- *)

(*  [set frame slot index v] writes [v] into the given frame.             *)
(** [set frame slot idx v] writes [v] into [frame.(idx)].  The caller
    must supply the same [slot] tag that was used during layout
    construction; otherwise undefined behaviour may follow.  No bounds
    checks are performed – the resolver is responsible for generating
    valid indices. *)
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
(** [get frame slot idx] returns the value stored at [frame.(idx)] cast
    back to the appropriate ML type as indicated by [slot].  The
    function uses [Obj.magic] internally but remains type-safe at the
    call-site thanks to the GADT witness. *)
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

(** Convenience wrappers that specialise {!get}/{!set} for each slot
    constructor.  They remove the need to repeat the slot witness at
    every call-site. *)
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

(** Descriptor that pinpoints a variable's storage location at run-time. *)
type 'a location =
  { depth : int (** How many frames to pop before the variable's frame. *)
  ; index : int (** Zero-based offset inside that frame. *)
  ; slot : 'a slot (** Slot descriptor used to cast the value safely. *)
  }

(*  The GADT slot already carries enough information to safely coerce     *)
(*  the [Obj.t] embedded in the frame back into an ['a].  Therefore       *)
(*  [location] does *not* store or expose [Obj.t] to the outside world.   *)

(* --------------------------------------------------------------------- *)
(*  7.  Variable access along a chain of frames                            *)
(* --------------------------------------------------------------------- *)

(** [load loc env] traverses [env] by popping [loc.depth] frames and
    returns the value stored at [loc.index] in that frame.

    @raise Failure if the environment is shallower than [loc.depth]
           (internal invariant violation). *)
let rec load : type a. a location -> env -> a =
  fun loc env ->
  match env, loc.depth with
  | frame :: _, 0 -> get frame loc.slot loc.index
  | _ :: outer, d when d > 0 -> load { loc with depth = d - 1 } outer
  | _ -> failwith "Frame stack underflow in Frame_env.load"
;;

(** [store loc env v] writes [v] to the slot referenced by [loc] inside
    [env].  The traversal logic mirrors {!load}.

    @raise Failure if the environment is shallower than [loc.depth]. *)
let rec store : type a. a location -> env -> a -> unit =
  fun loc env v ->
  match env, loc.depth with
  | frame :: _, 0 -> set frame loc.slot loc.index v
  | _ :: outer, d when d > 0 -> store { loc with depth = d - 1 } outer v
  | _ -> failwith "Frame stack underflow in Frame_env.store"
;;
