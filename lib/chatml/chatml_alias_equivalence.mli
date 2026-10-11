open Core
open Chatml_inferred_type

module Constructor : sig
  type t =
    | Int
    | Float
    | Bool
    | String
    | Unit
    | Empty_row
    | Array
    | Ref
    | Record
    | Module
    | Variant
    | Variant_payload
    | Tuple
    | Con of string
    | Shape_kind of Chatml.Chatml_shape_kind.t
    | Field_receiver
  [@@deriving equal]
end

(** Immutable symbolic alias expansions for interface type equality and fixed-arity
    universal value claims. Parameters identify argument positions. Recursive binder identifiers
    must be unique across an expansion; every recursive variable must be bound.
    Nodes mirror validated inference type constructors and their arities.

    [Legacy_function] retains conditional unit-argument erasure and flattening of
    a substituted function result. [Function] retains explicit arity. Construct
    legacy arrows through [arrow] so statically known cases are normalized. *)
type t =
  | Parameter of int
  | Function of t list * t
  | Legacy_function of argument list * result
  | Node of Constructor.t * t list
  | Row of t String.Map.t * t
  | Recursive of int * t
  | Recursive_variable of int

and argument =
  | Required of t
  | Unless_unit of t

and result =
  | Direct of t
  | Flatten of t

(** Normalize one legacy arrow, without assuming a parameter's substituted
    outer shape. This does not change ordinary annotation elaboration. *)
val arrow : t -> result:t -> checkpoint:(unit -> unit) -> t

(** Bind [binder] when the body contains that recursive variable. *)
val recursive : t -> binder:int -> checkpoint:(unit -> unit) -> t

(** Project an immutable closed inference type. Mutable/generic variables and
    free recursive variables reject. [fresh_binder] allocates identifiers unique
    across the enclosing symbolic expansion; no supplied type is mutated. *)
val of_closed_type
  :  typ
  -> fresh_binder:(unit -> int)
  -> checkpoint:(unit -> unit)
  -> (t, string) Result.t

(** Establish equality by structural comparison of normalized symbolic terms,
    including alpha-renamed recursive binders. Success proves equal application
    behavior for every substitution; no representative substitutions are sampled.

    This is deliberately incomplete: different recursive unfoldings and other
    identities not expressed by the symbolic normalization can return [false].
    Recursive unfoldings are not equated because legacy arrows inspect the outer
    constructor (including [Mu]) before flattening. All traversals poll
    [checkpoint], and cancellation propagates unchanged. *)
val equivalent : t -> t -> checkpoint:(unit -> unit) -> bool
