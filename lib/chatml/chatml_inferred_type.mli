open Core
module Shape_kind = Chatml.Chatml_shape_kind
module Equality_requirement = Chatml.Chatml_equality_requirement

(** Compiler-owned type representation. Free variables have per-compilation
    identity and mutable unification links; schemes retain equality obligations.
    Raw rows and variant payloads are internal kinds, never runtime values.
    Consumers must not mutate variables belonging to another compilation. *)
module Env : sig
  type 'a t = 'a String.Map.t

  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find : 'a t -> string -> 'a option
  val find_exn : 'a t -> string -> 'a
  val of_list : (string * 'a) list -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val is_empty : 'a t -> bool
  val bindings : 'a t -> (string * 'a) list
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val iter : 'a t -> f:(string -> 'a -> unit) -> unit
  val choose : 'a t -> string * 'a
  val exists : 'a t -> (string -> 'a -> bool) -> bool
  val fold : 'a t -> init:'acc -> f:(key:string -> data:'a -> 'acc -> 'acc) -> 'acc
  val singleton : string -> 'a -> 'a t
end

type name = string

type typ =
  | Fun of typ list * typ
  | Generic of
      { name : name
      ; equality : Equality_requirement.t option
      }
  | Var of tv ref
  | Con of string * typ list
  | Mu of name * typ
  | Rec_var of name
  | Ref of typ
  | Record of typ (* row *)
  | Module of typ (* exported value row *)
  | Field_receiver of typ * typ (* representation kind, accessible row *)
  | Shape_kind of Shape_kind.t
  | Variant of typ (* row with Variant_payload fields *)
  | Variant_payload of typ list (* constructor arguments, never a value type *)
  | Tuple of
      typ list (* User products have >=2; internal multi-root bundles may have 0/1. *)
  | Array of typ
  | TInt
  | TFloat
  | Boolean
  | String
  | Row of typ Env.t * typ (* fields * tail *)
  | Empty_row
  | Unit

and tv =
  | Bound of typ
  | Free of
      { name : name
      ; level : int
      ; equality : Equality_requirement.t option
      }
