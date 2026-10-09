(** Immutable subscription epoch captured at actual notification admission.
    An ID alone cannot identify a delivered occurrence after rearming. This
    record conveys no permission: the owner also checks delivery source, creator,
    generation and the exact committed history identity. *)
type t = private
  { subscription_id : Id.Subscription.t
  ; epoch : int
  }
[@@deriving equal, sexp]

val create : subscription_id:Id.Subscription.t -> epoch:int -> (t, Error.t) result
val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
