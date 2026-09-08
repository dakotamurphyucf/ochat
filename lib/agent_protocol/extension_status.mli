(** Payload-free extension lifecycle summaries. These deliberately omit arguments,
    results, errors, schemas, capability identities and arbitrary correlation text.
    Servers must still filter them by the principal's security-view scope. *)

type kind =
  | Invocation
  | Subscription
  | Delivery
[@@deriving compare, equal, sexp]

type t = private
  { kind : kind
  ; id : string
  ; generation : int
  ; state : string
  }
[@@deriving sexp]

val invocation : Invocation.t -> t
val subscription : Subscription.t -> t
val delivery : Delivery.t -> t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Rejects duplicate identities, including ambiguous updates in one event. *)
val list_of_json : Jsonaf.t -> (t list, Error.t) result
