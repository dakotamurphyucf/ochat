(** Ochat agent protocol versions and feature negotiation. *)

type t =
  { major : int
  ; minor : int
  }
[@@deriving compare, equal, sexp]

(** [initial] is the initial Ochat agent protocol version, [1.0]. *)
val initial : t

(** [current] is protocol [1.1], adding scoped ingress submission. Servers retain
    [1.0] negotiation without exposing the new closed scope variant to old clients. *)
val current : t

(** Minimum negotiated version for ingress submission and its scope vocabulary. *)
val ingress_minimum : t

(** [create ~major ~minor] creates a non-negative protocol version. *)
val create : major:int -> minor:int -> (t, Error.t) result

(** [negotiate ~client_min ~client_max ~supported] selects the highest mutually
    supported version. The client range must stay within one major version. *)
val negotiate : client_min:t -> client_max:t -> supported:t list -> (t, Error.t) result

(** [validate_feature feature] accepts lowercase dotted feature identifiers. *)
val validate_feature : string -> (string, Error.t) result

(** [to_json t] encodes [t] using explicit [major] and [minor] fields. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes a protocol version. *)
val of_json : Jsonaf.t -> (t, Error.t) result
