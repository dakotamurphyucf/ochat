(** Public and administratively detailed daemon health projections. *)

type status =
  | Healthy
  | Degraded
  | Unhealthy
[@@deriving compare, equal, sexp]

module Request : sig
  type t = { include_details : bool } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Component : sig
  type t =
    { name : string
    ; status : status
    ; message : string option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Response : sig
  type t =
    { status : status
    ; ready : bool
    ; draining : bool
    ; checked_at : Timestamp.t
    ; components : Component.t list
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
