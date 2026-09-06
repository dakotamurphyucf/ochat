(** Protocol liveness request and readiness response. *)

module Request : sig
  type t = { payload : Jsonaf.t option } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Response : sig
  type t =
    { payload : Jsonaf.t option
    ; server_time : Timestamp.t
    ; ready : bool
    ; draining : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
