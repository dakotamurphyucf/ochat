(** Goal admission ceilings, independent of host/runtime limits. No usage or
    billing observations are retained here. Zero permits no new admission. *)
module Dimension : sig
  type t =
    | Admitted_turns
    | Admitted_attempts
    | Observed_tokens
  [@@deriving equal, sexp_of]
end

module Token_stop : sig
  type t = private
    { component : Inference.Observation.Usage.Component.t
    ; ceiling : int64
    }
  [@@deriving equal, sexp_of]

  val create
    :  component:Inference.Observation.Usage.Component.t
    -> ceiling:int64
    -> (t, Error.t) result
end

type t [@@deriving equal, sexp_of]

val create
  :  admitted_turns:int64 option
  -> admitted_attempts:int64 option
  -> observed_tokens:Token_stop.t option
  -> deadline:Timestamp.t option
  -> (t, Error.t) result

val unlimited : t
val admitted_turns : t -> int64 option
val admitted_attempts : t -> int64 option
val observed_tokens : t -> Token_stop.t option
val deadline : t -> Timestamp.t option
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
