(** Terminal background outcomes, separate from initial tool acknowledgements. *)
type t =
  | Succeeded of Jsonaf.t
  | Failed of Invocation.tool_error
  | Cancelled of string
  | Expired
[@@deriving sexp]

type wake =
  | Request_turn
  | Next_turn
  | No_wake
[@@deriving compare, equal, sexp]

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val wake_to_json : wake -> Jsonaf.t
val wake_of_json : Jsonaf.t -> (wake, Error.t) result
