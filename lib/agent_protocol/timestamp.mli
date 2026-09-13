(** RFC 3339 UTC timestamps used by the agent protocol. *)

type t [@@deriving compare, equal, sexp]

(** [now ()] returns the current wall-clock time. *)
val now : unit -> t

(** [of_time_ns time] converts [time] without losing nanosecond precision. *)
val of_time_ns : Core.Time_ns.t -> t

(** [to_time_ns t] returns the underlying absolute time. *)
val to_time_ns : t -> Core.Time_ns.t

(** [diff_ns t since] returns the signed nanosecond difference without narrowing
    it to a Time_ns span. The difference of two timestamps fits in int64. *)
val diff_ns : t -> t -> int64

(** [add_ms t delay_ms] adds a nonnegative millisecond delay. Rejects negative
    delays and unrepresentable endpoints without wrapping or float rounding. *)
val add_ms : t -> int -> (t, Error.t) result

(** [of_string encoded] parses an RFC 3339 timestamp with an uppercase UTC [Z] suffix. *)
val of_string : string -> (t, Error.t) result

(** [to_string t] returns an RFC 3339 UTC timestamp. *)
val to_string : t -> string

(** [of_json json] decodes an RFC 3339 UTC JSON string. *)
val of_json : Jsonaf.t -> (t, Error.t) result

(** [to_json t] encodes [t] as an RFC 3339 UTC JSON string. *)
val to_json : t -> Jsonaf.t
