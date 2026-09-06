(** RFC 3339 UTC timestamps used by the agent protocol. *)

type t [@@deriving compare, equal, sexp]

(** [now ()] returns the current wall-clock time. *)
val now : unit -> t

(** [of_time_ns time] converts [time] without losing nanosecond precision. *)
val of_time_ns : Core.Time_ns.t -> t

(** [to_time_ns t] returns the underlying absolute time. *)
val to_time_ns : t -> Core.Time_ns.t

(** [of_string encoded] parses an RFC 3339 timestamp with an uppercase UTC [Z] suffix. *)
val of_string : string -> (t, Error.t) result

(** [to_string t] returns an RFC 3339 UTC timestamp. *)
val to_string : t -> string

(** [of_json json] decodes an RFC 3339 UTC JSON string. *)
val of_json : Jsonaf.t -> (t, Error.t) result

(** [to_json t] encodes [t] as an RFC 3339 UTC JSON string. *)
val to_json : t -> Jsonaf.t
