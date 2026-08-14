open! Core

(** Strict duration and byte-size values used by shell declarations. *)

type t = private float [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]
type bytes = private int64 [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [parse value] parses a positive decimal duration with unit [ms], [s], [m],
    or [h]. *)
val parse : string -> (t, string) result

(** [to_seconds t] returns [t] in seconds. *)
val to_seconds : t -> float

(** [to_string t] returns a canonical seconds spelling. *)
val to_string : t -> string

(** [parse_bytes value] parses a non-negative integer with optional [B], [KiB],
    [MiB], or [GiB] suffix. *)
val parse_bytes : string -> (bytes, string) result

(** [bytes_to_int64 bytes] returns the byte count. *)
val bytes_to_int64 : bytes -> int64

(** [bytes_to_string bytes] returns a canonical byte spelling. *)
val bytes_to_string : bytes -> string
