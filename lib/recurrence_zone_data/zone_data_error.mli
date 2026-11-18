open Core

type t =
  | Too_large
  | Malformed of
      { byte_offset : int
      ; reason : string
      }
  | Unsupported_version of char
  | Unsupported_leap_seconds
  | Unsupported_footer of string
  | Unsupported_offset of int
  | Coverage_unavailable
  | Projection_limit
  | Invalid of Recurrence_rules.Timing_error.t
[@@deriving equal, sexp_of]
