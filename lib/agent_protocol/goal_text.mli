(** Validated goal text. Bounds count UTF-8 bytes; text is preserved verbatim.
    Malformed UTF-8, NUL and controls other than tab/newline are rejected. *)
module type S = sig
  type t [@@deriving equal, sexp_of]

  val max_bytes : int
  val of_string : string -> (t, Error.t) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Nonblank, at most 8 KiB. *)
module Objective : S

(** Progress may be empty, at most 16 KiB. *)
module Progress : S

(** Nonblank reason or evidence, at most 4 KiB. *)
module Reason : S
