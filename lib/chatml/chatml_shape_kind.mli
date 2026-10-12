open Core

(** Runtime representations admitted by field receivers. Module environments
    retain their identity and never become persistent record values. *)
type t =
  | Record
  | Module
[@@deriving equal, sexp_of]

val name : t -> string
