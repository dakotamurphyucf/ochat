open! Core

(** Stable configuration diagnostics safe to expose to users and models. *)

type severity =
  | Error
  | Warning
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { code : string
  ; severity : severity
  ; path : string list
  ; source : Source_ref.t option
  ; message : string
  ; hints : string list
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [create ?path ?source ?hints ~severity ~code message] creates a diagnostic. *)
val create
  :  ?path:string list
  -> ?source:Source_ref.t
  -> ?hints:string list
  -> severity:severity
  -> code:string
  -> string
  -> t

(** [error ?path ?source ?hints ~code message] creates an error diagnostic. *)
val error
  :  ?path:string list
  -> ?source:Source_ref.t
  -> ?hints:string list
  -> code:string
  -> string
  -> t

(** [warning ?path ?source ?hints ~code message] creates a warning diagnostic. *)
val warning
  :  ?path:string list
  -> ?source:Source_ref.t
  -> ?hints:string list
  -> code:string
  -> string
  -> t

(** [to_string t] formats [t] without exposing configuration values beyond its
    safe message. *)
val to_string : t -> string
