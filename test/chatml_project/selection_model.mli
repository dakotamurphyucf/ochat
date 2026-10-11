open Core

(** Independent finite selector model. No production project or JSON module is
    imported; fixtures supply their authored environment names and selectors.
    Selection finds the first present name in priority order, then validates it
    against this package's names. File overrides replace the selected name. *)
type t =
  { environments : string list
  ; package_default : string option
  ; entry_environment : string option
  ; files : (string * string) list
  }

module Request : sig
  type t =
    | Entry of string option
    | Library of string option
  [@@deriving sexp_of]
end

module Error : sig
  type t =
    | Missing_selector
    | Unknown_environment of string
  [@@deriving equal, sexp_of]
end

val select : t -> request:Request.t -> (string, Error.t) Result.t
val scope_for : t -> selected:string -> source:string -> string
