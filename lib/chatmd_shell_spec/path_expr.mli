open! Core

(** Serializable path expressions resolved only during runtime instantiation. *)

type base =
  | Workspace
  | Source_dir
  | Prompt_dir
  | Tool_dir
  | Session_dir
  | Cache_dir
  | Home
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  | Absolute of string
  | Relative of
      { base : base
      ; path : string
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [parse ?default_base value] parses one standard path variable or a literal
    path. Relative literals use [default_base], which defaults to
    {!Source_dir}. *)
val parse : ?default_base:base -> string -> (t, string) result

(** [to_string t] returns the canonical serializable spelling of [t]. *)
val to_string : t -> string

(** [base_to_string base] returns the standard variable name for [base]. *)
val base_to_string : base -> string
