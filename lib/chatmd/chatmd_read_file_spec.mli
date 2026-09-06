open! Core
open Jsonaf.Export

(** Configurable roots for the built-in [read_file] tool. *)

module Root : sig
  type t =
    { id : string
    ; path : Chatmd_shell_spec.Path_expr.t
    ; description : string option
    }
  [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]
end

type t =
  { roots : Root.t list
  ; description : string option
  ; source : Chatmd_shell_spec.Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [default ~source ?description ()] permits reads beneath the ochat launch
    directory, represented by [${tool_dir}]. *)
val default : source:Chatmd_shell_spec.Source_ref.t -> ?description:string -> unit -> t
