open! Core
open Jsonaf.Export

module Root = struct
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

let default ~source ?description () =
  let path = Chatmd_shell_spec.Path_expr.Relative { base = Tool_dir; path = "" } in
  { roots = [ { Root.id = "cwd"; path; description = Some "ochat launch directory" } ]
  ; description
  ; source
  }
;;
