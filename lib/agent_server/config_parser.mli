(** Lossless-enough source wrapper for independent config validation. *)

module Raw_config : sig
  type t =
    { source_file : string
    ; forms : Core.Sexp.t list
    }
end

val parse_string
  :  source_file:string
  -> string
  -> (Raw_config.t, Config.Diagnostic.t list) result

val load
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> (Raw_config.t, Config.Diagnostic.t list) result
