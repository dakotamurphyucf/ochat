(** Multi-phase normalization and validation for raw server configuration. *)

val validate
  :  env:Eio_unix.Stdenv.base
  -> Config_parser.Raw_config.t
  -> (Config.t, Config.Diagnostic.t list) result
