(** Multi-phase normalization and validation for raw server configuration.
    Prompt preflight uses bounded owned parsing, including captured-document and
    configured-script declarations. It does not acquire documents or project
    dependencies, compile scripts, or initialize modules. Actual source capture
    and admission remain the catalog's responsibility. Cancellation propagates. *)

val validate
  :  env:Eio_unix.Stdenv.base
  -> Config_parser.Raw_config.t
  -> (Config.t, Config.Diagnostic.t list) result
