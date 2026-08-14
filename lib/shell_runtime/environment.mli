open! Core

(** Runtime environment construction and secret loading. *)

type t =
  { values : string array
  ; secrets : string list
  }

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

(** [find_process_value host name] reads [name] from the explicit process
    environment captured in [host]. *)
val find_process_value : Host.t -> string -> string option

(** [create host ~source specification] constructs the child environment and
    collects values marked secret for output redaction. *)
val create
  :  Host.t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_shell_spec.Shell_spec.environment
  -> (t, error) result

(** [load_secrets host ~source specification] loads configured secret values
    through Eio without exposing them in diagnostics. *)
val load_secrets
  :  Host.t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_shell_spec.Shell_spec.secrets
  -> (string list, error) result
