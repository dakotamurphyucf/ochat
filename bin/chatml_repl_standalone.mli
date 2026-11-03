open! Core

(** Private startup files for a default REPL. The directory is owned by this
    invocation, mode 0700, and removed after the callback and its local host
    finish, including on cancellation. Nothing is read from the working
    directory or the user's provider configuration by this bootstrap. *)
type t =
  { policy_file : string
  ; prompt_file : string
  ; workspace : string
  ; source : string
  }

val with_environment : env:Eio_unix.Stdenv.base -> f:(t -> 'a) -> 'a
