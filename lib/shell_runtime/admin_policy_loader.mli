open! Core

(** Eio-backed loader for host administrative shell policy. *)

type error =
  { code : string
  ; message : string
  ; path : string option
  }
[@@deriving sexp, compare, equal]

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> path:string -> (Admin_policy.t, error) result

(** Loads the path named by [OCHAT_SHELL_ADMIN_POLICY]. An unset variable
    selects {!Admin_policy.permissive}; a configured but unreadable policy
    fails closed. *)
val load_from_environment : env:Eio_unix.Stdenv.base -> (Admin_policy.t, error) result

(* File loading maps expected Eio filesystem errors, JSON syntax and generated
    JSON conversion failures only. Cancellation, Timeout and unexpected failures
    propagate with their original exception/backtrace. Host policy files are
    operator-selected; this API does not impose a new file-size/depth policy. *)
