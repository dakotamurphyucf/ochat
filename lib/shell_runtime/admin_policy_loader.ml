open! Core

type error =
  { code : string
  ; message : string
  ; path : string option
  }
[@@deriving sexp, compare, equal]

let load ~fs ~path =
  try
    Eio.Path.(fs / path)
    |> Eio.Path.load
    |> Jsonaf.of_string
    |> Admin_policy.t_of_jsonaf
    |> Result.return
  with
  | exn ->
    Error
      { code = "shell.admin_policy_load_failed"
      ; message = Exn.to_string exn
      ; path = Some path
      }
;;

let load_from_environment ~env =
  match Sys.getenv "OCHAT_SHELL_ADMIN_POLICY" with
  | None -> Ok Admin_policy.permissive
  | Some path -> load ~fs:(Eio.Stdenv.fs env) ~path
;;
