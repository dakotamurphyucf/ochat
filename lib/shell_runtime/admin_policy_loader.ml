open! Core

type error =
  { code : string
  ; message : string
  ; path : string option
  }
[@@deriving sexp, compare, equal]

let load ~fs ~path =
  let failed message =
    Error { code = "shell.admin_policy_load_failed"; message; path = Some path }
  in
  let open Result.Let_syntax in
  let%bind contents =
    try Ok (Eio.Path.(fs / path) |> Eio.Path.load) with
    | Eio.Io _ as exn -> failed (Exn.to_string exn)
  in
  let%bind json =
    Jsonaf.parse contents
    |> Result.map_error ~f:(fun message ->
      { code = "shell.admin_policy_load_failed"
      ; message = Error.to_string_hum message
      ; path = Some path
      })
  in
  try Ok (Admin_policy.t_of_jsonaf json) with
  | Jsonaf_kernel.Conv.Of_jsonaf_error _ as exn -> failed (Exn.to_string exn)
;;

let load_from_environment ~env =
  match Sys.getenv "OCHAT_SHELL_ADMIN_POLICY" with
  | None -> Ok Admin_policy.permissive
  | Some path -> load ~fs:(Eio.Stdenv.fs env) ~path
;;
