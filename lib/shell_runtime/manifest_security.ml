open! Core

type status =
  { trusted_sources : Trusted_source.evidence list
  ; signature_key_id : string option
  ; signature_issuer : string option
  }
[@@deriving sexp, compare, equal]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

let missing variable =
  Error
    [ { code = "shell.manifest_security_configuration_missing"
      ; message = "required environment variable is unset: " ^ variable
      }
    ]
;;

let trusted_sources ~env ~required ~manifest =
  match Sys.getenv "OCHAT_SHELL_TRUSTED_SOURCES" with
  | None when required -> missing "OCHAT_SHELL_TRUSTED_SOURCES"
  | None -> Ok []
  | Some path ->
    Result.bind
      (Trusted_source.load ~fs:(Eio.Stdenv.fs env) ~path
       |> Result.map_error ~f:(List.map ~f:(fun error ->
         { code = error.Trusted_source.code; message = error.message })))
      ~f:(fun policy ->
        Trusted_source.verify_manifest
          policy
          ?repository_identity:(Sys.getenv "OCHAT_SHELL_REPOSITORY_IDENTITY")
          manifest
        |> Result.map_error ~f:(List.map ~f:(fun error ->
          { code = error.Trusted_source.code; message = error.message })))
;;

let current_unix_seconds env =
  Eio.Time.now (Eio.Stdenv.clock env) |> Float.iround_down_exn |> Int64.of_int
;;

let signature ~env ~required ~manifest =
  match
    Sys.getenv "OCHAT_SHELL_MANIFEST_SIGNATURE",
    Sys.getenv "OCHAT_SHELL_MANIFEST_PUBLIC_KEYS"
  with
  | None, None when required -> missing "OCHAT_SHELL_MANIFEST_SIGNATURE"
  | None, None -> Ok None
  | Some _, None -> missing "OCHAT_SHELL_MANIFEST_PUBLIC_KEYS"
  | None, Some _ -> missing "OCHAT_SHELL_MANIFEST_SIGNATURE"
  | Some signature_path, Some keys_path ->
    let fs = Eio.Stdenv.fs env in
    let audience =
      Sys.getenv "OCHAT_SHELL_SIGNATURE_AUDIENCE" |> Option.value ~default:"ochat"
    in
    let open Result.Let_syntax in
    let%bind signature =
      Manifest_signature.load_signature ~fs ~path:signature_path
      |> Result.map_error ~f:(fun error -> [ { code = error.code; message = error.message } ])
    in
    let%bind public_keys =
      Manifest_signature.load_public_keys ~fs ~path:keys_path
      |> Result.map_error ~f:(fun error -> [ { code = error.code; message = error.message } ])
    in
    Manifest_signature.verify
      ~now_unix:(current_unix_seconds env)
      ~audience
      ~public_keys
      ~manifest
      signature
    |> Result.map_error ~f:(fun error -> [ { code = error.code; message = error.message } ])
    |> Result.map ~f:(fun () -> Some signature)
;;

let verify ~env ~admin_policy ~manifest =
  let open Result.Let_syntax in
  let%bind trusted_sources =
    trusted_sources
      ~env
      ~required:admin_policy.Admin_policy.require_trusted_source
      ~manifest
  in
  let%map signature =
    signature ~env ~required:admin_policy.require_signature ~manifest
  in
  { trusted_sources
  ; signature_key_id = Option.map signature ~f:(fun signature -> signature.Manifest_signature.key_id)
  ; signature_issuer = Option.map signature ~f:(fun signature -> signature.payload.issuer)
  }
;;
