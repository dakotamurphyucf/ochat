open! Core
module M = Chatmd_shell_spec.Manifest

type request =
  { manifest : M.t
  ; summary : string
  }

type response =
  | Authorize_once
  | Reject of string

type grant = { manifest_sha256 : string } [@@deriving sexp, compare, equal]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

type t = request -> response

let summary manifest =
  let yolo =
    List.exists manifest.M.payload.runtimes ~f:(fun runtime ->
      Option.exists runtime.Chatmd_shell_spec.Shell_spec.resolved_profile ~f:(String.equal "builtin:yolo@1"))
  in
  let warning =
    if yolo
    then "CRITICAL: grants the agent the user's full local process authority. "
    else ""
  in
  sprintf
    "%s%d shell runtime(s), %d shell tool(s), manifest %s"
    warning
    (List.length manifest.M.payload.runtimes)
    (List.length manifest.payload.tools)
    manifest.sha256
;;

let authorize authorizer manifest =
  match authorizer { manifest; summary = summary manifest } with
  | Authorize_once -> Ok { manifest_sha256 = manifest.sha256 }
  | Reject message -> Error { code = "shell.manifest_rejected"; message }
;;

let verify grant manifest =
  if String.equal grant.manifest_sha256 manifest.M.sha256
  then Ok ()
  else
    Error
      { code = "shell.manifest_grant_mismatch"
      ; message = "manifest grant does not match the compiled manifest"
      }
;;

let assume_authorized _ = Authorize_once
let deny _ = Reject "shell manifest authorization is unavailable"
