open! Core

(** Environment-configured source/signature verification performed after
    canonical compilation and before manifest authorization. *)

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

val verify
  :  env:Eio_unix.Stdenv.base
  -> admin_policy:Admin_policy.t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> (status, error list) result
