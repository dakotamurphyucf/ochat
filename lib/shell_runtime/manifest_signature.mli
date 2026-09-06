open! Core

(** Ed25519 signatures over canonical shell manifests and every authority-
    relevant version/source field. Runtime code loads public keys only. *)

type payload =
  { canonical_manifest_sha256 : string
  ; encoding_version : string
  ; builtin_versions : (string * string) list
  ; issuer : string
  ; audience : string list
  ; issued_at_unix : int64
  ; expires_at_unix : int64 option
  ; imported_source_sha256 : (string * string) list
  }
[@@deriving sexp, compare, equal, jsonaf]

type t =
  { key_id : string
  ; algorithm : string
  ; payload : payload
  ; signature_base64 : string
  }
[@@deriving sexp, compare, equal, jsonaf]

type public_key =
  { key_id : string
  ; public_key_base64 : string
  }
[@@deriving sexp, compare, equal, jsonaf]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

val payload
  :  manifest:Chatmd_shell_spec.Manifest.t
  -> issuer:string
  -> audience:string list
  -> issued_at_unix:int64
  -> expires_at_unix:int64 option
  -> payload

val canonical_payload : payload -> string

val verify
  :  now_unix:int64
  -> audience:string
  -> public_keys:public_key list
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> t
  -> (unit, error) result

val load_signature
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> path:string
  -> (t, error) result

val load_public_keys
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> path:string
  -> (public_key list, error) result
