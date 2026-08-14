open! Core

(** Trust policy binding source authority to canonical roots, repository
    identity, exact source digests, and optional signers. *)

type capability_ceiling =
  { network : bool
  ; child_processes : bool
  ; arbitrary_code : bool
  ; privilege_change : bool
  ; external_backends : bool
  ; executable_hooks : bool
  ; model_reviewers : bool
  ; durable_approvals : bool
  }
[@@deriving sexp, compare, equal, jsonaf]

type entry =
  { id : string
  ; canonical_root : string
  ; repository_identity : string option
  ; source_sha256 : string list
  ; signer_ids : string list
  ; capabilities : capability_ceiling
  }
[@@deriving sexp, compare, equal, jsonaf]

type policy = { entries : entry list } [@@deriving sexp, compare, equal, jsonaf]

type evidence =
  { source_file : string
  ; source_sha256 : string
  ; trusted_source_id : string
  ; repository_identity : string option
  ; signer_id : string option
  }
[@@deriving sexp, compare, equal]

type error =
  { code : string
  ; source_file : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

val verify_manifest
  :  policy
  -> ?repository_identity:string
  -> ?signer_id:string
  -> Chatmd_shell_spec.Manifest.t
  -> (evidence list, error list) result

(** Loads a JSON trust policy using Eio. *)
val load
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> path:string
  -> (policy, error list) result
