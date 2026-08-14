open! Core

(** Session-backed authorization for exact canonical shell manifests. *)

type source =
  { canonical_source_root : string
  ; source_sha256 : string
  ; repository_identity : string option
  }

type bindings =
  { user_id : string option
  ; host_id : string option
  }

(** [session_authorizer] first checks active persisted grants using complete
    source, manifest, version, import, and host bindings. If none match it
    invokes [fallback]. A successful explicit fallback authorization is
    persisted before it is returned. *)
val session_authorizer
  :  session:Session.t ref
  -> persist:(Session.t -> (unit, string) result)
  -> source:source
  -> bindings:bindings
  -> fallback:Manifest_authorizer.t
  -> Manifest_authorizer.t

val revoke
  :  session:Session.t ref
  -> persist:(Session.t -> (unit, string) result)
  -> grant_id:string
  -> reason:string option
  -> (unit, string) result
