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

type grant = Session.Shell_state.Manifest_grant.persisted [@@deriving bin_io, sexp]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

(** [authorizer] checks caller-owned exact grants before consulting
    [fallback]. A fallback authorization is returned only after [remember]
    commits the exact grant. *)
val authorizer
  :  load:(unit -> (grant list, error) result)
  -> remember:(grant -> (unit, error) result)
  -> now_ns:(unit -> int64)
  -> session_id:string
  -> source:source
  -> bindings:bindings
  -> fallback:Manifest_authorizer.t
  -> Manifest_authorizer.t

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
