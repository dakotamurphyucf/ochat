open! Core

(** Durable ownership boundary for shell approval grants.

    The executor sees this module only through {!executor_store}; persistence,
    revocation, identity matching, and binding checks remain runtime concerns. *)

type grant = Session.Shell_state.Approval_grant.persisted [@@deriving bin_io, sexp]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

type bindings =
  { user_id : string option
  ; host_id : string option
  }
[@@deriving sexp, compare, equal]

type t

(** Process-local, fiber-safe storage. *)
val memory : ?initial:grant list -> bindings:bindings -> unit -> t

(** Session-backed storage. Successful mutations replace [session] and call
    [persist] before becoming visible. *)
val session
  :  session:Session.t ref
  -> persist:(Session.t -> (unit, string) result)
  -> bindings:bindings
  -> t

(** [durable_file] loads or creates a versioned grant file below [fs]. Writes
    use mode 0600, a sibling temporary file, atomic rename, and optional fsync.
    When [integrity_key] is supplied every payload is authenticated with
    HMAC-SHA256. *)
val durable_file
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> ?integrity_key:string
  -> ?durable:bool
  -> bindings:bindings
  -> unit
  -> (t, error) result

(** Returns the matching active grant and records its last-use time. *)
val lookup
  :  t
  -> now:Time_ns.t
  -> session_id:string option
  -> Shell_access.Approval.identity
  -> (grant option, error) result

(** Persists a scoped grant. [Once] is accepted as a no-op and is never
    persisted. *)
val remember
  :  t
  -> now:Time_ns.t
  -> session_id:string option
  -> Shell_access.Approval.identity
  -> Shell_access.Approval.scope
  -> Shell_access.Approval.reviewer_metadata option
  -> (unit, error) result

val revoke
  :  t
  -> now:Time_ns.t
  -> grant_id:string
  -> reason:string option
  -> (unit, error) result

val list : t -> (grant list, error) result

(** Adapts [t] to the minimal callback interface consumed by the executor. *)
val executor_store : t -> Shell_access.Approval.store
