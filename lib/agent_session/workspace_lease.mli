(** Fiber-safe conflict-domain leases for shared and exclusive workspaces. *)

type mode =
  | Shared
  | Exclusive
[@@deriving compare, equal, sexp]

type lease
type t

val create : unit -> t

val acquire
  :  t
  -> conflict_domain:string
  -> session_id:Agent_protocol.Id.Session.t
  -> mode:mode
  -> (lease, Agent_protocol.Error.t) result

(** The same conflict-domain ledger admits a developer Live owner. [owner] is
    the genuine already issued Store owner, not a Session ID or a reconstructed
    principal. The opaque lease token, not equal owner coordinates, controls
    release. Session and developer leases exclude one another using exactly
    the existing Shared/Exclusive rules; neither adds a second registry. *)
val acquire_developer
  :  t
  -> conflict_domain:string
  -> owner:Agent_protocol.Execution_owner.developer
  -> mode:mode
  -> (lease, Agent_protocol.Error.t) result

(** Cleanup-only predicate for an EXACT lease from this ledger. The owner keeps
    its lease while removing its temporary workspace, so no new holder can
    enter during yielding deletion. This ignores only that token's nonce;
    another active holder in the same domain remains visible. A foreign or
    released token is never treated as authority to ignore a holder. *)
val active_other_than : t -> lease -> conflict_domain:string -> bool

(** Exact physical membership of one unreleased token in this ledger. Owner
    equality or a matching numeric nonce from another ledger is insufficient. *)
val is_active : t -> lease -> bool

val release : t -> lease -> unit
val active : t -> conflict_domain:string -> int
