(** Immutable checked host snapshot for effective membership/filter projection.
    Construct only from [Organization_store.snapshot_checked] after collecting
    actor/index snapshots without an organization lock. No actor activation/IO. *)
type t

val create : Agent_store.Organization_state.t -> t
val host_id : t -> Agent_protocol.Id.Server.t
val revision : t -> int64

(** Explicit filters require View_organization and owner/admin visibility of each
    live referenced group. Tombstoned/private filters return organization-not-found;
    default query requires no additional organization scope. *)
val authorize_query
  :  t
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Session_organization.Query.t
  -> (unit, Agent_protocol.Error.t) result

(** First validate raw IDs against retained authoritative entries, before visibility
    or redaction. Missing IDs are corruption; tombstones remain valid. Then no
    View_organization returns empty. Otherwise preserve only live groups visible
    to principal. Retained tombstones are normal historical refs; an ID absent from
    authoritative state is corruption, never silent complete-results omission.
    Names/creator metadata are never copied into the returned ID-only values. *)
val effective
  :  t
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Session_organization.Values.t
  -> (Agent_protocol.Session_organization.Values.t, Agent_protocol.Error.t) result
