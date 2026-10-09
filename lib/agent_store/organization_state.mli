(** Pure host organization state. IDs remain retained after deletion. Scope and
    owner/admin authorization precede receipt replay and every fresh mutation.
    No execution configuration or session membership participates. *)
module Project_entry : sig
  type t =
    { group : Agent_protocol.Organization_group.Project.t
    ; deleted_at : Agent_protocol.Timestamp.t option
    }
end

module Collection_entry : sig
  type t =
    { group : Agent_protocol.Organization_group.Collection.t
    ; deleted_at : Agent_protocol.Timestamp.t option
    }
end

module Receipt : sig
  type t =
    { key : Idempotency_store.Key.t
    ; request_digest : string
    ; result : Agent_protocol.Organization_result.t
    ; created_at : Agent_protocol.Timestamp.t
    ; expires_at : Agent_protocol.Timestamp.t
    }

  (** Exact retained lifetime, including original digest/result and expiry. *)
  val equal : t -> t -> bool
end

module Mutation : sig
  type t =
    | Create_project of Agent_protocol.Organization_request.Create.t
    | Update_project of Agent_protocol.Organization_request.Project.Update.t
    | Delete_project of Agent_protocol.Organization_request.Project.Delete.t
    | Create_collection of Agent_protocol.Organization_request.Create.t
    | Update_collection of Agent_protocol.Organization_request.Collection.Update.t
    | Delete_collection of Agent_protocol.Organization_request.Collection.Delete.t
end

module Candidate : sig
  type t =
    | Project of Agent_protocol.Id.Project.t
    | Collection of Agent_protocol.Id.Collection.t
end

type t

val empty : server_id:Agent_protocol.Id.Server.t -> t

val restore
  :  server_id:Agent_protocol.Id.Server.t
  -> revision:int64
  -> projects:Project_entry.t list
  -> collections:Collection_entry.t list
  -> receipts:Receipt.t list
  -> (t, Agent_protocol.Error.t) result

val server_id : t -> Agent_protocol.Id.Server.t
val revision : t -> int64
val projects : t -> Project_entry.t list
val collections : t -> Collection_entry.t list
val receipts : t -> Receipt.t list

val visible_project
  :  t
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Id.Project.t
  -> (Agent_protocol.Organization_group.Project.t, Agent_protocol.Error.t) result

val visible_collection
  :  t
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Id.Collection.t
  -> (Agent_protocol.Organization_group.Collection.t, Agent_protocol.Error.t) result

val lookup_receipt
  :  t
  -> principal:Agent_protocol.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> key:Idempotency_store.Key.t
  -> request_digest:string
  -> (Agent_protocol.Organization_result.t option, Agent_protocol.Error.t) result

(** Fresh successful mutations install an exact terminal receipt in this same
    immutable state. Expired receipts may retire, never tombstones. Maximum 4096
    entries per kind and 4096 unexpired receipts; no early retry eviction. *)
val request_digest : Mutation.t -> (string, Agent_protocol.Error.t) result

val apply
  :  t
  -> principal:Agent_protocol.Principal.t
  -> audit:Idempotency_store.Command_audit.t
  -> now:Agent_protocol.Timestamp.t
  -> candidate:Candidate.t option
  -> Mutation.t
  -> (t * Agent_protocol.Organization_result.t, Agent_protocol.Error.t) result

(** Validate immutable identity/tombstone retention and explicit receipt expiry. *)
val validate_retention
  :  t
  -> next_state:t
  -> now:Agent_protocol.Timestamp.t
  -> (unit, Agent_protocol.Error.t) result

(** Retained identity lookup; does not authorize visibility. Returned immutable
    entries include tombstones and belong to this host snapshot. *)
val project_entry : t -> Agent_protocol.Id.Project.t -> Project_entry.t option

val collection_entry : t -> Agent_protocol.Id.Collection.t -> Collection_entry.t option

(** Current manage/host/owner-or-admin checks, including tombstones for receipt
    and no-op authorization. [require_live] additionally rejects tombstones at
    fresh association commit; absent IDs reject before ownership checks. *)
val authorize_membership
  :  t
  -> principal:Agent_protocol.Principal.t
  -> host_id:Agent_protocol.Id.Server.t
  -> references:Agent_protocol.Session_organization.Values.t
  -> require_live:bool
  -> (unit, Agent_protocol.Error.t) result
