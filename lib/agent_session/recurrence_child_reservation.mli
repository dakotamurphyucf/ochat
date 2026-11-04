open! Core

(** Immutable protected creation intent for one actual selected occurrence. This
    value records reservation only: no artifact installation, child publication,
    link, cancellation or Run outcome is implied. Historical restoration and
    validated construction grant no authority. The owning actor issues a separate
    process-local permit after its real persistence acknowledgement.

    The complete carrier is bounded to 64 KiB, including the original selected
    nominal and compatible unknowns. Full source/target custody remains in the
    retained immutable definition; it is never duplicated here. *)
type t

(** Pure data construction. The frontier owner must supply the exact complete
    JSON of its Ready nominal, rather than re-encoding a projection that loses
    compatible fields. Source/target/artifact associations are derived from the
    actual captured fresh-child definition. IDs are proposed identities only;
    this function neither reserves them nor permits effects. *)
val create
  :  definition:Recurrence_definition.t
  -> control_revision:int64
  -> principal_id:Agent_protocol.Id.Principal.t
  -> child_session_id:Agent_protocol.Id.Session.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> nominal_json:Jsonaf.t
  -> (t, Agent_protocol.Error.t) result

val nominal : t -> Recurrence_nominal.t
val nominal_json : t -> Jsonaf.t
val occurrence : t -> Recurrence_identity.Occurrence.t
val control_revision : t -> int64
val principal_id : t -> Agent_protocol.Id.Principal.t
val child_session_id : t -> Agent_protocol.Id.Session.t
val transaction_id : t -> Agent_protocol.Id.Transaction.t
val artifact_revision_id : t -> Agent_protocol.Id.Prompt_revision.t
val manifest_sha256 : t -> string
val request_sha256 : t -> string

(** Captured source identity, not a current source permit. *)
val source : t -> Agent_protocol.Run_source.t

(** Deterministic scoped lower-store locator. Original principal remains audit
    data on retries by a different freshly authorized caller. This grants no
    reservation, child existence or execution authority. *)
val delegation_key
  :  t
  -> (Agent_store.Delegation_store.Key.t, Agent_protocol.Error.t) result

(** Exact captured association, never authentication. Retained historical
    definitions remain valid evidence after an edit or source fence. New effects
    additionally require current source, birth, enabled control and fresh policy. *)
val validate
  :  t
  -> definition:Recurrence_definition.t
  -> (unit, Agent_protocol.Error.t) result

(** Equality includes complete original carrier and nested nominal custody. *)
val equal : t -> t -> bool

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t

module Association_capacity : sig
  (** Quota for the later owner's complete occurrence/reference association:
      4 KiB, depth 24, at most 64 fields and 128 nodes. The owner must validate
      the actual complete envelope BEFORE external child creation effects.
      This validates storage capacity only, never reference authenticity, child
      existence, installation or link success. Compatible retained fields count. *)
  val limits : Document_schema.Limits.t

  val validate : Jsonaf.t -> (unit, Agent_protocol.Error.t) result

  (** Private conservative future growth witness around the complete immutable
      reservation. Never decode, persist or interpret it as a child reference.
      Enclosing frontier/entry/catalog/host/journal owners measure it additively. *)
  val reservation_projection : t -> Jsonaf.t

  (** The same conservative byte/field/node witness without a reservation copy,
      for an enclosing association array's actual key and wrapper layout. Never
      decode, persist or interpret this storage-only value as association proof. *)
  val growth_projection : unit -> Jsonaf.t
end
