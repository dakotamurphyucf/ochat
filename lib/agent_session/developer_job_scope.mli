(** Physical proof for a developer job worker. A decoded Running row, owner
    coordinate or claim-time document revision cannot create a live parent. *)
open! Core

type t
type active

(** The issuer is tied to the actual retained store. [max_active] is positive
    and finite; construction starts no worker or callback. *)
val create
  :  store:Agent_store.Developer_run_store.t
  -> initial:Agent_store.Developer_run_store.handle
  -> max_active:int
  -> (t, Agent_protocol.Error.t) result

(** Enter an exact durably issued Claim once. The Store validates its physical
    issuing handle, current Active epoch and domain before consuming its entry
    bit; foreign rejection leaves the token unconsumed. The callback receives
    a child Eio.Switch. Every child fiber/resource belongs to this switch and
    joins before [active] expires. The actual callback fiber is registered for
    cancellation and close/join; no detached or synthetic worker is created. *)
val with_claim
  :  t
  -> handle:Agent_store.Developer_run_store.handle
  -> claim:Agent_store.Developer_run_store.Claim.t
  -> (active:active -> sw:Eio.Switch.t -> 'a)
  -> ('a, Agent_protocol.Error.t) result

(** Direct-parent proof check for native admission. It verifies this physical
    issuer, owner/capture/job/attempt, same domain, live entered scope and an
    actual latest-epoch/phase read from the issuing Store on EVERY call. That
    read may yield; local liveness/domain is rechecked immediately afterward.
    A retained token after return/cancellation/reset fails. Current principal,
    source capture and selected capability authorization belong to the host. *)
val check_parent
  :  t
  -> active
  -> owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> job_id:Agent_protocol.Id.Job.t
  -> attempt:int
  -> (unit, Agent_protocol.Error.t) result

val claim : active -> Agent_store.Developer_run_store.Claim.t

(** Verify an actual new retained Store handle for the same host/run/principal
    and exact next durable epoch. Revoke old active proofs for new effects;
    their physical workers may still complete exact-attempt bookkeeping. An
    omitted notification is harmless to [check_parent]'s Store read. *)
val advance_epoch
  :  t
  -> new_current:Agent_store.Developer_run_store.handle
  -> (unit, Agent_protocol.Error.t) result

(** Exclude new entries, cancel and join every registered physical worker and
    its children. A worker cannot join itself; self-close rejects before
    changing the issuer. This is resource lifetime, not a scheduler. *)
val close_and_wait : t -> (unit, Agent_protocol.Error.t) result
