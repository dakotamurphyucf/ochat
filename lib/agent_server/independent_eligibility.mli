open! Core

(** Request-local source DATA used only by the Factory's shared Independent
    permission walk. The retained Actors, sources and digests issue no permission. *)
type t

(** Nonactivating metadata dependencies bound by the actual Factory. The target
    lookup returns its canonical metadata entry. The existing permission walk
    separately owns ledger linkage, host authorization and profile decisions. *)
type host =
  { registry : Session_registry.t
  ; store : Agent_store.Session_store.t
  ; env : Eio_unix.Stdenv.base
  ; prompts : Agent_session.Prompt_catalog.t
  ; script_preparation : Agent_session.Runtime_script_preparation.t option
  ; content_limits : Chatmd_prompt_content.Limits.t option
  ; max_depth : int
  ; generated_limits : Chatml_compilation.limits option
  ; generated_source_limits : Chatmd_source_bundle.limits option
  ; target :
      Agent_protocol.Id.Session.t
      -> (Session_registry.entry, Agent_protocol.Error.t) result
  ; profile :
      Agent_session.Session_state.t
      -> (Agent_session.Permission_policy.t, Agent_protocol.Error.t) result
  }

(** Run only after the existing request admission. This bounded topology prepass
    acquires Actor observations and prepares exact saved source DATA for
    [recheck], the existing Independent walker. The supplied check is the current
    request's existing guard and is not retained. Retained Sources require the
    original lexical parent runtime loan. *)
val prepare
  :  ?retained_sources:Agent_session.Runtime_builder.ancestor_sources
  -> host
  -> walk:Independent_resources.host
  -> parent:Session_registry.entry
  -> check_request:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** Pure and domain-first. Recheck all actual Owner tuples, Actor observation
    read-sets and any physical Sources against current accepted State. Recompute
    the same digests the walker saw. Combine with [check_reservations] at an
    effect boundary. There is no new permission or reauthorization. *)
val check_current : t -> (unit, Agent_protocol.Error.t) result

(** Nonyielding, domain-first check of the same saved Registry members and
    current reservation phases. Retryable reservation contention leaves
    durable authority unchanged. This check supplies no lease past a wait. *)
val check_reservations : t -> (unit, Agent_protocol.Error.t) result

(** Compare an already resolved unchanged record with the exact earlier
    shared-walk input, without performing a second authorization. *)
val check_record
  :  t
  -> Agent_store.Delegation_store.record
  -> (unit, Agent_protocol.Error.t) result

(** Revalidate the same captured inputs through the existing shared walker after
    an intervening wait, outside any Actor checkpoint. A changed digest, record
    or physical association is rejected without obtaining a replacement. *)
val recheck
  :  t
  -> walk:Independent_resources.host
  -> (unit, Agent_protocol.Error.t) result

(** For an existing pure Actor publication gate supplying the actual current
    State of this exact saved Owner. No reentrant Actor or Store call. *)
val fingerprint_state
  :  t
  -> owner:Runtime_owner.t
  -> Agent_session.Session_state.t
  -> (string, Agent_protocol.Error.t) result
