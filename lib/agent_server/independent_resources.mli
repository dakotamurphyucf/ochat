type ancestor =
  { state : Agent_session.Session_state.t
  ; owner : Runtime_owner.t
  }

type host =
  { find : Agent_protocol.Id.Session.t -> (ancestor, Agent_protocol.Error.t) result
  ; resolve :
      Agent_store.Delegation_store.Reference.t
      -> (Agent_store.Delegation_store.record, Agent_protocol.Error.t) result
  ; authorize :
      Agent_store.Delegation_store.record -> (unit, Agent_protocol.Error.t) result
  ; build_root :
      sw:Eio.Switch.t
      -> register_tracking:(Graph_tracking.t -> unit)
      -> ancestor
      -> (Agent_session.Runtime_builder.resources, Agent_protocol.Error.t) result
  ; build_generated :
      sw:Eio.Switch.t
      -> parent:Agent_session.Runtime_builder.resources
      -> ancestor
      -> (Agent_session.Runtime_builder.resources, Agent_protocol.Error.t) result
  }

(** Qualified private ancestry, without native resource setup or execution.
    Stateful parent moderation requires a separate original-owner service and is
    currently rejected. Current host policy must validate Independent grants. *)
val check_parent
  :  max_depth:int
  -> host:host
  -> parent_id:Agent_protocol.Id.Session.t
  -> (unit, Agent_protocol.Error.t) result

module Source_fingerprint : sig
  (** A trusted, request-local operation on the actual ancestor found by the
      walk. It checks the prepared source association and computes the
      authority fingerprint from this ancestor's State. The operation is pure
      and may be called again for the same ancestor at an edge check. *)
  type t = ancestor -> (string, Agent_protocol.Error.t) result
end

(** Runs the same ancestry walk with explicit source-aware fingerprinting.
    The walk checks the same record linkage, authorization, edge digest and
    inherited profile. It issues no grant and constructs no resources.
    [check_parent] and the [with_chain] family still use strict fingerprinting. *)
val check_parent_with_source
  :  max_depth:int
  -> host:host
  -> source_fingerprint:Source_fingerprint.t
  -> parent_id:Agent_protocol.Id.Session.t
  -> (unit, Agent_protocol.Error.t) result

type t

val find
  :  t
  -> Agent_protocol.Id.Session.t
  -> (Agent_session.Runtime_builder.resources, Agent_protocol.Error.t) result

(** Retain all ancestor owners without loading execution runtimes; construct
    root-to-leaf resources under a dedicated switch, recheck current private
    ancestry and keep everything alive through [f]. Permanent owner closure or
    caller cancellation joins the whole resource scope. Every constructed graph
    registers tracking before constructors; admission is sealed before this
    switch joins and finish runs afterwards while its actor is live. Cleanup
    visits every graph and preserves a primary typed failure or exception. This only accepts an
    explicitly authorized Linked Independent admission. Child execution must
    still install Delegation_authority for each effect/disclosure boundary. *)
val with_chain
  :  max_depth:int
  -> host:host
  -> reference:Agent_store.Delegation_store.Reference.t
  -> f:(t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Same implementation and primary projection, with actual bracket custody
    supplied before ancestry/preparation yields. The fresh holder records only
    graph/Switch cleanup, never successful outer Owner finalizers. Even rejected
    ancestry closes the holder as Never_entered without allocating a Switch. *)
val with_chain_reported
  :  cleanup:Independent_cleanup.t
  -> max_depth:int
  -> host:host
  -> reference:Agent_store.Delegation_store.Reference.t
  -> f:(t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Public additions inside Independent_resources. No capture/enrollment/seal
    mutators or recipe constructors are exposed by this facade. *)
module Cleanup : sig
  type t

  module Own_post_callback_check : sig
    (** Read-only evidence of an actual original own post-callback check, not
        callback/Local/factual cleanup. Only one cancellation in its selected
        group can qualify. No public receipt issuer or delivery mutation. *)
    type t

    val diagnostic : t -> Agent_session.Cleanup_diagnostic.t

    val eligible
      :  t
      -> selected:Agent_session.Cleanup_diagnostic.t
      -> excluded:Agent_session.Cleanup_diagnostic.t list
      -> bool
  end

  module Snapshot : sig
    type t

    val cleanup
      :  t
      -> ( (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
           , Agent_protocol.Error.t )
           result

    (** Actual structural override priority, independent of original encounter
        order. Each observation retains its full incoming diagnostic groups. *)
    val fixed_controls
      :  t
      -> ( (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t list
           , Agent_protocol.Error.t )
           result

    (** SAME structural priority and actual groups as fixed_controls, omitting
        only producer-tagged preparation controls. Actual cleanup and unknown
        escapes remain; this is not a joined-custody certificate. *)
    val cleanup_controls
      :  t
      -> ( (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t list
           , Agent_protocol.Error.t )
           result

    val original_view : t -> Cleanup_original_view.t
    val custody_joined : t -> bool

    (** Original immutable check evidence; this getter has no delivery effects. *)
    val own_post_callback_checks : t -> Own_post_callback_check.t list
  end

  module Retry : sig
    type t =
      | Not_started of Agent_protocol.Error.t
      | Completed of Snapshot.t
      | Stopped of
          { refusal : Agent_protocol.Error.t
          ; current : Snapshot.t
          }
  end

  (** Fresh original custody only; no graph or lease is fabricated by create. *)
  val create : unit -> t

  val graph : t -> Independent_cleanup.t
  val snapshot : t -> (Snapshot.t option, Agent_protocol.Error.t) result

  (** For an actual early descriptor whose Driver original terminal proves
      Adoption.Not_created before the chain began. Fresh seals the actual
      unentered graph without allocating a Switch or lease; actual invariant
      failures remain in the sealed snapshot. Complete returns the SAME value.
      Running/retrying and foreign-domain use refuse before effects. This is
      never a way to infer Not_created from missing Local evidence. *)
  val finish_without_entry : t -> (Snapshot.t, Agent_protocol.Error.t) result

  val retry_observed : t -> Retry.t
end

(** Actual unit callback observation passes through the SAME ancestry validation,
    Owner admission, graph and unwind sequence as raw with_chain. The actual
    capture is retained before admission yields; if original context entry
    finishes before capture ran, associate its SAME returned cleanup holder
    nonyieldingly after return. All original exits seal the holder.
    Retain this exact holder with the Driver before invoking preparation.
    When supplied, [consumer] identifies the actual child Source or fixed Resource whose
    construction holds each ancestor lifetime lease. The caller checks its
    affinity to the selected child Owner before invoking this operation; every
    ancestor receives this SAME consumer rather than a fabricated native loan.
    Omission retains the lexical behavior for standalone ancestry brackets. *)
val with_chain_observed
  :  ?consumer:Runtime_owner.Native_consumer.t
  -> Cleanup.t
  -> max_depth:int
  -> host:host
  -> reference:Agent_store.Delegation_store.Reference.t
  -> f:(t -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
