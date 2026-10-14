module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source

module Proof : sig
  type t =
    | Project of Chatml_authoring_bundle_project.Validated.t
    | Chatmd_document of Chatml_authoring_bundle_document.Validated.t
end

module Graph : sig
  type t

  val selector : t -> Bundle.Manifest.Script.Selector.t option
  val lock_source : t -> Captured.t
  val sources : t -> Captured.t list
end

type t

(** Derive the only receipt basis and graph inventories accepted by the durable
    publisher from genuine opaque validator results. Require the exact manifest
    basis/fingerprint, roots, scripts and selected Project/Source_only choices.
    Every selected graph is kept separately even when qualified IDs overlap.
    This is pure candidate preparation, not upload custody or authority. *)
val of_proof
  :  Bundle.Manifest.t
  -> Proof.t
  -> checkpoint:(unit -> unit)
  -> (t, unit) result

val basis : t -> Bundle.Receipt.basis
val graphs : t -> Graph.t list

module Installed : sig
  type t

  val intent : t -> Authoring_upload_document.Intent.t
  val intent_bytes : t -> string
  val receipt : t -> Bundle.Receipt.t
  val supplemental : t -> (int * Captured.t) list
end

(** Assign exact uploaded-member ordinal references and distinct graph-local
    supplementary ordinals. Comparisons poll over bytes; a shared qualified ID
    with different revisions is never collapsed. Every graph/source/byte and
    encoded intent cap is checked before any durable mutation. *)
val install_plan
  :  t
  -> manifest:Bundle.Manifest.t
  -> uploaded:Captured.t list
  -> owner:Bundle.Owner.t
  -> upload_id:Agent_protocol.Id.Authoring_upload.t
  -> commit_key:Agent_protocol.Idempotency_key.t
  -> payload_digest:Captured.Digest.t
  -> bundle_id:Agent_protocol.Id.Authoring_bundle.t
  -> expires_at:Agent_protocol.Timestamp.t
  -> max_graphs:int
  -> max_sources:int
  -> max_supplemental_bytes:int
  -> max_intent_bytes:int
  -> checkpoint:(unit -> unit)
  -> (Installed.t, unit) result
