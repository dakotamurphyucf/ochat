(** Pure, prepared membership change for one canonical actor state. The caller
    separately validates writer/principal authority and serializes group admission
    with persistence. This module performs no IO and owns no actor/store. *)
type t

(** Check shared metadata CAS, apply the bounded patch and compute only newly
    added references. No-op preserves metadata revision; a change at max revision
    fails. Unrelated execution/name/label/configuration state is preserved. *)
val create
  :  Session_state.t
  -> expected_metadata_revision:int64
  -> patch:Agent_protocol.Session_organization.Patch.t
  -> (t, Agent_protocol.Error.t) result

val changed : t -> bool
val values : t -> Agent_protocol.Session_organization.Values.t
val additions : t -> Agent_protocol.Session_organization.Values.t

(** [Batch []] for a no-op; otherwise validated [Organization_changed] with the
    exact next metadata revision. The existing transition commits receipt and
    durable state together; no second receipt writer is introduced. *)
val delta : t -> Session_delta.t
