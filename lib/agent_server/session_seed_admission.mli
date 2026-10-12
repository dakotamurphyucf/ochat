open! Core
module P = Agent_protocol
module S = Agent_session

(** Private pure admission and initial-state recipe. Factory retains the actual
    catalog/retained-session reservation; these immutable values grant no source,
    workspace, profile, model or execution authority. *)
type t

(** Current enabled Ready definition and exact requested revision. Factory admits
    principal visibility/create scope and obtains [inference_target] through its
    existing effect-free capture policy before calling. Workspace permission and
    any explicit requested profile must agree with the current definition. *)
val catalog
  :  P.Session_seed.Request.t
  -> definition:S.Prompt_definition.t
  -> revision:S.Prompt_revision.t
  -> workspace:S.Workspace_definition.t
  -> profile:S.Permission_policy.t
  -> inference_target:Inference.Selection.t
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) Result.t

(** Factory has validated the exact Expected lifecycle witness and current
    visibility under the actual retained reservation. Reject generated/delegated
    Specs and unresolved inference selections. Preserve the actual retained
    source revision, root invocation and complete captured target.

    Current definition metadata must exist and be enabled; its source need not
    be Ready. Require the current definition's profile and its actual digest to
    equal the retained pinned profile, and typed runtime-policy equality. Drift
    rejects rather than recaptures or retains revoked privileges. Use the actual
    requested currently permitted configured workspace, never old instance labels.
    This is admission of a new identity, distinct from same-session resume. *)
val retained
  :  P.Session_seed.Request.t
  -> state:S.Session_state.t
  -> definition:S.Prompt_definition.t
  -> workspace:S.Workspace_definition.t
  -> current_profile:S.Permission_policy.t
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) Result.t

val workspace : t -> S.Workspace_definition.t

module Prelayout : sig
  (** Size evidence only, not a provisional native Workspace_instance. Known
      encoded fields use their actual existing codecs. Unknown workspace scalar
      fields are bounded separately before layout; no guessed fixed headroom. *)
  type recipe = t

  type t

  (** Admit a conservative complete first Created transaction bound BEFORE
      Store.create_session_initialized. Include exact escaped known fields,
      transcript, captured target, both state/Session_created projections, audit
      and transaction/document envelopes. [workspace_projection] comes from the
      workspace owner's JSON-only creation projection over its actual planned
      and canonical routes. Each actual scalar's encoded width and the complete
      shape must fit this projection. Repeated quota/conflict-domain fields use
      that same projection in the actual whole transaction encoder. This is not
      a caller-authorized native identity.

      All arithmetic is overflow checked. The complete journal structural/byte
      profile and [max_work_steps] apply before directories or ownership markers
      are created. Original checkpoint exceptions propagate. *)
  val admit
    :  recipe
    -> identity:S.Session_state.Identity.t
    -> workspace_instance_id:P.Id.Workspace_instance.t
    -> workspace_projection:Jsonaf.t
    -> command_audit:Document_schema.Document.t
    -> limits:Document_schema.Limits.t
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> (t, P.Error.t) Result.t

  (** Supply the actual newly resolved workspace. Check definition/instance
      affinity and its encoded envelope, then ordinary complete state validation.
      Install the supplied neutral history/high water and Pending(false), with no
      prior grants, operations, tool receipts, shell/moderator state or attachment.
      The final actual complete Created transaction is admitted by the ordinary
      persistence owner before writer entry; this size recipe cannot bypass it. *)
  val state
    :  t
    -> workspace:S.Workspace_instance.t
    -> checkpoint:(unit -> unit)
    -> (S.Session_state.t, P.Error.t) Result.t
end
