(* Intentional nominal bridge to the ONE private DATA implementation. The
   private module stays hidden from installed consumers; no codec or authority
   conversion is needed by trusted in-library producers. *)
(** Pure canonical policy subsets. The inference ledger alone owns provider rows.
    Immutable per-H Goal choices prevent double-counting local/downward coverage.
    No value or decoded witness authenticates an owner, grants quota, rehydrates
    permission, or admits a canonical Handle. All operations are finite, pure,
    same-session DATA validation; runtime causal authorization belongs elsewhere.

    Complete preserved component: 4 MiB/depth80/280000 fields/560000 nodes;
    256 causes,1024 work witnesses,64 lifetime streams,256 tracked H bindings.
    Unknown compatible fields survive. Protected removal rejects, never drops.
    Historical absence/null remains unavailable; no constructor invents old zero. *)
type t = Policy_canonical_usage_carrier.t

val limits : Document_schema.Limits.t
val max_causes : int
val max_memberships : int
val max_streams : int
val max_bindings : int

(** Exact retained callee DATA; not a live caller/source proof. *)
val callee : t -> Agent_protocol.Session_ref.t

module Choice : sig
  type t

  val target : t -> Policy_cause_data.Goal_target.t
  val coverage : t -> Policy_cause_data.Coverage.t
end

module Selection : sig
  type accounting = t
  type t

  val prepare
    :  accounting
    -> cause:Policy_cause_data.Key.t
    -> work:Policy_cause_data.Allocation_work.t
    -> live_prefix:Policy_cause_data.Context.t
    -> coverage_after:Policy_cause_data.Context.t
    -> native_admission:(Agent_protocol.Id.Goal.t * Goal_accounting.Admission.t) option
    -> before_ledger:Inference_ledger.t
    -> after_ledger:Inference_ledger.t
    -> canonical:Goal_descendants.Canonical.t
    -> occurrence:Goal_descendant_occurrence.t
    -> (t, Agent_protocol.Error.t) result

  (** Actual one-H check, not a source-wide classification. Native association
      presence is biconditional with the unique actual native Attempt account
      advance. Tracked requires the actual same-H native binding; Untracked
      requires exact native +1 Unknown settlement/full canonical fold. Missing
      association with an actual native +1 rejects, rather than charging overlay.
      Remaining targets require same-H Included or enter the sparse overlay. *)
  val choices : t -> Choice.t list

  (** Exact immutable selected subject/known coverage. DATA only: neither
      comparison authenticates source work or admits a canonical occurrence. *)
  val matches_subject
    :  t
    -> cause:Policy_cause_data.Key.t
    -> work:Policy_cause_data.Allocation_work.t
    -> occurrence:Goal_descendant_occurrence.t
    -> bool

  val equal : t -> t -> bool
end

module Admission : sig
  type accounting = t
  type t

  val create
    :  cause:Policy_cause_data.Key.t
    -> work:Policy_cause_data.Allocation_work.t
    -> live_prefix:Policy_cause_data.Context.t
    -> descendants_after:Goal_descendants.t option
    -> canonical:Goal_descendants.Canonical.t
    -> occurrence:Goal_descendant_occurrence.t
    -> (t, Agent_protocol.Error.t) result

  (** [accounts] is the actual native plan computed first by
      Canonical_usage_update.prepare with the SAME native admission tuple.
      This prospective claim is checked again against real ordered native leaves;
      it cannot select a covered Boolean, fabricate an account, or grant work. *)
  val select
    :  t
    -> accounting
    -> accounts:Goal_accounts.t option
    -> native_admission:(Agent_protocol.Id.Goal.t * Goal_accounting.Admission.t) option
    -> before:Inference_ledger.t
    -> after:Inference_ledger.t
    -> (Selection.t, Agent_protocol.Error.t) result
end

module Update : sig
  type accounting = t
  type t = Policy_canonical_usage_update.Update.t

  val candidate : t -> accounting

  (** Actual per-H selection DATA only; metadata/factual reasons return None. *)
  val selection : t -> Selection.t option

  val to_json : t -> (Jsonaf.t, Agent_protocol.Error.t) result
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

  (** Closed reason plus complete candidate. A decoded reason is a claim;
      actual ordered birth/canonical/native coverage evidence must reproduce it. *)
  val shape : Document_schema.Shape.t
end

val register_event
  :  t option
  -> before:Policy_cause_data.Context.t
  -> after:Policy_cause_data.Context.t
  -> ledger:Inference_ledger.t
  -> cause:Policy_cause_data.t
  -> (Update.t, Agent_protocol.Error.t) result

(** Registration requires actual missing-to-present original work and exact
    previously caused parent. Existing source IDs cannot acquire a cause later.
    Only a new delegated seed initializes a component, at the actual canonical
    future frontier. This never infers historical unrelatedness. *)
val register_work
  :  t
  -> before:Policy_cause_data.Context.t
  -> after:Policy_cause_data.Context.t
  -> cause:Policy_cause_data.Key.t
  -> member:Policy_cause_data.Member.t
  -> (Update.t, Agent_protocol.Error.t) result

(** Every actual canonical edge, including factual corrections/retirement/reset/
    host turns. [None] preserves old choices and requires no current old work;
    removed ended selected rows fold once. [Some] is one actual new Attempt.
    Complete State must pair this proposal with exact ordered native evidence. *)
val reconcile
  :  t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> selection:Selection.t option
  -> (Update.t, Agent_protocol.Error.t) result

(** Recompute only factual lifecycle reconciliation from the actual canonical
    before/after ledgers. All four canonical admission marks must be unchanged.
    Retained rows may settle/retire and generation may advance under the ledger's
    existing validator. No cause, membership, route, stream or Attempt is added.
    Compatible preserved children survive; protected retirement still rejects. *)
val reconcile_factual
  :  t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

(** Optional pruning only after actual work absence, with no retained H/latest
    head/ancestor dependency. Unknown protected children can reject the proposal.
    Streams and their cumulative settled history are never removed or reused. *)
val prune_work
  :  t
  -> context:Policy_cause_data.Context.t
  -> (Update.t, Agent_protocol.Error.t) result

val validate
  :  t
  -> context:Policy_cause_data.Context.t
  -> ledger:Inference_ledger.t
  -> (unit, Agent_protocol.Error.t) result

val protected_ordinals
  :  t
  -> ledger:Inference_ledger.t
  -> (Core.Int64.Set.t, Agent_protocol.Error.t) result

val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val required_depth : t -> (int, Agent_protocol.Error.t) result
val validate_encoded_capacity : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result
val of_document : Document_schema.Document.t -> (t, Agent_protocol.Error.t) result
val to_document : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result

(** Adoption can only transfer compatible unknowns with identical known value;
    known movement requires an actual closed Update/ordered edge. *)
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result

module Binding : sig
  type t

  val ordinal : t -> int64
  val cause : t -> Policy_cause_data.Key.t
  val choices : t -> Choice.t list
end

val bindings : t -> Binding.t list

module Stream : sig
  type t

  val id : t -> string
  val equal : t -> t -> bool
  val caller : t -> Agent_protocol.Session_ref.t
  val callee : t -> Agent_protocol.Session_ref.t
  val callee_source : t -> Agent_protocol.Run_source.t
  val path : t -> Agent_store.Delegation_store.Reference.t list
  val forwarding_route : t -> Policy_cause_data.Forwarding.Route.t option
  val known_json : t -> Jsonaf.t
  val target : t -> Policy_cause_data.Goal_target.t

  (** Validated descriptor DATA for one exact target already carried by the cause.
      No canonical projection, original admission or initial zero is issued. *)
  val for_target
    :  Policy_cause_data.t
    -> target:Policy_cause_data.Goal_target.t
    -> (t, Agent_protocol.Error.t) result

  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Head : sig
  module Witness : sig
    type t

    val forwarding : t -> Policy_cause_data.Forwarding.Witness.t option
    val cause : t -> Policy_cause_data.Key.t
    val caller_origin : t -> Agent_protocol.Inference_origin.t option
    val child_invocation : t -> Agent_protocol.Id.Invocation.t
    val admission_sha256 : t -> string
  end

  type t

  val witness : t -> Witness.t
  val canonical : t -> Goal_descendants.Canonical.Fact.t
  val cause : t -> Policy_cause_data.Key.t
  val work : t -> Policy_cause_data.Allocation_work.t
end

module Projection : sig
  type t

  val session_revision : t -> int64
  val stream : t -> Stream.t
  val admitted_attempts : t -> int64
  val usage : t -> Goal_usage.t
  val head : t -> Head.t option
  val shape : Document_schema.Shape.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

  (** Known fields only, for native codec composition. Use [to_json] to retain
      unknown fields when storing or forwarding the projection. *)
  val known_json : t -> Jsonaf.t

  (** Complete preserved 256KiB/depth80 projection. Count is the selected subset
      prefix, never global ordinal. Head contains its root-relevant immutable
      witness after live work/row prune, not provider payload or runtime loans. *)
  val to_json : t -> Jsonaf.t
end

val projections
  :  t
  -> ledger:Inference_ledger.t
  -> session_revision:int64
  -> (Projection.t list, Agent_protocol.Error.t) result

module Ordered : sig
  type accounting = t
  type t

  val begin_
    :  previous:accounting option
    -> context:Policy_cause_data.Context.t
    -> ledger:Inference_ledger.t
    -> (t, Agent_protocol.Error.t) result

  (** Closed absence-only reconciliation. Retains no birth window and cannot
      accept policy metadata, a paired canonical Update or positive publication.
      Actual whole-delta lookahead selects this only for ordinary absent paths. *)
  val begin_absent
    :  context:Policy_cause_data.Context.t
    -> ledger:Inference_ledger.t
    -> (t, Agent_protocol.Error.t) result

  type step =
    | Source_work of Policy_cause_data.Context.t
    | Lifecycle_work of
        { context : Policy_cause_data.Context.t
        ; receipt : Agent_protocol.Moderator_execution.t
        ; capture : Moderator_lifecycle.t option
        }
    | Source_changed of
        { current_source : Agent_protocol.Run_source.t option
        ; runs : Run_state.t option
        }
    | Accounts of Goal_accounts.t option
    | Descendants of Goal_descendants.t option
    | Catalog of Goal_catalog.t option
    | Metadata of Update.t
    | Canonical of
        { before : Inference_ledger.t
        ; after : Inference_ledger.t
        ; update : Update.t
        }
    | Ordinary_canonical of
        { before : Inference_ledger.t
        ; after : Inference_ledger.t
        }
    | Replacement of
        { context : Policy_cause_data.Context.t
        ; ledger : Inference_ledger.t
        ; accounting : accounting option
        }

  (** [Lifecycle_work] admits only a new Running lifecycle receipt and its exact
      pointer capture ([Some]) or uncaptured clear ([None]). All other Run state
      and native/source basis remain exact. Births keep the actual full contexts;
      existing receipt updates remain [Source_work]. *)
  val advance : t -> step -> (t, Agent_protocol.Error.t) result

  (** Check each newly selected fact against the actual published transaction
      revision. Old retained heads keep their original immutable revision. *)
  val validate_publication : t -> revision:int64 -> (unit, Agent_protocol.Error.t) result

  (** Consume real ordered projections. Metadata needs a captured bounded actual
      birth. Every changed canonical ledger with positive component needs its
      paired Update. Only causal Attempt reasons open one pending native basis;
      exact actual accounts/GD/catalog must seal it before another canonical,
      metadata, work mutation, replacement or finish. Factual reasons never
      rederive old route choices. Replacement is absent-to-absent or the exact
      factual reconciliation of the previous positive component; it cannot
      initialize/discard causes or introduce canonical admission marks. Existing
      native Created/Reset guards still run. These checks supplement them. *)
  val check
    :  t
    -> incoming:accounting option
    -> context:Policy_cause_data.Context.t
    -> ledger:Inference_ledger.t
    -> (unit, Agent_protocol.Error.t) result
end

(** Export ALL governing targets of the SAME registered ready source work using
    its complete CURRENT carrier child. Missing membership never means empty. *)
val capture_forwarding
  :  t
  -> context:Policy_cause_data.Context.t
  -> work:Policy_cause_data.Allocation_work.t
  -> (Policy_cause_data.Forwarding.t, Agent_protocol.Error.t) result

(** Regenerate and compare the ENTIRE known AND preserved raw export, including
    every prior frame/target/introduction. No old permission or Run is refreshed. *)
val check_forwarding_current
  :  t
  -> context:Policy_cause_data.Context.t
  -> work:Policy_cause_data.Allocation_work.t
  -> Policy_cause_data.Forwarding.t
  -> (unit, Agent_protocol.Error.t) result

(** Exact native ancestry DATA only. No decoded row or absent carrier grants
    execution; actual producer/current original/source remain Actor-owned. *)
module Source : sig
  type accounting = t

  module Native_prefix : sig
    type t = Policy_native_ancestry.t

    (** O1 immutable framing, without whole-inventory validation. *)
    val create
      :  session_id:Agent_protocol.Id.Session.t
      -> generation:int
      -> current_source:Agent_protocol.Run_source.t option
      -> events:Agent_protocol.Moderator_execution.t list
      -> invocations:Agent_protocol.Invocation.t list
      -> jobs:Agent_protocol.Job.t list
      -> runs:Run_state.t option
      -> (t, Agent_protocol.Error.t) result

    val job_member
      :  t
      -> job_id:Agent_protocol.Id.Job.t
      -> (Policy_cause_data.Member.t, Agent_protocol.Error.t) result

    val member_of_job
      :  Agent_protocol.Job.t
      -> (Policy_cause_data.Member.t, Agent_protocol.Error.t) result
  end

  (** Exact current generation and recorded Event/Job execution-source observers
      must match. Optional evidence stays absent. Invocation observation observers
      are deferred sinks, retained exactly and checked at notification claim. A
      full installation epoch is checked for Causes that record it. None means
      unrelated ancestry, not current installation/original/execute authority. *)
  val cause
    :  accounting option
    -> prefix:Native_prefix.t
    -> member:Policy_cause_data.Member.t
    -> (Policy_cause_data.t option, Agent_protocol.Error.t) result

  val invocation_birth
    :  accounting option
    -> prefix:Native_prefix.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> Agent_protocol.Invocation.t
    -> (Update.t option, Agent_protocol.Error.t) result

  val model_job_birth
    :  accounting option
    -> prefix:Native_prefix.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> parent:Policy_cause_data.Member.t option
    -> Agent_protocol.Job.t
    -> (Update.t option, Agent_protocol.Error.t) result

  val async_job_birth
    :  accounting option
    -> prefix:Native_prefix.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> Agent_protocol.Job.t
    -> (Update.t option, Agent_protocol.Error.t) result

  (** A factual queue claim may preserve unavailable historical evidence without
      metadata. It does not authorize a descendant or issue unrelatedness. *)
  val queued_event_birth
    :  accounting option
    -> prefix:Native_prefix.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> Agent_protocol.Moderator_execution.t
    -> (Update.t option, Agent_protocol.Error.t) result

  val job_event_birth
    :  accounting option
    -> prefix:Native_prefix.t
    -> before:(unit -> (Policy_cause_data.Context.t, Agent_protocol.Error.t) result)
    -> Agent_protocol.Moderator_execution.t
    -> (Update.t option, Agent_protocol.Error.t) result
end
