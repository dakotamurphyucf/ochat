(** Private actual prefix/canonical/native coverage cursor; no State/Actor dependency. *)
type t = Policy_canonical_usage_carrier.t

module Update = Policy_canonical_usage_update.Update

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
