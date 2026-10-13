module Prepared = Chatml_prepared_program
module Driver = Chatml_checked_graph
module Snapshot = Chatml_semantic_snapshot
module Identity = Chatml_semantic_identity
module Limits = Chatml_semantic_limits
module Budget = Chatml_semantic_budget

(** One all-group freeze after successful actual graph/interface/importer/host
    settlement, then bounded immutable projection. Raw ID maps and borrowed
    schemes exist only during this function; no cells/callback enter Snapshot.
    Projection preserves principal symbols AND selected lexical/export Binding
    views. Expected rejection uses Budget's private escape under Budget.run;
    unexpected exceptions/checkpoints remain original. *)
val publish
  :  Prepared.t
  -> Driver.t
  -> identity:Identity.t
  -> limits:Limits.t
  -> budget:Budget.t
  -> checkpoint:(unit -> unit)
  -> Snapshot.t

(** Same frozen publication over an opaque authoring-only checked result. This
    never produces a linked artifact or host requirement. *)
val publish_provisional
  :  Prepared.t
  -> Driver.Provisional.t
  -> identity:Identity.t
  -> limits:Limits.t
  -> budget:Budget.t
  -> checkpoint:(unit -> unit)
  -> Snapshot.t
