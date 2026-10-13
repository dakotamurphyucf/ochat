open Core
module Captured = Chatml_source
module Semantics = Chatml_semantics
module Wire = Agent_protocol.Authoring_analysis

module Evidence : sig
  type t =
    | Complete of Semantics.Snapshot.t
    | Provisional of
        { provisional : Semantics.Provisional.t
        ; full_source : Captured.t
        }
end

type failure =
  | Evidence_mismatch
  | Stale_source
  | Source_unavailable

(** Comparison-only admission; host owns actor, context, selected graph and
    Snapshot_id authority. The returned source is from the actual immutable
    semantic inventory, never manufactured from a wire Source label. *)
val semantic
  :  Evidence.t
  -> Wire.Query.Request.t
  -> budget:Authoring_query_budget.t
  -> (Semantics.Snapshot.t * Captured.t, failure) Result.t

val syntax
  :  Semantics.Partial.t
  -> Wire.Query.Request.t
  -> budget:Authoring_query_budget.t
  -> (Captured.t, failure) Result.t
