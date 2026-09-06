open! Core

(** Applies server projections to the existing mutable TUI presentation
    model. Durable history is replaced by identity; recoverable provider
    events remain presentation-only. Sourced events own live text; their paired
    history-correlated notifications must not append the same delta twice.
    Committed IDs suppress stale live rows, while tool progress is applied once
    per operation sequence even when a durable revision rebuilds Chat rows.
    Reconciliation considers both revision and durable sequence because one
    commit may contain several visible changes. Durable terminal observations
    close Agent-page calls even when transient finish events were coalesced away;
    starting another operation clears the previous operation's transient calls. *)

type t

val create : unit -> t

val apply
  :  t
  -> model:Model.t
  -> viewport_height:int
  -> Agent_projection.t
  -> (Model.projection_damage, Agent_protocol.Error.t) result
