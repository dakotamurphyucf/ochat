(** Bounded authorized metadata projection, not execution permission. No provider
    rows, raw session document, private custody or command receipt history. *)
module Account : sig
  type t = private
    { revision : int64
    ; admitted_turns : int64
    ; admitted_attempts : int64
    ; untracked_turns : int64
    ; untracked_attempts : int64
    ; usage : Goal_usage.t
    }
end

module Current_run : sig
  type t
end

type row = private
  { goal : Agent_protocol.Goal.t
  ; current_run : Current_run.t option
  ; accounting : Account.t option
  ; governance : Goal_policy_totals.View.t option
  }

type t = private
  { session : Agent_protocol.Session_ref.t
  ; generation : int
  ; catalog_revision : int64
  ; source : Agent_protocol.Run_source.t option
  ; goals : row list
  }

(** Uses canonical Goal codecs and resolves each retained account against the
    actual ledger. Local accounting remains physical-only; governance reports the
    composed local/physical/causal usage, pending counts and labelled coverage.
    Missing historical accounting stays absent, never zero usage.
    A source fence may be absent before actual installation. For an open Goal
    with a nonterminal Workflow on that retained source, [current_run] reports
    the ID and actual Run revision from the reciprocal episode/Admission. It is
    otherwise null. Both fields are advisory data for a later authorized CAS. *)
val capture
  :  Session_state.t
  -> session:Agent_protocol.Session_ref.t
  -> goal_id:Agent_protocol.Id.Goal.t option
  -> (t, Agent_protocol.Error.t) result

val to_json : t -> (Jsonaf.t, Agent_protocol.Error.t) result

(** Same typed projection for external inspection. An absent requested Goal
    yields an empty row list. The caller supplies host authorization, and must
    project any externally exposed Goal and Run data at the caller's scope. *)
val capture_query
  :  Session_state.t
  -> session:Agent_protocol.Session_ref.t
  -> goal_id:Agent_protocol.Id.Goal.t option
  -> (t, Agent_protocol.Error.t) result
