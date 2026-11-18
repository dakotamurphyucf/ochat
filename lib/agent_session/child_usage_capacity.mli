(** Scoped capacity composition for the existing child snapshot carrier. These
    invalid raw projections are capacity witnesses, never snapshots, admission
    evidence or execution authority. The canonical accounting DATA and codecs
    remain owned by [Child_usage_snapshot]. *)

(** Same-kind baseline using the slot's actual frozen origin and observed
    snapshot, retaining compatible unknowns in the enclosing carrier. Missing
    heads use maximum Attempt, which dominates maximum Host_turn structurally.
    Add [extra_growth] once to the complete-carrier baseline reservation.
    Historical unknown slots use [Child_usage_snapshot.capacity_projection]. *)
val projection
  :  reference:Agent_store.Delegation_store.Reference.t
  -> origin:Agent_store.Delegation_goal_origin.t
  -> snapshot:Child_usage_snapshot.t option
  -> Jsonaf.t

(** Additional growth beyond [projection], summed independently per scope.
    Temporary local shape carriers retain actual raw unknowns; protected kind
    replacements are excluded while compatible correction remains readable.
    No whole child component is encoded per scope. [raw_snapshot] must be the
    actual preserved raw snapshot when one exists; [None] is appropriate for
    authored values without retained unknowns. The caller adds this once to its
    complete-carrier baseline growth and checks actual host nesting depth. *)
val extra_growth
  :  origin:Agent_store.Delegation_goal_origin.t
  -> snapshot:Child_usage_snapshot.t option
  -> raw_snapshot:Jsonaf.t option
  -> (Capacity_growth.t, Agent_protocol.Error.t) result

(** Same witnesses and preservation as [extra_growth], measured once together.
    Required depth is relative to the goal_scopes ARRAY (root depth1, scalar
    leaves included) and includes all compatible local occurrence alternatives.
    Intrinsic complete scope-vector depth is checked before returning. *)
type t

val capture
  :  origin:Agent_store.Delegation_goal_origin.t
  -> snapshot:Child_usage_snapshot.t option
  -> raw_snapshot:Jsonaf.t option
  -> (t, Agent_protocol.Error.t) result

val extra : t -> Capacity_growth.t
val required_depth : t -> int
