open! Core
module Store = Agent_store.Developer_run_store

type t =
  { reservation : Job_capacity.reservation
  ; prepared : Store.Prepared.t
  ; handoff : Store.Handoff.t
  }

let create ~reservation ~prepared =
  let handoff =
    Store.Handoff.create ~settle:(function
      | Store.Settlement.Installed _ -> Job_capacity.publish reservation
      | Absent | Prewrite_conflict | Diverged -> ())
  in
  { reservation; prepared; handoff }
;;

let commit t handle ~admit = Store.commit_and_settle handle t.prepared t.handoff ~admit
let reconcile t handle = Store.reconcile_and_settle handle t.prepared t.handoff
let abort_proven_absent t = Job_capacity.abort t.reservation
