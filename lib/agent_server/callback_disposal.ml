open! Core
module C = Agent_session.Callback_cleanup

(* Callback_retirement.release records Completion release only AFTER clearing
   the exact borrow/custody/admission and installing delegated acknowledgement.
   Completed(Error) is therefore a completed releasing-handler diagnostic after
   that actual bookkeeping boundary, not inferred release from a generic result.
   This proves only producer/bookkeeping settlement. The caller must additionally
   prove its original End/terminal and separately retained factual/physical duties. *)
let completion_settled snapshot =
  (match C.Completion_snapshot.producer_exit snapshot with
   | Not_returned -> false
   | Returned _ -> true)
  && (match C.Completion_snapshot.finish_call snapshot with
      | Not_attempted | Awaiting_reply -> false
      | Completed _ -> true)
  &&
  match C.Completion_snapshot.release snapshot with
  | Not_released | Finishing -> false
  | Completed _ -> true
;;

let parent_settled snapshot =
  C.Cleanup_snapshot.registration_closed snapshot
  && List.for_all (C.Cleanup_snapshot.claims snapshot) ~f:(fun claim ->
    completion_settled (C.Completion_observation.snapshot claim))
;;

let parent_diagnostics_sealed snapshot =
  C.Cleanup_snapshot.registration_closed snapshot
  && List.for_all (C.Cleanup_snapshot.claims snapshot) ~f:(fun claim ->
    C.Completion_snapshot.original_finish_settled
      (C.Completion_observation.snapshot claim))
;;
