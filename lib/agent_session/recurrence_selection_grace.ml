open! Core
module P = Agent_protocol

type t =
  { occurrence : Recurrence_identity.Occurrence.t
  ; elapsed_ns : int64
  ; lateness_ns : int64
  }

let occurrence t = t.occurrence

let create ~occurrence ~snapshot ~lateness_ns =
  if Int64.(lateness_ns < 0L)
  then Error (P.Error.invalid_request "negative selected recurrence lateness")
  else
    Ok
      { occurrence
      ; elapsed_ns = Recurrence_phase.Snapshot.elapsed_ns snapshot
      ; lateness_ns
      }
;;

let lateness_ns t ~snapshot =
  let elapsed = Recurrence_phase.Snapshot.elapsed_ns snapshot in
  if Int64.(elapsed < t.elapsed_ns)
  then Error (P.Error.invalid_request "selected recurrence elapsed clock moved backward")
  else (
    let delta = Int64.(elapsed - t.elapsed_ns) in
    if Int64.(delta > max_value - t.lateness_ns)
    then Error (P.Error.invalid_request "selected recurrence lateness overflow")
    else Ok Int64.(t.lateness_ns + delta))
;;
