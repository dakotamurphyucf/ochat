open Core

type snapshot =
  { wall : Time_ns.t
  ; elapsed_ns : int64
  }

type t =
  { deadline : Time_ns.t
  ; elapsed_deadline_ns : int64
  }

let snapshot ~wall ~elapsed_ns =
  if Int64.(elapsed_ns < 0L)
  then Error (Timing_error.Invalid "process elapsed time must be nonnegative")
  else Ok { wall; elapsed_ns }
;;

let add_elapsed elapsed amount =
  if Int64.(amount < 0L || amount > max_value - elapsed)
  then Error (Timing_error.Out_of_range "process elapsed deadline")
  else Ok Int64.(elapsed + amount)
;;

let after snapshot ~delay =
  let open Result.Let_syntax in
  let amount = Rule.Period.nanoseconds delay in
  let%bind deadline = Time_arithmetic.add_ns snapshot.wall amount in
  let%map elapsed_deadline_ns = add_elapsed snapshot.elapsed_ns amount in
  { deadline; elapsed_deadline_ns }
;;

let recover snapshot ~deadline =
  let open Result.Let_syntax in
  let remaining =
    Int64.max
      0L
      Int64.(
        Time_arithmetic.nanoseconds deadline - Time_arithmetic.nanoseconds snapshot.wall)
  in
  let%map elapsed_deadline_ns = add_elapsed snapshot.elapsed_ns remaining in
  { deadline; elapsed_deadline_ns }
;;

let deadline t = t.deadline
let is_due t snapshot = Int64.(snapshot.elapsed_ns >= t.elapsed_deadline_ns)

let due_count t ~period snapshot =
  if not (is_due t snapshot)
  then 0L
  else
    Int64.(
      ((snapshot.elapsed_ns - t.elapsed_deadline_ns) / Rule.Period.nanoseconds period)
      + 1L)
;;

let advance t ~period ~count =
  let open Result.Let_syntax in
  let period = Rule.Period.nanoseconds period in
  if Int64.(count <= 0L || count > max_value / period)
  then Error (Timing_error.Out_of_range "fixed-rate phase advance")
  else (
    let amount = Int64.(period * count) in
    let%bind deadline = Time_arithmetic.add_ns t.deadline amount in
    let%map elapsed_deadline_ns = add_elapsed t.elapsed_deadline_ns amount in
    { deadline; elapsed_deadline_ns })
;;
