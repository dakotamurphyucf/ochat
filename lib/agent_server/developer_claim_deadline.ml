open! Core
module P = Agent_protocol
module B = Chat_response.Background_request

type disposition =
  | Expired
  | Active of Chatml_execution.limits

type t =
  { deadline : P.Timestamp.t
  ; disposition : disposition
  }

let create ~now ~job ~request =
  let policy = B.policy request in
  let deadline =
    let duration =
      Result.try_with (fun () ->
        Time_ns.Span.of_sec policy.execution.wall_seconds
        |> Time_ns.Span.to_int63_ns
        |> Int63.to_int64)
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "background wall limit is outside the supported range")
    in
    Result.bind duration ~f:(fun duration ->
      let start =
        P.Developer_job.created_at job
        |> P.Timestamp.to_time_ns
        |> Time_ns.to_int63_ns_since_epoch
        |> Int63.to_int64
      in
      let maximum = Int63.to_int64 Int63.max_value in
      if Int64.(duration < 0L || start > maximum - duration)
      then Error (P.Error.invalid_request "background deadline exceeds timestamp range")
      else
        Int64.(start + duration)
        |> Int63.of_int64_exn
        |> Time_ns.of_int63_ns_since_epoch
        |> P.Timestamp.of_time_ns
        |> Result.return)
  in
  Result.map deadline ~f:(fun deadline ->
    let remaining =
      P.Timestamp.diff_ns deadline now
      |> Int64.to_float
      |> fun nanoseconds -> nanoseconds /. 1e9 |> Float.min policy.execution.wall_seconds
    in
    { deadline
    ; disposition =
        (if Float.(remaining <= 0.)
         then Expired
         else Active { policy.execution with wall_seconds = remaining })
    })
;;

let deadline t = t.deadline
let disposition t = t.disposition
