open! Core
module L = Inference_ledger
module Q = Agent_protocol.Inference_query
module U = Goal_usage
module C = Goal_accounting_counter

module Error = struct
  type t =
    | Scope_mismatch
    | Missing_bound_row
    | Conflicting_binding
    | Invalid_retirement
    | Counter_exhausted
  [@@deriving equal, sexp_of]
end

let counter result =
  Result.map_error result ~f:(fun (_ : C.error) -> Error.Counter_exhausted)
;;

let sum values =
  List.fold_result values ~init:0L ~f:(fun total value ->
    let open Result.Let_syntax in
    let%bind total = counter (C.of_int64 total) in
    let%bind value = counter (C.of_int64 value) in
    Result.map (counter (C.add total value)) ~f:C.to_int64)
;;

module Marks = struct
  type t =
    { attempt : int64
    ; turn : int64
    ; untracked_attempts : int64
    ; untracked_turns : int64
    }
  [@@deriving equal, sexp_of]

  let of_ledger ledger =
    let open Result.Let_syntax in
    let summary = L.summary ledger in
    let coverage = Q.Summary.coverage summary in
    let turns = Q.Summary.turns summary in
    let%bind attempt =
      sum
        [ Q.Summary.retained_attempts summary
        ; coverage.retired_attempts
        ; coverage.untracked_attempts
        ]
    in
    let%map turn =
      sum
        [ turns.pending
        ; turns.completed
        ; turns.failed
        ; turns.cancelled
        ; turns.interrupted
        ; coverage.retired_turns
        ; coverage.untracked_turns
        ]
    in
    { attempt
    ; turn
    ; untracked_attempts = coverage.untracked_attempts
    ; untracked_turns = coverage.untracked_turns
    }
  ;;
end

module Plan = struct
  type t =
    { settled : U.t
    ; retired : L.Handle.t list
    }

  let settled t = t.settled
  let retired t = t.retired
end

let ended record =
  match Inference.Observation.Attempt_record.state record with
  | Prepared | Running -> false
  | Terminal _ | Interrupted _ -> true
;;

let prepare settled ~before ~after ~handles =
  let open Result.Let_syntax in
  let%bind () =
    if Agent_protocol.Id.Session.equal (L.session_id before) (L.session_id after)
    then Ok ()
    else Error Error.Scope_mismatch
  in
  let%map settled, retired, _ =
    List.fold_result
      handles
      ~init:(settled, [], Int64.Set.empty)
      ~f:(fun (settled, retired, seen) handle ->
        let ordinal = L.Handle.ordinal handle in
        let%bind () =
          if
            not
              (Agent_protocol.Id.Session.equal
                 (L.Handle.session_id handle)
                 (L.session_id before))
          then Error Error.Scope_mismatch
          else if Set.mem seen ordinal
          then Error Error.Conflicting_binding
          else Ok ()
        in
        let%bind row =
          match L.find before ~ordinal with
          | None -> Error Error.Missing_bound_row
          | Some row ->
            if L.Handle.equal handle (L.Row.handle row)
            then Ok row
            else Error Error.Conflicting_binding
        in
        let seen = Set.add seen ordinal in
        match L.find after ~ordinal with
        | Some row ->
          if L.Handle.equal handle (L.Row.handle row)
          then Ok (settled, retired, seen)
          else Error Error.Conflicting_binding
        | None ->
          if not (ended (L.Row.record row))
          then Error Error.Invalid_retirement
          else (
            let%map settled = counter (U.add_record settled (L.Row.record row)) in
            settled, handle :: retired, seen))
  in
  { Plan.settled; retired = List.rev retired }
;;
