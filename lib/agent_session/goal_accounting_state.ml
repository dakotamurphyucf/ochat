open! Core
module P = Agent_protocol
module G = P.Goal
module L = Inference_ledger
module Q = P.Inference_query
module O = Inference.Observation
module Usage = Goal_usage
module Counter = Goal_accounting_counter

module Error = struct
  type t =
    | Ledger of L.Error.t
    | Scope_mismatch
    | Conflicting_binding
    | Missing_bound_row
    | Invalid_retirement
    | Invalid_admission
    | Stale_plan
    | Counter_exhausted
    | Binding_capacity
    | Invalid_field of
        { field : string
        ; reason : string
        }
  [@@deriving equal, sexp_of]
end

let ledger result = Result.map_error result ~f:(fun error -> Error.Ledger error)

let counter result =
  Result.map_error result ~f:(fun (_ : Counter.error) -> Error.Counter_exhausted)
;;

let add left right =
  let open Result.Let_syntax in
  let%bind left = counter (Counter.of_int64 left) in
  let%bind right = counter (Counter.of_int64 right) in
  counter (Counter.add left right) |> Result.map ~f:Counter.to_int64
;;

let increment value = add value 1L
let sum values = List.fold_result values ~init:0L ~f:add

module Scope = struct
  type t =
    { goal_id : P.Id.Goal.t
    ; session : P.Session_ref.t
    }
  [@@deriving equal, sexp_of]

  let goal_id t = t.goal_id
  let session t = t.session
end

module Binding = struct
  type t =
    { handle : L.Handle.t
    ; run_id : P.Id.Run.t
    ; source : P.Run_source.t
    }

  let equal a b =
    L.Handle.equal a.handle b.handle
    && P.Id.Run.equal a.run_id b.run_id
    && P.Run_source.equal a.source b.source
  ;;

  let handle t = t.handle
  let run_id t = t.run_id
  let source t = t.source
end

type t =
  { scope : Scope.t
  ; revision : int64
  ; admitted_turns : int64
  ; admitted_attempts : int64
  ; untracked_turns : int64
  ; settled : Usage.t
  ; bindings : Binding.t Int64.Map.t
  ; max_bindings : int
  ; last_attempt_ordinal : int64
  ; last_turn_ordinal : int64
  ; last_ledger_revision : int64
  }

let scope t = t.scope
let revision t = t.revision
let admitted_turns t = t.admitted_turns
let admitted_attempts t = t.admitted_attempts
let untracked_turns t = t.untracked_turns
let untracked_attempts t = Usage.untracked_attempts t.settled

let settled_tracked_attempts t =
  Int64.(Usage.attempts t.settled - Usage.untracked_attempts t.settled)
;;

let bindings t = Map.data t.bindings

let equal a b =
  Scope.equal a.scope b.scope
  && Int64.equal a.revision b.revision
  && Int64.equal a.admitted_turns b.admitted_turns
  && Int64.equal a.admitted_attempts b.admitted_attempts
  && Int64.equal a.untracked_turns b.untracked_turns
  && Usage.equal a.settled b.settled
  && Map.equal Binding.equal a.bindings b.bindings
  && Int.equal a.max_bindings b.max_bindings
  && Int64.equal a.last_attempt_ordinal b.last_attempt_ordinal
  && Int64.equal a.last_turn_ordinal b.last_turn_ordinal
  && Int64.equal a.last_ledger_revision b.last_ledger_revision
;;

let fold_error = function
  | Canonical_usage_fold.Error.Scope_mismatch -> Error.Scope_mismatch
  | Conflicting_binding -> Error.Conflicting_binding
  | Missing_bound_row -> Error.Missing_bound_row
  | Invalid_retirement -> Error.Invalid_retirement
  | Counter_exhausted -> Error.Counter_exhausted
;;

type marks = Canonical_usage_fold.Marks.t =
  { attempt : int64
  ; turn : int64
  ; untracked_attempts : int64
  ; untracked_turns : int64
  }

let marks ledger =
  Canonical_usage_fold.Marks.of_ledger ledger |> Result.map_error ~f:fold_error
;;

let check_session t ledger =
  if P.Id.Session.equal (P.Session_ref.session_id t.scope.session) (L.session_id ledger)
  then Ok ()
  else Error Error.Scope_mismatch
;;

let ended record =
  match O.Attempt_record.state record with
  | Prepared | Running -> false
  | Terminal _ | Interrupted _ -> true
;;
