open! Core
module O = Inference.Observation
module Counter = Goal_accounting_counter
module Component = O.Usage.Component
module Sum = Agent_protocol.Inference_query.Metric

let add_count left right =
  let open Result.Let_syntax in
  let%bind left = Counter.of_int64 left in
  let%bind right = Counter.of_int64 right in
  Result.map (Counter.add left right) ~f:Counter.to_int64
;;

let increment value = add_count value 1L

module Unknown = struct
  type t =
    { not_reported : int64
    ; explicit_null : int64
    ; interrupted : int64
    ; not_submitted : int64
    ; before_tracking : int64
    }
  [@@deriving equal, sexp_of]

  let empty =
    { not_reported = 0L
    ; explicit_null = 0L
    ; interrupted = 0L
    ; not_submitted = 0L
    ; before_tracking = 0L
    }
  ;;

  let count t = function
    | O.Count.Not_reported -> t.not_reported
    | Explicit_null -> t.explicit_null
    | Interrupted -> t.interrupted
    | Not_submitted -> t.not_submitted
    | Before_tracking -> t.before_tracking
  ;;

  let add t reason =
    Result.map
      (increment (count t reason))
      ~f:(fun value ->
        match reason with
        | O.Count.Not_reported -> { t with not_reported = value }
        | Explicit_null -> { t with explicit_null = value }
        | Interrupted -> { t with interrupted = value }
        | Not_submitted -> { t with not_submitted = value }
        | Before_tracking -> { t with before_tracking = value })
  ;;
end

module Metric = struct
  type estimator = O.Estimator.method_ * int [@@deriving equal, sexp_of]

  type t =
    { actual : Sum.sum
    ; actual_attempts : int64
    ; estimated : Sum.sum
    ; estimated_attempts : int64
    ; first_estimator : estimator option
    ; mixed_estimators : bool
    ; unknown : Unknown.t
    ; untracked_attempts : int64
    }
  [@@deriving equal, sexp_of]

  let empty =
    { actual = Tokens 0L
    ; actual_attempts = 0L
    ; estimated = Tokens 0L
    ; estimated_attempts = 0L
    ; first_estimator = None
    ; mixed_estimators = false
    ; unknown = Unknown.empty
    ; untracked_attempts = 0L
    }
  ;;

  let actual t = t.actual
  let actual_attempts t = t.actual_attempts
  let estimated t = t.estimated
  let estimated_attempts t = t.estimated_attempts
  let mixed_estimators t = t.mixed_estimators
  let first_estimator t = t.first_estimator
  let unknown t = t.unknown
  let untracked_attempts t = t.untracked_attempts

  let add_tokens sum tokens =
    match sum with
    | Sum.Overflow -> Sum.Overflow
    | Tokens current ->
      if Int64.(tokens > max_value - current)
      then Sum.Overflow
      else Tokens Int64.(current + tokens)
  ;;

  let add t = function
    | O.Count.Actual tokens ->
      Result.map (increment t.actual_attempts) ~f:(fun actual_attempts ->
        { t with actual = add_tokens t.actual tokens; actual_attempts })
    | Estimated { tokens; estimator } ->
      let current = O.Estimator.method_ estimator, O.Estimator.version estimator in
      Result.map (increment t.estimated_attempts) ~f:(fun estimated_attempts ->
        { t with
          estimated = add_tokens t.estimated tokens
        ; estimated_attempts
        ; first_estimator = Some (Option.value t.first_estimator ~default:current)
        ; mixed_estimators =
            t.mixed_estimators
            || Option.exists t.first_estimator ~f:(fun first ->
              not (equal_estimator first current))
        })
    | Unknown reason ->
      Result.map (Unknown.add t.unknown reason) ~f:(fun unknown -> { t with unknown })
  ;;

  let add_untracked t =
    Result.map (increment t.untracked_attempts) ~f:(fun untracked_attempts ->
      { t with untracked_attempts })
  ;;
end

type t =
  { attempts : int64
  ; untracked_attempts : int64
  ; input : Metric.t
  ; output : Metric.t
  ; reported_total : Metric.t
  ; cached_input : Metric.t
  ; cache_write_input : Metric.t
  ; reasoning_output : Metric.t
  }
[@@deriving equal, sexp_of]

let empty =
  { attempts = 0L
  ; untracked_attempts = 0L
  ; input = Metric.empty
  ; output = Metric.empty
  ; reported_total = Metric.empty
  ; cached_input = Metric.empty
  ; cache_write_input = Metric.empty
  ; reasoning_output = Metric.empty
  }
;;

let attempts t = t.attempts
let untracked_attempts t = t.untracked_attempts

let component t = function
  | Component.Input -> t.input
  | Output -> t.output
  | Reported_total -> t.reported_total
  | Cached_input -> t.cached_input
  | Cache_write_input -> t.cache_write_input
  | Reasoning_output -> t.reasoning_output
;;

let map_metrics t ~f =
  let open Result.Let_syntax in
  let%bind input = f t.input Component.Input in
  let%bind output = f t.output Output in
  let%bind reported_total = f t.reported_total Reported_total in
  let%bind cached_input = f t.cached_input Cached_input in
  let%bind cache_write_input = f t.cache_write_input Cache_write_input in
  let%map reasoning_output = f t.reasoning_output Reasoning_output in
  { t with
    input
  ; output
  ; reported_total
  ; cached_input
  ; cache_write_input
  ; reasoning_output
  }
;;

let add_record t record =
  let open Result.Let_syntax in
  let%bind attempts = increment t.attempts in
  let%map next =
    map_metrics t ~f:(fun metric component ->
      Metric.add metric (Inference_usage_count.count record component))
  in
  { next with attempts }
;;

let add_untracked t =
  let open Result.Let_syntax in
  let%bind attempts = increment t.attempts in
  let%bind untracked_attempts = increment t.untracked_attempts in
  let%map next = map_metrics t ~f:(fun metric _ -> Metric.add_untracked metric) in
  { next with attempts; untracked_attempts }
;;
