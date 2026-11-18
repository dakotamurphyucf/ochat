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

  let add_untracked_count t count =
    Result.map (add_count t.untracked_attempts count) ~f:(fun untracked_attempts ->
      { t with untracked_attempts })
  ;;

  let add_disjoint t other =
    let open Result.Let_syntax in
    let%bind actual_attempts = add_count t.actual_attempts other.actual_attempts in
    let%bind estimated_attempts =
      add_count t.estimated_attempts other.estimated_attempts
    in
    let%bind untracked_attempts =
      add_count t.untracked_attempts other.untracked_attempts
    in
    let%bind not_reported = add_count t.unknown.not_reported other.unknown.not_reported in
    let%bind explicit_null =
      add_count t.unknown.explicit_null other.unknown.explicit_null
    in
    let%bind interrupted = add_count t.unknown.interrupted other.unknown.interrupted in
    let%bind not_submitted =
      add_count t.unknown.not_submitted other.unknown.not_submitted
    in
    let%map before_tracking =
      add_count t.unknown.before_tracking other.unknown.before_tracking
    in
    let add_sum (left : Sum.sum) (right : Sum.sum) =
      match left, right with
      | Sum.Tokens _, Tokens value -> add_tokens left value
      | Overflow, (Tokens _ | Overflow) | Tokens _, Overflow -> Sum.Overflow
    in
    { actual = add_sum t.actual other.actual
    ; actual_attempts
    ; estimated = add_sum t.estimated other.estimated
    ; estimated_attempts
    ; first_estimator = Option.first_some t.first_estimator other.first_estimator
    ; mixed_estimators =
        (t.mixed_estimators
         || other.mixed_estimators
         ||
         match t.first_estimator, other.first_estimator with
         | Some left, Some right -> not (equal_estimator left right)
         | None, (None | Some _) | Some _, None -> false)
    ; unknown =
        { not_reported; explicit_null; interrupted; not_submitted; before_tracking }
    ; untracked_attempts
    }
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

let add_untracked_count t ~count =
  let open Result.Let_syntax in
  let%bind _ = Counter.of_int64 count in
  let%bind attempts = add_count t.attempts count in
  let%bind untracked_attempts = add_count t.untracked_attempts count in
  let%map next =
    map_metrics t ~f:(fun metric _ -> Metric.add_untracked_count metric count)
  in
  { next with attempts; untracked_attempts }
;;

let add_untracked t = add_untracked_count t ~count:1L

let add_disjoint t other =
  let open Result.Let_syntax in
  let%bind attempts = add_count t.attempts other.attempts in
  let%bind untracked_attempts = add_count t.untracked_attempts other.untracked_attempts in
  let%map next =
    map_metrics t ~f:(fun metric kind ->
      Metric.add_disjoint metric (component other kind))
  in
  { next with attempts; untracked_attempts }
;;

module Persistence = struct
  module P = Agent_protocol
  module X = Persistence_codec
  module D = Document_schema

  let invalid message = Error (P.Error.invalid_request message)

  let sum_json = function
    | Sum.Tokens value ->
      `Object [ "kind", `String "tokens"; "amount", X.int64_json value ]
    | Overflow -> `Object [ "kind", `String "overflow"; "amount", `Null ]
  ;;

  let sum_of_json json =
    let open Result.Let_syntax in
    let%bind fields = X.object_ json in
    let%bind kind = X.required fields "kind" P.Json_codec.string in
    let%bind amount = X.required fields "amount" (X.nullable X.nonnegative_int64) in
    match kind, amount with
    | "tokens", Some value -> Ok (Sum.Tokens value)
    | "overflow", None -> Ok Sum.Overflow
    | _ -> invalid "invalid aggregate token sum"
  ;;

  let estimator_json = function
    | None -> `Null
    | Some (method_, version) ->
      `Object
        [ ( "method"
          , `String
              (match method_ with
               | O.Estimator.O200k_serialized_history -> "o200k_serialized_history"
               | Utf8_heuristic -> "utf8_heuristic") )
        ; "version", X.integer_json version
        ]
  ;;

  let estimator_of_json =
    X.nullable (fun json ->
      let open Result.Let_syntax in
      let%bind fields = X.object_ json in
      let%bind method_ = X.required fields "method" P.Json_codec.string in
      let%bind method_ =
        match method_ with
        | "o200k_serialized_history" -> Ok O.Estimator.O200k_serialized_history
        | "utf8_heuristic" -> Ok O.Estimator.Utf8_heuristic
        | _ -> invalid "unknown aggregate estimator"
      in
      let%bind version = X.required fields "version" X.integer in
      if version > 0
      then Ok (method_, version)
      else invalid "nonpositive aggregate estimator version")
  ;;

  let unknown_json (t : Unknown.t) =
    `Object
      [ "not_reported", X.int64_json t.not_reported
      ; "explicit_null", X.int64_json t.explicit_null
      ; "interrupted", X.int64_json t.interrupted
      ; "not_submitted", X.int64_json t.not_submitted
      ; "before_tracking", X.int64_json t.before_tracking
      ]
  ;;

  let metric_json (t : Metric.t) =
    `Object
      [ "actual", sum_json t.actual
      ; "actual_attempts", X.int64_json t.actual_attempts
      ; "estimated", sum_json t.estimated
      ; "estimated_attempts", X.int64_json t.estimated_attempts
      ; "first_estimator", estimator_json t.first_estimator
      ; "mixed_estimators", X.bool_json t.mixed_estimators
      ; "unknown", unknown_json t.unknown
      ; "untracked_attempts", X.int64_json t.untracked_attempts
      ]
  ;;

  let metrics =
    [ "input", Component.Input
    ; "output", Output
    ; "reported_total", Reported_total
    ; "cached_input", Cached_input
    ; "cache_write_input", Cache_write_input
    ; "reasoning_output", Reasoning_output
    ]
  ;;

  let to_json t =
    `Object
      ([ "attempts", X.int64_json t.attempts
       ; "untracked_attempts", X.int64_json t.untracked_attempts
       ]
       @ List.map metrics ~f:(fun (name, component_) ->
         name, metric_json (component t component_)))
  ;;

  let zero_sum = function
    | Sum.Tokens value -> Int64.equal value 0L
    | Overflow -> false
  ;;

  let metric_of_json ~attempts ~untracked json =
    let open Result.Let_syntax in
    let%bind fields = X.object_ json in
    let%bind actual = X.required fields "actual" sum_of_json in
    let%bind actual_attempts = X.required fields "actual_attempts" X.nonnegative_int64 in
    let%bind estimated = X.required fields "estimated" sum_of_json in
    let%bind estimated_attempts =
      X.required fields "estimated_attempts" X.nonnegative_int64
    in
    let%bind first_estimator = X.required fields "first_estimator" estimator_of_json in
    let%bind mixed_estimators = X.required fields "mixed_estimators" P.Json_codec.bool in
    let%bind untracked_attempts =
      X.required fields "untracked_attempts" X.nonnegative_int64
    in
    let%bind u = X.required fields "unknown" X.object_ in
    let%bind not_reported = X.required u "not_reported" X.nonnegative_int64 in
    let%bind explicit_null = X.required u "explicit_null" X.nonnegative_int64 in
    let%bind interrupted = X.required u "interrupted" X.nonnegative_int64 in
    let%bind not_submitted = X.required u "not_submitted" X.nonnegative_int64 in
    let%bind before_tracking = X.required u "before_tracking" X.nonnegative_int64 in
    let%bind count =
      List.fold_result
        [ actual_attempts
        ; estimated_attempts
        ; not_reported
        ; explicit_null
        ; interrupted
        ; not_submitted
        ; before_tracking
        ; untracked_attempts
        ]
        ~init:0L
        ~f:(fun a b ->
          Result.map_error (add_count a b) ~f:(fun _ ->
            P.Error.invalid_request "aggregate count overflow"))
    in
    if
      (not (Int64.equal count attempts && Int64.equal untracked_attempts untracked))
      || (Int64.equal actual_attempts 0L && not (zero_sum actual))
      || (Int64.equal estimated_attempts 0L
          && ((not (zero_sum estimated))
              || Option.is_some first_estimator
              || mixed_estimators))
      || (Int64.(estimated_attempts > 0L) && Option.is_none first_estimator)
      || (mixed_estimators && Int64.(estimated_attempts < 2L))
    then invalid "inconsistent aggregate contribution counts"
    else
      Ok
        ({ Metric.actual
         ; actual_attempts
         ; estimated
         ; estimated_attempts
         ; first_estimator
         ; mixed_estimators
         ; unknown =
             { not_reported; explicit_null; interrupted; not_submitted; before_tracking }
         ; untracked_attempts
         }
         : Metric.t)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let limits =
      D.Limits.create ~max_bytes:65536 ~max_depth:16 ~max_fields:4096 ~max_nodes:8192
      |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
      |> Result.ok_or_failwith
    in
    let%bind () =
      D.Json.validate ~limits json
      |> Result.map_error ~f:(fun e ->
        P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
    in
    let%bind fields = X.object_ json in
    let%bind attempts = X.required fields "attempts" X.nonnegative_int64 in
    let%bind untracked_attempts =
      X.required fields "untracked_attempts" X.nonnegative_int64
    in
    let metric name =
      X.required fields name (metric_of_json ~attempts ~untracked:untracked_attempts)
    in
    let%bind input = metric "input" in
    let%bind output = metric "output" in
    let%bind reported_total = metric "reported_total" in
    let%bind cached_input = metric "cached_input" in
    let%bind cache_write_input = metric "cache_write_input" in
    let%map reasoning_output = metric "reasoning_output" in
    { attempts
    ; untracked_attempts
    ; input
    ; output
    ; reported_total
    ; cached_input
    ; cache_write_input
    ; reasoning_output
    }
  ;;

  let value = D.Shape.value
  let sum_shape = X.fields_shape [ "kind"; "amount" ]

  let metric_shape =
    X.shape_exn
      [ "actual", sum_shape
      ; "actual_attempts", value
      ; "estimated", sum_shape
      ; "estimated_attempts", value
      ; "first_estimator", D.Shape.nullable (X.fields_shape [ "method"; "version" ])
      ; "mixed_estimators", value
      ; ( "unknown"
        , X.fields_shape
            [ "not_reported"
            ; "explicit_null"
            ; "interrupted"
            ; "not_submitted"
            ; "before_tracking"
            ] )
      ; "untracked_attempts", value
      ]
  ;;

  let shape =
    X.shape_exn
      ([ "attempts", value; "untracked_attempts", value ]
       @ List.map metrics ~f:(fun (name, _) -> name, metric_shape))
  ;;

  let capacity_projection _t =
    let maximal_metric =
      Metric.
        { actual = Tokens Int64.max_value
        ; actual_attempts = Int64.max_value
        ; estimated = Tokens Int64.max_value
        ; estimated_attempts = Int64.max_value
        ; first_estimator = Some (O.Estimator.O200k_serialized_history, Int.max_value)
        ; mixed_estimators = false
        ; unknown =
            { not_reported = Int64.max_value
            ; explicit_null = Int64.max_value
            ; interrupted = Int64.max_value
            ; not_submitted = Int64.max_value
            ; before_tracking = Int64.max_value
            }
        ; untracked_attempts = Int64.max_value
        }
    in
    `Object
      ([ "attempts", X.int64_json Int64.max_value
       ; "untracked_attempts", X.int64_json Int64.max_value
       ]
       @ List.map metrics ~f:(fun (name, _) -> name, metric_json maximal_metric))
  ;;
end
