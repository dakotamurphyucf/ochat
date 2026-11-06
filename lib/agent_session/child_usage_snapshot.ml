open! Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module X = Persistence_codec
module F = Canonical_usage_fold
module U = Goal_usage
module Q = P.Inference_query
module C = Goal_accounting_counter

let invalid message = Error (P.Error.invalid_request message)

let counter result =
  Result.map_error result ~f:(fun (_ : C.error) ->
    P.Error.invalid_request "child snapshot count exhausted")
;;

let add left right =
  let open Result.Let_syntax in
  let%bind left = C.of_int64 left |> counter in
  let%bind right = C.of_int64 right |> counter in
  C.add left right |> counter |> Result.map ~f:C.to_int64
;;

module Partition = struct
  type t =
    { usage : U.t
    ; marks : F.Marks.t
    }

  let usage t = t.usage
  let marks t = t.marks

  let create ~usage ~(marks : F.Marks.t) =
    if
      Int64.(
        marks.attempt < 0L
        || marks.turn < 0L
        || marks.untracked_attempts < 0L
        || marks.untracked_turns < 0L
        || marks.untracked_attempts > marks.attempt
        || marks.untracked_turns > marks.turn)
      || (not (Int64.equal (U.attempts usage) marks.attempt))
      || not (Int64.equal (U.untracked_attempts usage) marks.untracked_attempts)
    then invalid "child partition usage and lifetime counts differ"
    else Ok { usage; marks }
  ;;

  let empty =
    { usage = U.empty
    ; marks = { attempt = 0L; turn = 0L; untracked_attempts = 0L; untracked_turns = 0L }
    }
  ;;

  let add_disjoint t other =
    let open Result.Let_syntax in
    let%bind usage = U.add_disjoint t.usage other.usage |> counter in
    let%bind attempt = add t.marks.attempt other.marks.attempt in
    let%bind turn = add t.marks.turn other.marks.turn in
    let%bind untracked_attempts =
      add t.marks.untracked_attempts other.marks.untracked_attempts
    in
    let%bind untracked_turns = add t.marks.untracked_turns other.marks.untracked_turns in
    create ~usage ~marks:{ attempt; turn; untracked_attempts; untracked_turns }
  ;;

  let to_json t =
    `Object
      [ "usage", U.Persistence.to_json t.usage
      ; "attempts", X.int64_json t.marks.attempt
      ; "turns", X.int64_json t.marks.turn
      ; "untracked_attempts", X.int64_json t.marks.untracked_attempts
      ; "untracked_turns", X.int64_json t.marks.untracked_turns
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind usage = X.required fields "usage" U.Persistence.of_json in
    let n name = X.required fields name X.nonnegative_int64 in
    let%bind attempt = n "attempts" in
    let%bind turn = n "turns" in
    let%bind untracked_attempts = n "untracked_attempts" in
    let%bind untracked_turns = n "untracked_turns" in
    create ~usage ~marks:{ attempt; turn; untracked_attempts; untracked_turns }
  ;;

  let shape =
    X.shape_exn
      [ "usage", U.Persistence.shape
      ; "attempts", Document_schema.Shape.value
      ; "turns", Document_schema.Shape.value
      ; "untracked_attempts", Document_schema.Shape.value
      ; "untracked_turns", Document_schema.Shape.value
      ]
  ;;

  let monotone previous incoming =
    Int64.(
      incoming.marks.attempt >= previous.marks.attempt
      && incoming.marks.turn >= previous.marks.turn
      && incoming.marks.untracked_attempts >= previous.marks.untracked_attempts
      && incoming.marks.untracked_turns >= previous.marks.untracked_turns)
  ;;

  let capacity_projection () =
    `Object
      [ "usage", U.Persistence.capacity_projection U.empty
      ; "attempts", X.int64_json Int64.max_value
      ; "turns", X.int64_json Int64.max_value
      ; "untracked_attempts", X.int64_json Int64.max_value
      ; "untracked_turns", X.int64_json Int64.max_value
      ]
  ;;
end

module Coverage = struct
  type t =
    { historical_children_unknown : bool
    ; unobserved_children : int64
    }

  let create ~historical_children_unknown ~unobserved_children =
    if Int64.(unobserved_children < 0L)
    then invalid "negative unobserved child count"
    else Ok { historical_children_unknown; unobserved_children }
  ;;

  let complete = { historical_children_unknown = false; unobserved_children = 0L }
  let historical_children_unknown t = t.historical_children_unknown
  let unobserved_children t = t.unobserved_children

  let add_disjoint t other =
    let%bind.Result unobserved_children =
      add t.unobserved_children other.unobserved_children
    in
    create
      ~historical_children_unknown:
        (t.historical_children_unknown || other.historical_children_unknown)
      ~unobserved_children
  ;;

  let to_json t =
    `Object
      [ "historical_children_unknown", X.bool_json t.historical_children_unknown
      ; "unobserved_children", X.int64_json t.unobserved_children
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind historical_children_unknown =
      X.required fields "historical_children_unknown" P.Json_codec.bool
    in
    let%bind unobserved_children =
      X.required fields "unobserved_children" X.nonnegative_int64
    in
    create ~historical_children_unknown ~unobserved_children
  ;;

  let shape = X.fields_shape [ "historical_children_unknown"; "unobserved_children" ]
end

module Descendants = struct
  type t =
    { partition : Partition.t
    ; coverage : Coverage.t
    }

  let create ~partition ~coverage = { partition; coverage }
  let partition t = t.partition
  let coverage t = t.coverage
end

type t =
  { reference : D.Reference.t
  ; revision : int64
  ; own : Partition.t
  ; descendants : Descendants.t
  ; canonical_coverage : Q.Coverage.t
  }

let reference t = t.reference
let revision t = t.revision
let own t = t.own
let descendants t = t.descendants
let coverage t = Descendants.coverage t.descendants
let total t = Partition.add_disjoint t.own (Descendants.partition t.descendants)

let create ~reference ~revision ~own ~descendants ~canonical_coverage =
  let open Result.Let_syntax in
  let%bind () =
    D.validate_reference reference
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let%bind accounted_attempts =
    add
      canonical_coverage.Q.Coverage.retired_attempts
      canonical_coverage.untracked_attempts
  in
  let%bind accounted_turns =
    add canonical_coverage.retired_turns canonical_coverage.untracked_turns
  in
  let%bind () =
    if
      Int64.(revision < 0L)
      || Int64.(
           accounted_attempts > (Partition.marks own).attempt
           || accounted_turns > (Partition.marks own).turn)
      || canonical_coverage.Q.Coverage.before_tracking_unknown
      || (not
            (Int64.equal
               canonical_coverage.untracked_attempts
               (Partition.marks own).untracked_attempts))
      || not
           (Int64.equal
              canonical_coverage.untracked_turns
              (Partition.marks own).untracked_turns)
    then invalid "invalid child snapshot canonical identity or coverage"
    else Ok ()
  in
  let t = { reference; revision; own; descendants; canonical_coverage } in
  let%map _ = total t in
  t
;;

let to_json t =
  `Object
    [ "reference", D.reference_to_jsonaf t.reference
    ; "revision", X.int64_json t.revision
    ; "own", Partition.to_json t.own
    ; "descendants", Partition.to_json (Descendants.partition t.descendants)
    ; "canonical_coverage", Q.Coverage.to_json t.canonical_coverage
    ; "coverage", Coverage.to_json (coverage t)
    ]
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind fields = X.object_ raw in
  let%bind reference =
    X.required fields "reference" (fun raw ->
      D.reference_of_jsonaf raw
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
  in
  let%bind revision = X.required fields "revision" X.nonnegative_int64 in
  let%bind own = X.required fields "own" Partition.of_json in
  let%bind descendants = X.required fields "descendants" Partition.of_json in
  let%bind canonical_coverage =
    X.required fields "canonical_coverage" Q.Coverage.of_json
  in
  let%bind coverage = X.required fields "coverage" Coverage.of_json in
  create
    ~reference
    ~revision
    ~own
    ~descendants:(Descendants.create ~partition:descendants ~coverage)
    ~canonical_coverage
;;

let digest t = Digestif.SHA256.(digest_string (Jsonaf.to_string (to_json t)) |> to_hex)

let validate_update previous ~incoming =
  if not (D.Reference.equal previous.reference incoming.reference)
  then invalid "child snapshot admission identity changed"
  else if Int64.(incoming.revision < previous.revision)
  then invalid "child snapshot revision regressed"
  else if Int64.equal incoming.revision previous.revision
  then
    if String.equal (digest previous) (digest incoming)
    then Ok ()
    else invalid "same child snapshot revision has different contents"
  else if
    (not
       (Partition.monotone previous.own incoming.own
        && Partition.monotone
             (Descendants.partition previous.descendants)
             (Descendants.partition incoming.descendants)))
    || ((coverage previous).historical_children_unknown
        && not (coverage incoming).historical_children_unknown)
    || Int64.(
         incoming.canonical_coverage.retired_attempts
         < previous.canonical_coverage.retired_attempts
         || incoming.canonical_coverage.retired_turns
            < previous.canonical_coverage.retired_turns
         || incoming.canonical_coverage.untracked_attempts
            < previous.canonical_coverage.untracked_attempts
         || incoming.canonical_coverage.untracked_turns
            < previous.canonical_coverage.untracked_turns)
  then invalid "child snapshot lifetime counts or unknown history regressed"
  else Ok ()
;;

let shape =
  X.shape_exn
    [ "reference", D.reference_shape
    ; "revision", Document_schema.Shape.value
    ; "own", Partition.shape
    ; "descendants", Partition.shape
    ; ( "canonical_coverage"
      , X.fields_shape
          [ "before_tracking_unknown"
          ; "retired_attempts"
          ; "untracked_attempts"
          ; "retired_turns"
          ; "untracked_turns"
          ; "tracking_limit"
          ] )
    ; "coverage", Coverage.shape
    ]
;;

let capacity_projection ~reference =
  `Object
    [ "reference", D.reference_to_jsonaf reference
    ; "revision", X.int64_json Int64.max_value
    ; "own", Partition.capacity_projection ()
    ; "descendants", Partition.capacity_projection ()
    ; ( "canonical_coverage"
      , `Object
          [ "before_tracking_unknown", `False
          ; "retired_attempts", X.int64_json Int64.max_value
          ; "untracked_attempts", X.int64_json Int64.max_value
          ; "retired_turns", X.int64_json Int64.max_value
          ; "untracked_turns", X.int64_json Int64.max_value
          ; "tracking_limit", `String "protected_future_data"
          ] )
    ; ( "coverage"
      , `Object
          [ "historical_children_unknown", `False
          ; "unobserved_children", X.int64_json Int64.max_value
          ] )
    ]
;;
