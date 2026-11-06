open! Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module L = Inference_ledger
module F = Canonical_usage_fold
module U = Goal_usage
module Q = P.Inference_query
module X = Persistence_codec

type t =
  { reference : D.Reference.t
  ; settled : U.t
  ; last_ledger_revision : int64
  ; marks : F.Marks.t
  }

let reference t = t.reference
let settled t = t.settled
let last_ledger_revision t = t.last_ledger_revision
let invalid message = Error (P.Error.invalid_request message)

let ledger result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (L.Error.sexp_of_t error)))
;;

let fold result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (F.Error.sexp_of_t error)))
;;

let counter result =
  Result.map_error result ~f:(fun (_ : Goal_accounting_counter.error) ->
    P.Error.invalid_request "delegated usage count exhausted")
;;

let equal a b =
  D.Reference.equal a.reference b.reference
  && U.equal a.settled b.settled
  && Int64.equal a.last_ledger_revision b.last_ledger_revision
  && F.Marks.equal a.marks b.marks
;;

let canonical reference current =
  ledger
    (L.validate
       current
       ~limits:L.Limits.default
       ~session_id:reference.D.Reference.child_session_id
       ~generation:(L.generation current))
;;

let validate t ~ledger:current =
  let open Result.Let_syntax in
  let%bind () = canonical t.reference current in
  let%bind marks = F.Marks.of_ledger current |> fold in
  let coverage = Q.Summary.coverage (L.summary current) in
  let%bind ended =
    let%bind a = Goal_accounting_counter.of_int64 coverage.retired_attempts |> counter in
    let%bind b =
      Goal_accounting_counter.of_int64 coverage.untracked_attempts |> counter
    in
    Goal_accounting_counter.add a b
    |> counter
    |> Result.map ~f:Goal_accounting_counter.to_int64
  in
  if
    coverage.before_tracking_unknown
    || (not (Int64.equal t.last_ledger_revision (L.revision current)))
    || (not (F.Marks.equal t.marks marks))
    || (not (Int64.equal (U.attempts t.settled) ended))
    || not (Int64.equal (U.untracked_attempts t.settled) marks.untracked_attempts)
  then invalid "delegated partition differs from its exact canonical frontier"
  else Ok ()
;;

let create ~reference ~ledger:current =
  let open Result.Let_syntax in
  let%bind () =
    D.validate_reference reference
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error)))
  in
  let%bind () = canonical reference current in
  let%bind marks = F.Marks.of_ledger current |> fold in
  let%bind () =
    if
      Int64.equal marks.attempt 0L
      && Int64.equal marks.turn 0L
      && Int64.equal (L.revision current) 0L
    then Ok ()
    else invalid "historical delegated ledger cannot bootstrap exact zero usage"
  in
  let t = { reference; settled = U.empty; last_ledger_revision = 0L; marks } in
  let%map () = validate t ~ledger:current in
  t
;;

let reconcile t ~before ~after =
  let open Result.Let_syntax in
  let%bind () = validate t ~ledger:before in
  let%bind () = L.validate_update before ~incoming:after |> ledger in
  let%bind plan =
    F.prepare t.settled ~before ~after ~handles:(List.map (L.rows before) ~f:L.Row.handle)
    |> fold
  in
  let%bind marks = F.Marks.of_ledger after |> fold in
  let%bind settled =
    U.add_untracked_count
      (F.Plan.settled plan)
      ~count:Int64.(marks.untracked_attempts - t.marks.untracked_attempts)
    |> counter
  in
  let candidate = { t with settled; marks; last_ledger_revision = L.revision after } in
  let%map () = validate candidate ~ledger:after in
  candidate
;;

let usage t ~ledger:current =
  let open Result.Let_syntax in
  let%bind () = validate t ~ledger:current in
  List.fold_result (L.rows current) ~init:t.settled ~f:(fun total row ->
    U.add_record total (L.Row.record row) |> counter)
;;

let to_json t =
  `Object
    [ "reference", D.reference_to_jsonaf t.reference
    ; "settled", U.Persistence.to_json t.settled
    ; "last_ledger_revision", X.int64_json t.last_ledger_revision
    ; "attempts", X.int64_json t.marks.attempt
    ; "turns", X.int64_json t.marks.turn
    ; "untracked_attempts", X.int64_json t.marks.untracked_attempts
    ; "untracked_turns", X.int64_json t.marks.untracked_turns
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind reference =
    X.required fields "reference" (fun raw ->
      D.reference_of_jsonaf raw
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))))
  in
  let%bind settled = X.required fields "settled" U.Persistence.of_json in
  let n name = X.required fields name X.nonnegative_int64 in
  let%bind last_ledger_revision = n "last_ledger_revision" in
  let%bind attempt = n "attempts" in
  let%bind turn = n "turns" in
  let%bind untracked_attempts = n "untracked_attempts" in
  let%bind untracked_turns = n "untracked_turns" in
  if
    Int64.(
      U.attempts settled > attempt
      || untracked_attempts > attempt
      || untracked_turns > turn)
    || not (Int64.equal (U.untracked_attempts settled) untracked_attempts)
  then invalid "inconsistent delegated usage counters"
  else
    Ok
      { reference
      ; settled
      ; last_ledger_revision
      ; marks = { attempt; turn; untracked_attempts; untracked_turns }
      }
;;

let validate_update previous ~incoming =
  if equal previous incoming
  then Ok ()
  else if
    (not (D.Reference.equal previous.reference incoming.reference))
    || Int64.(
         incoming.last_ledger_revision <= previous.last_ledger_revision
         || incoming.marks.attempt < previous.marks.attempt
         || incoming.marks.turn < previous.marks.turn
         || incoming.marks.untracked_attempts < previous.marks.untracked_attempts
         || incoming.marks.untracked_turns < previous.marks.untracked_turns)
  then invalid "delegated usage identity or canonical frontier regressed"
  else Ok ()
;;

let validate_transition previous ~incoming ~before ~after =
  let open Result.Let_syntax in
  let%bind () = validate_update previous ~incoming in
  let%bind expected = reconcile previous ~before ~after in
  if equal expected incoming
  then Ok ()
  else invalid "delegated usage differs from actual ordered canonical fold"
;;

let shape =
  X.shape_exn
    [ "reference", D.reference_shape
    ; "settled", U.Persistence.shape
    ; "last_ledger_revision", Document_schema.Shape.value
    ; "attempts", Document_schema.Shape.value
    ; "turns", Document_schema.Shape.value
    ; "untracked_attempts", Document_schema.Shape.value
    ; "untracked_turns", Document_schema.Shape.value
    ]
;;

let capacity_projection t =
  match to_json t with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         ( key
         , if String.equal key "reference"
           then value
           else if String.equal key "settled"
           then U.Persistence.capacity_projection t.settled
           else X.int64_json Int64.max_value )))
  | `Null | `False | `True | `String _ | `Number _ | `Array _ -> assert false
;;
