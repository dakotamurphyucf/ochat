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

module Admission = struct
  type t =
    | Attempt of
        { episode : G.Episode.t
        ; handle : L.Handle.t
        ; tracking : L.tracking
        }
    | Turn of
        { episode : G.Episode.t
        ; operation : P.Operation.t
        ; handle : L.Turn_handle.t
        ; tracking : L.tracking
        }
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

type marks =
  { attempt : int64
  ; turn : int64
  ; untracked_attempts : int64
  ; untracked_turns : int64
  }

let marks ledger =
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

let check_session t ledger =
  if P.Id.Session.equal (P.Session_ref.session_id t.scope.session) (L.session_id ledger)
  then Ok ()
  else Error Error.Scope_mismatch
;;

let create ?(max_bindings = 256) goal ~ledger:current =
  let open Result.Let_syntax in
  if max_bindings <= 0 || max_bindings > 256
  then
    Error
      (Error.Invalid_field
         { field = "max_bindings"; reason = "required positive bound at most256" })
  else (
    let scope = { Scope.goal_id = G.id goal; session = G.session goal } in
    let%bind () =
      ledger
        (L.validate
           current
           ~limits:L.Limits.default
           ~session_id:(P.Session_ref.session_id scope.session)
           ~generation:(L.generation current))
    in
    let%map fences = marks current in
    { scope
    ; revision = 0L
    ; admitted_turns = 0L
    ; admitted_attempts = 0L
    ; untracked_turns = 0L
    ; settled = Usage.empty
    ; bindings = Int64.Map.empty
    ; max_bindings
    ; last_attempt_ordinal = fences.attempt
    ; last_turn_ordinal = fences.turn
    ; last_ledger_revision = L.revision current
    })
;;

let bound_row current binding =
  match L.find current ~ordinal:(L.Handle.ordinal binding.Binding.handle) with
  | Some row when L.Handle.equal (L.Row.handle row) binding.handle -> Ok row
  | Some _ -> Error Error.Conflicting_binding
  | None -> Error Error.Missing_bound_row
;;

let usage t ~ledger:current =
  let open Result.Let_syntax in
  let%bind () = check_session t current in
  let%bind () =
    if Int64.(L.revision current < t.last_ledger_revision)
    then Error Error.Stale_plan
    else Ok ()
  in
  List.fold_result (bindings t) ~init:t.settled ~f:(fun totals binding ->
    let%bind row = bound_row current binding in
    counter (Usage.add_record totals (L.Row.record row)))
;;

let ended record =
  match O.Attempt_record.state record with
  | Prepared | Running -> false
  | Terminal _ | Interrupted _ -> true
;;

let reconcile t ~before ~after =
  let open Result.Let_syntax in
  List.fold_result (bindings t) ~init:(t, []) ~f:(fun (candidate, retired) binding ->
    let ordinal = L.Handle.ordinal binding.Binding.handle in
    let%bind old = bound_row before binding in
    match L.find after ~ordinal with
    | Some row ->
      if L.Handle.equal (L.Row.handle row) binding.handle
      then Ok (candidate, retired)
      else Error Error.Conflicting_binding
    | None ->
      if not (ended (L.Row.record old))
      then Error Error.Invalid_retirement
      else (
        let%map settled =
          counter (Usage.add_record candidate.settled (L.Row.record old))
        in
        ( { candidate with settled; bindings = Map.remove candidate.bindings ordinal }
        , binding :: retired )))
;;

let check_episode t goal episode ~after =
  let source = (G.Episode.admission episode).source in
  if
    P.Session_ref.equal (G.Episode.session episode) t.scope.session
    && Option.exists (G.episode goal) ~f:(G.Episode.equal episode)
    && P.Run_source.equal source (G.source goal)
    && Int.equal source.generation (L.generation after)
  then Ok ()
  else Error Error.Scope_mismatch
;;

let check_new before after =
  Result.bind (increment before) ~f:(fun expected ->
    if Int64.equal expected after then Ok () else Error Error.Invalid_admission)
;;

let admit_attempt t ~before:_ ~after ~before_marks ~after_marks ~episode ~handle ~tracking
  =
  let open Result.Let_syntax in
  let%bind () = check_new before_marks.attempt after_marks.attempt in
  let%bind () =
    if
      Int64.equal (L.Handle.ordinal handle) after_marks.attempt
      && P.Id.Session.equal (L.Handle.session_id handle) (L.session_id after)
      && Int.equal (L.Handle.generation handle) (L.generation after)
    then Ok ()
    else Error Error.Invalid_admission
  in
  let%bind admitted_attempts = increment t.admitted_attempts in
  match tracking, L.find after ~ordinal:(L.Handle.ordinal handle) with
  | L.Tracked, Some row when L.Handle.equal handle (L.Row.handle row) ->
    if Map.mem t.bindings (L.Handle.ordinal handle)
    then Error Error.Conflicting_binding
    else if Map.length t.bindings >= t.max_bindings
    then Error Error.Binding_capacity
    else (
      let admission = G.Episode.admission episode in
      let binding =
        { Binding.handle; run_id = admission.run_id; source = admission.source }
      in
      Ok
        { t with
          admitted_attempts
        ; bindings = Map.set t.bindings ~key:(L.Handle.ordinal handle) ~data:binding
        })
  | Untracked _, None ->
    let%bind () =
      check_new before_marks.untracked_attempts after_marks.untracked_attempts
    in
    let%map settled = counter (Usage.add_untracked t.settled) in
    { t with admitted_attempts; settled }
  | Tracked, None | Tracked, Some _ | Untracked _, Some _ -> Error Error.Invalid_admission
;;

let matching_turn current handle =
  L.find_turn_handle
    current
    ~operation_id:(L.Turn_handle.operation_id handle)
    ~generation:(L.Turn_handle.generation handle)
  |> Option.exists ~f:(fun found ->
    P.Id.Operation.equal
      (L.Turn_handle.operation_id found)
      (L.Turn_handle.operation_id handle)
    && Int.equal (L.Turn_handle.generation found) (L.Turn_handle.generation handle))
;;

let admit_turn t ~before ~after ~before_marks ~after_marks ~operation ~handle ~tracking =
  let open Result.Let_syntax in
  let%bind () =
    match P.Operation.of_json (P.Operation.to_json operation) with
    | Error _ -> Error Error.Invalid_admission
    | Ok operation ->
      if
        P.Id.Operation.equal operation.id (L.Turn_handle.operation_id handle)
        && Int.equal operation.generation (L.Turn_handle.generation handle)
        && Int.equal operation.generation (L.generation after)
        &&
        match operation.kind, operation.state with
        | Turn _, (Starting | Running) -> true
        | Turn _, (Cancelling | Completed | Failed _ | Cancelled | Interrupted _)
        | Compaction, _ -> false
      then Ok ()
      else Error Error.Invalid_admission
  in
  if
    Int64.equal before_marks.turn after_marks.turn
    && matching_turn before handle
    && matching_turn after handle
    && L.equal_tracking tracking Tracked
  then Ok (t, false)
  else (
    let%bind () = check_new before_marks.turn after_marks.turn in
    let%bind () =
      if matching_turn before handle then Error Error.Invalid_admission else Ok ()
    in
    let%bind () =
      match tracking with
      | L.Tracked ->
        if matching_turn after handle then Ok () else Error Error.Invalid_admission
      | Untracked _ ->
        if matching_turn after handle
        then Error Error.Invalid_admission
        else check_new before_marks.untracked_turns after_marks.untracked_turns
    in
    let%bind admitted_turns = increment t.admitted_turns in
    let%map untracked_turns =
      match tracking with
      | L.Tracked -> Ok t.untracked_turns
      | Untracked _ -> increment t.untracked_turns
    in
    { t with admitted_turns; untracked_turns }, true)
;;

module Plan = struct
  type accounting = t

  type admission_disposition =
    | Not_requested
    | Counted
    | Already_retained
  [@@deriving equal, sexp_of]

  type t =
    { expected_accounting_revision : int64
    ; before_ledger : L.t
    ; after_ledger : L.t
    ; candidate : accounting
    ; retired_bindings : Binding.t list
    ; admission_disposition : admission_disposition
    }

  let expected_accounting_revision t = t.expected_accounting_revision
  let before_ledger_revision t = L.revision t.before_ledger
  let after_ledger_revision t = L.revision t.after_ledger
  let before_ledger t = t.before_ledger
  let after_ledger t = t.after_ledger
  let candidate t = t.candidate
  let retired_bindings t = t.retired_bindings
  let admission_disposition t = t.admission_disposition
end

let plan t ~goal ~before ~after ~admission =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Goal.equal t.scope.goal_id (G.id goal)
      && P.Session_ref.equal t.scope.session (G.session goal)
    then Ok ()
    else Error Error.Scope_mismatch
  in
  let%bind () = check_session t before in
  let%bind () = check_session t after in
  let%bind () = ledger (L.validate_update before ~incoming:after) in
  let%bind before_marks = marks before in
  let%bind after_marks = marks after in
  let%bind () =
    if
      Int64.(
        before_marks.attempt < t.last_attempt_ordinal
        || before_marks.turn < t.last_turn_ordinal
        || L.revision before < t.last_ledger_revision)
    then Error Error.Stale_plan
    else Ok ()
  in
  let%bind candidate, retired_bindings = reconcile t ~before ~after in
  let%bind candidate, admission_disposition =
    match admission with
    | None -> Ok (candidate, Plan.Not_requested)
    | Some (Admission.Attempt { episode; handle; tracking }) ->
      let%bind () = check_episode t goal episode ~after in
      let%map candidate =
        admit_attempt
          candidate
          ~before
          ~after
          ~before_marks
          ~after_marks
          ~episode
          ~handle
          ~tracking
      in
      candidate, Plan.Counted
    | Some (Turn { episode; operation; handle; tracking }) ->
      let%bind () = check_episode t goal episode ~after in
      let%map candidate, counted =
        admit_turn
          candidate
          ~before
          ~after
          ~before_marks
          ~after_marks
          ~operation
          ~handle
          ~tracking
      in
      candidate, if counted then Plan.Counted else Already_retained
  in
  let%bind candidate =
    if equal t candidate
    then Ok t
    else (
      let%map revision = increment t.revision in
      { candidate with
        revision
      ; last_attempt_ordinal = after_marks.attempt
      ; last_turn_ordinal = after_marks.turn
      ; last_ledger_revision = L.revision after
      })
  in
  Ok
    { Plan.expected_accounting_revision = t.revision
    ; before_ledger = before
    ; after_ledger = after
    ; candidate
    ; retired_bindings = List.rev retired_bindings
    ; admission_disposition
    }
;;
