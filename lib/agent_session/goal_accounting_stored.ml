open! Core
open Goal_accounting_state
module P = Agent_protocol
module G = P.Goal
module L = Inference_ledger
module O = Inference.Observation
module Usage = Goal_usage
module X = Persistence_codec
module D = Document_schema

type accounting = t

type claim =
  { ordinal : int64
  ; session_id : P.Id.Session.t
  ; generation : int
  ; scope : Transcript.Scope.t
  ; accounting_id : O.Observation_id.t
  ; context_id : O.Observation_id.t
  ; operation_id : P.Id.Operation.t option
  ; invocation_id : P.Id.Invocation.t option
  ; run_id : P.Id.Run.t
  ; source : P.Run_source.t
  }

type t =
  { scope : Scope.t
  ; revision : int64
  ; admitted_turns : int64
  ; admitted_attempts : int64
  ; untracked_turns : int64
  ; settled : Usage.t
  ; bindings : claim list
  ; max_bindings : int
  ; last_attempt_ordinal : int64
  ; last_turn_ordinal : int64
  ; last_ledger_revision : int64
  }

let invalid message = Error (P.Error.invalid_request message)

let claim (binding : Binding.t) =
  let h = binding.handle in
  { ordinal = L.Handle.ordinal h
  ; session_id = L.Handle.session_id h
  ; generation = L.Handle.generation h
  ; scope = L.Handle.scope h
  ; accounting_id = L.Handle.accounting_id h
  ; context_id = L.Handle.context_id h
  ; operation_id = L.Handle.operation_id h
  ; invocation_id = L.Handle.invocation_id h
  ; run_id = binding.run_id
  ; source = binding.source
  }
;;

let capture (a : accounting) =
  { scope = a.scope
  ; revision = a.revision
  ; admitted_turns = a.admitted_turns
  ; admitted_attempts = a.admitted_attempts
  ; untracked_turns = a.untracked_turns
  ; settled = a.settled
  ; bindings = List.map (bindings a) ~f:claim
  ; max_bindings = a.max_bindings
  ; last_attempt_ordinal = a.last_attempt_ordinal
  ; last_turn_ordinal = a.last_turn_ordinal
  ; last_ledger_revision = a.last_ledger_revision
  }
;;

let goal_id t = t.scope.goal_id
let session t = t.scope.session
let revision t = t.revision
let admitted_attempts t = t.admitted_attempts
let admitted_turns t = t.admitted_turns
let untracked_attempts t = Usage.untracked_attempts t.settled
let untracked_turns t = t.untracked_turns
let ordinals t = List.map t.bindings ~f:(fun c -> c.ordinal)
let run_bindings t = List.map t.bindings ~f:(fun c -> c.run_id, c.source)

let claim_json (c : claim) =
  `Object
    [ "ordinal", X.int64_json c.ordinal
    ; "session_id", P.Id.Session.to_json c.session_id
    ; "generation", X.integer_json c.generation
    ; "scope", Transcript.Scope.to_json c.scope
    ; "accounting_id", `String (O.Observation_id.to_string c.accounting_id)
    ; "context_id", `String (O.Observation_id.to_string c.context_id)
    ; "operation_id", X.option_json P.Id.Operation.to_json c.operation_id
    ; "invocation_id", X.option_json P.Id.Invocation.to_json c.invocation_id
    ; "run_id", P.Id.Run.to_json c.run_id
    ; "source", P.Run_source.to_json c.source
    ]
;;

let to_json t =
  `Object
    [ "goal_id", P.Id.Goal.to_json t.scope.goal_id
    ; "session", P.Session_ref.to_json t.scope.session
    ; "revision", X.int64_json t.revision
    ; "admitted_turns", X.int64_json t.admitted_turns
    ; "admitted_attempts", X.int64_json t.admitted_attempts
    ; "untracked_turns", X.int64_json t.untracked_turns
    ; "settled", Usage.Persistence.to_json t.settled
    ; "bindings", `Array (List.map t.bindings ~f:claim_json)
    ; "max_bindings", X.integer_json t.max_bindings
    ; "last_attempt_ordinal", X.int64_json t.last_attempt_ordinal
    ; "last_turn_ordinal", X.int64_json t.last_turn_ordinal
    ; "last_ledger_revision", X.int64_json t.last_ledger_revision
    ]
;;

let equal a b = D.Json.equal (to_json a) (to_json b)
let claim_equal a b = D.Json.equal (claim_json a) (claim_json b)

let claim_of_json json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind ordinal = X.required f "ordinal" X.nonnegative_int64 in
  let%bind session_id = X.required f "session_id" P.Id.Session.of_json in
  let%bind generation = X.required f "generation" X.integer in
  let%bind scope =
    X.required f "scope" (fun json ->
      Transcript.Scope.of_json json ~limits:D.Limits.default
      |> Result.map_error ~f:P.Error.invalid_request)
  in
  let oid json =
    P.Json_codec.string json
    |> Result.bind ~f:(fun s ->
      O.Observation_id.of_string s
      |> Result.map_error ~f:(fun e ->
        P.Error.invalid_request (Sexp.to_string_hum (O.Error.sexp_of_t e))))
  in
  let%bind accounting_id = X.required f "accounting_id" oid in
  let%bind context_id = X.required f "context_id" oid in
  let%bind operation_id =
    X.required f "operation_id" (X.nullable P.Id.Operation.of_json)
  in
  let%bind invocation_id =
    X.required f "invocation_id" (X.nullable P.Id.Invocation.of_json)
  in
  let%bind run_id = X.required f "run_id" P.Id.Run.of_json in
  let%bind source = X.required f "source" P.Run_source.of_json in
  if Int64.(ordinal <= 0L) || generation < 0 || generation <> source.generation
  then invalid "invalid stored binding identity"
  else
    Ok
      { ordinal
      ; session_id
      ; generation
      ; scope
      ; accounting_id
      ; context_id
      ; operation_id
      ; invocation_id
      ; run_id
      ; source
      }
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate
      ~limits:
        (X.limits ~max_bytes:(256 * 1024)
         |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
         |> Result.ok_or_failwith)
      json
    |> Result.map_error ~f:(fun e ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
  in
  let%bind f = X.object_ json in
  let%bind goal_id = X.required f "goal_id" P.Id.Goal.of_json in
  let%bind session = X.required f "session" P.Session_ref.of_json in
  let n name = X.required f name X.nonnegative_int64 in
  let%bind revision = n "revision" in
  let%bind admitted_turns = n "admitted_turns" in
  let%bind admitted_attempts = n "admitted_attempts" in
  let%bind untracked_turns = n "untracked_turns" in
  let%bind settled = X.required f "settled" Usage.Persistence.of_json in
  let%bind max_bindings = X.required f "max_bindings" X.integer in
  let%bind bindings =
    X.required f "bindings" (function
      | `Array xs when List.length xs <= 256 -> Result.all (List.map xs ~f:claim_of_json)
      | _ -> invalid "stored bindings exceed256")
  in
  let%bind last_attempt_ordinal = n "last_attempt_ordinal" in
  let%bind last_turn_ordinal = n "last_turn_ordinal" in
  let%bind last_ledger_revision = n "last_ledger_revision" in
  let%bind total =
    add (Usage.attempts settled) (Int64.of_int (List.length bindings))
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "accounting attempt count overflow")
  in
  let%bind _ =
    List.fold_result bindings ~init:0L ~f:(fun previous c ->
      if
        Int64.(c.ordinal <= previous || c.ordinal > last_attempt_ordinal)
        || not (P.Id.Session.equal c.session_id (P.Session_ref.session_id session))
      then invalid "stored binding order or scope differs"
      else Ok c.ordinal)
  in
  if
    max_bindings <= 0
    || max_bindings > 256
    || List.length bindings > max_bindings
    || (not (Int64.equal total admitted_attempts))
    || Int64.(
         untracked_turns > admitted_turns
         || admitted_turns > last_turn_ordinal
         || admitted_attempts > last_attempt_ordinal)
    || ((not (List.is_empty bindings)) && Int64.equal revision Int64.max_value)
  then invalid "inconsistent stored accounting counters or exhausted retirement frontier"
  else
    Ok
      { scope = { Scope.goal_id; session }
      ; revision
      ; admitted_turns
      ; admitted_attempts
      ; untracked_turns
      ; settled
      ; bindings
      ; max_bindings
      ; last_attempt_ordinal
      ; last_turn_ordinal
      ; last_ledger_revision
      }
;;

let resolve t ~goal ~ledger:current =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Goal.equal (G.id goal) t.scope.goal_id
      && P.Session_ref.equal (G.session goal) t.scope.session
    then Ok ()
    else Error Error.Scope_mismatch
  in
  let%bind () =
    if
      P.Id.Session.equal (P.Session_ref.session_id t.scope.session) (L.session_id current)
    then Ok ()
    else Error Error.Scope_mismatch
  in
  let%bind current_marks = marks current in
  let%bind () =
    if
      Int64.(
        t.last_attempt_ordinal > current_marks.attempt
        || t.last_turn_ordinal > current_marks.turn
        || t.last_ledger_revision > L.revision current)
    then Error Error.Stale_plan
    else Ok ()
  in
  let%map bindings =
    List.fold_result t.bindings ~init:Int64.Map.empty ~f:(fun found c ->
      match L.find current ~ordinal:c.ordinal with
      | None -> Error Error.Missing_bound_row
      | Some row ->
        let binding =
          Binding.{ handle = L.Row.handle row; run_id = c.run_id; source = c.source }
        in
        if claim_equal c (claim binding)
        then Ok (Map.set found ~key:c.ordinal ~data:binding)
        else Error Error.Conflicting_binding)
  in
  ({ scope = t.scope
   ; revision = t.revision
   ; admitted_turns = t.admitted_turns
   ; admitted_attempts = t.admitted_attempts
   ; untracked_turns = t.untracked_turns
   ; settled = t.settled
   ; bindings
   ; max_bindings = t.max_bindings
   ; last_attempt_ordinal = t.last_attempt_ordinal
   ; last_turn_ordinal = t.last_turn_ordinal
   ; last_ledger_revision = t.last_ledger_revision
   }
   : accounting)
;;

let validate_update previous ~incoming =
  let open Result.Let_syntax in
  let%bind _ = of_json (to_json incoming) in
  if equal previous incoming
  then Ok ()
  else if
    (not (Scope.equal previous.scope incoming.scope))
    || previous.max_bindings <> incoming.max_bindings
    || Int64.equal previous.revision Int64.max_value
    || (not (Int64.equal incoming.revision (Int64.succ previous.revision)))
    || Int64.(
         incoming.admitted_turns < previous.admitted_turns
         || incoming.admitted_attempts < previous.admitted_attempts
         || incoming.untracked_turns < previous.untracked_turns
         || incoming.last_attempt_ordinal < previous.last_attempt_ordinal
         || incoming.last_turn_ordinal < previous.last_turn_ordinal
         || incoming.last_ledger_revision < previous.last_ledger_revision)
  then invalid "accounting identity or frontier changed"
  else
    List.fold_result previous.bindings ~init:() ~f:(fun () old ->
      match
        List.find incoming.bindings ~f:(fun c -> Int64.equal c.ordinal old.ordinal)
      with
      | None -> Ok ()
      | Some current ->
        if claim_equal old current
        then Ok ()
        else invalid "retained accounting binding changed")
;;

let validate_transition previous ~incoming ~before ~after =
  let open Result.Let_syntax in
  let%bind () = validate_update previous ~incoming in
  (* Exact retirement evidence is checked independently of any new admission.
       Actual admission authorization remains with the actor integration. *)
  let%bind handles =
    List.map previous.bindings ~f:(fun c ->
      match L.find before ~ordinal:c.ordinal with
      | None -> invalid "accounting binding lacks its actual before-ledger row"
      | Some row ->
        let binding =
          Binding.{ handle = L.Row.handle row; run_id = c.run_id; source = c.source }
        in
        if claim_equal c (claim binding)
        then Ok binding.handle
        else invalid "accounting binding differs from its actual before-ledger row")
    |> Result.all
  in
  let%bind fold =
    Canonical_usage_fold.prepare previous.settled ~before ~after ~handles
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Canonical_usage_fold.Error.sexp_of_t error)))
  in
  let%bind () =
    List.fold_result previous.bindings ~init:() ~f:(fun () c ->
      let removed =
        not (List.exists incoming.bindings ~f:(fun n -> Int64.equal n.ordinal c.ordinal))
      in
      let retired =
        List.exists (Canonical_usage_fold.Plan.retired fold) ~f:(fun handle ->
          Int64.equal (L.Handle.ordinal handle) c.ordinal)
      in
      if Bool.equal removed retired
      then Ok ()
      else invalid "accounting binding removal differs from actual canonical retirement")
  in
  let expected = Canonical_usage_fold.Plan.settled fold in
  let delta =
    Int64.(
      Usage.untracked_attempts incoming.settled
      - Usage.untracked_attempts previous.settled)
  in
  let%bind expected =
    if Int64.equal delta 0L
    then Ok expected
    else if Int64.equal delta 1L
    then (
      let%bind a =
        marks before
        |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid ledger marks")
      in
      let%bind b =
        marks after
        |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid ledger marks")
      in
      if
        Int64.(a.untracked_attempts < Int64.max_value)
        && Int64.equal b.untracked_attempts (Int64.succ a.untracked_attempts)
      then
        Usage.add_untracked expected
        |> Result.map_error ~f:(fun _ ->
          P.Error.invalid_request "untracked aggregate overflow")
      else invalid "untracked account lacks canonical admission")
    else invalid "invalid untracked account advance"
  in
  let%bind () =
    if Usage.equal expected incoming.settled
    then Ok ()
    else invalid "settled accounting differs from final canonical rows"
  in
  let%bind a =
    marks before
    |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid ledger marks")
  in
  let%bind b =
    marks after
    |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid ledger marks")
  in
  let added =
    List.filter incoming.bindings ~f:(fun c ->
      not
        (List.exists previous.bindings ~f:(fun old -> Int64.equal old.ordinal c.ordinal)))
  in
  let attempts = Int64.(incoming.admitted_attempts - previous.admitted_attempts) in
  let turns = Int64.(incoming.admitted_turns - previous.admitted_turns) in
  let%bind () =
    if
      Int64.equal attempts (Int64.of_int (List.length added) |> Int64.( + ) delta)
      && Int64.(attempts >= 0L && attempts <= 1L && turns >= 0L && turns <= 1L)
      && (Int64.equal attempts 0L
          || (Int64.(a.attempt < Int64.max_value)
              && Int64.equal b.attempt (Int64.succ a.attempt)))
      && (Int64.equal turns 0L
          || (Int64.(a.turn < Int64.max_value) && Int64.equal b.turn (Int64.succ a.turn))
         )
      && List.for_all added ~f:(fun c -> Int64.equal c.ordinal b.attempt)
    then Ok ()
    else invalid "account admission counters lack exact canonical progress"
  in
  let%bind () =
    if
      Int64.equal incoming.untracked_turns previous.untracked_turns
      || (Int64.equal turns 1L
          && Int64.(
               previous.untracked_turns < Int64.max_value
               && a.untracked_turns < Int64.max_value)
          && Int64.equal incoming.untracked_turns (Int64.succ previous.untracked_turns)
          && Int64.equal b.untracked_turns (Int64.succ a.untracked_turns))
    then Ok ()
    else invalid "untracked turn lacks canonical admission"
  in
  if
    equal previous incoming
    || (Int64.equal incoming.last_attempt_ordinal b.attempt
        && Int64.equal incoming.last_turn_ordinal b.turn
        && Int64.equal incoming.last_ledger_revision (L.revision after))
  then Ok ()
  else invalid "changed accounting has stale canonical frontier"
;;

let value = D.Shape.value
let key_shape = X.fields_shape [ "source"; "attempt" ]

let scope_shape =
  X.shape_exn
    [ "key", key_shape
    ; ( "parent"
      , D.Shape.nullable
          (X.shape_exn
             [ "scope", key_shape; "call_entry_id", value; "call_alias", value ]) )
    ]
;;

let claim_shape =
  X.shape_exn
    [ "ordinal", value
    ; "session_id", value
    ; "generation", value
    ; "scope", scope_shape
    ; "accounting_id", value
    ; "context_id", value
    ; "operation_id", value
    ; "invocation_id", value
    ; "run_id", value
    ; "source", Run_record_shapes.source
    ]
;;

let shape =
  X.shape_exn
    [ "goal_id", value
    ; "session", X.fields_shape [ "server_id"; "session_id" ]
    ; "revision", value
    ; "admitted_turns", value
    ; "admitted_attempts", value
    ; "untracked_turns", value
    ; "settled", Usage.Persistence.shape
    ; "bindings", X.array_shape_exn ~identity_field:"ordinal" claim_shape
    ; "max_bindings", value
    ; "last_attempt_ordinal", value
    ; "last_turn_ordinal", value
    ; "last_ledger_revision", value
    ]
;;

let retirement_projection t ~ordinal =
  let bindings =
    match ordinal with
    | None -> t.bindings
    | Some ordinal ->
      List.filter t.bindings ~f:(fun c -> not (Int64.equal c.ordinal ordinal))
  in
  match to_json { t with bindings } with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, raw) ->
         if String.equal name "settled"
         then name, Usage.Persistence.capacity_projection t.settled
         else if
           List.mem
             [ "revision"
             ; "last_attempt_ordinal"
             ; "last_turn_ordinal"
             ; "last_ledger_revision"
             ]
             name
             ~equal:String.equal
         then name, X.int64_json Int64.max_value
         else name, raw))
  | _ -> assert false
;;
