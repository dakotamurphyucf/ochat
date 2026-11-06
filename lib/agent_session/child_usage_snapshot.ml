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

  let equal t other = U.equal t.usage other.usage && F.Marks.equal t.marks other.marks
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

module O = Agent_store.Delegation_goal_origin
module Pending = Goal_descendant_pending
module Included = Goal_descendant_included

let scoped_limits =
  Document_schema.Limits.create
    ~max_bytes:(256 * 1024)
    ~max_depth:64
    ~max_fields:16384
    ~max_nodes:32768
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let validate_scoped_raw raw =
  Document_schema.Json.validate ~limits:scoped_limits raw
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
;;

module Goal_scope = struct
  type t =
    { scope : O.Scope.t
    ; own : Partition.t
    ; descendants : Descendants.t
    ; included : Included.t option
    }

  let scope t = t.scope
  let own t = t.own
  let descendants t = t.descendants
  let total t = Partition.add_disjoint t.own (Descendants.partition t.descendants)
  let coverage t = Descendants.coverage t.descendants
  let included t = t.included

  let create ~scope ~own ~descendants ~included =
    let open Result.Let_syntax in
    let%bind () =
      match included with
      | None -> Ok ()
      | Some included ->
        if O.Scope.equal scope (Pending.scope (Included.pending included))
        then Ok ()
        else invalid "child scoped receipt differs from its frozen root"
    in
    let t = { scope; own; descendants; included } in
    let%map _ = total t in
    t
  ;;

  let to_json t =
    `Object
      [ "scope_id", `String (O.Scope.root_id t.scope)
      ; "scope", O.Scope.to_json t.scope
      ; "own", Partition.to_json t.own
      ; "descendants", Partition.to_json (Descendants.partition t.descendants)
      ; "coverage", Coverage.to_json (coverage t)
      ; "included", X.option_json Included.to_json t.included
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = validate_scoped_raw raw in
    let%bind fields = X.object_ raw in
    let%bind scope_id = X.required fields "scope_id" P.Json_codec.string in
    let%bind scope = X.required fields "scope" O.Scope.of_json in
    let%bind () =
      if String.equal scope_id (O.Scope.root_id scope)
      then Ok ()
      else invalid "child scoped identity differs from its root"
    in
    let%bind own = X.required fields "own" Partition.of_json in
    let%bind partition = X.required fields "descendants" Partition.of_json in
    let%bind coverage = X.required fields "coverage" Coverage.of_json in
    let%bind included = X.required fields "included" (X.nullable Included.of_json) in
    create ~scope ~own ~descendants:(Descendants.create ~partition ~coverage) ~included
  ;;

  let validate_update previous ~incoming =
    let open Result.Let_syntax in
    let%bind () =
      if not (O.Scope.equal previous.scope incoming.scope)
      then invalid "child scoped frozen origin changed"
      else if
        (not
           (Partition.monotone previous.own incoming.own
            && Partition.monotone
                 (Descendants.partition previous.descendants)
                 (Descendants.partition incoming.descendants)))
        || (Coverage.historical_children_unknown (coverage previous)
            && not (Coverage.historical_children_unknown (coverage incoming)))
      then invalid "child scoped counts or unknown history regressed"
      else Ok ()
    in
    match previous.included, incoming.included with
    | None, (None | Some _) -> Ok ()
    | Some _, None -> invalid "child scoped included receipt disappeared"
    | Some before, Some after ->
      let old = Included.pending before in
      let next = Included.pending after in
      if
        not
          (D.Reference.equal
             (Pending.branch_reference old)
             (Pending.branch_reference next))
      then invalid "child scoped receipt branch admission changed"
      else if Int64.(Pending.next_prefix next < Pending.next_prefix old)
      then invalid "child scoped included prefix regressed"
      else if Int64.equal (Pending.next_prefix old) (Pending.next_prefix next)
      then
        if
          Included.equal before after
          && F.Marks.equal (Partition.marks previous.own) (Partition.marks incoming.own)
          && F.Marks.equal
               (Partition.marks (Descendants.partition previous.descendants))
               (Partition.marks (Descendants.partition incoming.descendants))
        then Ok ()
        else invalid "same child scoped prefix changed receipt or counts"
      else if Int64.(Pending.nonce next <= Pending.nonce old)
      then invalid "child scoped nonce did not advance with its prefix"
      else Ok ()
  ;;

  let shape =
    X.shape_exn
      [ "scope_id", Document_schema.Shape.value
      ; "scope", O.Scope.shape
      ; "own", Partition.shape
      ; "descendants", Partition.shape
      ; "coverage", Coverage.shape
      ; "included", Document_schema.Shape.nullable Included.shape
      ]
  ;;

  let capacity_projection ~scope ~included ~maximum_attempt =
    let branch = O.Scope.branch scope in
    let reference =
      match included with
      | Some included ->
        D.reference_to_jsonaf (Pending.branch_reference (Included.pending included))
      | None ->
        `Object
          [ ( "key"
            , `Object
                [ ( "parent_session_id"
                  , P.Id.Session.to_json (O.Anchor.parent_session_id branch) )
                ; "parent_generation", X.integer_json (O.Anchor.parent_generation branch)
                ; "principal_id", P.Id.Principal.to_json (O.Anchor.principal_id branch)
                ; "idempotency_key", P.Idempotency_key.to_json (O.Anchor.key branch)
                ] )
          ; "child_session_id", P.Id.Session.to_json (O.Anchor.child_session_id branch)
          ; "revision_id", `String (String.make 96 'r')
          ; "request_sha256", `String (String.make 64 'a')
          ; "admission_sha256", `String (String.make 64 'b')
          ]
    in
    `Object
      [ "scope_id", `String (O.Scope.root_id scope)
      ; "scope", O.Scope.to_json scope
      ; "own", Partition.capacity_projection ()
      ; "descendants", Partition.capacity_projection ()
      ; ( "coverage"
        , `Object
            [ "historical_children_unknown", `False
            ; "unobserved_children", X.int64_json Int64.max_value
            ] )
      ; ( "included"
        , `Object
            [ "scope", O.Scope.to_json scope
            ; "branch_reference", reference
            ; "nonce", X.int64_json Int64.max_value
            ; "next_prefix", X.int64_json Int64.max_value
            ; ( "occurrence"
              , match included with
                | None -> Goal_descendant_occurrence.capacity_projection ()
                | Some included ->
                  if maximum_attempt
                  then Goal_descendant_occurrence.capacity_projection ()
                  else
                    Goal_descendant_occurrence.capacity_projection_for
                      (Pending.occurrence (Included.pending included)) )
            ] )
      ]
  ;;

  let capacity_limits = scoped_limits
end

type t =
  { reference : D.Reference.t
  ; revision : int64
  ; own : Partition.t
  ; descendants : Descendants.t
  ; canonical_coverage : Q.Coverage.t
  ; goal_scopes : Goal_scope.t list option
  }

let goal_scopes t = t.goal_scopes
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
  let t =
    { reference; revision; own; descendants; canonical_coverage; goal_scopes = None }
  in
  let%map _ = total t in
  t
;;

let validate_scope_vector ~own scopes =
  let open Result.Let_syntax in
  let%map _ =
    List.fold_result scopes ~init:None ~f:(fun previous entry ->
      let scope = Goal_scope.scope entry in
      if
        Option.exists previous ~f:(fun previous ->
          O.Scope.compare_root previous scope >= 0)
        || not (Partition.equal own (Goal_scope.own entry))
      then
        invalid
          "child snapshot scope vector is unordered, repeated or has different own usage"
      else Ok (Some scope))
  in
  ()
;;

let validate_origin t ~origin =
  match t.goal_scopes with
  | None -> invalid "known child origin requires an explicit scoped snapshot vector"
  | Some entries ->
    if List.equal O.Scope.equal (O.scopes origin) (List.map entries ~f:Goal_scope.scope)
    then Ok ()
    else invalid "child snapshot differs from exact frozen origin vector"
;;

let create_scoped ~reference ~revision ~own ~descendants ~canonical_coverage ~goal_scopes =
  let open Result.Let_syntax in
  let%bind t = create ~reference ~revision ~own ~descendants ~canonical_coverage in
  let%bind () = validate_scope_vector ~own goal_scopes in
  let%map () =
    validate_scoped_raw (`Array (List.map goal_scopes ~f:Goal_scope.to_json))
  in
  { t with goal_scopes = Some goal_scopes }
;;

let create_with_scopes
      ~reference
      ~revision
      ~own
      ~descendants
      ~canonical_coverage
      ~origin
      ~goal_scopes
  =
  let open Result.Let_syntax in
  let%bind t =
    create_scoped ~reference ~revision ~own ~descendants ~canonical_coverage ~goal_scopes
  in
  let%map () = validate_origin t ~origin in
  t
;;

let to_json t =
  let fields =
    [ "reference", D.reference_to_jsonaf t.reference
    ; "revision", X.int64_json t.revision
    ; "own", Partition.to_json t.own
    ; "descendants", Partition.to_json (Descendants.partition t.descendants)
    ; "canonical_coverage", Q.Coverage.to_json t.canonical_coverage
    ; "coverage", Coverage.to_json (coverage t)
    ]
  in
  `Object
    (match t.goal_scopes with
     | None -> fields
     | Some scopes ->
       fields @ [ "goal_scopes", `Array (List.map scopes ~f:Goal_scope.to_json) ])
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
  let descendants = Descendants.create ~partition:descendants ~coverage in
  match P.Json_codec.optional fields "goal_scopes" with
  | None -> create ~reference ~revision ~own ~descendants ~canonical_coverage
  | Some raw ->
    let%bind () = validate_scoped_raw raw in
    let%bind goal_scopes = X.list Goal_scope.of_json raw in
    create_scoped ~reference ~revision ~own ~descendants ~canonical_coverage ~goal_scopes
;;

let digest t = Digestif.SHA256.(digest_string (Jsonaf.to_string (to_json t)) |> to_hex)

let validate_scoped_update previous incoming =
  match previous.goal_scopes, incoming.goal_scopes with
  | None, None -> Ok ()
  | None, Some _ | Some _, None -> invalid "child snapshot scoped presence changed"
  | Some previous, Some incoming ->
    if List.length previous <> List.length incoming
    then invalid "child snapshot frozen scope vector length changed"
    else
      List.fold_result
        (List.zip_exn previous incoming)
        ~init:()
        ~f:(fun () (previous, incoming) -> Goal_scope.validate_update previous ~incoming)
;;

let validate_update previous ~incoming =
  let%bind.Result () = validate_scoped_update previous incoming in
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
    ; "goal_scopes", X.array_shape_exn ~identity_field:"scope_id" Goal_scope.shape
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
