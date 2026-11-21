open! Core
module P = Agent_protocol
module Scope = P.Recovery_window.Scope
module J = P.Json_codec
module D = Document_schema

let nonnegative = J.bounded_int64 ~min:0L ~max:Int64.max_value
let revision_json revision = `Number (Int64.to_string revision)
let invalid message = Error (P.Error.invalid_request message)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:true ())

module Checkpoint = Recovery_rollover_checkpoint
module Expected = Checkpoint.Expected

module Requester = struct
  type t =
    | Operator of
        { principal : P.Id.Principal.t
        ; key : P.Idempotency_key.t
        ; force : bool
        }
    | Native of
        { principal : P.Id.Principal.t
        ; operation : P.Id.Operation.t
        ; invocation : P.Id.Invocation.t
        }
    | Automatic of
        { principal : P.Id.Principal.t
        ; cut : Recovery_rollover_capacity.t
        }
  [@@deriving equal, sexp_of]

  module Key = struct
    module T = struct
      type t =
        | Operator of P.Id.Principal.t * P.Idempotency_key.t
        | Native of P.Id.Principal.t * P.Id.Operation.t * P.Id.Invocation.t
        | Automatic of P.Id.Operation.t
      [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  let key = function
    | Operator { principal; key; _ } -> Key.Operator (principal, key)
    | Native { principal; operation; invocation } ->
      Key.Native (principal, operation, invocation)
    | Automatic { cut; _ } -> Key.Automatic cut.operation
  ;;

  let same_key a b = Int.equal (Key.compare (key a) (key b)) 0

  let to_json t =
    let principal id = "principal", P.Id.Principal.to_json id in
    `Object
      (match t with
       | Operator { principal = id; key; force } ->
         [ "kind", `String "operator"
         ; principal id
         ; "key", P.Idempotency_key.to_json key
         ; ("force", if force then `True else `False)
         ]
       | Native { principal = id; operation; invocation } ->
         [ "kind", `String "native"
         ; principal id
         ; "operation", P.Id.Operation.to_json operation
         ; "invocation", P.Id.Invocation.to_json invocation
         ]
       | Automatic { principal = id; cut } ->
         [ "kind", `String "automatic"
         ; principal id
         ; "cut", Recovery_rollover_capacity.to_json cut
         ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    let%bind principal = J.required_as fields "principal" P.Id.Principal.of_json in
    match kind with
    | "operator" ->
      let%bind key = J.required_as fields "key" P.Idempotency_key.of_json in
      let%map force = J.required_as fields "force" J.bool in
      Operator { principal; key; force }
    | "native" ->
      let%bind operation = J.required_as fields "operation" P.Id.Operation.of_json in
      let%map invocation = J.required_as fields "invocation" P.Id.Invocation.of_json in
      Native { principal; operation; invocation }
    | "automatic" ->
      let%map cut = J.required_as fields "cut" Recovery_rollover_capacity.of_json in
      Automatic { principal; cut }
    | _ -> invalid "unknown recovery rollover requester"
  ;;
end

module Intent = struct
  type t =
    { id : P.Id.Operation.t
    ; expected : Expected.t
    ; requester : Requester.t
    ; admitted_at : P.Timestamp.t
    }
  [@@deriving equal, sexp_of]

  let create ~id ~expected ~requester ~admitted_at =
    { id; expected; requester; admitted_at }
  ;;

  let public_fields t =
    [ "id", P.Id.Operation.to_json t.id
    ; "expected", Expected.to_json t.expected
    ; "admitted_at", P.Timestamp.to_json t.admitted_at
    ]
  ;;

  let to_json t = `Object (("requester", Requester.to_json t.requester) :: public_fields t)

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
    let%bind expected = J.required_as fields "expected" Expected.of_json in
    let%bind requester = J.required_as fields "requester" Requester.of_json in
    let%map admitted_at = J.required_as fields "admitted_at" P.Timestamp.of_json in
    create ~id ~expected ~requester ~admitted_at
  ;;
end

module Receipt = struct
  type supersession =
    | Basis_changed
    | Cancelled
    | Checkpoint_required
    | Hook_vetoed
    | Hook_failed
  [@@deriving equal, sexp_of]

  type archive_only_reason =
    | Checkpoint_missing
    | Checkpoint_unavailable
  [@@deriving equal, sexp_of]

  type checkpoint_quality =
    | Checkpointed of Checkpoint.t
    | Archive_only of archive_only_reason
  [@@deriving equal, sexp_of]

  type continuation =
    | Awaiting_after_commit of P.Id.Operation.t
    | Awaiting_settled_foreground of
        { owner : P.Id.Operation.t
        ; predecessor : P.Id.Operation.t
        }
    | Scheduled_foreground of Recovery_rollover_continuation.t
  [@@deriving equal, sexp_of]

  type committed =
    { successor : Scope.t
    ; cut_revision : int64
    ; archive : Chatml_source.Digest.t
    ; checkpoint : checkpoint_quality
    ; continuation : continuation
    }
  [@@deriving equal, sexp_of]

  type terminal =
    | Committed of committed
    | Superseded of supersession
  [@@deriving equal, sexp_of]

  type status =
    | Pending
    | Terminal of terminal
  [@@deriving equal, sexp_of]

  type t =
    { intent : Intent.t
    ; status : status
    ; hooks : Recovery_rollover_hooks.t option
    }
  [@@deriving equal, sexp_of]

  let supersession_name = function
    | Basis_changed -> "basis_changed"
    | Cancelled -> "cancelled"
    | Checkpoint_required -> "checkpoint_required"
    | Hook_vetoed -> "hook_vetoed"
    | Hook_failed -> "hook_failed"
  ;;

  let checkpoint_to_json ~public = function
    | Checkpointed checkpoint ->
      `Object
        ([ "kind", `String "checkpointed" ]
         @ if public then [] else [ "selection", Checkpoint.to_json checkpoint ])
    | Archive_only reason ->
      let reason =
        match reason with
        | Checkpoint_missing -> "checkpoint_missing"
        | Checkpoint_unavailable -> "checkpoint_unavailable"
      in
      `Object [ "kind", `String "archive_only"; "reason", `String reason ]
  ;;

  let checkpoint_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    match kind with
    | "checkpointed" ->
      let%map selection = J.required_as fields "selection" Checkpoint.of_json in
      Checkpointed selection
    | "archive_only" ->
      let%bind reason = J.required_as fields "reason" J.string in
      (match reason with
       | "checkpoint_missing" -> Ok (Archive_only Checkpoint_missing)
       | "checkpoint_unavailable" -> Ok (Archive_only Checkpoint_unavailable)
       | _ -> invalid "unknown archive-only rollover reason")
    | _ -> invalid "unknown rollover checkpoint quality"
  ;;

  let continuation_to_json = function
    | Awaiting_after_commit id ->
      `Object [ "kind", `String "awaiting_after_commit"; "id", P.Id.Operation.to_json id ]
    | Awaiting_settled_foreground { owner; predecessor } ->
      `Object
        [ "kind", `String "awaiting_settled_foreground"
        ; "id", P.Id.Operation.to_json owner
        ; "predecessor", P.Id.Operation.to_json predecessor
        ]
    | Scheduled_foreground continuation ->
      `Object
        [ "kind", `String "scheduled_foreground"
        ; "id", P.Id.Operation.to_json continuation.owner
        ; "schedule", Recovery_rollover_continuation.to_json continuation
        ]
  ;;

  let continuation_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    let%bind owner = J.required_as fields "id" P.Id.Operation.of_json in
    match kind with
    | "awaiting_after_commit" -> Ok (Awaiting_after_commit owner)
    | "awaiting_settled_foreground" ->
      let%map predecessor = J.required_as fields "predecessor" P.Id.Operation.of_json in
      Awaiting_settled_foreground { owner; predecessor }
    | "scheduled_foreground" ->
      let%bind schedule =
        J.required_as fields "schedule" Recovery_rollover_continuation.of_json
      in
      if P.Id.Operation.equal owner schedule.owner
      then Ok (Scheduled_foreground schedule)
      else invalid "rollover schedule owner changed"
    | _ -> invalid "unknown rollover continuation obligation"
  ;;

  let status_to_json ?(public = false) t =
    `Object
      (match t with
       | Pending -> [ "kind", `String "pending" ]
       | Terminal (Superseded reason) ->
         [ "kind", `String "superseded"; "reason", `String (supersession_name reason) ]
       | Terminal
           (Committed { successor; cut_revision; archive; checkpoint; continuation }) ->
         [ "kind", `String "committed"
         ; "successor", Scope.to_json successor
         ; "cut_revision", revision_json cut_revision
         ; "archive", `String (Chatml_source.Digest.to_hex archive)
         ; "checkpoint", checkpoint_to_json ~public checkpoint
         ; "continuation", continuation_to_json continuation
         ])
  ;;

  let status_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    match kind with
    | "pending" -> Ok Pending
    | "superseded" ->
      let%bind reason = J.required_as fields "reason" J.string in
      let%map reason =
        match reason with
        | "basis_changed" -> Ok Basis_changed
        | "cancelled" -> Ok Cancelled
        | "checkpoint_required" -> Ok Checkpoint_required
        | "hook_vetoed" -> Ok Hook_vetoed
        | "hook_failed" -> Ok Hook_failed
        | _ -> invalid "unknown recovery rollover supersession"
      in
      Terminal (Superseded reason)
    | "committed" ->
      let%bind successor = J.required_as fields "successor" Scope.of_json in
      let%bind cut_revision = J.required_as fields "cut_revision" nonnegative in
      let%bind archive = J.required_as fields "archive" J.string in
      let%bind checkpoint = J.required_as fields "checkpoint" checkpoint_of_json in
      let%bind continuation = J.required_as fields "continuation" continuation_of_json in
      let%map archive =
        Chatml_source.Digest.of_hex archive
        |> Result.map_error ~f:(fun _ ->
          P.Error.invalid_request "invalid rollover archive digest")
      in
      Terminal (Committed { successor; cut_revision; archive; checkpoint; continuation })
    | _ -> invalid "unknown recovery rollover status"
  ;;

  let validate_hooks t =
    let module H = Recovery_rollover_hooks in
    let within_initial_scope (claim : H.Claim.t) =
      let scope = t.intent.expected.scope in
      Scope.equal scope claim.window
      && Int64.(claim.revision >= t.intent.expected.revision)
      &&
      match t.intent.requester with
      | Native { operation; _ } -> P.Id.Operation.equal operation claim.predecessor
      | Automatic { cut; _ } -> P.Id.Operation.equal cut.operation claim.predecessor
      | Operator _ -> not (P.Id.Operation.equal t.intent.id claim.predecessor)
    in
    let after_binds_commit (claim : H.Claim.t) (committed : committed) =
      Scope.equal claim.window committed.successor
      && Int64.(claim.revision > committed.cut_revision)
      &&
      match committed.continuation with
      | Awaiting_settled_foreground { predecessor; _ }
      | Scheduled_foreground { predecessor; _ } ->
        P.Id.Operation.equal predecessor claim.predecessor
      | Awaiting_after_commit _ -> false
    in
    let invalid_hooks () = invalid "rollover hook receipt differs from its owning cut" in
    match t.hooks, t.status with
    | ( None
      , ( Pending
        | Terminal
            (Committed _ | Superseded (Basis_changed | Cancelled | Checkpoint_required))
          ) ) -> Ok ()
    | None, Terminal (Superseded (Hook_vetoed | Hook_failed)) -> invalid_hooks ()
    | Some hooks, status ->
      let before = H.before hooks in
      let before_claim = H.claim before in
      let after_claim = H.claim (H.after hooks) in
      if not (Option.exists before_claim ~f:within_initial_scope)
      then invalid_hooks ()
      else (
        match status, before, after_claim with
        | Pending, _, None
        | Terminal (Superseded (Basis_changed | Cancelled | Checkpoint_required)), _, None
        | Terminal (Superseded Hook_vetoed), Completed (_, Veto), None
        | Terminal (Superseded Hook_failed), Stopped _, None -> Ok ()
        | Terminal (Committed committed), Completed (claim, Proceed _), after
          when Int64.(claim.revision < committed.cut_revision)
               && Option.for_all after ~f:(fun claim ->
                 after_binds_commit claim committed) -> Ok ()
        | _ -> invalid_hooks ())
  ;;

  let validate t =
    let open Result.Let_syntax in
    let%bind () = validate_hooks t in
    let expected = t.intent.expected in
    match t.status with
    | Pending | Terminal (Superseded _) -> Ok ()
    | Terminal
        (Committed { successor; cut_revision; checkpoint; continuation; archive = _ }) ->
      let owner =
        match continuation with
        | Awaiting_after_commit owner | Awaiting_settled_foreground { owner; _ } -> owner
        | Scheduled_foreground schedule -> schedule.owner
      in
      let predecessor =
        match continuation with
        | Awaiting_after_commit _ -> None
        | Awaiting_settled_foreground { predecessor; _ } -> Some predecessor
        | Scheduled_foreground schedule -> Some schedule.predecessor
      in
      let continuation_binds_requester =
        match t.intent.requester with
        | Automatic { cut; _ } ->
          Option.exists predecessor ~f:(P.Id.Operation.equal cut.operation)
        | Native { operation; _ } ->
          Option.for_all predecessor ~f:(P.Id.Operation.equal operation)
        | Operator _ ->
          Option.for_all predecessor ~f:(fun id ->
            not (P.Id.Operation.equal id t.intent.id))
      in
      let checkpoint_binds_cut =
        match checkpoint with
        | Checkpointed checkpoint ->
          Scope.equal (Checkpoint.expected checkpoint).scope expected.scope
          && Int64.((Checkpoint.expected checkpoint).revision < cut_revision)
        | Archive_only _ ->
          (match t.intent.requester with
           | Operator { force; _ } -> force
           | Automatic _ -> true
           | Native _ -> false)
      in
      if
        P.Session_ref.equal (Scope.session expected.scope) (Scope.session successor)
        && Int.equal (Scope.generation expected.scope) (Scope.generation successor)
        && (not (Scope.equal expected.scope successor))
        && Int64.(cut_revision > expected.revision)
        && P.Id.Operation.equal owner t.intent.id
        && continuation_binds_requester
        && checkpoint_binds_cut
      then Ok ()
      else invalid "rollover terminal receipt differs from its admitted intent"
  ;;

  let to_public_json t =
    let module H = Recovery_rollover_hooks in
    let phase = function
      | H.Pending -> `String "pending"
      | Claimed _ -> `String "claimed"
      | Completed _ -> `String "completed"
      | Stopped (_, Failed) -> `String "failed"
      | Stopped (_, Authority_lost) -> `String "authority_lost"
      | Stopped (_, Interrupted) -> `String "interrupted"
    in
    let hooks =
      Option.to_list
        (Option.map t.hooks ~f:(fun hooks ->
           ( "hooks"
           , `Object [ "before", phase (H.before hooks); "after", phase (H.after hooks) ]
           )))
    in
    let trigger =
      match t.intent.requester with
      | Automatic { cut; _ } ->
        [ ( "capacity_trigger"
          , `String (Recovery_rollover_capacity.trigger_name cut.trigger) )
        ]
      | Operator _ | Native _ -> []
    in
    `Object
      (("status", status_to_json ~public:true t.status)
       :: (hooks @ trigger @ Intent.public_fields t.intent))
  ;;

  let to_json t =
    `Object
      ([ "intent", Intent.to_json t.intent; "status", status_to_json t.status ]
       @ Option.to_list
           (Option.map t.hooks ~f:(fun hooks ->
              "hooks", Recovery_rollover_hooks.to_json hooks)))
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind intent = J.required_as fields "intent" Intent.of_json in
    let%bind status = J.required_as fields "status" status_of_json in
    let%bind hooks = J.optional_as fields "hooks" Recovery_rollover_hooks.of_json in
    let receipt = { intent; status; hooks } in
    let%map () = validate receipt in
    receipt
  ;;
end

module Commit = struct
  type t =
    { id : P.Id.Operation.t
    ; expected : Expected.t
    ; successor : Scope.t
    ; archive : Chatml_source.Digest.t
    ; checkpoint : Receipt.checkpoint_quality
    ; retain : P.History.Id.t list
    ; batch : Recovery_rollover_batch.t option
    ; capacity : Recovery_rollover_capacity.t option
    ; retired_at : P.Timestamp.t option
    ; schedule_foreground : bool
    }

  let predecessor t =
    match t.batch, t.capacity with
    | Some batch, _ -> Some batch.operation
    | None, Some capacity -> Some capacity.operation
    | None, None -> None
  ;;

  let retirement t ~(active : P.Operation.t option) =
    match t.retired_at, predecessor t, active with
    | None, _, _ -> Ok None
    | ( Some updated_at
      , Some expected
      , Some ({ kind = Turn _; state = Running; _ } as operation) )
      when P.Id.Operation.equal expected operation.id
           && Int.equal operation.generation (Scope.generation t.expected.scope)
           && P.Timestamp.compare updated_at operation.updated_at >= 0 ->
      Ok
        (Some
           { operation with
             state = Interrupted { reason = "context window retired"; retryable = false }
           ; updated_at
           })
    | Some _, _, _ -> conflict "rollover retirement differs from its exact foreground"
  ;;

  let validate_selection t =
    if
      Stdlib.List.compare_length_with t.retain Recovery_window_record.max_members > 0
      || List.contains_dup t.retain ~compare:P.History.Id.compare
    then invalid "invalid rollover carry selection"
    else if Option.is_some t.retired_at && Option.is_none (predecessor t)
    then invalid "rollover retirement requires a foreground cut"
    else if
      Option.is_some t.capacity && (Option.is_some t.batch || not t.schedule_foreground)
    then invalid "automatic rollover requires only its verified capacity cut"
    else if t.schedule_foreground
    then (
      match predecessor t with
      | Some operation when not (P.Id.Operation.equal t.id operation) -> Ok t
      | _ -> invalid "scheduled rollover requires an independent foreground")
    else Ok t
  ;;

  let terminal t =
    Receipt.Committed
      { successor = t.successor
      ; cut_revision = t.expected.revision
      ; archive = t.archive
      ; checkpoint = t.checkpoint
      ; continuation =
          (match predecessor t with
           | None -> Awaiting_after_commit t.id
           | Some predecessor when t.schedule_foreground ->
             let schedule =
               Recovery_rollover_continuation.create ~owner:t.id ~predecessor
               |> Result.map_error ~f:(fun (failure : P.Error.t) -> failure.message)
               |> Result.ok_or_failwith
             in
             Scheduled_foreground schedule
           | Some predecessor -> Awaiting_settled_foreground { owner = t.id; predecessor })
      }
  ;;

  let check_intent t (intent : Intent.t) =
    let capacity_matches_requester =
      match intent.requester, t.capacity with
      | Automatic { cut; _ }, Some verified ->
        Recovery_rollover_capacity.equal cut verified
      | (Operator _ | Native _), None -> true
      | _ -> false
    in
    if
      capacity_matches_requester
      && P.Id.Operation.equal t.id intent.id
      && Scope.equal t.expected.scope intent.expected.scope
      && Int64.(t.expected.revision > intent.expected.revision)
    then Receipt.validate { intent; status = Terminal (terminal t); hooks = None }
    else conflict "rollover publication does not match the admitted intent"
  ;;

  let create
        ?batch
        ?capacity
        ?retired_at
        ?(schedule_foreground = false)
        (intent : Intent.t)
        ~expected
        ~successor
        ~archive
        ~checkpoint
        ~retain
    =
    let open Result.Let_syntax in
    let%bind t =
      validate_selection
        { id = intent.id
        ; expected
        ; successor
        ; archive
        ; checkpoint
        ; retain
        ; batch
        ; capacity
        ; retired_at
        ; schedule_foreground
        }
    in
    let%map () = check_intent t intent in
    t
  ;;

  let to_json t =
    `Object
      ([ "id", P.Id.Operation.to_json t.id
       ; "expected", Expected.to_json t.expected
       ; "successor", Scope.to_json t.successor
       ; "archive", `String (Chatml_source.Digest.to_hex t.archive)
       ; "checkpoint", Receipt.checkpoint_to_json ~public:false t.checkpoint
       ; "retain", `Array (List.map t.retain ~f:P.History.Id.to_json)
       ]
       @ (if t.schedule_foreground then [ "schedule_foreground", `True ] else [])
       @ Option.to_list
           (Option.map t.batch ~f:(fun batch ->
              "batch", Recovery_rollover_batch.to_json batch))
       @ Option.to_list
           (Option.map t.capacity ~f:(fun cut ->
              "capacity", Recovery_rollover_capacity.to_json cut))
       @ Option.to_list
           (Option.map t.retired_at ~f:(fun at -> "retired_at", P.Timestamp.to_json at)))
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
    let%bind expected = J.required_as fields "expected" Expected.of_json in
    let%bind successor = J.required_as fields "successor" Scope.of_json in
    let%bind archive = J.required_as fields "archive" J.string in
    let%bind archive =
      Chatml_source.Digest.of_hex archive
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid rollover archive digest")
    in
    let%bind checkpoint = J.required_as fields "checkpoint" Receipt.checkpoint_of_json in
    let%bind retain = J.required_as fields "retain" (J.list P.History.Id.of_json) in
    let%bind batch = J.optional_as fields "batch" Recovery_rollover_batch.of_json in
    let%bind capacity =
      J.optional_as fields "capacity" Recovery_rollover_capacity.of_json
    in
    let%bind retired_at = J.optional_as fields "retired_at" P.Timestamp.of_json in
    let%bind schedule_foreground = J.optional_as fields "schedule_foreground" J.bool in
    let schedule_foreground = Option.value schedule_foreground ~default:false in
    validate_selection
      { id
      ; expected
      ; successor
      ; archive
      ; checkpoint
      ; retain
      ; batch
      ; capacity
      ; retired_at
      ; schedule_foreground
      }
  ;;
end

module Settlement = struct
  type t =
    | Cancel
    | Publish of
        { checkpoint : Checkpoint.t option
        ; retain : P.History.Id.t list
        }
end

type t =
  { receipts : Receipt.t list
  ; checkpoint : Checkpoint.t option
  ; authored : Checkpoint.t list
  }

let max_authored_per_window = 64

let authored_checkpoint t ~scope ~principal ~key =
  List.find t.authored ~f:(fun candidate ->
    Scope.equal (Checkpoint.expected candidate).scope scope
    &&
    match Checkpoint.recipe candidate with
    | Authored value -> Recovery_authored_checkpoint.same_key value ~principal ~key
    | Owner_selection _ -> false)
;;

let find t ~id =
  List.find t.receipts ~f:(fun receipt ->
    P.Id.Operation.equal receipt.Receipt.intent.id id)
;;

let pending t =
  List.find_map t.receipts ~f:(fun receipt ->
    match receipt.Receipt.status with
    | Pending -> Some receipt.intent
    | Terminal _ -> None)
;;

let checkpoint t = t.checkpoint

let latest t ~scope =
  List.find t.receipts ~f:(fun receipt ->
    Scope.equal receipt.Receipt.intent.expected.scope scope
    ||
    match receipt.status with
    | Terminal (Committed { successor; _ }) -> Scope.equal successor scope
    | Terminal (Superseded _) | Pending -> false)
;;

let find_operator t ~principal ~key =
  List.find t.receipts ~f:(fun receipt ->
    match receipt.Receipt.intent.requester with
    | Operator requester ->
      P.Id.Principal.equal requester.principal principal
      && P.Idempotency_key.equal requester.key key
    | Native _ | Automatic _ -> false)
;;

let foreground t ~scope =
  List.find_map t.receipts ~f:(fun receipt ->
    match receipt.status with
    | Terminal (Committed { successor; continuation; _ }) when Scope.equal successor scope
      ->
      (match continuation with
       | Awaiting_after_commit _ -> None
       | Scheduled_foreground schedule -> Some schedule
       | Awaiting_settled_foreground { owner; predecessor } ->
         Recovery_rollover_continuation.create ~owner ~predecessor |> Result.ok)
    | Pending | Terminal _ -> None)
;;

let awaiting_foreground t ~scope =
  Option.exists (foreground t ~scope) ~f:Recovery_rollover_continuation.is_pending
;;

let automatic_for_operation t ~operation =
  Option.bind t ~f:(fun ledger ->
    List.find ledger.receipts ~f:(fun receipt ->
      match receipt.Receipt.intent.requester with
      | Automatic { cut; _ } -> P.Id.Operation.equal cut.operation operation
      | Native _ | Operator _ -> false))
;;

let previous t ~requester ~expected =
  match
    Option.bind t ~f:(fun t ->
      List.find t.receipts ~f:(fun receipt ->
        Requester.same_key requester receipt.Receipt.intent.requester))
  with
  | None -> Ok None
  | Some receipt
    when Requester.equal requester receipt.intent.requester
         && Expected.equal expected receipt.intent.expected -> Ok (Some receipt)
  | Some _ ->
    Error
      (P.Error.create
         Idempotency_conflict
         ~message:"rollover retry arguments changed"
         ~retryable:false
         ())
;;

module Change = struct
  type t =
    | Admit of Intent.t
    | Record_checkpoint of Checkpoint.t
    | Settle of
        { id : P.Id.Operation.t
        ; terminal : Receipt.terminal
        }
    | Schedule of
        { id : P.Id.Operation.t
        ; expected : Expected.t
        ; change : Recovery_rollover_continuation.change
        }
    | Hook of
        { id : P.Id.Operation.t
        ; expected : Expected.t
        ; change : Recovery_rollover_hooks.change
        }

  let to_json = function
    | Admit intent -> `Object [ "kind", `String "admit"; "intent", Intent.to_json intent ]
    | Record_checkpoint checkpoint ->
      `Object
        [ "kind", `String "record_checkpoint"
        ; "checkpoint", Checkpoint.to_json checkpoint
        ]
    | Settle { id; terminal } ->
      `Object
        [ "kind", `String "settle"
        ; "id", P.Id.Operation.to_json id
        ; "status", Receipt.status_to_json (Terminal terminal)
        ]
    | Schedule { id; expected; change } ->
      `Object
        [ "kind", `String "schedule"
        ; "id", P.Id.Operation.to_json id
        ; "expected", Expected.to_json expected
        ; "change", Recovery_rollover_continuation.change_to_json change
        ]
    | Hook { id; expected; change } ->
      `Object
        [ "kind", `String "hook"
        ; "id", P.Id.Operation.to_json id
        ; "expected", Expected.to_json expected
        ; "change", Recovery_rollover_hooks.change_to_json change
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    match kind with
    | "admit" ->
      let%map intent = J.required_as fields "intent" Intent.of_json in
      Admit intent
    | "record_checkpoint" ->
      let%map checkpoint = J.required_as fields "checkpoint" Checkpoint.of_json in
      Record_checkpoint checkpoint
    | "settle" ->
      let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
      let%bind status = J.required_as fields "status" Receipt.status_of_json in
      (match status with
       | Pending -> invalid "settlement requires a terminal disposition"
       | Terminal terminal -> Ok (Settle { id; terminal }))
    | "schedule" ->
      let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
      let%bind expected = J.required_as fields "expected" Expected.of_json in
      let%map change =
        J.required_as fields "change" Recovery_rollover_continuation.change_of_json
      in
      Schedule { id; expected; change }
    | "hook" ->
      let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
      let%bind expected = J.required_as fields "expected" Expected.of_json in
      let%map change =
        J.required_as fields "change" Recovery_rollover_hooks.change_of_json
      in
      Hook { id; expected; change }
    | _ -> invalid "unknown recovery rollover change"
  ;;
end

let to_json t =
  `Object
    ([ "version", `Number "1"
     ; "receipts", `Array (List.map t.receipts ~f:Receipt.to_json)
     ]
     @ Option.to_list
         (Option.map t.checkpoint ~f:(fun value -> "checkpoint", Checkpoint.to_json value))
     @
     match t.authored with
     | [] -> []
     | authored -> [ "authored", `Array (List.map authored ~f:Checkpoint.to_json) ])
;;

let bounded t =
  D.Json.validate ~limits:D.Limits.default (to_json t)
  |> Result.map_error ~f:(fun _ ->
    P.Error.create
      Resource_limit
      ~message:"recovery rollover ledger capacity exceeded"
      ~retryable:false
      ())
;;

let settle t ~id ~terminal =
  let open Result.Let_syntax in
  match find t ~id with
  | Some ({ status = Pending; _ } as receipt) ->
    let settled = { receipt with status = Terminal terminal } in
    let%bind () = Receipt.validate settled in
    let receipts =
      List.map t.receipts ~f:(fun previous ->
        if P.Id.Operation.equal previous.intent.id id then settled else previous)
    in
    Ok { t with receipts }
  | Some { status = Terminal current; _ } when Receipt.equal_terminal current terminal ->
    Ok t
  | Some _ | None -> conflict "rollover settlement has no matching pending intent"
;;

let abandon_automatic t ~operation =
  match t with
  | Some ledger ->
    (match pending ledger with
     | Some { id; requester = Automatic { cut; _ }; _ }
       when P.Id.Operation.equal cut.operation operation ->
       Result.map (settle ledger ~id ~terminal:(Superseded Cancelled)) ~f:Option.some
     | Some _ | None -> Ok t)
  | None -> Ok None
;;

let replace_receipt t (updated : Receipt.t) =
  { t with
    receipts =
      List.map t.receipts ~f:(fun retained ->
        if P.Id.Operation.equal retained.intent.id updated.intent.id
        then updated
        else retained)
  }
;;

let apply_hook t ~active ~revision ~id ~(expected : Expected.t) change =
  let module H = Recovery_rollover_hooks in
  let open Result.Let_syntax in
  let phase = H.phase change in
  let stale () = conflict "rollover hook cut or phase changed" in
  let%bind receipt =
    match find t ~id with
    | Some ({ status = Pending; _ } as receipt)
      when H.equal_phase phase Before && Scope.equal receipt.intent.expected.scope active
      -> Ok receipt
    | Some ({ status = Terminal (Committed committed); _ } as receipt)
      when H.equal_phase phase After
           && Scope.equal committed.successor active
           &&
           match committed.continuation with
           | Scheduled_foreground { phase = Awaiting; _ } -> true
           | _ -> false -> Ok receipt
    | None | Some _ -> stale ()
  in
  let%bind () =
    if Scope.equal active expected.scope && Int64.equal revision expected.revision
    then (
      match change with
      | H.Claim (_, claim)
        when not (Scope.equal claim.window active && Int64.equal claim.revision revision)
        -> stale ()
      | _ -> Ok ())
    else stale ()
  in
  let%bind hooks = H.apply (Option.value receipt.hooks ~default:H.empty) change in
  let updated = { receipt with hooks = Some hooks } in
  let%map () = Receipt.validate updated in
  replace_receipt t updated
;;

let apply previous_ledger ~active ~revision ~history ~catalog change =
  let open Result.Let_syntax in
  let t =
    Option.value
      previous_ledger
      ~default:{ receipts = []; checkpoint = None; authored = [] }
  in
  let%bind next =
    match change with
    | Change.Admit intent ->
      let%bind duplicate =
        previous previous_ledger ~requester:intent.requester ~expected:intent.expected
      in
      if Option.is_some duplicate || Option.is_some (find t ~id:intent.id)
      then conflict "rollover admission identity already retained"
      else if
        not
          (Scope.equal active intent.expected.scope
           && Int64.equal revision intent.expected.revision)
      then conflict "expected recovery window or revision changed"
      else if Option.is_some (pending t)
      then conflict "another rollover intent is pending"
      else
        Ok
          { t with
            receipts = Receipt.{ intent; status = Pending; hooks = None } :: t.receipts
          }
    | Record_checkpoint checkpoint ->
      if
        Scope.equal active (Checkpoint.expected checkpoint).scope
        && Int64.equal revision (Checkpoint.expected checkpoint).revision
      then (
        let%bind () = Checkpoint.check_contents checkpoint ~history ~catalog in
        match Checkpoint.recipe checkpoint with
        | Owner_selection _ -> Ok { t with checkpoint = Some checkpoint }
        | Authored binding ->
          let authored =
            List.filter t.authored ~f:(fun previous ->
              Scope.equal (Checkpoint.expected previous).scope active)
          in
          if
            Option.is_some
              (authored_checkpoint
                 t
                 ~scope:active
                 ~principal:binding.principal
                 ~key:binding.key)
          then conflict "authored checkpoint identity already retained"
          else if Stdlib.List.compare_length_with authored max_authored_per_window >= 0
          then
            Error
              (P.Error.create
                 Resource_limit
                 ~message:"authored checkpoint receipt capacity exceeded"
                 ~retryable:false
                 ())
          else
            Ok { t with checkpoint = Some checkpoint; authored = checkpoint :: authored })
      else conflict "checkpoint recording cut is stale"
    | Settle { terminal = Committed _; _ } ->
      invalid "committed rollover requires its atomic archive and successor delta"
    | Settle { id; terminal = Superseded _ as terminal } -> settle t ~id ~terminal
    | Hook { id; expected; change } -> apply_hook t ~active ~revision ~id ~expected change
    | Schedule { id; expected; change } ->
      let%bind receipt, committed, schedule =
        match find t ~id, foreground t ~scope:active with
        | Some ({ status = Terminal (Committed committed); _ } as receipt), Some schedule
          when P.Id.Operation.equal schedule.owner id
               && Scope.equal committed.successor active
               && Scope.equal expected.scope active
               && Int64.equal expected.revision revision ->
          Ok (receipt, committed, schedule)
        | _ -> conflict "rollover continuation window or revision changed"
      in
      let%bind () =
        match committed.continuation, change with
        | Awaiting_settled_foreground _, Recovery_rollover_continuation.Finish _ -> Ok ()
        | Awaiting_settled_foreground _, _ ->
          conflict "historical rollover has no retained successor input witness"
        | Scheduled_foreground _, _ -> Ok ()
        | Awaiting_after_commit _, _ -> invalid "rollover has no settled foreground work"
      in
      let%bind () =
        match receipt.hooks with
        | None -> Ok ()
        | Some hooks ->
          (match Recovery_rollover_hooks.after hooks with
           | Pending | Completed _ | Stopped _ -> Ok ()
           | Claimed _ ->
             conflict "rollover continuation awaits its authored after-hook disposition")
      in
      let%bind schedule = Recovery_rollover_continuation.apply schedule change in
      let updated =
        { receipt with
          status =
            Terminal
              (Committed { committed with continuation = Scheduled_foreground schedule })
        }
      in
      let%map () = Receipt.validate updated in
      replace_receipt t updated
  in
  let%map () = bounded next in
  next
;;

let check_checkpoint
      ?checkpoint_history
      ?validate_authored
      t
      selected
      ~expected
      ~history
      ~catalog
  =
  match t.checkpoint with
  | Some checkpoint
    when Checkpoint.equal checkpoint selected
         && Scope.equal (Checkpoint.expected checkpoint).scope expected.Expected.scope
         && Int64.((Checkpoint.expected checkpoint).revision < expected.revision) ->
    (match Checkpoint.recipe checkpoint with
     | Authored _ ->
       (match validate_authored with
        | None ->
          conflict "authored checkpoint requires exact retained foreground evidence"
        | Some validate -> validate checkpoint)
     | Owner_selection _ ->
       (match
          Checkpoint.check_contents checkpoint ~history ~catalog, checkpoint_history
        with
        | Error _, Some history -> Checkpoint.check_contents checkpoint ~history ~catalog
        | result, _ -> result))
  | Some _ | None -> conflict "rollover checkpoint is not recorded for the current cut"
;;

let commit
      ?checkpoint_history
      ?validate_authored
      t
      (change : Commit.t)
      ~active
      ~revision
      ~history
      ~catalog
  =
  let open Result.Let_syntax in
  let%bind intent =
    Result.of_option
      (pending t)
      ~error:
        (P.Error.invalid_request "rollover publication has no matching pending intent")
  in
  let%bind () = Commit.check_intent change intent in
  let%bind () =
    if
      Scope.equal active change.expected.scope
      && Int64.equal revision change.expected.revision
    then Ok ()
    else conflict "rollover publication cut is stale"
  in
  let%bind () =
    match change.checkpoint with
    | Checkpointed selected ->
      check_checkpoint
        ?checkpoint_history
        ?validate_authored
        t
        selected
        ~expected:change.expected
        ~history
        ~catalog
    | Archive_only _ -> Ok ()
  in
  let%bind t = settle t ~id:change.id ~terminal:(Commit.terminal change) in
  let next = { t with checkpoint = None; authored = [] } in
  let%map () = bounded next in
  next
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate ~limits:D.Limits.default json
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "invalid recovery rollover ledger bounds")
  in
  let%bind fields = J.fields json in
  let%bind version = J.required_as fields "version" (J.bounded_int ~min:1 ~max:1) in
  if not (Int.equal version 1)
  then invalid "unsupported recovery rollover ledger"
  else (
    let%bind receipts = J.required_as fields "receipts" (J.list Receipt.of_json) in
    let%bind checkpoint = J.optional_as fields "checkpoint" Checkpoint.of_json in
    let%bind authored =
      J.optional_as fields "authored" (function
        | `Array values
          when Stdlib.List.compare_length_with values max_authored_per_window <= 0 ->
          List.map values ~f:Checkpoint.of_json |> Result.all
        | _ -> invalid "invalid authored checkpoint receipts")
    in
    let authored = Option.value authored ~default:[] in
    let%bind _ =
      let scope =
        Option.map (List.hd authored) ~f:(fun value -> (Checkpoint.expected value).scope)
      in
      List.fold_result authored ~init:[] ~f:(fun seen candidate ->
        match Checkpoint.recipe candidate with
        | Authored binding
          when Option.exists scope ~f:(Scope.equal (Checkpoint.expected candidate).scope)
               && not
                    (List.exists seen ~f:(fun prior ->
                       Recovery_authored_checkpoint.same_key
                         prior
                         ~principal:binding.principal
                         ~key:binding.key)) -> Ok (binding :: seen)
        | Owner_selection _ | Authored _ ->
          invalid "duplicate or foreign authored checkpoint")
    in
    let%bind () =
      match checkpoint with
      | Some value ->
        (match Checkpoint.recipe value with
         | Authored _ when not (List.mem authored value ~equal:Checkpoint.equal) ->
           invalid "active authored checkpoint has no receipt"
         | Authored _ | Owner_selection _ -> Ok ())
      | None -> Ok ()
    in
    let ids = Hash_set.create (module P.Id.Operation) in
    let%map _ =
      List.fold_result
        receipts
        ~init:(false, Set.empty (module Requester.Key))
        ~f:(fun (active, requesters) (receipt : Receipt.t) ->
          let pending = Receipt.equal_status receipt.status Pending in
          let key = Requester.key receipt.intent.requester in
          if
            Hash_set.mem ids receipt.intent.id
            || (active && pending)
            || Set.mem requesters key
          then invalid "duplicate recovery rollover identity or pending intent"
          else (
            Hash_set.add ids receipt.intent.id;
            Ok (active || pending, Set.add requesters key)))
    in
    { receipts; checkpoint; authored })
;;

let validate_state t ~windows ~revision ~has_archive =
  let open Result.Let_syntax in
  let%bind ledger =
    Result.of_option
      windows
      ~error:(P.Error.invalid_request "rollover ledger requires recovery windows")
  in
  let owner = Recovery_window_ledger.owner ledger in
  let windows = Recovery_window_ledger.windows ledger in
  let covers_existing_window (expected : Expected.t) =
    P.Session_ref.equal owner (Scope.session expected.scope)
    && Int64.(expected.revision < revision)
    && List.exists windows ~f:(fun window ->
      Scope.equal (Recovery_window_record.scope window) expected.scope)
  in
  let%bind () =
    match t.checkpoint with
    | None -> Ok ()
    | Some checkpoint when covers_existing_window (Checkpoint.expected checkpoint) ->
      Ok ()
    | Some _ -> invalid "rollover checkpoint has no owning recovery window"
  in
  let%bind () =
    if
      List.for_all t.authored ~f:(fun checkpoint ->
        covers_existing_window (Checkpoint.expected checkpoint))
    then Ok ()
    else invalid "authored checkpoint receipt has no owning recovery window"
  in
  List.fold_result t.receipts ~init:() ~f:(fun () (receipt : Receipt.t) ->
    let expected = receipt.intent.expected in
    let hook_revisions_are_committed =
      Option.for_all receipt.hooks ~f:(fun hooks ->
        let module H = Recovery_rollover_hooks in
        let committed (claim : H.Claim.t) = Int64.(claim.revision < revision) in
        Option.for_all (H.claim (H.before hooks)) ~f:committed
        && Option.for_all (H.claim (H.after hooks)) ~f:committed)
    in
    if not (covers_existing_window expected && hook_revisions_are_committed)
    then invalid "rollover receipt has no owning recovery window"
    else (
      match receipt.status with
      | Pending | Terminal (Superseded _) -> Ok ()
      | Terminal (Committed { successor; cut_revision; archive; _ }) ->
        if
          Int64.(cut_revision < revision)
          && has_archive ~id:receipt.intent.id ~revision:cut_revision archive
          && List.exists windows ~f:(fun window ->
            Scope.equal (Recovery_window_record.scope window) successor
            &&
            match Recovery_window_record.origin window with
            | Successor { previous; operation } ->
              P.Id.Context_window.equal previous (Scope.window expected.scope)
              && P.Id.Operation.equal operation receipt.intent.id
            | Root | New_generation _ -> false)
        then Ok ()
        else invalid "rollover terminal receipt has no matching successor"))
;;

let check_unchanged t ~incoming =
  let equal left right =
    List.equal Receipt.equal left.receipts right.receipts
    && Option.equal Checkpoint.equal left.checkpoint right.checkpoint
    && List.equal Checkpoint.equal left.authored right.authored
  in
  if Option.equal equal t incoming
  then Ok ()
  else invalid "replacement changed durable rollover receipts"
;;
