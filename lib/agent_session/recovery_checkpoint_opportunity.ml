open! Core
module P = Agent_protocol
module Catalog = Recovery_notes.Catalog
module Expected = Recovery_rollover_checkpoint.Expected
module Attempt_id = Inference.Observation.Observation_id
module Scope = P.Recovery_window.Scope
module J = P.Json_codec
module Digest = Chatml_source.Digest

let invalid message = Error (P.Error.invalid_request message)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let number n = `Number (Int64.to_string n)
let nonnegative = J.bounded_int64 ~min:0L ~max:Int64.max_value

let identity value =
  Result.map_error (Attempt_id.of_string value) ~f:(fun _ ->
    P.Error.invalid_request "invalid checkpoint opportunity preparation identity")
;;

let identity_of_json value = Result.bind (J.string value) ~f:identity
let identity_to_json value = `String (Attempt_id.to_string value)

type unavailable =
  | No_recipe
  | Unproven_fit
  | Unproven_source
  | Ineligible_response
  | Checkpoint_refused
[@@deriving equal, sexp_of]

let unavailable_to_json = function
  | No_recipe -> `String "no_recipe"
  | Unproven_fit -> `String "unproven_fit"
  | Unproven_source -> `String "unproven_source"
  | Ineligible_response -> `String "ineligible_response"
  | Checkpoint_refused -> `String "checkpoint_refused"
;;

let unavailable_of_json = function
  | `String "no_recipe" -> Ok No_recipe
  | `String "unproven_fit" -> Ok Unproven_fit
  | `String "unproven_source" -> Ok Unproven_source
  | `String "ineligible_response" -> Ok Ineligible_response
  | `String "checkpoint_refused" -> Ok Checkpoint_refused
  | _ -> invalid "unknown checkpoint opportunity refusal"
;;

type recipe =
  | Register
  | Create_note of
      { catalog : Catalog.Catalog_revision.t
      ; path : Catalog.Note_path.t
      }
[@@deriving equal, sexp_of]

type decision =
  | Offer of recipe
  | Withhold of unavailable
[@@deriving equal, sexp_of]

let decision_to_json = function
  | Offer Register -> `Object [ "kind", `String "register" ]
  | Offer (Create_note { catalog; path }) ->
    `Object
      [ "kind", `String "create_note"
      ; "catalog", number (Catalog.Catalog_revision.to_int64 catalog)
      ; "path", `String (Catalog.Note_path.to_string path)
      ]
  | Withhold reason ->
    `Object [ "kind", `String "withhold"; "reason", unavailable_to_json reason ]
;;

let decision_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  match kind with
  | "register" -> Ok (Offer Register)
  | "create_note" ->
    let%bind revision = J.required_as fields "catalog" nonnegative in
    let%bind catalog = Catalog.Catalog_revision.of_int64 revision in
    let%bind path = J.required_as fields "path" J.string in
    let%map path = Catalog.Note_path.of_string path in
    Offer (Create_note { catalog; path })
  | "withhold" ->
    let%map reason = J.required_as fields "reason" unavailable_of_json in
    Withhold reason
  | _ -> invalid "unknown checkpoint opportunity decision"
;;

module Source = struct
  type t =
    { rows : int
    ; digest : Digest.t
    }
  [@@deriving equal, sexp_of]

  let capture history =
    let%map.Result digest = Recovery_rollover_checkpoint.history_digest history in
    { rows = List.length history; digest }
  ;;

  let check t history ~own_call =
    let extra = if Option.is_some own_call then 1 else 0 in
    if Stdlib.List.compare_length_with history (t.rows + extra) <> 0
    then conflict "checkpoint source has intervening canonical history"
    else (
      let prefix, suffix = List.split_n history t.rows in
      let open Result.Let_syntax in
      let%bind () =
        match own_call, suffix with
        | None, [] -> Ok ()
        | Some call, [ (entry : P.History.entry) ] when P.History.Id.equal call entry.id
          -> Ok ()
        | _ -> conflict "checkpoint source does not end at its original call"
      in
      let%bind digest = Recovery_rollover_checkpoint.history_digest prefix in
      if Digest.equal t.digest digest
      then Ok ()
      else conflict "checkpoint source changed after preparation")
  ;;

  let to_json t =
    `Object
      [ "rows", `Number (Int.to_string t.rows)
      ; "digest", `String (Digest.to_hex t.digest)
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind rows =
      J.required_as
        fields
        "rows"
        (J.bounded_int ~min:0 ~max:Recovery_window_record.max_members)
    in
    let%bind digest = J.required_as fields "digest" J.string in
    let%map digest =
      Digest.of_hex digest
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid checkpoint source digest")
    in
    { rows; digest }
  ;;
end

module Claim = struct
  type t =
    { id : P.Id.Operation.t
    ; expected : Expected.t
    ; owner : P.Id.Operation.t
    ; principal : P.Id.Principal.t
    ; configuration_revision : int64
    ; preparation : Attempt_id.t
    ; policy : Chatml_source.Digest.t
    ; decision : decision
    ; source : Source.t option
    }
  [@@deriving equal, sexp_of]

  let create
        ~id
        expected
        ~owner
        ~principal
        ~configuration_revision
        ~preparation
        ~policy
        ~decision
        ~source
    =
    let has_source =
      match decision, source with
      | Offer _, Some _ | Withhold _, None -> true
      | _ -> false
    in
    if
      Int64.(configuration_revision < 0L)
      || P.Id.Operation.equal id owner
      || not has_source
    then invalid "invalid checkpoint opportunity identity"
    else (
      let%map.Result preparation = identity preparation in
      { id
      ; expected
      ; owner
      ; principal
      ; configuration_revision
      ; preparation
      ; policy
      ; decision
      ; source
      })
  ;;

  let to_json t =
    `Object
      ([ "id", P.Id.Operation.to_json t.id
       ; "expected", Expected.to_json t.expected
       ; "owner", P.Id.Operation.to_json t.owner
       ; "principal", P.Id.Principal.to_json t.principal
       ; "configuration_revision", number t.configuration_revision
       ; "preparation", identity_to_json t.preparation
       ; "policy", `String (Digest.to_hex t.policy)
       ; "decision", decision_to_json t.decision
       ]
       @ List.map (Option.to_list t.source) ~f:(fun source ->
         "source", Source.to_json source))
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
    let%bind expected = J.required_as fields "expected" Expected.of_json in
    let%bind owner = J.required_as fields "owner" P.Id.Operation.of_json in
    let%bind principal = J.required_as fields "principal" P.Id.Principal.of_json in
    let%bind configuration_revision =
      J.required_as fields "configuration_revision" nonnegative
    in
    let%bind preparation = J.required_as fields "preparation" J.string in
    let%bind policy = J.required_as fields "policy" J.string in
    let%bind policy =
      Digest.of_hex policy
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid checkpoint opportunity policy identity")
    in
    let%bind decision = J.required_as fields "decision" decision_of_json in
    let%bind source = J.optional_as fields "source" Source.of_json in
    create
      ~id
      expected
      ~owner
      ~principal
      ~configuration_revision
      ~preparation
      ~policy
      ~decision
      ~source
  ;;
end

type outcome =
  | Checkpointed of
      { invocation : P.Id.Invocation.t
      ; attempt_ordinal : int64
      }
  | Unavailable of unavailable
  | Interrupted
[@@deriving equal, sexp_of]

let outcome_to_json = function
  | Checkpointed { invocation; attempt_ordinal } ->
    `Object
      [ "kind", `String "checkpointed"
      ; "invocation", P.Id.Invocation.to_json invocation
      ; "attempt_ordinal", number attempt_ordinal
      ]
  | Unavailable reason ->
    `Object [ "kind", `String "unavailable"; "reason", unavailable_to_json reason ]
  | Interrupted -> `Object [ "kind", `String "interrupted" ]
;;

let outcome_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  match kind with
  | "checkpointed" ->
    let%bind invocation = J.required_as fields "invocation" P.Id.Invocation.of_json in
    let%map attempt_ordinal =
      J.required_as
        fields
        "attempt_ordinal"
        (J.bounded_int64 ~min:1L ~max:Int64.max_value)
    in
    Checkpointed { invocation; attempt_ordinal }
  | "unavailable" ->
    let%map reason = J.required_as fields "reason" unavailable_of_json in
    Unavailable reason
  | "interrupted" -> Ok Interrupted
  | _ -> invalid "unknown checkpoint opportunity outcome"
;;

type change =
  | Admit of Claim.t
  | Accept of
      { id : P.Id.Operation.t
      ; expected : Expected.t
      ; attempt : Attempt_id.t
      }
  | Settle of
      { id : P.Id.Operation.t
      ; expected : Expected.t
      ; outcome : outcome
      }

type t =
  { claim : Claim.t
  ; attempt : Attempt_id.t option
  ; outcome : outcome option
  }
[@@deriving equal, sexp_of]

let claim t = t.claim
let attempt t = t.attempt
let outcome t = t.outcome

let apply previous ~active ~revision change =
  let current (expected : Expected.t) =
    Scope.equal active expected.scope && Int64.equal revision expected.revision
  in
  let pending id expected =
    match previous with
    | Some ({ claim; outcome = None; _ } as t)
      when current expected
           && Scope.equal active claim.expected.scope
           && P.Id.Operation.equal id claim.id
           && Int64.(claim.expected.revision < revision) -> Ok t
    | Some _ | None -> conflict "checkpoint opportunity settlement is stale or complete"
  in
  match change with
  | Admit claim ->
    if
      (not (current claim.expected))
      || Option.exists previous ~f:(fun t -> Scope.equal active t.claim.expected.scope)
    then conflict "checkpoint opportunity is stale or already used"
    else (
      let outcome =
        match claim.decision with
        | Offer _ -> None
        | Withhold reason -> Some (Unavailable reason)
      in
      Ok { claim; attempt = None; outcome })
  | Accept { id; expected; attempt } ->
    let%bind.Result t = pending id expected in
    if Option.is_none t.attempt
    then Ok { t with attempt = Some attempt }
    else conflict "checkpoint opportunity already has an accepted attempt"
  | Settle { id; expected; outcome } ->
    let%bind.Result t = pending id expected in
    (match outcome, t.attempt with
     | Checkpointed { attempt_ordinal; _ }, _ when Int64.(attempt_ordinal <= 0L) ->
       invalid "invalid checkpoint attempt ordinal"
     | Checkpointed _, None -> invalid "checkpoint requires an accepted attempt"
     | (Checkpointed _ | Unavailable _ | Interrupted), _ ->
       Ok { t with outcome = Some outcome })
;;

let change_to_json = function
  | Admit claim -> `Object [ "kind", `String "admit"; "claim", Claim.to_json claim ]
  | Accept { id; expected; attempt } ->
    `Object
      [ "kind", `String "accept"
      ; "id", P.Id.Operation.to_json id
      ; "expected", Expected.to_json expected
      ; "attempt", identity_to_json attempt
      ]
  | Settle { id; expected; outcome } ->
    `Object
      [ "kind", `String "settle"
      ; "id", P.Id.Operation.to_json id
      ; "expected", Expected.to_json expected
      ; "outcome", outcome_to_json outcome
      ]
;;

let change_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  match kind with
  | "admit" ->
    let%map claim = J.required_as fields "claim" Claim.of_json in
    Admit claim
  | "accept" ->
    let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
    let%bind expected = J.required_as fields "expected" Expected.of_json in
    let%map attempt = J.required_as fields "attempt" identity_of_json in
    Accept { id; expected; attempt }
  | "settle" ->
    let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
    let%bind expected = J.required_as fields "expected" Expected.of_json in
    let%map outcome = J.required_as fields "outcome" outcome_of_json in
    Settle { id; expected; outcome }
  | _ -> invalid "unknown checkpoint opportunity change"
;;

let to_json t =
  `Object
    ([ "claim", Claim.to_json t.claim ]
     @ Option.to_list
         (Option.map t.attempt ~f:(fun value -> "attempt", identity_to_json value))
     @ Option.to_list
         (Option.map t.outcome ~f:(fun value -> "outcome", outcome_to_json value)))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind claim = J.required_as fields "claim" Claim.of_json in
  let%bind attempt = J.optional_as fields "attempt" identity_of_json in
  let%bind outcome = J.optional_as fields "outcome" outcome_of_json in
  match claim.decision, attempt, outcome with
  | Withhold expected, None, Some (Unavailable actual)
    when equal_unavailable expected actual -> Ok { claim; attempt; outcome }
  | Offer _, None, Some (Checkpointed _) ->
    invalid "checkpoint requires an accepted attempt"
  | Offer _, _, _ -> Ok { claim; attempt; outcome }
  | Withhold _, _, _ -> invalid "withheld checkpoint opportunity has an invalid outcome"
;;

let%test_unit "strict replay of a lost offer acknowledgement never reopens its attempt" =
  let ok = function
    | Ok value -> value
    | Error _ -> failwith "invalid checkpoint opportunity fixture"
  in
  let scope =
    Scope.create
      ~session:
        (P.Session_ref.create
           ~server_id:(P.Id.Server.of_string "srv_checkpoint" |> ok)
           ~session_id:(P.Id.Session.of_string "ses_checkpoint" |> ok))
      ~generation:0
      ~window:(P.Id.Context_window.of_string "win_checkpoint" |> ok)
    |> ok
  in
  let owner = P.Id.Operation.of_string "op_foreground" |> ok in
  let expected revision = Expected.create scope ~revision |> ok in
  let make ~revision ~decision =
    Claim.create
      ~id:(P.Id.Operation.of_string (sprintf "op_checkpoint_%Ld" revision) |> ok)
      (expected revision)
      ~owner
      ~principal:(P.Id.Principal.of_string "pri_checkpoint" |> ok)
      ~configuration_revision:4L
      ~preparation:"checkpoint-fixture"
      ~policy:(Digest.of_contents "fixture-policy" ~checkpoint:ignore)
      ~decision
      ~source:
        (match decision with
         | Offer _ -> Some (Source.capture [] |> ok)
         | Withhold _ -> None)
    |> ok
  in
  let claim = make ~revision:10L ~decision:(Offer Register) in
  let reduce previous revision change =
    let change = change_of_json (change_to_json change) |> ok in
    apply previous ~active:scope ~revision change
  in
  let offered = reduce None 10L (Admit claim) |> ok in
  let attempt = identity "checkpoint-attempt" |> ok in
  let accepted =
    reduce (Some offered) 11L (Accept { id = claim.id; expected = expected 11L; attempt })
    |> ok
  in
  let reopened = of_json (to_json accepted) |> ok in
  assert (equal accepted reopened);
  assert (Option.is_none (outcome reopened));
  assert (
    Result.is_error
      (reduce (Some reopened) 12L (Admit (make ~revision:12L ~decision:(Offer Register)))));
  assert (
    Result.is_error
      (reduce
         (Some reopened)
         12L
         (Accept { id = claim.id; expected = expected 12L; attempt })));
  let settled =
    reduce
      (Some reopened)
      12L
      (Settle { id = claim.id; expected = expected 12L; outcome = Interrupted })
    |> ok
  in
  assert (Option.equal equal_outcome (outcome settled) (Some Interrupted))
;;

let%test_unit
    "checkpoint offers retain the exact canonical source and admit only their call"
  =
  let ok = function
    | Ok value -> value
    | Error _ -> failwith "invalid checkpoint source fixture"
  in
  let entry id ~role ~kind body : P.History.entry =
    { id = P.History.Id.of_string id |> ok
    ; content_revision = P.History.Content_revision.zero
    ; role
    ; kind
    ; payload = `String body
    ; provenance = Canonical
    ; redacted = false
    }
  in
  let row = entry "10:checkpoint:1" ~role:User ~kind:Message "source" in
  let call = entry "10:checkpoint:2" ~role:Assistant ~kind:Tool_call "{}" in
  let source = Source.capture [ row ] |> ok in
  let edited = { row with payload = `String "edited" } in
  let own_call = Some call.id in
  assert (Result.is_ok (Source.check source [ row ] ~own_call:None));
  assert (Result.is_ok (Source.check source [ row; call ] ~own_call));
  assert (Result.is_error (Source.check source [ edited; call ] ~own_call));
  assert (Result.is_error (Source.check source [ row; edited; call ] ~own_call));
  assert (Result.is_error (Source.check source [ row; call ] ~own_call:(Some row.id)))
;;
