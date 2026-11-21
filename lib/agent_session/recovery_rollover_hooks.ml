open! Core
module P = Agent_protocol
module J = P.Json_codec
module Scope = P.Recovery_window.Scope
module Hint_selection = Recovery_rollover_hint_ref.Selection

let invalid message = Error (P.Error.invalid_request message)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let text value = `String value
let number n = `Number (Int64.to_string n)
let nullable encode value = Option.value_map value ~default:`Null ~f:encode

let optional decode = function
  | `Null -> Ok None
  | value -> Result.map (decode value) ~f:Option.some
;;

let nonnegative = J.bounded_int64 ~min:0L ~max:Int64.max_value

type phase =
  | Before
  | After
[@@deriving equal]

let phase_to_json = function
  | Before -> text "before"
  | After -> text "after"
;;

let phase_of_json = function
  | `String "before" -> Ok Before
  | `String "after" -> Ok After
  | _ -> invalid "invalid rollover hook phase"
;;

module Claim = struct
  type t =
    { execution : P.Id.Moderator_execution.t
    ; predecessor : P.Id.Operation.t
    ; window : Scope.t
    ; revision : int64
    ; observer : P.Invocation.observer
    }
  [@@deriving equal]

  let create ~execution ~predecessor ~window ~revision ~(observer : P.Invocation.observer)
    =
    if
      Int64.(revision >= 0L)
      && String.length observer.script_id > 0
      && String.length observer.script_id <= 256
      && Int.equal (String.length observer.source_sha256) 64
      && Result.is_ok (Chatml_source.Digest.of_hex observer.source_sha256)
    then Ok { execution; predecessor; window; revision; observer }
    else invalid "invalid rollover hook source or revision"
  ;;

  let to_json t =
    `Object
      [ "execution", P.Id.Moderator_execution.to_json t.execution
      ; "predecessor", P.Id.Operation.to_json t.predecessor
      ; "window", Scope.to_json t.window
      ; "revision", number t.revision
      ; "script", text t.observer.script_id
      ; "source", text t.observer.source_sha256
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind execution =
      J.required_as fields "execution" P.Id.Moderator_execution.of_json
    in
    let%bind predecessor = J.required_as fields "predecessor" P.Id.Operation.of_json in
    let%bind window = J.required_as fields "window" Scope.of_json in
    let%bind revision = J.required_as fields "revision" nonnegative in
    let%bind script_id = J.required_as fields "script" J.string in
    let%bind source_sha256 = J.required_as fields "source" J.string in
    create
      ~execution
      ~predecessor
      ~window
      ~revision
      ~observer:{ script_id; source_sha256 }
  ;;
end

type continuation = Chat_response.Context_rollover_hook.continuation =
  | Continue
  | Wait
  | Finish
[@@deriving equal]

type before =
  | Veto
  | Proceed of
      { hints : Hint_selection.t option
      ; continuation : continuation option
      }

type failure =
  | Failed
  | Authority_lost
  | Interrupted
[@@deriving equal]

type 'a disposition =
  | Pending
  | Claimed of Claim.t
  | Completed of Claim.t * 'a
  | Stopped of Claim.t * failure

type change =
  | Claim of phase * Claim.t
  | Complete_before of P.Id.Moderator_execution.t * before
  | Complete_after of P.Id.Moderator_execution.t * continuation
  | Stop of phase * P.Id.Moderator_execution.t * failure

type t =
  { before : before disposition
  ; after : continuation disposition
  }

let claim = function
  | Pending -> None
  | Claimed claim | Completed (claim, _) | Stopped (claim, _) -> Some claim
;;

let phase = function
  | Claim (phase, _) | Stop (phase, _, _) -> phase
  | Complete_before _ -> Before
  | Complete_after _ -> After
;;

let empty = { before = Pending; after = Pending }
let before t = t.before
let after t = t.after

let same_claim (claim : Claim.t) execution =
  P.Id.Moderator_execution.equal claim.execution execution
;;

let apply t change =
  match t.before, t.after, change with
  | Pending, Pending, Claim (Before, claim) -> Ok { t with before = Claimed claim }
  | Claimed claim, Pending, Complete_before (id, decision) when same_claim claim id ->
    Ok { t with before = Completed (claim, decision) }
  | Claimed claim, Pending, Stop (Before, id, failure) when same_claim claim id ->
    Ok { t with before = Stopped (claim, failure) }
  | Completed (previous, Proceed _), Pending, Claim (After, claim)
    when P.Invocation.equal_observer previous.observer claim.observer
         && P.Id.Operation.equal previous.predecessor claim.predecessor
         && P.Session_ref.equal
              (Scope.session previous.window)
              (Scope.session claim.window)
         && Int.equal (Scope.generation previous.window) (Scope.generation claim.window)
         && (not (Scope.equal previous.window claim.window))
         && Int64.(previous.revision < claim.revision)
         && not (same_claim previous claim.execution) ->
    Ok { t with after = Claimed claim }
  | _, Claimed claim, Complete_after (id, selected) when same_claim claim id ->
    Ok { t with after = Completed (claim, selected) }
  | _, Claimed claim, Stop (After, id, failure) when same_claim claim id ->
    Ok { t with after = Stopped (claim, failure) }
  | _, _, _ -> conflict "rollover hook phase or claim already changed"
;;

let continuation_to_json = function
  | Continue -> text "continue"
  | Wait -> text "wait"
  | Finish -> text "finish"
;;

let continuation_of_json = function
  | `String "continue" -> Ok Continue
  | `String "wait" -> Ok Wait
  | `String "finish" -> Ok Finish
  | _ -> invalid "invalid rollover hook continuation"
;;

let before_to_json = function
  | Veto -> `Object [ "kind", text "veto" ]
  | Proceed { hints; continuation } ->
    `Object
      [ "kind", text "proceed"
      ; "hints", nullable Hint_selection.to_json hints
      ; "continuation", nullable continuation_to_json continuation
      ]
;;

let before_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  match%bind J.required_as fields "kind" J.string with
  | "veto" -> Ok Veto
  | "proceed" ->
    let%bind hints = J.required_as fields "hints" (optional Hint_selection.of_json) in
    let%map continuation =
      J.required_as fields "continuation" (optional continuation_of_json)
    in
    Proceed { hints; continuation }
  | _ -> invalid "invalid before-rollover decision"
;;

let stop_to_json = function
  | Failed -> text "failed"
  | Authority_lost -> text "authority_lost"
  | Interrupted -> text "interrupted"
;;

let stop_of_json = function
  | `String "failed" -> Ok Failed
  | `String "authority_lost" -> Ok Authority_lost
  | `String "interrupted" -> Ok Interrupted
  | _ -> invalid "invalid rollover hook stop reason"
;;

let change_to_json change =
  let data =
    match change with
    | Claim (phase, claim) ->
      [ "kind", text "claim"; "phase", phase_to_json phase; "claim", Claim.to_json claim ]
    | Complete_before (id, decision) ->
      [ "kind", text "complete_before"
      ; "execution", P.Id.Moderator_execution.to_json id
      ; "decision", before_to_json decision
      ]
    | Complete_after (id, decision) ->
      [ "kind", text "complete_after"
      ; "execution", P.Id.Moderator_execution.to_json id
      ; "decision", continuation_to_json decision
      ]
    | Stop (phase, id, reason) ->
      [ "kind", text "stop"
      ; "phase", phase_to_json phase
      ; "execution", P.Id.Moderator_execution.to_json id
      ; "reason", stop_to_json reason
      ]
  in
  `Object data
;;

let change_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let phase () = J.required_as fields "phase" phase_of_json in
  let execution () = J.required_as fields "execution" P.Id.Moderator_execution.of_json in
  match%bind J.required_as fields "kind" J.string with
  | "claim" ->
    let%bind phase = phase () in
    let%map claim = J.required_as fields "claim" Claim.of_json in
    Claim (phase, claim)
  | "complete_before" ->
    let%bind id = execution () in
    let%map decision = J.required_as fields "decision" before_of_json in
    Complete_before (id, decision)
  | "complete_after" ->
    let%bind id = execution () in
    let%map decision = J.required_as fields "decision" continuation_of_json in
    Complete_after (id, decision)
  | "stop" ->
    let%bind phase = phase () in
    let%bind id = execution () in
    let%map reason = J.required_as fields "reason" stop_of_json in
    Stop (phase, id, reason)
  | _ -> invalid "invalid rollover hook change"
;;

let to_json t =
  let before =
    match t.before with
    | Pending -> []
    | Claimed claim -> [ Claim (Before, claim) ]
    | Completed (claim, decision) ->
      [ Claim (Before, claim); Complete_before (claim.execution, decision) ]
    | Stopped (claim, reason) ->
      [ Claim (Before, claim); Stop (Before, claim.execution, reason) ]
  in
  let after =
    match t.after with
    | Pending -> []
    | Claimed claim -> [ Claim (After, claim) ]
    | Completed (claim, decision) ->
      [ Claim (After, claim); Complete_after (claim.execution, decision) ]
    | Stopped (claim, reason) ->
      [ Claim (After, claim); Stop (After, claim.execution, reason) ]
  in
  `Array (List.map (before @ after) ~f:change_to_json)
;;

let of_json = function
  | `Array entries when Stdlib.List.compare_length_with entries 4 <= 0 ->
    List.fold_result entries ~init:empty ~f:(fun t json ->
      let%bind.Result change = change_of_json json in
      apply t change)
  | _ -> invalid "invalid retained rollover hook receipts"
;;

let equal_before left right =
  match left, right with
  | Veto, Veto -> true
  | Proceed left, Proceed right ->
    Option.equal Hint_selection.equal left.hints right.hints
    && Option.equal equal_continuation left.continuation right.continuation
  | (Veto | Proceed _), _ -> false
;;

let equal_disposition equal_value left right =
  match left, right with
  | Pending, Pending -> true
  | Claimed left, Claimed right -> Claim.equal left right
  | Completed (left, a), Completed (right, b) -> Claim.equal left right && equal_value a b
  | Stopped (left, a), Stopped (right, b) -> Claim.equal left right && equal_failure a b
  | (Pending | Claimed _ | Completed _ | Stopped _), _ -> false
;;

let equal left right =
  equal_disposition equal_before left.before right.before
  && equal_disposition equal_continuation left.after right.after
;;

let sexp_of_t _ = Sexp.Atom "<rollover hooks>"
