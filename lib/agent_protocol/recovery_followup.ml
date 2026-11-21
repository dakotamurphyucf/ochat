open! Core
module Basis = Recovery_rollover_control.Basis
module Scope = Recovery_window.Scope
module J = Json_codec
module W = Recovery_wire

let feature = "recovery.followup_control"
let method_name = "session.recovery.followup"
let limits = W.limits ~max_bytes:8192 ~max_fields:100 ~max_nodes:200

type action =
  | Continue
  | Wait
  | Finish
  | Cancel

type selection =
  | Inspect of Session_ref.t
  | Apply of Basis.t * action

type t =
  { attachment_id : Id.Attachment.t
  ; selection : selection
  }

let session t =
  match t.selection with
  | Inspect session -> session
  | Apply (expected, _) -> Scope.session expected.scope
;;

let action_name = function
  | Continue -> "continue"
  | Wait -> "wait"
  | Finish -> "finish"
  | Cancel -> "cancel"
;;

let to_json t =
  let fields =
    match t.selection with
    | Inspect session ->
      [ "action", `String "inspect"; "session", Session_ref.to_json session ]
    | Apply (expected, action) ->
      [ "action", `String (action_name action); "expected", Basis.to_json expected ]
  in
  `Object (("attachment_id", Id.Attachment.to_json t.attachment_id) :: fields)
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind _ = W.measure ~limits json in
  let%bind fields = W.fields json [ "action"; "session"; "expected"; "attachment_id" ] in
  let%bind action = J.required_as fields "action" J.string in
  let%bind attachment_id = J.required_as fields "attachment_id" Id.Attachment.of_json in
  let%map selection =
    match action with
    | "inspect" ->
      let%bind fields = W.fields json [ "action"; "session"; "attachment_id" ] in
      let%bind raw = J.required fields "session" in
      let%bind _ = W.fields raw [ "server_id"; "session_id" ] in
      let%map session = Session_ref.of_json raw in
      Inspect session
    | other ->
      let%bind fields = W.fields json [ "action"; "expected"; "attachment_id" ] in
      let%bind expected = J.required_as fields "expected" Basis.of_json in
      let%map action =
        match other with
        | "continue" -> Ok Continue
        | "wait" -> Ok Wait
        | "finish" -> Ok Finish
        | "cancel" -> Ok Cancel
        | _ -> Error (Protocol_error.invalid_request "unknown recovery follow-up action")
      in
      Apply (expected, action)
  in
  { attachment_id; selection }
;;

module Reply = struct
  type phase =
    | Idle
    | Awaiting
    | Waiting
    | Preparing
    | Held_capacity
    | Held_capacity_unknown
    | Held_authority
    | Held_input
    | Held_interrupted
    | Admitted
    | Finished
    | Cancelled
  [@@deriving enumerate, equal, sexp_of]

  let phase_name phase = Sexp.to_string (sexp_of_phase phase) |> String.lowercase

  type t =
    { basis : Basis.t
    ; phase : phase
    }
  [@@deriving equal, sexp_of]

  let to_json t =
    `Object [ "basis", Basis.to_json t.basis; "phase", `String (phase_name t.phase) ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind _ = W.measure ~limits json in
    let%bind fields = W.fields json [ "basis"; "phase" ] in
    let%bind basis = J.required_as fields "basis" Basis.of_json in
    let%bind name = J.required_as fields "phase" J.string in
    let%map phase =
      List.find all_of_phase ~f:(fun phase -> String.equal (phase_name phase) name)
      |> Result.of_option
           ~error:(Protocol_error.invalid_request "unknown recovery follow-up phase")
    in
    { basis; phase }
  ;;
end

let validate (reply : Reply.t) ~request =
  let valid =
    match request.selection with
    | Inspect session -> Session_ref.equal session (Scope.session reply.basis.scope)
    | Apply (expected, _) ->
      Scope.equal expected.scope reply.basis.scope
      && Int64.(reply.basis.revision >= expected.revision)
  in
  if valid
  then Ok ()
  else Error (Protocol_error.invalid_request "recovery follow-up reply has another basis")
;;
