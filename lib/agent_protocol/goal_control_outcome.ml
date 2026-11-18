(* Compact, authorized acknowledgements for Goal commands and continuations. *)
open! Core
module J = Json_codec
module W = Goal_wire
module Error = Protocol_error

let command_receipt_of_json json =
  let open Core.Result.Let_syntax in
  let%bind fields =
    W.fields
      json
      [ "principal_id"
      ; "key"
      ; "request_sha256"
      ; "goal_id"
      ; "goal_revision"
      ; "catalog_revision"
      ; "source"
      ; "accepted_at"
      ]
  in
  let%bind _ = J.required_as fields "source" (W.nullable W.source) in
  Goal_command_receipt.of_json json
;;

module Callback_control = struct
  type disposition =
    | Held of Goal.Reason.t
    | Abandoned of Goal.Reason.t

  type t =
    { goal_id : Id.Goal.t
    ; goal_revision : int64
    ; catalog_revision : int64
    ; disposition : disposition
    ; waiting : bool
    }

  let to_json t =
    let kind, reason =
      match t.disposition with
      | Held reason -> "held", reason
      | Abandoned reason -> "abandoned", reason
    in
    W.tagged
      kind
      [ "reason", Goal.Reason.to_json reason
      ; "goal_id", Id.Goal.to_json t.goal_id
      ; "goal_revision", W.counter_json t.goal_revision
      ; "catalog_revision", W.counter_json t.catalog_revision
      ; "waiting", W.bool_json t.waiting
      ]
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "kind"; "reason"; "goal_id"; "goal_revision"; "catalog_revision"; "waiting" ]
    in
    let%bind kind = J.required_as fields "kind" J.string in
    let%bind reason = J.required_as fields "reason" Goal.Reason.of_json in
    let%bind disposition =
      match kind with
      | "held" -> Ok (Held reason)
      | "abandoned" -> Ok (Abandoned reason)
      | _ -> W.invalid "unknown Goal callback control"
    in
    let%bind goal_id = J.required_as fields "goal_id" Id.Goal.of_json in
    let%bind goal_revision = J.required_as fields "goal_revision" W.counter in
    let%bind catalog_revision = J.required_as fields "catalog_revision" W.counter in
    let%map waiting = J.required_as fields "waiting" J.bool in
    { goal_id; goal_revision; catalog_revision; disposition; waiting }
  ;;
end

module Start_result = struct
  module Admission = struct
    type t =
      { receipt : Run_receipt.t
      ; association_sha256 : string
      }

    let create (receipt : Run_receipt.t) ~association_sha256 =
      if
        Run_receipt.Kind.equal receipt.kind Admission
        && Int64.equal receipt.run_revision 0L
        && Int.equal (String.length association_sha256) 64
        && String.for_all association_sha256 ~f:(function
          | '0' .. '9' | 'a' .. 'f' -> true
          | _ -> false)
      then Ok { receipt; association_sha256 }
      else W.invalid "invalid Goal Start admission binding"
    ;;

    let to_json t =
      `Object
        [ "receipt", Run_receipt.to_json t.receipt
        ; "association_sha256", `String t.association_sha256
        ]
    ;;

    let receipt_of_json json =
      let open Core.Result.Let_syntax in
      let%bind fields =
        W.fields
          json
          [ "run_id"
          ; "principal_id"
          ; "source"
          ; "key"
          ; "request_sha256"
          ; "kind"
          ; "run_revision"
          ; "session_revision"
          ; "committed_at"
          ]
      in
      let%bind _ = J.required_as fields "source" W.source in
      Run_receipt.of_json json
    ;;

    let of_json json =
      let open Core.Result.Let_syntax in
      let%bind fields = W.fields json [ "receipt"; "association_sha256" ] in
      let%bind receipt = J.required_as fields "receipt" receipt_of_json in
      let%bind association_sha256 = J.required_as fields "association_sha256" J.string in
      create receipt ~association_sha256
    ;;
  end

  type t =
    | Activated of Admission.t
    | Controlled of
        { admission : Admission.t
        ; control : Callback_control.t
        }
    | Settled of
        { admission : Admission.t
        ; run_revision : int64
        }
    | Retained of Admission.t
    | Admission_uncertain of Error.t
    | Activation_failed of
        { admission : Admission.t
        ; error : Error.t
        }

  let admission_json value = "admission", Admission.to_json value

  let to_json = function
    | Activated admission -> W.tagged "activated" [ admission_json admission ]
    | Controlled { admission; control } ->
      W.tagged
        "controlled"
        [ admission_json admission; "control", Callback_control.to_json control ]
    | Settled { admission; run_revision } ->
      W.tagged
        "settled"
        [ admission_json admission; "run_revision", W.counter_json run_revision ]
    | Retained admission -> W.tagged "retained" [ admission_json admission ]
    | Admission_uncertain error ->
      W.tagged "admission_uncertain" [ "error", Error.to_json error ]
    | Activation_failed { admission; error } ->
      W.tagged
        "activation_failed"
        [ admission_json admission; "error", Error.to_json error ]
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    let close fields_ = Extension_codec.closed fields ("kind" :: fields_) in
    let admission () = J.required_as fields "admission" Admission.of_json in
    match kind with
    | "activated" ->
      let%bind () = close [ "admission" ] in
      let%map admission = admission () in
      Activated admission
    | "controlled" ->
      let%bind () = close [ "admission"; "control" ] in
      let%bind admission = admission () in
      let%map control = J.required_as fields "control" Callback_control.of_json in
      Controlled { admission; control }
    | "settled" ->
      let%bind () = close [ "admission"; "run_revision" ] in
      let%bind admission = admission () in
      let%bind run_revision = J.required_as fields "run_revision" W.counter in
      if Int64.(run_revision > admission.receipt.run_revision)
      then Ok (Settled { admission; run_revision })
      else W.invalid "Goal settlement does not advance admission"
    | "retained" ->
      let%bind () = close [ "admission" ] in
      let%map admission = admission () in
      Retained admission
    | "admission_uncertain" ->
      let%bind () = close [ "error" ] in
      let%map error = J.required_as fields "error" Error.of_json in
      Admission_uncertain error
    | "activation_failed" ->
      let%bind () = close [ "admission"; "error" ] in
      let%bind admission = admission () in
      let%map error = J.required_as fields "error" Error.of_json in
      Activation_failed { admission; error }
    | _ -> W.invalid "unknown Goal Start result"
  ;;
end

module Resume_result = struct
  type continuation =
    | Started of
        { operation_id : Id.Operation.t
        ; generation : int
        }
    | Waiting
    | Settled of
        { run_id : Id.Run.t
        ; run_revision : int64
        }
    | Controlled of Callback_control.t

  type t =
    | Resumed of
        { receipt : Goal_command_receipt.t
        ; continuation : continuation
        }
    | Retained of Goal_command_receipt.t
    | Resume_failed of
        { receipt : Goal_command_receipt.t
        ; error : Error.t
        }
    | Rejected of Error.t
    | Uncertain of Error.t

  let continuation_to_json = function
    | Started { operation_id; generation } ->
      W.tagged
        "started"
        [ "operation_id", Id.Operation.to_json operation_id
        ; "generation", W.generation_json generation
        ]
    | Waiting -> W.tagged "waiting" []
    | Settled { run_id; run_revision } ->
      W.tagged
        "settled"
        [ "run_id", Id.Run.to_json run_id; "run_revision", W.counter_json run_revision ]
    | Controlled control ->
      W.tagged "controlled" [ "control", Callback_control.to_json control ]
  ;;

  let continuation_of_json json =
    let open Core.Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    let close names = Extension_codec.closed fields ("kind" :: names) in
    match kind with
    | "started" ->
      let%bind () = close [ "operation_id"; "generation" ] in
      let%bind operation_id = J.required_as fields "operation_id" Id.Operation.of_json in
      let%map generation = J.required_as fields "generation" W.generation in
      Started { operation_id; generation }
    | "waiting" ->
      let%map () = close [] in
      Waiting
    | "settled" ->
      let%bind () = close [ "run_id"; "run_revision" ] in
      let%bind run_id = J.required_as fields "run_id" Id.Run.of_json in
      let%map run_revision = J.required_as fields "run_revision" W.counter in
      Settled { run_id; run_revision }
    | "controlled" ->
      let%bind () = close [ "control" ] in
      let%map control = J.required_as fields "control" Callback_control.of_json in
      Controlled control
    | _ -> W.invalid "unknown Goal Resume continuation"
  ;;

  let receipt_json value = "receipt", Goal_command_receipt.to_json value

  let to_json = function
    | Resumed { receipt; continuation } ->
      W.tagged
        "resumed"
        [ receipt_json receipt; "continuation", continuation_to_json continuation ]
    | Retained receipt -> W.tagged "retained" [ receipt_json receipt ]
    | Resume_failed { receipt; error } ->
      W.tagged "resume_failed" [ receipt_json receipt; "error", Error.to_json error ]
    | Rejected error -> W.tagged "rejected" [ "error", Error.to_json error ]
    | Uncertain error -> W.tagged "uncertain" [ "error", Error.to_json error ]
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    let close fields_ = Extension_codec.closed fields ("kind" :: fields_) in
    let receipt () = J.required_as fields "receipt" command_receipt_of_json in
    let error () = J.required_as fields "error" Error.of_json in
    match kind with
    | "resumed" ->
      let%bind () = close [ "receipt"; "continuation" ] in
      let%bind receipt = receipt () in
      let%map continuation = J.required_as fields "continuation" continuation_of_json in
      Resumed { receipt; continuation }
    | "retained" ->
      let%bind () = close [ "receipt" ] in
      let%map receipt = receipt () in
      Retained receipt
    | "resume_failed" ->
      let%bind () = close [ "receipt"; "error" ] in
      let%bind receipt = receipt () in
      let%map error = error () in
      Resume_failed { receipt; error }
    | "rejected" ->
      let%bind () = close [ "error" ] in
      let%map error = error () in
      Rejected error
    | "uncertain" ->
      let%bind () = close [ "error" ] in
      let%map error = error () in
      Uncertain error
    | _ -> W.invalid "unknown Goal Resume result"
  ;;
end
