open! Core
module Error = Protocol_error
module C = Developer_codec
module Common = Developer_common
module Base = Developer_repl_wire_common
module Limits = Base.Limits

let ( let* ) result f = Result.bind result ~f
let get = Base.get
let optional = Base.optional

module Submission = Developer_repl_wire_result.Submission

module Task_view = struct
  module Diagnostic = struct
    type t =
      { code : string
      ; message : string
      ; location : Submission.Location.t option
      }

    let to_json t =
      `Object
        ([ "code", `String t.code; "message", `String t.message ]
         @ C.optional_field "location" t.location Submission.Location.to_json)
    ;;

    let create ~limits ~code ~message ~location =
      let t = { code; message; location } in
      if
        String.is_empty code
        || String.length code > 256
        || String.length message > Limits.max_output_bytes limits
      then C.invalid "invalid REPL Task diagnostic"
      else
        let* () =
          match location with
          | None -> Ok ()
          | Some location ->
            Submission.Location.of_json ~limits (Submission.Location.to_json location)
            |> Result.map ~f:ignore
        in
        Result.map
          (C.validate ~limits:(Limits.result limits) (to_json t))
          ~f:(fun () -> t)
    ;;

    let code t = t.code
    let message t = t.message
    let location t = t.location

    let of_json ~limits json =
      let* fields = C.fields ~allowed:[ "code"; "message"; "location" ] json in
      let* code = get fields "code" (C.string ~max_bytes:256) in
      let* message =
        get fields "message" (C.string ~max_bytes:(Limits.max_output_bytes limits))
      in
      let* location = optional fields "location" (Submission.Location.of_json ~limits) in
      create ~limits ~code ~message ~location
    ;;
  end

  type outcome =
    | Succeeded of string option
    | Failed of
        { error : Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Limited of
        { error : Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Interrupted
    | Cancelled

  type phase =
    | Reserved
    | Waiting_permission
    | Execution_started
    | Terminal of
        { execution_started : bool
        ; outcome : outcome
        }

  type t =
    { command_id : Developer_command_id.t
    ; owner : Common.Owner.t
    ; key : Idempotency_key.t
    ; phase : phase
    ; request_fingerprint : Chatml_source.Digest.t
    ; capture_fingerprint : Chatml_source.Digest.t
    }

  let outcome_json = function
    | Succeeded value_preview ->
      `Object
        ([ "kind", `String "succeeded" ]
         @ C.optional_field "value_preview" value_preview (fun x -> `String x))
    | Failed { error; diagnostic } ->
      `Object
        ([ "kind", `String "failed"; "error", Error.to_json error ]
         @ C.optional_field "diagnostic" diagnostic Diagnostic.to_json)
    | Limited { error; diagnostic } ->
      `Object
        ([ "kind", `String "limited"; "error", Error.to_json error ]
         @ C.optional_field "diagnostic" diagnostic Diagnostic.to_json)
    | Interrupted -> `Object [ "kind", `String "interrupted" ]
    | Cancelled -> `Object [ "kind", `String "cancelled" ]
  ;;

  let phase_json = function
    | Reserved -> `Object [ "state", `String "reserved" ]
    | Waiting_permission -> `Object [ "state", `String "waiting_permission" ]
    | Execution_started -> `Object [ "state", `String "execution_started" ]
    | Terminal { execution_started; outcome } ->
      `Object
        [ "state", `String "terminal"
        ; ("execution_started", if execution_started then `True else `False)
        ; "outcome", outcome_json outcome
        ]
  ;;

  let to_json t =
    `Object
      [ "command_id", Developer_command_id.to_json t.command_id
      ; "owner", Common.Owner.to_json t.owner
      ; "key", Idempotency_key.to_json t.key
      ; "kind", `String "repl_task_run"
      ; "phase", phase_json t.phase
      ; "request_fingerprint", C.digest_json t.request_fingerprint
      ; "capture_fingerprint", C.digest_json t.capture_fingerprint
      ]
  ;;

  let create
        ~limits
        ~command_id
        ~owner
        ~key
        ~phase
        ~request_fingerprint
        ~capture_fingerprint
    =
    let t = { command_id; owner; key; phase; request_fingerprint; capture_fingerprint } in
    let* () = Common.Owner.of_json (Common.Owner.to_json owner) |> Result.map ~f:ignore in
    let* () =
      match phase with
      | Terminal { execution_started = false; outcome = Succeeded _ } ->
        C.invalid "REPL Task success requires durable Start"
      | Terminal { outcome = Succeeded (Some preview); _ }
        when String.length preview > Limits.max_output_bytes limits ->
        C.limit "REPL Task preview exceeds output bound"
      | Reserved | Waiting_permission | Execution_started | Terminal _ -> Ok ()
    in
    let* () =
      match phase with
      | Terminal
          { outcome = Failed { error; diagnostic } | Limited { error; diagnostic }; _ } ->
        let* _ = Error.of_json (Error.to_json error) in
        (match diagnostic with
         | None -> Ok ()
         | Some diagnostic ->
           Diagnostic.of_json ~limits (Diagnostic.to_json diagnostic)
           |> Result.map ~f:ignore)
      | Reserved | Waiting_permission | Execution_started | Terminal _ -> Ok ()
    in
    Result.map (C.validate ~limits:(Limits.result limits) (to_json t)) ~f:(fun () -> t)
  ;;

  let command_id t = t.command_id
  let owner t = t.owner
  let key t = t.key
  let phase t = t.phase
  let request_fingerprint t = t.request_fingerprint
  let capture_fingerprint t = t.capture_fingerprint

  let outcome_of_json ~limits json =
    let* fields =
      C.fields ~allowed:[ "kind"; "value_preview"; "error"; "diagnostic" ] json
    in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "succeeded" ->
      let* () =
        C.fields ~allowed:[ "kind"; "value_preview" ] json |> Result.map ~f:ignore
      in
      let* preview =
        optional
          fields
          "value_preview"
          (C.string ~max_bytes:(Limits.max_output_bytes limits))
      in
      Ok (Succeeded preview)
    | "failed" | "limited" ->
      let* () =
        C.fields ~allowed:[ "kind"; "error"; "diagnostic" ] json |> Result.map ~f:ignore
      in
      let* error = get fields "error" Error.of_json in
      let* diagnostic = optional fields "diagnostic" (Diagnostic.of_json ~limits) in
      if String.equal kind "failed"
      then Ok (Failed { error; diagnostic })
      else Ok (Limited { error; diagnostic })
    | "interrupted" | "cancelled" ->
      let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
      if String.equal kind "interrupted" then Ok Interrupted else Ok Cancelled
    | _ -> C.invalid "unknown REPL Task outcome"
  ;;

  let phase_of_json ~limits json =
    let* fields = C.fields ~allowed:[ "state"; "execution_started"; "outcome" ] json in
    let* state = get fields "state" (C.string ~max_bytes:32) in
    match state with
    | "reserved" ->
      let* () = C.fields ~allowed:[ "state" ] json |> Result.map ~f:ignore in
      Ok Reserved
    | "waiting_permission" ->
      let* () = C.fields ~allowed:[ "state" ] json |> Result.map ~f:ignore in
      Ok Waiting_permission
    | "execution_started" ->
      let* () = C.fields ~allowed:[ "state" ] json |> Result.map ~f:ignore in
      Ok Execution_started
    | "terminal" ->
      let* execution_started = get fields "execution_started" C.boolean in
      let* outcome = get fields "outcome" (outcome_of_json ~limits) in
      Ok (Terminal { execution_started; outcome })
    | _ -> C.invalid "unknown REPL Task phase"
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.result limits) json in
    let* fields =
      C.fields
        ~allowed:
          [ "command_id"
          ; "owner"
          ; "key"
          ; "kind"
          ; "phase"
          ; "request_fingerprint"
          ; "capture_fingerprint"
          ]
        json
    in
    let* command_id = get fields "command_id" Developer_command_id.of_json in
    let* owner = get fields "owner" Common.Owner.of_json in
    let* key = get fields "key" Idempotency_key.of_json in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    if not (String.equal kind "repl_task_run")
    then C.invalid "expected REPL Task view"
    else
      let* phase = get fields "phase" (phase_of_json ~limits) in
      let* request_fingerprint = get fields "request_fingerprint" C.digest in
      let* capture_fingerprint = get fields "capture_fingerprint" C.digest in
      create
        ~limits
        ~command_id
        ~owner
        ~key
        ~phase
        ~request_fingerprint
        ~capture_fingerprint
  ;;
end
