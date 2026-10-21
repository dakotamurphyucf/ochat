open! Core
module C = Developer_codec
module R = Developer_receipt
module Limits = Developer_metadata.Limits
module Owner = Developer_metadata.Owner

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode

module View = struct
  type phase =
    | Reserved
    | Waiting_permission
    | Execution_started
    | Terminal of
        { execution_started : bool
        ; receipt : R.t
        }

  type t =
    { command_id : Developer_command_id.t
    ; owner : Owner.t
    ; key : Idempotency_key.t
    ; kind : R.Kind.t
    ; phase : phase
    ; request_fingerprint : Chatml_source.Digest.t
    ; capture_fingerprint : Chatml_source.Digest.t
    }

  let valid_phase kind phase =
    match kind, phase with
    | R.Kind.Live_run, (Reserved | Waiting_permission | Execution_started) -> true
    | (Check | Fixture_run), Reserved -> true
    | (Check | Fixture_run), (Waiting_permission | Execution_started) -> false
    | kind, Terminal { execution_started; receipt } ->
      R.Kind.equal kind (R.kind receipt)
      &&
        (match kind, R.value receipt with
        | Live_run, Live (Succeeded _) -> execution_started
        | Live_run, Live (Failed _ | Limited _ | Interrupted | Cancelled) -> true
        | (Check | Fixture_run), (Check _ | Fixture _) -> not execution_started
        | _ -> false)
  ;;

  let rec create
            ~limits
            ~command_id
            ~owner
            ~key
            ~kind
            ~phase
            ~request_fingerprint
            ~capture_fingerprint
    =
    if not (valid_phase kind phase)
    then C.invalid "invalid developer command phase"
    else
      let* () = Owner.of_json (Owner.to_json owner) |> Result.map ~f:ignore in
      let t =
        { command_id; owner; key; kind; phase; request_fingerprint; capture_fingerprint }
      in
      let* () =
        match phase with
        | Terminal { receipt; _ } ->
          R.of_json ~limits:(Limits.receipt limits) (R.to_json receipt)
          |> Result.map ~f:ignore
        | Reserved | Waiting_permission | Execution_started -> Ok ()
      in
      let* () = C.validate ~limits:(Limits.response limits) (to_json t) in
      Ok t

  and to_json t =
    let phase =
      match t.phase with
      | Reserved -> `Object [ "state", `String "reserved" ]
      | Waiting_permission -> `Object [ "state", `String "waiting_permission" ]
      | Execution_started -> `Object [ "state", `String "execution_started" ]
      | Terminal { execution_started; receipt } ->
        `Object
          [ "state", `String "terminal"
          ; ("execution_started", if execution_started then `True else `False)
          ; "receipt", R.to_json receipt
          ]
    in
    `Object
      [ "command_id", Developer_command_id.to_json t.command_id
      ; "owner", Owner.to_json t.owner
      ; "key", Idempotency_key.to_json t.key
      ; ( "kind"
        , `String
            (match t.kind with
             | Check -> "check"
             | Fixture_run -> "fixture_run"
             | Live_run -> "live_run") )
      ; "phase", phase
      ; "request_fingerprint", C.digest_json t.request_fingerprint
      ; "capture_fingerprint", C.digest_json t.capture_fingerprint
      ]
  ;;

  let command_id t = t.command_id
  let owner t = t.owner
  let key t = t.key
  let kind t = t.kind
  let phase t = t.phase
  let request_fingerprint t = t.request_fingerprint
  let capture_fingerprint t = t.capture_fingerprint

  let phase_of_json ~limits json =
    let* fields = C.fields ~allowed:[ "state"; "execution_started"; "receipt" ] json in
    let* state = get fields "state" (C.string ~max_bytes:32) in
    let check allowed = C.fields ~allowed json |> Result.map ~f:ignore in
    match state with
    | "reserved" ->
      let* () = check [ "state" ] in
      Ok Reserved
    | "waiting_permission" ->
      let* () = check [ "state" ] in
      Ok Waiting_permission
    | "execution_started" ->
      let* () = check [ "state" ] in
      Ok Execution_started
    | "terminal" ->
      let* execution_started = get fields "execution_started" C.boolean in
      let* receipt = get fields "receipt" (R.of_json ~limits:(Limits.receipt limits)) in
      Ok (Terminal { execution_started; receipt })
    | _ -> C.invalid "unknown developer command phase"
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.response limits) json in
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
    let* owner = get fields "owner" Owner.of_json in
    let* key = get fields "key" Idempotency_key.of_json in
    let* kind = get fields "kind" (C.string ~max_bytes:16) in
    let* kind =
      match kind with
      | "check" -> Ok R.Kind.Check
      | "fixture_run" -> Ok Fixture_run
      | "live_run" -> Ok Live_run
      | _ -> C.invalid "unknown developer command kind"
    in
    let* phase = get fields "phase" (phase_of_json ~limits) in
    let* request_fingerprint = get fields "request_fingerprint" C.digest in
    let* capture_fingerprint = get fields "capture_fingerprint" C.digest in
    create
      ~limits
      ~command_id
      ~owner
      ~key
      ~kind
      ~phase
      ~request_fingerprint
      ~capture_fingerprint
  ;;
end
