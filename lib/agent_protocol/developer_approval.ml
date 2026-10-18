open! Core
module Error = Protocol_error
module C = Developer_codec
module L = Developer_common.Limits

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode

module Owner = struct
  type t =
    | Command of Developer_command_id.t
    | Claimed_job of
        { job_id : Id.Job.t
        ; attempt : int
        }

  let to_json = function
    | Command id ->
      `Object [ "kind", `String "command"; "command_id", Developer_command_id.to_json id ]
    | Claimed_job { job_id; attempt } ->
      `Object
        [ "kind", `String "claimed_job"
        ; "job_id", Id.Job.to_json job_id
        ; "attempt", C.integer_json attempt
        ]
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "command_id"; "job_id"; "attempt" ] json in
    let* kind = get fields "kind" (C.string ~max_bytes:24) in
    match kind with
    | "command" ->
      let* () = C.fields ~allowed:[ "kind"; "command_id" ] json |> Result.map ~f:ignore in
      let* id = get fields "command_id" Developer_command_id.of_json in
      Ok (Command id)
    | "claimed_job" ->
      let* () =
        C.fields ~allowed:[ "kind"; "job_id"; "attempt" ] json |> Result.map ~f:ignore
      in
      let* job_id = get fields "job_id" Id.Job.of_json in
      let* attempt = get fields "attempt" C.positive in
      Ok (Claimed_job { job_id; attempt })
    | _ -> C.invalid "unknown approval owner"
  ;;
end

module Execution = struct
  type t =
    | Native_invocation of Id.Invocation.t
    | Claimed_model_job of
        { job_id : Id.Job.t
        ; attempt : int
        }

  let to_json = function
    | Native_invocation id ->
      `Object
        [ "kind", `String "native_invocation"; "invocation_id", Id.Invocation.to_json id ]
    | Claimed_model_job { job_id; attempt } ->
      `Object
        [ "kind", `String "claimed_model_job"
        ; "job_id", Id.Job.to_json job_id
        ; "attempt", C.integer_json attempt
        ]
  ;;

  let of_json json =
    let* fields =
      C.fields ~allowed:[ "kind"; "invocation_id"; "job_id"; "attempt" ] json
    in
    let* kind = get fields "kind" (C.string ~max_bytes:24) in
    match kind with
    | "native_invocation" ->
      let* () =
        C.fields ~allowed:[ "kind"; "invocation_id" ] json |> Result.map ~f:ignore
      in
      let* id = get fields "invocation_id" Id.Invocation.of_json in
      Ok (Native_invocation id)
    | "claimed_model_job" ->
      let* () =
        C.fields ~allowed:[ "kind"; "job_id"; "attempt" ] json |> Result.map ~f:ignore
      in
      let* job_id = get fields "job_id" Id.Job.of_json in
      let* attempt = get fields "attempt" C.positive in
      Ok (Claimed_model_job { job_id; attempt })
    | _ -> C.invalid "unknown approval execution"
  ;;
end

module Pending = struct
  type t =
    { id : Id.Permission.t
    ; owner : Owner.t
    ; execution : Execution.t
    ; runtime_id : string
    ; manifest_sha256 : string
    ; redacted_display : string
    ; expires_at : Timestamp.t
    }

  let to_json t =
    `Object
      [ "id", Id.Permission.to_json t.id
      ; "owner", Owner.to_json t.owner
      ; "execution", Execution.to_json t.execution
      ; "runtime_id", `String t.runtime_id
      ; "manifest_sha256", `String t.manifest_sha256
      ; "redacted_display", `String t.redacted_display
      ; "expires_at", Timestamp.to_json t.expires_at
      ]
  ;;

  let create
        ~limits
        ~id
        ~owner
        ~execution
        ~runtime_id
        ~manifest_sha256
        ~redacted_display
        ~expires_at
    =
    let matching =
      match owner, execution with
      | Owner.Command _, Execution.Native_invocation _ -> true
      | Claimed_job _, Native_invocation _ -> true
      | Claimed_job a, Claimed_model_job b ->
        Id.Job.equal a.job_id b.job_id && Int.equal a.attempt b.attempt
      | Command _, Claimed_model_job _ -> false
    in
    if
      (not matching)
      || (match owner with
          | Owner.Claimed_job { attempt; _ } ->
            attempt <= 0 || attempt > Authoring_codec.maximum_wire_integer
          | Command _ -> false)
      || String.is_empty runtime_id
      || String.length runtime_id > 1024
      || String.length redacted_display > L.max_approval_display_bytes limits
      || String.length manifest_sha256 <> 64
      || not
           (String.for_all manifest_sha256 ~f:(function
              | '0' .. '9' | 'a' .. 'f' -> true
              | _ -> false))
    then C.invalid "invalid pending approval"
    else (
      let t =
        { id
        ; owner
        ; execution
        ; runtime_id
        ; manifest_sha256
        ; redacted_display
        ; expires_at
        }
      in
      let* () = C.validate ~limits:(L.approval_page limits) (to_json t) in
      Ok t)
  ;;

  let id t = t.id
  let owner t = t.owner
  let execution t = t.execution
  let runtime_id t = t.runtime_id
  let manifest_sha256 t = t.manifest_sha256
  let redacted_display t = t.redacted_display
  let expires_at t = t.expires_at

  let of_json ~limits json =
    let* () = C.validate ~limits:(L.approval_page limits) json in
    let* fields =
      C.fields
        ~allowed:
          [ "id"
          ; "owner"
          ; "execution"
          ; "runtime_id"
          ; "manifest_sha256"
          ; "redacted_display"
          ; "expires_at"
          ]
        json
    in
    let* id = get fields "id" Id.Permission.of_json in
    let* owner = get fields "owner" Owner.of_json in
    let* execution = get fields "execution" Execution.of_json in
    let* runtime_id = get fields "runtime_id" (C.string ~max_bytes:1024) in
    let* manifest_sha256 = get fields "manifest_sha256" (C.string ~max_bytes:64) in
    let* redacted_display =
      get
        fields
        "redacted_display"
        (C.string ~max_bytes:(L.max_approval_display_bytes limits))
    in
    let* expires_at = get fields "expires_at" Timestamp.of_json in
    create
      ~limits
      ~id
      ~owner
      ~execution
      ~runtime_id
      ~manifest_sha256
      ~redacted_display
      ~expires_at
  ;;
end

module Page = struct
  type t = { pending : Pending.t list }

  let pending t = t.pending
  let to_json t = `Object [ "pending", `Array (List.map t.pending ~f:Pending.to_json) ]

  let create ~limits pending =
    if List.length pending > L.max_approval_page limits
    then C.limit "approval page exceeds installed count"
    else
      let* () =
        Result.all
          (List.map pending ~f:(fun item ->
             Pending.of_json ~limits (Pending.to_json item) |> Result.map ~f:ignore))
        |> Result.map ~f:ignore
      in
      let t = { pending } in
      let* () = C.validate ~limits:(L.approval_page limits) (to_json t) in
      Ok t
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:(L.approval_page limits) json in
    let* fields = C.fields ~allowed:[ "pending" ] json in
    let* pending =
      get
        fields
        "pending"
        (C.array ~max_items:(L.max_approval_page limits) (Pending.of_json ~limits))
    in
    create ~limits pending
  ;;
end

module Decision = struct
  type t =
    | Approve_once
    | Deny of string

  let create_deny ~max_reason_bytes reason =
    if max_reason_bytes <= 0 || String.length reason > max_reason_bytes
    then C.invalid "invalid approval decision reason"
    else Ok (Deny reason)
  ;;

  let to_json = function
    | Approve_once -> `Object [ "kind", `String "approve_once" ]
    | Deny reason -> `Object [ "kind", `String "deny"; "reason", `String reason ]
  ;;

  let of_json ~max_reason_bytes json =
    let* fields = C.fields ~allowed:[ "kind"; "reason" ] json in
    let* kind = get fields "kind" (C.string ~max_bytes:16) in
    match kind with
    | "approve_once" ->
      let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
      Ok Approve_once
    | "deny" ->
      let* reason = get fields "reason" (C.string ~max_bytes:max_reason_bytes) in
      create_deny ~max_reason_bytes reason
    | _ -> C.invalid "unknown approval decision"
  ;;
end

module Response = struct
  type t =
    { run_id : Id.Developer_run.t
    ; id : Id.Permission.t
    ; decision : Decision.t
    }

  let create ~max_reason_bytes ~run_id ~id ~decision =
    match decision with
    | Decision.Approve_once -> Ok { run_id; id; decision }
    | Deny reason ->
      let* _ = Decision.create_deny ~max_reason_bytes reason in
      Ok { run_id; id; decision }
  ;;

  let to_json t =
    `Object
      [ "run_id", Id.Developer_run.to_json t.run_id
      ; "id", Id.Permission.to_json t.id
      ; "decision", Decision.to_json t.decision
      ]
  ;;

  let of_json ~max_reason_bytes json =
    let* fields = C.fields ~allowed:[ "run_id"; "id"; "decision" ] json in
    let* run_id = get fields "run_id" Id.Developer_run.of_json in
    let* id = get fields "id" Id.Permission.of_json in
    let* decision = get fields "decision" (Decision.of_json ~max_reason_bytes) in
    create ~max_reason_bytes ~run_id ~id ~decision
  ;;
end
