open! Core
module P = Agent_protocol
module D = Document_schema
module Receipt = Developer_command_receipt
module R = Receipt
module S = Chatml_source

let ( let* ) result f = Result.bind result ~f
let corrupt message = Error (Store_error.Corrupt message)

module Phase = struct
  type t =
    | Reserved
    | Waiting_permission
    | Execution_started
    | Terminal of
        { execution_started : bool
        ; receipt : R.t
        }
end

open Phase

module Change = struct
  type t =
    | Wait_permission
    | Start_execution
    | Finish of R.t
end

module Error = struct
  type t =
    | Conflict
    | Store of Store_error.t
end

type t =
  { id : R.Id.t
  ; key : P.Idempotency_key.t
  ; owner : P.Execution_owner.developer
  ; capture_fingerprint : S.Digest.t
  ; request_fingerprint : S.Digest.t
  ; kind : R.Kind.t
  ; phase : Phase.t
  ; encoded_bytes : int
  }

let id t = t.id
let key t = t.key
let owner t = t.owner
let capture_fingerprint t = t.capture_fingerprint
let request_fingerprint t = t.request_fingerprint
let kind t = t.kind
let phase t = t.phase
let encoded_bytes t = t.encoded_bytes

let same_identity a b =
  R.Id.equal a.id b.id
  && P.Idempotency_key.equal a.key b.key
  && P.Execution_owner.equal_developer a.owner b.owner
  && S.Digest.equal a.capture_fingerprint b.capture_fingerprint
  && S.Digest.equal a.request_fingerprint b.request_fingerprint
  && R.Kind.equal a.kind b.kind
;;

let kind_string = function
  | R.Kind.Check -> "check"
  | Fixture_run -> "fixture_run"
  | Live_run -> "live_run"
  | Repl_task_run -> "repl_task_run"
;;

let kind_of_string = function
  | "check" -> Ok R.Kind.Check
  | "fixture_run" -> Ok Fixture_run
  | "live_run" -> Ok Live_run
  | "repl_task_run" -> Ok Repl_task_run
  | _ -> corrupt "invalid developer command kind"
;;

let phase_json : Phase.t -> Jsonaf.t = function
  | Phase.Reserved -> `Object [ "state", `String "reserved" ]
  | Waiting_permission -> `Object [ "state", `String "waiting_permission" ]
  | Execution_started -> `Object [ "state", `String "execution_started" ]
  | Terminal { execution_started; receipt } ->
    `Object
      [ "state", `String "terminal"
      ; ("execution_started", if execution_started then `True else `False)
      ; "receipt", R.to_json receipt
      ]
;;

let to_json t =
  `Object
    [ "id", `String (R.Id.to_string t.id)
    ; "key", P.Idempotency_key.to_json t.key
    ; "owner", Developer_owner_document.to_json t.owner
    ; "capture_fingerprint", `String (S.Digest.to_hex t.capture_fingerprint)
    ; "request_fingerprint", `String (S.Digest.to_hex t.request_fingerprint)
    ; "kind", `String (kind_string t.kind)
    ; "phase", phase_json t.phase
    ]
;;

let phase_valid kind phase =
  match kind with
  | R.Kind.Check | Fixture_run ->
    (match phase with
     | Reserved -> true
     | Waiting_permission | Execution_started -> false
     | Terminal { execution_started; receipt } ->
       (not execution_started) && R.Kind.equal kind (R.kind receipt))
  | (Live_run | Repl_task_run) as kind ->
    (match phase with
     | Reserved | Waiting_permission | Execution_started -> true
     | Terminal { execution_started; receipt } ->
       R.Kind.equal kind (R.kind receipt)
       &&
       if execution_started
       then true
       else (
         match R.value receipt with
         | R.Live (R.Live.Succeeded _) | Task (R.Task.Succeeded _) -> false
         | Live _ | Task _ -> true
         | Check _ | Fixture _ -> false))
;;

let validate_identity t =
  let* () =
    P.Execution_owner.validate (P.Execution_owner.Developer t.owner)
    |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid command owner")
  in
  let* () =
    P.Idempotency_key.of_json (P.Idempotency_key.to_json t.key)
    |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid command key")
    |> Result.map ~f:(fun _ -> ())
  in
  if phase_valid t.kind t.phase
  then Ok ()
  else corrupt "invalid command phase or receipt kind"
;;

let measure t ~limits ~poll =
  let* () = validate_identity t in
  let* bytes =
    D.Json.validate_and_measure_with_checkpoint ~limits ~checkpoint:poll (to_json t)
    |> Document_fields.store
  in
  Ok { t with encoded_bytes = bytes }
;;

let create ~id ~key ~owner ~capture_fingerprint ~request_fingerprint ~kind ~limits ~poll =
  measure
    { id
    ; key
    ; owner
    ; capture_fingerprint
    ; request_fingerprint
    ; kind
    ; phase = Reserved
    ; encoded_bytes = 0
    }
    ~limits
    ~poll
;;

let apply t change ~limits ~poll =
  let phase =
    match t.kind, t.phase, change with
    | (Live_run | Repl_task_run), Reserved, Change.Wait_permission
    | (Live_run | Repl_task_run), Waiting_permission, Change.Wait_permission ->
      Some Waiting_permission
    | (Live_run | Repl_task_run), (Reserved | Waiting_permission), Change.Start_execution
      -> Some Execution_started
    | _, Terminal { receipt = prior; execution_started }, Change.Finish next
      when R.equal prior next -> Some (Terminal { receipt = prior; execution_started })
    | _, (Reserved | Waiting_permission | Execution_started), Change.Finish receipt ->
      Some
        (Terminal
           { receipt
           ; execution_started =
               (match t.phase with
                | Execution_started -> true
                | Reserved | Waiting_permission | Terminal _ -> false)
           })
    | _, _, _ -> None
  in
  match phase with
  | None -> Error Error.Conflict
  | Some phase ->
    if not (phase_valid t.kind phase)
    then Error Error.Conflict
    else
      measure { t with phase } ~limits ~poll
      |> Result.map_error ~f:(fun error -> Error.Store error)
;;

let exact json names =
  match json with
  | `Object fields ->
    let actual = List.map fields ~f:fst in
    if
      List.length actual = List.length names
      && List.length actual
         = List.length (List.dedup_and_sort actual ~compare:String.compare)
      && List.for_all names ~f:(List.mem actual ~equal:String.equal)
    then Ok ()
    else corrupt "developer command fields are missing, duplicate or unexpected"
  | _ -> corrupt "developer command is not an object"
;;

let required json name =
  match D.Json.field json ~name with
  | Value value -> Ok value
  | Absent | Null -> corrupt ("missing command field " ^ name)
;;

let string json name =
  let* value = required json name in
  match value with
  | `String value -> Ok value
  | _ -> corrupt ("invalid command string " ^ name)
;;

let digest json name =
  let* value = string json name in
  S.Digest.of_hex value
  |> Result.map_error ~f:(fun _ -> Store_error.Corrupt ("invalid command digest " ^ name))
;;

let decode_phase json ~limits ~poll =
  let* state = string json "state" in
  match state with
  | "reserved" | "waiting_permission" | "execution_started" ->
    let* () = exact json [ "state" ] in
    (match state with
     | "reserved" -> Ok Phase.Reserved
     | "waiting_permission" -> Ok Waiting_permission
     | "execution_started" -> Ok Execution_started
     | _ -> corrupt "invalid command phase")
  | "terminal" ->
    let* () = exact json [ "state"; "execution_started"; "receipt" ] in
    let* execution_started =
      match D.Json.field json ~name:"execution_started" with
      | Value `True -> Ok true
      | Value `False -> Ok false
      | Value _ | Null | Absent -> corrupt "invalid command execution-start flag"
    in
    let* receipt =
      required json "receipt"
      |> Result.bind ~f:(fun json ->
        R.of_json json ~limits ~poll |> Document_fields.store)
    in
    Ok (Phase.Terminal { execution_started; receipt })
  | _ -> corrupt "invalid command phase"
;;

let of_json json ~limits ~poll =
  let* _ =
    D.Json.validate_and_measure_with_checkpoint ~limits ~checkpoint:poll json
    |> Document_fields.store
  in
  let* () =
    exact
      json
      [ "id"
      ; "key"
      ; "owner"
      ; "capture_fingerprint"
      ; "request_fingerprint"
      ; "kind"
      ; "phase"
      ]
  in
  let* id =
    string json "id"
    |> Result.bind ~f:(fun text -> R.Id.of_string text |> Document_fields.store)
  in
  let* key =
    required json "key"
    |> Result.bind ~f:(fun json ->
      P.Idempotency_key.of_json json
      |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid command key"))
  in
  let* owner =
    required json "owner"
    |> Result.bind ~f:(fun json ->
      Developer_owner_document.of_json json |> Document_fields.store)
  in
  let* capture_fingerprint = digest json "capture_fingerprint" in
  let* request_fingerprint = digest json "request_fingerprint" in
  let* kind = string json "kind" |> Result.bind ~f:kind_of_string in
  let* phase = required json "phase" |> Result.bind ~f:(decode_phase ~limits ~poll) in
  let* row =
    measure
      { id
      ; key
      ; owner
      ; capture_fingerprint
      ; request_fingerprint
      ; kind
      ; phase
      ; encoded_bytes = 0
      }
      ~limits
      ~poll
  in
  if D.Json.equal json (to_json row)
  then Ok row
  else corrupt "developer command row changed during canonical projection"
;;
