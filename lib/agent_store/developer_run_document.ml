open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type epoch =
  { number : int
  ; capture : Chatml_source.Digest.t
  ; phase : [ `Active | `Interrupted | `Closed ]
  ; revision : int64
  }

type t =
  { host_id : P.Id.Server.t
  ; run_id : P.Id.Developer_run.t
  ; principal_id : P.Id.Principal.t
  ; revision : int64
  ; epochs : epoch list
  }

let kind = "store.developer_run"
let version = 1
let ( let* ) result f = Result.bind result ~f
let corrupt reason = Error (Store_error.Corrupt reason)
let invalid field reason = F.invalid field reason |> F.store
let host_id t = t.host_id
let run_id t = t.run_id
let principal_id t = t.principal_id
let revision t = t.revision
let epochs t = t.epochs
let epoch_number t = t.number
let epoch_capture t = t.capture
let epoch_phase t = t.phase
let epoch_revision (t : epoch) = t.revision

let next_revision revision =
  if Int64.(revision <= zero || revision >= max_value - one)
  then corrupt "developer run document revision exhausted"
  else Ok Int64.(revision + one)
;;

let create ~(owner : P.Execution_owner.developer) ~capture_fingerprint =
  { host_id = owner.host_id
  ; run_id = owner.run_id
  ; principal_id = owner.principal_id
  ; revision = 1L
  ; epochs =
      [ { number = owner.epoch
        ; capture = capture_fingerprint
        ; phase = `Active
        ; revision = 1L
        }
      ]
  }
;;

let same_root t (owner : P.Execution_owner.developer) =
  P.Id.Server.equal t.host_id owner.host_id
  && P.Id.Developer_run.equal t.run_id owner.run_id
  && P.Id.Principal.equal t.principal_id owner.principal_id
;;

let append_epoch t ~owner ~capture_fingerprint ~max_epochs =
  let open Result.Let_syntax in
  if not (same_root t owner)
  then corrupt "developer run epoch belongs to another root or principal"
  else if List.length t.epochs >= max_epochs
  then corrupt "developer run epoch limit exceeded"
  else (
    match List.last t.epochs with
    | None -> corrupt "developer run has no epoch"
    | Some last when last.number = Int.max_value ->
      corrupt "developer run epoch number exhausted"
    | Some last when owner.epoch <> last.number + 1 ->
      corrupt "developer run epoch is not the next epoch"
    | Some last ->
      let%bind revision = next_revision t.revision in
      let%map old_revision = next_revision last.revision in
      let prior =
        List.map t.epochs ~f:(fun value ->
          if value.number = last.number
          then
            { value with
              phase =
                (match value.phase with
                 | `Closed -> `Closed
                 | `Active | `Interrupted -> `Interrupted)
            ; revision = old_revision
            }
          else value)
      in
      { t with
        revision
      ; epochs =
          prior
          @ [ { number = owner.epoch
              ; capture = capture_fingerprint
              ; phase = `Active
              ; revision = 1L
              }
            ]
      })
;;

let set_phase t ~epoch ~phase =
  let open Result.Let_syntax in
  let%bind revision = next_revision t.revision in
  let found = ref false in
  let%map epochs =
    List.map t.epochs ~f:(fun value ->
      if value.number <> epoch
      then Ok value
      else (
        found := true;
        let%map epoch_revision = next_revision value.revision in
        { value with phase; revision = epoch_revision }))
    |> Result.all
    |> Result.bind ~f:(fun epochs ->
      if !found then Ok epochs else corrupt "developer run epoch is missing")
  in
  { t with revision; epochs }
;;

let phase_json = function
  | `Active -> `String "active"
  | `Interrupted -> `String "interrupted"
  | `Closed -> `String "closed"
;;

let phase_of_json = function
  | `String "active" -> Ok `Active
  | `String "interrupted" -> Ok `Interrupted
  | `String "closed" -> Ok `Closed
  | _ -> F.invalid "phase" "unsupported developer run phase"
;;

let exact_fields json names =
  match json with
  | `Object fields ->
    if
      List.length fields = List.length names
      && List.for_all fields ~f:(fun (name, _) -> List.mem names name ~equal:String.equal)
    then Ok ()
    else invalid "payload" "developer run fields are missing or unexpected"
  | _ -> invalid "payload" "developer run payload is not an object"
;;

let epoch_json epoch =
  `Object
    [ "number", F.decimal_json (Int64.of_int epoch.number)
    ; "capture_fingerprint", `String (Chatml_source.Digest.to_hex epoch.capture)
    ; "phase", phase_json epoch.phase
    ; "revision", F.decimal_json epoch.revision
    ]
;;

let to_payload t =
  `Object
    [ "host_id", P.Id.Server.to_json t.host_id
    ; "run_id", P.Id.Developer_run.to_json t.run_id
    ; "principal_id", P.Id.Principal.to_json t.principal_id
    ; "revision", F.decimal_json t.revision
    ; "epochs", `Array (List.map t.epochs ~f:epoch_json)
    ; "invocations", `Array []
    ; "jobs", `Array []
    ]
;;

let validate t ~max_epochs =
  let open Result.Let_syntax in
  let%bind () =
    if List.is_empty t.epochs || List.length t.epochs > max_epochs
    then corrupt "developer run epoch count is invalid"
    else Ok ()
  in
  let%bind () =
    if Int64.(t.revision <= zero || t.revision >= max_value)
    then corrupt "developer run revision is invalid"
    else Ok ()
  in
  let rec check previous = function
    | [] -> Ok ()
    | epoch :: rest ->
      let%bind () =
        if
          epoch.number < 0
          || Int64.(epoch.revision <= zero || epoch.revision >= max_value)
          || Option.value_map previous ~default:false ~f:(fun old ->
            old = Int.max_value || epoch.number <> old + 1)
        then corrupt "developer run epoch sequence is invalid"
        else Ok ()
      in
      let%bind () =
        match rest, epoch.phase with
        | _ :: _, `Active -> corrupt "only the latest developer epoch may be active"
        | _ -> Ok ()
      in
      check (Some epoch.number) rest
  in
  check None t.epochs
;;

let to_bytes t ~limits ~max_epochs =
  let open Result.Let_syntax in
  let%bind () = validate t ~max_epochs in
  let%map document =
    D.Document.create ~limits ~kind ~version ~payload:(to_payload t) |> F.store
  in
  D.Document.to_string document
;;

let required json name decode = F.required json name decode |> F.store
let protocol decode json = decode json |> F.protocol

let epoch_of_json json =
  let open Result.Let_syntax in
  let%bind () =
    exact_fields json [ "number"; "capture_fingerprint"; "phase"; "revision" ]
  in
  let%bind number =
    required json "number" F.decimal
    |> Result.bind ~f:(fun value ->
      if Int64.(value > of_int Int.max_value)
      then invalid "number" "developer run epoch exceeds int range"
      else Ok (Int64.to_int_exn value))
  in
  let%bind capture =
    required json "capture_fingerprint" F.digest
    |> Result.bind ~f:(fun hex ->
      Chatml_source.Digest.of_hex hex
      |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid capture digest"))
  in
  let%bind phase = required json "phase" phase_of_json in
  let%map revision = required json "revision" F.decimal in
  { number; capture; phase; revision }
;;

let of_bytes bytes ~limits ~max_epochs =
  let open Result.Let_syntax in
  let%bind document = D.Document.decode ~limits bytes |> F.store in
  let%bind () = F.expect document ~kind ~version |> F.store in
  let%bind () =
    exact_fields
      (D.Document.json document)
      [ "format"; "schema_version"; "kind"; "payload" ]
  in
  let payload = D.Document.payload document in
  let%bind () =
    exact_fields
      payload
      [ "host_id"; "run_id"; "principal_id"; "revision"; "epochs"; "invocations"; "jobs" ]
  in
  let%bind host_id = required payload "host_id" (protocol P.Id.Server.of_json) in
  let%bind run_id = required payload "run_id" (protocol P.Id.Developer_run.of_json) in
  let%bind principal_id =
    required payload "principal_id" (protocol P.Id.Principal.of_json)
  in
  let%bind revision = required payload "revision" F.decimal in
  let%bind epochs_json = required payload "epochs" F.array in
  let%bind () =
    if List.is_empty epochs_json || List.length epochs_json > max_epochs
    then corrupt "developer run epoch count exceeds its bound"
    else Ok ()
  in
  let%bind epochs = List.map epochs_json ~f:epoch_of_json |> Result.all in
  let%bind invocations = required payload "invocations" F.array in
  let%bind jobs = required payload "jobs" F.array in
  let%bind () =
    if List.is_empty invocations && List.is_empty jobs
    then Ok ()
    else corrupt "developer run invocation/job records are not yet admitted"
  in
  let t = { host_id; run_id; principal_id; revision; epochs } in
  let%map () = validate t ~max_epochs in
  t
;;
