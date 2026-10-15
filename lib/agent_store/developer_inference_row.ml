open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol
module O = Inference.Observation

type t =
  { ordinal : int64
  ; owner : P.Execution_owner.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; job_id : P.Id.Job.t
  ; job_attempt : int
  ; context_id : O.Observation_id.t
  ; record : O.Attempt_record.t
  ; encoded_bytes : int
  }

let ordinal t = t.ordinal
let owner t = t.owner
let capture_fingerprint t = t.capture_fingerprint
let job_id t = t.job_id
let job_attempt t = t.job_attempt
let context_id t = t.context_id
let record t = t.record
let encoded_bytes t = t.encoded_bytes
let ( let* ) result f = Result.bind result ~f
let corrupt message = Error (Store_error.Corrupt message)

let valid_id to_json of_json id =
  match of_json (to_json id) with
  | Ok _ -> Ok ()
  | Error _ -> corrupt "invalid developer inference coordinate"
;;

let scope ~owner ~job_id ~ordinal ~relation =
  let* () =
    match P.Execution_owner.validate (P.Execution_owner.Developer owner) with
    | Ok () -> Ok ()
    | Error _ -> corrupt "invalid developer inference owner"
  in
  let* () = valid_id P.Id.Job.to_json P.Id.Job.of_json job_id in
  if Int64.(ordinal <= zero)
  then corrupt "invalid developer inference ordinal"
  else (
    let source_text =
      String.concat
        ~sep:":"
        [ "developer"
        ; P.Id.Server.to_string owner.host_id
        ; P.Id.Developer_run.to_string owner.run_id
        ; Int.to_string owner.epoch
        ; P.Id.Job.to_string job_id
        ]
    in
    let attempt_text = "ordinal:" ^ Int64.to_string ordinal in
    let* source =
      Transcript.Source_id.of_string source_text
      |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid inference source")
    in
    let* attempt =
      Transcript.Attempt_id.of_string attempt_text
      |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid inference attempt")
    in
    Transcript.Scope.create ~source ~attempt ~relation
    |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid inference scope"))
;;

let to_json t =
  `Object
    [ "ordinal", F.decimal_json t.ordinal
    ; "host_id", P.Id.Server.to_json t.owner.host_id
    ; "run_id", P.Id.Developer_run.to_json t.owner.run_id
    ; "epoch", F.decimal_json (Int64.of_int t.owner.epoch)
    ; "principal_id", P.Id.Principal.to_json t.owner.principal_id
    ; "capture_fingerprint", `String (Chatml_source.Digest.to_hex t.capture_fingerprint)
    ; "job_id", P.Id.Job.to_json t.job_id
    ; "job_attempt", F.decimal_json (Int64.of_int t.job_attempt)
    ; "context_id", `String (O.Observation_id.to_string t.context_id)
    ; "record", O.Attempt_record.to_json t.record
    ]
;;

let create
      ~ordinal
      ~owner
      ~capture_fingerprint
      ~job_id
      ~job_attempt
      ~context_id
      ~record
      ~limits
  =
  let* () = if job_attempt <= 0 then corrupt "invalid inference job attempt" else Ok () in
  let* expected =
    let relation = (O.Attempt_record.scope record).relation in
    scope ~owner ~job_id ~ordinal ~relation
  in
  let* () =
    if Transcript.Scope.equal expected (O.Attempt_record.scope record)
    then Ok ()
    else corrupt "inference scope disagrees with job and ordinal"
  in
  let* () =
    O.Attempt_record.validate record ~limits:O.Admission.attempt
    |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid inference attempt")
  in
  let* () =
    match O.Observation_id.of_string "context", O.Observation_id.of_string "usage" with
    | Ok context, Ok usage ->
      if
        O.Observation_id.equal context_id context
        && O.Observation_id.equal (O.Attempt_record.accounting_id record) usage
      then Ok ()
      else corrupt "inference designated observation IDs disagree"
    | Error _, _ | _, Error _ -> corrupt "invalid designated observation ID"
  in
  let provisional =
    { ordinal
    ; owner
    ; capture_fingerprint
    ; job_id
    ; job_attempt
    ; context_id
    ; record
    ; encoded_bytes = 0
    }
  in
  let* encoded_bytes =
    D.Json.validate_and_measure ~limits (to_json provisional) |> F.store
  in
  Ok { provisional with encoded_bytes }
;;

let same_identity left right =
  Int64.equal left.ordinal right.ordinal
  && P.Execution_owner.equal_developer left.owner right.owner
  && Chatml_source.Digest.equal left.capture_fingerprint right.capture_fingerprint
  && P.Id.Job.equal left.job_id right.job_id
  && Int.equal left.job_attempt right.job_attempt
  && O.Observation_id.equal left.context_id right.context_id
  && Transcript.Scope.equal
       (O.Attempt_record.scope left.record)
       (O.Attempt_record.scope right.record)
  && O.Observation_id.equal
       (O.Attempt_record.accounting_id left.record)
       (O.Attempt_record.accounting_id right.record)
  && O.Configuration.equal
       (O.Attempt_record.configuration left.record)
       (O.Attempt_record.configuration right.record)
;;

let with_record t record ~limits =
  let* next =
    create
      ~ordinal:t.ordinal
      ~owner:t.owner
      ~capture_fingerprint:t.capture_fingerprint
      ~job_id:t.job_id
      ~job_attempt:t.job_attempt
      ~context_id:t.context_id
      ~record
      ~limits
  in
  if same_identity t next
  then Ok next
  else corrupt "developer inference row identity changed"
;;

let exact_fields json names =
  match json with
  | `Object fields ->
    if
      List.length fields = List.length names
      && List.for_all fields ~f:(fun (name, _) -> List.mem names name ~equal:String.equal)
    then Ok ()
    else corrupt "developer inference row fields are missing or unexpected"
  | _ -> corrupt "developer inference row is not an object"
;;

let required json name decode = F.required json name decode |> F.store
let protocol decode json = decode json |> F.protocol

let of_json json ~limits =
  let open Result.Let_syntax in
  let%bind () =
    exact_fields
      json
      [ "ordinal"
      ; "host_id"
      ; "run_id"
      ; "epoch"
      ; "principal_id"
      ; "capture_fingerprint"
      ; "job_id"
      ; "job_attempt"
      ; "context_id"
      ; "record"
      ]
  in
  let%bind ordinal = required json "ordinal" F.decimal in
  let%bind host_id = required json "host_id" (protocol P.Id.Server.of_json) in
  let%bind run_id = required json "run_id" (protocol P.Id.Developer_run.of_json) in
  let%bind epoch = required json "epoch" F.decimal in
  let%bind principal_id =
    required json "principal_id" (protocol P.Id.Principal.of_json)
  in
  let%bind capture_fingerprint =
    required json "capture_fingerprint" F.digest
    |> Result.bind ~f:(fun hex ->
      Chatml_source.Digest.of_hex hex
      |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid capture digest"))
  in
  let%bind job_id = required json "job_id" (protocol P.Id.Job.of_json) in
  let%bind job_attempt = required json "job_attempt" F.decimal in
  let%bind context_id =
    required json "context_id" F.string
    |> Result.bind ~f:(fun text ->
      O.Observation_id.of_string text
      |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid context ID"))
  in
  let%bind record =
    required json "record" (fun json ->
      O.Attempt_record.of_json json ~limits:O.Admission.attempt
      |> Result.map_error ~f:(fun _ ->
        D.Error.Invalid_field
          { path = [ "record" ]; reason = "invalid inference attempt" }))
  in
  if
    Int64.(epoch < zero || epoch > of_int Int.max_value)
    || Int64.(job_attempt <= zero || job_attempt > of_int Int.max_value)
  then corrupt "developer inference coordinate exceeds int range"
  else (
    let%bind owner =
      P.Execution_owner.developer
        ~host_id
        ~run_id
        ~epoch:(Int64.to_int_exn epoch)
        ~principal_id
      |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid inference owner")
    in
    match owner with
    | P.Execution_owner.Developer owner ->
      create
        ~ordinal
        ~owner
        ~capture_fingerprint
        ~job_id
        ~job_attempt:(Int64.to_int_exn job_attempt)
        ~context_id
        ~record
        ~limits
    | Conversation _ -> corrupt "invalid inference owner constructor")
;;
