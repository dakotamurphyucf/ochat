open! Core
module L = Agent_session.Inference_ledger
module O = Inference.Observation
module R = Inference.Request
module Q = Agent_protocol.Inference_query
module P = Agent_protocol
module D = Document_schema

let ledger_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (L.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let observation_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (O.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let protocol_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (P.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let document_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let bounds bytes = Transcript.Admission.limits ~max_bytes:bytes |> document_ok
let sid = P.Id.Session.of_string "ses_ledger" |> protocol_ok

let configuration =
  let target =
    R.Target.create
      ~adapter:"synthetic"
      ~profile:"selected"
      ~profile_revision:None
      ~account:(Some "safe-account")
      ~endpoint:"https://private.example.test"
      ~model:"model"
      ~settings:[]
      ~limits:Transcript.Admission.default
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (R.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  O.Configuration.of_target
    target
    ~preparation_id:"prepared-1"
    ~transport:Http_sse
    ~capabilities:[]
    ~limits:O.Admission.observation
  |> observation_ok
;;

let limits ?(attempts = 4) ?(turns = 4) ?(bytes = 256 * 1024) () =
  L.Limits.create
    ~max_attempts:attempts
    ~max_turns:turns
    ~max_retained_bytes:bytes
    ~document_limits:(bounds (1024 * 1024))
  |> ledger_ok
;;

let ledger ?(limits = limits ()) () =
  L.create ~session_id:sid ~generation:0 ~before_tracking_unknown:false ~limits
  |> ledger_ok
;;

let admit ledger =
  L.admit
    ledger
    ~source:(Transcript.Source_id.of_string "dispatch" |> Result.ok_or_failwith)
    ~relation:Root
    ~operation_id:None
    ~invocation_id:None
    ~configuration
  |> ledger_ok
;;

let actual n = O.Count.create (Actual n) |> observation_ok
let unknown reason = O.Count.create (Unknown reason) |> observation_ok

let usage handle ~revision n =
  let value =
    O.Usage.create
      ~counts:
        { input = actual n
        ; output = actual 0L
        ; reported_total = unknown Not_reported
        ; cached_input = unknown Explicit_null
        ; cache_write_input = unknown Not_reported
        ; reasoning_output = unknown Not_reported
        }
      ~inclusions:[]
    |> observation_ok
  in
  O.create
    ~scope:(L.Handle.scope handle)
    ~id:(L.Handle.accounting_id handle)
    ~revision
    ~payload:(Usage value)
    ~limits:O.Admission.observation
  |> observation_ok
;;

let interrupted =
  O.Attempt_record.Interrupted { reason = Cancelled; delivery = Possibly_submitted }
;;

let replace json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal key name then value else old))
  | _ -> failwith "expected object"
;;

let add json name value =
  match json with
  | `Object fields -> `Object (fields @ [ name, value ])
  | _ -> failwith "expected object"
;;

let document_json ledger = D.Document.json (L.to_document ledger |> ledger_ok)

let roundtrip ledger ~limits =
  L.of_document (L.to_document ledger |> ledger_ok) ~limits |> ledger_ok
;;

let patch_rows json ~f =
  match D.Json.field json ~name:"payload" with
  | Value payload ->
    (match D.Json.field payload ~name:"rows" with
     | Value (`Array rows) ->
       replace json "payload" (replace payload "rows" (`Array (List.map rows ~f)))
     | _ -> failwith "rows")
  | _ -> failwith "payload"
;;

let captured json ~limits =
  L.of_document
    (D.Document.inspect ~limits:(bounds (1024 * 1024)) json |> document_ok)
    ~limits
  |> ledger_ok
;;
