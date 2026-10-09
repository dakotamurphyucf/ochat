open! Core
module P = Agent_protocol
module S = Agent_store
module D = Document_schema

let limits = D.Limits.default

let document = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let protocol = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let session_id = P.Id.Session.of_string "ses_run_versions" |> protocol
let timestamp = P.Timestamp.of_string "2026-10-09T00:00:00Z" |> protocol

let transaction version =
  let delta =
    D.Document.create
      ~limits
      ~kind:"session.delta"
      ~version
      ~payload:(`Object [ "changes", `Array [] ])
    |> document
  in
  S.Transaction.create
    ~limits
    ~session_id
    ~generation:0
    ~transaction_sequence:1L
    ~previous_transaction_hash:None
    ~session_revision:1L
    ~first_event_sequence:None
    ~last_event_sequence:None
    ~accepted_at_ns:0L
    ~command_audit:None
    ~delta
    ~durable_events:[]
;;

let snapshot version =
  let payload =
    `Object
      [ ( "identity"
        , `Object
            [ "session_id", P.Id.Session.to_json session_id; "generation", `String "0" ] )
      ; ( "counters"
        , `Object
            [ "revision", `String "0"
            ; "transaction_sequence", `String "0"
            ; "event_sequence", `String "0"
            ] )
      ; ( "spec"
        , `Object
            [ "prompt_revision_id", `String "prv_run_versions"
            ; "workspace_instance", `Object [ "conflict_domain", `String "workspace" ]
            ] )
      ]
  in
  let state =
    D.Document.create ~limits ~kind:"session.state" ~version ~payload |> document
  in
  S.Snapshot.create
    ~limits
    ~session_id
    ~transaction_sequence:0L
    ~transaction_hash:None
    ~event_sequence:0L
    ~created_at:timestamp
    ~prompt_artifact:"prv_run_versions"
    ~workspace_identity:"workspace"
    ~payload:state
;;

let%expect_test
    "persistence envelopes admit run schemas and reject unsupported successors"
  =
  print_s
    [%sexp
      { delta6_admitted = (Result.is_ok (transaction 6) : bool)
      ; state9_admitted = (Result.is_ok (snapshot 9) : bool)
      ; unknown_delta_rejected = (Result.is_error (transaction 999) : bool)
      ; unknown_state_rejected = (Result.is_error (snapshot 999) : bool)
      }];
  [%expect
    {|
    ((delta6_admitted true) (state9_admitted true) (unknown_delta_rejected true)
     (unknown_state_rejected true))
    |}]
;;
