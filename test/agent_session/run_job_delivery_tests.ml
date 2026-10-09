open! Core
module A = Agent_session
module P = Agent_protocol
module Carrier = A.Run_job_delivery
module Frame = Chat_response.Background_delivery

let get = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let maximum_id prefix = prefix ^ "_" ^ String.make (95 - String.length prefix) 'x'

let%expect_test
    "canonical artifact carrier bounds include twice escaped source and final custody"
  =
  let session_id = P.Id.Session.of_string (maximum_id "ses") |> get in
  let job_id = P.Id.Job.of_string (maximum_id "job") |> get in
  let run_id = P.Id.Run.of_string (maximum_id "run") |> get in
  let source =
    P.Run_source.create
      ~observer:{ script_id = String.make 256 '\001'; source_sha256 = String.make 64 'f' }
      ~generation:Int.max_value
      ~installation_epoch:Int64.max_value
    |> get
  in
  let now = P.Timestamp.of_string "2026-10-09T00:00:00.999999999Z" |> get in
  let work =
    P.Run_work.create
      ~key:(Retained (Job { id = job_id; attempt = Int.max_value }))
      ~generation:source.generation
    |> get
  in
  let wake =
    P.Run_wake.create
      ~run_id
      ~source
      ~occurrence:(Job_completion { job_id; attempt = Int.max_value })
    |> get
  in
  let run =
    P.Run.create
      ~id:run_id
      ~session:
        (P.Session_ref.create
           ~server_id:(P.Id.Server.of_string "srv_fixture" |> get)
           ~session_id)
      ~principal_id:(P.Id.Principal.of_string "pri_fixture" |> get)
      ~source
      ~mode:Workflow
      ~lifecycle:(Waiting wake)
      ~revision:0L
      ~owned_work:[ work ]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:now
      ~updated_at:now
    |> get
  in
  let blob =
    P.Blob.Metadata.create
      ~id:(P.Id.Blob.of_string (maximum_id "blb") |> get)
      ~kind:File
      ~media_type:P.Job_artifact.media_type
      ~byte_length:Int64.max_value
      ~digest:(String.make 64 'f')
      ~display_name:"job-result.json"
      ()
    |> get
  in
  let reference =
    P.Job_artifact.create
      ~session_id
      ~job_id
      ~generation:source.generation
      ~attempt:Int.max_value
      ~blob
    |> get
  in
  let make_frame result =
    Frame.of_json
      (`Object
          [ "session_id", P.Id.Session.to_json session_id
          ; "job_id", P.Id.Job.to_json job_id
          ; "generation", `String (Int.to_string source.generation)
          ; "attempt", `String (Int.to_string Int.max_value)
          ; "script_id", `String source.observer.script_id
          ; "source_sha256", `String source.observer.source_sha256
          ; "completed_at", P.Timestamp.to_json now
          ; "result", result
          ])
    |> function
    | Ok frame -> frame
    | Error message -> failwith message
  in
  let frame =
    make_frame (P.Stored_completion.to_json (Artifact { outcome = Cancelled; reference }))
  in
  let pending = Carrier.capture run ~frame |> get in
  let claimed =
    Carrier.enqueue pending ~at:now
    |> get
    |> Carrier.claim
         ~execution_id:(P.Id.Moderator_execution.of_string (maximum_id "mex") |> get)
    |> get
  in
  let retired = Carrier.retire claimed ~reason:Authorization_lost in
  let bytes = Carrier.to_jsonaf retired |> Jsonaf.to_string |> String.length in
  let oversized =
    make_frame (P.Completion.to_json (Succeeded (`String (String.make 8192 '\001'))))
  in
  let inline_frame size =
    make_frame (P.Completion.to_json (Succeeded (`String (String.make size 'x'))))
  in
  let empty_inline = Carrier.capture run ~frame:(inline_frame 0) |> get in
  let payload_bytes =
    Carrier.max_encoded_bytes
    - Carrier.disposition_reserve_bytes empty_inline
    - (Carrier.to_jsonaf empty_inline |> Jsonaf.to_string |> String.length)
  in
  let boundary = Carrier.capture run ~frame:(inline_frame payload_bytes) |> get in
  let boundary_enqueued = Carrier.enqueue boundary ~at:now |> get in
  let boundary_claimed =
    Carrier.claim
      boundary_enqueued
      ~execution_id:(P.Id.Moderator_execution.of_string (maximum_id "mex") |> get)
    |> get
  in
  let boundary_retired = Carrier.retire boundary_claimed ~reason:Authorization_lost in
  let boundary_decode_rejected =
    let json = Carrier.to_jsonaf boundary in
    match json with
    | `Object fields ->
      let frame = Frame.to_json (inline_frame (payload_bytes + 1)) |> Jsonaf.to_string in
      Carrier.of_jsonaf
        (`Object (List.Assoc.add fields ~equal:String.equal "frame" (`String frame)))
      |> Result.is_error
    | _ -> failwith "expected carrier fixture object"
  in
  let receipt =
    P.Run_receipt.create
      ~run_id
      ~principal_id:run.principal_id
      ~source
      ~key:(P.Idempotency_key.of_string "wait" |> get)
      ~request_sha256:(String.make 64 'a')
      ~kind:Action
      ~run_revision:run.revision
      ~session_revision:0L
      ~committed_at:now
    |> get
  in
  let intent =
    A.Run_intent.create
      ~receipt
      ~execution_id:(P.Id.Moderator_execution.of_string "mex_wait" |> get)
      ~action:(Wait wake)
    |> get
  in
  let raw_index =
    `Object
      [ ( "installation"
        , `Object
            [ "epoch", `String (Int64.to_string source.installation_epoch)
            ; ( "source"
              , `Object
                  [ "script_id", `String source.observer.script_id
                  ; "source_sha256", `String source.observer.source_sha256
                  ] )
            ] )
      ; "runs", `Array [ P.Run.to_json run ]
      ; "receipts", `Array [ P.Run_receipt.to_json receipt ]
      ; "intents", `Array [ A.Run_intent.to_jsonaf intent ]
      ]
  in
  let index = A.Run_state.of_jsonaf raw_index |> get in
  let oversized_future_index =
    match raw_index with
    | `Object fields ->
      let empty = `Object (fields @ [ "future_metadata", `String "" ]) in
      let padding =
        P.Run_limits.max_document_bytes
        - Carrier.reservation_bytes
        - (Jsonaf.to_string empty |> String.length)
        + 1
      in
      `Object (fields @ [ "future_metadata", `String (String.make padding 'x') ])
    | _ -> failwith "expected run index fixture object"
  in
  print_s
    [%sexp
      { bounded = (bytes <= Carrier.max_encoded_bytes : bool)
      ; reserved = (Carrier.max_encoded_bytes < Carrier.reservation_bytes : bool)
      ; round_trip =
          (Carrier.equal retired (Carrier.to_jsonaf retired |> Carrier.of_jsonaf |> get)
           : bool)
      ; duplicate_claim_rejected =
          (Result.is_error
             (Carrier.claim
                claimed
                ~execution_id:(P.Id.Moderator_execution.of_string "mex_duplicate" |> get))
           : bool)
      ; oversized_capture_rejected =
          (Result.is_error (Carrier.capture run ~frame:oversized) : bool)
      ; near_boundary_retires =
          (Result.is_ok (Carrier.of_jsonaf (Carrier.to_jsonaf boundary_retired)) : bool)
      ; boundary_decode_rejected : bool
      ; future_metadata_charged =
          (Result.is_error
             (A.Run_state.validate_encoded_capacity index oversized_future_index)
           && Result.is_error (A.Run_state.of_jsonaf oversized_future_index)
           : bool)
      }];
  [%expect
    {|
    ((bounded true) (reserved true) (round_trip true)
     (duplicate_claim_rejected true) (oversized_capture_rejected true)
     (near_boundary_retires true) (boundary_decode_rejected true)
     (future_metadata_charged true))
    |}]
;;
