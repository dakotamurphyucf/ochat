open! Core
module P = Agent_protocol
module Execution = Recurrence_execution
module Attempt = Recurrence_execution_attempt
module Origin = Recurrence_run_origin
module Reference = Recurrence_run_reference

(* This module deliberately returns a presentation value, not an observation.
   In particular, missing retained detail cannot be used to authorize retry. *)
type t = Jsonaf.t

let to_json t = t
let decimal n = `String (Int64.to_string n)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let terminal_json = function
  | P.Run.Terminal.Completed _ -> `Object [ "kind", `String "completed" ]
  | Failed code ->
    `Object
      [ "kind", `String "failed"
      ; ( "code"
        , Option.value_map code ~default:`Null ~f:(fun code ->
            `String (P.Error.code_to_string code)) )
      ]
  | Cancelled -> `Object [ "kind", `String "cancelled" ]
  | Limited -> `Object [ "kind", `String "limited" ]
  | Interrupted -> `Object [ "kind", `String "interrupted" ]
;;

let outcome_json = function
  | Reference.Outcome.Completed -> `Object [ "kind", `String "completed" ]
  | Failed code ->
    `Object
      [ "kind", `String "failed"
      ; ( "code"
        , Option.value_map code ~default:`Null ~f:(fun code ->
            `String (P.Error.code_to_string code)) )
      ]
  | Cancelled -> `Object [ "kind", `String "cancelled" ]
  | Limited -> `Object [ "kind", `String "limited" ]
  | Interrupted -> `Object [ "kind", `String "interrupted" ]
;;

let lifecycle_json = function
  | P.Run.Lifecycle.Admitted -> `Object [ "kind", `String "admitted" ]
  | Active -> `Object [ "kind", `String "active" ]
  | Waiting _ -> `Object [ "kind", `String "waiting" ]
  | Terminal terminal -> terminal_json terminal
;;

let project (state : Session_state.t) ~intent ~attempt =
  let open Result.Let_syntax in
  let request = Execution.Attempt.request attempt in
  let session = Recurrence_execution_intent.target intent in
  let%bind () = Attempt.validate request ~intent in
  let%bind () =
    if P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
    then Ok ()
    else conflict "recurrence target read returned another session"
  in
  let index = Option.value state.run_state ~default:Run_state.empty in
  let%bind admission =
    Run_state.receipt
      index
      ~principal_id:(Attempt.principal_id request)
      ~key:(Attempt.key request)
      ~request_sha256:(Attempt.request_sha256 request)
  in
  let common =
    [ "visibility", `String "visible"
    ; "session", P.Session_ref.to_json session
    ; "session_revision", decimal state.counters.revision
    ]
  in
  match admission with
  | None -> Ok (`Object (common @ [ "detail", `String "not_retained_or_not_recorded" ]))
  | Some admission ->
    let%bind origin =
      match Run_state.recurrence_origin index admission.run_id with
      | Some origin -> Ok origin
      | None -> conflict "retained Run lacks its exact recurrence origin"
    in
    let%bind run =
      match Run_state.find index admission.run_id with
      | Some run -> Ok run
      | None -> conflict "recurrence origin lost its retained Run"
    in
    let%bind () = Origin.validate origin ~intent ~attempt:request in
    let%bind () = Origin.validate_run origin ~run ~admission in
    let%bind () =
      match Execution.Attempt.origin attempt with
      | None -> Ok ()
      | Some retained ->
        let%bind () = Origin.validate retained ~intent ~attempt:request in
        Origin.validate_run retained ~run ~admission
    in
    let%bind terminal =
      match run.lifecycle with
      | Admitted | Active | Waiting _ -> Ok None
      | Terminal _ ->
        let receipts =
          List.filter (Run_state.receipts index) ~f:(fun receipt ->
            P.Run_receipt.Kind.equal receipt.kind Terminal
            && P.Id.Run.equal receipt.run_id run.id
            && Int64.equal receipt.run_revision run.revision)
        in
        (match receipts with
         | [ receipt ] ->
           Result.map (Reference.create ~origin ~run ~receipt) ~f:Option.some
         | [] | _ :: _ :: _ -> conflict "terminal Run lacks one exact terminal receipt")
    in
    let%bind () =
      match Execution.Attempt.terminal attempt, terminal with
      | None, _ -> Ok ()
      | Some retained, Some actual ->
        Reference.validate retained ~origin ~run ~receipt:(Reference.receipt actual)
      | Some _, None ->
        conflict "controller terminal recurrence evidence lacks target terminal"
    in
    Ok
      (`Object
          (common
           @ [ "detail", `String "retained"
             ; "run_id", P.Id.Run.to_json run.id
             ; "run_revision", decimal run.revision
             ; "observed_run", lifecycle_json run.lifecycle
             ; ( "retained_outcome"
               , Option.value_map
                   (Execution.Attempt.terminal attempt)
                   ~default:`Null
                   ~f:(fun reference -> outcome_json (Reference.outcome reference)) )
             ]))
;;
