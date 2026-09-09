open Core
open Extension_codec
module Error = Protocol_error

module Jsonaf = struct
  include Jsonaf

  let equal = exactly_equal
end

type phase =
  | Session_start
  | Session_resume
  | Turn_start
  | Message_appended
  | Pre_tool_call
  | Post_tool_response
  | Turn_end
  | Internal_event
[@@deriving equal, sexp]

type job_attempt =
  { job_id : Id.Job.t
  ; attempt : int
  ; deadline : Timestamp.t option [@sexp.option]
  }
[@@deriving equal, sexp]

type context =
  { id : Id.Moderator_execution.t
  ; session_id : Id.Session.t
  ; generation : int
  ; source : Invocation.observer
  ; operation_id : Id.Operation.t option
  ; job : job_attempt option [@sexp.option]
  ; phase : phase
  ; event : Jsonaf.t
  ; checkpoint_sha256 : string
  ; created_at : Timestamp.t
  }
[@@deriving equal, sexp]

type status =
  | Running
  | Completed of string
  | Failed of Invocation.tool_error
  | Interrupted of string
[@@deriving equal, sexp]

type intent =
  | Pending
  | Waiting_compaction of Id.Operation.t
  | Applied
  | Discarded of string
[@@deriving equal, sexp]

type retirement =
  { checkpoint_sha256 : string
  ; reason : string
  }
[@@deriving equal, sexp]

type t =
  { context : context
  ; status : status
  ; requests : Invocation.follow_up option
  ; intent : intent option
  ; compaction_operation_id : Id.Operation.t option
  ; retirement : retirement option [@sexp.option]
  }
[@@deriving equal, sexp]

let failure code message = Error (Error.create code ~message ~retryable:false ())

let digest value =
  match
    String.length value = 64
    && String.for_all value ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  with
  | true -> Ok ()
  | false -> invalid "moderator execution digest must be lowercase SHA256"
;;

let has_requests (r : Invocation.follow_up) =
  r.request_turn || r.request_compaction || Option.is_some r.end_session
;;

let validate t =
  let open Result.Let_syntax in
  let c = t.context in
  let%bind () =
    validate_id Id.Moderator_execution.to_json Id.Moderator_execution.of_json c.id
  in
  let%bind () = validate_id Id.Session.to_json Id.Session.of_json c.session_id in
  let%bind () =
    match c.operation_id with
    | None -> Ok ()
    | Some id -> validate_id Id.Operation.to_json Id.Operation.of_json id
  in
  let%bind () = if c.generation < 0 then invalid "negative event generation" else Ok () in
  let%bind () =
    match c.job, c.operation_id, c.phase with
    | None, _, _ -> Ok ()
    | Some job, None, (Pre_tool_call | Post_tool_response) when job.attempt > 0 ->
      validate_id Id.Job.to_json Id.Job.of_json job.job_id
    | _ ->
      invalid "job event requires a claimed attempt, tool phase and no model operation"
  in
  let%bind () = text ~name:"moderator script ID" ~max:256 c.source.script_id in
  let%bind () = digest c.source.source_sha256 in
  let%bind () = digest c.checkpoint_sha256 in
  let%bind () = validate_json ~max_bytes:(1024 * 1024) c.event in
  let%bind () =
    match t.retirement, c.phase, t.status with
    | None, _, _ -> Ok ()
    | Some retired, Internal_event, (Failed _ | Interrupted _) ->
      let%bind () = digest retired.checkpoint_sha256 in
      text ~name:"event retirement reason" ~max:1024 retired.reason
    | _ -> invalid "retirement requires a failed or interrupted internal event"
  in
  let%bind () =
    match t.status with
    | Running -> Ok ()
    | Completed checkpoint -> digest checkpoint
    | Interrupted reason -> text ~name:"interruption reason" ~max:1024 reason
    | Failed error ->
      Invocation.outcome_to_json (Fail error)
      |> Invocation.outcome_of_json
      |> Result.map ~f:ignore
  in
  let%bind () =
    match t.compaction_operation_id, t.intent, t.requests with
    | None, Some (Waiting_compaction _), _ -> invalid "missing compaction binding"
    | None, _, _ -> Ok ()
    | Some id, Some (Waiting_compaction bound), Some _
      when not (Id.Operation.equal id bound) ->
      invalid "compaction binding differs from intent"
    | Some id, Some (Waiting_compaction _ | Applied | Discarded _), Some requests
      when requests.request_turn
           && requests.request_compaction
           && Option.is_none requests.end_session ->
      validate_id Id.Operation.to_json Id.Operation.of_json id
    | _ -> invalid "invalid compaction binding"
  in
  match t.status, t.requests, t.intent with
  | _, None, None -> Ok ()
  | Completed _, Some requests, Some intent ->
    let%bind () =
      if has_requests requests then Ok () else invalid "empty runtime intent"
    in
    let%bind () =
      match requests.end_session with
      | None -> Ok ()
      | Some reason -> text ~name:"end-session reason" ~max:1024 reason
    in
    (match intent with
     | Pending | Applied -> Ok ()
     | Discarded reason -> text ~name:"intent discard reason" ~max:1024 reason
     | Waiting_compaction id ->
       let%bind () = validate_id Id.Operation.to_json Id.Operation.of_json id in
       if
         requests.request_turn
         && requests.request_compaction
         && Option.is_none requests.end_session
       then Ok ()
       else invalid "waiting compaction requires a dependent turn")
  | _ -> invalid "runtime intent requires a completed event and matching requests"
;;

let create context =
  let t =
    { context
    ; status = Running
    ; requests = None
    ; intent = None
    ; compaction_operation_id = None
    ; retirement = None
    }
  in
  Result.map (validate t) ~f:(fun () -> t)
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = validate next in
  match previous with
  | None ->
    (match next.status with
     | Running -> Ok ()
     | _ -> failure Invalid_state "event must start running")
  | Some previous ->
    let%bind () = validate previous in
    if not (equal_context previous.context next.context)
    then failure Conflict "event execution context is immutable"
    else if equal previous next
    then Ok ()
    else (
      let%bind () =
        match previous.retirement, next.retirement with
        | None, None -> Ok ()
        | None, Some _ when equal_status previous.status next.status -> Ok ()
        | Some before, Some after when equal_retirement before after -> Ok ()
        | _ -> failure Conflict "event retirement is immutable"
      in
      let%bind () =
        match previous.compaction_operation_id, next.compaction_operation_id with
        | None, None -> Ok ()
        | None, Some _
          when match previous.intent, next.intent with
               | Some Pending, Some (Waiting_compaction _) -> true
               | _ -> false -> Ok ()
        | Some before, Some after when Id.Operation.equal before after -> Ok ()
        | _ -> failure Conflict "compaction binding is immutable"
      in
      match previous.status, next.status with
      | (Failed _ | Interrupted _), _
        when equal_status previous.status next.status
             && Option.is_none previous.retirement
             && Option.is_some next.retirement -> Ok ()
      | Running, (Completed _ | Failed _ | Interrupted _) ->
        (match next.intent with
         | None | Some Pending -> Ok ()
         | _ -> failure Invalid_state "new intent must be pending")
      | Completed before, Completed after
        when String.equal before after
             && Option.equal Invocation.equal_follow_up previous.requests next.requests ->
        (match previous.intent, next.intent with
         | Some Pending, Some (Waiting_compaction _ | Applied | Discarded _)
         | Some (Waiting_compaction _), Some (Applied | Discarded _) -> Ok ()
         | _ -> failure Already_resolved "event intent cannot be replayed or rebound")
      | _ -> failure Already_resolved "event execution outcome is immutable")
;;

let change t next =
  Result.map (validate_transition ~previous:(Some t) next) ~f:(fun () -> next)
;;

let finish t next =
  match t.status with
  | Running -> change t next
  | Completed _ | Failed _ | Interrupted _ ->
    failure Already_resolved "event is already terminal"
;;

let complete t ~checkpoint_sha256 ~requests =
  let requests = Option.some_if (has_requests requests) requests in
  finish
    t
    { t with
      status = Completed checkpoint_sha256
    ; requests
    ; intent = Option.map requests ~f:(fun _ -> Pending)
    }
;;

let fail t error = finish t { t with status = Failed error }
let interrupt t ~reason = finish t { t with status = Interrupted reason }

let retire t ~checkpoint_sha256 ~reason =
  match t.status, t.retirement with
  | (Failed _ | Interrupted _), None ->
    change t { t with retirement = Some { checkpoint_sha256; reason } }
  | _ -> failure Already_resolved "event is not awaiting failed-head retirement"
;;

let accept_compaction t ~operation_id =
  match t.intent with
  | Some Pending ->
    change
      t
      { t with
        intent = Some (Waiting_compaction operation_id)
      ; compaction_operation_id = Some operation_id
      }
  | _ -> failure Already_resolved "event intent is not pending"
;;

let apply_intent t =
  match t.intent with
  | Some (Pending | Waiting_compaction _) -> change t { t with intent = Some Applied }
  | _ -> failure Already_resolved "event intent is already settled or absent"
;;

let discard_intent t ~reason =
  match t.intent with
  | Some (Pending | Waiting_compaction _) ->
    change t { t with intent = Some (Discarded reason) }
  | _ -> failure Already_resolved "event intent is already settled or absent"
;;

let phases =
  [ "session_start", Session_start
  ; "session_resume", Session_resume
  ; "turn_start", Turn_start
  ; "message_appended", Message_appended
  ; "pre_tool_call", Pre_tool_call
  ; "post_tool_response", Post_tool_response
  ; "turn_end", Turn_end
  ; "internal_event", Internal_event
  ]
;;

let bool value = if value then `True else `False

let optional key value encode =
  Option.to_list (Option.map value ~f:(fun v -> key, encode v))
;;

let status_to_json = function
  | Running -> `Object [ "kind", `String "running" ]
  | Completed checkpoint ->
    `Object [ "kind", `String "completed"; "checkpoint_sha256", `String checkpoint ]
  | Failed error ->
    `Object [ "kind", `String "failed"; "error", Invocation.outcome_to_json (Fail error) ]
  | Interrupted reason ->
    `Object [ "kind", `String "interrupted"; "reason", `String reason ]
;;

let intent_to_json = function
  | Pending -> `Object [ "kind", `String "pending" ]
  | Applied -> `Object [ "kind", `String "applied" ]
  | Waiting_compaction id ->
    `Object
      [ "kind", `String "waiting_compaction"; "operation_id", Id.Operation.to_json id ]
  | Discarded reason -> `Object [ "kind", `String "discarded"; "reason", `String reason ]
;;

let requests_to_json (r : Invocation.follow_up) =
  `Object
    ([ "request_turn", bool r.request_turn
     ; "request_compaction", bool r.request_compaction
     ]
     @ optional "end_session" r.end_session (fun s -> `String s))
;;

let to_json t =
  let c = t.context in
  `Object
    ([ ( "schema_version"
       , `Number
           (match c.job with
            | None -> "2"
            | Some _ -> "3") )
     ; "id", Id.Moderator_execution.to_json c.id
     ; "session_id", Id.Session.to_json c.session_id
     ; "generation", `Number (Int.to_string c.generation)
     ; "script_id", `String c.source.script_id
     ; "source_sha256", `String c.source.source_sha256
     ; ( "phase"
       , `String
           (List.find_exn phases ~f:(fun (_, phase) -> equal_phase phase c.phase) |> fst)
       )
     ; "event", c.event
     ; "checkpoint_sha256", `String c.checkpoint_sha256
     ; "created_at", Timestamp.to_json c.created_at
     ; "status", status_to_json t.status
     ]
     @ optional "operation_id" c.operation_id Id.Operation.to_json
     @ optional "job" c.job (fun job ->
       `Object
         ([ "job_id", Id.Job.to_json job.job_id
          ; "attempt", `Number (Int.to_string job.attempt)
          ]
          @ optional "deadline" job.deadline Timestamp.to_json))
     @ optional "requests" t.requests requests_to_json
     @ optional "intent" t.intent intent_to_json
     @ optional "compaction_operation_id" t.compaction_operation_id Id.Operation.to_json
     @ optional "retirement" t.retirement (fun retired ->
       `Object
         [ "checkpoint_sha256", `String retired.checkpoint_sha256
         ; "reason", `String retired.reason
         ]))
;;

let status_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "kind" Json_codec.string in
  match kind with
  | "running" ->
    let%map () = closed fields [ "kind" ] in
    Running
  | "completed" ->
    let%bind () = closed fields [ "kind"; "checkpoint_sha256" ] in
    let%map checkpoint =
      Json_codec.required_as fields "checkpoint_sha256" Json_codec.string
    in
    Completed checkpoint
  | "interrupted" ->
    let%bind () = closed fields [ "kind"; "reason" ] in
    let%map reason = Json_codec.required_as fields "reason" Json_codec.string in
    Interrupted reason
  | "failed" ->
    let%bind () = closed fields [ "kind"; "error" ] in
    let%bind error = Json_codec.required_as fields "error" Invocation.outcome_of_json in
    (match error with
     | Fail error -> Ok (Failed error)
     | _ -> invalid "expected event failure")
  | _ -> invalid "unknown event execution status"
;;

let intent_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "kind" Json_codec.string in
  match kind with
  | "pending" ->
    let%map () = closed fields [ "kind" ] in
    Pending
  | "applied" ->
    let%map () = closed fields [ "kind" ] in
    Applied
  | "waiting_compaction" ->
    let%bind () = closed fields [ "kind"; "operation_id" ] in
    let%map id = Json_codec.required_as fields "operation_id" Id.Operation.of_json in
    Waiting_compaction id
  | "discarded" ->
    let%bind () = closed fields [ "kind"; "reason" ] in
    let%map reason = Json_codec.required_as fields "reason" Json_codec.string in
    Discarded reason
  | _ -> invalid "unknown event intent state"
;;

let requests_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () = closed fields [ "request_turn"; "request_compaction"; "end_session" ] in
  let%bind request_turn = Json_codec.required_as fields "request_turn" Json_codec.bool in
  let%bind request_compaction =
    Json_codec.required_as fields "request_compaction" Json_codec.bool
  in
  let%map end_session = Json_codec.optional_as fields "end_session" Json_codec.string in
  Invocation.{ request_turn; request_compaction; end_session }
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = validate_json json in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    closed
      fields
      [ "schema_version"
      ; "id"
      ; "session_id"
      ; "generation"
      ; "script_id"
      ; "source_sha256"
      ; "operation_id"
      ; "job"
      ; "phase"
      ; "event"
      ; "checkpoint_sha256"
      ; "created_at"
      ; "status"
      ; "requests"
      ; "intent"
      ; "compaction_operation_id"
      ; "retirement"
      ]
  in
  let%bind version =
    Json_codec.required_as fields "schema_version" (Json_codec.bounded_int ~min:1 ~max:3)
  in
  let%bind () =
    match version, Option.is_some (Json_codec.optional fields "retirement") with
    | 1, true -> invalid "event retirement requires schema version 2"
    | _ -> Ok ()
  in
  let%bind id = Json_codec.required_as fields "id" Id.Moderator_execution.of_json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind script_id = Json_codec.required_as fields "script_id" Json_codec.string in
  let%bind source_sha256 =
    Json_codec.required_as fields "source_sha256" Json_codec.string
  in
  let%bind operation_id =
    Json_codec.optional_as fields "operation_id" Id.Operation.of_json
  in
  let%bind job =
    Json_codec.optional_as fields "job" (fun json ->
      let%bind () =
        if version < 3 then invalid "job event requires schema version 3" else Ok ()
      in
      let%bind fields = Json_codec.fields json in
      let%bind () = closed fields [ "job_id"; "attempt"; "deadline" ] in
      let%bind job_id = Json_codec.required_as fields "job_id" Id.Job.of_json in
      let%bind attempt =
        Json_codec.required_as
          fields
          "attempt"
          (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
      in
      let%map deadline = Json_codec.optional_as fields "deadline" Timestamp.of_json in
      { job_id; attempt; deadline })
  in
  let%bind phase =
    Json_codec.required_as
      fields
      "phase"
      (Json_codec.enum ~name:"moderator event phase" phases)
  in
  let%bind event = Json_codec.required_as fields "event" (fun json -> Ok json) in
  let%bind checkpoint_sha256 =
    Json_codec.required_as fields "checkpoint_sha256" Json_codec.string
  in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%bind status = Json_codec.required_as fields "status" status_of_json in
  let%bind requests = Json_codec.optional_as fields "requests" requests_of_json in
  let%bind intent = Json_codec.optional_as fields "intent" intent_of_json in
  let%bind compaction_operation_id =
    Json_codec.optional_as fields "compaction_operation_id" Id.Operation.of_json
  in
  let%bind retirement =
    Json_codec.optional_as fields "retirement" (fun json ->
      let%bind fields = Json_codec.fields json in
      let%bind () = closed fields [ "checkpoint_sha256"; "reason" ] in
      let%bind checkpoint_sha256 =
        Json_codec.required_as fields "checkpoint_sha256" Json_codec.string
      in
      let%map reason = Json_codec.required_as fields "reason" Json_codec.string in
      { checkpoint_sha256; reason })
  in
  let t =
    { context =
        { id
        ; session_id
        ; generation
        ; source = { script_id; source_sha256 }
        ; operation_id
        ; job
        ; phase
        ; event
        ; checkpoint_sha256
        ; created_at
        }
    ; status
    ; requests
    ; intent
    ; compaction_operation_id
    ; retirement
    }
  in
  let%map () = validate t in
  t
;;
