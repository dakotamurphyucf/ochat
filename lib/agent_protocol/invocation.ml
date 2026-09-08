open Core
module Error = Protocol_error
open Extension_codec

(* Invocation identity/outcomes preserve exact JSON structure, including field order. *)
module Jsonaf = struct
  include Jsonaf

  let equal = exactly_equal
end

type origin =
  | Model
  | Moderator
  | Script
  | Delegated_agent
  | External_adapter
[@@deriving compare, equal, sexp]

type work =
  | Job of Id.Job.t
  | Subscription of Id.Subscription.t
[@@deriving compare, equal, sexp]

type tool_error =
  { code : string
  ; message : string
  ; retryable : bool
  ; details : Jsonaf.t
  }
[@@deriving equal, sexp]

type outcome =
  | Complete of Jsonaf.t
  | Pending of work * Jsonaf.t
  | Fail of tool_error
  | Cancelled of string
[@@deriving equal, sexp]

type context =
  { id : Id.Invocation.t
  ; session_id : Id.Session.t
  ; generation : int
  ; origin : origin
  ; provider_call_id : string option
  ; call_entry_id : History.Id.t option [@sexp.option]
  ; parent_invocation : Id.Invocation.t option
  ; parent_job : Id.Job.t option
  ; tool_name : string
  ; implementation_revision : string
  ; capability_fingerprint : string
  ; input : Jsonaf.t
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t option
  }
[@@deriving equal, sexp]

type status =
  | Admitted
  | Dispatching
  | Resolved of outcome
  | Published of outcome
[@@deriving equal, sexp]

type call_kind =
  | Function
  | Custom
[@@deriving sexp, equal]

type payload_fingerprint =
  { sha256 : string
  ; byte_length : int
  }
[@@deriving sexp, equal]

type preparation =
  | Passed
  | Invalid_input
  | Pre_tool_rejected
  | Pre_tool_failed
  | Session_ended
[@@deriving equal, sexp]

type routing =
  { kind : call_kind
  ; original_name : string
  ; original_payload : payload_fingerprint
  ; final_payload : payload_fingerprint
  ; canonical_payload : payload_fingerprint option [@sexp.option]
  ; preparation : preparation
  }
[@@deriving equal, sexp]

type observer =
  { script_id : string
  ; source_sha256 : string
  }
[@@deriving equal, sexp]

type observation_status =
  | Awaiting
  | Observing
  | Observed
  | Observation_failed of string
[@@deriving equal, sexp]

type follow_up =
  { request_turn : bool
  ; request_compaction : bool
  ; end_session : string option
  }
[@@deriving equal, sexp]

type follow_up_status =
  | Pending_follow_up of follow_up
  | Compaction_accepted_follow_up of follow_up
  | Applied_follow_up of follow_up
  | Discarded_follow_up of follow_up * string
[@@deriving equal, sexp]

type observation =
  { observer : observer
  ; status : observation_status
  ; follow_up : follow_up_status option [@sexp.option]
  }
[@@deriving equal, sexp]

type t =
  { context : context
  ; status : status
  ; output_entry_id : History.Id.t option [@sexp.option]
  ; routing : routing option [@sexp.option]
  ; publication_discarded : string option [@sexp.option]
  ; observation : observation option [@sexp.option]
  }
[@@deriving equal, sexp]

let invalid message = Error (Error.invalid_request message)
let failure code message = Error (Error.create code ~message ~retryable:false ())

let validate_work = function
  | Job id -> validate_id Id.Job.to_json Id.Job.of_json id
  | Subscription id -> validate_id Id.Subscription.to_json Id.Subscription.of_json id
;;

let validate_outcome = function
  | Complete value -> validate_json value
  | Pending (work, value) ->
    let open Result.Let_syntax in
    let%bind () = validate_work work in
    validate_json value
  | Cancelled reason -> text ~name:"cancellation reason" ~max:16384 reason
  | Fail error ->
    let open Result.Let_syntax in
    let%bind () = text ~name:"tool error code" ~max:128 error.code in
    let%bind () = text ~name:"tool error message" ~max:16384 error.message in
    validate_json error.details
;;

let validate_context context =
  let open Result.Let_syntax in
  let%bind () = validate_id Id.Invocation.to_json Id.Invocation.of_json context.id in
  let%bind () = validate_id Id.Session.to_json Id.Session.of_json context.session_id in
  let%bind () =
    match context.parent_invocation with
    | None -> Ok ()
    | Some id -> validate_id Id.Invocation.to_json Id.Invocation.of_json id
  in
  let%bind () =
    match context.parent_job with
    | None -> Ok ()
    | Some id -> validate_id Id.Job.to_json Id.Job.of_json id
  in
  let%bind () =
    if context.generation < 0 then invalid "negative invocation generation" else Ok ()
  in
  let%bind () = text ~name:"tool name" ~max:256 context.tool_name in
  let%bind () =
    text ~name:"implementation revision" ~max:256 context.implementation_revision
  in
  let%bind () =
    text ~name:"capability fingerprint" ~max:256 context.capability_fingerprint
  in
  let%bind () =
    match context.origin, context.provider_call_id with
    | Model, Some value -> text ~name:"provider call ID" ~max:1024 value
    | Model, None -> invalid "model invocation requires a provider call ID"
    | _, Some _ -> invalid "non-model invocation cannot carry a provider call ID"
    | _, None -> Ok ()
  in
  let%bind () =
    match context.origin, context.call_entry_id with
    | Model, Some id -> validate_id History.Id.to_json History.Id.of_json id
    | _, None -> Ok ()
    | _, Some _ -> invalid "non-model invocation cannot bind a canonical call"
  in
  let%bind () =
    if
      Option.exists context.parent_invocation ~f:(fun id ->
        Id.Invocation.compare context.id id = 0)
    then invalid "invocation cannot be its own parent"
    else Ok ()
  in
  let%bind () =
    if
      Option.exists context.deadline ~f:(fun deadline ->
        Timestamp.compare deadline context.created_at < 0)
    then invalid "invocation deadline precedes creation"
    else Ok ()
  in
  validate_json context.input
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = validate_context t.context in
  let%bind () =
    match t.observation with
    | None -> Ok ()
    | Some observation ->
      let%bind () =
        text ~name:"observer script ID" ~max:256 observation.observer.script_id
      in
      let digest = observation.observer.source_sha256 in
      let%bind () =
        if
          String.length digest = 64
          && String.for_all digest ~f:(function
            | '0' .. '9' | 'a' .. 'f' -> true
            | _ -> false)
        then Ok ()
        else invalid "observer source digest must be a lowercase SHA256"
      in
      let%bind () =
        match t.context.origin, t.context.parent_invocation with
        | Moderator, Some _ -> Ok ()
        | _ -> invalid "deferred observation requires a nested moderator invocation"
      in
      let%bind () =
        match t.status, observation.status with
        | (Admitted | Dispatching), (Observing | Observed | Observation_failed _) ->
          invalid "observation handling requires a recorded invocation outcome"
        | _ -> Ok ()
      in
      let%bind () =
        match observation.follow_up, observation.status with
        | None, _ -> Ok ()
        | ( Some
              ( Pending_follow_up requests
              | Applied_follow_up requests
              | Compaction_accepted_follow_up requests
              | Discarded_follow_up (requests, _) )
          , Observed ) ->
          let%bind () =
            match observation.follow_up with
            | Some (Compaction_accepted_follow_up _)
              when not
                     (requests.request_compaction
                      && requests.request_turn
                      && Option.is_none requests.end_session) ->
              invalid "partial acceptance requires compaction followed by a turn"
            | Some (Discarded_follow_up (_, reason)) ->
              text ~name:"follow-up discard reason" ~max:1024 reason
            | _ -> Ok ()
          in
          let%bind () =
            if
              requests.request_turn
              || requests.request_compaction
              || Option.is_some requests.end_session
            then Ok ()
            else invalid "observation follow-up must request work"
          in
          (match requests.end_session with
           | None -> Ok ()
           | Some reason -> text ~name:"observation end-session reason" ~max:1024 reason)
        | Some _, _ -> invalid "follow-up intent requires an acknowledged observation"
      in
      (match observation.status with
       | Observation_failed reason -> text ~name:"observation failure" ~max:1024 reason
       | Awaiting | Observing | Observed -> Ok ())
  in
  let%bind () =
    match t.publication_discarded with
    | None -> Ok ()
    | Some reason ->
      let%bind () = text ~name:"publication discard reason" ~max:1024 reason in
      (match t.context.origin, t.status, t.output_entry_id with
       | Model, Resolved _, None -> Ok ()
       | _ ->
         invalid "discarded publication requires an unpublished resolved model invocation")
  in
  let%bind () =
    match t.routing with
    | None -> Ok ()
    | Some routing ->
      let fingerprint value =
        if
          value.byte_length < 0
          || String.length value.sha256 <> 64
          || not
               (String.for_all value.sha256 ~f:(function
                  | '0' .. '9' | 'a' .. 'f' -> true
                  | _ -> false))
        then invalid "invalid invocation payload fingerprint"
        else Ok ()
      in
      let%bind () = text ~name:"original tool name" ~max:256 routing.original_name in
      let%bind () = fingerprint routing.original_payload in
      let%bind () = fingerprint routing.final_payload in
      let%bind () =
        match routing.canonical_payload with
        | None -> Ok ()
        | Some value -> fingerprint value
      in
      let%bind () =
        match t.context.origin, t.context.call_entry_id, routing.canonical_payload with
        | Model, Some _, Some _ -> Ok ()
        | Model, _, _ ->
          invalid "model routing requires canonical call and payload bindings"
        | _, _, None -> Ok ()
        | _, _, Some _ -> invalid "non-model routing cannot bind a canonical payload"
      in
      (match routing.preparation with
       | Passed -> Ok ()
       | Session_ended ->
         (match t.status with
          | Resolved (Complete _ | Pending _) | Published (Complete _ | Pending _) ->
            invalid "stopped preparation cannot have a successful outcome"
          | _ -> Ok ())
       | Invalid_input | Pre_tool_rejected | Pre_tool_failed ->
         if
           not
             (String.equal routing.original_name t.context.tool_name
              && equal_payload_fingerprint routing.original_payload routing.final_payload
             )
         then invalid "rejected preparation cannot change the original target or payload"
         else (
           match t.status with
           | Resolved (Complete _ | Pending _) | Published (Complete _ | Pending _) ->
             invalid "rejected preparation cannot have a successful outcome"
           | _ -> Ok ()))
  in
  let%bind () =
    match t.status, t.context.call_entry_id, t.output_entry_id with
    | Published _, Some call_id, Some id ->
      if History.Id.compare call_id id = 0
      then invalid "call and output occurrences must differ"
      else validate_id History.Id.to_json History.Id.of_json id
    | Published _, Some _, None ->
      invalid "bound publication requires an output occurrence"
    | Published _, None, Some _ ->
      invalid "publication receipt requires a call occurrence"
    | (Admitted | Dispatching | Resolved _), _, Some _ ->
      invalid "unpublished invocation cannot carry an output occurrence"
    | _, _, None -> Ok ()
  in
  match t.status with
  | Admitted | Dispatching -> Ok ()
  | Resolved outcome | Published outcome -> validate_outcome outcome
;;

let create ?routing ?observer context =
  let t =
    { context
    ; status = Admitted
    ; output_entry_id = None
    ; routing
    ; publication_discarded = None
    ; observation =
        Option.map observer ~f:(fun observer ->
          { observer; status = Awaiting; follow_up = None })
    }
  in
  Result.map (validate t) ~f:(fun () -> t)
;;

let dispatch t =
  match t.status with
  | Admitted -> Ok { t with status = Dispatching }
  | Dispatching | Resolved _ | Published _ ->
    failure Invalid_state "invocation has already been dispatched or resolved"
;;

let resolve t ~session_id ~generation outcome =
  if Id.Session.compare session_id t.context.session_id <> 0
  then failure Permission_denied "invocation belongs to another session"
  else if generation <> t.context.generation
  then failure Conflict "invocation generation is stale"
  else (
    match t.status with
    | Dispatching ->
      let next = { t with status = Resolved outcome } in
      Result.map (validate next) ~f:(fun () -> next)
    | Admitted -> failure Invalid_state "invocation has not been dispatched"
    | Resolved _ | Published _ -> failure Already_resolved "invocation already resolved")
;;

let cancel t ~reason =
  match t.status with
  | Admitted | Dispatching ->
    let outcome = Cancelled reason in
    Result.map (validate_outcome outcome) ~f:(fun () ->
      { t with status = Resolved outcome })
  | Resolved _ | Published _ -> failure Already_resolved "invocation already resolved"
;;

let publish t =
  if Option.is_some t.publication_discarded
  then failure Invalid_state "publication was discarded"
  else if Option.is_some t.context.call_entry_id
  then
    failure
      Invalid_state
      "bound invocation publication requires a canonical output occurrence"
  else (
    match t.status with
    | Resolved outcome -> Ok { t with status = Published outcome }
    | Published _ -> Ok t
    | Admitted | Dispatching -> failure Invalid_state "invocation has no recorded outcome")
;;

let publish_with_history t ~output_entry_id =
  let open Result.Let_syntax in
  let%bind () = validate t in
  let%bind () =
    if Option.is_some t.publication_discarded
    then failure Invalid_state "publication was discarded"
    else Ok ()
  in
  let%bind () = validate_id History.Id.to_json History.Id.of_json output_entry_id in
  if
    Option.exists t.context.call_entry_id ~f:(fun id ->
      History.Id.compare id output_entry_id = 0)
  then invalid "call and output occurrences must differ"
  else if Option.is_none t.context.call_entry_id
  then failure Invalid_state "invocation has no canonical call occurrence"
  else (
    match t.status, t.output_entry_id with
    | Resolved outcome, None ->
      Ok { t with status = Published outcome; output_entry_id = Some output_entry_id }
    | Published _, Some existing when History.Id.compare existing output_entry_id = 0 ->
      Ok t
    | Published _, _ -> failure Conflict "publication output occurrence is immutable"
    | _ -> failure Invalid_state "invocation has no recorded outcome")
;;

let discard_publication t ~reason =
  match t.publication_discarded with
  | Some existing when String.equal existing reason -> Ok t
  | Some _ -> failure Conflict "publication discard reason is immutable"
  | None ->
    let next = { t with publication_discarded = Some reason } in
    Result.map (validate next) ~f:(fun () -> next)
;;

let change_observation t ~status =
  match t.observation with
  | None -> failure Invalid_state "invocation has no deferred observation"
  | Some observation ->
    let next = { t with observation = Some { observation with status } } in
    Result.map (validate next) ~f:(fun () -> next)
;;

let claim_observation t =
  match t.observation with
  | Some { status = Awaiting; _ } -> change_observation t ~status:Observing
  | _ -> failure Invalid_state "observation is absent or already attempted"
;;

let complete_observation ?follow_up t =
  match t.observation with
  | Some ({ status = Observing; _ } as observation) ->
    let next =
      { t with
        observation =
          Some
            { observation with
              status = Observed
            ; follow_up =
                Option.map follow_up ~f:(fun requests -> Pending_follow_up requests)
            }
      }
    in
    Result.map (validate next) ~f:(fun () -> next)
  | _ -> failure Invalid_state "observation is not being handled"
;;

let apply_observation_follow_up t =
  match t.observation with
  | Some
      ({ status = Observed
       ; follow_up =
           Some (Pending_follow_up requests | Compaction_accepted_follow_up requests)
       ; _
       } as observation) ->
    Ok
      { t with
        observation =
          Some { observation with follow_up = Some (Applied_follow_up requests) }
      }
  | Some { status = Observed; follow_up = Some (Applied_follow_up _); _ } -> Ok t
  | _ -> failure Invalid_state "observation has no acknowledged follow-up intent"
;;

let change_follow_up t follow_up =
  match t.observation with
  | Some observation ->
    let next =
      { t with observation = Some { observation with follow_up = Some follow_up } }
    in
    Result.map (validate next) ~f:(fun () -> next)
  | None -> failure Invalid_state "invocation has no observation"
;;

let accept_observation_compaction t =
  match t.observation with
  | Some { status = Observed; follow_up = Some (Pending_follow_up requests); _ } ->
    change_follow_up t (Compaction_accepted_follow_up requests)
  | Some { status = Observed; follow_up = Some (Compaction_accepted_follow_up _); _ } ->
    Ok t
  | _ -> failure Invalid_state "observation has no pending compaction and turn"
;;

let discard_observation_follow_up t ~reason =
  match t.observation with
  | Some
      { status = Observed
      ; follow_up =
          Some (Pending_follow_up requests | Compaction_accepted_follow_up requests)
      ; _
      } -> change_follow_up t (Discarded_follow_up (requests, reason))
  | Some { status = Observed; follow_up = Some (Discarded_follow_up (_, existing)); _ }
    when String.equal existing reason -> Ok t
  | _ -> failure Invalid_state "observation has no pending follow-up to discard"
;;

let fail_observation t ~reason =
  match t.observation with
  | Some { status = Awaiting | Observing; _ } ->
    change_observation t ~status:(Observation_failed reason)
  | Some { status = Observation_failed existing; _ } when String.equal existing reason ->
    Ok t
  | _ -> failure Invalid_state "observation is absent or already finished"
;;

let validate_observation_transition previous next =
  match previous.observation, next.observation with
  | None, None -> Ok ()
  | Some old, Some current when equal_observer old.observer current.observer ->
    let open Result.Let_syntax in
    let%bind () =
      match old.follow_up, current.follow_up with
      | old, current when Option.equal equal_follow_up_status old current -> Ok ()
      | None, Some (Pending_follow_up _)
        when equal_observation_status old.status Observing
             && equal_observation_status current.status Observed -> Ok ()
      | Some (Pending_follow_up before), Some (Compaction_accepted_follow_up after)
      | ( Some (Pending_follow_up before | Compaction_accepted_follow_up before)
        , Some (Applied_follow_up after | Discarded_follow_up (after, _)) )
        when equal_follow_up before after && equal_status previous.status next.status ->
        Ok ()
      | _ ->
        failure Conflict "observation follow-up intent is immutable and cannot be rearmed"
    in
    (match old.status, current.status with
     | old, current when equal_observation_status old current -> Ok ()
     | Awaiting, (Observing | Observation_failed _)
     | Observing, (Observed | Observation_failed _) ->
       if equal_status previous.status next.status
       then Ok ()
       else failure Conflict "observation transition must preserve the invocation outcome"
     | _ -> failure Conflict "observation handling cannot be repeated or rewritten")
  | _ -> failure Conflict "observation owner and admission intent are immutable"
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = validate next in
  match previous with
  | None ->
    (match next.status with
     | Admitted -> Ok ()
     | _ -> failure Invalid_state "new invocation must be admitted")
  | Some previous ->
    let%bind () = validate previous in
    let%bind () = validate_observation_transition previous next in
    if not (equal_context previous.context next.context)
    then failure Conflict "invocation context is immutable"
    else if not (Option.equal equal_routing previous.routing next.routing)
    then failure Conflict "invocation routing provenance is immutable"
    else if
      Option.is_some previous.publication_discarded
      && not
           (Option.equal
              String.equal
              previous.publication_discarded
              next.publication_discarded)
    then failure Conflict "publication discard disposition is immutable"
    else if
      Option.is_some previous.output_entry_id
      && not
           (Option.equal
              (fun a b -> History.Id.compare a b = 0)
              previous.output_entry_id
              next.output_entry_id)
    then failure Conflict "publication output occurrence is immutable"
    else (
      match previous.status, next.status with
      | Resolved old, Resolved current
        when equal_outcome old current
             && (Option.is_some next.publication_discarded
                 || not
                      (Option.equal
                         equal_observation
                         previous.observation
                         next.observation)) -> Ok ()
      | Admitted, Dispatching | Admitted, Resolved (Cancelled _) | Dispatching, Resolved _
        -> Ok ()
      | (Resolved old, Published current | Published old, Published current)
        when equal_outcome old current -> Ok ()
      | Resolved _, _ | Published _, _ ->
        failure Already_resolved "recorded invocation outcome cannot be replaced"
      | _ -> failure Invalid_state "invalid invocation transition")
;;

let origin_values =
  [ "model", Model
  ; "moderator", Moderator
  ; "script", Script
  ; "delegated_agent", Delegated_agent
  ; "external_adapter", External_adapter
  ]
;;

let origin_to_json origin =
  `String
    (fst (List.find_exn origin_values ~f:(fun (_, value) -> equal_origin value origin)))
;;

let work_to_json = function
  | Job id -> `Object [ "type", `String "job"; "id", Id.Job.to_json id ]
  | Subscription id ->
    `Object [ "type", `String "subscription"; "id", Id.Subscription.to_json id ]
;;

let outcome_to_json = function
  | Complete value -> `Object [ "type", `String "complete"; "value", value ]
  | Pending (work, acknowledgement) ->
    `Object
      [ "type", `String "pending"
      ; "work", work_to_json work
      ; "acknowledgement", acknowledgement
      ]
  | Fail error ->
    `Object
      [ "type", `String "fail"
      ; "code", `String error.code
      ; "message", `String error.message
      ; ("retryable", if error.retryable then `True else `False)
      ; "details", error.details
      ]
  | Cancelled reason -> `Object [ "type", `String "cancelled"; "reason", `String reason ]
;;

let work_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () = closed fields [ "type"; "id" ] in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  match kind with
  | "job" ->
    Result.map (Json_codec.required_as fields "id" Id.Job.of_json) ~f:(fun id -> Job id)
  | "subscription" ->
    Result.map (Json_codec.required_as fields "id" Id.Subscription.of_json) ~f:(fun id ->
      Subscription id)
  | _ -> invalid "unknown invocation work type"
;;

let decode_outcome json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  match kind with
  | "complete" ->
    let%bind () = closed fields [ "type"; "value" ] in
    let%map value = Json_codec.required fields "value" in
    Complete value
  | "pending" ->
    let%bind () = closed fields [ "type"; "work"; "acknowledgement" ] in
    let%bind work = Json_codec.required_as fields "work" work_of_json in
    let%map acknowledgement = Json_codec.required fields "acknowledgement" in
    Pending (work, acknowledgement)
  | "fail" ->
    let%bind () = closed fields [ "type"; "code"; "message"; "retryable"; "details" ] in
    let%bind code = Json_codec.required_as fields "code" Json_codec.string in
    let%bind message = Json_codec.required_as fields "message" Json_codec.string in
    let%bind retryable = Json_codec.required_as fields "retryable" Json_codec.bool in
    let%map details = Json_codec.required fields "details" in
    Fail { code; message; retryable; details }
  | "cancelled" ->
    let%bind () = closed fields [ "type"; "reason" ] in
    let%map reason = Json_codec.required_as fields "reason" Json_codec.string in
    Cancelled reason
  | _ -> invalid "unknown invocation outcome"
;;

let outcome_of_json json =
  let open Result.Let_syntax in
  let%bind () = validate_json ~max_bytes:(9 * 1024 * 1024) ~max_depth:132 json in
  let%bind outcome = decode_outcome json in
  let%map () = validate_outcome outcome in
  outcome
;;

let optional name value encode =
  Option.to_list (Option.map value ~f:(fun value -> name, encode value))
;;

let fingerprint_to_json value =
  `Object
    [ "sha256", `String value.sha256
    ; "byte_length", `Number (Int.to_string value.byte_length)
    ]
;;

let fingerprint_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () = closed fields [ "sha256"; "byte_length" ] in
  let%bind sha256 = Json_codec.required_as fields "sha256" Json_codec.string in
  let%map byte_length =
    Json_codec.required_as
      fields
      "byte_length"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  { sha256; byte_length }
;;

let preparation_values =
  [ "passed", Passed
  ; "invalid_input", Invalid_input
  ; "pre_tool_rejected", Pre_tool_rejected
  ; "pre_tool_failed", Pre_tool_failed
  ; "session_ended", Session_ended
  ]
;;

let routing_to_json routing =
  `Object
    ([ ( "kind"
       , `String
           (match routing.kind with
            | Function -> "function"
            | Custom -> "custom") )
     ; "original_name", `String routing.original_name
     ; "original_payload", fingerprint_to_json routing.original_payload
     ; "final_payload", fingerprint_to_json routing.final_payload
     ; ( "preparation"
       , `String
           (fst
              (List.find_exn preparation_values ~f:(fun (_, value) ->
                 Sexp.equal
                   (sexp_of_preparation value)
                   (sexp_of_preparation routing.preparation)))) )
     ]
     @ optional "canonical_payload" routing.canonical_payload fingerprint_to_json)
;;

let routing_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    closed
      fields
      [ "kind"
      ; "original_name"
      ; "original_payload"
      ; "final_payload"
      ; "canonical_payload"
      ; "preparation"
      ]
  in
  let%bind kind =
    Json_codec.required_as
      fields
      "kind"
      (Json_codec.enum
         ~name:"invocation call kind"
         [ "function", Function; "custom", Custom ])
  in
  let%bind original_name =
    Json_codec.required_as fields "original_name" Json_codec.string
  in
  let%bind original_payload =
    Json_codec.required_as fields "original_payload" fingerprint_of_json
  in
  let%bind final_payload =
    Json_codec.required_as fields "final_payload" fingerprint_of_json
  in
  let%bind canonical_payload =
    Json_codec.optional_as fields "canonical_payload" fingerprint_of_json
  in
  let%map preparation =
    Json_codec.required_as
      fields
      "preparation"
      (Json_codec.enum ~name:"invocation preparation" preparation_values)
  in
  { kind; original_name; original_payload; final_payload; canonical_payload; preparation }
;;

let context_to_json context =
  `Object
    ([ "id", Id.Invocation.to_json context.id
     ; "session_id", Id.Session.to_json context.session_id
     ; "generation", `Number (Int.to_string context.generation)
     ; "origin", origin_to_json context.origin
     ; "tool_name", `String context.tool_name
     ; "implementation_revision", `String context.implementation_revision
     ; "capability_fingerprint", `String context.capability_fingerprint
     ; "input", context.input
     ; "created_at", Timestamp.to_json context.created_at
     ]
     @ optional "provider_call_id" context.provider_call_id (fun x -> `String x)
     @ optional "call_entry_id" context.call_entry_id History.Id.to_json
     @ optional "parent_invocation" context.parent_invocation Id.Invocation.to_json
     @ optional "parent_job" context.parent_job Id.Job.to_json
     @ optional "deadline" context.deadline Timestamp.to_json)
;;

let context_of_json ~version json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    closed
      fields
      ([ "id"
       ; "session_id"
       ; "generation"
       ; "origin"
       ; "provider_call_id"
       ; "parent_invocation"
       ; "parent_job"
       ; "tool_name"
       ; "implementation_revision"
       ; "capability_fingerprint"
       ; "input"
       ; "created_at"
       ; "deadline"
       ]
       @ if version >= 2 then [ "call_entry_id" ] else [])
  in
  let%bind id = Json_codec.required_as fields "id" Id.Invocation.of_json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind origin =
    Json_codec.required_as
      fields
      "origin"
      (Json_codec.enum ~name:"invocation origin" origin_values)
  in
  let%bind provider_call_id =
    Json_codec.optional_as fields "provider_call_id" Json_codec.string
  in
  let%bind call_entry_id =
    Json_codec.optional_as fields "call_entry_id" History.Id.of_json
  in
  let%bind parent_invocation =
    Json_codec.optional_as fields "parent_invocation" Id.Invocation.of_json
  in
  let%bind parent_job = Json_codec.optional_as fields "parent_job" Id.Job.of_json in
  let%bind tool_name = Json_codec.required_as fields "tool_name" Json_codec.string in
  let%bind implementation_revision =
    Json_codec.required_as fields "implementation_revision" Json_codec.string
  in
  let%bind capability_fingerprint =
    Json_codec.required_as fields "capability_fingerprint" Json_codec.string
  in
  let%bind input = Json_codec.required fields "input" in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%map deadline = Json_codec.optional_as fields "deadline" Timestamp.of_json in
  { id
  ; session_id
  ; generation
  ; origin
  ; provider_call_id
  ; call_entry_id
  ; parent_invocation
  ; parent_job
  ; tool_name
  ; implementation_revision
  ; capability_fingerprint
  ; input
  ; created_at
  ; deadline
  }
;;

let status_to_json = function
  | Admitted -> `Object [ "type", `String "admitted" ]
  | Dispatching -> `Object [ "type", `String "dispatching" ]
  | Resolved outcome ->
    `Object [ "type", `String "resolved"; "outcome", outcome_to_json outcome ]
  | Published outcome ->
    `Object [ "type", `String "published"; "outcome", outcome_to_json outcome ]
;;

let status_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  match kind with
  | "admitted" | "dispatching" ->
    let%map () = closed fields [ "type" ] in
    if String.equal kind "admitted" then Admitted else Dispatching
  | "resolved" | "published" ->
    let%bind () = closed fields [ "type"; "outcome" ] in
    let%map outcome = Json_codec.required_as fields "outcome" outcome_of_json in
    if String.equal kind "resolved" then Resolved outcome else Published outcome
  | _ -> invalid "unknown invocation status"
;;

let follow_up_to_json follow_up =
  let kind, requests, reason =
    match follow_up with
    | Pending_follow_up requests -> "pending", requests, None
    | Applied_follow_up requests -> "applied", requests, None
    | Compaction_accepted_follow_up requests -> "compaction_accepted", requests, None
    | Discarded_follow_up (requests, reason) -> "discarded", requests, Some reason
  in
  `Object
    ([ "type", `String kind
     ; ("request_turn", if requests.request_turn then `True else `False)
     ; ("request_compaction", if requests.request_compaction then `True else `False)
     ]
     @ optional "end_session" requests.end_session (fun value -> `String value)
     @ optional "reason" reason (fun value -> `String value))
;;

let follow_up_of_json ~version json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
  let%bind () =
    closed
      fields
      ([ "type"; "request_turn"; "request_compaction"; "end_session" ]
       @ if version >= 7 && String.equal kind "discarded" then [ "reason" ] else [])
  in
  let%bind request_turn = Json_codec.required_as fields "request_turn" Json_codec.bool in
  let%bind request_compaction =
    Json_codec.required_as fields "request_compaction" Json_codec.bool
  in
  let%bind end_session = Json_codec.optional_as fields "end_session" Json_codec.string in
  let requests = { request_turn; request_compaction; end_session } in
  match kind with
  | "pending" -> Ok (Pending_follow_up requests)
  | "applied" -> Ok (Applied_follow_up requests)
  | "compaction_accepted" when version >= 7 -> Ok (Compaction_accepted_follow_up requests)
  | "discarded" when version >= 7 ->
    let%map reason = Json_codec.required_as fields "reason" Json_codec.string in
    Discarded_follow_up (requests, reason)
  | _ -> invalid "unknown observation follow-up status"
;;

let observation_to_json (observation : observation) =
  let status, reason =
    match observation.status with
    | Awaiting -> "awaiting", None
    | Observing -> "observing", None
    | Observed -> "observed", None
    | Observation_failed reason -> "failed", Some reason
  in
  `Object
    ([ "script_id", `String observation.observer.script_id
     ; "source_sha256", `String observation.observer.source_sha256
     ; "status", `String status
     ]
     @ optional "reason" reason (fun value -> `String value)
     @ optional "follow_up" observation.follow_up follow_up_to_json)
;;

let observation_of_json ~version json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind script_id = Json_codec.required_as fields "script_id" Json_codec.string in
  let%bind source_sha256 =
    Json_codec.required_as fields "source_sha256" Json_codec.string
  in
  let%bind kind = Json_codec.required_as fields "status" Json_codec.string in
  let base_fields =
    [ "script_id"; "source_sha256"; "status" ]
    @ if version >= 6 then [ "follow_up" ] else []
  in
  let%bind follow_up =
    if version >= 6
    then
      Json_codec.required_as fields "follow_up" (follow_up_of_json ~version)
      |> Result.map ~f:Option.some
    else Ok None
  in
  let%map status =
    match kind with
    | "awaiting" | "observing" | "observed" ->
      let%map () = closed fields base_fields in
      (match kind with
       | "awaiting" -> Awaiting
       | "observing" -> Observing
       | _ -> Observed)
    | "failed" ->
      let%bind () = closed fields ("reason" :: base_fields) in
      let%map reason = Json_codec.required_as fields "reason" Json_codec.string in
      Observation_failed reason
    | _ -> invalid "unknown observation status"
  in
  { observer = { script_id; source_sha256 }; status; follow_up }
;;

let to_json t =
  `Object
    ([ ( "schema_version"
       , `Number
           (if
              Option.exists t.observation ~f:(fun observation ->
                match observation.follow_up with
                | Some (Compaction_accepted_follow_up _ | Discarded_follow_up _) -> true
                | _ -> false)
            then "7"
            else if
              Option.exists t.observation ~f:(fun observation ->
                Option.is_some observation.follow_up)
            then "6"
            else if Option.is_some t.observation
            then "5"
            else if Option.is_some t.publication_discarded
            then "4"
            else if Option.is_some t.routing
            then "3"
            else if Option.is_some t.context.call_entry_id
            then "2"
            else "1") )
     ; "context", context_to_json t.context
     ; "status", status_to_json t.status
     ]
     @ optional "output_entry_id" t.output_entry_id History.Id.to_json
     @ optional "routing" t.routing routing_to_json
     @ optional "publication_discarded" t.publication_discarded (fun reason ->
       `String reason)
     @ optional "observation" t.observation observation_to_json)
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = validate_json ~max_bytes:(18 * 1024 * 1024) ~max_depth:136 json in
  let%bind fields = Json_codec.fields json in
  let%bind version =
    Json_codec.required_as
      fields
      "schema_version"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind () =
    match version with
    | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> Ok ()
    | _ -> failure Incompatible_protocol "unsupported invocation schema version"
  in
  let%bind () =
    closed
      fields
      ([ "schema_version"; "context"; "status" ]
       @ (if version >= 2 then [ "output_entry_id" ] else [])
       @ (if version >= 3 then [ "routing" ] else [])
       @ (if version >= 4 then [ "publication_discarded" ] else [])
       @ if version >= 5 then [ "observation" ] else [])
  in
  let%bind context = Json_codec.required_as fields "context" (context_of_json ~version) in
  let%bind () =
    if version = 2 && Option.is_none context.call_entry_id
    then invalid "invocation schema 2 requires a canonical call occurrence"
    else Ok ()
  in
  let%bind status = Json_codec.required_as fields "status" status_of_json in
  let%bind output_entry_id =
    Json_codec.optional_as fields "output_entry_id" History.Id.of_json
  in
  let%bind routing =
    if version = 3
    then
      Json_codec.required_as fields "routing" routing_of_json |> Result.map ~f:Option.some
    else if version >= 4
    then Json_codec.optional_as fields "routing" routing_of_json
    else Ok None
  in
  let%bind publication_discarded =
    if version = 4
    then
      Json_codec.required_as fields "publication_discarded" Json_codec.string
      |> Result.map ~f:Option.some
    else if version >= 5
    then Json_codec.optional_as fields "publication_discarded" Json_codec.string
    else Ok None
  in
  let%bind observation =
    if version >= 5
    then
      Json_codec.required_as fields "observation" (observation_of_json ~version)
      |> Result.map ~f:Option.some
    else Ok None
  in
  let t =
    { context; status; output_entry_id; routing; publication_discarded; observation }
  in
  let%map () = validate t in
  t
;;
