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

type handler_intent =
  { follow_up : follow_up_status
  ; compaction_operation_id : Id.Operation.t option [@sexp.option]
  }
[@@deriving equal, sexp]

type observation =
  { observer : observer
  ; status : observation_status
  ; follow_up : follow_up_status option [@sexp.option]
  ; compaction_operation_id : Id.Operation.t option [@sexp.option]
  }
[@@deriving equal, sexp]

type t =
  { context : context
  ; status : status
  ; output_entry_id : History.Id.t option [@sexp.option]
  ; routing : routing option [@sexp.option]
  ; publication_discarded : string option [@sexp.option]
  ; observation : observation option [@sexp.option]
  ; parent_event : Id.Moderator_execution.t option [@sexp.option]
  ; handler_intent : handler_intent option [@sexp.option]
  ; completion_contract : Completion_contract.t option [@sexp.option]
  ; authoring_reference : Authoring_reference.t option [@sexp.option]
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

let follow_up_requests = function
  | Pending_follow_up requests
  | Compaction_accepted_follow_up requests
  | Applied_follow_up requests
  | Discarded_follow_up (requests, _) -> requests
;;

let validate_handler_intent (intent : handler_intent) =
  let open Result.Let_syntax in
  let requests = follow_up_requests intent.follow_up in
  let%bind () =
    match
      requests.request_turn
      || requests.request_compaction
      || Option.is_some requests.end_session
    with
    | true -> Ok ()
    | false -> invalid "handler intent must request an action"
  in
  let%bind () =
    match requests.end_session with
    | None -> Ok ()
    | Some reason -> text ~name:"handler end-session reason" ~max:1024 reason
  in
  let%bind () =
    match intent.follow_up with
    | Discarded_follow_up (_, reason) ->
      text ~name:"handler discard reason" ~max:1024 reason
    | _ -> Ok ()
  in
  match intent.follow_up, intent.compaction_operation_id with
  | Compaction_accepted_follow_up _, None ->
    invalid "handler compaction requires its operation binding"
  | Pending_follow_up _, Some _ -> invalid "handler compaction binding requires admission"
  | _, Some id
    when requests.request_turn
         && requests.request_compaction
         && Option.is_none requests.end_session ->
    validate_id Id.Operation.to_json Id.Operation.of_json id
  | _, Some _ -> invalid "handler compaction requires a dependent turn"
  | _, None -> Ok ()
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = validate_context t.context in
  let%bind () =
    match t.handler_intent, t.status with
    | None, _ -> Ok ()
    | ( Some intent
      , ( Resolved (Complete _ | Fail _ | Pending _)
        | Published (Complete _ | Fail _ | Pending _) ) ) ->
      validate_handler_intent intent
    | Some _, _ -> invalid "handler intent requires a resolved noncancelled invocation"
  in
  let%bind () =
    match
      ( t.parent_event
      , t.context.origin
      , t.context.parent_invocation
      , t.context.parent_job
      , t.observation )
    with
    | None, _, _, _, _ -> Ok ()
    | Some id, Moderator, None, None, Some _ ->
      validate_id Id.Moderator_execution.to_json Id.Moderator_execution.of_json id
    | _ ->
      invalid
        "event-owned invocation requires moderator origin, one event parent and \
         observation intent"
  in
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
        match t.context.origin, t.context.parent_invocation, t.parent_event with
        | (Moderator | Script | Delegated_agent), Some _, None | Moderator, None, Some _
          -> Ok ()
        | _ ->
          invalid "deferred observation requires a nested script or moderator invocation"
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
      let%bind () =
        match observation.compaction_operation_id, observation.follow_up with
        | None, _ -> Ok ()
        | ( Some _
          , Some
              ( Compaction_accepted_follow_up requests
              | Applied_follow_up requests
              | Discarded_follow_up (requests, _) ) )
          when requests.request_compaction
               && requests.request_turn
               && Option.is_none requests.end_session -> Ok ()
        | _ ->
          invalid "compaction binding requires an accepted compaction and dependent turn"
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
  let%bind () =
    match t.completion_contract with
    | None -> Ok ()
    | Some contract ->
      let%bind () = Completion_contract.validate contract in
      (match t.context.origin, t.context.call_entry_id with
       | Model, Some _ when String.equal t.context.tool_name contract.tool_name -> Ok ()
       | _ -> invalid "completion contract requires its model tool call")
  in
  let%bind () =
    match t.authoring_reference with
    | None -> Ok ()
    | Some reference ->
      let%bind () = Authoring_reference.validate reference in
      (match t.status with
       | (Resolved (Complete value) | Published (Complete value))
         when String.equal
                reference.scope
                (Authoring_reference.scope_for
                   ~session_id:t.context.session_id
                   ~generation:t.context.generation)
              && Authoring_reference.matches_output reference value -> Ok ()
       | _ ->
         invalid "authoring reference requires its exact successful invocation output")
  in
  match t.status with
  | Admitted | Dispatching -> Ok ()
  | Resolved outcome | Published outcome -> validate_outcome outcome
;;

let create ?routing ?observer ?completion_contract ?parent_event context =
  let t =
    { context
    ; status = Admitted
    ; output_entry_id = None
    ; routing
    ; parent_event
    ; handler_intent = None
    ; completion_contract
    ; authoring_reference = None
    ; publication_discarded = None
    ; observation =
        Option.map observer ~f:(fun observer ->
          { observer
          ; status = Awaiting
          ; follow_up = None
          ; compaction_operation_id = None
          })
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

let resolve ?authoring_reference t ~session_id ~generation outcome =
  if Id.Session.compare session_id t.context.session_id <> 0
  then failure Permission_denied "invocation belongs to another session"
  else if generation <> t.context.generation
  then failure Conflict "invocation generation is stale"
  else (
    match t.status with
    | Dispatching ->
      let next = { t with status = Resolved outcome; authoring_reference } in
      Result.map (validate next) ~f:(fun () -> next)
    | Admitted -> failure Invalid_state "invocation has not been dispatched"
    | Resolved _ | Published _ -> failure Already_resolved "invocation already resolved")
;;

let record_authoring_reference t reference =
  match t.status, t.authoring_reference with
  | Resolved (Complete _), None ->
    let next = { t with authoring_reference = Some reference } in
    Result.map (validate next) ~f:(fun () -> next)
  | Resolved (Complete _), Some current when Authoring_reference.equal current reference
    -> Result.map (validate t) ~f:(fun () -> t)
  | _ ->
    failure
      Invalid_state
      "authoring reference requires an uncommitted successful resolution"
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

let accept_observation_compaction t ~operation_id =
  match t.observation with
  | Some { status = Observed; follow_up = Some (Pending_follow_up requests); _ } ->
    let t =
      { t with
        observation =
          Option.map t.observation ~f:(fun observation ->
            { observation with compaction_operation_id = Some operation_id })
      }
    in
    change_follow_up t (Compaction_accepted_follow_up requests)
  | Some
      { status = Observed
      ; follow_up = Some (Compaction_accepted_follow_up _)
      ; compaction_operation_id = Some existing
      ; _
      }
    when Id.Operation.equal existing operation_id -> Ok t
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
      match old.compaction_operation_id, current.compaction_operation_id with
      | before, after when Option.equal Id.Operation.equal before after -> Ok ()
      | None, Some _ ->
        (match old.follow_up, current.follow_up with
         | Some (Pending_follow_up _), Some (Compaction_accepted_follow_up _) -> Ok ()
         | _ -> failure Conflict "compaction binding can only be attached at admission")
      | _ -> failure Conflict "compaction operation binding is immutable"
    in
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

let record_handler_intent t ~requests =
  let open Result.Let_syntax in
  match
    requests.request_turn
    || requests.request_compaction
    || Option.is_some requests.end_session
  with
  | false -> Ok t
  | true ->
    let%bind () =
      match t.handler_intent with
      | None -> Ok ()
      | Some _ -> failure Already_resolved "handler intent is already recorded"
    in
    let next =
      { t with
        handler_intent =
          Some { follow_up = Pending_follow_up requests; compaction_operation_id = None }
      }
    in
    let%map () = validate next in
    next
;;

let change_handler_intent t change =
  let open Result.Let_syntax in
  let%bind current =
    Result.of_option
      t.handler_intent
      ~error:
        (Error.create
           Invalid_state
           ~message:"invocation has no handler intent"
           ~retryable:false
           ())
  in
  let%bind intent = change current in
  let next = { t with handler_intent = Some intent } in
  let%map () = validate next in
  next
;;

let apply_handler_intent t =
  change_handler_intent t (fun intent ->
    match intent.follow_up with
    | Pending_follow_up requests | Compaction_accepted_follow_up requests ->
      Ok { intent with follow_up = Applied_follow_up requests }
    | Applied_follow_up _ -> Ok intent
    | Discarded_follow_up _ -> failure Invalid_state "handler intent was discarded")
;;

let accept_handler_compaction t ~operation_id =
  change_handler_intent t (fun intent ->
    match intent.follow_up, intent.compaction_operation_id with
    | Pending_follow_up requests, None ->
      Ok
        { follow_up = Compaction_accepted_follow_up requests
        ; compaction_operation_id = Some operation_id
        }
    | Compaction_accepted_follow_up _, Some current
      when Id.Operation.equal current operation_id -> Ok intent
    | _ -> failure Invalid_state "handler has no pending compaction and dependent turn")
;;

let discard_handler_intent t ~reason =
  change_handler_intent t (fun intent ->
    match intent.follow_up with
    | Pending_follow_up requests | Compaction_accepted_follow_up requests ->
      Ok { intent with follow_up = Discarded_follow_up (requests, reason) }
    | Discarded_follow_up (_, current) when String.equal current reason -> Ok intent
    | _ -> failure Invalid_state "handler has no pending intent to discard")
;;

let validate_handler_transition previous next =
  match previous.handler_intent, next.handler_intent with
  | before, after when Option.equal equal_handler_intent before after -> Ok ()
  | None, Some { follow_up = Pending_follow_up _; compaction_operation_id = None }
    when equal_status previous.status Dispatching -> Ok ()
  | Some before, Some after when equal_status previous.status next.status ->
    let open Result.Let_syntax in
    let%bind () =
      match before.compaction_operation_id, after.compaction_operation_id with
      | before, after when Option.equal Id.Operation.equal before after -> Ok ()
      | None, Some _ ->
        (match before.follow_up, after.follow_up with
         | Pending_follow_up _, Compaction_accepted_follow_up _ -> Ok ()
         | _ -> failure Conflict "handler compaction binding requires admission")
      | _ -> failure Conflict "handler compaction binding is immutable"
    in
    (match before.follow_up, after.follow_up with
     | Pending_follow_up before, Compaction_accepted_follow_up after
     | ( (Pending_follow_up before | Compaction_accepted_follow_up before)
       , (Applied_follow_up after | Discarded_follow_up (after, _)) )
       when equal_follow_up before after -> Ok ()
     | _ -> failure Conflict "handler intent cannot be rewritten or rearmed")
  | _ -> failure Conflict "handler intent must be recorded with its original outcome"
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
    let%bind () = validate_handler_transition previous next in
    let%bind () =
      match
        ( previous.authoring_reference
        , next.authoring_reference
        , previous.status
        , next.status )
      with
      | None, Some _, Dispatching, Resolved (Complete _) -> Ok ()
      | before, after, _, _ when Option.equal Authoring_reference.equal before after ->
        Ok ()
      | _ ->
        failure Conflict "authoring reference must be committed with its original outcome"
    in
    let%bind () =
      match
        Option.equal
          Completion_contract.equal
          previous.completion_contract
          next.completion_contract
      with
      | true -> Ok ()
      | false -> failure Conflict "invocation completion contract is immutable"
    in
    if not (equal_context previous.context next.context)
    then failure Conflict "invocation context is immutable"
    else if
      not
        (Option.equal
           Id.Moderator_execution.equal
           previous.parent_event
           next.parent_event)
    then failure Conflict "invocation event parent is immutable"
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
                 || (not
                       (Option.equal
                          equal_handler_intent
                          previous.handler_intent
                          next.handler_intent))
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
     @ optional "follow_up" observation.follow_up follow_up_to_json
     @ optional "compaction_operation_id" observation.compaction_operation_id (fun id ->
       `String (Id.Operation.to_string id)))
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
    @ (if version >= 6 then [ "follow_up" ] else [])
    @ if version >= 8 then [ "compaction_operation_id" ] else []
  in
  let%bind follow_up =
    if version >= 9
    then Json_codec.optional_as fields "follow_up" (follow_up_of_json ~version)
    else if version >= 6
    then
      Json_codec.required_as fields "follow_up" (follow_up_of_json ~version)
      |> Result.map ~f:Option.some
    else Ok None
  in
  let%bind compaction_operation_id =
    if version >= 9
    then Json_codec.optional_as fields "compaction_operation_id" Id.Operation.of_json
    else if version >= 8
    then
      Json_codec.required_as fields "compaction_operation_id" (fun json ->
        Result.bind (Json_codec.string json) ~f:Id.Operation.of_string)
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
  { observer = { script_id; source_sha256 }; status; follow_up; compaction_operation_id }
;;

let handler_intent_to_json (intent : handler_intent) =
  `Object
    ([ "follow_up", follow_up_to_json intent.follow_up ]
     @ optional
         "compaction_operation_id"
         intent.compaction_operation_id
         Id.Operation.to_json)
;;

let handler_intent_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () = closed fields [ "follow_up"; "compaction_operation_id" ] in
  let%bind follow_up =
    Json_codec.required_as fields "follow_up" (follow_up_of_json ~version:10)
  in
  let%map compaction_operation_id =
    Json_codec.optional_as fields "compaction_operation_id" Id.Operation.of_json
  in
  { follow_up; compaction_operation_id }
;;

let to_json t =
  `Object
    ([ ( "schema_version"
       , `Number
           (if Option.is_some t.authoring_reference
            then "12"
            else if Option.is_some t.completion_contract
            then "11"
            else if Option.is_some t.handler_intent
            then "10"
            else if Option.is_some t.parent_event
            then "9"
            else if
              Option.exists t.observation ~f:(fun observation ->
                Option.is_some observation.compaction_operation_id)
            then "8"
            else if
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
     @ optional "observation" t.observation observation_to_json
     @ optional "parent_event" t.parent_event Id.Moderator_execution.to_json
     @ optional "handler_intent" t.handler_intent handler_intent_to_json
     @ optional "completion_contract" t.completion_contract Completion_contract.to_json
     @ optional "authoring_reference" t.authoring_reference Authoring_reference.to_json)
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
    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 -> Ok ()
    | _ -> failure Incompatible_protocol "unsupported invocation schema version"
  in
  let%bind () =
    closed
      fields
      ([ "schema_version"; "context"; "status" ]
       @ (if version >= 2 then [ "output_entry_id" ] else [])
       @ (if version >= 3 then [ "routing" ] else [])
       @ (if version >= 4 then [ "publication_discarded" ] else [])
       @ (if version >= 5 then [ "observation" ] else [])
       @ (if version >= 9 then [ "parent_event" ] else [])
       @ (if version >= 10 then [ "handler_intent" ] else [])
       @ (if version >= 11 then [ "completion_contract" ] else [])
       @ if version >= 12 then [ "authoring_reference" ] else [])
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
    if version >= 10
    then Json_codec.optional_as fields "observation" (observation_of_json ~version)
    else if version >= 5
    then
      Json_codec.required_as fields "observation" (observation_of_json ~version)
      |> Result.map ~f:Option.some
    else Ok None
  in
  let%bind handler_intent =
    Json_codec.optional_as fields "handler_intent" handler_intent_of_json
  in
  let%bind completion_contract =
    match version with
    | 12 ->
      Json_codec.optional_as fields "completion_contract" Completion_contract.of_json
    | 11 ->
      Json_codec.required_as fields "completion_contract" Completion_contract.of_json
      |> Result.map ~f:Option.some
    | _ -> Ok None
  in
  let%bind authoring_reference =
    match version with
    | 12 ->
      Json_codec.required_as fields "authoring_reference" Authoring_reference.of_json
      |> Result.map ~f:Option.some
    | _ -> Ok None
  in
  let t =
    { context
    ; status
    ; output_entry_id
    ; routing
    ; publication_discarded
    ; observation
    ; parent_event = None
    ; handler_intent
    ; completion_contract
    ; authoring_reference
    }
  in
  let%bind t =
    match version with
    | 10 | 11 | 12 ->
      Json_codec.optional_as fields "parent_event" Id.Moderator_execution.of_json
      |> Result.map ~f:(fun parent_event -> { t with parent_event })
    | 9 ->
      Json_codec.required_as fields "parent_event" Id.Moderator_execution.of_json
      |> Result.map ~f:(fun id -> { t with parent_event = Some id })
    | _ -> Ok t
  in
  let%map () = validate t in
  t
;;
