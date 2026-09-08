open Core
module Error = Protocol_error
open Extension_codec

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
[@@deriving compare, sexp]

type tool_error =
  { code : string
  ; message : string
  ; retryable : bool
  ; details : Jsonaf.t
  }
[@@deriving sexp]

type outcome =
  | Complete of Jsonaf.t
  | Pending of work * Jsonaf.t
  | Fail of tool_error
  | Cancelled of string
[@@deriving sexp]

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
[@@deriving sexp]

type status =
  | Admitted
  | Dispatching
  | Resolved of outcome
  | Published of outcome
[@@deriving sexp]

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
[@@deriving sexp]

type routing =
  { kind : call_kind
  ; original_name : string
  ; original_payload : payload_fingerprint
  ; final_payload : payload_fingerprint
  ; canonical_payload : payload_fingerprint option [@sexp.option]
  ; preparation : preparation
  }
[@@deriving sexp]

type t =
  { context : context
  ; status : status
  ; output_entry_id : History.Id.t option [@sexp.option]
  ; routing : routing option [@sexp.option]
  }
[@@deriving sexp]

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
       | Invalid_input | Pre_tool_rejected ->
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

let create ?routing context =
  let t = { context; status = Admitted; output_entry_id = None; routing } in
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
  if Option.is_some t.context.call_entry_id
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
    if not (Sexp.equal (sexp_of_context previous.context) (sexp_of_context next.context))
    then failure Conflict "invocation context is immutable"
    else if
      not
        (Option.equal
           (fun a b -> Sexp.equal (sexp_of_routing a) (sexp_of_routing b))
           previous.routing
           next.routing)
    then failure Conflict "invocation routing provenance is immutable"
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
      | Admitted, Dispatching | Admitted, Resolved (Cancelled _) | Dispatching, Resolved _
        -> Ok ()
      | (Resolved old, Published current | Published old, Published current)
        when Sexp.equal (sexp_of_outcome old) (sexp_of_outcome current) -> Ok ()
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

let to_json t =
  `Object
    ([ ( "schema_version"
       , `Number
           (if Option.is_some t.routing
            then "3"
            else if Option.is_some t.context.call_entry_id
            then "2"
            else "1") )
     ; "context", context_to_json t.context
     ; "status", status_to_json t.status
     ]
     @ optional "output_entry_id" t.output_entry_id History.Id.to_json
     @ optional "routing" t.routing routing_to_json)
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
    if version = 1 || version = 2 || version = 3
    then Ok ()
    else failure Incompatible_protocol "unsupported invocation schema version"
  in
  let%bind () =
    closed
      fields
      ([ "schema_version"; "context"; "status" ]
       @ (if version >= 2 then [ "output_entry_id" ] else [])
       @ if version = 3 then [ "routing" ] else [])
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
    else Ok None
  in
  let t = { context; status; output_entry_id; routing } in
  let%map () = validate t in
  t
;;
