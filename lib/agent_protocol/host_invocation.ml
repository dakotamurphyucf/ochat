open Core
open Extension_codec
module I = Invocation

module Jsonaf = struct
  include Jsonaf

  let equal = exactly_equal
end

type context =
  { id : Id.Invocation.t
  ; owner : Execution_owner.t
  ; origin : I.origin
  ; parent_invocation : Id.Invocation.t option
  ; parent_job : Id.Job.t option
  ; tool_name : string
  ; implementation_revision : string
  ; capability_fingerprint : string
  ; input : Jsonaf.t
  ; created_at : Timestamp.t
  ; deadline : Timestamp.t option
  }
[@@deriving equal, sexp_of]

type t =
  | Conversation of I.t
  | Developer of
      { context : context
      ; status : I.status
      }

let context = function
  | Developer value -> value.context
  | Conversation original ->
    let c = original.context in
    let owner =
      match
        Execution_owner.conversation ~session_id:c.session_id ~generation:c.generation
      with
      | Ok owner -> owner
      | Error _ -> failwith "validated conversation invocation has invalid owner"
    in
    { id = c.id
    ; owner
    ; origin = c.origin
    ; parent_invocation = c.parent_invocation
    ; parent_job = c.parent_job
    ; tool_name = c.tool_name
    ; implementation_revision = c.implementation_revision
    ; capability_fingerprint = c.capability_fingerprint
    ; input = c.input
    ; created_at = c.created_at
    ; deadline = c.deadline
    }
;;

let status = function
  | Conversation original -> original.status
  | Developer value -> value.status
;;

let outcome t =
  match status t with
  | I.Resolved value | Published value -> Some value
  | Admitted | Dispatching -> None
;;

let to_conversation = function
  | Conversation original -> Some original
  | Developer _ -> None
;;

let validate_context context =
  let open Result.Let_syntax in
  let%bind () = validate_id Id.Invocation.to_json Id.Invocation.of_json context.id in
  let%bind () = Execution_owner.validate context.owner in
  let%bind () =
    match context.owner with
    | Execution_owner.Conversation _ ->
      Error (Error.invalid_request "developer invocation needs a developer owner")
    | Developer owner ->
      if owner.epoch < 0
      then Error (Error.invalid_request "negative developer run epoch")
      else Ok ()
  in
  let%bind () =
    match context.origin with
    | I.Script | Delegated_agent -> Ok ()
    | Model | Moderator | External_adapter ->
      Error (Error.invalid_request "invalid developer invocation origin")
  in
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
  let%bind () = text ~name:"tool name" ~max:256 context.tool_name in
  let%bind () =
    text ~name:"implementation revision" ~max:256 context.implementation_revision
  in
  let%bind () =
    text ~name:"capability fingerprint" ~max:256 context.capability_fingerprint
  in
  let%bind () =
    if Option.exists context.parent_invocation ~f:(Id.Invocation.equal context.id)
    then Error (Error.invalid_request "invocation cannot be its own parent")
    else Ok ()
  in
  let%bind () =
    if
      Option.exists context.deadline ~f:(fun deadline ->
        Timestamp.compare deadline context.created_at < 0)
    then Error (Error.invalid_request "invocation deadline precedes creation")
    else Ok ()
  in
  validate_json context.input
;;

let validate = function
  | Conversation original -> I.validate original
  | Developer { context; status } ->
    let open Result.Let_syntax in
    let%bind () = validate_context context in
    (match status with
     | I.Resolved outcome | Published outcome -> I.validate_outcome outcome
     | Admitted | Dispatching -> Ok ())
;;

let of_conversation original =
  Result.map (I.validate original) ~f:(fun () -> Conversation original)
;;

let create_developer context =
  let value = Developer { context; status = I.Admitted } in
  Result.map (validate value) ~f:(fun () -> value)
;;

let dispatch = function
  | Conversation original -> Result.map (I.dispatch original) ~f:(fun v -> Conversation v)
  | Developer { context; status = I.Admitted } ->
    Ok (Developer { context; status = I.Dispatching })
  | Developer _ ->
    Error
      (Error.create
         Invalid_state
         ~message:"invocation has already been dispatched or resolved"
         ~retryable:false
         ())
;;

let resolve t outcome =
  match t with
  | Conversation original ->
    let context = original.context in
    Result.map
      (I.resolve
         original
         ~session_id:context.session_id
         ~generation:context.generation
         outcome)
      ~f:(fun value -> Conversation value)
  | Developer { context; status = I.Dispatching } ->
    let value = Developer { context; status = I.Resolved outcome } in
    Result.map (validate value) ~f:(fun () -> value)
  | Developer _ ->
    Error
      (Error.create
         Invalid_state
         ~message:"invocation has not been dispatched or is already resolved"
         ~retryable:false
         ())
;;

let cancel t ~reason =
  match t with
  | Conversation original ->
    Result.map (I.cancel original ~reason) ~f:(fun value -> Conversation value)
  | Developer { context; status = I.Admitted | Dispatching } ->
    let value = Developer { context; status = I.Resolved (Cancelled reason) } in
    Result.map (validate value) ~f:(fun () -> value)
  | Developer _ ->
    Error
      (Error.create
         Already_resolved
         ~message:"invocation already resolved"
         ~retryable:false
         ())
;;

let optional name value encode =
  Option.to_list (Option.map value ~f:(fun item -> name, encode item))
;;

let developer_to_json value =
  let open Result.Let_syntax in
  let%bind () = validate value in
  match value with
  | Conversation _ ->
    Error (Error.invalid_request "conversation invocation is not a developer record")
  | Developer { context; status } ->
    (match context.owner, status with
     | Execution_owner.Developer owner, (I.Admitted | Dispatching | Resolved _) ->
       Ok
         (`Object
             ([ "schema_version", `Number "1"
              ; "id", Id.Invocation.to_json context.id
              ; "host_id", Id.Server.to_json owner.host_id
              ; "run_id", Id.Developer_run.to_json owner.run_id
              ; "epoch", `Number (Int.to_string owner.epoch)
              ; "principal_id", Id.Principal.to_json owner.principal_id
              ; "origin", I.origin_to_json context.origin
              ; "tool_name", `String context.tool_name
              ; "implementation_revision", `String context.implementation_revision
              ; "capability_fingerprint", `String context.capability_fingerprint
              ; "input", context.input
              ; "created_at", Timestamp.to_json context.created_at
              ; "status", I.status_to_json status
              ]
              @ optional
                  "parent_invocation"
                  context.parent_invocation
                  Id.Invocation.to_json
              @ optional "parent_job" context.parent_job Id.Job.to_json
              @ optional "deadline" context.deadline Timestamp.to_json))
     | Execution_owner.Conversation _, _ ->
       Error (Error.invalid_request "developer invocation has a conversation owner")
     | Execution_owner.Developer _, Published _ ->
       Error (Error.invalid_request "developer invocation cannot be published"))
;;

let developer_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "schema_version"
      ; "id"
      ; "host_id"
      ; "run_id"
      ; "epoch"
      ; "principal_id"
      ; "origin"
      ; "parent_invocation"
      ; "parent_job"
      ; "tool_name"
      ; "implementation_revision"
      ; "capability_fingerprint"
      ; "input"
      ; "created_at"
      ; "deadline"
      ; "status"
      ]
  in
  let%bind _version =
    Json_codec.required_as fields "schema_version" (Json_codec.bounded_int ~min:1 ~max:1)
  in
  let%bind id = Json_codec.required_as fields "id" Id.Invocation.of_json in
  let%bind host_id = Json_codec.required_as fields "host_id" Id.Server.of_json in
  let%bind run_id = Json_codec.required_as fields "run_id" Id.Developer_run.of_json in
  let%bind epoch =
    Json_codec.required_as
      fields
      "epoch"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind principal_id =
    Json_codec.required_as fields "principal_id" Id.Principal.of_json
  in
  let%bind owner = Execution_owner.developer ~host_id ~run_id ~epoch ~principal_id in
  let%bind origin = Json_codec.required_as fields "origin" I.origin_of_json in
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
  let%bind deadline = Json_codec.optional_as fields "deadline" Timestamp.of_json in
  let%bind status = Json_codec.required_as fields "status" I.status_of_json in
  let context =
    { id
    ; owner
    ; origin
    ; parent_invocation
    ; parent_job
    ; tool_name
    ; implementation_revision
    ; capability_fingerprint
    ; input
    ; created_at
    ; deadline
    }
  in
  let%bind admitted = create_developer context in
  let%bind value =
    match status with
    | I.Admitted -> Ok admitted
    | Dispatching -> dispatch admitted
    | Resolved outcome ->
      let%bind dispatched = dispatch admitted in
      resolve dispatched outcome
    | Published _ ->
      Error (Error.invalid_request "developer invocation cannot be published")
  in
  let%bind canonical = developer_to_json value in
  if Jsonaf.exactly_equal json canonical
  then Ok value
  else Error (Error.invalid_request "noncanonical developer invocation record")
;;
