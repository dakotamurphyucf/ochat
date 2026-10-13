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
