open Core
module P = Agent_protocol.Permission
module H = Agent_protocol.Host_invocation

type developer =
  { invocation : H.t
  ; id : Agent_protocol.Id.Permission.t
  ; identity_digest : string
  ; invocation_display : string
  ; effects : string list
  ; created_at : Agent_protocol.Timestamp.t
  ; expires_at : Agent_protocol.Timestamp.t option
  }

type t =
  | Conversation of P.t
  | Developer of developer

let invalid message = Error (Agent_protocol.Error.invalid_request message)

let of_conversation permission =
  Result.map (P.of_json (P.to_json permission)) ~f:(fun _ -> Conversation permission)
;;

let developer
      ~invocation
      ~id
      ~identity_digest
      ~invocation_display
      ~effects
      ~created_at
      ~expires_at
  =
  let open Result.Let_syntax in
  let%bind () = H.validate invocation in
  let context = H.context invocation in
  let%bind () =
    match context.owner, H.status invocation with
    | Agent_protocol.Execution_owner.Developer _, Dispatching -> Ok ()
    | _ -> invalid "developer permission requires dispatched developer invocation"
  in
  let%bind () =
    if String.is_empty identity_digest || String.length identity_digest > 256
    then invalid "developer permission identity is out of bounds"
    else Ok ()
  in
  let%bind () =
    if String.is_empty invocation_display || String.length invocation_display > 16384
    then invalid "developer permission display is out of bounds"
    else Ok ()
  in
  let%bind () =
    let valid =
      List.fold_until
        effects
        ~init:0
        ~f:(fun count value ->
          if count >= 128 || String.is_empty value || String.length value > 1024
          then Stop false
          else Continue (count + 1))
        ~finish:(fun _ -> true)
    in
    if not valid then invalid "developer permission effects are out of bounds" else Ok ()
  in
  let%bind () =
    Agent_protocol.Id.Permission.of_json (Agent_protocol.Id.Permission.to_json id)
    |> Result.map ~f:ignore
  in
  let%bind () =
    match expires_at with
    | Some expires_at when Agent_protocol.Timestamp.compare expires_at created_at < 0 ->
      invalid "developer permission expires before creation"
    | Some _ | None -> Ok ()
  in
  Ok
    (Developer
       { invocation
       ; id
       ; identity_digest
       ; invocation_display
       ; effects
       ; created_at
       ; expires_at
       })
;;

let owner = function
  | Developer value -> (H.context value.invocation).owner
  | Conversation value ->
    Agent_protocol.Execution_owner.conversation
      ~session_id:value.session_id
      ~generation:value.generation
    |> (function
     | Ok owner -> owner
     | Error _ -> failwith "validated conversation permission has invalid owner")
;;

let invocation_id = function
  | Developer value -> Some (H.context value.invocation).id
  | Conversation value ->
    (match value.owner with
     | P.Invocation id -> Some id
     | Operation _ -> None)
;;

let request = function
  | Developer value ->
    { Permission_policy.tool_name = (H.context value.invocation).tool_name
    ; identity_digest = value.identity_digest
    ; invocation_display = value.invocation_display
    ; effects = value.effects
    }
  | Conversation value ->
    { Permission_policy.tool_name = value.tool_name
    ; identity_digest = Option.value value.runtime_identity ~default:""
    ; invocation_display = value.invocation_display
    ; effects = value.effects
    }
;;

let choices = function
  | Developer _ -> [ P.Approve_once; Deny ]
  | Conversation value -> value.choices
;;

let expires_at = function
  | Developer value -> value.expires_at
  | Conversation value -> value.expires_at
;;

let to_conversation = function
  | Conversation value -> Some value
  | Developer _ -> None
;;
