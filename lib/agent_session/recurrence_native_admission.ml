open! Core
module P = Agent_protocol
module N = Native_tool_invocation
module C = Chat_response.Tool_capability

module Effect = struct
  type t =
    | Create
    | List
    | Get
    | Edit
    | Pause
    | Resume
    | Cancel
    | Status
    | History
  [@@deriving equal, enumerate]

  let name = function
    | Create -> "recurrence_create"
    | List -> "recurrence_list"
    | Get -> "recurrence_get"
    | Edit -> "recurrence_edit"
    | Pause -> "recurrence_pause"
    | Resume -> "recurrence_resume"
    | Cancel -> "recurrence_cancel"
    | Status -> "recurrence_status"
    | History -> "recurrence_history"
  ;;
end

type t =
  { borrowed : N.borrowed
  ; token : N.Admission.t
  ; binding : C.binding
  ; effect_kind : Effect.t
  ; actor : Operator_authorization.t
  }

let denied () =
  Error
    (P.Error.create
       Permission_denied
       ~message:"recurrence effect authority denied or expired"
       ~retryable:false
       ())
;;

let create borrowed ~binding ~effect_kind =
  let open Result.Let_syntax in
  let%bind token = N.Admission.capture borrowed ~binding in
  if not (String.equal (C.reference binding).name (Effect.name effect_kind))
  then denied ()
  else (
    match N.Admission.actor token with
    | None -> denied ()
    | Some actor ->
      if Operator_authorization.is_current actor
      then Ok { borrowed; token; binding; effect_kind; actor }
      else denied ())
;;

let actor t = t.actor

let check_now t (state : Session_state.t) ~effect_kind =
  let open Result.Let_syntax in
  let%bind () = N.Admission.check_now t.token ~binding:t.binding in
  let invocation = N.Admission.invocation t.token in
  let scope =
    match effect_kind with
    | Effect.Create | Edit | Pause | Resume | Cancel -> P.Scope.Send_messages
    | List | Get | Status | History -> View_session_transcript
  in
  if
    Effect.equal t.effect_kind effect_kind
    && P.Id.Session.equal invocation.context.session_id state.identity.session_id
    && Int.equal invocation.context.generation state.identity.generation
    && Operator_authorization.is_current t.actor
    && P.Principal.has_scope (Operator_authorization.principal t.actor) scope
  then Ok ()
  else denied ()
;;

let capabilities t =
  let open Result.Let_syntax in
  let%bind () = N.Admission.check_now t.token ~binding:t.binding in
  if not (Operator_authorization.is_current t.actor)
  then denied ()
  else (
    let%bind capabilities = N.borrowed_capabilities t.borrowed in
    let%map () = N.Admission.check_now t.token ~binding:t.binding in
    capabilities)
;;

let invocation t = N.Admission.invocation t.token
let binding t = t.binding
let effect_kind t = t.effect_kind
