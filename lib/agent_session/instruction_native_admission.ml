open! Core
module P = Agent_protocol
module N = Native_tool_invocation
module C = Chat_response.Tool_capability

module Effect = struct
  type t =
    | Refresh
    | Read
    | Activate
    | Deactivate
    | Status
  [@@deriving equal, enumerate]

  let name = function
    | Refresh -> "instructions_refresh"
    | Read -> "instructions_read"
    | Activate -> "instructions_activate"
    | Deactivate -> "instructions_deactivate"
    | Status -> "instructions_status"
  ;;
end

type t =
  { token : N.Admission.t
  ; binding : C.binding
  ; effect_kind : Effect.t
  ; actor : Operator_authorization.t
  }

let denied () =
  Error
    (P.Error.create
       Permission_denied
       ~message:"instruction effect authority denied or expired"
       ~retryable:false
       ())
;;

let create token ~binding ~effect_kind =
  let open Result.Let_syntax in
  let%bind () = N.Admission.check_now token ~binding in
  if not (String.equal (C.reference binding).name (Effect.name effect_kind))
  then denied ()
  else (
    match N.Admission.actor token with
    | None -> denied ()
    | Some actor ->
      if Operator_authorization.is_current actor
      then Ok { token; binding; effect_kind; actor }
      else denied ())
;;

let actor t = t.actor

let check_now t (state : Session_state.t) ~effect_kind =
  let open Result.Let_syntax in
  let%bind () = N.Admission.check_now t.token ~binding:t.binding in
  let invocation = N.Admission.invocation t.token in
  let scope =
    match effect_kind with
    | Effect.Activate | Deactivate -> P.Scope.Send_messages
    | Refresh | Read | Status -> View_session_transcript
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
