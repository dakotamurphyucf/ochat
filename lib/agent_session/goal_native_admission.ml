open! Core
module P = Agent_protocol
module N = Native_tool_invocation
module C = Chat_response.Tool_capability

module Effect = struct
  type t =
    | Get
    | Create
    | Hold
    | Abandon
    | Objective
    | Progress
    | Start
    | Resume
  [@@deriving equal, enumerate]

  let name = function
    | Get -> "goal_get"
    | Create -> "goal_create"
    | Hold -> "goal_hold"
    | Abandon -> "goal_abandon"
    | Objective -> "goal_objective"
    | Progress -> "goal_progress"
    | Start -> "goal_start"
    | Resume -> "goal_resume"
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
       ~message:"native Goal effect authority denied or expired"
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
    | Some actor when Operator_authorization.is_current actor ->
      Ok { token; binding; effect_kind; actor }
    | Some _ | None -> denied ())
;;

let actor t = t.actor
let invocation t = N.Admission.invocation t.token
let callback_scope t = N.Admission.callback_scope t.token

let check_now t (state : Session_state.t) ~effect_kind =
  let open Result.Let_syntax in
  let%bind () = N.Admission.check_now t.token ~binding:t.binding in
  let invocation = invocation t in
  let scope =
    match effect_kind with
    | Effect.Get -> P.Scope.View_session_transcript
    | Create | Hold | Abandon | Objective | Progress | Start | Resume -> Send_messages
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
