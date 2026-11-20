open! Core
module C = Recovery_notes.Catalog
module P = Agent_protocol
module N = Native_tool_invocation.Admission

module Effect = struct
  type t =
    | Read
    | List
    | Write
    | Append
    | Delete
  [@@deriving equal]

  let name = function
    | Read -> "recovery_note_read"
    | List -> "recovery_note_list"
    | Write -> "recovery_note_write"
    | Append -> "recovery_note_append"
    | Delete -> "recovery_note_delete"
  ;;

  let of_mutation = function
    | C.Create _ | Replace _ -> Write
    | Append _ -> Append
    | Delete _ -> Delete
  ;;

  let scope = function
    | Read | List -> P.Scope.View_session_transcript
    | Write | Append | Delete -> Send_messages
  ;;
end

type t =
  | Operator_route of
      { actor : Operator_authorization.t
      ; owner : P.Session_ref.t
      ; generation : int
      ; attachment_id : P.Id.Attachment.t option
      ; authorize : Session_state.t -> (unit, P.Error.t) result
      }
  | Native_route of
      { token : N.t
      ; binding : Chat_response.Tool_capability.binding
      ; effect_kind : Effect.t
      ; actor : Operator_authorization.t
      }

type checked =
  | Operator of
      { principal : P.Id.Principal.t
      ; attachment_id : P.Id.Attachment.t option
      }
  | Native of P.Invocation.t

let denied () =
  Error
    (P.Error.create
       Permission_denied
       ~message:"recovery note authority denied or expired"
       ~retryable:false
       ())
;;

let current actor effect_kind =
  let principal = Operator_authorization.principal actor in
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope principal (Effect.scope effect_kind)
  then Ok principal.id
  else denied ()
;;

let operator actor ~owner ~generation ~attachment_id ~authorize =
  Operator_route { actor; owner; generation; attachment_id; authorize }
;;

let native token ~binding ~effect_kind =
  let open Result.Let_syntax in
  let%bind () = N.check_now token ~binding in
  let binding_name = (Chat_response.Tool_capability.reference binding).name in
  match N.actor token with
  | Some actor when String.equal binding_name (Effect.name effect_kind) ->
    let%map _ = current actor effect_kind in
    Native_route { token; binding; effect_kind; actor }
  | Some _ | None -> denied ()
;;

let check_now t (state : Session_state.t) ~owner ~effect_kind =
  let open Result.Let_syntax in
  match t with
  | Operator_route route ->
    if
      (not (P.Session_ref.equal route.owner owner))
      || not (Int.equal route.generation state.identity.generation)
    then denied ()
    else (
      let%bind _ = current route.actor effect_kind in
      let%bind () = route.authorize state in
      let%map principal = current route.actor effect_kind in
      Operator { principal; attachment_id = route.attachment_id })
  | Native_route route ->
    let%bind () = N.check_now route.token ~binding:route.binding in
    let invocation = N.invocation route.token in
    if
      Effect.equal route.effect_kind effect_kind
      && P.Id.Session.equal invocation.context.session_id state.identity.session_id
      && Int.equal invocation.context.generation state.identity.generation
    then (
      let%map _ = current route.actor effect_kind in
      Native invocation)
    else denied ()
;;
