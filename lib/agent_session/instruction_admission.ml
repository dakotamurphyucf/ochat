open! Core
module P = Agent_protocol
module Native = Instruction_native_admission
module Effect = Native.Effect

type operator =
  { actor : Operator_authorization.t
  ; session_id : P.Id.Session.t
  ; generation : int
  ; effect_kind : Effect.t
  ; attachment_id : P.Id.Attachment.t option
  ; authorize : Session_state.t -> (unit, P.Error.t) result
  }

type t =
  | Native of Native.t
  | Operator of operator

let of_native admission = Native admission

let native = function
  | Native admission -> Some admission
  | Operator _ -> None
;;

let actor = function
  | Native admission -> Native.actor admission
  | Operator t -> t.actor
;;

let scope = function
  | Effect.Activate | Deactivate -> P.Scope.Send_messages
  | Refresh | Read | Status -> View_session_transcript
;;

let denied () =
  Error
    (P.Error.create
       Permission_denied
       ~message:"instruction effect authority denied or expired"
       ~retryable:false
       ())
;;

let current actor effect_kind =
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope (Operator_authorization.principal actor) (scope effect_kind)
  then Ok ()
  else denied ()
;;

let of_operator actor ~session_id ~generation ~effect_kind ~attachment_id ~authorize =
  let open Result.Let_syntax in
  let%bind () = current actor effect_kind in
  let valid_attachment =
    match effect_kind, attachment_id with
    | (Effect.Activate | Deactivate), Some _ -> true
    | (Refresh | Read | Status), None -> true
    | (Activate | Deactivate), None | (Refresh | Read | Status), Some _ -> false
  in
  if generation < 0 || not valid_attachment
  then
    Error
      (P.Error.invalid_request "instruction admission target or attachment is invalid")
  else
    Ok (Operator { actor; session_id; generation; effect_kind; attachment_id; authorize })
;;

let check_now t (state : Session_state.t) ~effect_kind =
  match t with
  | Native admission -> Native.check_now admission state ~effect_kind
  | Operator t ->
    let open Result.Let_syntax in
    let%bind () = current t.actor effect_kind in
    if
      (not (Effect.equal t.effect_kind effect_kind))
      || (not (P.Id.Session.equal t.session_id state.identity.session_id))
      || not (Int.equal t.generation state.identity.generation)
    then denied ()
    else (
      let%bind () = t.authorize state in
      current t.actor effect_kind)
;;

let writer_attachment = function
  | Native _ -> None
  | Operator t -> t.attachment_id
;;
