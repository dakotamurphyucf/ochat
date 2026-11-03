open! Core
module P = Agent_protocol

let check actor ~authorize state =
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope (Operator_authorization.principal actor) Send_messages
  then authorize state
  else
    Error
      (P.Error.create
         Permission_denied
         ~retryable:false
         ~message:"headless run command authority expired, was revoked or lacks scope"
         ())
;;
