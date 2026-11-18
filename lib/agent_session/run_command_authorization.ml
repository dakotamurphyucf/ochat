open! Core
module P = Agent_protocol

let check actor ~authorize state =
  let current () =
    if
      Operator_authorization.is_current actor
      && P.Principal.has_scope (Operator_authorization.principal actor) Send_messages
    then Ok ()
    else
      Error
        (P.Error.create
           Permission_denied
           ~retryable:false
           ~message:"headless run command authority expired, was revoked or lacks scope"
           ())
  in
  let open Result.Let_syntax in
  let%bind () = current () in
  let result = authorize state in
  let%bind () = current () in
  result
;;
