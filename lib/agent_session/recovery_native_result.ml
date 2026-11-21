open! Core
module N = Native_tool_invocation.Admission

type rejection = Protected_native_call.rejection =
  | Missing
  | Invalid
  | Denied
  | Conflict
  | Limit
  | Expired_cursor
  | Unavailable

let select token ~binding selection =
  let open Result.Let_syntax in
  let%bind () = N.check_now token ~binding in
  Protected_native_call.select (N.invocation token) ~binding selection
;;

let acknowledge token ~binding = select token ~binding Acknowledged
let reject token ~binding reason = select token ~binding (Rejected reason)

let deliver token ~binding ~materialize ~custody ~refresh =
  select
    token
    ~binding
    (Deliver
       (fun ~call ~output ->
         Inference_runtime.Protected_delivery.create
           ~call
           ~output
           ~materialize
           ~custody
           ~refresh))
;;
