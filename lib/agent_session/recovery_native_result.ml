open! Core
module N = Native_tool_invocation.Admission

let select token ~binding selection =
  let open Result.Let_syntax in
  let%bind () = N.check_now token ~binding in
  Protected_native_call.select (N.invocation token) ~binding selection
;;

let acknowledge token ~binding = select token ~binding Acknowledged

let deliver token ~binding ~materialize ~refresh =
  select
    token
    ~binding
    (Deliver
       (fun ~call ~output ->
         Inference_runtime.Protected_delivery.create ~call ~output ~materialize ~refresh))
;;
