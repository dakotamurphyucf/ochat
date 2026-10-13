open! Core
module P = Agent_protocol
module A = P.Authoring
module W = A.Wire

type t =
  { connection : Connection.t
  ; initialization : P.Initialize.Response.t
  ; capabilities : A.Capabilities.t
  }

let incompatible message =
  Error (P.Error.create Incompatible_protocol ~message ~retryable:false ())
;;

let require_current t =
  match Connection.initialization t.connection with
  | Some current when phys_equal current t.initialization -> Ok ()
  | Some _ -> incompatible "authoring connection was initialized again"
  | None -> incompatible "authoring connection is no longer initialized"
;;

let require_feature t feature =
  let open Result.Let_syntax in
  let%bind () = require_current t in
  if List.mem t.initialization.enabled_features feature ~equal:String.equal
  then Ok ()
  else
    Error
      (P.Error.create
         Incompatible_protocol
         ~message:"authoring feature was not selected"
         ~retryable:false
         ())
;;

let capabilities t = t.capabilities
let host t = t.initialization.server_id
let principal t = t.initialization.principal.id

let send_raw connection request =
  let method_name = W.Request.method_name request in
  match Connection.request_without_history connection (P.Command.Authoring request) with
  | Error _ as failure -> failure
  | Ok (P.Method_result.Authoring response)
    when String.equal (W.Response.method_name response) method_name -> Ok response
  | Ok _ -> incompatible ("unexpected authoring result for " ^ method_name)
;;

let require_method t request =
  let method_name = W.Request.method_name request in
  match A.Method.of_string method_name with
  | Error _ -> incompatible "unknown authoring client method"
  | Ok method_ ->
    if A.Capabilities.supports t.capabilities method_
    then Ok ()
    else incompatible ("authoring method is not advertised: " ^ method_name)
;;

let send t request =
  let open Result.Let_syntax in
  let%bind () = require_current t in
  let%bind () = require_method t request in
  let%bind response = send_raw t.connection request in
  let%bind () = require_current t in
  Ok response
;;

let create connection ~feature =
  let open Result.Let_syntax in
  let%bind initialization =
    match Connection.initialization connection with
    | Some response -> Ok response
    | None -> incompatible "authoring requires a successful initialization"
  in
  let%bind () =
    if List.mem initialization.enabled_features feature ~equal:String.equal
    then Ok ()
    else incompatible "authoring feature was not selected"
  in
  let%bind capabilities =
    match send_raw connection W.Request.Capabilities with
    | Ok (W.Response.Capabilities capabilities) -> Ok capabilities
    | Ok _ -> incompatible "unexpected authoring capabilities result"
    | Error _ as failure -> failure
  in
  let result = { connection; initialization; capabilities } in
  let%bind () = require_current result in
  let%bind () =
    if A.Capabilities.supports capabilities A.Method.Capabilities
    then Ok ()
    else incompatible "authoring capabilities omits its own method"
  in
  Ok result
;;
