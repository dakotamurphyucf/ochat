open! Core
module P = Agent_protocol

let unexpected method_name =
  Error (P.Error.invalid_request ("unexpected " ^ method_name ^ " result"))
;;

let status connection request =
  match Connection.request_without_history connection (Provider_status request) with
  | Ok (Provider_status result) -> Ok result
  | Ok _ -> unexpected "provider.status"
  | Error _ as failure -> failure
;;

let begin_login connection request =
  match Connection.request_without_history connection (Provider_login_begin request) with
  | Ok (Provider_login_begin result) -> Ok result
  | Ok _ -> unexpected "provider.login.begin"
  | Error _ as failure -> failure
;;

let challenge connection request =
  match Connection.request connection (Provider_login_challenge request) with
  | Ok (Private_provider_challenge result) -> Ok result
  | Ok (Non_history _ | Session_get _ | Session_attach _ | Session_create _) ->
    unexpected "provider.login.challenge"
  | Error _ as failure -> failure
;;

let cancel connection request =
  match Connection.request_without_history connection (Provider_login_cancel request) with
  | Ok (Provider_login_cancel result) -> Ok result
  | Ok _ -> unexpected "provider.login.cancel"
  | Error _ as failure -> failure
;;

let logout connection request =
  match Connection.request_without_history connection (Provider_logout request) with
  | Ok (Provider_logout result) -> Ok result
  | Ok _ -> unexpected "provider.logout"
  | Error _ as failure -> failure
;;
