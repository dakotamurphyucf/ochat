open! Core
module P = Agent_protocol
module Q = P.Inference_query

let unavailable message =
  Error (P.Error.create Incompatible_protocol ~message ~retryable:false ())
;;

let require_feature response feature =
  if List.mem response.P.Initialize.Response.enabled_features feature ~equal:String.equal
  then Ok ()
  else unavailable ("inference read feature was not selected: " ^ feature)
;;

let initialized connection =
  match Connection.initialization connection with
  | Some response -> Ok response
  | None -> unavailable "inference reads require a successful initialization"
;;

let summary connection session_id =
  let open Result.Let_syntax in
  let%bind initialization = initialized connection in
  let%bind () = require_feature initialization Q.Features.observations in
  match
    Connection.request_without_history
      connection
      (Session_inference_summary { session_id })
  with
  | Ok (Session_inference_summary value) -> Ok value
  | Ok _ -> Error (P.Error.invalid_request "unexpected inference summary result")
  | Error _ as failure -> failure
;;

let observations connection (request : Q.Request.t) =
  let open Result.Let_syntax in
  let%bind initialization = initialized connection in
  let%bind () = require_feature initialization Q.Features.observations in
  let%bind () =
    if request.include_configuration
    then require_feature initialization Q.Features.configuration
    else Ok ()
  in
  let%bind () =
    if request.include_diagnostics
    then require_feature initialization Q.Features.diagnostics
    else Ok ()
  in
  let%bind request = Q.Request.of_json (Q.Request.to_json request) in
  if request.page.limit > initialization.limits.max_page_size
  then Error (P.Error.invalid_request "inference page exceeds selected server limit")
  else (
    match
      Connection.request_without_history
        connection
        (Session_inference_observations request)
    with
    | Ok (Session_inference_observations value) -> Ok value
    | Ok _ -> Error (P.Error.invalid_request "unexpected inference observations result")
    | Error _ as failure -> failure)
;;
