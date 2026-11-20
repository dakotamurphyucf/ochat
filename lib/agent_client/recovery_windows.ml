open! Core
module P = Agent_protocol
module Q = P.Recovery_query
module Scope = P.Recovery_window.Scope

let check_host connection ~feature session =
  match Connection.initialization connection with
  | Some negotiated
    when P.Id.Server.equal negotiated.server_id (P.Session_ref.server_id session) ->
    if List.mem negotiated.enabled_features feature ~equal:String.equal
    then Ok negotiated
    else
      Error
        (P.Error.create
           Method_not_found
           ~message:"this connection has no requested recovery feature"
           ~retryable:false
           ())
  | Some _ | None ->
    Error
      (P.Error.invalid_request "recovery requires initialization on the selected host")
;;

let enable connection (request : Q.Enable.t) =
  let open Result.Let_syntax in
  let%bind initialized_as =
    check_host connection ~feature:Q.Features.windows request.session
  in
  let%bind result =
    Connection.request_recovery connection ~initialized_as (Enable request)
  in
  match result with
  | Recovery_enable scope
    when P.Session_ref.equal request.session (Scope.session scope)
         && request.expected_generation = Scope.generation scope -> Ok scope
  | _ -> Error (P.Error.invalid_request "unexpected recovery activation result")
;;

let list connection (request : Q.List_windows.t) =
  let open Result.Let_syntax in
  let%bind initialized_as =
    check_host connection ~feature:Q.Features.windows request.session
  in
  let%bind result =
    Connection.request_recovery connection ~initialized_as (List_windows request)
  in
  match result with
  | Recovery_windows page
    when List.length page.items <= request.page.limit
         && List.for_all page.items ~f:(fun (row : Q.Summary.t) ->
           P.Session_ref.equal request.session (Scope.session row.scope)
           && Option.for_all
                request.generation
                ~f:(Int.equal (Scope.generation row.scope))
           && Option.for_all request.state ~f:(Q.State.equal row.state)) -> Ok page
  | _ -> Error (P.Error.invalid_request "unexpected recovery window page")
;;

let query connection (request : P.Recovery_history_request.t) =
  let open Result.Let_syntax in
  let%bind initialized_as =
    check_host connection ~feature:P.Recovery_history_request.feature request.session
  in
  let%bind result =
    Connection.request_recovery connection ~initialized_as (Query_history request)
  in
  match result with
  | Recovery_history page ->
    let%map () = P.Recovery_history_page.validate page ~request in
    page
  | _ -> Error (P.Error.invalid_request "unexpected recovery history result")
;;
