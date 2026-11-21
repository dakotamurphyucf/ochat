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

let read_text connection request =
  let module Range = P.Recovery_text_range in
  let open Result.Let_syntax in
  let%bind initialized_as =
    check_host connection ~feature:Range.feature (Range.session request)
  in
  let%bind result =
    Connection.request_recovery connection ~initialized_as (Read_text request)
  in
  match result with
  | Recovery_range slice ->
    let%map () = Range.Reply.validate slice ~request in
    slice
  | _ -> Error (P.Error.invalid_request "unexpected recovery text range result")
;;

let read_asset connection request =
  let module Asset = P.Recovery_asset_read in
  let open Result.Let_syntax in
  let%bind initialized_as =
    check_host connection ~feature:Asset.feature (Asset.session request)
  in
  let%bind result =
    Connection.request_recovery connection ~initialized_as (Read_asset request)
  in
  match result with
  | Recovery_asset_read chunk ->
    let%map () = Asset.Reply.validate chunk ~request in
    chunk
  | _ -> Error (P.Error.invalid_request "unexpected recovery asset result")
;;

module Control = P.Recovery_rollover_control

let inspect_rollover connection (request : Control.Inspect.t) =
  let open Result.Let_syntax in
  let%bind initialized_as =
    check_host connection ~feature:Control.feature request.session
  in
  let%bind result =
    Connection.request_recovery connection ~initialized_as (Inspect_rollover request)
  in
  match result with
  | Recovery_status status ->
    let%map () = Control.Status.validate status ~request in
    status
  | _ -> Error (P.Error.invalid_request "unexpected recovery status result")
;;

let request_rollover connection request =
  let open Result.Let_syntax in
  let%bind initialized_as =
    check_host connection ~feature:Control.feature (Control.Request.session request)
  in
  let%bind result =
    Connection.request_recovery connection ~initialized_as (Request_rollover request)
  in
  match result with
  | Recovery_rollover receipt ->
    let%map () = Control.Receipt.validate receipt ~request in
    receipt
  | _ -> Error (P.Error.invalid_request "unexpected recovery rollover result")
;;

module Note_shares = struct
  module Wire = P.Recovery_note_share_control
  module W = Wire

  let execute connection request =
    let open Result.Let_syntax in
    let source = W.Request.source request in
    let%bind initialized_as = check_host connection ~feature:W.feature source.session in
    let%bind result =
      Connection.request_recovery connection ~initialized_as (Note_shares request)
    in
    match result with
    | Recovery_note_shares result ->
      let%map () = W.Response.validate result ~request in
      result
    | _ -> Error (P.Error.invalid_request "unexpected recovery note management reply")
  ;;

  let inspect connection source ?path () =
    match%bind.Result execute connection (Inspect { source; path }) with
    | Inspected metadata -> Ok metadata
    | Shared _ | Revoked _ ->
      Error (P.Error.invalid_request "unexpected note metadata reply")
  ;;

  let create connection source ~expected ~selection ~recipient ~expires_at ~key =
    match%bind.Result
      execute
        connection
        (Share { source; expected; selection; recipient; expires_at; key })
    with
    | Shared receipt -> Ok receipt
    | Inspected _ | Revoked _ ->
      Error (P.Error.invalid_request "unexpected note share receipt")
  ;;

  let revoke connection source ~expected_share_revision reference ~key =
    match%bind.Result
      execute connection (Revoke { source; expected_share_revision; reference; key })
    with
    | Revoked receipt -> Ok receipt
    | Inspected _ | Shared _ ->
      Error (P.Error.invalid_request "unexpected note revocation receipt")
  ;;
end
