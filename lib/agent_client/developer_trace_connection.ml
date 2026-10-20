open! Core
module P = Agent_protocol
module T = P.Developer_trace_wire

type t =
  { connection : Connection.t
  ; initialization : P.Initialize.Response.t
  ; capabilities : T.Capabilities.t
  }

let incompatible message =
  P.Error.create Incompatible_protocol ~message ~retryable:false ()
;;

let current t =
  match Connection.initialization t.connection with
  | Some initialization when phys_equal initialization t.initialization -> Ok ()
  | Some _ | None -> Error (incompatible "trace initialization changed")
;;

let create connection =
  let open Result.Let_syntax in
  let%bind initialization =
    Connection.initialization connection
    |> Result.of_option ~error:(incompatible "trace requires initialization")
  in
  let%bind capabilities = Connection.trace_capabilities connection in
  let t = { connection; initialization; capabilities } in
  let%map () = current t in
  t
;;

let capabilities t = t.capabilities

let reconnect t connection =
  let open Result.Let_syntax in
  let%bind fresh = create connection in
  let former = t.initialization
  and next = fresh.initialization in
  if
    P.Id.Server.equal former.server_id next.server_id
    && P.Id.Principal.equal former.principal.id next.principal.id
    && String.equal
         former.principal.authentication_kind
         next.principal.authentication_kind
  then Ok fresh
  else Error (incompatible "trace reconnect changed host, principal or authentication")
;;

let cursor_equal left right =
  T.Locator.equal (T.Cursor.locator left) (T.Cursor.locator right)
  && String.equal (T.Cursor.execution_id left) (T.Cursor.execution_id right)
  && Chatml_source.Digest.equal
       (T.Cursor.submission_fingerprint left)
       (T.Cursor.submission_fingerprint right)
  && Chatml_source.Digest.equal
       (T.Cursor.selection_fingerprint left)
       (T.Cursor.selection_fingerprint right)
  && Int64.equal (T.Cursor.after_sequence left) (T.Cursor.after_sequence right)
;;

let page t request =
  let open Result.Let_syntax in
  let%bind () = current t in
  let limits = T.Capabilities.limits t.capabilities in
  let%bind response =
    Connection.request_trace t.connection ~limits (T.Request.Page request)
  in
  match response with
  | T.Response.Page (T.Availability.Retained page)
    when T.Locator.equal (T.Page.locator page) (T.Page_request.locator request)
         && Option.equal cursor_equal (T.Page.after page) (T.Page_request.after request)
         && List.length (T.Page.events page) <= T.Page_request.limit request ->
    Ok (T.Availability.Retained page)
  | T.Response.Page ((T.Availability.Unavailable | Expired) as availability) ->
    Ok availability
  | T.Response.Page (T.Availability.Retained _) ->
    Error (incompatible "trace page changed request identity or item limit")
  | T.Response.Capabilities _ -> Error (incompatible "trace page response changed method")
;;
