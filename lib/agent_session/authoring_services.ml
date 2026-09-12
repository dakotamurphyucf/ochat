open Core
module P = Agent_protocol
module N = Native_tool_invocation
module C = Chat_response.Tool_capability
module Q = Chat_response.Authoring_context
module V = Chat_response.Authoring_validation

type t =
  { env : Eio_unix.Stdenv.base
  ; host : V.host
  ; reference : (Q.t, string) result Lazy.t
  }

let create ~env ~host =
  { env
  ; host
  ; reference =
      lazy
        (let secret = P.Id.Transaction.create () |> P.Id.Transaction.to_string in
         Q.create ~secret ())
  }
;;

let failure code message =
  P.Invocation.{ code; message; retryable = false; details = `Null }
;;

let with_capabilities borrowed f =
  let open Result.Let_syntax in
  let current () =
    N.borrowed_capabilities borrowed
    |> Result.map_error ~f:(fun _ ->
      failure "authoring.denied" "The authoring invocation has expired.")
  in
  let%bind capabilities = current () in
  let%bind response = f capabilities in
  let%bind latest = current () in
  match String.equal (C.fingerprint capabilities) (C.fingerprint latest) with
  | true -> Ok response
  | false ->
    Error (failure "authoring.denied" "Authoring permissions changed during the request.")
;;

let reference t borrowed request =
  with_capabilities borrowed (fun capabilities ->
    let open Result.Let_syntax in
    let%bind service =
      Lazy.force t.reference |> Result.map_error ~f:(failure "authoring.unavailable")
    in
    let invocation = N.borrowed_invocation borrowed in
    let scope =
      [%sexp
        (invocation.context.session_id : P.Id.Session.t)
      , (invocation.context.generation : int)]
      |> Sexp.to_string
    in
    let response =
      Q.query_with_receipt service ~host:t.host ~capabilities ~scope request
    in
    let%map () =
      N.record_authoring_reference borrowed response
      |> Result.map_error ~f:(fun error ->
        failure "authoring.denied" error.P.Error.message)
    in
    response.json)
;;

let validate t borrowed request =
  with_capabilities borrowed (fun capabilities ->
    Ok (V.validate ~env:t.env ~host:t.host ~capabilities request |> V.to_json))
;;
