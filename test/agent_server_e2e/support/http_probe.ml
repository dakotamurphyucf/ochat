open Core
module P = Piaf

let client_config = { P.Config.default with allow_insecure = true }

let decode_response response =
  let open Result.Let_syntax in
  let status = P.Status.to_code response.P.Response.status in
  let%bind body =
    P.Body.to_string response.body |> Result.map_error ~f:P.Error.to_string
  in
  if not (Int.equal status 200)
  then Error (sprintf "health request returned HTTP %d: %s" status body)
  else (
    try
      Jsonaf.of_string body
      |> Agent_protocol.Health.Response.of_json
      |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
    with
    | exn -> Error ("invalid health response: " ^ Exn.to_string exn))
;;

let get_health env ~port ~token =
  Eio.Switch.run (fun sw ->
    let uri = Uri.of_string (sprintf "http://127.0.0.1:%d/v1/health" port) in
    let headers = [ "authorization", "Bearer " ^ token ] in
    P.Client.Oneshot.get ~config:client_config ~headers ~sw env uri
    |> Result.map_error ~f:P.Error.to_string
    |> Result.bind ~f:decode_response)
;;
