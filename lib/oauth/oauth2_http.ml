open Core

let piaf_cfg = { Piaf.Config.default with allow_insecure = true }

let protect f =
  try f () with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> Error "OAuth operation failed"
;;

let decode_json body =
  try Ok (Jsonaf.of_string body) with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> Error "invalid OAuth JSON response"
;;

let transport_error message = function
  | `Exn (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | _ -> message
;;

let read_response response =
  let open Result.Let_syntax in
  let%bind resp =
    Result.map_error response ~f:(transport_error "OAuth transport failure")
  in
  let status = Piaf.Status.to_code resp.Piaf.Response.status in
  let%bind body =
    Piaf.Body.to_string resp.body
    |> Result.map_error ~f:(transport_error "OAuth response body failure")
  in
  if status >= 200 && status < 300
  then decode_json body
  else Error (sprintf "OAuth HTTP status %d" status)
;;

let get_json ~env ~sw url =
  protect (fun () ->
    Piaf.Client.Oneshot.get ~config:piaf_cfg env ~sw (Uri.of_string url) |> read_response)
;;

let post ~env ~sw ~content_type url body =
  protect (fun () ->
    Piaf.Client.Oneshot.post
      ~config:piaf_cfg
      ~headers:[ "content-type", content_type ]
      ~body:(Piaf.Body.of_string body)
      env
      ~sw
      (Uri.of_string url)
    |> read_response)
;;

let post_form ~env ~sw url params =
  let body =
    List.map params ~f:(fun (key, value) ->
      Uri.pct_encode key ^ "=" ^ Uri.pct_encode value)
    |> String.concat ~sep:"&"
  in
  post ~env ~sw ~content_type:"application/x-www-form-urlencoded" url body
;;

let post_json ~env ~sw url json =
  post ~env ~sw ~content_type:"application/json" url (Jsonaf.to_string json)
;;
