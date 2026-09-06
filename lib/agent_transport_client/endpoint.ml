open! Core

type t =
  | Socket_endpoint of { path : string }
  | Http_endpoint of
      { uri : Uri.t
      ; bearer_token : string option
      }

type kind =
  | Unix_socket
  | Http
[@@deriving compare, equal, sexp_of]

let invalid message = Agent_protocol.Error.invalid_request message

let interrupted description exn =
  Agent_protocol.Error.create
    Interrupted
    ~message:(description ^ " connection failed: " ^ Exn.to_string exn)
    ~retryable:true
    ()
;;

let expand_home ~home path =
  match String.chop_prefix path ~prefix:"~/", home with
  | None, _ -> Ok path
  | Some rest, Some home -> Ok (Filename.concat home rest)
  | Some _, None -> Error (invalid "cannot expand ~/ without a home directory")
;;

let unix_endpoint ~home ~bearer_token value =
  let open Result.Let_syntax in
  let%bind () =
    if Option.is_none bearer_token
    then Ok ()
    else Error (invalid "bearer credentials are only valid for HTTP endpoints")
  in
  let%bind path = expand_home ~home value in
  if String.is_empty path
  then Error (invalid "Unix daemon URI has an empty path")
  else if not (Filename.is_absolute path)
  then Error (invalid "Unix daemon URI path must be absolute")
  else Ok (Socket_endpoint { path })
;;

let is_supported_http_scheme = function
  | "http" | "https" -> true
  | _ -> false
;;

let validate_http_uri uri =
  match Uri.scheme uri, Uri.host uri with
  | Some scheme, Some host
    when is_supported_http_scheme (String.lowercase scheme) && not (String.is_empty host)
    ->
    if Option.is_some (Uri.userinfo uri)
    then Error (invalid "HTTP daemon URI must not contain user information")
    else if Option.is_some (Uri.fragment uri)
    then Error (invalid "HTTP daemon URI must not contain a fragment")
    else if not (List.is_empty (Uri.query uri))
    then Error (invalid "HTTP daemon URI must not contain a query")
    else Ok ()
  | Some scheme, _ when not (is_supported_http_scheme (String.lowercase scheme)) ->
    Error (invalid (Printf.sprintf "unsupported daemon URI scheme %S" scheme))
  | _ -> Error (invalid "HTTP daemon URI must include an http(s) scheme and host")
;;

let is_valid_bearer_token token =
  (not (String.is_empty token))
  && String.for_all token ~f:(fun char ->
    let code = Char.to_int char in
    (not (Char.is_whitespace char)) && code >= 32 && code <> 127)
;;

let validate_bearer_token = function
  | None -> Ok ()
  | Some token when is_valid_bearer_token token -> Ok ()
  | Some _ ->
    Error (invalid "HTTP bearer token must be nonempty and contain no whitespace")
;;

let http_endpoint ~bearer_token value =
  let open Result.Let_syntax in
  let uri = Uri.of_string value in
  let%bind () = validate_http_uri uri in
  let%map () = validate_bearer_token bearer_token in
  Http_endpoint { uri; bearer_token }
;;

let create ~home ~bearer_token value =
  match String.chop_prefix value ~prefix:"unix://" with
  | Some path -> unix_endpoint ~home ~bearer_token path
  | None -> http_endpoint ~bearer_token value
;;

let token_path ~env path =
  let base =
    if Filename.is_absolute path then Eio.Stdenv.fs env else Eio.Stdenv.cwd env
  in
  Eio.Path.(base / path)
;;

let load_bearer_token ~env ~path =
  match Result.try_with (fun () -> Eio.Path.load (token_path ~env path)) with
  | Error exn ->
    Error (invalid ("unable to load HTTP bearer token file: " ^ Exn.to_string exn))
  | Ok contents ->
    let token = String.strip contents in
    Result.map (validate_bearer_token (Some token)) ~f:(fun () -> token)
;;

let kind = function
  | Socket_endpoint _ -> Unix_socket
  | Http_endpoint _ -> Http
;;

let description = function
  | Socket_endpoint { path } -> "unix://" ^ path
  | Http_endpoint { uri; _ } -> Uri.to_string uri
;;

let connect_socket ~sw ~env ~path ~notification_capacity =
  Result.try_with (fun () ->
    Agent_transport_socket.Client.connect
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~socket_path:path
      ~max_line_length:(16 * 1024 * 1024)
      ~notification_capacity)
  |> Result.map_error ~f:(interrupted ("daemon " ^ "unix://" ^ path))
;;

let connect t ~sw ~env ~notification_capacity =
  if notification_capacity <= 0
  then Error (invalid "notification capacity must be positive")
  else (
    match t with
    | Socket_endpoint { path } -> connect_socket ~sw ~env ~path ~notification_capacity
    | Http_endpoint { uri; bearer_token } ->
      Agent_transport_http.Client.connect
        ~sw
        ~env
        ~uri
        ~bearer_token
        ~notification_capacity)
;;
