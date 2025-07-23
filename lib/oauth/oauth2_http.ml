(** One-shot HTTP helpers tailored to OAuth&nbsp;2.0 flows.

    The module wraps {!Piaf.Client.Oneshot} so that callers can exchange or
    retrieve JSON documents in just a few lines.  All functions run inside an
    {!Eio} fibre and therefore take an explicit [`env`] and [`sw`] argument –
    the standard pattern for network code in Eio-based applications.

    {1 API at a glance}

    • {!val:get_json}   – `GET` a JSON resource (discovery document, user-info …)
    • {!val:post_form}  – `POST` an
      [application/x-www-form-urlencoded] body (token exchange / refresh)
    • {!val:post_json}  – `POST` a JSON body (dynamic client registration)

    Each helper returns a [(Jsonaf.t, string) Result.t] where the [Error]
    variant is a human-readable description produced by
    {!Piaf.Error.to_string} (transport-layer issues) or {!Exn.to_string}
    (JSON decoding failures).  The only exception is {!val:post_form}, which
    keeps the historical behaviour of **re-raising** the JSON parse error if
    the response is not valid.
*)

open Core

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) res f = bind res ~f
    let ( let+ ) res f = map res ~f [@@warning "-32"]
  end
end

(* Common Piaf config – allow insecure for localhost during development. *)
let piaf_cfg = { Piaf.Config.default with allow_insecure = true }

let get_json ~env ~sw (url : string) : (Jsonaf.t, string) Result.t =
  let open Result.Let_syntax in
  let uri = Uri.of_string url in
  let* resp =
    Piaf.Client.Oneshot.get ~config:piaf_cfg env ~sw uri
    |> Result.map_error ~f:Piaf.Error.to_string
  in
  let* body = Piaf.Body.to_string resp.body |> Result.map_error ~f:Piaf.Error.to_string in
  try Ok (Jsonaf.of_string body) with
  | exn -> Error (Exn.to_string exn)
;;

let post_form ~env ~sw (url : string) (params : (string * string) list)
  : (Jsonaf.t, string) Result.t
  =
  let open Result.Let_syntax in
  let body_str =
    params
    |> List.map ~f:(fun (k, v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v)
    |> String.concat ~sep:"&"
  in
  let headers = [ "content-type", "application/x-www-form-urlencoded" ] in
  let uri = Uri.of_string url in
  let* resp =
    Piaf.Client.Oneshot.post
      ~config:piaf_cfg
      ~headers
      ~body:(Piaf.Body.of_string body_str)
      env
      ~sw
      uri
    |> Result.map_error ~f:Piaf.Error.to_string
  in
  let* body = Piaf.Body.to_string resp.body |> Result.map_error ~f:Piaf.Error.to_string in
  (* If the body is not valid JSON, this will raise an exception. *)
  Ok (Jsonaf.of_string body)
;;

let post_json ~env ~sw (url : string) (json : Jsonaf.t) : (Jsonaf.t, string) Result.t =
  let open Result.Let_syntax in
  let headers = [ "content-type", "application/json" ] in
  let uri = Uri.of_string url in
  let* resp =
    Piaf.Client.Oneshot.post
      ~config:piaf_cfg
      ~headers
      ~body:(Piaf.Body.of_string (Jsonaf.to_string json))
      env
      ~sw
      uri
    |> Result.map_error ~f:Piaf.Error.to_string
  in
  let* body = Piaf.Body.to_string resp.body |> Result.map_error ~f:Piaf.Error.to_string in
  try Ok (Jsonaf.of_string body) with
  | exn -> Error (Exn.to_string exn)
;;
