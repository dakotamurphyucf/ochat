open Core

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) res f = bind res ~f
    let ( let+ ) res f = map res ~f
  end
end

(* Common Piaf config – allow insecure for localhost during development. *)
let piaf_cfg = { Piaf.Config.default with allow_insecure = true }

let get_json ~env ~sw (url : string) : (Jsonaf.t, string) Result.t =
  let open Result.Let_syntax in
  let uri = Uri.of_string url in
  let* resp = Piaf.Client.Oneshot.get ~config:piaf_cfg env ~sw uri
    |> Result.map_error ~f:Piaf.Error.to_string
  in
  let* body =
    Piaf.Body.to_string resp.body |> Result.map_error ~f:Piaf.Error.to_string
  in
  Ok (Jsonaf.of_string body)

let post_form ~env ~sw (url : string) (params : (string * string) list) : (Jsonaf.t, string) Result.t =
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
  let* body =
    Piaf.Body.to_string resp.body |> Result.map_error ~f:Piaf.Error.to_string
  in
  Ok (Jsonaf.of_string body)

