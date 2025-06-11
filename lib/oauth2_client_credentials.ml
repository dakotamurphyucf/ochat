open Core

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) r f = bind r ~f
    let ( let+ ) r f = map r ~f
  end
end

module Tok = Oauth2_types.Token

let fetch_token
      ~env
      ~sw
      ~(token_uri : string)
      ~(client_id : string)
      ~(client_secret : string)
      ?scope
      ()
  : (Tok.t, string) Result.t
  =
  let open Result.Let_syntax in
  let params =
    [ "grant_type", "client_credentials"
    ; "client_id", client_id
    ; "client_secret", client_secret
    ]
    @ Option.value_map scope ~default:[] ~f:(fun s -> [ "scope", s ])
  in
  let* json = Oauth2_http.post_form ~env ~sw token_uri params in
  Ok Tok.{ (Tok.t_of_jsonaf json) with obtained_at = Caml_unix.gettimeofday () }
;;
