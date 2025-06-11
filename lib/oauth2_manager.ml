open Core

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) r f = bind r ~f
    let ( let+ ) r f = map r ~f
  end
end

module Tok = Oauth2_types.Token

let cache_dir : string =
  match Sys.getenv "XDG_CACHE_HOME" with
  | Some d -> Filename.concat d "ocamlgpt/tokens"
  | None ->
    (match Sys.getenv "HOME" with
     | Some home -> Filename.concat home ".cache/ocamlgpt/tokens"
     | None -> Filename.concat "." ".cache/ocamlgpt/tokens")
;;

let cache_file issuer =
  let digest = Md5.digest_string issuer |> Md5.to_hex in
  Filename.concat cache_dir (digest ^ ".json")
;;

let load issuer : (Tok.t, string) Result.t =
  try
    let s = In_channel.read_all (cache_file issuer) in
    Ok (Tok.t_of_jsonaf (Jsonaf.of_string s))
  with
  | _ -> Error "token_cache_read"
;;

let store issuer tok =
  try
    Core_unix.mkdir_p cache_dir;
    Out_channel.write_all
      (cache_file issuer)
      ~data:(Jsonaf.to_string (Tok.jsonaf_of_t tok))
  with
  | _ -> ()
;;

type creds =
  | Client_secret of
      { id : string
      ; secret : string
      ; scope : string option
      }

let obtain ~env ~sw issuer = function
  | Client_secret { id; secret; scope } ->
    Oauth2_client_credentials.fetch_token
      ~env
      ~sw
      ~token_uri:(issuer ^ "/token")
      ~client_id:id
      ~client_secret:secret
      ?scope
      ()
;;

(* PKCE flow not yet supported in the lightweight OAuth client – return an
     explicit error so callers can fall back to the headless
     client-credentials grant. *)
(* | _ -> Error "PKCE flow not supported in this build" *)

let get ~env ~sw ~issuer creds : (Tok.t, string) Result.t =
  match load issuer with
  | Ok tok when not (Tok.is_expired tok) -> Ok tok
  | _ ->
    let open Result.Let_syntax in
    let* tok = obtain ~env ~sw issuer creds in
    let () = store issuer tok in
    Ok tok
;;
