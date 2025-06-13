open Core

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) r f = bind r ~f
    let ( let+ ) r f = map r ~f
  end
end

module Tok = Oauth2_types.Token

let cache_dir () : string =
  match Sys.getenv "XDG_CACHE_HOME" with
  | Some d -> Filename.concat d "ocamlgpt/tokens"
  | None ->
    (match Sys.getenv "HOME" with
     | Some home -> Filename.concat home ".cache/ocamlgpt/tokens"
     | None -> Filename.concat "." ".cache/ocamlgpt/tokens")
;;

let cache_file issuer =
  let digest = Md5.digest_string issuer |> Md5.to_hex in
  Filename.concat (cache_dir ()) (digest ^ ".json")
;;

(*────────────────────────  Metadata retrieval with fallback  ─────────────*)

let fallback_metadata ~(issuer : string) : Oauth2_types.Metadata.t =
  (* Strip any path component – we only want scheme://host[:port] *)
  let uri = Uri.of_string issuer in
  let base = Uri.with_path uri "" in
  let base_s = Uri.to_string base |> String.rstrip ~drop:(fun c -> Char.(c = '/')) in
  let path p = base_s ^ p in
  { Oauth2_types.Metadata.authorization_endpoint = path "/authorize"
  ; token_endpoint = path "/token"
  ; registration_endpoint = Some (path "/register")
  }
;;

let fetch_metadata ~env ~sw ~(issuer : string)
  : (Oauth2_types.Metadata.t, string) Result.t
  =
  match
    Oauth2_http.get_json ~env ~sw (issuer ^ "/.well-known/oauth-authorization-server")
  with
  | Ok json ->
    (* Attempt to parse the metadata from the JSON response *)
    (* If parsing fails, fall back to a basic metadata structure *)
    (try Ok (Oauth2_types.Metadata.t_of_jsonaf json) with
     | _ -> Ok (fallback_metadata ~issuer))
  | Error err ->
    if true then failwith err;
    (* If the request fails, fall back to a basic metadata structure *)
    (* This is useful for servers that do not support the well-known endpoint *)
    Ok (fallback_metadata ~issuer)
;;

let load ~env issuer : (Tok.t, string) Result.t =
  let fs = Eio.Stdenv.fs env in
  let rel = cache_file issuer in
  let path = Eio.Path.(fs / rel) in
  try
    let stats = Eio.Path.stat ~follow:true path in
    if stats.perm land 0o077 <> 0
    then Error "insecure_token_cache_permissions"
    else (
      let s = Eio.Path.load path in
      Ok (Tok.t_of_jsonaf (Jsonaf.of_string s)))
  with
  | _ -> Error "token_cache_read"
;;

let store ~env issuer tok =
  try
    Io.mkdir ~exists_ok:true ~dir:(Eio.Stdenv.fs env) (cache_dir ());
    let fs = Eio.Stdenv.fs env in
    let tmp = cache_file issuer ^ ".tmp" in
    let final = cache_file issuer in
    let tmp_path = Eio.Path.(fs / tmp) in
    let final_path = Eio.Path.(fs / final) in
    (* Ensure the directory exists *)
    (* Write temporary file with strict permissions *)
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      tmp_path
      (Jsonaf.to_string (Tok.jsonaf_of_t tok));
    (try Eio.Path.rename tmp_path final_path with
     | _ -> ());
    ()
  with
  | _ -> ()
;;

(*────────────────────────  Refresh token flow  ─────────────────────────*)

type creds =
  | Client_secret of
      { id : string
      ; secret : string
      ; scope : string option
      }
  | Pkce of { client_id : string }

let refresh_access_token ~env ~sw ~issuer creds (tok : Tok.t) : (Tok.t, string) Result.t =
  match tok.refresh_token with
  | None -> Error "no_refresh_token"
  | Some refresh_token ->
    let open Result.Let_syntax in
    (match creds with
     | Client_secret { id; secret; scope = _ } ->
       let params =
         [ "grant_type", "refresh_token"
         ; "refresh_token", refresh_token
         ; "client_id", id
         ; "client_secret", secret
         ]
       in
       let* json = Oauth2_http.post_form ~env ~sw (issuer ^ "/token") params in
       Ok
         Tok.
           { (Tok.t_of_jsonaf json) with
             obtained_at = Eio.Time.now (Eio.Stdenv.clock env)
           }
     | Pkce { client_id } ->
       let* meta = fetch_metadata ~env ~sw ~issuer in
       let params =
         [ "grant_type", "refresh_token"
         ; "refresh_token", refresh_token
         ; "client_id", client_id
         ]
       in
       let* json = Oauth2_http.post_form ~env ~sw meta.token_endpoint params in
       Ok
         Tok.
           { (Tok.t_of_jsonaf json) with
             obtained_at = Eio.Time.now (Eio.Stdenv.clock env)
           })
;;

let obtain ~env ~sw issuer = function
  | Client_secret { id; secret; scope } ->
    (* If the client credentials flow is not supported, we can return an error or
       handle it differently based on the application's requirements. *)
    Oauth2_client_credentials.fetch_token
      ~env
      ~sw
      ~token_uri:(issuer ^ "/token")
      ~client_id:id
      ~client_secret:secret
      ?scope
      ()
  | Pkce { client_id } ->
    let open Result.Let_syntax in
    let* meta = fetch_metadata ~env ~sw ~issuer in
    let code, verifier, redirect = Oauth2_pkce_flow.run ~env ~sw ~meta ~client_id in
    Oauth2_pkce_flow.exchange_token
      ~env
      ~sw
      ~meta
      ~client_id
      ~code
      ~code_verifier:verifier
      ~redirect_uri:redirect
;;

(* PKCE flow not yet supported in the lightweight OAuth client – return an
     explicit error so callers can fall back to the headless
     client-credentials grant. *)
(* | _ -> Error "PKCE flow not supported in this build" *)

let get ~env ~sw ~issuer creds : (Tok.t, string) Result.t =
  match load ~env issuer with
  | Ok tok when not (Tok.is_expired tok) ->
    let () = store ~env issuer tok in
    Ok tok
  | Ok tok ->
    (* Token expired – attempt refresh first *)
    (let open Result.Let_syntax in
     let* refreshed = refresh_access_token ~env ~sw ~issuer creds tok in
     if Tok.is_expired refreshed
     then Error "refresh_yielded_expired_token"
     else (
       store ~env issuer refreshed;
       Ok refreshed))
    |> (function
     | Ok t -> Ok t
     | Error _ ->
       let open Result.Let_syntax in
       let* tok = obtain ~env ~sw issuer creds in
       let () = store ~env issuer tok in
       Ok tok)
  | Error _ ->
    let open Result.Let_syntax in
    let* tok = obtain ~env ~sw issuer creds in
    let () = store ~env issuer tok in
    Ok tok
;;
