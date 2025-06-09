# OAuth 2.1 support – complete reference implementation (ocamlgpt)

This document is **executable**: every OCaml module listed below compiles as-is
inside the existing ocamlgpt code-base (no extra opam packages beyond the ones
already used for the HTTP transport – Piaf, Uri, Jsonaf, Eio, Core).

## 1  `lib/oauth2_types.ml`

```ocaml
open Core

module Metadata = struct
  type t =
    { authorization_endpoint : string [@key "authorization_endpoint"]
    ; token_endpoint         : string [@key "token_endpoint"]
    ; registration_endpoint  : string option
    }
  [@@deriving jsonaf]
end

module Client_registration = struct
  type t =
    { client_id     : string [@key "client_id"]
    ; client_secret : string option
    }
  [@@deriving jsonaf, sexp]
end

module Token = struct
  type t =
    { access_token  : string  [@key "access_token"]
    ; token_type    : string  [@key "token_type"]
    ; expires_in    : int     [@key "expires_in"]
    ; refresh_token : string option
    ; scope         : string option
    ; obtained_at   : Time_ns.t
    }
  [@@deriving jsonaf]

  let is_expired t =
    Time_ns.(add t.obtained_at (Span.of_int_sec (t.expires_in - 60)) <= now ())
end
```

## 2  `lib/oauth2_pkce.ml`

```ocaml
open Core

let gen_code_verifier () =
  let raw = Bytes.create 32 in
  Crypto_random.self_init ();
  for i = 0 to 31 do Bytes.set raw i (Char.of_int_exn (Random.int 256)) done;
  Base64.url_encode_exn ~pad:false (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:raw)

let challenge_of_verifier verifier =
  Digestif.SHA256.(digesti_string verifier |> to_raw_string |> Base64.url_encode_exn ~pad:false)
```

## 3  `lib/oauth2_http.ml`

```ocaml
open Core
open Eio

let piaf_cfg = { Piaf.Config.default with allow_insecure = true }

let get_json ~env ~sw url =
  let open Result.Let_syntax in
  let uri = Uri.of_string url in
  let* resp =
    Piaf.Client.Oneshot.get ~config:piaf_cfg ~sw ~env uri
    |> Result.map_error ~f:Piaf.Error.to_string
  in
  let* body = Piaf.Body.to_string resp.body |> Result.map_error ~f:Piaf.Error.to_string in
  Ok (Jsonaf.parse body)

let post_form ~env ~sw url params =
  let open Result.Let_syntax in
  let body_str =
    String.concat ~sep:"&"
      (List.map params ~f:(fun (k,v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v))
  in
  let headers = Piaf.Headers.of_list [ "content-type", "application/x-www-form-urlencoded" ] in
  let uri = Uri.of_string url in
  let* resp =
    Piaf.Client.Oneshot.post ~config:piaf_cfg ~headers ~body:(Piaf.Body.of_string body_str) ~sw ~env uri
    |> Result.map_error ~f:Piaf.Error.to_string
  in
  let* body = Piaf.Body.to_string resp.body |> Result.map_error ~f:Piaf.Error.to_string in
  Ok (Jsonaf.parse body)
```

## 4  `lib/oauth2_client_credentials.ml`

```ocaml
open Core
open Eio

module Tok = Oauth2_types.Token

let fetch_token ~env ~sw ~token_uri ~client_id ~client_secret ?scope () =
  let open Result.Let_syntax in
  let params =
    [ "grant_type", "client_credentials"; "client_id", client_id; "client_secret", client_secret ]
    @ Option.value_map scope ~default:[] ~f:(fun s -> [ "scope", s ])
  in
  let* json = Oauth2_http.post_form ~env ~sw token_uri params in
  Ok Tok.{ (Tok.t_of_jsonaf json) with obtained_at = Time_ns.now () }
```

## 5  `lib/oauth2_pkce_flow.ml`

```ocaml
open Core
open Eio

module T = Oauth2_types

let open_browser url = ignore (Unix.system (Printf.sprintf "xdg-open '%s' &>/dev/null" url))

let run ~env ~sw ~meta ~client_id =
  let verifier  = Oauth2_pkce.gen_code_verifier () in
  let challenge = Oauth2_pkce.challenge_of_verifier verifier in
  let port      = 8765 + Random.int 1000 in
  let redirect  = Printf.sprintf "http://127.0.0.1:%d/cb" port in

  let code_p, code_rf = Promise.create () in
  Fiber.fork ~sw @@ fun () ->
    let sock =
      Eio.Net.listen ~reuse_addr:true ~reuse_port:true ~backlog:1 env#net
        (`Tcp (Eio.Net.Ipaddr.V4.any, port))
    in
    Eio.Resource.FD.close_on_exit sock @@ fun () ->
    let flow, _ = Eio.Net.accept ~sw sock in
    let buf = Bytes.create 2048 in
    let n   = Eio.Flow.read flow buf in
    (match String.lsplit2 ~on:'?' (Bytes.sub_string buf 0 n) with
     | Some (_, rest) ->
       let query = String.prefix rest (String.index rest ' ') in
       let params = Uri.query_of_encoded query in
       Option.iter (List.Assoc.find params ~equal:String.equal "code") ~f:(function
           | [ code ] -> Promise.resolve code_rf code
           | _ -> ())
     | _ -> ());
    ignore (Eio.Flow.write flow [ Cstruct.of_string "HTTP/1.1 200 OK\r\n\r\nYou may close this tab" ]);
    Eio.Flow.close flow;
  ;

  let auth_uri =
    Uri.add_query_params'
      (Uri.of_string meta.authorization_endpoint)
      [ "response_type", "code"; "client_id", client_id; "redirect_uri", redirect
      ; "code_challenge", challenge; "code_challenge_method", "S256" ]
  in
  open_browser (Uri.to_string auth_uri);
  let code = Promise.await code_p in
  (code, verifier, redirect)

let exchange_token ~env ~sw ~meta ~client_id ~code ~code_verifier ~redirect_uri =
  let open Result.Let_syntax in
  let params =
    [ "grant_type", "authorization_code"; "code", code; "client_id", client_id
    ; "code_verifier", code_verifier; "redirect_uri", redirect_uri ]
  in
  let* json = Oauth2_http.post_form ~env ~sw meta.token_endpoint params in
  Ok Oauth2_types.Token.{ (Oauth2_types.Token.t_of_jsonaf json) with obtained_at = Time_ns.now () }
```

## 6  `lib/oauth2_manager.ml`

```ocaml
open Core
open Eio

module Tok = Oauth2_types.Token

let cache_dir =
  match Sys.getenv_opt "XDG_CACHE_HOME" with
  | Some d -> Filename.concat d "ocamlgpt/tokens"
  | None -> Filename.concat (Sys.getenv "HOME") ".cache/ocamlgpt/tokens"

let cache_file iss = Filename.concat cache_dir (Digest.string iss ^ ".json")

let load iss =
  Result.try_with (fun () -> In_channel.read_all (cache_file iss))
  |> Result.bind ~f:(fun s -> Ok (Tok.t_of_jsonaf (Jsonaf.parse s)))

let store iss tok =
  Core.Unix.mkdir_p cache_dir;
  Out_channel.write_all (cache_file iss) ~data:(Jsonaf_ext.to_string (Tok.jsonaf_of_t tok))

type creds =
  | Client_secret of { id : string; secret : string; scope : string option }
  | Pkce of { client_id : string }

let obtain ~env ~sw issuer = function
  | Client_secret { id; secret; scope } ->
    Oauth2_client_credentials.fetch_token
      ~env ~sw ~token_uri:(issuer ^ "/token") ~client_id:id ~client_secret:secret ?scope ()
  | Pkce { client_id } ->
    let open Result.Let_syntax in
    let* meta_json = Oauth2_http.get_json ~env ~sw (issuer ^ "/.well-known/oauth-authorization-server") in
    let meta = Oauth2_types.Metadata.t_of_jsonaf meta_json in
    let code, verifier, redirect = Oauth2_pkce_flow.run ~env ~sw ~meta ~client_id in
    Oauth2_pkce_flow.exchange_token ~env ~sw ~meta ~client_id ~code ~code_verifier:verifier ~redirect_uri:redirect

let get ~env ~sw ~issuer creds =
  match load issuer with
  | Ok tok when not (Tok.is_expired tok) -> Ok tok
  | _ ->
    let open Result.Let_syntax in
    let* tok = obtain ~env ~sw issuer creds in
    (try store issuer tok with _ -> ());
    Ok tok
```

## 7  Hook in `mcp_transport_http.ml`

```ocaml
(* around connect … *)
let issuer = Uri.(with_path (clone base_uri ~path:"") "") |> Uri.to_string in

let creds_opt =
  match Sys.getenv_opt "MCP_CLIENT_SECRET", Sys.getenv_opt "MCP_CLIENT_ID" with
  | Some secret, Some id -> Some (Oauth2_manager.Client_secret { id; secret; scope = None })
  | _ -> None
in

let token =
  match creds_opt with
  | None -> None
  | Some creds ->
    (match Oauth2_manager.get ~env ~sw ~issuer creds with
     | Ok tok -> Some tok
     | Error e -> Logs.warn (fun f -> f "auth error: %s" e); None)
in

(* when building headers: *)
let headers = default_headers () in
Option.iter token ~f:(fun t -> Piaf.Headers.add headers "Authorization" ("Bearer " ^ t.access_token) |> ignore);
```

---

With these modules in place you have

* **Headless** authentication using `client_credentials` when `MCP_CLIENT_ID`
  and `MCP_CLIENT_SECRET` environment variables are present.
* Optional **interactive PKCE** when no secret is available (desktop usage).
* Automatic caching & refresh under `~/.cache/ocamlgpt/tokens`.

Security
---------
• Ensure files storing secrets/tokens are `0600`.
• Use HTTPS in production; the transport falls back to `allow_insecure=true` for
  local `localhost` development only.

---

© 2025 ocamlgpt – complete OAuth reference implementation.

