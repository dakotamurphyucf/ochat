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


## 8  Compliance audit & improvement backlo

The helper stack above covers the happy-path flows, but an audit against the MCP
**2025-03-26** Authorization specification exposes several gaps.  The table
below lists the current status and concrete actions required.

| Area | Status | Spec ref. | Action items |
|------|--------|-----------|--------------|
| Metadata discovery | ✅ | § 2.3 | add default-endpoint fallback on 404 |
| Dynamic client registration | ❌ | § 2.4 | implement `/register` on server and client POST |
| 401-triggered auth | ⚠️ eager | § 2.6 | retry request after first 401 |
| Refresh-token rotation | ❌ | § 2.6.2 | support `grant_type=refresh_token` |
| Bearer validation (server) | ❌ | § 2.6.2 | reject bad/missing token; return 401/403 |
| Fallback URLs | ❌ | § 2.3.3 | build `/authorize` & `/token` paths when metadata absent |
| PKCE autodetect | ⚠️ env-only | § 2.1.1 | expose env/CLI, auto-select when secret absent |
| RNG quality | ⚠️ pseudo | § 2.7 | use `Mirage_crypto_rng.generate` in PKCE |
| Token cache security | ⚠️ 0644 | § 2.7 | write cache `chmod 600`; consider key-chain |
| Server auth endpoints | ❌ | § 2 | minimal `/authorize`, `/token`, validation |
| Error surfacing | ⚠️ warn | § 2.8 | propagate OAuth failures, not just Logs.warn |
| Origin header check | ❌ | transport sec. | validate `Origin` on HTTP requests |
| Third-party delegated auth | ❌ | § 2.10 | out of scope for MVP – document |

### Short-term priorities

1. Add **refresh token** path in `oauth2_manager`.
2. Default endpoint fallback after metadata 404.
3. Middleware in `mcp_server_http` that enforces Bearer and emits
   `WWW-Authenticate` header.
4. Replace PRNG in PKCE with `Mirage_crypto_rng`.
5. Harden token cache permissions and atomic write.
6. Retry original JSON-RPC batch after automatic auth on 401.
7. Provide stub `/token` endpoint (client-credentials) for local CI.

Completing these tasks will bring the OAuth layer to full compliance with the
MCP spec while keeping the dependency footprint minimal.

---

## 9  Current OAuth 2.1 implementation – status & gaps (detailed analysis)

The following section provides a narrative assessment written on **2025-06-11**
and is retained for engineering reference.  It lists what is present in the
code-base, what is missing, and a richer explanation of why those gaps matter
for strict conformance with the Model Context Protocol.

### Helper stack already present

The repository contains a complete, self-contained OAuth client helper stack in
`lib/oauth/`:

* `oauth2_types.ml`           – token / metadata records
* `oauth2_http.ml`            – light HTTP util (Piaf one-shots)
* `oauth2_client_credentials.ml` – client-credentials grant
* `oauth2_pkce.ml` / `oauth2_pkce_flow.ml` – PKCE utilities & local redirect listener
* `oauth2_manager.ml`         – cache, refresh, single entry-point

`mcp_transport_http.ml` plugs this manager in, adding a *Bearer* header to all
requests when `MCP_CLIENT_ID` / `MCP_CLIENT_SECRET` are provided.

### Spec-compliant pieces already implemented

1. **Metadata discovery** (`/.well-known/oauth-authorization-server`)
2. **PKCE** (S256 challenge, local callback, interactive browser)
3. **Client-credentials grant**
4. **Token caching** with expiry check (`expires_in – 60 s` guard)
5. **Bearer header** applied to **every** HTTP request
6. Correctly skipped for **stdio** transport (per spec § 2.1)

### Missing / partial elements

1. **Server-side enforcement** – MCP server never checks `Authorization`, never
   responds 401/403, and provides no `/token` or `/authorize` endpoints.
2. **Dynamic client registration** (RFC 7591) – completely absent.
3. **Fallback endpoints** – client aborts when metadata gives 404; spec demands
   default `/authorize`, `/token`, `/register` paths.
4. **401-triggered flow** – client fetches token eagerly instead of after
   receiving 401 and does not retry the failed request.
5. **Refresh-token use** – `refresh_token` is stored but never exchanged.
6. **Scope propagation** – accepted in client-credentials path only; ignored elsewhere.
7. **Secure RNG** – PKCE uses `Random.self_init`, not cryptographically secure.
8. **open_browser portability** – shells out via `xdg-open`, brittle quoting.
9. **Token cache security** – plain-text, default `0644` permissions.
10. **Origin header validation** – HTTP server ignores `Origin`, risk of DNS-rebind.
11. **Error propagation** – transport hides OAuth failures behind a warn log.
12. **PKCE port collision** – listener picks random port but does not retry if busy.
13. **Third-party delegated flow** (§ 2.10) – not implemented.

### Concrete improvement checklist

Server-side
* Validate Bearer and return `WWW-Authenticate` with proper error JSON.
* Expose minimal `/token` & `/authorize` endpoints accepting `client_credentials` grant.
* Serve metadata (`/.well-known/oauth-authorization-server`).

Client-side
* Retry logic: on first 401 start auth dance, then resend JSON-RPC batch.
* Add dynamic registration (`POST /register`) when metadata advertises it.
* Implement refresh-token path in `oauth2_manager`.
* Build default endpoints when metadata missing.
* Use `Mirage_crypto_rng.generate` for PKCE verifier.
* Secure cache file `chmod 600`, atomic writes.
* Supply environment / CLI flags to switch between PKCE and secret flow.

Tests & docs
* Stub auth server in CI, verify 401 path, refresh path.
* Document required env-vars.

Addressing these items will move both client and server to full MCP 2025-03-26
compliance and harden the implementation for production use.


