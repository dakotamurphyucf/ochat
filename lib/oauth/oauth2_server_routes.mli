open Piaf

(** HTTP route handlers for a *minimal* OAuth&nbsp;2.0 Authorisation Server.

    The module wires together the in-memory tables provided by
    {!module:Oauth2_server_storage} and
    {!module:Oauth2_server_client_storage} and exposes four thin HTTP
    endpoints that cover the *happy path* required by the rest of the code
    base.

    {2 Dispatch table}

    {ul
      {li `/.well-known/oauth-authorization-server` ⟶
          {!val:handle_metadata}}
      {li `/token`   ⟶ {!val:handle_token}}
      {li `/authorize` ⟶ {!val:handle_authorize}}
      {li `/register`  ⟶ {!val:handle_register}} }

    All helpers are *exception-free*: internal errors are mapped to an
    appropriate HTTP status code and a short problem JSON in the body.

    The implementation is intentionally **state-of-the-art but not
    production-ready** – all state lives in the current OCaml process, with
    no persistence, replication or concurrency guarantees.

  {1 Example – mounting the routes with Piaf / Eio}

  The snippet below starts a tiny HTTP server on port 8080 that exposes
  all four endpoints under the root URI space (no URL prefix):

  {[
    open Piaf

    let () =
      Eio_main.run @@ fun env ->
      let callback (_client : Client.t) req =
        match Request.path req with
        | [ ".well-known"; "oauth-authorization-server" ] ->
            Oauth2_server_routes.handle_metadata req 8080
        | [ "token" ] ->
            Oauth2_server_routes.handle_token ~env req
        | [ "authorize" ] ->
            Oauth2_server_routes.handle_authorize req
        | [ "register" ] ->
            Oauth2_server_routes.handle_register req
        | _ -> Response.create `Not_found
      in
      let server_cfg = Server.Config.create ~port:8080 () in
      Server.start ~config:server_cfg env callback
  ]}

  Interact with the token endpoint:

  {[
    # curl -s \
        -d 'grant_type=client_credentials' \
        -d 'client_id=my-client' \
        -d 'client_secret=my-secret' \
        http://localhost:8080/token | jq
    {
      "access_token": "xg0C9t…",
      "token_type": "Bearer",
      "expires_in": 3600,
      "obtained_at": 1.694e9
    }
  ]}
*)

(** {1 Metadata endpoint} *)

(** [handle_metadata req port] returns an *Authorization-Server Metadata*
    document (RFC&nbsp;8414) as a JSON response.

    The function derives the absolute base URL from [req] (scheme and
    [`Host`] header) and forces the supplied [port] so that test
    environments behind a reverse proxy still receive self-contained
    links.

    The response serialises an {!Oauth2_server_types.Metadata.t} record and
    carries the media-type ["application/json"].  Always replies with
    status&nbsp;200.

    Example – fetch metadata:
    {[
      (* inside an Eio fiber *)
      let open Piaf.Client.Oneshot in
      match
        get ~sw env (Uri.of_string "http://localhost:8080/.well-known/oauth-authorization-server")
      with
      | Ok resp ->
          Piaf.Body.to_string resp.body |> Result.ok_or_failwith |> print_endline
      | Error _ -> failwith "request failed"
    ]} *)
val handle_metadata : Request.t -> int -> Response.t

(** {1 Token endpoint} *)

(** [handle_token ~env req] implements the `/token` endpoint (RFC&nbsp;6749
    §4.4) for the *Client Credentials* grant.

    Expected [`application/x-www-form-urlencoded`] body parameters:

    {ul
      {li [grant_type] = ["client_credentials"]}
      {li [client_id]            – identifier issued during registration}
      {li [client_secret]        – secret bound to [client_id]
           (confidential clients only)} }

    Successful flow:

    {ol
      {li Validate credentials via
          {!Oauth2_server_client_storage.validate_secret}.}
      {li Generate a fresh {!Oauth2_server_types.Token.t} whose
          [access_token] is a 32-byte URL-safe Base64 string.}
      {li Persist the token with {!Oauth2_server_storage.insert}.}
      {li Respond with status&nbsp;200 and the token encoded as JSON.}}

    Failure modes:

    {ul
      {li malformed or missing parameters → 400}
      {li unsupported [grant_type]        → 400}
      {li invalid credentials             → 401}}

    The system clock from [env] stamps the token’s [obtained_at] field.
    No exceptions escape the function.

    Example – obtain an access token from a confidential client:
    {[
      let body =
        [ "grant_type", "client_credentials" ;
          "client_id", "my-client" ;
          "client_secret", "my-secret" ]
        |> Uri.encoded_of_query
        |> Piaf.Body.of_string
      in
      match
        Piaf.Client.Oneshot.post
          ~sw env
          ~headers:[ "content-type", "application/x-www-form-urlencoded" ]
          ~body
          (Uri.of_string "http://localhost:8080/token")
      with
      | Ok resp ->
          Result.ok_or_failwith (Piaf.Body.to_string resp.body)
      | Error _ -> failwith "request failed"
    ]} *)
val handle_token : env:Eio_unix.Stdenv.base -> Request.t -> Response.t

(** {1 Authorise endpoint} *)

(** [handle_authorize _] is a placeholder for the *Authorisation Code* / PKCE
    flow.  It always returns status 501 (`Not_implemented`) and the JSON
    body {js|{"error":"not_implemented"}|js}.

    Example – request authorisation code (currently placeholder):
    {[
      match Piaf.Client.Oneshot.get ~sw env (Uri.of_string "http://localhost:8080/authorize") with
      | Ok resp -> assert (resp.status = `Not_implemented)
      | Error _ -> assert false
    ]} *)
val handle_authorize : Request.t -> Response.t

(** {1 Dynamic Client Registration} *)

(** [handle_register req] creates a new OAuth&nbsp;2.0 client as described in
    RFC&nbsp;7591 and returns the credentials as JSON.

    The request body must be `application/json`.  Only a subset of the
    standard is implemented – all that the wider application stack needs:

    • [client_name] – optional string label shown in consent pages.  
    • [redirect_uris] – optional array of permitted redirect URIs.  
    • [token_endpoint_auth_method] – ignored; every client is currently
      registered as **confidential** with a random secret to avoid the PKCE
      public-client flow during automated test runs.

    The returned value is an {!Oauth2_server_types.Client.t} record.  The
    handler replies with status 201 on success and stores the new entry in
    {!Oauth2_server_client_storage} so that subsequent `/token` requests
    can authenticate.

    Example – dynamic client registration:
    {[
      let json_body =
        Jsonaf.to_string
          (`Object
             [ "client_name", `String "My test app" ;
               "redirect_uris", `Array [ `String "https://example.org/cb" ] ])
        |> Piaf.Body.of_string
      in
      match
        Piaf.Client.Oneshot.post
          ~sw env
          ~headers:[ "content-type", "application/json" ]
          ~body:json_body
          (Uri.of_string "http://localhost:8080/register")
      with
      | Ok resp ->
          let body = Result.ok_or_failwith (Piaf.Body.to_string resp.body) in
          print_endline body
      | Error _ -> failwith "registration failed"
    ]}
*)
val handle_register : Request.t -> Response.t
