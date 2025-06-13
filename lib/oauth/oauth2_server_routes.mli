open Piaf

val handle_metadata : Request.t -> int -> Response.t
val handle_token : env:Eio_unix.Stdenv.base -> Request.t -> Response.t
val handle_authorize : Request.t -> Response.t

(** [handle_register req] implements the OAuth 2.0 Dynamic Client Registration
    endpoint (RFC 7591).  It accepts a JSON body describing the desired
    client configuration and responds with a freshly generated client record
    encoded as JSON.  The implementation only supports a minimal subset of the
    specification that is required by the MCP happy-path: we recognise

    {ul
      {li [client_name] – optional string}
      {li [redirect_uris] – optional array of strings}
      {li [token_endpoint_auth_method] – when set to ["none"] we register a
           public client (no secret); any other value registers a confidential
           client.}}

    On success the endpoint returns status 201 and a JSON body matching the
    [Oauth2_server_types.Client.t] schema.  The newly created client is stored
    in [Oauth2_server_client_storage] so it can subsequently obtain access
    tokens via the `/token` endpoint. *)

val handle_register : Request.t -> Response.t
