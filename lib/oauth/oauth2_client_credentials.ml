open Core

(** {{:https://datatracker.ietf.org/doc/html/rfc6749#section-4.4}OAuth&nbsp;2.0
    Client&nbsp;Credentials Grant} helpers.

    The *client&nbsp;credentials* grant type is suited for
    **machine-to-machine** access where the application ("client") acts on
    its own behalf and therefore does {i not} require any end-user
    interaction.  The flow is straightforward:

    1. The client authenticates with its [client_id] / [client_secret]
       against the authorisation server’s [token_uri].
    2. The server responds with an *access token* that can subsequently be
       attached to HTTP requests (typically in an [Authorization: Bearer …]
       header).

    This module offers a single convenience function – {!fetch_token} –
    which wraps the HTTP exchange, JSON decoding and timestamp handling in
    a single call designed for use from {!Eio} fibres.

    {1 Example}

    Retrieving a token and adding it to an HTTP request made with
    {{!Piaf}Piaf}:

    {[
      Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
          let token_res =
            Oauth2_client_credentials.fetch_token
              ~env ~sw
              ~token_uri:"https://auth.server/token"
              ~client_id:"my-cli"
              ~client_secret:(Sys.getenv_exn "CLIENT_SECRET")
              ()
          in
          match token_res with
          | Error e -> failwith e
          | Ok token ->
              let headers =
                [ "authorization", "Bearer " ^ token.access_token ]
              in
              let uri = Uri.of_string "https://api.example/resource" in
              match Piaf.Client.Oneshot.get ~headers env ~sw uri with
              | Ok resp -> Format.printf "Status: %d@." resp.status
              | Error err ->
                  Format.eprintf "HTTP error: %s@." (Piaf.Error.to_string err)
    ]}
*)

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) r f = bind r ~f
    let ( let+ ) r f = map r ~f
  end
end

module Tok = Oauth2_types.Token

(** [fetch_token ~env ~sw ~token_uri ~client_id ~client_secret ?scope ()]
    exchanges the *client credentials* for a fresh access token.

    The call performs an HTTP `POST` to [token_uri] with body
    `grant_type=client_credentials&client_id=…&client_secret=…` and an
    optional [scope] parameter when provided.  The response must follow the
    {e JSON shape} defined in RFC&nbsp;6749 §5.1; the helper decodes it into
    {!Oauth2_types.Token.t} via the automatically generated
    [t_of_jsonaf] converter.

    On success the returned record’s {!Oauth2_types.Token.obtained_at}
    field is overwritten with the current POSIX time obtained from
    {!Eio.Time.now} – callers can therefore feed the token to
    {!Oauth2_types.Token.is_expired} without having to perform the
    timestamp bookkeeping themselves.

    Parameters:
    - [env] – the standard {!Eio_unix.Stdenv.base} environment of the
      running fibre.
    - [sw]  – a switch used to scope the network sockets created by Piaf.
    - [token_uri] – full HTTPS URL of the authorisation server’s
      [token] endpoint.
    - [client_id] – public identifier issued during (dynamic) client
      registration.
    - [client_secret] – confidential secret tied to the client ID.
    - [?scope] – optional space-separated list restricting the access
      rights requested.

    {b Errors}.  Network issues, non-2xx HTTP responses, or invalid JSON
    bodies are folded into the [Error _] variant with a human-readable
    description.  No exceptions are leaked: the helper is fully
    exception-safe.

    Example – request a token scoped to ["profile openid"]:

    {[
      let token =
        match
          fetch_token
            ~env ~sw
            ~token_uri:"https://auth.server/token"
            ~client_id:"svc"
            ~client_secret:"s3cr3t"
            ~scope:"profile openid"
            ()
        with
        | Ok t -> t
        | Error e -> failwith e
      in
      assert (not (Oauth2_types.Token.is_expired token))
    ]}

    @return [`Ok token`] on success, [`Error msg`] otherwise. *)
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
  Ok Tok.{ (Tok.t_of_jsonaf json) with obtained_at = Eio.Time.now (Eio.Stdenv.clock env) }
;;
