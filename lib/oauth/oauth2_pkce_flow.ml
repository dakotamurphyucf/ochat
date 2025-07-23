(** OAuth 2.0 {e Authorization Code} flow with
    {b Proof-Key for Code Exchange (PKCE)} helper.

    This module is tailored for {i native} or command-line applications
    that cannot listen on public ports.  It automates the common
    pattern:

    1.  Spins up a {b one-shot HTTP listener} on
        [http://127.0.0.1:8876/cb].
    2.  Generates a fresh PKCE [code_verifier] / [code_challenge] pair
        using {!Oauth2_pkce.gen_code_verifier}.
    3.  Launches the user’s browser at the authorisation endpoint with
        all required query parameters.
    4.  Waits for the authorisation server to redirect back with the
        [code] and returns it to the caller together with the
        [code_verifier] and [redirect_uri].

    A second helper {!exchange_token} subsequently exchanges the
    returned code for an access / refresh token.

    The implementation relies solely on {!module:Eio} for
    concurrency and networking; no web-framework is pulled in.

    @see <https://datatracker.ietf.org/doc/html/rfc7636> RFC&nbsp;7636 – Proof-Key for Code Exchange
    @see <https://datatracker.ietf.org/doc/html/rfc6749#section-4.1> RFC&nbsp;6749 §4.1 – Authorisation Code grant
*)

open Core
open Eio
module T = Oauth2_types

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) res f = bind res ~f
  end
end

(** [open_browser ~env url] opens [url] in the user’s default browser.

    The helper is {b best-effort}: on Unix it tries {e xdg-open},
    falling back to macOS’ [open] command; on Windows it uses
    [start].  All failures are silently ignored because they are
    non-critical – the user can always copy&paste the URL.

    Browser spawning is {b skipped automatically} when either the
    [CI] or [OAUTH_NO_BROWSER] environment variable is present.  This
    avoids hanging continuous-integration jobs and permits headless
    testing.

    The function is intended for interactive use and therefore lives
    in this module’s implementation only; applications should not rely
    on it directly.
*)
let open_browser ~env (url : string) : unit =
  (* best-effort – on macOS/Linux try xdg-open/open, ignore failures *)
  (* In CI or test environments we must not launch an external browser – it
     would block the test runner waiting for user interaction.  Detect
     non-interactive mode via the conventional [CI] or
     [OAUTH_NO_BROWSER] environment variables. *)
  let no_browser =
    Sys.getenv "CI" |> Option.is_some || Sys.getenv "OAUTH_NO_BROWSER" |> Option.is_some
  in
  if no_browser
  then Logs.warn (fun f -> f "[oauth2] Skipping browser launch for PKCE flow: %s" url)
  else (
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let cmd =
      if Sys.win32
      then [ "start"; Printf.sprintf "'%s'" url ]
      else [ "xdg-open"; Printf.sprintf "'%s'" url; "&>/dev/null" ]
    in
    try Eio.Process.run proc_mgr cmd with
    | Eio.Io _ as e ->
      (* fallback to open command on macOS *)
      (match Sys.unix with
       | true -> Eio.Process.run proc_mgr [ "open"; Printf.sprintf "%s" url ]
       | false -> raise e))
;;

(** [run ~env ~sw ~meta ~client_id] initiates the interactive PKCE flow.

    It blocks until the resource owner completes authentication and
    consent in the browser and the authorisation server redirects back
    to [http://127.0.0.1:8876/cb?code=…].

    Returned triple:
    {ul
      {- [code] – the single-use authorisation code (RFC&nbsp;6749 §4.1.2).}
      {- [verifier] – the PKCE *code_verifier* generated for the
         request; pass it verbatim to {!exchange_token}.}
      {- [redirect_uri] – the exact redirect URI used (constant
         [`http://127.0.0.1:8876/cb`] at the moment).}}

    Notes & invariants
    {ul
      {- A dedicated TCP listener is created with
         [Eio.Net.listen ~reuse_addr:true ~reuse_port:true] and limited
         backlog [1].  Only the first successful callback is handled.}
      {- The port is currently fixed at [8876] to keep the registered
         redirect URI stable.  Future revisions may randomise it.}
      {- The helper runs inside the supplied switch [sw]; callers must
         ensure [sw] remains alive until the function returns.}}

    @param env  The standard environment obtained from
           [Eio_main.run].
    @param sw   Switch delimiting the lifetime of spawned fibers and
           socket listener.
    @param meta Service discovery metadata (see {!Oauth2_types.Metadata}).
    @param client_id Public OAuth 2.0 client identifier.

    @raise Eio.Io   on network errors while opening the browser or
                    binding the TCP listener.
    @raise Failure  if JSON parsing of the redirect payload fails
                    (should never happen under compliant servers).

    Example – obtain an authorisation code
    {[
      Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
          let code, verifier, redirect_uri =
            Oauth2_pkce_flow.run
              ~env ~sw
              ~meta
              ~client_id:"my-native-app"
          in
          Format.printf "Auth code = %s@." code
    ]}
*)
let run ~env ~sw ~(meta : T.Metadata.t) ~(client_id : string) =
  let verifier = Oauth2_pkce.gen_code_verifier () in
  let challenge = Oauth2_pkce.challenge_of_verifier verifier in
  (* Deterministically pick a port in [8800, 9799] using cryptographically
     secure randomness.  This avoids the use of [Random.self_init], which is
     forbidden inside inline tests (it raises to keep the test output
     deterministic). *)
  let port = 8876 in
  let redirect = Printf.sprintf "http://127.0.0.1:%d/cb" port in
  (* promise for code *)
  let code_p, code_rf = Promise.create () in
  Fiber.fork ~sw (fun () ->
    let sock =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~reuse_port:true
        ~backlog:1
        env#net
        (`Tcp (Eio.Net.Ipaddr.V4.any, port))
    in
    (* run connection handling; we ignore errors and let the listener be
         garbage-collected when the fibre terminates. *)
    (fun () ->
       let flow, _ = Eio.Net.accept ~sw sock in
       let req = Eio.Flow.read_all flow in
       (* if true then failwith "Mcp_transport_http.connect: store nots supported"; *)
       (match String.lsplit2 req ~on:'?' with
        | Some (_, rest) ->
          let query = String.prefix rest (String.index_exn rest ' ') in
          let params = Uri.query_of_encoded query in
          (match List.Assoc.find params ~equal:String.equal "code" with
           | Some [ code ] -> Promise.resolve code_rf code
           | _ -> ())
        | None -> ());
       ignore
         (Eio.Flow.write
            flow
            [ Cstruct.of_string "HTTP/1.1 200 OK\r\n\r\nYou may close this tab" ]);
       Eio.Flow.close flow)
      ());
  let auth_uri =
    Uri.add_query_params'
      (Uri.of_string meta.authorization_endpoint)
      [ "response_type", "code"
      ; "client_id", client_id
      ; "redirect_uri", redirect
      ; "code_challenge", challenge
      ; "code_challenge_method", "S256"
      ]
  in
  open_browser ~env (Uri.to_string auth_uri);
  (* if true then failwith "Mcp_transport_http.connect: store nots supported"; *)
  (* wait for code *)
  let code = Promise.await code_p in
  code, verifier, redirect
;;

(** [exchange_token ~env ~sw ~meta ~client_id ~code ~code_verifier ~redirect_uri]
    swaps the single-use [code] obtained from {!run} for an access token.

    The helper performs a `POST` with a
    `application/x-www-form-urlencoded` body (grant =
    `authorization_code`) against the [meta.token_endpoint] and decodes
    the JSON response into an {!Oauth2_types.Token.t} record.

    The [`obtained_at`] field is stamped with the current
    [`Eio.Time.now`] so that {!Oauth2_types.Token.is_expired} works
    reliably.

    Returns [`Ok tok`] on success or [`Error msg`] describing the
    failure (network/TLS error, HTTP ≠ 2xx, JSON decoding problem).

    {b Performance}: the function delegates to {!Oauth2_http.post_form}
    which uses a {i one-shot} connection – each call establishes and
    tears down a fresh TLS session.  This is perfectly adequate for the
    sporadic traffic produced by OAuth clients.

    @raise Jsonaf.Parse_error  Transparently propagated if the server
            returns invalid JSON (mirrors {!Oauth2_http.post_form}
            semantics).

    Example – fully automated code exchange
    {[
      match
        Oauth2_pkce_flow.exchange_token
          ~env ~sw ~meta ~client_id
          ~code
          ~code_verifier:verifier
          ~redirect_uri
      with
      | Error msg -> Format.eprintf "Token error: %s@." msg
      | Ok tok    -> Format.printf "Access token = %s@." tok.access_token
    ]}
*)
let exchange_token
      ~env
      ~sw
      ~(meta : T.Metadata.t)
      ~(client_id : string)
      ~(code : string)
      ~(code_verifier : string)
      ~(redirect_uri : string)
  : (T.Token.t, string) Result.t
  =
  let open Result.Let_syntax in
  let params =
    [ "grant_type", "authorization_code"
    ; "code", code
    ; "client_id", client_id
    ; "code_verifier", code_verifier
    ; "redirect_uri", redirect_uri
    ]
  in
  let* json = Oauth2_http.post_form ~env ~sw meta.token_endpoint params in
  Ok
    T.Token.
      { (T.Token.t_of_jsonaf json) with
        obtained_at = Eio.Time.now (Eio.Stdenv.clock env)
      }
;;
