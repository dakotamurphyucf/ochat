open Core

(** Proof Key for Code Exchange (PKCE) helper utilities.

    This module generates the *code-verifier* / *code-challenge* pair used
    by the {e OAuth 2.0 Authorization Code flow with PKCE}
    ({{:https://datatracker.ietf.org/doc/html/rfc7636} RFC&nbsp;7636}).  All
    outputs are {b base64url-encoded} (URL-safe alphabet) without
    trailing [=] padding, ready for use in HTTP query parameters or JSON
    request bodies.

    Typical usage – public client authenticating a user in a browser:
    {[
      let verifier  = Oauth2_pkce.gen_code_verifier () in
      let challenge = Oauth2_pkce.challenge_of_verifier verifier in

      (* 1. Redirect the user-agent to the authorisation endpoint *)
      let auth_uri =
        Uri.add_query_params'
          (Uri.of_string authorization_endpoint)
          [ "response_type", "code";
            "client_id",      client_id;
            "code_challenge", challenge;
            "code_challenge_method", "S256" ]
      in
      Browser.open_uri auth_uri;

      (* 2. After the user grants consent, the server redirects back with
         "code=<auth_code>".  Exchange it for a token: *)
      let params =
        [ "grant_type",    "authorization_code";
          "code",          auth_code;
          "client_id",     client_id;
          "code_verifier", verifier ]
      in
      Oauth2_http.post_form ~env ~sw token_endpoint params
      |> Result.map ~f:Oauth2_types.Token.t_of_jsonaf
    ]}
*)

(*--------------------------------------------------------------------
   PKCE helpers

   The verifier / challenge pair must be generated using a
   cryptographically-secure RNG.  We rely on [mirage-crypto-rng] which is
   already pulled in transitively via [mirage-crypto-rng-eio].
--------------------------------------------------------------------*)

let gen_code_verifier () : string =
  (* Ensure default RNG active. *)
  (try Mirage_crypto_rng_unix.use_default () with
   | _ -> ());
  (* 32 random bytes, URL-safe base64-encoded without padding *)
  let raw = Mirage_crypto_rng.generate 32 in
  (* RFC&nbsp;7636 §4.1 mandates "base64url" encoding (URI-safe alphabet
     "A–Z a–z 0–9 - _") without trailing padding characters.  Use the
     dedicated [Base64.uri_safe_alphabet] to avoid the ['+'] and ['/']
     symbols of the regular alphabet. *)
  let b64 = Base64.encode_exn ~pad:true ~alphabet:Base64.uri_safe_alphabet raw in
  String.filter b64 ~f:(fun c -> not (Char.equal c '='))
;;

(** [gen_code_verifier ()] creates a fresh, cryptographically-random
    *code-verifier*.

    Invariants:
    - The verifier follows RFC&nbsp;7636 §4.1 – it is base64url-encoded
      with no [=] padding, using the URI-safe alphabet {-_}.
    - Length is 43 or 44 characters, satisfying the mandated
      [43&nbsp;≤&nbsp;length&nbsp;≤&nbsp;128] interval.

    The function relies on {!Mirage_crypto_rng_unix.use_default} to ensure a
    properly seeded CSPRNG and generates {e 32 bytes} of entropy, which is
    more than sufficient for the maximum entropy allowed by the standard.

    @raise Mirage_crypto_rng.Unseeded_generator if the RNG has not been
           initialised and cannot be seeded automatically. *)

let challenge_of_verifier (verifier : string) : string =
  let sha = Digestif.SHA256.(digest_string verifier |> to_raw_string) in
  let b64 = Base64.encode_exn ~pad:true ~alphabet:Base64.uri_safe_alphabet sha in
  String.filter b64 ~f:(fun c -> not (Char.equal c '='))
;;

(** [challenge_of_verifier v] computes the S256 *code-challenge*
    corresponding to the *code-verifier* [v].

    The function applies SHA-256 over [v] and base64url-encodes the raw
    digest without padding.  It therefore implements the "S256" method
    defined in RFC&nbsp;7636 §4.2.

    @param verifier The output of {!gen_code_verifier} or an externally
           supplied string that already satisfies the PKCE verifier
           requirements.

    @return A 43-character base64url string suitable for the
            [code_challenge] request parameter.

    Example: creating a verifier / challenge pair
    {[
      let v = Oauth2_pkce.gen_code_verifier () in
      let c = Oauth2_pkce.challenge_of_verifier v in
      (* pass [c] as "code_challenge", later send [v] as "code_verifier" *)
    ]} *)

(*--------------------------------------------------------------------
   Documentation – function-level comments follow the ocaml-documentation
   guidelines defined in [doc/ocaml-documentation-guidelines].  Keeping the
   comments physically close to the implementation makes it harder for them
   to get out of sync.
 --------------------------------------------------------------------*)
