open Core

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
  let b64 = Base64.encode_exn raw in
  String.filter b64 ~f:(fun c -> not (Char.equal c '='))
;;

let challenge_of_verifier (verifier : string) : string =
  let sha = Digestif.SHA256.(digest_string verifier |> to_raw_string) in
  let b64 = Base64.encode_exn sha in
  String.filter b64 ~f:(fun c -> not (Char.equal c '='))
;;
