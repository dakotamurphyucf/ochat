open Core

let gen_code_verifier () : string =
  (* 32 random bytes, URL-safe base64-encoded without padding *)
  let raw = Bytes.create 32 in
  Random.self_init ();
  for i = 0 to 31 do
    Bytes.set raw i (Char.of_int_exn (Random.int 256))
  done;
  let raw_str = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:raw in
  let b64 = Base64.encode_exn raw_str in
  String.filter b64 ~f:(fun c -> not (Char.equal c '='))
;;

let challenge_of_verifier (verifier : string) : string =
  let sha = Digestif.SHA256.(digest_string verifier |> to_raw_string) in
  let b64 = Base64.encode_exn sha in
  String.filter b64 ~f:(fun c -> not (Char.equal c '='))
;;
