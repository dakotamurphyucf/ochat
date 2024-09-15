(** [hash_string_md5 s] creates a unique hash encoding of the input string [s] using Core.Md5.
    @param s is the input string to be hashed.
    @return
      a string representing the unique MD5 hash encoding of the input string in hexadecimal format. *)
let hash_string_md5 s =
  let open Core.Md5 in
  digest_string s |> to_hex
;;

