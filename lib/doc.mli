(** [hash_string_md5 s] creates a unique hash encoding of the input string [s] using Core.Md5.
    @param s is the input string to be hashed.
    @return a string representing the unique MD5 hash encoding of the input string in hexadecimal format. *)
val hash_string_md5 : string -> string
