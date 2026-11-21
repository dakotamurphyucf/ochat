open! Core

(** A bounded local image carried by an admitted neutral image part. Validates
    canonical base64 and the media signature. This does not parse the image,
    fetch a URL, or establish who may read the part. *)
type t

type error =
  | Unsupported
  | Invalid_encoding
  | Media_mismatch
  | Size_limit
[@@deriving equal, sexp_of]

(** Inspect only the signature of the supplied local bytes. *)
val media_type_of_contents : string -> string option

(** Accept PNG, JPEG, GIF and WebP data URLs. Check the decoded byte length
    before allocating. The caller may select up to 1 MiB per image. *)
val of_data_uri : string -> max_bytes:int -> (t, error) result

val media_type : t -> string
val contents : t -> string
