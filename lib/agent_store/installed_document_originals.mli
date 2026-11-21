(** Private immutable inventory for raw installed ChatMD document inputs.
    An occurrence or digest is an identifier, and confers no custody. *)
module Occurrence : sig
  type t

  val of_digest : string -> (t, Store_error.t) result
  val digest : t -> string
end

module Entry : sig
  type t

  val create
    :  occurrence:Occurrence.t
    -> sha256:string
    -> byte_length:int
    -> media_type:string
    -> (t, Store_error.t) result

  val occurrence : t -> Occurrence.t
  val sha256 : t -> string
  val byte_length : t -> int
  val media_type : t -> string
end

module Manifest : sig
  type t

  val max_bytes : int
  val of_bytes : string -> expected_sha256:string -> (t, Store_error.t) result
  val bytes : t -> string
  val sha256 : t -> string
  val find : t -> occurrence:Occurrence.t -> Entry.t option
end

type t

(** Validate occurrence identities and exact bytes before encoding the manifest.
    Physical payloads may deduplicate by digest. Each occurrence remains distinct. *)
val create : (Entry.t * string) list -> (t, Store_error.t) result

val manifest : t -> Manifest.t
val payloads : t -> Prompt_program_bundle.Payload.t list
