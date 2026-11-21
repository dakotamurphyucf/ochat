(** Read bounded bytes from one exact canonical original. A continuation must
    pin the SHA-256 returned by the preceding authenticated read. *)
val method_name : string

val feature : string
val max_original_bytes : int
val max_chunk_bytes : int

(** Maximum serialized JSON-RPC success response, including request ID. *)
val max_envelope_bytes : int

type t = private
  { citation : Recovery_window.Citation.t
  ; part : int
  ; offset : int
  ; max_bytes : int
  ; expected_sha256 : string option
  }

(** [offset] is a zero-based byte offset. End of file is a valid offset.
    [max_bytes] defaults to 16 KiB and is at most 64 KiB. *)
val create
  :  Recovery_window.Citation.t
  -> part:int
  -> offset:int
  -> ?max_bytes:int
  -> ?expected_sha256:string
  -> unit
  -> (t, Protocol_error.t) result

val session : t -> Session_ref.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result

module Reply : sig
  type request := t

  type kind =
    | Image
    | Document
  [@@deriving equal]

  type t = private
    { citation : Recovery_window.Citation.t
    ; part : int
    ; kind : kind
    ; media_type : string
    ; byte_length : int
    ; sha256 : string
    ; offset : int
    ; bytes : string
    ; next_offset : int option
    }

  (** Project at most one chunk from an already verified complete original.
      [sha256] is the whole original's lowercase hexadecimal SHA-256. *)
  val of_verified_original
    :  request
    -> kind:kind
    -> media_type:string
    -> sha256:string
    -> string
    -> (t, Protocol_error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
  val validate : t -> request:request -> (unit, Protocol_error.t) result
end
