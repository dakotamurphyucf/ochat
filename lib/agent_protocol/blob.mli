(** Server-managed blob metadata and typed message input references. *)

type kind =
  | File
  | Image
  | Audio
  | Binary
[@@deriving compare, equal, sexp]

module Metadata : sig
  type t =
    { id : Id.Blob.t
    ; kind : kind
    ; media_type : string
    ; byte_length : int64
    ; digest : string
    ; display_name : string option
    }
  [@@deriving sexp]

  val create
    :  id:Id.Blob.t
    -> kind:kind
    -> media_type:string
    -> byte_length:int64
    -> digest:string
    -> ?display_name:string
    -> unit
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Input : sig
  type source =
    | Stored of Id.Blob.t
    | Inline_base64 of string
  [@@deriving sexp]

  type t =
    { kind : kind
    ; media_type : string
    ; byte_length : int64
    ; digest : string
    ; display_name : string option
    ; source : source
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Bounded transport-neutral reads for server-owned session blobs. This is
    used by duplex transports that cannot use the HTTP streaming route. *)
module Read_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; blob_id : Id.Blob.t
    ; offset : int64
    ; max_bytes : int
    }
  [@@deriving sexp]

  val max_chunk_bytes : int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** One base64-encoded blob chunk. [next_offset] is the exact cursor for the
    next request and [eof] is true only after the advertised blob length has
    been reached. *)
module Chunk : sig
  type t =
    { blob : Metadata.t
    ; offset : int64
    ; next_offset : int64
    ; data_base64 : string
    ; eof : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
