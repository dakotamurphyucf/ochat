(** Versioned, checksummed binary framing for journals and snapshots. *)

type t

type decoded =
  | Complete of
      { frame : t
      ; next_offset : int
      }
  | Incomplete_tail of { offset : int }

type error =
  | Invalid_offset of int
  | Invalid_magic
  | Unsupported_version of int
  | Invalid_flags of int
  | Payload_too_large of int64
  | Checksum_mismatch
[@@deriving sexp]

val current_version : int
val flags : t -> int
val payload : t -> string
val checksum_raw : t -> string
val checksum_hex : t -> string

(** [encode ~max_payload_length ~flags payload] returns one complete frame.
    Header integers use network byte order and the checksum is raw SHA-256. *)
val encode : max_payload_length:int -> flags:int -> string -> (string, error) result

(** [decode ~max_payload_length ~contents ~offset] decodes one frame. A short
    final header, payload, or checksum is reported as [Incomplete_tail]. *)
val decode
  :  max_payload_length:int
  -> contents:string
  -> offset:int
  -> (decoded, error) result
