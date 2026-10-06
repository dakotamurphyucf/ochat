(** Pure bridge from independently versioned Frame v1 to universal documents.
    Concrete snapshot/transaction kinds and chain/session/counter validators
    belong to their existing storage owners. No files are opened or rewritten. *)
open! Core

module Error : sig
  type t =
    | Frame of Frame.error
    | Incomplete_frame
    | Trailing_bytes
    | Digest_mismatch of
        { expected : string
        ; actual : string
        }
    | Invalid_digest of string
    | Document of Document_schema.Error.t
  [@@deriving sexp]
end

type t

(** SHA-256 of exact persisted payload bytes, independent of frame checksum and
    JSON normalization. Callers use this for original transaction chain anchors. *)
val digest : string -> string

(** Frame validation precedes document inspection. Expected digest, when given,
    is a lowercase 64-character SHA-256 anchor and is checked against the exact
    stored bytes before parsing or conversion. Offset permits journal records. *)
val decode_frame
  :  limits:Document_schema.Limits.t
  -> contents:string
  -> offset:int
  -> expected_digest:string option
  -> (t * int, Error.t) Result.t

(** Snapshot helper additionally rejects trailing bytes. *)
val decode_file
  :  limits:Document_schema.Limits.t
  -> expected_digest:string option
  -> string
  -> (t, Error.t) Result.t

val stored_bytes : t -> string
val stored_digest : t -> string
val document : t -> Document_schema.Document.t

(** Conversion returns a new logical document alongside an unchanged verified
    record. It cannot replace the stored-byte digest. Domain validation follows
    through [Document_schema.Domain_codec.decode]. *)
val upgrade
  :  t
  -> conversion:Document_schema.Conversion.t
  -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

(** New baseline writes use named-field JSON as the complete frame payload;
    there is no binary wrapper. Flags and frame version remain independent. *)
val encode
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> flags:int
  -> (string, Error.t) Result.t
