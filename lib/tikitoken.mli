open Core

(** In-memory byte-pair encoding with a fixed UTF-8 pre-tokenization regex.
    Loading a vocabulary does not switch regex or special-token policies.
    Filesystem I/O belongs to the caller. PCRE uses native code. *)
type vocab_index = (int64, (Bytes.t * int) list) Hashtbl.t

type codec =
  { encoder : (bytes, int) Hashtbl.t
  ; decoder : (int, bytes) Hashtbl.t
  ; vocab_idx : vocab_index
  }

(** [create_codec contents] parses Base64-token/rank rows and builds lookup
    tables. Malformed rows/Base64/ranks and duplicate ranks can raise.
    The vocabulary must contain single-byte tokens required by the input.
    Reuse a codec; do not mutate its tables while encoding. *)
val create_codec : string -> codec

(** [encode ~codec ~text] pre-tokenizes with the module's fixed PCRE regex,
    then performs ranked adjacent merges using a rolling-hash index and heap.
    Input must be accepted by the UTF-8 regex. Full match/token lists, node
    arrays and heap entries are allocated; no streaming or small constant
    allocation bound is promised. This is not provider billing accounting. *)
val encode : codec:codec -> text:string -> int list

(** [decode ~codec ~encoded] concatenates vocabulary bytes, silently ignoring
    unknown IDs. Returned bytes need not be valid UTF-8. *)
val decode : codec:codec -> encoded:int list -> bytes
