open! Core

(** Private-to-the-host observations from one actual message conversion.
    Ordinals address the emitted content parts in that message. They carry
    neither source authority nor a claim that an Actor accepted the message. *)
type t

type original =
  | Local_image of { part : int }
  | Local_document of
      { part : int
      ; media_type : string
      ; contents : string
      }

(** The host supplies finite limits and a nonyielding current-writer check.
    An unavailable or exhausted capture leaves ordinary rendering intact. *)
val create : max_parts:int -> max_bytes:int -> is_current:(unit -> bool) -> t

(** Converter-only observation at the emitted local image part. The actual
    canonical image URI owns its bytes.  The host does not keep another copy. *)
val record_image : t -> part:int -> unit

(** Converter-only observation from the same read as the rendered part. *)
val record_document : t -> part:int -> media_type:string -> contents:string -> unit

(** Consume the private batch once. Displaced authority releases all originals. *)
val take : t -> original list
