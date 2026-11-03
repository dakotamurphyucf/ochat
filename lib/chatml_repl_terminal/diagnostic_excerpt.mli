open! Core

(** A presentation-only excerpt of an authoritative diagnostic location. The
    source ID is always the server-provided ID. Local text is shown only when
    its cached digest matches that location, the byte range is valid, and the
    source is at most 64 KiB. Unavailable/external text retains location metadata. *)
type t =
  | Location_only of string
  | Excerpt of
      { location : string
      ; prefix : string
      ; before : string
      ; marked : string
      ; after : string
      ; marker : string
      }

(** Escape controls and expand tabs before measuring terminal columns. The
    displayed range expands to grapheme boundaries; the location's byte
    coordinates remain unchanged. [max_line_bytes] is clamped to 32..8192. *)
val create
  :  Agent_protocol.Developer_repl_wire.Submission.Diagnostic.t
  -> submitted_source:(string * Chatml_source.Digest.t) option
  -> max_line_bytes:int
  -> t option
