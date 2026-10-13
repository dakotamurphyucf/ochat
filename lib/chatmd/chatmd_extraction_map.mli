open! Core

(** Parser-owned provenance for the exact ChatML body emitted by one ChatMD
    script declaration. This module neither resolves paths nor reads files.
    External locations require a separate captured-source custody check. *)

module Limits : sig
  type t

  val create : max_map_bytes:int -> max_segments:int -> max_work_steps:int -> t Or_error.t
end

module Budget : sig
  (** One bounded ledger shared by every declaration in one owned parse. Once
      exhausted, later maps are approximate; the parser and script bodies remain
      unchanged. No callbacks are retained. *)
  type t

  val create : Limits.t -> t
end

type t
type map = t
type bound

module Input_span : sig
  type t

  val input : t -> Chatmd_parser_input.t
  val start_byte : t -> int
  val end_byte : t -> int
end

module Raw_projection : sig
  type t =
    | Exact_inline of Input_span.t
    | External_origin of
        { member : string
        ; body_digest : Chatml_source.Digest.t
        ; start_byte : int
        ; end_byte : int
        }
    | Approximate_declaration of Chatmd_parser_input.Range.t
end

module Producer : sig
  type t

  (** Construct only from the declaring element's genuine parser range. The
      producer emits the SAME bytes given to the script declaration parser.
      Append operations charge the shared budget before map allocation; after
      exhaustion they still emit body bytes but abandon exact map recording. *)
  val create
    :  Budget.t
    -> declaration:Chatmd_parser_input.Range.t
    -> checkpoint:(unit -> unit)
    -> t

  (** [output] is the actual serializer output of one Text child. Its ordered
      lexer fragments must concatenate exactly to [output] and must match the
      owning effective input byte-for-byte; mismatch degrades the map. *)
  val append_text
    :  t
    -> input:Chatmd_parser_input.t
    -> fragments:Chatmd_parser_builder.Text_fragment.t list
    -> output:string
    -> unit

  (** Synthesized/normalized child serialization remains unmappable even if its
      output happens to equal authored text. *)
  val append_unmapped : t -> string -> unit

  val finish_inline : t -> string * map

  (** An external [src] is recorded only from the original successful loader
      resolve/read; [member] is not a source ID or an execution authority.
      Exact external editor locations require the host's immutable capture
      receipt and its original source construction. *)
  val finish_external : t -> member:string -> source_text:string -> string * map
end

module Error : sig
  type t =
    | Stale_script_source
    | Invalid_compiler_location
    | Invalid_authored_point
  [@@deriving sexp_of]
end

module Authored_point : sig
  type unavailable =
    | Degraded_map
    | External_body
    | Effective_only
    | Foreign_input
    | Query_work_limit
    | Unmapped_or_ambiguous
  [@@deriving sexp_of]

  type t =
    | Exact_compiled_byte of int
    | Unavailable of unavailable
  [@@deriving sexp_of]
end

(** Reverse point lookup within this SAME bound parser-owned extraction map.
    Accept a validated authored byte cursor, including EOF, only when the
    supplied input is the exact parser input of a unique identity segment and
    its authored/effective bytes match exactly. Equal adjacent identity
    segments may meet at one compiled byte; a boundary touching an unmapped or
    foreign segment is unavailable. Duplicate output origins mapping the same
    authored point to different compiled bytes are ambiguous. Transformed,
    external, degraded and work-exhausted maps return typed Unavailable, never
    a guessed text search. Out-of-bounds returns Invalid_authored_point.
    Scans at most the map's finite query-work allowance and polls checkpoint;
    cancellation/unexpected exceptions keep their identity. This is coordinate
    evidence only: caller authenticates current parser input/source custody. *)
val authored_point
  :  bound
  -> input:Chatmd_parser_input.t
  -> authored_byte:int
  -> checkpoint:(unit -> unit)
  -> (Authored_point.t, Error.t) Result.t

(** Verify full compiled source ID/digest/bytes against the emitted script body.
    The caller must also authenticate this declaration and revision through the
    parsed source/capture or authoring bundle receipt. Original checkpoint and
    cancellation exceptions propagate unchanged. No source read or parser run. *)
val bind
  :  t
  -> compiled_source:Chatml_source.t
  -> checkpoint:(unit -> unit)
  -> (bound, Error.t) Result.t

(** Project a compiler location only within the bound source. Empty spans are
    exact only at an unambiguous identity boundary. Cross-segment locations are
    exact only when all bytes have one contiguous authored input origin; otherwise
    the declaration range is an approximate related site and the caller keeps
    the native ChatML location. For external [src], return the original
    unresolved-for-editor member/digest and byte span, never a caller-supplied
    source ID. Query work scans at most the producer's finite segment/work caps;
    exhaustion degrades to approximate, never a partial exact location. *)
val project_raw
  :  bound
  -> Chatml_source.Location.t
  -> checkpoint:(unit -> unit)
  -> (Raw_projection.t, Error.t) Result.t
