open! Core

(** Body-free optional successor hint routes. These coordinates convey no source
    or recipient authority. Materialize and recheck them through the actual
    recovery readers. *)
type t = private
  | Local_note of Recovery_notes.Catalog.selection
  | Shared_note of Recovery_note_share.Reference.t
  | Canonical_text of Agent_protocol.Recovery_text_range.t

val local_note : Recovery_notes.Catalog.selection -> t
val shared_note : Recovery_note_share.Reference.t -> t

(** Only ordinary canonical citation ranges of at most 4 KiB enter this proposal.
    The actual source reader checks visibility, the original part, and UTF-8
    scalar boundaries at materialization. *)
val canonical_text
  :  Agent_protocol.Recovery_text_range.t
  -> (t, Agent_protocol.Error.t) result

val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

module Selection : sig
  type reference := t
  type t

  val empty : t
  val max_references : int

  (** Preserve author order. Reject repeated exact references and over-capacity
      proposals before walking or serializing the list. *)
  val create : reference list -> (t, Agent_protocol.Error.t) result

  val references : t -> reference list
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Witness : sig
  (** Process-local recipient observation. The trusted host reads it only from
      the actual owning worker capability and rechecks it at materialization and
      the first physical provider write. *)
  type t

  val create : references:Selection.t -> check_current:(unit -> bool) -> t
  val references : t -> Selection.t
  val check_current : t -> bool
end
