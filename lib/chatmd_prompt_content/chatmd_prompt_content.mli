open Core
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Occurrence = Prompt.Chat_markdown.Document_occurrence
module Projection = Prompt.Chat_markdown.Document_projection
module Storage = Agent_store.Prompt_program_bundle

(** Immutable rendered-document custody, separate from executable program sources.
    No loader, native directory, filesystem/network authority or installed catalog
    lookup is retained. *)
module Limits : sig
  type t

  (** Explicit aggregate ceilings across registering Roots and occurrences.
      Reads, rendered bytes and logical work count every occurrence, including
      those sharing one physical output. Depth is per conversion. The enclosing
      artifact additionally applies its joint authored/program/content payload
      and complete-manifest bounds; these are not additional storage allowances. *)
  val create
    :  max_occurrences:int
    -> max_parser_inputs:int
    -> max_input_bytes:int
    -> max_output_bytes:int
    -> max_work_steps:int
    -> max_nodes:int
    -> max_depth:int
    -> max_payloads:int
    -> max_payload_bytes:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t

  val occurrences : t -> int
  val parser_inputs : t -> int
  val input_bytes : t -> int
  val output_bytes : t -> int
  val work_steps : t -> int
  val nodes : t -> int
  val depth : t -> int
  val payloads : t -> int
  val payload_bytes : t -> int
  val document : t -> Document_schema.Limits.t
end

module Error : sig
  type t

  val range : t -> Input.Range.t option
  val sexp_of_t : t -> Sexp.t
end

module Accounting : sig
  type t

  val occurrences : t -> int
  val input_bytes : t -> int
  val output_bytes : t -> int
  val work_steps : t -> int
  val nodes : t -> int
end

module Rendered : sig
  type t

  module Source : sig
    (** Local bytes already observed under the acquisition owner's authority;
        supplying them to this pure constructor does not prove a historical read
        or grant filesystem authority. Builtin is exact immutable embedded asset. *)
    type t =
      | Local of
          { member : Source.Portable_path.t
          ; contents : string
          }
      | Builtin of Builtin_instructions.Asset.t
  end

  (** Require actual occurrence membership in Parsed. Check selected Local member
      against its actual declaring Input.path, or selected builtin ID against the
      supplied asset. Dynamic slots reject. Apply bounded document conversion with
      existing flag precedence (cleanup HTML before Markdown); no implicit gzip.
      Hash exact observed input and final output, with explicit limits admitted
      before input-sized work. All borrowed checkpoint exceptions propagate.
      These per-render ceilings must be remaining aggregate allowances in native
      capture. [capture] additionally reconciles their observed receipts. *)
  val create
    :  Parsed.t
    -> occurrence:Occurrence.t
    -> source:Source.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val contents : t -> string
  val accounting : t -> Accounting.t
end

type t

val to_storage : t -> Storage.t
val accounting : t -> Accounting.t

(** Capture every actual included slot and registering Root from these contexts,
    including Dynamic metadata and zero-document Roots. Only non-dynamic slots
    require Rendered values. Rejecting
    missing, foreign, duplicate and conflicting records. Preflight context/input/
    occurrence counts and cumulative rendered receipts before metadata or payload
    projections. Physical output SHA pool deduplicates only exact equal bytes.
    Only final output bodies are persisted; raw document bodies are unnecessary
    for restart. Full named-document extension ownership is retained. *)
val capture
  :  parsed_contexts:Parsed.t list
  -> rendered:Rendered.t list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Admit complete active and inactive records against exact supplied parser-input
    revisions. Reject malformed selectors/ranges, duplicate occurrence keys and
    missing/surplus payloads before materialization. Unknown metadata is retained.
    Structural admission proves inventories, not that inactive Roots were parsed
    or that asserted local provenance grants authority. No read or conversion. *)
val of_storage
  :  Storage.t
  -> inputs:Input.t list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

type content = t

module Materialized : sig
  type t

  val projection : t -> Projection.t

  (** Deliberate immutable content-owner affinity, in addition to the projection
      retaining its original Parsed. No equality or native authority claim. *)
  val belongs_to : t -> content:content -> bool

  (** Complete invocation receipt, including exact parser-input reconciliation,
      retained occurrence validation and final tree projection. Charge this once;
      Projection.work_steps describes only its final phase, already included.
      Occurrences count all selected actual slots, including Dynamic; output
      bytes count each selected captured literal, despite physical deduplication.
      Input bytes and conversion nodes are zero: replay reads and converts none. *)
  val accounting : t -> Accounting.t
end

(** Match the selected Parsed's complete actual occurrence set and flags. Captured
    slots become literal text before history admission; Dynamic remains unchanged.
    Projection retains original Parsed for executable declaration ownership. No
    original document or catalog lookup/fallback. Projection work consumes this
    invocation's explicit remaining allowance and appears in its result receipt. *)
val materialize
  :  t
  -> parsed:Parsed.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Materialized.t, Error.t) Result.t

(** Incoming custody must preserve every previous record and exact payload;
    compatible unknown fields from both documents survive. Child selection uses
    its own exact retained Root through [materialize], without dropping inactive
    parent records or rereading originals. No mutable editing API. *)
val adopt
  :  t
  -> previous:t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t
