open Core

(** Pure selected-member intent below both Builder and Source_owner. Retains
    the original validated Definition/Receipt, admitted portable member and
    bounded parser-owned identity/range metadata. Never retains Parsed,
    Agent_reference, Revision, materialized directory, loader or native owner.
    No serializer and no permission is granted by this value. *)
type t

(** Require the actual reference object in parent's authentic Parsed inventory,
    an original artifact source receipt matching parent's Definition, and the
    exact positive parser-owned queued_key. Validate queued_key and both Root
    and declaring-input paths with Portable_path BEFORE identity hashing.
    No native path reversal, display-context lookup or basename inference.

    [max_work_steps] is the caller-supplied REMAINING logical allowance for
    this complete reference scan and fixed-bounded metadata admission/hash
    operations; zero rejects before scanning. It is not a fresh policy default.
    Successful [work_steps] must be charged ONCE to that enclosing attempt; any
    failure aborts that attempt, with no hidden retry/refund. No budget callback
    or owner is retained. The caller reserves allowance if operations overlap.
    Poll before every reference/metadata operation. Receipt already admits its
    4096-byte root spelling; portable paths admit 1024 bytes. Reject missing
    legacy ownership, external/absolute reference and foreign reference; no
    captured fallback.
    Already-admitted parent bounds limit retained field sizes, not this scan
    allowance: every current reference still consumes work. Unexpected/checkpoint
    exceptions preserve object and backtrace. Each reference costs one work unit;
    fixed bounded admission/projection operations cost eight more on success.
    This is a logical work model, not a CPU or wall-clock bound. *)
val of_reference
  :  parent:Prompt_revision.t
  -> Prompt.Chat_markdown.Agent_reference.t
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

val member : t -> Source.Portable_path.t
val receipt : t -> Agent_store.Prompt_source_receipt.t
val definition : t -> Prompt_definition.t
val work_steps : t -> int
