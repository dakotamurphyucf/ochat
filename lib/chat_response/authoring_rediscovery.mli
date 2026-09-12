open Core

(** A scoped, metadata-only view of documentation that the current authoring
    tools can use. Construction captures the host corpus and selected authored
    packages; historical receipts cannot restore private package visibility.
    Unavailable optional package dependencies in manual mode suppress pointers
    rather than making previously valid manual execution depend on guidance. *)
type t

val create
  :  context:Authoring_context.t
  -> host:Authoring_validation.host
  -> policy:Authoring_policy.t
  -> (t, string) result

type pointer = private
  { text : string
  ; topics : Agent_protocol.Authoring_guidance.topic list
  }

(** Render missing historical references only. [known] must come from the owning
    session's trusted index. Topics scheduled for automatic insertion are omitted.
    A matching current effective pointer avoids repeating its topic metadata;
    pointers never count as topic prose. Unavailable topics and redacted entries
    are omitted. Current hashes and installed/authored provenance are explicit,
    alongside the remembered version. No topic body, retrieval or model turn.

    Default pointer limits are 32 topics and 8192 UTF-8 content bytes (excluding
    the provider message envelope). Whole entries are retained newest-first and
    truncation is explicit. Already effective current pointers consume the same
    aggregate topic/byte allowance, so repeated turns cannot grow a truncated
    index without bound. Their serialized payload bytes are charged conservatively.
    Manual policy lists only selected helper bindings.
    Agents without authoring tools receive no pointer. *)
val render
  :  ?max_topics:int
  -> ?max_bytes:int
  -> t
  -> context_identity:string
  -> known:Authoring_presence.receipt list
  -> effective:Agent_protocol.History.entry list
  -> inserting:Agent_protocol.Authoring_guidance.topic list
  -> unit
  -> (pointer option, Agent_protocol.Error.t) result
