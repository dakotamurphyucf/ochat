open Core
module Guidance = Agent_protocol.Authoring_guidance
module History = Agent_protocol.History

(** Compact remembered references, not evidence that prose is still in context.
    These are serializable for the host's retained rediscovery index. *)
type receipt =
  { entry_id : History.Id.t
  ; guidance : Guidance.t
  }
[@@deriving equal, sexp]

type presence =
  | Present
  | Absent
  | Modified
  | Redacted
  | Stale_context
  | Stale_policy
[@@deriving compare, equal, sexp]

type observation =
  { receipt : receipt
  ; presence : presence
  }
[@@deriving sexp]

type report =
  { observations : observation list
  ; refresh_primer : bool
  ; missing_preload : string list
  }
[@@deriving sexp]

(** Merge host-labelled canonical/archive entries into an index. A history ID
    cannot acquire a different guidance identity. Does not infer provenance from
    text or return content. The host owns persistence, scope and retention. *)
val remember
  :  previous:receipt list
  -> history:History.entry list
  -> (receipt list, Agent_protocol.Error.t) result

(** Inspect the actual effective input, after moderator edits and compaction.
    Matching topic names or stable entry IDs alone do not prove presence: the
    role/payload digest and host provenance must match. Old context/policy receipts
    stay inspectable but cannot satisfy the current automatic plan. Authored prose
    and incomplete/rediscovery references never satisfy installed guidance.

    This is a pure hook, not an injector or scheduler. Manual plans request no
    automatic refresh. A01 owns corpus lookup, bounded retained pointers and the
    safe-model-input-boundary application of the resulting plan. *)
val inspect
  :  policy:Authoring_policy.t
  -> context_identity:string
  -> known:receipt list
  -> effective:History.entry list
  -> (report, Agent_protocol.Error.t) result

(** Inspection against exact topic/source digests from a trusted materialization
    plan. Authored preloads can satisfy their own expected conventions, never an
    installed topic or primer. Expected topics must come from the owning host's
    admitted plan, not from model-supplied labels or observed history. *)
val inspect_with_topics
  :  expected_topics:Agent_protocol.Authoring_guidance.topic list
  -> policy:Authoring_policy.t
  -> context_identity:string
  -> known:receipt list
  -> effective:History.entry list
  -> (report, Agent_protocol.Error.t) result
