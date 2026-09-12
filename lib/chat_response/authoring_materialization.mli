open Core

(** Assemble installed guidance for an admitted policy. This module does not
    expose tools, allocate history IDs or persist/insert messages. The owner must
    commit returned entries at the effective model-input boundary before exposing
    authoring tools. Public auto/preload enablement remains a separate gate. *)
type t

type message = private
  { payload : Jsonaf.t
  ; guidance : Agent_protocol.Authoring_guidance.t
  }

(** Installed native help packages and topic/task compatibility, restricted to
    this host's enabled compiler targets. No package completeness claim, custom
    package discovery or new helper capability is implied. *)
val catalog
  :  Authoring_context.t
  -> host:Authoring_validation.host
  -> (Authoring_policy.catalog, string) result

(** Require the policy's exact effective capability selection and installed
    corpus. [scope] is the owning session/generation identity, never model input.
    Preload closures are assembled in full with shared prerequisites deduplicated;
    incompatible topics fail without falling back to another target. The full
    initial batch must fit [max_tokens], estimated as ceil(payload UTF-8 bytes/3).
    Manual policy and ordinary tools produce no automatic guidance. *)
val create
  :  ?max_tokens:int
  -> context:Authoring_context.t
  -> host:Authoring_validation.host
  -> policy:Authoring_policy.t
  -> capabilities:Tool_capability.t
  -> scope:string
  -> unit
  -> (t, string) result

val context_identity : t -> string
val initial : t -> message list
val estimated_tokens : message list -> int

(** Examine actual effective history after edits/compaction. Return missing
    guidance only, never count a modified/redacted/stale entry or a rediscovery
    pointer as complete context. No internal mutable cache: concurrent sessions
    cannot satisfy each other's presence checks. The caller owns bounded receipt
    retention and atomic history insertion. *)
val refresh
  :  t
  -> known:Authoring_presence.receipt list
  -> effective:Agent_protocol.History.entry list
  -> (message list, Agent_protocol.Error.t) result

(** Apply the caller's durably reserved ID to an inspectable user-role reference
    message. Guidance is not installed as a new system/developer instruction. *)
val entry : message -> id:Agent_protocol.History.Id.t -> Agent_protocol.History.entry
