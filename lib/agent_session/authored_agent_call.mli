open Core

(** Call-and-answer composition for a host-admitted authored agent tool. This
    module does not load files, grant tools, or admit authored definitions. *)
type host =
  { create :
      Native_tool_invocation.borrowed
      -> key:Agent_protocol.Idempotency_key.t
      -> (Agent_protocol.Id.Session.t, Agent_protocol.Invocation.tool_error) result
  ; validate :
      Native_tool_invocation.borrowed
      -> Agent_protocol.Id.Session.t
      -> (unit, Agent_protocol.Invocation.tool_error) result
  ; one_off :
      Native_tool_invocation.borrowed
      -> input:string
      -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result
  }

(** Trusted construction only: callbacks must capture the exact admitted authored
    declaration and private source/resource closure. [create] durably reserves the
    supplied key, comparing the pinned definition and actual caller on replay,
    and returns an Owned session ready to accept input. [validate] must check that
    the target belongs to this exact authored declaration/revision and caller,
    including current authority and revocation. Generic child ownership alone is
    insufficient. Both callbacks must recheck authority after yielding.

    [one_off] must use an admitted adapter with actual caller policy/approvals;
    it must not fall back to the unrestricted legacy runner. These callbacks are
    host services, never values supplied by model arguments. *)

(** Compose create/validate/send/wait/read outside actor locks. Uses one durable
    creation key and one send key per actual invocation, independent of input and
    declaration edits so a changed retry conflicts instead of duplicating work.
    Same-invocation retries reuse those keys; genuinely new invocations may create
    new instances. The host and shared services enforce durability/concurrency.

    [wait_timeout_ms] is a host choice, 0--30000, default 10000. Pending results are
    ordinary completed tool calls containing session ID, receipt, and a bounded
    first output page with its continuation cursor, not unowned Invocation.Pending
    work. Terminal failed/cancelled submissions are not called successful answers.
    The returned output page retains the normal disclosure and fragmentation
    contract; callers use agent_read for remaining pages. Never stops or resumes
    a child, retries effects on a new key, or swallows caller cancellation.

    This internal composition is not registered publicly until the authored
    admission adapter and source-bound restoration have been qualified. *)
val run
  :  ?wait_timeout_ms:int
  -> host:host
  -> sessions:Managed_session_service.t
  -> borrowed:Native_tool_invocation.borrowed
  -> policy:Prompt.Chat_markdown.agent_persistence
  -> Jsonaf.t
  -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result

(** Construct the source/resource-bound native wrapper for a qualified host.
    Its schema and appended description follow the authored persistence policy.
    Services are resolved from the actual actor-dispatched borrow on each call,
    after semantic argument validation. No ambient parent/driver fallback exists.
    [services] must admit this exact source/private closure and caller before
    returning callbacks, retaining their resources through the call. This function
    does not install the registration or enable the public feature. *)
val registration
  :  ?wait_timeout_ms:int
  -> source:Authored_agent_source.t
  -> capabilities:Chat_response.Tool_capability.t
  -> services:
       (Native_tool_invocation.borrowed
        -> (host * Managed_session_service.t, Agent_protocol.Invocation.tool_error) result)
  -> unit
  -> (Chat_response.Agent_runtime.native_registration, Agent_protocol.Error.t) result
