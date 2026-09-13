open Core

(** Live host-owned authored wrapper bindings. This is a resource index, not an
    agent-name/session map or durable creation identity. Entries are selected only
    through a caller's exact current capability registry. *)
type t

val create : unit -> t

(** Bind all prepared specialists to their actual public native registrations.
    All validation completes before publication. [sw] must be the same resource
    scope that owns the prepared private tools and public wrappers; its release
    removes the entries. The factory must retain that scope through inherited
    borrowers. Duplicate names/IDs and same-name replacement reject.

    This function neither builds resources nor admits sessions or approvals. *)
val install
  :  t
  -> sw:Eio.Switch.t
  -> public:Chat_response.Tool_capability.t
  -> Agent_session.Runtime_builder.authored_resources list
  -> (unit, Agent_protocol.Error.t) result

(** Find the captured private closure through a live wrapper present in [public].
    The caller must already hold the owning runtime/resource lease. Names alone
    cannot discover another owner's wrapper or private tools. *)
val find
  :  t
  -> public:Chat_response.Tool_capability.t
  -> name:string
  -> (Agent_session.Runtime_builder.authored_resources, Agent_protocol.Error.t) result

(** Resolve a private ledger origin against the current source-bound wrapper.
    Suitable for Delegation_authority's authored_capabilities callback. The common
    authority service separately validates ledger, ancestry and current policy. *)
val resolve
  :  t
  -> Agent_store.Delegation_store.record
  -> public:Chat_response.Tool_capability.t
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

(** Prepare captured nested declarations from leaves to root. Validate all source
    edges, cycles and [max_depth] before calling [build], which may perform native
    setup effects. Siblings retain separate private bindings even when they name
    the same file. Each child's wrappers are installed against its exact private
    capability registry in [sw]. No session or moderator is started.

    Returns the direct specialists and their native registrations; the caller must
    build the public runtime with those registrations and [install] the direct
    specialists in the same scope. Release [sw] on any preparation failure. *)
val prepare
  :  t
  -> sw:Eio.Switch.t
  -> max_depth:int
  -> revision:Agent_session.Prompt_revision.t
  -> build:
       (parent_revision:Agent_session.Prompt_revision.t
        -> tool_name:string
        -> native_registrations:Chat_response.Agent_runtime.native_registration list
        -> ( Agent_session.Runtime_builder.authored_resources
             , Agent_protocol.Error.t )
             result)
  -> services:
       (Agent_session.Runtime_builder.authored_resources
        -> Agent_session.Native_tool_invocation.borrowed
        -> ( Agent_session.Authored_agent_call.host
             * Agent_session.Managed_session_service.t
             , Agent_protocol.Invocation.tool_error )
             result)
  -> ( Agent_session.Runtime_builder.authored_resources list
       * Chat_response.Agent_runtime.native_registration list
       , Agent_protocol.Error.t )
       result
