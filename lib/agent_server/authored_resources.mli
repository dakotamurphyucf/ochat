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

(** Find the captured private closure through the SAME source-bound live wrapper
    present in [public], rechecking its installed implementation and exact current
    private registry fingerprint. Private capabilities must already be realized;
    lookup never realizes resources or runs setup effects. Actual installation
    and preparation retain their existing resource realization ownership.

    The caller must already hold the owning runtime/resource lease. Names alone
    cannot discover another owner's wrapper or private tools. This is not caller,
    native effect or recurrence admission. *)
val find
  :  t
  -> public:Chat_response.Tool_capability.t
  -> name:string
  -> (Agent_session.Runtime_builder.authored_resources, Agent_protocol.Error.t) result

(** Resolve a validated ordinary or captured authored selector against the SAME
    current source-bound wrapper and already-realized private registry. Lookup
    never performs resource setup. Suitable for Delegation_authority's
    authored_capabilities callback, under the actual owner's resource lease.
    The common authority service separately validates ledger, ancestry and current
    policy; immutable selector data grants no resource or execution permission. *)
val resolve
  :  t
  -> Agent_store.Delegation_store.record
  -> public:Chat_response.Tool_capability.t
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

(* [prepare_source] is the Factory's request-aware installed-artifact preparer.
   It receives the exact function just constructed, and must authenticate the
   current launch owner/registry before source acquisition. Native registrations
   retain that same physical function and never use a model-supplied name as
   authority. Resume returns no fresh code; new admission installs before stage. *)

module Source_plan : sig
  (** Immutable captured declaration forest, factored from the literal planning
      phase of [Authored_resources.prepare]. No index, resource owner, switch,
      public/private registry, native registration, Startup or callback retained.
      This is source provenance, not current wrapper visibility or authority. *)
  type t

  (** Capture the complete bounded forest with the SAME existing cycle/depth,
      parser-owned declaration and delegated program-role checks. [capture]
      borrows the caller's one lexical Startup/accounting context. No [build],
      service, source-registration or [install] operation runs. Original callback
      Error/exceptions/cancellation are preserved. Existing prepare uses this
      exact producer before its existing children-first construction. *)
  val capture
    :  max_depth:int
    -> revision:Agent_session.Prompt_revision.t
    -> capture:
         (parent_revision:Agent_session.Prompt_revision.t
          -> tool_name:string
          -> (Agent_session.Authored_source_preparation.t, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  (** Find an exact immutable declaration by BOTH recorded name and full source
      fingerprint, including its original defining publication and selected
      program/content custody. Equivalent repeated matches retain the first
      existing forest-preorder entry
      only after all matches agree on Source.Identity, full fingerprint, and
      selected revision ID/manifest. Any disagreement returns typed ambiguity;
      never choose a first differing member or infer physical-owner equality.
      Search only this actual captured forest; never
      the live Authored_resources index, ambient catalog, files, capabilities or
      a same-name fallback. Nested/inherited declarations retain their original
      defining revision. This supplies no current capability-visibility proof. *)
  val find
    :  t
    -> selector:Agent_store.Captured_source_kind.Authored_tool.t
    -> (Agent_session.Authored_source_preparation.t, Agent_protocol.Error.t) result

  (** Select an exact direct declaration of the immediate defining parent.
      Returns the original preparation and only its direct descendant plan.
      This corresponds to the original private registry's declaration scope and
      grants no wrapper or capability visibility. *)
  val find_direct
    :  t
    -> selector:Agent_store.Captured_source_kind.Authored_tool.t
    -> (Agent_session.Authored_source_preparation.t * t, Agent_protocol.Error.t) result
end

(** Prepare captured nested declarations from leaves to root. Validate all source
    edges, cycles and [max_depth] before calling [build], which may perform native
    setup effects. [capture] selects each final source/revision once; recursive
    planning and [build] use that exact owner. Both callbacks must borrow the
    caller's same startup context across the entire graph. Siblings retain separate private bindings even when they name
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
  -> capture:
       (parent_revision:Agent_session.Prompt_revision.t
        -> tool_name:string
        -> (Agent_session.Authored_source_preparation.t, Agent_protocol.Error.t) result)
  -> build:
       (source:Agent_session.Authored_source_preparation.t
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
  -> prepare_source:
       (Agent_session.Runtime_builder.authored_resources
        -> implementation:Ochat_function.t
        -> owner:Agent_protocol.Job.launch_owner
        -> request:Chat_response.Background_request.t
        -> checkpoint:(unit -> unit)
        -> ( Chat_response.Background_request.Agent_artifact.t option
             , Agent_protocol.Error.t )
             Result.t)
  -> ( Agent_session.Runtime_builder.authored_resources list
       * Chat_response.Agent_runtime.native_registration list
       , Agent_protocol.Error.t )
       result
