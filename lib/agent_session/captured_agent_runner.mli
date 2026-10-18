open Core

(** Explicit retained-code child runner, confined to its creating Eio domain.
    This does not select the default for newly authored file-backed children.
    No original source grant or initialized child state is retained. *)
type t

(** [policy_revision] is the host's versioned identity for the supplied callback
    implementation, including shell authorizer/approval behavior and host changes.
    Actual script/content numeric limits are incorporated automatically. A digest grants no permissions. The
    factory's implementation/mode tag and each defining artifact's complete
    manifest digest participate separately in the returned port identity.

    The supplied shell policy callbacks and response-directory capability are
    borrowed through each bound runtime's lifetime; these are execution policy
    and output coordinates, not original source acquisition authority. *)
val create
  :  script_preparation:Runtime_script_preparation.t
  -> tool_contract:Chatml_compilation.Tool_contract.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> policy_revision:Chatml_source.Digest.t
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> t

(** Host implementation tag plus actual script/content numeric policy. *)
val policy_identity : t -> Chatml_source.Digest.t

(** Bind exact owned parser/program/content custody to one runtime switch.
    Reject legacy or inconsistent owned revisions. Performs bounded metadata
    checks; no child compilation, initializer or original source acquisition.
    Binding is lazy: recursive source references do not initialize/bind children.

    Every invocation starts its own startup owner BEFORE bounded exact descriptor
    lookup, then prepares the captured child's authentic Headless role using the
    same startup and original compiler limits. The child prompt gets a new runner;
    caller-authored invocation content keeps this original caller's separate port.
    Content descriptors use the defining revision's exact document projection.
    No Startup is retained in a port or initialized state retained across calls.

    Runtime_activity owns cancellation and joined cleanup for both caller and
    runtime lifetimes. Closing [sw] revokes new calls and cancels/joins active ones.
    Child runtime ownership ends when its invocation returns. Concurrent calls
    share immutable custody only. Eio cancellation polls cover metadata lookup,
    capture/restoration and preflight; activation has its own execution controls.

    Missing/nonlocal/foreign references reject without Fetch, legacy content cache
    or parent program fallback. Expected preparation errors use the existing
    lower runner failure form; unexpected exceptions and cancellation preserve
    identity/backtrace. Lower runtime checks govern supported script contracts;
    this factory does not imply that Driver implements managed versioned dispatch.
    Do not transfer this factory or its callbacks across Eio domains. *)
val bind
  :  t
  -> revision:Prompt_revision.t
  -> sw:Eio.Switch.t
  -> (Chat_response.Tool.Agent_source_runner.t, Agent_protocol.Error.t) Result.t

module Source : sig
  (** Acquisition policy, distinct from runtime restoration/activation policy.
      The callback uses the SAME Startup and actual Scope with current history.
      policy_revision versions preprocessing/preparation callbacks and host
      implementation. Actual parser/compiler/work limits are incorporated
      automatically; the digest grants no authority and binds
      installed-factory native implementation identity separately from captured. *)
  type t

  val create
    :  policy_revision:Chatml_source.Digest.t
    -> compilation:Chatml_compilation.limits
    -> parser_limits:Prompt.Chat_markdown.Limits.t
    -> max_work_steps:int
    -> preprocess:(string -> string)
    -> prepare_programs:
         (Chat_response.Startup_compilation.t
          -> tool_contract:Chatml_compilation.Tool_contract.t
          -> invocation:Runtime_script_preparation.Invocation.t
          -> Prompt_source_owner.Scope.t
          -> Prompt_source_owner.Capture.t
          -> ( Chatml_prompt_programs.t
               , Prompt_revision_builder.Diagnostic.t list )
               Result.t)
    -> t Or_error.t

  (** Host implementation tag plus actual admitted numeric acquisition policy. *)
  val policy_identity : t -> Chatml_source.Digest.t
end

(** Installed headless runtime binding. Exact saved current-services artifact wins
    before File/Captured selection. New File uses the actual original source lease;
    new Captured uses retained code. Both install/verify before returning a queued
    reference, then execute under authentic Headless policy. The runtime switch
    joins preparation/execution before releasing Store/lease. No native resource
    or source capability is serialized. Child ports belong to the selected child;
    invocation content retains its caller's port. The explicit prune embedding
    must serialize selection/install/admission; Daemon prunes only pre-recovery.
    A retained native closure may use its authenticated current service's exact
    selected code-view declaration. This requires the actual invoking function;
    saved queued selection wins first. Selected child content and nested tools
    keep their defining selected revision and do not inherit an enclosing target.
    Persistent precreated specialists require their separate Authored/code-view
    owner and cannot use this ordinary headless implementation. *)
val bind_installed
  :  t
  -> source:Source.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> source_lease:Prompt_source_lease.t
  -> env:Eio_unix.Stdenv.base
  -> revision:Prompt_revision.t
  -> sw:Eio.Switch.t
  -> (Chat_response.Tool.Agent_source_runner.t, Agent_protocol.Error.t) Result.t

module Claimed_developer : sig
  module Selected : sig
    type t
    type selected = t

    module Target : sig
      type t

      (** The exact selected/restored child revision from this saved Agent.run
          callback. This is the TARGET artifact, not its defining parent.
          Captured_agent_runner alone issues it after genuine source selection;
          callers cannot pair a copied artifact or another source result with
          a saved selected callback. *)
      val revision : t -> Prompt_revision.t

      (** Comparison-only original caller revision from the selected invoke
          triple; dynamic reads still require the bound caller source port. *)
      val caller_revision : t -> Prompt_revision.t

      val saved : t -> selected

      (** Full source, Worker and original/current actor check before and after
          yielding target preparation. No file read or new source selection. *)
      val check : t -> (unit, Agent_protocol.Error.t) result

      (** Direct nonyielding callback check before a nested selected borrow
          shadows this target's original native fiber binding. *)
      val check_now : t -> (unit, Agent_protocol.Error.t) result

      (** Nonyielding same-source ancestor check during a physically entered
          child. The lower selected-borrow chain must contain the exact saved
          parent, whose Worker/source scope and implementation remain live.
          Root/terminal/current child selection checks are separately required. *)
      val check_descendant_now
        :  t
        -> child:Native_tool_invocation.selected_borrowed
        -> (unit, Agent_protocol.Error.t) result
    end

    (** Physically borrowed saved Agent.run v2 source inside its exact selected
        native callback. The entered Worker Claim, registered runtime source,
        selected implementation, borrowed invocation, saved artifact and
        original/current parent remain bound to this one lexical proof. It
        expires on callback return and cannot be recreated from payload data. *)
    val active : t -> Developer_job_scope.active

    val invocation : t -> Agent_protocol.Host_invocation.t

    (** Full yielding source, selected implementation and Worker/current-actor
        recheck after restoration or host work. No callback or file read is
        replayed. *)
    val check : t -> (unit, Agent_protocol.Error.t) result

    (** Local, nonyielding check before Store admission and provider dispatch.
        Requires the same domain and live inherited lexical saved-source and
        selected-native callback bindings, exact Worker Parent and native
        implementation. Joined child fibers may inherit those bindings; none
        can outlive the enclosing callbacks. Root separately checks its
        physical Native_terminal, current actor, provider and epoch. *)
    val check_now : t -> (unit, Agent_protocol.Error.t) result

    (** Comparison only: is this the same physical selected-native borrow that
        originally entered the saved Agent callback? The caller must first
        obtain [borrowed] from the current fiber and separately run [check_now]
        or the exact descendant check. A copied ID or equal capability does not
        classify the execution phase. *)
    val matches_borrowed : t -> Native_tool_invocation.selected_borrowed -> bool

    (** The saved selected callback remains the exact ancestor of a current
        nested native borrow. This does not bypass the direct [check_now] at
        saved-source entry; it is only for the physically descended child
        after that entry's fiber binding has been shadowed. *)
    val check_descendant_now
      :  t
      -> child:Native_tool_invocation.selected_borrowed
      -> (unit, Agent_protocol.Error.t) result
  end

  (** One saved ordinary Agent.run reference from the exact entered Worker Claim.
      [request] must decode canonically from that Claim's payload under [policy].
      [prepared] is the same request's genuinely re-admitted Tool execution,
      and [binding] must be its resolved pinned native binding. The source scope
      retains no capability registry, Store writer or child Session. It expires
      when [f] returns and never makes a raw artifact reference authoritative.

      [invocation] is the exact new Host invocation constructed for this
      Claim and Tool request; the scope rejects a different or nested callback
      even when it calls the same function under the same Worker. The later
      native callback must present the actual selected_borrowed for that
      invocation, same physical Worker Parent, selected binding and implementation.
      [agent_runtime] must contain this very implementation's genuine
      Native_output/Source(Some preparer) registration; admission charges its
      bounded traversal before binding the saved scope. A copied saved request
      or caller-supplied function with no registered source cannot mint
      [Selected.t]. The callback calls [check_current] after yielding restoration/selection
      and before child execution; original exceptions and cancellation pass
      through unchanged. A foreign or expired selected callback rejects rather
      than falling back to File or a conversation native service. *)
  val with_saved_artifact
    :  active:Developer_job_scope.active
    -> parent:Developer_job_scope.Parent.t
    -> request:Chat_response.Background_request.t
    -> policy:Chat_response.One_off_request.policy
    -> prepared:Chat_response.Background_request.execution
    -> binding:Chat_response.Tool_capability.binding
    -> agent_runtime:Chat_response.Agent_runtime.t
    -> max_registrations:int
    -> checkpoint:(unit -> unit)
    -> invocation:Agent_protocol.Host_invocation.t
    -> check_current:(unit -> (unit, Agent_protocol.Error.t) result)
    -> (unit -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** Only inside the above physically entered saved-source bracket and the
      existing selected native callback. Verify the borrowed selected scope,
      exact selected implementation and saved artifact with the current
      Worker/actor, then lend one non-escaping proof to Root inference. The
      ordinary [selected] lookup continues to serve Headless restoration and
      cannot by itself grant provider authority. *)
  val with_selected
    :  borrowed:Native_tool_invocation.selected_borrowed
    -> implementation:Ochat_function.t
    -> (Selected.t -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result
end

module Selected_child_target : sig
  (** Private common ancestry node used by checked Content and Model targets.
      Only the separately minted wrappers below expose typed receivers; no
      caller can construct or serialize a node from coordinates. *)
  module Model : sig
    type t
  end
end

module Selected_content_target : sig
  (** One authored Agent content occurrence selected inside a physically entered
      saved-Agent child. This is distinct from the saved Tool target: content
      keeps its own defining parser/reference and cannot consume the enclosing
      Tool's recorded artifact. The issuer is minted only from the actual
      selected native borrow, the original invoke triple and the immediate
      selected parent: either the saved Tool child or another authored Content
      child. It also retains that ancestry's original saved Tool target. Every
      level expires when its own callback or any ancestor child ends. *)
  type t

  (** Comparison coordinates for the immediate physical parent. Constructing a
      variant is not a source grant; [matches_parent] checks the retained
      issuer and [check] verifies the original saved ancestor and live chain. *)
  type parent =
    | Saved of Claimed_developer.Selected.Target.t
    | Content of t
    | Model of Selected_child_target.Model.t

  val revision : t -> Prompt_revision.t
  val caller_revision : t -> Prompt_revision.t

  (** Comparison only: the exact saved Tool target retained by this Content
      ancestry. Root must still check its registered native terminal, current
      physical borrow and the live immediate parent; this projection grants no
      source, model or provider effect. *)
  val saved_ancestor : t -> Claimed_developer.Selected.Target.t

  (** Physical comparison only. The host must additionally check its live Root,
      native child and current source before entering an effect. *)
  val matches_parent : t -> parent -> bool

  (** Yielding source/reference and selected-native recheck after restoration. *)
  val check : t -> (unit, Agent_protocol.Error.t) result

  (** Final nonyielding check of this callback and every logical parent. A
      synchronous Content child may use the SAME selected native borrow as its
      parent. Only an actually entered descendant native callback is checked
      by [Native_tool_invocation.check_selected_ancestor]. *)
  val check_now : t -> (unit, Agent_protocol.Error.t) result

  (** Nonyielding physical check of the current borrow against this target's
      own retained entering borrow. The direct/descendant branch is chosen by
      identity BEFORE either check; a denied direct check never falls through
      to descendant admission. Root separately proves its registered native
      terminal and original saved ancestry. *)
  val check_current_or_descendant_now : t -> (unit, Agent_protocol.Error.t) result

  (** Final nonyielding check when a genuine distinct native child invocation
      shadows the borrow that selected this Content child. Logical Content
      recursion alone does not create such a native invocation. *)
  val check_descendant_now
    :  t
    -> child:Native_tool_invocation.selected_borrowed
    -> (unit, Agent_protocol.Error.t) result
end

module Selected_model_target : sig
  (** An immediate Model member selected under the same genuine saved-child
      ancestry as Content, but never minted from a parser Agent occurrence.
      Its distinct opaque constructor requires the active source binding,
      selected parent, bounded member and an installed/replayed child revision.
      Both Model and Content children may nest beneath it. A logical Model
      selection does not manufacture a native invocation; [borrowed] is the
      ACTUAL entered native scope at selection. The target and descendants
      expire with the invoking child callback. *)
  type t = Selected_child_target.Model.t

  val revision : t -> Prompt_revision.t
  val caller_revision : t -> Prompt_revision.t

  (** Comparison only: the exact saved Tool target retained by this Model
      ancestry. It is not a Root, native or inference admission proof. *)
  val saved_ancestor : t -> Claimed_developer.Selected.Target.t

  val matches_parent : t -> Selected_content_target.parent -> bool
  val check : t -> (unit, Agent_protocol.Error.t) result
  val check_now : t -> (unit, Agent_protocol.Error.t) result

  (** Same direct-or-genuine-descendant choice as Content, relative to this
      Model target's own entering borrow; no Root authority is granted. *)
  val check_current_or_descendant_now : t -> (unit, Agent_protocol.Error.t) result

  val check_descendant_now
    :  t
    -> child:Native_tool_invocation.selected_borrowed
    -> (unit, Agent_protocol.Error.t) result
end

(** Trusted host callback for the one selected descendant content target. The
    source runner mints the target; this function value does not itself grant
    Root, parser, provider or source authority. *)
type selected_content_runner =
  target:Selected_content_target.t
  -> startup:Chat_response.Startup_compilation.t
  -> compilation_limits:Chatml_compilation.limits
  -> programs:Chatml_prompt_programs.Restored.t option
  -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> input:Prompt.Chat_markdown.content_item list
  -> source:string
  -> (string, Agent_protocol.Error.t) result

module Source_binding : sig
  (** Immutable original-source template; borrows Store and source lease, without
      opening or claiming either. Bound ports belong to their actual resource
      switch. Join them before releasing the borrowed host owners. *)
  type t

  module Policy_identity : sig
    type t

    val source : t -> Chatml_source.Digest.t
    val runtime : t -> Chatml_source.Digest.t
  end

  (** Comparison-only identities of this installed source policy and the
      runtime factory that [bind_selected_child] actually constructs. Reuse
      its existing digest composition, including the exact script/content
      numeric limits; no caller-supplied revision or digest enters. *)
  val policy_identity
    :  t
    -> script_preparation:Runtime_script_preparation.t
    -> Policy_identity.t

  module Content_source : sig
    type t

    type side =
      | Declaring
      | Caller

    module Selected_member : sig
      (** Bounded Model selector bound to THIS physical Saved, Content or Model
          source port, its selected revision/side and original receipt. A path
          spelling or equal revision cannot manufacture this value. The
          immutable member is comparison data only: actual File/Captured
          acquisition and child publication repeat target/lease checks. *)
      type t

      (** Exact admitted request choice, never inferred from inventory. *)
      val mode : t -> Agent_protocol.Model_spawn_request.Source_mode.t
    end

    (** Select a local Model path relative to the exact selected revision's
        portable root member. The request string is a selector, not a read
        grant. This is pure bounded admission; it never opens a Scope. The
        caller chooses Captured versus File from its explicit current source
        policy BEFORE acquisition, never as fallback after either denies. *)
    val select_local_member
      :  t
      -> side:side
      -> reference:string
      -> mode:Agent_protocol.Model_spawn_request.Source_mode.t
      -> checkpoint:(unit -> unit)
      -> (Selected_member.t, Agent_protocol.Error.t) result

    (** Repeat the exact issuer, side, selected revision, original receipt,
        immediate logical parent and actual native borrow checks. A selected
        member is not a persistent grant and cannot outlive its callback. *)
    val check_selected_member
      :  t
      -> Selected_member.t
      -> (unit, Agent_protocol.Error.t) result

    (** Current selected Tool or descendant Content target and exact binding
        issuer, without opening a root Scope. Captured/builtin/literal
        conversion remains valid if the original lease is unavailable; this
        check is not a source grant. *)
    val check : t -> (unit, Agent_protocol.Error.t) result

    (** Comparison only; a caller must still use this port's checked read. *)
    val matches_revision : t -> side:side -> Prompt_revision.t -> bool

    (** A short exact original-lease bind/check for a dynamic operation after
        a yielding transport phase; no parser lookup or source read. *)
    val check_dynamic : t -> side:side -> (unit, Agent_protocol.Error.t) result

    (** Each operation opens the genuine original Scope, binds the exact
        selected declaration or post-code-view caller revision separately,
        performs ONE bounded lookup/read, then joins/closes Scope before
        returning. A missing, foreign or revoked caller denies dynamic input.
        No network I/O occurs while Scope is held. Work/byte observations are
        returned for the one aggregate upper budget. *)
    val read_local
      :  t
      -> side:side
      -> item:Prompt.Chat_markdown.basic_content_item
      -> max_bytes:int
      -> max_work_steps:int
      -> checkpoint:(unit -> unit)
      -> on_acquire:(unit -> unit)
      -> (Prompt_source_owner.Scope.Content_read.t, Agent_protocol.Error.t) result

    val remote_origin
      :  t
      -> side:side
      -> item:Prompt.Chat_markdown.basic_content_item
      -> max_work_steps:int
      -> checkpoint:(unit -> unit)
      -> (Prompt_source_owner.Scope.Origin_read.t, Agent_protocol.Error.t) result
  end

  module Selected_model_source : sig
    (** Physical source-selection result issued by THIS binding for one exact
        selected member. It retains the verified installed/replayed revision
        and observed bounded acquisition costs. Its representation cannot be
        paired with a different Content_source, member or Content_ports. *)
    type t

    val input_bytes : t -> int
    val work_steps : t -> int
  end

  module Content_ports : sig
    (** Two physical parser-owned Agent ports from one selected child: its own
        restored declaring parser and this invocation's actual caller.
        Neither port is an independent original-source grant or an enduring
        native callback; File selection still checks the actual live lease. *)
    type t

    (** Check the immediate selected parent and original saved ancestor, exact
        source binding and side's parser membership before invoking the lower
        content selector. Expected
        selected-source denial remains typed; cancellation and unexpected
        callback exceptions retain identity/backtrace. No ordinary source
        fallback occurs after denial. *)
    val run
      :  t
      -> side:Content_source.side
      -> declaration:Prompt.Chat_markdown.agent_content
      -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
      -> (string, Agent_protocol.Error.t) result

    (** Lend one Model target under the actual selected declaring or caller
        Port. [member] and [selected] must arise from this SAME binding, side,
        physical parent and retained member; equal paths/artifact IDs alone
        reject. The target expires when [f] returns, including exceptions and
        cancellation. The later with_selected_model_child preparation binds
        the selected revision's own parser Port for Model->Content and retains
        the actual caller Port separately. No parser Agent occurrence or native
        invocation is forged here. *)
    val with_selected_model_target
      :  t
      -> source:Content_source.t
      -> member:Content_source.Selected_member.t
      -> selected:Selected_model_source.t
      -> f:(Selected_model_target.t -> ('a, Agent_protocol.Error.t) result)
      -> ('a, Agent_protocol.Error.t) result
  end

  val create
    :  source:Source.t
    -> tool_contract:Chatml_compilation.Tool_contract.t
    -> runtime_policy_revision:Chatml_source.Digest.t
    -> content_limits:Chatmd_prompt_content.Limits.t option
    -> artifact_store:Agent_store.Prompt_artifact_store.t
    -> source_lease:Prompt_source_lease.t
    -> t

  (** Make a strict source-read port for this exact saved callback. [target]
      must carry THIS physical Source_binding issuer; an equal receipt or
      arbitrary revision is insufficient. No Scope remains open across
      Converter traversal, nested Agent invocation or provider entry. The
      port expires with the selected callback; each read rechecks Target and
      opens/closes its own short Scope. *)
  val selected_content_source
    :  t
    -> target:Claimed_developer.Selected.Target.t
    -> (Content_source.t, Agent_protocol.Error.t) result

  (** The same short read port for a separately selected descendant Agent
      content target. This cannot be minted by casting a saved Tool target or
      comparing revision IDs; the exact binding issuer, saved ancestor and
      current selected borrow must all match. Declaration/caller revisions are
      taken from the content target's authentic invoke triple. *)
  val selected_descendant_content_source
    :  t
    -> target:Selected_content_target.t
    -> (Content_source.t, Agent_protocol.Error.t) result

  (** Short read port for a checked Model member target. This target carries
      its own selected revision and actual caller Port; no declaring/caller
      side is inferred from a saved ancestor. *)
  val selected_model_content_source
    :  t
    -> target:Selected_model_target.t
    -> (Content_source.t, Agent_protocol.Error.t) result

  (** Select installed child code through this genuine borrowed source template.
      The caller authenticates its original wrapper/request and holds the native
      resource loan. Recorded selection always restores before source-mode
      branching; new File reads the original lease and new Captured uses custody.
      [invocation] is the actual admitted child role, not a permission grant.
      Install and verify before returning; no job/session/reservation is created.
      The whole source operation uses the original finite acquisition/runtime
      policies and borrowed checkpoint. Exceptions retain identity/backtrace. *)
  val select_child
    :  t
    -> script_preparation:Runtime_script_preparation.t
    -> env:Eio_unix.Stdenv.base
    -> parent:Prompt_revision.t
    -> reference:Prompt.Chat_markdown.Agent_reference.t
    -> invocation:Runtime_script_preparation.Invocation.t
    -> recorded:Chat_response.Background_request.Agent_artifact.t option
    -> checkpoint:(unit -> unit)
    -> (Prompt_child_source.t, Agent_protocol.Error.t) Result.t

  (** Prepare one local Model member chosen by the exact live Content_source
      port. The request's immutable [Captured|File] decision is made before any
      source acquisition. Captured reuses verified immutable inventory; File
      uses only the original retained source lease and existing bounded
      Builder/install path. A denied/foreign/revoked selection never falls
      through to the other mode. This operation does not mint a Content target
      or new native scope; [Content_ports.with_selected_model_target] brackets
      that separate selected-parent proof. [on_acquire] belongs to the ONE
      shared upper budget and fires before possible File I/O. *)
  val select_model_member
    :  t
    -> source:Content_source.t
    -> member:Content_source.Selected_member.t
    -> script_preparation:Runtime_script_preparation.t
    -> env:Eio_unix.Stdenv.base
    -> max_input_bytes:int
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> on_acquire:(unit -> unit)
    -> (Selected_model_source.t, Agent_protocol.Error.t) result

  (** Bind the exact restored revision before native registration, using its
      constructor's actual shell/content policies and response directory. No
      source read or initializer. Missing/closed source loans still permit saved
      or Captured selections; a new File selection rejects without fallback. *)
  val bind
    :  t
    -> script_preparation:Runtime_script_preparation.t
    -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
    -> approval_provider:Shell_runtime.Approval_broker.provider
    -> response_dir:Eio.Fs.dir_ty Eio.Path.t
    -> env:Eio_unix.Stdenv.base
    -> revision:Prompt_revision.t
    -> sw:Eio.Switch.t
    -> (Chat_response.Tool.Agent_source_runner.t, Agent_protocol.Error.t) Result.t

  (** Bind the selected child revision to its own parser-owned Agent port under
      the immediate parent's joined switch. [parent] is the exact physically
      selected saved Tool, Content or Model child, and [revision] is this child's
      restored target, never its ancestor's parser. The port checks the entire
      original saved ancestry and this binding at each content invocation,
      then mints a distinct [Selected_content_target] for the trusted callback.
      The genuine caller port from this child's invoke triple remains separate.
      Each nested level binds afresh through this same operation; no parent Port
      is reused as the nested declaration Port. A failed callback never falls
      through to ordinary Driver or Fetch. No source Scope is retained. *)
  val bind_selected_child
    :  t
    -> parent:Selected_content_target.parent
    -> run_selected_content:selected_content_runner
    -> script_preparation:Runtime_script_preparation.t
    -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
    -> approval_provider:Shell_runtime.Approval_broker.provider
    -> response_dir:Eio.Fs.dir_ty Eio.Path.t
    -> env:Eio_unix.Stdenv.base
    -> revision:Prompt_revision.t
    -> sw:Eio.Switch.t
    -> (Content_ports.t, Agent_protocol.Error.t) result
end

module Deferred_developer : sig
  (** Exact ordinary Agent declarations from one owned Prompt revision, prepared
      before the physical developer Root exists. This is a source/native
      registration producer, not an inference or child capability owner. *)
  type t

  (** Bind [source] to [revision] under its actual original source lease and
      bounded runtime policy. Scan the genuine Parsed agent references and
      require exactly one physical Tool reference for every emitted ordinary
      Agent declaration. Duplicate names, foreign declarations and missing
      source preparers reject before any function is exposed. For this developer
      binding only, each selected child (restored, Captured or newly acquired
      File) receives a bounded preflight before Driver. Without
      [run_selected_target], only literal authored message/config/reasoning
      content is admitted: callable Tool declarations, executable scripts,
      authored tool calls/responses, nested Agent content and document/image
      fetch content reject. With the trusted Tool callback, only an exact saved
      target in the current selected native scope is lent to it. Within that
      physically entered child, the trusted host later binds a distinct
      [Source_binding.Content_ports] with its selected Content callback. That
      port may receive a distinct target for an actual parser-owned Agent
      occurrence; a failed descendant check never falls through to literal
      Driver execution. Outside an ambient
      saved child, existing literal-only content behavior remains unchanged.
      Each callback must separately admit every child effect.
      A callback error never falls back to Driver or the literal path. The
      selected Tool_contract is the explicit value already retained by
      [source]; a stored digest cannot choose another ABI.

      [with_context] is trusted host code which must reject until the exact
      physical Root is bound, after revocation, or outside a saved selected
      Agent callback. This producer never invokes it during preparation. Each
      native Source preparer delegates to this same bound source port; neither
      a queued artifact nor [Source_launch] is an original read grant. The
      positive count and shared checkpoint bound declaration/reference scans
      before retaining functions. [sw] joins all port borrowers. *)
  val prepare
    :  ?run_selected_target:
         (target:Claimed_developer.Selected.Target.t
          -> startup:Chat_response.Startup_compilation.t
          -> compilation_limits:Chatml_compilation.limits
          -> programs:Chatml_prompt_programs.Restored.t option
          -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
          -> input:Prompt.Chat_markdown.content_item list
          -> observer:Chat_response.Driver.agent_observer option
          -> source:string
          -> (string, Agent_protocol.Error.t) result)
    -> Source_binding.t
    -> script_preparation:Runtime_script_preparation.t
    -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
    -> approval_provider:Shell_runtime.Approval_broker.provider
    -> response_dir:Eio.Fs.dir_ty Eio.Path.t
    -> env:Eio_unix.Stdenv.base
    -> revision:Prompt_revision.t
    -> sw:Eio.Switch.t
    -> max_agents:int
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> with_context:
         (implementation:Ochat_function.t
          -> invocation:Ochat_function.Invocation.t
          -> (Eio_unix.Stdenv.base Chat_response.Ctx.t -> string)
          -> string)
    -> (t, Agent_protocol.Error.t) result

  (** Immutable registrations for this exact prepared revision/port. An upper
      native owner must admit their physical implementations before any call. *)
  val registrations : t -> Chat_response.Agent_runtime.native_registration list
end
