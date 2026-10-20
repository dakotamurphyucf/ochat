open Core

(** Host-only read/capture authority for authoring. This adapter borrows the
    actual configured roots admitted by {!Project_source_host}, and delegates
    native reads to {!Chatml_local_roots} / {!Chatml_local_source} and complete
    local project preparation to {!Chatml_project_prepare}. It owns no second
    resolver, runtime workspace, agent session, writable root or Git fetch path.
    OCH-277 extends Project_source_host's joined borrow to lend the configured
    prefix paired with each admitted anchor and the exact Project_source_policy
    project policy/limits already built at startup; the current [with_host]
    exposes only anchors and Prompt_script_preparation.Policy, insufficient for
    standalone authoring capture/browse. No profile is independently rebuilt.
    The portable editor revision state lives in Chatml_authoring_context (OCH-278). *)

module Root : sig
  (** Host-incarnation-local selection of one configured external source-read
      anchor. Returned only after operator policy admits the original actor.
      An index or display value is never a grant; stale/foreign selections reject.
      The prompt receipt's private original root is not exposed here. *)
  type t

  val index : t -> int
end

module Read_policy : sig
  type t

  (** Trusted host/operator decision. The callback receives an actor and an
      index into this host's already admitted [external_roots] in config order.
      Initial selection checks its original actor; later operations check both
      that actor and the current request actor. The host must require an
      explicit source-read permission; the
      current generic Agent_protocol scopes grant none. No default allow-all and
      no policy derived from a workspace/project ID or client path. Callbacks
      must be pure and may not yield. Exceptions preserve identity and
      backtrace; they are never converted into authorization results. *)
  val create : authorize:(Operator_authorization.t -> root_index:int -> bool) -> t

  val deny_all : t
end

module Document_policy : sig
  type t

  (** Separate host/operator decision to admit client-supplied document bytes.
      No source-read root, project membership or workspace ID follows from it.
      The original and request actors are checked for the same current
      principal on every operation. This pure callback may not yield; its
      exceptions keep identity/backtrace. Callers explicitly pass [deny_all]
      unless supplied-document admission is authorized. *)
  val create : authorize:(Operator_authorization.t -> bool) -> t

  val deny_all : t
end

module Limits : sig
  type t

  (** Aggregate host admission and per-call result limits. Project acquisition
      additionally keeps Project_source_policy's existing file/graph/work caps;
      this value never raises or resets them. [max_manifest_entries] bounds
      returned capture rows including the selected lock; [max_browse_entries]
      charges every native directory entry before filtering. [max_read_bytes]
      bounds one explicit read, and [max_context_read_bytes] charges all such
      reads over one context's lifetime. [max_context_requests] reserves one
      bounded capture, browse or read attempt before work, including failed and
      cancelled attempts; with existing per-attempt policy this bounds lifetime
      work without resetting an attempt. Fields are positive; sums are checked. *)
  val create
    :  max_contexts:int
    -> max_context_requests:int
    -> max_manifest_entries:int
    -> max_browse_entries:int
    -> max_read_bytes:int
    -> max_context_read_bytes:int
    -> t Or_error.t
end

module Error : sig
  type t =
    | Unauthorized
    | Expired_actor
    | Foreign_root
    | Closed
    | Busy
    | Foreign_domain
    | Context_limit
    | Result_limit
    | Unsupported_source_selection
    | Sources of Project_source_host.Error.t
    | Root of Chatml_local_roots.Error.t
    | Local of Chatml_local_source.Error.t
    | Project of Chatml_project_prepare.Error.t
    | Source_capture of Core.Error.t
    | Document_parser of Prompt.Chat_markdown.Generated.error
    | Document_bundle_limit
    | Project_declaration_unavailable
    | Invalid_project_declaration
    | Selected_script_unavailable

  (** Finite safe projection. Root errors expose only their finite code;
      project errors use a coarse tag. Neither exposes native paths, exception
      text, credentials or source bytes. *)
  val sexp_of_t : t -> Sexp.t
end

module Capture : sig
  (** Immutable successful local-only closure. No live root, actor proof, Eio
      owner, callback, cache, run permission or execution capability survives.
      Paths and digests describe exact acquired bytes, not durable filesystem
      identity or an all-file atomic observation. *)
  type t

  val build : t -> Chatml_project_build.t
  val prepared : t -> Chatml_prepared_program.t
  val fingerprint : t -> Chatml_source.Digest.t
  val sources : t -> Chatml_source.t list
end

module Entry : sig
  (** A portable candidate name returned by bounded nonrecursive directory
      enumeration. An entry is neither a regular-file proof nor a read grant;
      explicit [read_member] performs descriptor-relative admission again. *)
  type t

  val member : t -> Source.Portable_path.t
end

type t
type host = t

module Historical_roots : sig
  type t

  (** Copy only this Source_host's physical issuer and bounded distinct root
      indices from actual Root selections after checking their original/current
      actor and source-read admission. The count cannot exceed the configured
      external-root ceiling. No Root object or actor token survives; an
      invalid/foreign root fails this observational issuance without widening
      the enclosing capture. *)
  val issue
    :  host
    -> actor:Operator_authorization.t
    -> roots:Root.t list
    -> (t, Error.t) result

  (** Nonyielding current source-host/actor/read-policy comparison for every
      admitted index. Editor context closure or ordinary file edit is not a
      revocation. Closed/foreign host, revoked actor or read grant denies.
      This is not a filesystem read or execution grant. *)
  val check_read_now
    :  host
    -> actor:Operator_authorization.t
    -> t
    -> (unit, Error.t) result

  (** Checked conservative retained-byte charge for the physical issuer and
      bounded selected indices. No Root representation or mutable host state
      is exposed. [None] means the requested finite cap cannot contain it. *)
  val charge : t -> max_bytes:int -> int option
end

(** Bind the original daemon-owned Project_source_host and a mandatory trusted
    operator read policy. [sources] and its Eio switch outlive this owner. There
    is no filesystem path or root-opening constructor here. All methods remain
    on the creating Eio domain. Close revokes admission, cancels and joins work. *)
val create
  :  Project_source_host.t
  -> sw:Eio.Switch.t
  -> read_policy:Read_policy.t
  -> document_policy:Document_policy.t
  -> limits:Limits.t
  -> t

(** Enumerate only configured roots for which the original actor is current and
    the host read policy allows access now. A single enumeration at a time is
    tracked, cancelled and joined by host close; concurrent or reentrant calls
    reject [Busy], and enumeration cannot close its own host. The returned
    opaque selections remain host/actor-lifetime affine; every later method
    repeats both checks. The wire adapter may assign connection-local IDs,
    never deserialize a [Root.t]. *)
val roots
  :  t
  -> actor:Operator_authorization.t
  -> checkpoint:(unit -> unit)
  -> (Root.t list, Error.t) Result.t

(** Nonyielding final read-grant comparison for a previously issued root.
    It checks the physical host issuer and both original/current actors using
    the same current read policy as root operations. It is not a native read or
    permission to skip the full joined operation above. *)
val check_root_current
  :  t
  -> actor:Operator_authorization.t
  -> Root.t
  -> (unit, Error.t) Result.t

module Retained_owner : sig
  type t

  (** A separate, counted Source_host context minted from an authenticated
      project/document context. It retains the same physical root set or
      supplied-document policy and original actor, but no source bytes, reader,
      parser, editor connection or context-generation lease. Editor close does
      not close it; Source_host shutdown does. Current original/request actor,
      root and document policy are checked on every admission. *)
  val check_current : t -> actor:Operator_authorization.t -> (unit, Error.t) Result.t

  (** Idempotent joined release of this one retained context slot. Only the
      owning REPL instance may close it, after its statement/Task callbacks
      and rooted work have joined. *)
  val close_and_wait : t -> unit
end

module Context : sig
  (** Connection-scoped source selection, not a runtime workspace or prepared
      editor overlay. Stores the original actor and selected root; no native FD
      or Git grant is retained between operations. An authenticated request must
      supply its current actor each time. A new actor for the same principal may
      rebind only after policy reauthorization while the original proof remains
      current; reconnect otherwise opens a new context. *)
  type t

  val root : t -> Root.t

  (** Exact bounded root and dependency selections already admitted by this
      Context, for observational historical-origin issuance only. A caller
      must still pass them to [Historical_roots.issue] under current policy. *)
  val selected_roots : t -> Root.t list

  (** The exact contract admitted with this source context. Comparison data
      for recomposing its current overlay; callers still reauthorize the
      original/current actor and host selection before using it. *)
  val contract : t -> Chatml.Chatml_compiler_contract.t

  (** Currentness check for pinned project captures, overlays and
      cached analysis. Reuse the original actor plus current request actor and
      every explicitly selected root's existing policy; do not read, recapture
      or infer a grant from the context ID. The handler checks before serving
      and after yielding work. An async handler separately bounds its request
      lifetime until that final check. *)
  val check_current : t -> actor:Operator_authorization.t -> (unit, Error.t) Result.t

  (** Nonyielding, capacity-checked transfer of the authenticated root set to
      a separately counted retained owner. The original editor Context stays
      live and unchanged on success or failure. This does not recapture source
      or produce a read capability. The caller must still prove its exact
      selected graph/contract before publishing a REPL instance. *)
  val retain_owner
    :  t
    -> actor:Operator_authorization.t
    -> (Retained_owner.t, Error.t) Result.t

  (** Select only a member of the already granted root. [project] is a validated
      portable project declaration member, [selection] is the existing project
      selector, and [contract] is the host's authentic static contract, never a
      digest decoded from the client. This does not capture or execute anything.
      The host constructs a fresh lock ID for pure composition; no file write.
      [dependency_roots] explicitly selects additional configured anchors for
      local dependencies. Reject duplicate or foreign roots. Never pass every
      configured anchor to the resolver unfiltered; reauthorize the selected
      root and each dependency root before and after yielding work. *)
  val open_root
    :  host
    -> actor:Operator_authorization.t
    -> root:Root.t
    -> dependency_roots:Root.t list
    -> project:Source.Portable_path.t
    -> selection:Chatml_project.Selection.request
    -> contract:Chatml.Chatml_compiler_contract.t
    -> (t, Error.t) Result.t

  (** Complete host capture with one selected source policy/deadline/work budget.
      Borrow the source host under its joined [with_host], select only this
      context's explicitly authorized root/dependency anchor set and pass NO
      Git store or grants to project preparation.
      An authored Git dependency fails closed; existing exact captured Git/lock
      bundles belong to the separate immutable upload/restore path. Use
      Build_without_previous for a local-only unsaved lock, never a silent Update
      or network retry. Recheck actor/current grant before and after all yielding
      work. On failure publish no partial graph or source list. *)
  val capture
    :  t
    -> actor:Operator_authorization.t
    -> clock:_ Eio.Time.Mono.t
    -> checkpoint:(unit -> unit)
    -> (Capture.t, Error.t) Result.t

  (** Bounded nonrecursive path assistance through the selected retained root.
      Count every native entry against the existing source policy before
      filtering nonportable names; no ambient cwd/fs, implicit dependency scan,
      symlink traversal or hidden unbounded list. Results are candidates only. *)
  val list_directory
    :  t
    -> actor:Operator_authorization.t
    -> directory:Source.Portable_path.t option
    -> checkpoint:(unit -> unit)
    -> (Entry.t list, Error.t) Result.t

  (** Exact bounded read for an explicitly selected portable member of this
      granted root. It uses Local.read's no-follow descriptor operation and
      observed-change check, not a pathname read or a write lease. The returned
      bytes are a new read observation; only [capture] assembles a graph. *)
  val read_member
    :  t
    -> actor:Operator_authorization.t
    -> member:Source.Portable_path.t
    -> checkpoint:(unit -> unit)
    -> (string, Error.t) Result.t

  (** Idempotently revoke this context, cancel and join in-flight source work.
      Closing the connection closes its contexts before source-host shutdown;
      an immutable Capture returned earlier has an independent lifetime. All
      operations require the creating domain. Concurrent or reentrant operations
      reject Busy without waiting on their own gate. A close called from its
      own active operation raises [Invalid_argument] rather than self-joining. *)
  val close_and_wait : t -> unit
end

module Document_capture : sig
  (** One exact host-owned source observation. A host member is read through
      the admitted local root; supplied bytes are captured under a fresh
      host-minted source ID with no root grant. The receipt contains no live
      actor, path capability, graph, parser result or execution permission. *)
  type t

  val source : t -> Chatml_source.t
end

(** Fresh root-level observations before any project/document context exists.
    Both methods require the original Root actor and current request actor's
    independent native read grant, checked before and after yielding work.
    They reuse this host's joined root-operation gate, configured no-follow
    local source owner and existing per-operation path/byte/work ceilings; no
    project context, resolver or native path grant is minted. Concurrent root
    operations return Busy. [read_root_member] constructs one exact immutable
    host-owned source capture; later use still requires a context admission. *)
val list_root_directory
  :  t
  -> actor:Operator_authorization.t
  -> root:Root.t
  -> directory:Source.Portable_path.t option
  -> checkpoint:(unit -> unit)
  -> (Entry.t list, Error.t) Result.t

val read_root_member
  :  t
  -> actor:Operator_authorization.t
  -> root:Root.t
  -> member:Source.Portable_path.t
  -> checkpoint:(unit -> unit)
  -> (Document_capture.t, Error.t) Result.t

module Document_context : sig
  type t

  module Parsed_capture : sig
    (** Immutable exact root/import closure plus the genuine same-run parsed
        receipt. It retains no root owner, actor, loader, request callback or
        future file-read capability. The selected compiler Build and script
        membership still belong to the semantic authoring service. *)
    type t

    val source : t -> Chatml_source.t
    val bundle : t -> Chatmd_source_bundle.t
    val parsed : t -> Prompt.Chat_markdown.Generated.receipt

    (** Exact same-run parser Root/Import inputs associated with the captured
        sources that supplied their authored bytes. Root retains [source]; each
        imported source uses this context's admitted source-owner namespace and
        its validated portable captured member. Every input in the genuine
        parsed root and imported agents is represented once. At most1024 input
        visits and8MiB of byte comparisons are admitted before the source
        association is published. The association
        grants no native read; consumers still compare parser bytes/digest and
        current document generation before source-owned projections. *)
    val input_sources : t -> (Chatmd_parser_input.t * Chatml_source.t) list

    (** Pure extraction of one source-only script from THIS genuine parsed
        closure. The private route proves physical declaration/input/source
        membership and exact inline or captured external body bytes; an
        external path is relative to its declaring input. No project route,
        native read, actor grant or current-context assertion follows. The
        enclosing owner must still check this capture's current generation and
        separately admit an execution target. Bounds reject before body copy;
        checkpoint/cancellation exceptions preserve identity. *)
    val script_source
      :  t
      -> declaration:Prompt.Chat_markdown.Declaration.t
      -> max_script_bytes:int
      -> max_locator_bytes:int
      -> max_path_components:int
      -> checkpoint:(unit -> unit)
      -> (Chatml_source.t, Error.t) result

    (** Pure bounded root-only parse of the exact already supplied Source.
        It owns no host read grant; the one-member bundle makes imports absent
        unless all genuine parser inputs map to that same root. The enclosing
        context owner checks actor/source/generation before and after work and
        under the final publication gate. [dir] is display provenance only. *)
    val parse_root_only
      :  limits:Chatmd_source_bundle.limits
      -> source:Chatml_source.t
      -> dir:Eio.Fs.dir_ty Eio.Path.t
      -> checkpoint:(unit -> unit)
      -> (t, Error.t) Result.t

    (** Pure bounded reparse after an unsaved root edit, using only the same
        owner's previously captured immutable imports. [bundle]'s root bytes
        must equal [source]; each retained import member/byte/source ID must
        match [previous] exactly. The existing Generated parser and input
        binder create a NEW genuine Parsed receipt and associate its new parser
        input objects with the original captured source objects. It never
        reads [dir], recaptures a dependency or reuses old Parsed facts. New
        imports absent from the frozen bundle fail with the existing typed
        parser error until explicit Analyze/rebuild. The validated source,
        file, aggregate byte and parser-work limits are checked before copy or
        parse; checkpoint/cancellation exceptions retain their identity. *)
    val reparse_frozen
      :  previous:t
      -> source:Chatml_source.t
      -> bundle:Chatmd_source_bundle.t
      -> limits:Chatmd_source_bundle.limits
      -> dir:Eio.Fs.dir_ty Eio.Path.t
      -> checkpoint:(unit -> unit)
      -> (t, Error.t) Result.t
  end

  module Project_capture : sig
    (** Immutable exact declaration source and authoritative decoded project,
        captured during an explicit document Analyze/rebuild independently of a
        successful script Build. It contains no live root/read grant. *)
    type t

    val source : t -> Chatml_source.t
    val project : t -> Chatml_project.t
    val member : t -> Source.Portable_path.t

    (** Comparison only with the same host-incarnation selected root, including
        original actor. It does not reauthorize a current actor or turn this
        receipt into a root capability. *)
    val belongs_to_root : t -> Root.t -> bool

    (** Comparison-only owner association for an already captured project.
        Return its bound root index only when [issuer] is the exact physical
        configured host that minted the captured Root and [original_actor]
        matches that Root's original principal ID and authentication kind.
        The index is not a read grant;
        the configured policy must still authorize both current actors, and
        the Source owner independently checks current root authority. *)
    val root_index_if_bound
      :  t
      -> issuer:Project_source_host.Identity.t
      -> original_actor:Operator_authorization.t
      -> int option

    (** Match the same immutable Parsed_capture and actual declaration object
        admitted by [capture_project_declaration]. This is deliberate parser
        object identity, not text/path equivalence; the enclosing Core owner
        separately verifies current source/generation and actor authority. *)
    val matches
      :  t
      -> parsed:Parsed_capture.t
      -> declaration:Prompt.Chat_markdown.Declaration.t
      -> bool

    (** Fixed-size identity precomputed with bounded input metadata during
        admission. Comparison only; the current host policy and Core context
        generation remain the authority. *)
    val identity : t -> Chatml_source.Digest.t

    (** Compare exact captured bytes with the ACTUAL root project declaration
        in [build]'s lock and Prepared inventory. Source namespaces can differ;
        neither namespace equality nor basename/suffix equality is used. The
        caller first proves the admitted selected Build belongs to this root
        and [member] using its real host route/catalog policy. This pure
        bounded comparison alone grants no route or read authority. *)
    val matches_project_contents
      :  t
      -> build:Chatml_project_build.t
      -> max_compare_bytes:int
      -> checkpoint:(unit -> unit)
      -> (bool, Error.t) result
  end

  module Script_capture : sig
    (** Immutable locally prepared selected script. The declaration, parsed
        closure and independently captured project are the exact objects used
        during this one joined source-owner attempt. It may retain the opaque
        project comparison receipt but exposes no Root getter, native directory,
        reader, callback, Eio owner or execution grant. Current actors and
        root policy must be rechecked by the enclosing service. This is an
        authentic prepared Build and contract, not a checked script or an
        executable target: the upper Selected admission and semantic checker
        still prove actual surface/kind compatibility before serving facts. *)
    type t

    val build : t -> Chatml_project_build.t
    val contract : t -> Chatml.Chatml_compiler_contract.t
    val entry_source : t -> Chatml_source.t

    (** Physical same-run association for Core's later bounded immutable
        selection admission. Equal script IDs or paths are insufficient. *)
    val matches
      :  t
      -> parsed:Parsed_capture.t
      -> declaration:Prompt.Chat_markdown.Declaration.t
      -> project:Project_capture.t
      -> bool
  end

  (** Nonyielding owner-bound recheck for one retained Standard script AFTER
      semantic work may have yielded and BEFORE the Core publication gate. The
      receipt must have been issued by this exact live Document_context and
      still match its genuine Parsed/declaration/project objects. Recheck both
      original and request actors, private root/read grant and the exact
      project/Library environment's configured Standard target for each actor.
      Equal IDs, native paths or Build digests are not reauthorization. No
      filesystem read, source-owner borrow, parser, checkpoint or Eio yield;
      [select_contract] is trusted, pure and nonyielding. It is not called from
      the final no-yield Core CAS itself. Ordinary revocation and changed
      selection return finite [Error.t]; unexpected callback exceptions retain
      their identity and backtrace. *)
  val check_selected_now
    :  t
    -> script:Script_capture.t
    -> actor:Operator_authorization.t
    -> select_contract:
         (Operator_authorization.t
          -> root:Root.t
          -> project:Source.Portable_path.t
          -> selection:Agent_protocol.Authoring.Context.Selection.t
          -> Chatml.Chatml_compiler_contract.t option)
    -> (unit, Error.t) result

  (** Prepare one actual parsed script through the SAME joined local source
      owner and project preparation pipeline used by [Context.capture]. The
      [project_capture] must physically match [parsed] and [declaration] and
      belong to this document's private selected root; its project member and
      declaration-relative route are revalidated. The selected script body
      comes only from the exact captured parser bundle. Inline bodies use a
      captured fragment; an external source may use a project-relative
      captured file only after exact bundle-byte and same-root affinity proof.
      A missing or ambiguous route fails closed. No Root getter or Prompt owner
      receipt is exposed.

      [select_contract] is the installer's trusted, pure, nonyielding lookup
      for the actual root, project member and Library environment selected by
      this declaration. It must use current authenticated operator policy;
      this method calls it for both original and request actors before work
      and rechecks after yielding work. Only the same genuine Standard target
      can be retained. The lower Project policy reserves finite graph, file,
      source-byte and work allowances before native acquisition, including on
      failed attempts; [max_work_steps] bounds this source-host attempt's
      additional route, source and callback work. The document context's
      request allowance and actor/root checks remain active. Its separate
      aggregate-read ledger applies to explicit member/import reads, not to
      Project preparation, whose per-attempt limits and context request count
      bound lifetime preparation work.
      [max_script_bytes] bounds body admission before copy/hash. All supplied
      limits are positive and bounded by the existing parser/source ceilings.
      Cancellation and unexpected callback exceptions preserve their original
      identity/backtrace. A failure publishes no partial Build. *)
  val prepare_selected_script
    :  t
    -> actor:Operator_authorization.t
    -> parsed:Parsed_capture.t
    -> declaration:Prompt.Chat_markdown.Declaration.t
    -> project_capture:Project_capture.t
    -> select_contract:
         (Operator_authorization.t
          -> root:Root.t
          -> project:Source.Portable_path.t
          -> selection:Agent_protocol.Authoring.Context.Selection.t
          -> Chatml.Chatml_compiler_contract.t option)
    -> clock:_ Eio.Time.Mono.t
    -> max_script_bytes:int
    -> max_locator_bytes:int
    -> max_path_components:int
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> (Script_capture.t, Error.t) Result.t

  (** Uses the same host-owned context activity gate, actor/root rechecks,
      request allowance, explicit-read quotas where applicable, cancellation
      stack and joined switch lifetime as project source contexts. Nested
      A -> B -> A work must still see A as
      active: closing A or the host from that stack rejects self-join. There
      is no parallel document-specific authority or resolver. *)

  module Origin : sig
    type t =
      | Host_member
      | Supplied
    [@@deriving equal, sexp_of]
  end

  val origin : t -> Origin.t

  (** Comparison-only actual selected host Root for Host_member, and None
      exactly for Supplied. The enclosing Context owner still checks both
      actors, the current Root grant and source generation; this getter opens
      no source and does not make an index or path a grant. *)
  val root : t -> Root.t option

  val source_id : t -> Chatml_source.Id.t

  (** Explicitly rebuild an authorized imported closure for [source]'s exact
      current unsaved editor bytes. Reject another source ID or original member
      label, supplied-document origin, foreign domain, closed/revoked context,
      or exhausted context request/read allowance before parser allocation.
      Borrow the SAME configured root/Chatml_local_source under the existing
      joined context operation; every discovered import consumes its retained
      no-follow read/byte policy before allocation. The root itself comes only
      from [source], never from a fresh host read. Count parser work, source
      paths/files/bytes and retained strings under one validated generated
      bundle limit installed on the loader before parsing; host import-read
      allowance is intersected before each new native read. The root input's
      actual captured source ID and portable member must equal this context's
      original source ID/member; [dir] is never used as identity or authority.
      Recheck actor/root after waits before returning. No partial capture is
      returned on expected failure or cancellation. This operation does not
      alter the editor ledger or assert a multi-file atomic filesystem read.
      The caller checks its exact context generation before and after this
      operation and publishes the immutable result under its own CAS. Later
      Prefix/Query must not call it. [dir] is a capability-valued path used
      solely for parser display provenance; this operation never reads it or
      uses it as an alternative import resolver. *)
  val capture_parser_bundle
    :  ?limits:Chatmd_source_bundle.limits
    -> t
    -> actor:Operator_authorization.t
    -> source:Chatml_source.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> checkpoint:(unit -> unit)
    -> (Parsed_capture.t, Error.t) Result.t

  (** Explicitly capture one project declaration named by an ACTUAL member of
      [parsed]'s root/imported declaration registry. Bind its declaring input
      through [Parsed_capture.input_sources], including exact authored bytes,
      then resolve the authored project spelling relative to that input's
      portable member with the existing Source_loader rule. Never use script
      [src], display provenance or a suffix match. The same Document_context
      root, original/request actors, joined operation, no-follow reader,
      request and aggregate-read quotas govern the single local read. The
      admitted source ID belongs to this context's namespace and exact member.
      Bound locator components/bytes, membership visits, read bytes, schema
      decode and identity construction before allocation. [max_work_steps]
      charges checkpoints throughout root resolution, read and pure admission
      as one attempt, without a new allowance for each phase. Schema decoding is
      the existing bounded Chatml_project.decode; its JSON stage does not poll
      internally. The returned receipt is immutable even if the host file later
      changes. The caller separately checks exact editor context generation
      before/after this operation and publishes it with the same Parsed head.
      A supplied document or ungranted/outside-root route is unavailable; a
      foreign Parsed/declaration is Foreign_root; malformed project contents
      are Invalid_project_declaration. Budget exhaustion uses Result_limit.
      Checkpoint/cancellation exceptions propagate unchanged, with no partial
      receipt or alternative filesystem/network fallback. *)
  val capture_project_declaration
    :  t
    -> actor:Operator_authorization.t
    -> parsed:Parsed_capture.t
    -> declaration:Prompt.Chat_markdown.Declaration.t
    -> schema_limits:Document_schema.Limits.t
    -> max_locator_bytes:int
    -> max_path_components:int
    -> max_identity_bytes:int
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> (Project_capture.t, Error.t) Result.t

  (** Recheck the original actor, current request actor, retained root or
      supplied-byte policy, host lifetime and creating domain without reading
      or yielding. The handler calls this before serving pinned ledger
      bytes/overlays or syntax, and again after any yielding preparation before
      publishing a response. It does not grant project or execution authority.
      Concurrent pure reads need no activity gate; an async handler must retain
      its own bounded request lifetime until the final check. *)
  val check_current : t -> actor:Operator_authorization.t -> (unit, Error.t) Result.t

  (** Retain the same physical root or supplied policy and original actor in
      one separately charged Source_host slot. The captured source ID/bytes
      remain in the caller's immutable Prepared inventory; no new source ID,
      read or parse occurs here. Closing the editor context leaves this owner
      live until explicit release or Source_host shutdown. *)
  val retain_owner
    :  t
    -> actor:Operator_authorization.t
    -> (Retained_owner.t, Error.t) Result.t

  (** Bind one exact portable member of an explicitly selected read root.
      Reuse the host's existing root policy, original actor and joined
      operation gate; no project declaration, Build, selection or compiler
      contract is needed. Capture exact bytes once through the existing
      no-follow local read owner before publishing the context. A failed read
      registers no context or source. The source ID is minted by this host and
      remains stable across explicit refresh; later context source reads use
      the pure ledger's pinned bytes, not a new host observation. *)
  val open_host
    :  host
    -> actor:Operator_authorization.t
    -> root:Root.t
    -> member:Source.Portable_path.t
    -> checkpoint:(unit -> unit)
    -> (t * Document_capture.t, Error.t) Result.t

  (** Admit exact bounded supplied bytes under [Document_policy], independent
      of [Read_policy]. [member] is a validated source label, never a host path
      lookup. Mint a fresh source ID and capture exact bytes under the host's
      document/source limits, with no project/root authority. Failed capture
      registers no context. Do not normalize invalid UTF-8 or newlines. Return
      one immutable initial capture, which the pure ledger then admits. *)
  val open_supplied
    :  host
    -> actor:Operator_authorization.t
    -> member:Source.Portable_path.t
    -> contents:string
    -> checkpoint:(unit -> unit)
    -> (t * Document_capture.t, Error.t) Result.t

  (** Explicitly re-read only the same retained host root/member after the
      host-authorized context refresh action. A supplied context returns
      [Unsupported_source_selection]; changing supplied base bytes requires a
      new context. The old capture stays immutable. Recheck original and request
      actor/policy before and after yielding work; charge the same context
      attempt/read-byte caps. The pure ledger separately checks the expected
      generation and publishes the new base. *)
  val refresh
    :  t
    -> actor:Operator_authorization.t
    -> checkpoint:(unit -> unit)
    -> (Document_capture.t, Error.t) Result.t

  (** Bounded nonrecursive path candidates on a host-member root only. This
      grants no member read and uses the existing root owner/policy; a supplied
      document cannot browse a host root. *)
  val list_directory
    :  t
    -> actor:Operator_authorization.t
    -> directory:Source.Portable_path.t option
    -> checkpoint:(unit -> unit)
    -> (Entry.t list, Error.t) Result.t

  (** Idempotently revoke/cancel/join work, including from host switch release.
      The creating Eio domain owns this context. Concurrent/reentrant work
      fails [Busy]; closing one's own active operation, including through a
      nested other context, raises Invalid_argument rather than self-joining. *)
  val close_and_wait : t -> unit
end

(** Idempotently revoke, cancel and join every context, including from the
    owner's switch release hook. Explicit close unregisters that hook.
    Calling from an active operation on this host raises [Invalid_argument]. *)
val close_and_wait : t -> unit
