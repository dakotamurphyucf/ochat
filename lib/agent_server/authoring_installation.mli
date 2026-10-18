open! Core
module P = Agent_protocol
module Service = Authoring_context_service

(** Trusted, validated daemon composition for the existing joined authoring
    service. Policies are host callbacks over current authenticated authority;
    no client ID, path, digest, selected feature or receipt supplies a grant. *)
module Host_policy : sig
  type t

  module Source : sig
    type t

    val create
      :  read:Authoring_source_host.Read_policy.t
      -> document:Authoring_source_host.Document_policy.t
      -> limits:Authoring_source_host.Limits.t
      -> t
  end

  module Upload : sig
    type t

    val create
      :  limits:Agent_store.Authoring_upload_store.Limits.t
      -> grant:Authoring_upload_host.Upload_policy.t
      -> publication:Authoring_upload_host.Publication.t option
      -> rebind:Authoring_upload_host.Rebind.t option
      -> t
  end

  module Semantic : sig
    type t

    val create
      :  script_selection:Service.Semantic.Script_selection.Policy.t option
      -> project_metadata:Service.Semantic.Project_metadata.Policy.t option
      -> project_build_limits:Chatml_project_build.Limits.t
      -> limits:Service.Semantic.Limits.t
      -> prefix_limits:Service.Semantic.Prefix_limits.t
      -> query_limits:Service.Semantic.Query_limits.t
      -> document_dir:Eio.Fs.dir_ty Eio.Path.t
      -> t
  end

  module Assistance : sig
    type t

    (** The two feature names can be installed independently, borrowing one
        actual assistance sibling and the same context owner. *)
    val create
      :  lexical:bool
      -> assistance:bool
      -> selected_tools:Service.Assistance.Selected_tool_policy.t option
      -> project_build_limits:Chatml_project_build.Limits.t
      -> limits:Service.Assistance.Limits.t
      -> t Or_error.t
  end

  (** Concrete daemon/CLI baseline: current authenticated actors can open
      exact supplied document bytes and use core lifecycle, lexical scanning
      and local structural assistance. All limits are finite and validated
      against the daemon's actual Initialize inbound ceiling. No native source
      read, durable upload, semantic handler, selected Build or execution grant
      is installed. Semantic methods are therefore uninstalled by default. A
      separately installed semantic handler reports unavailable when no genuine
      selected Build exists; it never manufactures compiler facts. *)
  val basic_supplied : initialize_limits:P.Initialize.Limits.t -> (t, P.Error.t) result

  (** Finite upload-only composition with no context or source-read family.
      The explicit upload grant remains the sole authority for publication;
      selecting [authoring.bundle.v1] grants no project read or compiler access. *)
  val upload_only
    :  initialize_limits:P.Initialize.Limits.t
    -> upload:Upload.t
    -> (t, P.Error.t) result

  (** Install the explicit configured read/Project contract binding into an
      existing core policy, preserving its independent supplied-document,
      document-static, assistance and protocol settings. Reject a policy that
      already has a Source origin rather than silently replacing its grant.
      [analysis_policy] is the actual finite checked policy for these Projects;
      its compilation ceiling must be admitted for the selected Standard
      contracts, including Moderator when configured. The existing
      Authoring_source_host.Context.open_root/capture and owner final checks
      remain the sole Project acquisition/currentness path. The daemon/CLI
      composition can add upload deployment policies separately. *)
  val with_configured_projects
    :  t
    -> bound:Authoring_configured_project_policy.t
    -> source_limits:Authoring_source_host.Limits.t
    -> analysis_policy:Chatml_authoring_analysis.Policy.t
    -> (t, P.Error.t) result

  (** Add exactly one configured upload owner to an existing core policy,
      preserving its native/supplied/semantic/assistance families. Reject an
      already installed upload owner; an upload-only policy is built by
      [upload_only] instead. This pure composition grants no source read. *)
  val with_configured_upload : t -> upload:Upload.t -> (t, P.Error.t) result

  (** Install one real semantic sibling on the same core/source policy after
      finite lower admission. This is used by configured daemon composition;
      missing core/source origin or a second semantic sibling rejects. *)
  val with_semantic : t -> semantic:Semantic.t -> (t, P.Error.t) result

  (** Recheck an already built profile against the daemon's current Initialize
      ceiling before configured contract admission or child opening. *)
  val validate_initialize : t -> P.Initialize.Limits.t -> (unit, P.Error.t) result

  (** [core=false] permits a genuine upload-only owner. Semantic, lexical and
      assistance need core to create/edit contexts, while bundle needs its own
      upload owner and feature. [core=true] needs at least one actual source,
      supplied-document or uploaded-project origin. An omitted source never
      borrows a native root. Each supplied lower limit has its own existing
      constructor; this admission checks the advertised Authoring request
      ceiling against [initialize_limits] before any resource is opened.
      Lower owners enforce their own finite caps at use; a host policy cannot
      widen them. No independent
      parser, compiler, scheduler, context ledger or upload writer is created. *)
  val create
    :  core:bool
    -> source:Source.t option
    -> upload:Upload.t option
    -> supplied_document:Service.Supplied_document_policy.t option
    -> selection:Service.Selection_policy.t
    -> context_limits:Chatml_authoring_context.Limits.t
    -> analysis_policy:Chatml_authoring_analysis.Policy.t
    -> protocol_limits:P.Authoring.Limits.t
    -> service_limits:Service.Limits.t
    -> semantic:Semantic.t option
    -> assistance:Assistance.t option
    -> initialize_limits:P.Initialize.Limits.t
    -> (t, P.Error.t) result
end

type t

(** Comparison only with the exact physical source host supplied at
    installation, including an upload-only installation. This does not lend a
    Root or grant a native read. *)
val issued_by_source_host : t -> Project_source_host.t -> bool

module Origins : sig
  type t

  (** Comparison-only facts about constructed context owners. These do not
      lend a Root, select a source, or authorize an actor. *)
  val host_source : t -> bool

  val uploaded_project : t -> bool
  val supplied_document : t -> bool
end

(** [None] once the installation is closing or closed. Each available origin
    requires installed core context support and its actual child/policy:
    [host_source] requires Source_host; [uploaded_project] requires Upload_host
    with a rebind policy; [supplied_document] requires its installed policy.
    Upload-only publication does not make an uploaded Project executable.
    Capture still performs the original actor, Root and source checks. *)
val installed_origins : t -> Origins.t option

(** Revalidate [policy]'s advertised request ceiling against the CURRENT
    daemon [initialize_limits] at every installation. A policy made using
    default limits cannot silently widen a later options override. This
    check and exact physical [Project_source_host] identity check for a
    configured-project binding precede opening any optional source/upload
    owner. Equal root names, paths or list positions cannot substitute for
    the bound host.
    Open only the opted-in owners. The durable upload store comes from the
    SAME live Session_store daemon lock/root through its narrow opener, then
    transfers exclusive lifetime to Upload_host. Source_host borrows the
    already-opened Project_source_host. Both outlive the one Service owner.
    A failed/cancelled construction joins already opened children in reverse
    order without exposing a partial feature inventory. *)
val create
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> project_sources:Project_source_host.t
  -> store:Agent_store.Session_store.t
  -> policy:Host_policy.t
  -> initialize_limits:P.Initialize.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) result

(** Exact family set derived from constructed handlers. These are available
    host features for Server_info and Initialize intersection, not a claim
    that a client selected each one. Codec-supported method names never enter
    this inventory. *)
val available_features : t -> string list

(** Borrow the exact existing joined Service owner as a Developer
    capture facade. No second source/context owner is constructed. Closed
    installation rejects; a missing source route or selected Standard contract
    fails the relevant later preparation, not a synthetic constructor grant.
    The facade remains bound to this installation's original actor/source
    checks and expires when the installation closes; policy and limits cannot
    widen its admitted source grants. *)
val developer_capture
  :  t
  -> policy:Service.Developer.Target_policy.t
  -> limits:Service.Developer.Limits.t
  -> (Service.Developer.t, P.Error.t) result

(** Select opaque current Roots only through this installed Source_host's
    existing joined root enumeration and original/current actor read policy.
    Wire root indices remain comparison data, not grants. The returned list
    may omit denied configured roots; callers match each Root.index to the
    requested configured index and never renumber or use List.nth. A missing
    source owner, denied index or closed installation rejects; no root is
    opened from a client path. Developer command callers invoke this only after Host's
    authoritative historical-key miss. *)
val developer_roots
  :  t
  -> actor:Operator_authorization.t
  -> checkpoint:(unit -> unit)
  -> (Authoring_source_host.Root.t list, P.Error.t) result

(** The sole typed command route. Capabilities is admitted through the same
    joined Service request lifetime and lists only installed methods callable
    under this connection's current selected feature epoch. Bundle-only has
    no position encodings. Other variants dispatch to the actual same-owner
    core, bundle, semantic, lexical or assistance handler; an uninstalled or
    unselected family rejects before any source, upload or compiler work.
    Lower finite errors use the existing path-free protocol projection. *)
val handle
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Wire.Request.t
  -> (P.Authoring.Wire.Response.t, P.Error.t) result

(** Call only after constructing a successful Initialize response. Delegate
    to Service's existing serialized revocation/cancel/join transition;
    Connection_context mints the new version/selected-feature epoch only after
    old contexts and active work are gone. A failed response computation never
    enters this transition. No raw wire lease label is accepted. *)
val publish_initialize_and_wait
  :  t
  -> connection:Connection_context.t
  -> response:P.Initialize.Response.t
  -> (unit, P.Error.t) result

(** Transport close joins the exact physical connection before unrelated
    session detach. A failed join retains owner custody for daemon retry; it
    cannot be silently discarded or followed by borrowed-owner release.
    Unexpected exceptions/cancellation keep original identity/backtrace. *)
val close_connection_and_wait : t -> Connection_context.t -> (unit, P.Error.t) result

(** Daemon shutdown joins Service first, then Upload_host and Source_host;
    caller may release Project_source_host and Session_store only after Ok.
    Failed cleanup retains the still-owned children for a later retry. *)
val close_and_wait : t -> (unit, P.Error.t) result
