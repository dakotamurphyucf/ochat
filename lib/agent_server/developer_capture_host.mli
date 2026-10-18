open! Core

(** One bounded, immutable developer capture prepared through the installed
    Source_host or the SAME authoring Core owner. This module is private to the
    host; no candidate, digest, project label or contract grants execution.
    The actual Root/Store admission still owns its provisional slot and claim. *)
module P = Agent_protocol

module Capture = Chatml_developer_capture
module Contract = Chatml.Chatml_compiler_contract
module Sources = Authoring_source_host
module Ledger = Chatml_authoring_context

module Limits : sig
  type t

  (** Positive per-attempt source/identity/work, overlay Build, and origin
      ceilings. Lower Source_host, upload, ledger, Build and JSON caps remain
      binding; this owner never resets a lower attempt or grants more bytes. *)
  val create
    :  max_prepared_input_bytes:int
    -> max_document_origin_bytes:int
    -> max_editor_versions:int
    -> build:Chatml_project_build.Limits.t
    -> t Or_error.t
end

module Selected_contract : sig
  type t

  (** Admit only an actual [Authoring_standard_contracts.Entry] from the
      already bounded configured Standard cache. It must be Standard
      One_off_v1. Reuse that entry's canonical fingerprint/encoded-byte summary;
      no second target conversion, identity encoder or cache is introduced.
      Neither the entry nor this comparison wrapper grants execution. *)
  val of_standard_entry : Authoring_standard_contracts.Entry.t -> (t, P.Error.t) result

  (** Comparison-only selected Developer_tool_v1 identity from the same
      bounded Standard cache. This constructor does not infer a tool fixture,
      grant Model access, or bypass physical source selection. The retained
      contract and canonical fingerprint are checked again after waits. *)
  val of_standard_developer_tool_entry
    :  Authoring_standard_contracts.Entry.t
    -> (t, P.Error.t) result

  (** Trusted policy pairs an actual configured Standard Tool_v1 entry with its
      selected top-level tool label and already compiled input/output schemas.
      These are not inferred from source text or an ordered Tool.call fixture.
      The name is at most 256 bytes and the two already bounded 1 MiB schemas
      are charged before canonical identity retention. Their identity
      participates in [same_identity], so
      post-wait/final current selection cannot substitute a different policy. *)
  val of_standard_tool_entry
    :  Authoring_standard_contracts.Entry.t
    -> tool_name:string
    -> input_schema:Chatmd_shell_spec.Tool_schema.t
    -> output_schema:Chatmd_shell_spec.Tool_schema.t
    -> (t, P.Error.t) result

  module Tool_fixture : sig
    type t

    val name : t -> string
    val input_schema : t -> Chatmd_shell_spec.Tool_schema.t
    val output_schema : t -> Chatmd_shell_spec.Tool_schema.t
  end

  val contract : t -> Contract.t

  (** Canonical selected Standard entry identity; Tool_v1 also binds the
      declared label and exact input/output schema identities. Comparison
      data only, never authority without the issuing Source_selection. *)
  val fingerprint : t -> Chatml_source.Digest.t

  val tool_fixture : t -> Tool_fixture.t option
  val same_identity : t -> t -> bool
end

module Request : sig
  type t

  (** Bounded inert input for Check or deterministic fixture. This constructor
      charges JSON admission under [checkpoint], and has no file/owner/target
      effects. The original immutable JSON and fixture objects are still
      revalidated by Capture.create_prepared against exact Prepared. *)
  val create
    :  entrypoint:string
    -> target:Contract.Target.t
    -> input:Jsonaf.t
    -> fixtures:Capture.Fixture.t option
    -> json_bounds:Capture.Json_bounds.t
    -> checkpoint:(unit -> unit)
    -> (t, P.Error.t) result
end

module Target_policy : sig
  type t

  module Project_origin : sig
    type t =
      | Host_project of Sources.Root.t
      | Uploaded_project
  end

  module Document_origin : sig
    type t =
      | Host_document of Sources.Root.t
      | Supplied_document
  end

  (** All callbacks are trusted bounded NONYIELDING reads of already admitted
      policy/catalog state. Arguments are comparison data, never source or
      execution grants. They return only previously admitted Selected_contract
      values, or None. Each route reselects original and current actors after
      waits and at final publication, comparing the EXACT admitted identity;
      a newly authorized different target/tool/schema identity is stale. *)
  val create
    :  local_project:
         (Operator_authorization.t
          -> root:Sources.Root.t
          -> project:Source.Portable_path.t
          -> selection:Chatml_project.Selection.request
          -> Selected_contract.t option)
    -> local_source:
         (Operator_authorization.t
          -> root:Sources.Root.t
          -> member:Source.Portable_path.t
          -> Selected_contract.t option)
    -> authoring_project:
         (Operator_authorization.t
          -> origin:Project_origin.t
          -> context:P.Authoring.Context.t
          -> current:Ledger.Inputs.t
          -> Selected_contract.t option)
    -> document_project:
         (Operator_authorization.t
          -> origin:Document_origin.t
          -> context:P.Authoring.Context.t
          -> current:Ledger.Document.Inputs.t
          -> parsed:Prompt.Chat_markdown.Parsed.t
          -> declaration:Prompt.Chat_markdown.Declaration.t
          -> build:Chatml_project_build.t
          -> metadata:Sources.Document_context.Project_capture.t option
          -> Selected_contract.t option)
    -> document_source:
         (Operator_authorization.t
          -> origin:Document_origin.t
          -> context:P.Authoring.Context.t
          -> current:Ledger.Document.Inputs.t
          -> parsed:Prompt.Chat_markdown.Parsed.t
          -> declaration:Prompt.Chat_markdown.Declaration.t
          -> script:Chatml_source.t
          -> Selected_contract.t option)
    -> t
end

module Local_project : sig
  type t

  (** Root and dependency roots are actual Sources.Root values from the same
      physical Source_host; the requested project/selection do not grant a
      route. The host uses Source.Context.open_root/capture with exactly these
      roots and the explicit selected execution contract. *)
  val create
    :  root:Sources.Root.t
    -> dependency_roots:Sources.Root.t list
    -> project:Source.Portable_path.t
    -> selection:Chatml_project.Selection.request
    -> t
end

module Local_source : sig
  type t

  (** Exact portable member under an already selected Sources.Root. No
      Project/environment is invented; native read remains Source_host's
      no-follow bounded owner operation. *)
  val create : root:Sources.Root.t -> member:Source.Portable_path.t -> t
end

module Document_script : sig
  type t

  (** Selector is lookup text only; the owner resolves one actual declaration
      under the current genuine Parsed head and exact captured input/source. *)
  val create : selector:P.Authoring.Analysis.Target.selector -> t
end

type t

module Source_selection : sig
  (** The source/contract half of the existing developer capture. A REPL epoch
      and a command Candidate can use the same authenticated route, selected
      Prepared inventory, original actor and current policy checks. It is
      minted only by [prepare_source_selection], never from a digest, client
      path or caller-supplied Prepared value. It performs no command capture,
      entrypoint check, Store write or runtime effect. *)
  type t

  type checked

  val prepared : t -> Chatml_prepared_program.t

  (* [Prepared.create] guarantees that its entry ID resolves to a captured
     source. This accessor preserves that constructor invariant. *)
  val entry_source : t -> Chatml_source.t
  val selected_contract : t -> Selected_contract.t

  (** Versioned, domain-separated REPL Root identity over the exact selected
      Prepared graph and Selected_contract fingerprint. Stored once with this
      physical selection; unlike a command capture it contains no synthetic
      entrypoint/input/fixtures. Source_selection currentness still rechecks
      its original route/actor and both selected policies. The REPL View,
      Store.handle and Root.Source.Repl use these identical bytes. *)
  val repl_root_fingerprint : t -> Chatml_source.Digest.t

  (** Full original/current actor, route/source and selected-contract check.
      The returned attempt-local proof belongs to this physical selection. *)
  val check_current
    :  t
    -> actor:Operator_authorization.t
    -> checkpoint:(unit -> unit)
    -> (checked, P.Error.t) result

  (** Nonyielding final comparison after the last wait. This is source and
      compiler selection admission, not permission for a Task effect. *)
  val check_now
    :  t
    -> checked:checked
    -> actor:Operator_authorization.t
    -> (unit, P.Error.t) result

  module Shared : sig
    type checked

    (** The same full source/contract admission for a process-local owner with
        concurrent authenticated reads. A second read never revokes another
        read's proof. This does not lend Candidate's exclusive admission proof
        or bypass the final nonyielding owner/selection check. *)
    val check_current
      :  t
      -> actor:Operator_authorization.t
      -> checkpoint:(unit -> unit)
      -> (checked, P.Error.t) result

    val check_now
      :  t
      -> checked:checked
      -> actor:Operator_authorization.t
      -> (unit, P.Error.t) result
  end

  type selection = t

  module Retained : sig
    type t

    (** Called only by the serialized REPL Create owner after authentic route
        selection and before Instance publication. It fully checks the
        original physical Source_selection, transfers its Authoring pin into
        an independent Retained_selection where needed, then repeats final
        source/actor/selected-contract admission. The returned selection has
        the SAME immutable Prepared inventory, captured origin, selected
        Standard contract and REPL fingerprint, but a distinct physical owner
        whose full/final checks never fall back to the old connection pin.
        Local roots retain their existing policy checker; Uploaded retains
        the genuine Store publication pin; supplied bytes retain document
        policy. A retained selection cannot be transferred a second time.
        The caller owns release on every error/exception after this
        function returns, including failure to publish the Instance. *)
    val acquire
      :  selection
      -> actor:Operator_authorization.t
      -> checkpoint:(unit -> unit)
      -> (t, P.Error.t) result

    (** Comparison-only projection of the exact retained selection that must
        be installed in Epoch and Root.Source.Repl. It is not a grant without
        original/current actor and fresh full/final checks. *)
    val selection : t -> selection

    (** Idempotent joined release, after active interpreter/Task/Root work
        joins. Same-capture reset retains this owner unchanged. *)
    val close_and_wait : t -> unit
  end
end

(** Shared route acquisition below command-specific [Request]/[Candidate].
    Select the actual configured Standard contract for both original and
    current actors, then capture one immutable Prepared graph under existing
    source bounds. No dummy Check/Live command or entrypoint is created.
    Existing command preparers use these same kernels before building their
    Candidate; REPL selects the exact resulting graph/contract for its epoch.
    The upper caller compares its stable route with these genuine capabilities
    BEFORE invoking the corresponding operation. *)
val prepare_local_project_selection
  :  t
  -> original_actor:Operator_authorization.t
  -> actor:Operator_authorization.t
  -> local:Local_project.t
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> (Source_selection.t, P.Error.t) result

val prepare_local_source_selection
  :  t
  -> original_actor:Operator_authorization.t
  -> actor:Operator_authorization.t
  -> source:Local_source.t
  -> checkpoint:(unit -> unit)
  -> (Source_selection.t, P.Error.t) result

val prepare_authoring_project_selection
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> checkpoint:(unit -> unit)
  -> (Source_selection.t, P.Error.t) result

val prepare_document_script_selection
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> script:Document_script.t
  -> checkpoint:(unit -> unit)
  -> (Source_selection.t, P.Error.t) result

module Candidate : sig
  type t

  module Checked : sig
    (** Per-publication-attempt comparison receipt minted only by a successful
        full currentness check for this physical Candidate and exact original
        actor object. It is never a source grant or Root/Store authority. It
        cannot be reused after an uncertain admission is resumed: that attempt
        reruns [check_current] after its own final Store/worker wait. *)
    type t
  end

  val capture : t -> Capture.t

  (** Original selected contract and optional Tool_v1 schema comparison data.
      The host may inspect these only after the Candidate's full currentness
      check and must repeat its final check before a durable result write. *)
  val selected_contract : t -> Selected_contract.t

  (** Complete owner/actor/source/upload currentness and exact selected
      contract check. May yield, including Store pin verification. The already
      captured code remains immutable; this admission check is required only
      until Root publication. Never expose a borrowed Core.Current token. *)
  val check_current
    :  t
    -> actor:Operator_authorization.t
    -> checkpoint:(unit -> unit)
    -> (Checked.t, P.Error.t) result

  (** Bounded NONYIELDING final comparison of the same physical owner,
      original/current actor, context entry/expected generation, current
      target policy and retained capture/selection head. Local routes compare
      the exact physical Source_host root/dependency roots. For an Uploaded
      context the owner also compares the retained pin's fixed deadline with
      fresh trusted time and verifies the physical upload daemon is still
      open; the preceding full check alone cannot authorize publication after
      expiry. It is valid only after [check_current] for THIS attempt and must
      be invoked INSIDE the Root.Registry provisional-to-Live transition after
      its last Store wait.
      A different/stale Checked receipt rejects. No Current borrow, Eio wait
      or native read occurs here. The developer target/tool schema uses a preadmitted fixed
      identity; the existing authoring owner may separately rederive its
      bounded analysis-contract identity during its final selection check. *)
  val check_now
    :  t
    -> checked:Checked.t
    -> actor:Operator_authorization.t
    -> (unit, P.Error.t) result

  (** Root.Registry stores this exact closure in its existing provisional
      admission. Root supplies the actual requesting actor and captured program
      on each attempt; both must be physically the values bound to [t]. The
      context-opening actor is checked separately as retained owner evidence.
      Yielding full
      checks happen after Root's other Store/worker waits and return a fresh
      bounded final check for that attempt. Unknown recovery invokes the SAME
      closure with a fresh request checkpoint, never an old Checked receipt. *)
  val final_admission
    :  t
    -> actor:Operator_authorization.t
    -> capture:Capture.t
    -> checkpoint:(unit -> unit)
    -> (unit -> (unit, P.Error.t) result, P.Error.t) result
end

(** Command-only capture over the same selected source. The REPL retains the
    selection without manufacturing a command request or capture. Full source
    and current actor checks are repeated after the yielding capture. *)
val capture_selection
  :  Source_selection.t
  -> request:Request.t
  -> checkpoint:(unit -> unit)
  -> (Candidate.t, P.Error.t) result

(** Borrows the SAME Source_host and Authoring_context_service/Core owner used
    by installed authoring methods; no new opener, ledger, resolver or Store. *)
val create
  :  authoring:Authoring_context_service_core.t
  -> policy:Target_policy.t
  -> limits:Limits.t
  -> t

val prepare_local_project
  :  t
  -> original_actor:Operator_authorization.t
  -> actor:Operator_authorization.t
  -> local:Local_project.t
  -> request:Request.t
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> (Candidate.t, P.Error.t) result

(** Local preparation closes/joins its temporary Sources.Context BEFORE it
    returns Candidate. Candidate retains the authentic Source_host Root and
    dependency Root selections as comparison anchors, and later checks each
    with Sources.check_root_current. It retains captured immutable bytes, no
    live local source borrow. No captured file edit invalidates a Root after
    successful admission. *)

val prepare_local_source
  :  t
  -> original_actor:Operator_authorization.t
  -> actor:Operator_authorization.t
  -> source:Local_source.t
  -> request:Request.t
  -> checkpoint:(unit -> unit)
  -> (Candidate.t, P.Error.t) result

(** Project-basis Host or Uploaded context. Use Core.Owner.with_current for
    exact original/current actor, grant, Expected and lifecycle. A narrow
    Authoring_project_overlay.prepare_with_contract uses the genuine Inputs,
    fresh collision-checked lock and original one-budget Effective path under
    the separately selected One_off contract. Do not replace the analysis
    context's selected compiler contract. Host_project provenance takes exact
    current changed-source Versions; Uploaded_project takes the genuine
    Context_capture receipt/pin. Explicit requested environment/project must
    remain selected; never fall back to single-source implicit environment.
    A prior uploaded Build, when required for this route, must come from its
    independently authenticated parent selection. *)
val prepare_authoring_project
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> request:Request.t
  -> checkpoint:(unit -> unit)
  -> (Candidate.t, P.Error.t) result

(** Document-basis Host or Supplied context. Resolve the selected declaration
    against the retained real Parsed head and its input_sources. A genuine
    current selection row may give a Project Build; recompose its exact entry
    under the separately admitted One_off contract, preserving its selected
    project/environment. Without selected Build, only the independently
    authorized source-only policy may use Parsed_capture.script_source and
    Prepared.single_captured. Explicit project/environment requests cannot
    silently downgrade. Imports are previously captured immutable bytes;
    this path performs no implicit parser/source recapture. *)
val prepare_document_script
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> script:Document_script.t
  -> request:Request.t
  -> checkpoint:(unit -> unit)
  -> (Candidate.t, P.Error.t) result
