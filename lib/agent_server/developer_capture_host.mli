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
          -> context:P.Authoring.Context.t
          -> current:Ledger.Inputs.t
          -> Selected_contract.t option)
    -> document_project:
         (Operator_authorization.t
          -> context:P.Authoring.Context.t
          -> current:Ledger.Document.Inputs.t
          -> parsed:Prompt.Chat_markdown.Parsed.t
          -> declaration:Prompt.Chat_markdown.Declaration.t
          -> build:Chatml_project_build.t
          -> metadata:Sources.Document_context.Project_capture.t option
          -> Selected_contract.t option)
    -> document_source:
         (Operator_authorization.t
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

type t

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
