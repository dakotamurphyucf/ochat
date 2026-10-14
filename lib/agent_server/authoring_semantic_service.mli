open! Core
module P = Agent_protocol
module Owner = Authoring_context_service_core

(** PRIVATE Analyze sibling sharing the sole Core context/cache owner. Prefix
    publication uses the same owner; this module installs no authority or
    parser cache. Query delivery belongs to the dependent service ticket. *)
type t

module Prefix_limits = Authoring_semantic_prefix.Limits

module Limits : sig
  type t

  (** Exact source-bundle ceilings are installed in the host/supplied parser
      BEFORE import reads and copying. The request checkpoint bounds host,
      parser, metadata and projection visits. The joined checker uses the
      separate immutable Core analysis policy's compilation/semantic limits;
      response bytes use the Core host response cap. All components remain
      bounded, but they are not one interchangeable work allowance. These
      bundle bounds cannot exceed the checked Generated defaults. *)
  val create
    :  max_document_bundle_files:int
    -> max_document_bundle_bytes:int
    -> max_parser_work_steps:int
    -> max_diagnostic_work_steps:int
    -> t Or_error.t
end

(** [document_dir] is parser display provenance, never a read grant. Selected
    Build and decoded metadata policies are trusted current host callbacks;
    they cannot fetch bytes here. The sole Core owns the analysis policy,
    actor/source grants, joined lifetime and final cache/index gate. *)
val create
  :  owner:Owner.t
  -> env:Eio_unix.Stdenv.base
  -> document_dir:Eio.Fs.dir_ty Eio.Path.t
  -> script_selection:Authoring_script_selection.Policy.t option
  -> project_metadata:Authoring_project_metadata.Policy.t option
  -> project_build_limits:Chatml_project_build.Limits.t
  -> limits:Limits.t
  -> prefix_limits:Prefix_limits.t
  -> t

(** One explicit joined Project or selected-script Document analysis. Host
    imports and separately declared project metadata are captured only here,
    never during Prefix/Query. A successful Parsed-only closure may replace
    stale checked facts while returning typed semantic Unavailable. Complete
    gets a current snapshot label; Failed retains same-run syntax/diagnostics
    without any certified snapshot row. The final gate checks current actor,
    selection, context generation and all cache/capture/index heads before a
    pure pointer swap. Unexpected exceptions and cancellation keep identity. *)
val analyze
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Analysis.Analyze.Request.t
  -> (P.Authoring.Analysis.Analyze.t, Owner.Error.t) result

(** Current parser-confirmed Project or selected-script Document prefix. A
    Document prefix reparses current root bytes only with retained immutable
    imports and maps an authored cursor through the genuine selected Script.
    Its compiler-owned graph and source remain visibly provisional in a
    separately bounded latest-per-context index. No implicit import refresh. *)
val prefix
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Analysis.Prefix.Request.t
  -> (P.Authoring.Analysis.Prefix.t, Owner.Error.t) result
