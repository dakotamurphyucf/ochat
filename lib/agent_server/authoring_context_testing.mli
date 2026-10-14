open! Core
module P = Agent_protocol
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Service = Authoring_context_service_core

type error =
  | Stale_context
  | Unselected_feature
  | Other

(** Explicit test boundary for the authoring owner's otherwise private cache and
    capture invariants. These receipts convey no protocol/source authority and
    have no transport path. Production handlers never call this module. *)
type feature =
  | Semantic
  | Lexical
  | Assistance
  | Bundle_upload

type head =
  | Project_cache
  | Document_cache
  | Analysis_index
  | Parsed_capture

type probe
type expired_borrow

val with_publication_probe
  :  Service.t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> feature:feature
  -> checkpoint:(unit -> unit)
  -> after_borrow:(probe -> unit)
  -> final_check:(unit -> (unit, error) result)
  -> publish:(unit -> unit)
  -> (expired_borrow, error) result

(** Only the live probe can replace a physical head, under the original gate.
    Each replacement changes exactly the named head and no checked facts. *)
val replace_head : probe -> head -> (unit, error) result

val expired_borrow_rejects_inputs_index_and_capture : expired_borrow -> bool

type capture_state =
  { parsed : bool
    (** Exact association check for the root-only supplied receipt used by the
      grouped fixture; imported receipts have distinct genuine source IDs. *)
  ; source_matches : bool
  ; project_metadata : bool
  ; complete_rows : int
  }

val capture_state
  :  Service.t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> checkpoint:(unit -> unit)
  -> (capture_state, error) result

type index_view

val index_view
  :  Service.t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> checkpoint:(unit -> unit)
  -> (index_view, error) result

val row_matches
  :  index_view
  -> capture:P.Authoring.Analysis.Capture.t
  -> snapshot:P.Authoring.Analysis.Snapshot_id.t
  -> bool

val transition_index_to_current
  :  Service.t
  -> index_view
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> checkpoint:(unit -> unit)
  -> (index_view, error) result

(** Read-only preview through the SAME overlay adapter on a genuine captured
    host context and matching admitted Ledger/view. No source/root grant is
    manufactured from a wire label. *)
val preview_project_overlay
  :  context:Sources.Context.t
  -> original_actor:Operator_authorization.t
  -> ledger:Ledger.t
  -> identity:Ledger.Identity.t
  -> view:P.Authoring.Context.t
  -> analysis_policy:Chatml_authoring_analysis.Policy.t
  -> limits:Chatml_project_build.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_project_build.t * Chatml.Chatml_compiler_contract.t, error) result

module Retained_document : sig
  type t

  val prepare
    :  context:Sources.Document_context.t
    -> original_actor:Operator_authorization.t
    -> ledger:Ledger.t
    -> identity:Ledger.Identity.t
    -> view:P.Authoring.Context.t
    -> capture:Sources.Document_context.Parsed_capture.t
    -> max_sources:int
    -> max_bytes:int
    -> checkpoint:(unit -> unit)
    -> (t, error) result

  val same_head : t -> t -> bool

  val matches_inputs
    :  t
    -> ledger:Ledger.t
    -> identity:Ledger.Identity.t
    -> view:P.Authoring.Context.t
    -> checkpoint:(unit -> unit)
    -> (bool, error) result

  val recompose_current
    :  t
    -> ledger:Ledger.t
    -> identity:Ledger.Identity.t
    -> view:P.Authoring.Context.t
    -> checkpoint:(unit -> unit)
    -> (Chatmd_source_bundle.t option, error) result
end

val classify_compilation_error
  :  Chatml_compilation.Analysis_error.t
  -> [ `Resource | `Selection | `Invalid | `Internal | `Unexpected ]

val syntax_prefix_for_testing
  :  max_sites:int
  -> int list list
  -> checkpoint:(unit -> unit)
  -> int list * bool
