open! Core
module P = Agent_protocol
module Owner = Authoring_context_service_core

(** PRIVATE read-only Query sibling over the one current Core ledger, checked
    caches and immutable provisional index. It does not parse, compile,
    recapture an import, own a second graph or publish a replacement row. *)

module Limits : sig
  type t

  (** The host admission allowance covers cache/index/source comparisons,
      selected-script reauthorization, pure selected-entry recomposition and
      site callback setup. [projection] is the existing lower query producer's
      separate aggregate item/edge/display/work allowance. Both are fixed by
      the host, never chosen by a wire request. The already installed Core
      response cap is checked before final publication. *)
  val create
    :  max_host_work_steps:int
    -> projection:Chatml_authoring_query.Limits.t
    -> t Or_error.t
end

type prepared

val response : prepared -> P.Authoring.Analysis.Query.t

(** Nonyielding comparison of the SAME original selected Build/contract and
    independently admitted project-metadata route, if a selected Document
    result was used. Core repeats context/source/actor checks and all five
    cache/capture/index heads around this callback. No expired Current token,
    parser, filesystem read or yielding checkpoint is retained here. *)
val final_check_now : prepared -> (unit, Owner.Error.t) result

(** The request's closed evidence form chooses exactly one current branch:
    Certified requires exact owner-local index label, Capture, graph and a
    matching lower Complete Snapshot; Syntax_only requires same-run Failed
    Partial and cannot yield semantic names/types; Provisional requires the
    exact current prefix row, full/prefix source proof and selected contract.
    Client labels grant nothing. A selected Document branch uses the actual
    retained Script/Parsed/declaration, current Document Inputs and captured
    input-source associations, then reselects the original host Build and
    verifies its bounded pure edited-entry recomposition against the stored
    lower stamp. Project uses the current admitted project cache and owner
    selection. Site projection preserves native ownership and the parser's
    exact/approximate/external distinction. The existing lower query producer
    owns bounded traversal and wire payload construction; this layer admits
    the resulting complete response under the host byte ceiling. Cancellation
    and unexpected exceptions propagate unchanged. *)
val prepare
  :  Owner.t
  -> Owner.Current.t
  -> actor:Operator_authorization.t
  -> script_selection:Authoring_script_selection.Policy.t option
  -> project_metadata:Authoring_project_metadata.Policy.t option
  -> project_build_limits:Chatml_project_build.Limits.t
  -> max_document_body_bytes:int
  -> limits:Limits.t
  -> P.Authoring.Analysis.Query.Request.t
  -> checkpoint:(unit -> unit)
  -> (prepared, Owner.Error.t) result
