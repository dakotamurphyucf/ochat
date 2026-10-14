open! Core
module Owner = Authoring_context_service_core
module Effective = Chatml_authoring_effective

(** One private Project overlay preparation shared by semantic analysis and
    selected-catalog assistance. It consumes the current owner-scoped Inputs,
    the actual retained Host/Upload compiler contract. A deterministic bounded
    candidate scan selects a NEW generated lock ID distinct from the base
    Build's old lock and every captured Inputs source ID; no client path or
    authority chooses it. It then performs only bounded pure validation through
    [Effective.validate_overlay]: no source read, parser, native resolver,
    compiler effect or fresh authority. The enclosing Owner.with_current must
    recheck the actor/context/generation after yielding work and at its final
    gate before publishing or serving any derived fact. *)

module Error : sig
  type t =
    | Context of Owner.Error.t
    | No_selected_contract
    | Work_limit
    | Lock_identity_unavailable
    | Invalid_overlay of Chatml_project_build.Error.t
end

module Prepared : sig
  type t

  val overlay : t -> Effective.Overlay.t
  val build : t -> Chatml_project_build.t

  (** The actual Host/Upload-selected contract used to validate this exact
      overlay. Comparison and compiler input only; no authority to read or
      execute. The enclosing owner rechecks current selection before use. *)
  val contract : t -> Chatml.Chatml_compiler_contract.t
end

val prepare
  :  Owner.Current.t
  -> limits:Chatml_project_build.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Prepared.t, Error.t) result
