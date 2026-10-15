open Core

(** Pure captured-source utilities shared by Project overlay preparation and
    selected-script/template entry replacement. They do not borrow the
    Authoring_context_service_core owner, choose host roots or grant source
    custody. Opening Jane Street [Core] above only supplies ordinary data APIs. *)

module Error : sig
  type t =
    | Invalid_selected_entry
    | Body_limit
    | Lock_identity_unavailable
    | Recomposition of Chatml_project_build.Error.t
end

(** The input source list is checkpointed once into a bounded set. A generated
    candidate is distinct from the prior lock and every captured source ID;
    at most [source_count + 2] ordinal candidates are tried. Checkpoint and
    cancellation exceptions keep their original identity. *)
val fresh_lock_id_from_sources
  :  sources:Chatml_source.t list
  -> prior_lock:Chatml_source.Id.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_source.Id.t, Error.t) result

(** [max_body_bytes] is positive and checked before hashing/copying. Matching
    exact length and digest return the original Build unchanged. Otherwise only
    the already selected [Prepared.entry] bytes are replaced; all other source
    captures, catalog routes and the old lock remain frozen. The existing
    bounded Effective.recompose validates the new complete graph. No read,
    parser, initializer or host callback runs; checkpoint exceptions propagate. *)
val recompose_entry
  :  Chatml_project_build.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> contents:string
  -> max_body_bytes:int
  -> limits:Chatml_project_build.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_project_build.t, Error.t) result
