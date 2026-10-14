open! Core
module Owner = Authoring_context_service_core
module Publication = Authoring_analysis_publication

(** Prepare one genuine Project-basis Analyze outside the final Owner gate.
    Uses the current admitted overlay/contract and the actual Core policy and
    cache heads. The resulting immutable candidate is not published here. *)
val prepare
  :  Owner.t
  -> Owner.Current.t
  -> env:Eio_unix.Stdenv.base
  -> build_limits:Chatml_project_build.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Publication.Prepared.t, Owner.Error.t) result
