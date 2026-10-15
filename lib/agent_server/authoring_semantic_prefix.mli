open! Core
module P = Agent_protocol
module Owner = Authoring_context_service_core

(** PRIVATE preparation of one genuine Project or selected-script Document
    parser-confirmed prefix. It installs no source authority and publishes no
    checked result. The enclosing Semantic service owns the joined request,
    original/current actor rechecks and final cache-head CAS. *)
module Limits : sig
  type t

  (** Explicit finite host/parser work and frozen-bundle byte/file bounds.
      Bundle limits cannot enlarge Generated defaults. The joined explicit
      contract compiler still uses the SAME Core analysis policy's independent
      compilation/conversion/semantic limits. The host request allowance and
      lower worker budgets are complementary, not one fictitious counter. *)
  val create
    :  max_document_bundle_files:int
    -> max_document_bundle_bytes:int
    -> max_parser_work_steps:int
    -> max_host_work_steps:int
    -> t Or_error.t

  val max_host_work_steps : t -> int
end

type prepared

(** Project consumes the current genuine overlay and selected contract.
    Document reparses the current unsaved root and ONLY the previously captured
    immutable imports, associates the new parser inputs with the exact captured
    source owners, admits the selected Script/Build/contract, and maps an
    authored cursor via its bound extraction map. A new/missing import,
    malformed declaration, missing selected Build, transformed/external or
    ambiguous authored point returns honest Prefix.Unavailable. Neither path
    recaptures imports or guesses a compiler source from a display path.
    The lower joined explicit-contract prefix worker supplies the native
    provisional result. Any chosen label/index update is prepared outside the
    final gate; cancelled or rejected work publishes nothing. *)
val prepare
  :  Owner.t
  -> Owner.Current.t
  -> actor:Operator_authorization.t
  -> env:Eio_unix.Stdenv.base
  -> document_dir:Eio.Fs.dir_ty Eio.Path.t
  -> script_selection:Authoring_script_selection.Policy.t option
  -> project_metadata:Authoring_project_metadata.Policy.t option
  -> project_build_limits:Chatml_project_build.Limits.t
  -> limits:Limits.t
  -> P.Authoring.Analysis.Prefix.Request.t
  -> checkpoint:(unit -> unit)
  -> (prepared, Owner.Error.t) result

val response : prepared -> P.Authoring.Analysis.Prefix.t
val provisional_after : prepared -> Authoring_provisional_index.t option

(** Bounded nonyielding re-selection of exactly the originally admitted
    contract and optional metadata route after all work. Captures immutable
    comparison evidence, not the expired Current token or a source grant. *)
val final_check_now : prepared -> (unit, Owner.Error.t) result
