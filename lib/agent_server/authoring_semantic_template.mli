open Core
module Owner = Authoring_context_service_core
module Ledger = Chatml_authoring_context
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract

(** Prepare one optional checked example outside the Core publication gate.
    The returned immutable cache successor replaces the borrowed head only in
    the existing Analyze CAS. Any opted-in compiler/recomposition failure
    aborts Analyze; no ordinary result or partial witness is published. *)
val project
  :  Owner.Current.t
  -> inputs:Ledger.Inputs.t
  -> receipt:Chatml_authoring_effective.Overlay.t
  -> contract:Contract.t
  -> requested:bool
  -> limits:Authoring_checked_template.Limits.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (Authoring_checked_template_cache.t, Owner.Error.t) result

val document
  :  Owner.Current.t
  -> inputs:Ledger.Document.Inputs.t
  -> script:Chatmd_authoring_analysis.Script.t
  -> selected:Build.t
  -> contract:Contract.t
  -> requested:bool
  -> limits:Authoring_checked_template.Limits.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (Authoring_checked_template_cache.t, Owner.Error.t) result

val retire
  :  Owner.Current.t
  -> identity:Ledger.Identity.t
  -> (Authoring_checked_template_cache.t, Owner.Error.t) result
