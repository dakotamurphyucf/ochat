open! Core
open Authoring_context_service_core

(** Pure bounded projections and CAS comparison helpers shared by the two
    method families. No source IO, policy callback or gate wait here. *)
val protocol_error : ('a, P.Error.t) result -> ('a, Error.t) result

val source_error : ('a, 'e) result -> ('a, Error.t) result
val ledger_error : Ledger.Error.t -> Error.t
val check_snapshot : t -> connection_entry -> Ledger.t -> (unit, Error.t) result

val root_for
  :  connection_entry
  -> P.Id.Authoring_root.t
  -> (Sources.Root.t, Error.t) result

val portable_member : string -> (Source.Portable_path.t, Error.t) result

val identity_for
  :  t
  -> connection_entry
  -> P.Id.Authoring_context.t
  -> (Ledger.Identity.t, Error.t) result

val project_selection
  :  P.Authoring.Context.Selection.t
  -> Chatml_project.Selection.request

val editable_local_sources : Build.t -> Captured.Id.t list

val selected_sources
  :  Ledger.t
  -> Ledger.Identity.t
  -> checkpoint:(unit -> unit)
  -> (Captured.t list, Error.t) result

val captured_source : Captured.t list -> Captured.Id.t -> (Captured.t, Error.t) result
