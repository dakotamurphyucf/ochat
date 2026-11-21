(** Metadata-only exact checks to use under the physical source lease after
    an admitted immutable read. These predicates convey no source authority. *)
open! Core

module P = Agent_protocol

val check
  :  visible:(Session_state.t -> bool)
  -> checkpoint:(unit -> unit)
  -> Session_state.t
  -> source_revision:int64
  -> P.Recovery_window.Citation.t
  -> (Session_state.t -> bool, Recovery_source_reader.error) result
