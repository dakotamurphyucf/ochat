open! Core

(** Immutable result of an actual owned resource stage. A positive disposal flag
    concerns that actual reporting scope, not execution admission or repaired
    history. Constructors are confined to the concrete first-party owners. *)
type t = Disposal_report_data.t

module Failure = Agent_session.Callback_cleanup.Failure

val disposed : t -> bool
val failures : t -> Failure.t list
