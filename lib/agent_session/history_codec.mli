open! Core

(** Canonical history uses the pure neutral History_entry.Payload document.
    Provider DTO decoding occurs only at explicit runtime/presentation lowering. *)
val to_canonical
  :  ?provenance:Agent_protocol.History.provenance
  -> History_entry.t
  -> Agent_protocol.History.entry

val of_canonical
  :  Agent_protocol.History.entry
  -> (History_entry.t, Agent_protocol.Error.t) result

(** Current internal spellings are canonical aliases; they never lower DTOs. *)
val to_protocol
  :  ?provenance:Agent_protocol.History.provenance
  -> History_entry.t
  -> Agent_protocol.History.entry

val of_protocol
  :  Agent_protocol.History.entry
  -> (History_entry.t, Agent_protocol.Error.t) result

(** Temporary compatible legacy display projection. Its failure cannot make
    a neutral canonical payload invalid or rewrite captured data. *)
val to_presentation
  :  ?provenance:Agent_protocol.History.provenance
  -> History_entry.t
  -> (Agent_protocol.History.entry, Agent_protocol.Error.t) result

val canonical_encoder
  :  previous:Agent_protocol.History.entry list
  -> History_entry.t
  -> Agent_protocol.History.entry

val all_to_protocol
  :  ?previous:Agent_protocol.History.entry list
  -> History_entry.t list
  -> Agent_protocol.History.entry list

val all_of_protocol
  :  Agent_protocol.History.entry list
  -> (History_entry.t list, Agent_protocol.Error.t) result

val user_text : id:History_entry.Id.t -> string -> History_entry.t
