open! Core

(** Lossless conversion between canonical response history and protocol
    transcript entries. The protocol payload stores the complete response
    item JSON so a durable session can rebuild the model input after restart. *)

val to_protocol
  :  ?provenance:Agent_protocol.History.provenance
  -> History_entry.t
  -> Agent_protocol.History.entry

val of_protocol
  :  Agent_protocol.History.entry
  -> (History_entry.t, Agent_protocol.Error.t) result

val all_to_protocol : History_entry.t list -> Agent_protocol.History.entry list

val all_of_protocol
  :  Agent_protocol.History.entry list
  -> (History_entry.t list, Agent_protocol.Error.t) result

val user_text : id:History_entry.Id.t -> string -> History_entry.t
