(** Canonical occurrence checks for extension tool results. Provider call IDs
    alone are not unique. These checks grant no execution authority. *)

val validate_call
  :  history:Agent_protocol.History.entry list
  -> Agent_protocol.Invocation.t
  -> (unit, Agent_protocol.Error.t) result

(** Validate the exact JSON outcome envelope and provider call ID. The outcome
    must already have passed schema validation and disclosure policy. *)
val validate_output
  :  Agent_protocol.Invocation.t
  -> History_entry.t
  -> (unit, Agent_protocol.Error.t) result

(** Validate a newly published result against its retained canonical call and
    output occurrences, including intervening reuse of the provider call ID. *)
val validate_publication
  :  history:Agent_protocol.History.entry list
  -> Agent_protocol.Invocation.t
  -> (unit, Agent_protocol.Error.t) result

(** Check retained receipt payloads during snapshot/journal validation. History
    compaction may remove either occurrence; the receipt still prevents replay. *)
val validate_retained
  :  history:Agent_protocol.History.entry list
  -> Agent_protocol.Invocation.t
  -> (unit, Agent_protocol.Error.t) result
