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

(** Find an already committed matching output, or determine the missing output's
    kind from its canonical call. Reject mismatched outputs and intervening call
    ID reuse. Never reruns handlers or guesses using a provider ID alone. *)
val recover_output
  :  history:Agent_protocol.History.entry list
  -> Agent_protocol.Invocation.t
  -> ( [ `Missing of Agent_protocol.Invocation.call_kind | `Existing of History_entry.t ]
       , Agent_protocol.Error.t )
       result

(** Check retained receipt payloads during snapshot/journal validation. History
    compaction may remove either occurrence; the receipt still prevents replay. *)
val validate_retained
  :  history:Agent_protocol.History.entry list
  -> Agent_protocol.Invocation.t
  -> (unit, Agent_protocol.Error.t) result
