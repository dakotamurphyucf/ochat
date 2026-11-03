(** Internal pure mutation plan over the real universal session carrier. This is
    NOT an authenticated command endpoint or proof of a source read. Native/source
    admission must be supplied by the genuine host owner before exposing it.
    Commit through Session_persistence; publish the returned state/receipt only
    after its save acknowledgement. A failed save leaves the prior basis intact.
    Before exposing activation, that owner must preflight the reserved Clear
    against the independently configured complete Transaction/Journal profile,
    including audit and previous-hash envelope. This document plan does not
    establish that separate persistence-capacity guarantee. *)
type decision =
  | Retained of Instruction_command.Receipt.t
  | Prepared of
      { transition : Session_transition.t
      ; receipt : Instruction_command.Receipt.t
      }

val prepare
  :  Session_state_document.t
  -> request:Instruction_command.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> limits:Document_schema.Limits.t
  -> (decision, Agent_protocol.Error.t) result
