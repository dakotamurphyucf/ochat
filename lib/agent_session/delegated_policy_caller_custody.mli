(** Actor-private implementation; add to Dune private_modules. The public
    facade aliases only this abstract t. None of Host, issuer or raw Admission
    appears in the public facade's value signatures. No codec or restoration. *)
module Owner : sig
  type t =
    | Operation of Agent_protocol.Id.Operation.t
    | Invocation of Agent_protocol.Id.Invocation.t
    | Moderator_event of Agent_protocol.Id.Moderator_execution.t
  [@@deriving equal, sexp]
end

type t

val check_current : t -> (unit, Agent_protocol.Error.t) result
val close : t -> unit

module Host : sig
  (** Nonyielding factual association by actual child Actor issuers. [domain]
      is the actual Actor domain and is checked before any capture is created.
      The Actor checks it before reading its own state/admission. The actual
      admission Result is frozen: an Error is retained, never changed to None.
      Capturing calls neither [check_current] nor original permission checks, so
      issuing a transport does not veto unrelated factual callback completion.
      At use, the closure pins SAME physical execution/source epoch and performs
      no Actor/Store RPC or fresh admission. Never supplied by request DATA. *)
  val capture
    :  issuer:unit ref
    -> domain:int
    -> session_id:Agent_protocol.Id.Session.t
    -> generation:int
    -> owner:Owner.t
    -> admission:(Execution_custody.Admission.t option, Agent_protocol.Error.t) result
    -> governance:(Delegated_policy_governance_data.t, Agent_protocol.Error.t) result
    -> check_current:(unit -> (unit, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  (** Trusted negative restriction for the actual lexical Native borrow. Domain
      is checked first; no guard is invoked here. Retains SAME issuer, original,
      owner and live Atomic, so closing either view expires both. The additional
      nonyielding check cannot extend ownership or replace original custody. *)
  val restrict
    :  t
    -> check_current:(unit -> (unit, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  (** Actual child Actor consumes against its OWN issuer and current request.
      Model call entry/reservation correlation remains existing Factory logic. *)
  val check_owner
    :  t
    -> issuer:unit ref
    -> session_id:Agent_protocol.Id.Session.t
    -> generation:int
    -> owner:Owner.t
    -> (unit, Agent_protocol.Error.t) result

  (** Return the SAME frozen association Result after actual child owner and
      original-current checks. No mutable State, source lookup, RPC or new
      permission is stored here. Restriction preserves this payload unchanged. *)
  val governance
    :  t
    -> issuer:unit ref
    -> session_id:Agent_protocol.Id.Session.t
    -> generation:int
    -> owner:Owner.t
    -> (Delegated_policy_governance_data.t, Agent_protocol.Error.t) result

  (** Uses strict existing Admission.transfer after currentness. Explicit genuine
      resumed-Run origin None remains None; an actual admission whose custody is
      None remains Some SAME admission. Unavailable/error is never converted to
      None; the Actor's source-specific issuer preserves that negative fact. *)
  val transfer
    :  t
    -> (Execution_custody.Admission.t option, Agent_protocol.Error.t) result
end
