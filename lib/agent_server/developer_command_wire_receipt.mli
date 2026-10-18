open! Core

(** Conservative wire receipt capacity required to project a protocol-valid
    stored command receipt admitted by [stored]. A diagnostic location changes
    its flat source namespace to a nested base64 source ID. At the protocol's
    1,024-byte namespace ceiling that adds at most 362 compact JSON bytes, one
    total field, one node and one depth level. Checked arithmetic rejects
    unrepresentable limits. This is a comparison bound, not a disclosure grant. *)
val required_limits
  :  stored:Document_schema.Limits.t
  -> (Document_schema.Limits.t, Agent_protocol.Error.t) result

(** Pure compatibility check for every captured executable source ID. Check and
    Fixture diagnostic attribution can only name a matching source in this
    Prepared inventory. It grants no source authority and leaves nonportable
    Host captures unchanged; the Endpoint calls it on fresh portable misses
    before durable Reserve. *)
val validate_prepared : Chatml_prepared_program.t -> (unit, Agent_protocol.Error.t) result

(** Convert one already authorized, bounded Store command receipt into the
    closed portable Check, Fixture or Live receipt. This is a typed projection:
    it preserves diagnostic source ID, digest and half-open byte range through
    the protocol constructors, then validates the complete projected JSON
    under [limits]. It does not confer disclosure authority or retain a source.

    A retained REPL Task receipt belongs to the separate REPL wire family and
    is rejected. Invalid projection data or an exceeded portable receipt bound
    returns the protocol constructor's typed error; no JSON-shape repair or
    fallback changes the persisted outcome. *)
val project
  :  Agent_store.Developer_command_receipt.t
  -> limits:Document_schema.Limits.t
  -> (Agent_protocol.Developer_receipt.t, Agent_protocol.Error.t) result
