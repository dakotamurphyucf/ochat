open! Core

(** Shared internal signal for a typed host-effect rejection. [Ctx.Effect_rejected]
    aliases this exact constructor. It preserves the supplied error across stream
    observer adapters without granting authority or rolling back committed facts.
    Cancellation and unexpected exceptions are distinct. *)
exception Rejected of Agent_protocol.Error.t
