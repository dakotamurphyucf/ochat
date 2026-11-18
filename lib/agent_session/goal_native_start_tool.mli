(** Explicitly selected native initial-episode staging registration. Full input
    is decoded under the request's bounded profile, then validated by its real
    protocol constructor. Neither local Complete nor decoded data grants an ACK,
    admission, source loan or execution permission. *)
val registration : Chat_response.Agent_runtime.native_registration
