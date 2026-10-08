(** Trusted local provider operator CLI. Raw provider keys are accepted only by
    descriptor-validated private file, never command arguments or RPC. *)
val command : Core.Command.t
