(** [test env environment] crashes a child during an actual tool side effect,
    checks the acknowledged message and incomplete tool intent, then restarts
    twice with the same executable/provider available. Require one marker,
    no automatic provider call, and no active/deferred operation after recovery.
    Decode the stopped journals to require exactly one durable interruption
    event for the original operation across both restarts. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
