(** [run env environment] exercises the unmodified daemon Maintenance timer and
    registry-derived foreground protection over two real 60-second cycles.
    Start idle and permission-suspended sessions through HTTP, create exclusive
    artifacts beneath their exact durable store handles, observe idle expiration
    while the older active artifact survives, approve the tool, then observe the
    released artifact expire. Use a 100 ms response cutoff, 75-second bounded
    polling phases and a 170-second enclosing Eio deadline. No protection list,
    Maintenance.run_once call, artificial clock or timer override is injected. *)
val run : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
