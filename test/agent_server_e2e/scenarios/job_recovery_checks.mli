(** [run env environment] checks durable retry deadlines, attempt limits, late
    terminal-result rejection and delivery retention across two daemon reopens.
    Jobs enter through the actor API; controls/readback use production HTTP and
    scheduling uses the real daemon. Invalid recipes fail before any provider IO. *)
val run : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
