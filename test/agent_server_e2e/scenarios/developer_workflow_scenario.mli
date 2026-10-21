open Core

(** Deterministic composition of installed Authoring source revisions, explicit
    upload, Developer commands, REPL and trace over authenticated transports.
    Shared phases own independently joined configured HTTP and genuine local
    stdio hosts; neither path has provider credentials. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
