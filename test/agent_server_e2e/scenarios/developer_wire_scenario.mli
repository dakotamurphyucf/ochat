open! Core

(** Qualify installed Developer methods over the production HTTP and Unix socket
    servers against one in-process daemon. The Unix server uses the same stdio
    envelope dispatcher, while client relay parity belongs to OCH-426. This uses
    real authenticated Authoring context/source custody and a pure Check; it
    never dispatches inference or native effects. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
