open! Core

(** Qualify installed Authoring, Developer and REPL methods over production HTTP
    and Unix socket servers with owned in-process daemons. The Unix server uses
    the production stdio envelope dispatcher. Configured startup checks remain
    distinct from the embedded native-authorization barrier used to qualify
    saturation, cancellation and disconnected waiters. Successful qualification
    cancels the held native calls before their effects execute; inference is
    disabled. Client relay parity is qualified separately. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
