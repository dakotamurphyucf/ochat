open Core

(** Runs durable replay, snapshot replacement, and subscriber backpressure E2Es.
    The boundary case holds a real HTTP attach response after replay selection,
    commits live content before releasing it, and checks the full projection.
    Reconnect checks synchronize past attachment bookkeeping with a fresh durable
    event before comparing complete authoritative snapshots. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
