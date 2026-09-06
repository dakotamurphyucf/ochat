(** Run all ten automated E2E-25 checks explicitly: six projection/action traces
    and four real-PTY journeys. Human terminal usability and E2E-26 load/soak
    remain separate; this runner is not part of normal [runtest]. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
