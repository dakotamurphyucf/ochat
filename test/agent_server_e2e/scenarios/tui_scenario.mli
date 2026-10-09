(** Run eleven automated checks explicitly: six projection/action traces,
    four real-PTY journeys and durable local CLI/selection. Human terminal usability and E2E-26 load/soak
    remain separate; this runner is not part of normal [runtest]. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
