(** [run env ~case] runs an isolated loopback TCP relay for the manual HTTP
    launcher. [case] names the existing fixture launcher directory, or
    ["self-check"] for a bounded TCP forwarding/cut/resume test. The interactive
    runner checks authenticated protocol initialization through the relay before
    printing its launcher. It never prints bearer credentials or changes the
    original daemon configuration. Cut interrupts all relayed HTTP connections,
    not only SSE. No normal runtest alias invokes this operator-controlled tool. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
