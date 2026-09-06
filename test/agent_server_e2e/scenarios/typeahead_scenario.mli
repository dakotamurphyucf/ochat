(** Opt-in real PTY traces for legacy, native local and daemon typeahead.
    Only isolated loopback provider requests and disposable filesystem roots. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
