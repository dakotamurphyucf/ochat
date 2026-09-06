(** Reproduce HTTP/2 unknown-length bodies forwarded over HTTP/1.1 without
    message framing, and verify the relay's framing correction against that
    negative control. Local-only; use the same Cohttp/Eio body reader as OpenAI. *)
val run : Eio_unix.Stdenv.base -> unit
