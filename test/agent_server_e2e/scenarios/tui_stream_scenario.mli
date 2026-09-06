(** Execute the gated reasoning/fork-progress trace in credential-private
    subprocesses over embedded, Unix and HTTP, comparing the final transcript. *)
val run : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

(** Committed ChatML overlays, deferred adoption and terminal Agent-page state,
    exercised against all three hosts with gated nested progress. *)
val overlays : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

(** Shared permission modal, approved nested execution, operation cancellation,
    successful compaction and cancelled compaction followed by another turn. *)
val approval : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

(** Disconnect an active streaming TUI, keep an independent writer alive, then
    exercise real retained replay and expired-cursor snapshot replacement over
    both daemon transports, preserving draft, selection and undo history.
    The HTTP path cuts an actual TCP relay without closing the client explicitly;
    it must detect event-stream loss and enter shared reconnect handling. The
    Unix path still explicitly closes its client connection. *)
val reconnect : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

(** Internal child entry point. Refuse an unpinned endpoint or any API key other
    than the fixture's inert [tui-local-test-key] sentinel
    before starting the host; the parent supplies an isolated cwd/environment. *)
val child : Eio_unix.Stdenv.base -> string -> unit
