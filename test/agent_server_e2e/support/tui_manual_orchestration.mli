(** [configure fixture] installs an isolated moderator and nested prompt. A Go
    schedule spawns one durable model job; delivery and later Wake schedules
    append recognizable effective-history notices without requesting root turns. *)
val configure : Config_fixture.t -> unit

(** [self_check ~sw env fixture provider manual] verifies a running job survives
    closing its creating client, is delivered once, and leaves its moderator
    available for another scheduled event. Requires a fresh gated provider.
    This is a fixture check, not a human TUI or all-clients-disconnected result. *)
val self_check
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> Tui_stream_provider.t
  -> Tui_manual_provider.t
  -> unit
