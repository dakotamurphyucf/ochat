open Core

(** Stage exact downloadable bytes below a freshly created directory. Portable
    path collisions reject before any write; files are created exclusively. *)
val stage
  :  Eio.Fs.dir_ty Eio.Path.t
  -> sources:(Source.Portable_path.t * string) list
  -> unit

(** Check one configured entry through actual On_demand Daemon preparation. The
    complete bundle must produce one Ready prompt with retained Selected programs.
    Each required project member is then omitted independently and must produce
    an Unavailable prompt, after successful server startup. Every attempt gets a
    fresh separate home/data root, preventing history/cache rejection from serving
    as missing-source evidence. Shutdown joins before restoring omitted bytes.
    Cleanup runs under cancellation protection; if checking and cleanup both
    raise, the original check exception/backtrace takes precedence. Cleanup
    failures after a successful check remain visible.

    Local-only production grants confine acquisition to the staged bundle. No
    manifest JSON resolver, source namespace reversal, model request, session,
    initializer or local TUI execution is supplied by this check. Required paths
    come from declared edges reachable by this entry's actual parser reads; those
    edges select candidates, while the production resolver proves necessity. *)
val check
  :  env:Eio_unix.Stdenv.base
  -> home_parent:Eio.Fs.dir_ty Eio.Path.t
  -> bundle:Eio.Fs.dir_ty Eio.Path.t
  -> entry:Source.Portable_path.t
  -> required:Source.Portable_path.t list
  -> unit
