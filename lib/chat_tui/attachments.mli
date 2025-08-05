(** Utilities for copying *.chatmd* attachment directories when exporting
    a conversation.

    The helper is used by both the interactive TUI export and the
    command-line `--export-session` path, ensuring consistent behaviour
    and avoiding code duplication. *)

(** [copy_all ~prompt_dir ~cwd ~session_dir ~dst] copies the directory
    tree named [.chatmd] from each of the supplied locations into
    [dst].

    Missing sources are ignored, and directories are merged in the
    order [prompt_dir], [cwd], [session_dir] (later files overwrite
    earlier ones). *)
val copy_all
  :  prompt_dir:Eio.Fs.dir_ty Eio.Path.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> session_dir:Eio.Fs.dir_ty Eio.Path.t
  -> dst:Eio.Fs.dir_ty Eio.Path.t
  -> unit
