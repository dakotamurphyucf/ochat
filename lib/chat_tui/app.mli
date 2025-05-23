(** [run_chat ~env ~prompt_file] starts the interactive Chat-TUI session.

    The function returns only after the user has exited the UI.  Any
    conversation updates that happened during the session are written back
    to the [prompt_file]. *)
val run_chat
  :  env:
       < cwd : Eio.Fs.dir_ty Eio.Path.t
       ; fs : Eio.Fs.dir_ty Eio.Path.t
       ; net : [> [> `Generic ] Eio.Net.ty ] Eio.Resource.t
       ; process_mgr : [> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t
       ; stdin : [> Eio_unix.source_ty ] Eio.Resource.t
       ; stdout : [> Eio_unix.sink_ty ] Eio.Resource.t
       ; .. >
  -> prompt_file:string
  -> unit
  -> unit
