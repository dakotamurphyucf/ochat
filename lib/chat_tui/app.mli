(** [run_chat ~env ~prompt_file] starts the interactive Chat-TUI session.

    The function returns only after the user has exited the UI.  Any
    conversation updates that happened during the session are written back
    to the [prompt_file]. *)
val run_chat : env:Eio_unix.Stdenv.base -> prompt_file:string -> unit -> unit
