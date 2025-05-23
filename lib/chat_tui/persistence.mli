(** ChatMarkdown and transcript persistence helper functions.  All file IO
    related to the conversation buffer lives here. *)

(** [write_user_message ~dir ~file msg] appends or replaces the trailing user
    <msg> block in [file] with [msg]. *)
val write_user_message : dir:Eio.Fs.dir_ty Eio.Path.t -> file:string -> string -> unit

(** [persist_session ~dir ~datadir ~cfg ~initial_msg_count ~history_items]
    serialises all new messages that were added after [initial_msg_count]
    to the ChatMarkdown file [dir/file].  Large tool call arguments / results
    are, depending on [cfg.show_tool_call], stored in separate JSON files
    inside [datadir] (usually ~/.chatmd) and referenced via <doc src="…" local>.
  *)
val persist_session
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> prompt_file:string
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> cfg:Chat_response.Config.t
  -> initial_msg_count:int
  -> history_items:Openai.Responses.Item.t list
  -> unit
