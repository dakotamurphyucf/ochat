open! Core

(** [from_prompt ~allocator ~ctx ~run_agent elements] converts prompt messages
    to canonical entries. Explicit [ochat-history-id] values are preserved;
    ID-less messages receive fresh IDs. Duplicate explicit IDs are rejected
    after import expansion and conversion with their expanded message indexes.
    Explicit IDs in the allocator namespace advance its next-unused sequence
    before ID-less messages are allocated. *)
val from_prompt
  :  allocator:History_entry.Allocator.t
  -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> run_agent:
       (?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
        -> ?session_id:string
        -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
        -> string
        -> Prompt.Chat_markdown.content_item list
        -> string)
  -> Prompt.Chat_markdown.top_level_elements list
  -> (History_entry.t list, string) result

(** [resume_or_materialize] returns nonempty persisted history unchanged and
    materializes the prompt otherwise. *)
val resume_or_materialize
  :  session:Session.V4.t option
  -> allocator:History_entry.Allocator.t
  -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> run_agent:
       (?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
        -> ?session_id:string
        -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
        -> string
        -> Prompt.Chat_markdown.content_item list
        -> string)
  -> Prompt.Chat_markdown.top_level_elements list
  -> (History_entry.t list, string) result
