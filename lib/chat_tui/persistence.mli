(** Persistence helpers for ChatMarkdown transcripts.

    This module is responsible for keeping the *disk* representation of a chat
    session in sync with the in-memory conversation state.  All file I/O –
    writing user input, appending assistant responses, and off-loading bulky
    tool call payloads – funnels through the two functions below.  The helper
    sticks to the {{!module:Eio}Eio} capability style: callers must pass an
    explicit directory capability instead of relying on ambient authority. *)

(** [write_user_message ~dir ~file msg] updates the *last* [`<user>`] element
    of the ChatMarkdown document [file].

    If the transcript already ends with an {i empty} user stub – the pattern
    shown below – the stub is **replaced** in-place:

    {v
    <user>

    </user>
    v}

    Otherwise the function simply *appends* a new block at EOF.  In both cases
    the written XML fragment follows exactly this layout (final newline
    included):

    {v
    <user>
    $msg
    </user>
    v}

    where [$msg] is the verbatim content of [msg].  The helper never strips or
    escapes the text – callers are expected to sanitise user input up-front if
    necessary.

    The operation is atomic with respect to the underlying [Eio.Path] flow
    returned by {!Eio.Path.with_open_out}. *)
val write_user_message : dir:Eio.Fs.dir_ty Eio.Path.t -> file:string -> string -> unit

(** [persist_session ~dir ~prompt_file ~datadir ~cfg ~initial_msg_count
    ~history_items] appends every *new* item in [history_items] to the
    transcript file [prompt_file] located under [dir].

    "New" means entries whose list index is {>=} [initial_msg_count].  Earlier
    items are assumed to be already present on disk.

    {1 Serialisation rules}

    • *User / assistant / tool* messages become their corresponding
      ChatMarkdown blocks (`<user>…`, `<assistant>…`, `<tool_response>…`).

    • *Function / tool calls* (variants
      {!Openai.Responses.Item.Function_call} and
      {!Openai.Responses.Item.Function_call_output}) are handled in two
      mutually exclusive ways depending on [cfg.show_tool_call]:

      – When [true], the full JSON arguments / result is embedded inline
        between `RAW|` pipes so human readers can expand the details without
        leaving the file.

      – When [false] (the default) the payload is stored in a separate file
        `{N}.{call_id}.json` inside [datadir] – typically
        [$HOME/.chatmd] – and referenced through a `<doc src="./.chatmd/..."`>
        tag.  This keeps the main transcript readable even when the tool
        exchanges multi-kilobyte JSON blobs.

    • *Reasoning summaries* become `<reasoning>` blocks with nested
      `<summary>` children.

    The helper is {b append-only}: it never rewrites or deletes existing data
    and therefore preserves the original chronological order of the
    conversation.

    {1 Example}

    {[
      (* Append assistant response and tool output to the transcript. *)
      Chat_tui.Persistence.persist_session
        ~dir:(Eio.Stdenv.cwd env)
        ~prompt_file:"prompt.chatmd"
        ~datadir:(Io.ensure_chatmd_dir ~cwd:(Eio.Stdenv.cwd env))
        ~cfg
        ~initial_msg_count:List.length already_serialised
        ~history_items:new_history
    ]}

    @param cfg The active configuration record controlling formatting
           choices.  Only [show_tool_call] is inspected.
    @raise Failure Never – errors are reported synchronously via the Eio
           exception hierarchy. *)
val persist_session
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> prompt_file:string
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> cfg:Chat_response.Config.t
  -> initial_msg_count:int
  -> history_items:Openai.Responses.Item.t list
  -> unit
