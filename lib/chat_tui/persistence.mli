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

(** [history_entries_as_chatmd ~moderator_snapshot ~history] renders canonical
    entries with a distinct [ochat-history-id] attribute. Ordinary
    ChatMarkdown remains a semantic export; the binary V4 snapshot is
    authoritative for fields that ChatMarkdown cannot represent. *)
val history_entries_as_chatmd
  :  moderator_snapshot:Session.Moderator_snapshot.t option
  -> history:History_entry.t list
  -> string

module Checkpoint : sig
  type t

  val empty : unit -> t
  val of_entries : History_entry.t list -> t
end

(** [entries_after_checkpoint checkpoint history] selects entries by stable
    identity and payload revision rather than list position. Retained unchanged
    entries are omitted; new IDs and identity-preserving replacements are
    returned. *)
val entries_after_checkpoint
  :  Checkpoint.t
  -> History_entry.t list
  -> History_entry.t list

val persist_entries
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> prompt_file:string
  -> checkpoint:Checkpoint.t
  -> moderator_snapshot:Session.Moderator_snapshot.t option
  -> history:History_entry.t list
  -> unit
