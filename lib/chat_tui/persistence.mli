(** Persistence helpers for ChatMarkdown transcripts.

    Render canonical entries and optional moderator overlays for legacy
    transcript export. Checkpoints select stable IDs and payload changes,
    not list offsets. This is not the native/daemon durable session store.
    Tool text is serialized without universal truncation, terminal sanitization
    or secret redaction; callers own export authorization and privacy policy.
    Filesystem operations use the supplied Eio directory capability. *)

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

    This reads and rewrites the complete file. It is not an atomic replacement
    or a concurrent-writer-safe operation. *)
val write_user_message : dir:Eio.Fs.dir_ty Eio.Path.t -> file:string -> string -> unit

(** [history_entries_as_chatmd ~moderator_snapshot ~history] renders canonical
    entries with a distinct [ochat-history-id] attribute. Ordinary
    ChatMarkdown remains a semantic export; the binary session snapshot is
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

(** [persist_entries ~dir ~prompt_file ~checkpoint ~moderator_snapshot ~history]
    renders new or changed entries and the supplied overlay, then rewrites the
    file with its previous text followed by that rendering. The checkpoint is
    not advanced. This is not an idempotent save, deletion synchronization,
    transactional append, or tool-output redaction boundary. *)
val persist_entries
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> prompt_file:string
  -> checkpoint:Checkpoint.t
  -> moderator_snapshot:Session.Moderator_snapshot.t option
  -> history:History_entry.t list
  -> unit
