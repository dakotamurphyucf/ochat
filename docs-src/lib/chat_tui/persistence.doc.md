# Chat_tui.Persistence — identity-aware ChatMarkdown export

Render canonical history and optional moderator overlays as a semantic
ChatMarkdown transcript. This module supports legacy file export; it is not the
native/daemon durable session store or an automatic draft autosave service.
Filesystem operations use caller-supplied Eio directory capabilities.

## Public API

### write_user_message <a id="write_user_message"></a>

```ocaml
val write_user_message
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> file:string
  -> string
  -> unit
```

Read the file, replace a trailing empty `<user>\n\n</user>` stub when present,
or append a new user block, and save the complete text. Input is inserted
verbatim, without XML escaping or secret redaction. This is an explicit helper;
ordinary editor keystrokes do not call it.

### Canonical rendering

```ocaml
val history_entries_as_chatmd
  :  moderator_snapshot:Session.Moderator_snapshot.t option
  -> history:History_entry.t list
  -> string
```

Render each canonical entry with its `ochat-history-id`, then append the
supplied moderator overlay. Function/custom calls and outputs are inline
ChatMarkdown blocks; this API does not create numbered tool-output JSON files.
Provider item IDs and tool correlation IDs remain distinct from history IDs.

### Checkpoints and persist_entries <a id="persist_session"></a>

```ocaml
module Checkpoint : sig
  type t
  val empty : unit -> t
  val of_entries : History_entry.t list -> t
end

val entries_after_checkpoint
  :  Checkpoint.t -> History_entry.t list -> History_entry.t list

val persist_entries
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> prompt_file:string
  -> checkpoint:Checkpoint.t
  -> moderator_snapshot:Session.Moderator_snapshot.t option
  -> history:History_entry.t list
  -> unit
```

A checkpoint records stable occurrence IDs and serialized payload fingerprints.
Selection includes new IDs and changed payloads under retained IDs, omitting
unchanged entries regardless of their current list positions. Deleted entries
produce no deletion record. There is no `persist_session ~initial_msg_count`
API.

`persist_entries` renders the selected entries and the supplied overlay,
reads the existing file, strips trailing whitespace, and saves the combined
text. It logically adds content but physically rewrites the file; it is not
an atomic append. The checkpoint is not advanced, so repeating a call with the
same checkpoint can duplicate exported blocks.

## Example

Export changes since a captured canonical history. The destination must exist:

```ocaml
let persist_changes ~dir ~prompt_file ~before ~after =
  let checkpoint = Chat_tui.Persistence.Checkpoint.of_entries before in
  Chat_tui.Persistence.persist_entries
    ~dir ~prompt_file ~checkpoint ~moderator_snapshot:None ~history:after
```

To obtain a full semantic rendering without file I/O:

```ocaml
let render_history history =
  Chat_tui.Persistence.history_entries_as_chatmd
    ~moderator_snapshot:None ~history
```

## Privacy and shell security state

Exported tool text is **not universally bounded, terminal-sanitized or
secret-redacted**. This serializer preserves text supplied in canonical
function/custom-tool outputs. Some upstream tools or hosts apply their own
policies; those do not create a universal guarantee for arbitrary tool output.
Display truncation/sanitization in [Conversation](conversation.doc.md) does not
protect the exported transcript. Review exports and attachments before sharing.

ChatMarkdown does not encode all security/runtime state. The binary legacy
session snapshot remains authoritative for exact grants, shell extensions,
audit position and interrupted requests. Native/daemon exports use their own
authorized session APIs; this helper is not that authorization boundary.

## Limitations

Calls require single-writer coordination. Complete-file rewrites are neither
transactional nor crash-safe replacements and can leave partial output on failure.
A checkpoint is an export-selection aid, not full-file synchronization or an
exact snapshot round-trip. Large tool and assistant text can be written in full.

Sources: [interface](../../../lib/chat_tui/persistence.mli),
[implementation](../../../lib/chat_tui/persistence.ml),
[legacy export caller](../../../lib/chat_tui/export.ml).
