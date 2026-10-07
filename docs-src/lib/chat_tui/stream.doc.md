# `Chat_tui.Stream` — project neutral live drafts

`Chat_tui.Stream` admits `Transcript.Stream.t` events into a bounded draft and
projects parent Chat rows. Provider decoding belongs to the selected inference
adapter. Draft rows are presentation state; they do not append canonical history.

## Draft admission

`create ()` creates an empty draft with limits of 256 scopes, 4,096 items,
16,384 parts, 256 unknown events and 64 MiB of retained data, together with the
shared transcript document limits. `apply` returns an updated draft or an
admission error. Callers retain the previous value when admission fails.

`rows` projects the module's draft; `rows_of_draft` projects an existing
`Transcript.Draft.t`, such as a validated client projection. The module does not
apply these rows to `Model.t` or own a render loop.

## Root and nested presentation

Parent Chat rows include only actual Root scopes. Known items use
`Conversation.draft_row`; unknown Root events retain a sanitized evidence row.
Their local identities include the actual scope and original unknown-event
index, so filtering nested events does not renumber Root evidence.

Nested items and unknown events remain in the underlying draft. Owned tool and
agent activity presents readable nested progress separately; opaque nested
evidence is not converted into raw Agent-page text. Updating an activity channel
preserves its first-seen position.

A provider terminal is not a host history commit. `remove_committed` retires only
Root items carrying the actual committed `History_entry.Id.t`; it neither removes
nested occurrences with a similar identifier nor publishes their contents into
the parent conversation. The postcommit owner remains responsible for canonical
history and its stable row identities.

See [stream application](app_stream_apply.doc.md) and
[agent event application](agent_event_apply.doc.md) for the corresponding TUI
integration boundaries.

## Public contract

[Interface](../../../lib/chat_tui/stream.mli) ·
[implementation](../../../lib/chat_tui/stream.ml)

The following excerpt is the current callable contract.

```ocaml
(** Bounded neutral standalone drafts. No provider decoding and no canonical
    append: only the actual postcommit callback owns writable history. *)
type t

val create : unit -> t
val apply : t -> Transcript.Stream.t -> (t, string) result

(** Parent Chat rows include only actual Root scopes, including unknown evidence.
    Nested scopes remain in the underlying Draft; owned Agent activity presents
    their readable progress. This does not claim opaque nested evidence is
    rendered as activity text. *)
val rows : t -> Projected_message.t list

val rows_of_draft : Transcript.Draft.t -> Projected_message.t list

(** Retire only the actual committed root host occurrence; nested drafts remain
    presentation-only and cannot enter the parent canonical list. *)
val remove_committed : t -> History_entry.Id.t -> t
```
