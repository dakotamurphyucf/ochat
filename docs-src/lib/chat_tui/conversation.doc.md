# Chat_tui.Conversation — display text and stable projections

Convert the repository's `Openai.Responses.Item.t` values into display messages
and identity-bearing projected rows. These helpers perform no I/O. Tuple
conversion does not change canonical history or the content exported by
[Persistence](persistence.doc.md).

## pair_of_item <a id="pair_of_item"></a>

```ocaml
val pair_of_item : Openai.Responses.Item.t -> Types.message option
```

Return `Some (role, text)` for a supported item, or `None` when the variant
has no supported presentation.

- Input messages join text parts with newlines; image-only input yields empty
  display text.
- Assistant output joins text parts with spaces.
- Function/custom calls display `name(arguments)` or `name(input)`.
- Function/custom outputs join text and image placeholders, sanitize the text,
  and retain at most **10,000 bytes**, followed by `\n…truncated…` if needed.
- Reasoning joins summary parts with spaces.

Input text and call arguments use `Util.sanitize ~strip:true`; assistant,
reasoning and tool output use `~strip:false`. This is display sanitization,
not secret redaction. The tool-output byte truncation is not grapheme-aware.
Specialized tool rendering can use separate metadata; this tuple limit is not
a universal bound on all renderers, canonical history or exports.

A complete user-message example:

```ocaml
let hello =
  let open Openai.Responses in
  Item.Input_message
    { Input_message.role = Input_message.User
    ; content = [ Input_message.Text { text = "Hello"; _type = "input_text" } ]
    ; _type = "message"
    }

let displayed = Chat_tui.Conversation.pair_of_item hello
```

`displayed` is `Some ("user", "Hello")`.

## of_history <a id="of_history"></a>

```ocaml
val of_history : Openai.Responses.Item.t list -> Types.message list
```

Filter-map `pair_of_item` over the input. Relative order survives, but filtered
items mean **display indices do not equal original response/history indices**.
Use this helper for text conversion, not to identify a canonical deletion target.

```ocaml
let display_messages items =
  Chat_tui.Conversation.of_history items
```

## Identity-aware projections

```ocaml
val project_entries : History_entry.t list -> projection
val project_effective_entries
  :  Chat_response.Moderation.Effective_entry.t list -> projection
val rows : projection -> Projected_message.t list
val messages : projection -> Types.message list
val index_of_id : projection -> Projected_message.Id.t -> int option
```

A projection contains ordered rows and an ID-to-display-index lookup.
Canonical rows retain their entry IDs. Moderator insertions and replacements
carry explicit provenance; replacements use the target occurrence's projected
identity. Local approval/placeholder rows use separate namespaced IDs and have
no canonical entry ID.

```ocaml
let display_index entries projected_id =
  let projection = Chat_tui.Conversation.project_entries entries in
  Chat_tui.Conversation.index_of_id projection projected_id
```

Do not overwrite `Model.messages` directly to replace history: the model also
owns identity lookup, selection, caches and effective projection state.
Use the owning host's history/projection update path.

## Known limitations <a id="known-limitations"></a>

Text projection is lossy and is not a persistence format. Unsupported variants
are omitted. Byte-limited display text can omit details present in canonical
history; display sanitization does not establish export privacy.
Terminal-cell geometry belongs to the renderer, not this module.

Sources: [interface](../../../lib/chat_tui/conversation.mli),
[implementation](../../../lib/chat_tui/conversation.ml).

