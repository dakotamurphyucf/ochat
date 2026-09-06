# `Renderer_component_status_bar` — one-line mode indicator

`Chat_tui.Renderer_component_status_bar` renders a single-row bar showing:

- editor mode (`-- INSERT --`, `-- NORMAL --`, `-- CMD --`) or the search query;
- draft mode hint (`-- RAW --` when `Model.draft_mode = Raw_xml`);
- connection phase when present (connected, reconnecting attempt, disconnected,
  or failure code);
- independent animated agent activity: Thinking, Writing, Working, or Compacting;
- independent typeahead status, such as `[suggesting]` or the one-time sanitized
  `[typeahead unavailable]` notice.

Suggesting appears during actual requests in manual and auto modes, not during
auto debounce. It can coexist with agent activity; completion/cancellation clears
it through the UI adapter. A narrow terminal can crop later fields.

When a type-ahead completion exists and is relevant (see
`Model.typeahead_is_relevant`), the bar also appends a fixed hint string
describing the type-ahead key bindings:

`[Tab accept all] [Shift+Tab accept line] [Ctrl+Space preview] [Esc dismiss]`

## API

```ocaml
val render : width:int -> model:Chat_tui.Model.t -> Notty.I.t
```

The returned image is cropped/padded to exactly nonnegative `width`. Hints never
add a row or change the editor geometry.
