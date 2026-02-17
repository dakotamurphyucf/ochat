# `Renderer_component_status_bar` — one-line mode indicator

`Chat_tui.Renderer_component_status_bar` renders a single-row bar showing:

- editor mode (`-- INSERT --`, `-- NORMAL --`, `-- CMD --`), and
- draft mode hint (`-- RAW --` when `Model.draft_mode = Raw_xml`).

When a type-ahead completion exists and is relevant (see
`Model.typeahead_is_relevant`), the bar also appends a fixed hint string
describing the type-ahead key bindings:

`[Tab accept all] [Shift+Tab accept line] [Ctrl+Space preview] [Esc dismiss]`

## API

```ocaml
val render : width:int -> model:Chat_tui.Model.t -> Notty.I.t
```

The returned image is padded to `width`.

