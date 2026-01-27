# `Renderer_component_status_bar` — one-line mode indicator

`Chat_tui.Renderer_component_status_bar` renders a single-row bar showing:

- editor mode (`-- INSERT --`, `-- NORMAL --`, `-- CMD --`), and
- draft mode hint (`-- RAW --` when `Model.draft_mode = Raw_xml`).

## API

```ocaml
val render : width:int -> model:Chat_tui.Model.t -> Notty.I.t
```

The returned image is padded to `width`.

