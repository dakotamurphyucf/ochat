# `Renderer_pages` — dispatching to page renderers

The TUI renderer is structured as a small page framework. `Renderer_pages`
decides which page should be rendered based on `Model.active_page` and then
delegates to a page-specific implementation.

## API

```ocaml
val page_of_model : Chat_tui.Model.t -> Chat_tui.Model.Page_id.t
val render
  :  size:int * int
  -> model:Chat_tui.Model.t
  -> Notty.I.t * (int * int)
```

Currently the only page is `Chat`, implemented by `Renderer_page_chat`.

