# Chat_tui.Renderer

Render the current `Model.t` into colourful [Notty](https://pqwy.github.io/notty/doc/notty/) images for display in a terminal. The module is the view in an Elm-like design: it is pure, reads the current model and size, and returns a new image plus the cursor position. Side effects (network, persistence, input handling) live elsewhere (see Chat_tui.Controller / Chat_tui.App).

---

## Overview

- Colour palette – `attr_of_role` maps chat roles to `Notty.A.t` attributes (assistant cyan, user yellow, tool magenta, system dim grey, etc.).
- Wrapping and sanitisation – message text is word‑wrapped on display‑cell boundaries and sanitised once with `Chat_tui.Util.sanitize` to comply with Notty’s rules for `I.string` (no C0 controls; valid UTF‑8).
- Markdown‑like rendering – fenced code blocks are recognised using `Chat_tui.Markdown_fences.split` and highlighted per line by `Chat_tui.Highlight_tm_engine.highlight_text` with `Highlight_theme.default_dark`. Each highlighted code line is drawn over a subtle tinted background that spans the body area; the tint reverses under selection. Inline code spans (single backticks) are rendered with a subtle background. Tool‑like roles ("tool", "tool_output") render the body as a single code block; if the message is just one fenced block, its language is used, otherwise a small heuristic picks `bash`, `diff`, or `json`.
- Virtualised history – `history_image` renders only the slice that can be visible in the scroll box. Per‑message images and heights are cached in the model; a prefix‑sum array enables fast range lookups.
- Whole‑screen composition – `render_full` stitches history, a status bar, and a framed multi‑line input editor into one image and computes the cursor position.

Relevant Notty primitives (see the Notty docs): `Notty.I.(hsnap, vsnap, vcat)`, `Notty.Infix.(<|>)`, and attributes from `Notty.A`.

---

## API highlights

### attr_of_role : string -> Notty.A.t

Map a role string ("assistant", "user", …) to a foreground colour. Unknown roles map to `Notty.A.empty`.

```ocaml
let cyan = Chat_tui.Renderer.attr_of_role "assistant" in
(* cyan = Notty.A.(fg lightcyan) *)
```

### is_toollike : string -> bool

Return `true` for roles that should be rendered as tool output (currently `"tool"` and `"tool_output"`). Tool‑like roles treat the entire body as a single code block. If the message is a single fenced block, its language tag is used; otherwise a small heuristic chooses `bash`, `diff`, or `json`. When no grammar is available, highlighting falls back to plain spans.

### message_to_image : max_width:int -> ?selected:bool -> Types.message -> Notty.I.t

Render a single message as an image, wrapped to `max_width` cells.

- Prefix the first row with `role: ` and indent wrapped lines.
- Keep hard newlines as paragraph breaks.
- If the message contains fenced blocks, highlight them per line; otherwise render text with inline backtick styling.
- For role `"developer"`, drop a leading `"developer:"` prefix in the text to avoid duplicated labels.
- Append one blank row as vertical spacing. When `~selected:true`, draw in reverse video on top of the base colour.

Examples

Render a simple message:

```ocaml
let img = Chat_tui.Renderer.message_to_image
  ~max_width:40 ("assistant", "Hello, how can I help you today?")
```

Render a message with fenced code and inline code:

```ocaml
let msg =
  ( "assistant"
  , "Here is some code:\n\n```ocaml\nlet x = 42\n```\nUse `x` later." )
in
let img = Chat_tui.Renderer.message_to_image ~max_width:60 msg
```

### history_image : model:Model.t -> width:int -> height:int -> messages:Types.message list -> selected_idx:int option -> Notty.I.t

Render the scroll‑box content. Only potentially visible messages are rendered; the rest is represented by transparent padding so the total height matches the full transcript. Message images and heights are cached in `Model.t` and invalidated when the terminal width changes or text updates.

### render_full : size:int * int -> model:Model.t -> Notty.I.t * (int * int)

Return the complete screen image plus cursor position `(x, y)`. The status bar shows the editor mode and appends ` -- RAW --` when the draft buffer is `Raw_xml`.

```
┌───────────────────────── history (scrollable) ─────────────────────────┐
│                                                                       │
│  assistant: Hi!                                                       │
│  user:       Can you explain…                                         │
│                                                                       │
├───────────────────────────────── status bar ───────────────────────────┤
│ -- INSERT --                                                          │
├───────────────────────────── input editor ─────────────────────────────┤
│> The current multi-line prompt…                                        │
└────────────────────────────────────────────────────────────────────────┘
```

---

## Example: draw once with Notty_eio

```ocaml
open Core

let empty_model () =
  let open Chat_tui in
  let msg_buffers : (string, Chat_tui.Types.msg_buffer) Base.Hashtbl.t =
    Base.Hashtbl.create (module String)
  in
  let fn_by_id : (string, string) Base.Hashtbl.t =
    Base.Hashtbl.create (module String)
  in
  let reasoning_by_id : (string, int ref) Base.Hashtbl.t =
    Base.Hashtbl.create (module String)
  in
  let kv : (string, string) Base.Hashtbl.t =
    Base.Hashtbl.create (module String)
  in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Chat_tui.Model.create
    ~history_items:[]
    ~messages:[ "assistant", "Welcome to ochat!" ]
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers
    ~function_name_by_id:fn_by_id
    ~reasoning_idx_by_id:reasoning_by_id
    ~tasks:[]
    ~kv_store:kv
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Chat_tui.Model.Insert
    ~draft_mode:Chat_tui.Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0

let () =
  Eio_main.run @@ fun env ->
  let input  = Eio.Stdenv.stdin  env in
  let output = Eio.Stdenv.stdout env in
  let model  = empty_model () in
  Notty_eio.Term.run ~input ~output ~on_event:(fun _ -> ()) @@ fun term ->
    let size = Notty_eio.Term.size term in
    let img, (cx, cy) = Chat_tui.Renderer.render_full ~size ~model in
    Notty_eio.Term.image term img;
    Notty_eio.Term.cursor term (Some (cx, cy));
    (* Keep the frame on screen until the process is cancelled. *)
    Eio.Fiber.await_cancel ()
```

---

## Notes and limitations

- Notty does not allow control characters or newlines in `I.string`. The renderer sanitises text once per cached render using `Util.sanitize ~strip:false` to satisfy this contract.
- Wrapping is based on display‑cell measurements; some East‑Asian wide glyphs and emoji may still be approximated depending on your terminal’s Unicode width tables (see the Notty docs).
- Inline backtick parsing can duplicate text in rare cases due to a known limitation in `Markdown_fences.split_inline`.
- `render_full` allocates a new image each call; if you need diffed updates, perform them at the application level.

---

## References

- Notty basics and composition: `Notty.I.(string, vcat, hsnap, vsnap)`, `Notty.Infix.(<|>)`, `Notty.A.(fg, bg, st, reverse)`.
- Scrolling helper: `Notty_scroll_box` from this project.

