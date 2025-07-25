# `Chat_tui.App` — orchestration layer of the terminal UI

`Chat_tui.App` is the **engine room** of the Ochat TUI: it boots the
terminal, keeps the event-loop spinning, streams assistant replies and—when
everything is said and done—persists the session to disk so you can pick up
the conversation later.

The module was carved out of the old `bin/chat_tui.ml` monolith and lives in
`lib/` so that

* tests can spawn the UI without forking a process, and
* future GUI front-ends (e.g. a web UI) can reuse the same controller and
  streaming logic.

---

## Table of contents

1. [Boot-sequence – `run_chat`](#run_chat)
2. [Synchronous helpers](#sync_helpers)
3. [Asynchronous helpers](#async_helpers)
4. [Prompt context](#prompt_context)
5. [Known limitations](#limitations)

---

## 1  Boot-sequence – `run_chat` <a id="run_chat"></a>

```ocaml
val run_chat :
  env:_ Eio.Stdenv.t ->
  prompt_file:string ->
  unit ->
  unit  (** never returns *)
```

`run_chat` is the **only** function you need to call from your executable. It

1. parses the prompt file (`*.chatmd`) and converts it to
   [`Openai.Responses.Item.t`] values,
2. initialises a fresh [`Model.t`](model.doc.md) containing those items, and
3. enters the main **event-loop**:

```text
┌─────────────────────┐     push     ┌──────────────────┐
│ Notty_unescape.event│ ───────────▶ │ Controller.handle│
└─────────────────────┘              └──────────────────┘
           │                                  │ patches
           │                                  ▼
           │                 ┌──────────────────────────┐
           └───────────────▶ │  Model.apply_patches     │
                             └──────────────────────────┘
                                              │ redraw
                                              ▼
                             ┌──────────────────────────┐
                             │   Renderer.render_full   │
                             └──────────────────────────┘
```

### Usage example

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Chat_tui.App.run_chat ~env ~prompt_file:"examples/hello.chatmd" ()
```

The function blocks until the user exits the UI (either via `Ctrl-C` or by
pressing `q` / `Esc`).  On termination it prompts whether the finished
conversation should be exported and, if confirmed, writes a consolidated
`*.chatmd` file next to the prompt.

---

## 2  Synchronous helpers <a id="sync_helpers"></a>

### `add_placeholder_thinking_message`

```ocaml
val add_placeholder_thinking_message : Model.t -> unit
```

Pushes an assistant message containing the literal text
`"(thinking…)"`. The placeholder is removed as soon as the first streaming
token arrives.  Adding it synchronously after ⏎ provides immediate feedback
even on high-latency connections.

### `apply_local_submit_effects`

```ocaml
val apply_local_submit_effects :
  dir:#Eio.Fs.dir ->
  env:_ Eio.Stdenv.t ->
  cache:Chat_response.Cache.t ->
  model:Model.t ->
  ev_stream:_ Eio.Stream.t ->
  term:Notty_eio.Term.t ->
  unit
```

Performs UI updates that take effect *instantly* when the draft is submitted
but do **not** depend on network IO:

1. move the prompt text into the history (plain or raw XML),
2. clear & reset the draft buffer,
3. scroll the view port to the bottom, and
4. issue a `Redraw` event.

---

## 3  Asynchronous helpers <a id="async_helpers"></a>

### `handle_submit`

```ocaml
val handle_submit :
  env:_ Eio.Stdenv.t ->
  model:Model.t ->
  ev_stream:_ Eio.Stream.t ->
  prompt_ctx:prompt_context ->
  unit
```

Runs inside its own `Switch.run` and kicks off the OpenAI completion stream
via `Chat_response.Driver.run_completion_stream_in_memory_v1`.  All
callbacks (`on_event`, `on_fn_out`, …) forward data to the main loop by
enqueuing `Stream` events on `ev_stream`.

Failure handling:

* rolls back dangling reasoning or function-call stubs,
* restores the history to the last consistent prefix, and
* displays the exception in-line as an *error* message.

---

## 4  Prompt context <a id="prompt_context"></a>

```ocaml
type prompt_context = {
  cfg      : Chat_response.Config.t;
  tools    : Openai.Responses.Request.Tool.t list;
  tool_tbl : (string, string -> string) Hashtbl.t;
}
```

When a `*.chatmd` file declares

```xml
<tool name="weather" description="Returns the current temperature"/>
```

`run_chat` converts the declaration into a [`Tool.t`] value and stores it in
`prompt_context.tools`.  At runtime the assistant may respond with a tool
call — the implementation is looked up in `tool_tbl` and executed, the
return value is fed back into the stream as a `Function_call_output` item.

---

## 5  Known limitations <a id="limitations"></a>

| Limitation | Impact | Workaround |
|------------|--------|-----------|
| UTF-8 cursor positions are **byte-based** | Multi-byte glyphs break left/right navigation | none (yet) |
| Placeholder "(thinking…)" is not removed if the request fails before any item is streamed | Cosmetic / might confuse users | upcoming PR will reuse the error-handling path |
| No hard separation between *model* and *view* | Renderer mutates scroll-box for auto-follow | will be fixed once the Elm-style refactor is finished |



