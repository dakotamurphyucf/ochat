# Chat_tui.Types – Shared Data Types

This document complements the inline `odoc` comments and provides the human-oriented reference for the **Ochat Terminal UI** code-base.

While the implementation follows an Elm-style _model–view–update_ architecture, the individual OCaml compilation units are kept very small to avoid circular dependencies.  **`Types`** is the single place in which foundational data structures live so that all other modules can reference them without linking heavy libraries or higher-level concepts.

---

## 1 Chat Transcript Helpers

| Type | Purpose |
|------|---------|
| `role` | Alias for `string`. Indicates who authored a chat message. Valid values: `"system"`, `"user"`, `"assistant"`, `"function"`. |
| `message` | Tuple `(role * string)` representing the role and markdown content of one message. |

### Example – constructing a minimal transcript

```ocaml
open Chat_tui.Types

let seed : message list =
  [ "system",    "You are a helpful assistant." ;
    "user",      "Hello!" ]
```

---

## 2 Streaming Buffer

```ocaml
type msg_buffer = {
  text  : string ref;  (* Accumulating partial output            *)
  index : int;         (* Position in Model.messages to update   *)
}
```

The record is created **when** the first delta of a streaming OpenAI response arrives.  `text` grows in-place until the HTTP connection closes, at which point the accumulated string replaces `Model.messages.(index)`.

### Usage sketch

```ocaml
let buffers : (string, msg_buffer) Hashtbl.t = Hashtbl.create 8

let ensure_buffer ~id ~role ~messages =
  match Hashtbl.find_opt buffers id with
  | Some b -> b
  | None ->
      let index = List.length !messages in
      messages := !messages @ [ role, "" ];
      let buf = { text = ref "" ; index } in
      Hashtbl.add buffers id buf;
      buf
```

---

## 3 Commands (`cmd`)

Elm followers will recognise `cmd` as the escape hatch for **impure** effects.  The controller layer decides *what* needs to happen, `cmd` carries the _thunk_, and a small interpreter (`Cmd.run` elsewhere in the code-base) performs the side-effect in an Eio fibre.

| Constructor | Semantics |
|-------------|-----------|
| `Persist_session   of (unit -> unit)` | Spawn a fibre that writes the current conversation to disk, cloud, … |
| `Start_streaming   of (unit -> unit)` | Launch an OpenAI streaming request. |
| `Cancel_streaming  of (unit -> unit)` | Abort the request started above. |

The indirection through `unit -> unit` keeps the variant free of heavy types (`Eio.Path.t`, `Persistence.config`, …) and therefore free of dependency cycles.

### Example – emitting a command from the controller

```ocaml
let submit (state : Model.t) ~(run : cmd -> unit) () =
  let start () = Openai.Stream.request ~prompt:state.input_line () in
  run (Start_streaming start)
```

---

## 4 Patches (`patch`)

`patch` values describe **pure** transformations of `Model.t`.  They allow the renderer, persistence layer, and controller to evolve independently because each side only observes the *intent* (append text, insert message, …) instead of directly mutating shared state.

| Constructor | Effect on the model |
|-------------|--------------------|
| `Ensure_buffer        { id; role }` | Guarantee that a `msg_buffer` exists for `id`; append an empty placeholder message if necessary. |
| `Append_text          { id; role; text }` | Append `text` to the streaming buffer `id` (allocate lazily) **and** reflect the change in `Model.messages`. |
| `Set_function_name    { id; name }` | Remember which tool/function call is responsible for buffer `id`. |
| `Set_function_output  { id; output }` | Store the raw return value of the function call. |
| `Update_reasoning_idx { id; idx }` | Track the last reasoning summary index emitted for buffer `id` so the UI can insert line breaks neatly. |
| `Add_user_message     { text }` | Insert the user’s prompt into `history_items` **and** `messages`. |
| `Add_placeholder_message { role; text }` | Display a transient placeholder such as “(thinking…)”. Not persisted. |

### Example – applying a patch

```ocaml
let apply (model : Model.t) = function
  | Append_text { id; text; _ } ->
      let buf = Hashtbl.find model.msg_buffers id in
      buf.text := !(buf.text) ^ text;
      let role, _ = List.nth model.messages buf.index in
      model.messages <- List.mapi model.messages ~f:(fun i msg ->
        if i = buf.index then role, !(buf.text) else msg)
  | _ -> ()
```

---

## 5 Runtime Settings

```ocaml
type settings = {
  parallel_tool_calls : bool;
}

val default_settings : unit -> settings
```

`settings` groups user-togglable flags that influence runtime behaviour.  At
present the record contains a single field:

| Field | Effect |
|-------|--------|
| `parallel_tool_calls` | When `true` the assistant may ask for multiple tool/function calls in one turn.  Each call is executed concurrently using `Eio.Switch`.  Disable the flag while debugging or when using a model that does not yet support OpenAI's *parallel tool calls* feature. |

Retrieve the defaults:

```ocaml
let cfg = Chat_tui.Types.default_settings ()
(* val cfg : Chat_tui.Types.settings = { parallel_tool_calls = true } *)
```

---

## 6 Known Limitations

* `role` is a plain `string`, therefore invalid values are not enforced at compile-time.
* `msg_buffer.text` mutates in-place; callers must be cautious when sharing the reference across fibres.
* `cmd` carries raw `(unit -> unit)` thunks — error handling and resource management must be implemented by the interpreter.

---



