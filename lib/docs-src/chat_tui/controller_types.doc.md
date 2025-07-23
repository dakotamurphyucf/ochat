# `Chat_tui.Controller_types`

Shared reaction type used by every *controller* in the Chat-TUI code-base.

---

## 1  Module purpose

`Controller_types` exists only to declare the polymorphic variant:

```ocaml
type reaction =
  | Redraw
  | Submit_input
  | Cancel_or_quit
  | Quit
  | Unhandled
```

Placing the type in its own compilation unit avoids cyclic build
dependencies between `Chat_tui.Controller` (the main dispatcher) and the
mode-specific sub-controllers defined in the same directory.  All modules
agree on *exactly* the same variant, yet none of them has to `include` or
re-define it.

## 2  `reaction` constructors

| Variant | Meaning | Typical caller action |
|---------|---------|-----------------------|
| `Redraw` | The visible model state changed. | Invoke `Renderer.render` and refresh the Notty viewport. |
| `Submit_input` | The user finished editing the prompt (⌥⇧-Enter in normal mode). | Assemble an OpenAI request from the current input buffer, append a *pending* message bubble to the conversation view, and spawn the network fiber. |
| `Cancel_or_quit` | The *Escape* key was pressed.  If a request is running, cancel it; otherwise fall back to `Quit`. | `Eio.Switch.fail` the fetch fiber *or* terminate the app. |
| `Quit` | Immediate termination (Ctrl-C, `q`). | Cleanly shut down and exit. |
| `Unhandled` | The controller doesn’t recognise the event. | Pass the event to the next handler in the chain (global bindings, debug console, …). |

## 3  Example usage

```ocaml
let rec event_loop term model =
  match Notty_eio.Term.event term with
  | None -> ()
  | Some ev ->
    match Controller.handle_key ~model ~term ev with
    | Redraw ->
        Renderer.render ~model ~term;
        event_loop term model
    | Submit_input ->
        (* spawn_request performs the OpenAI call in a background fiber *)
        spawn_request ~model;
        Renderer.render ~model ~term;
        event_loop term model
    | Cancel_or_quit ->
        (* Implementation-specific; typically either cancel a running
           request or fall through to Quit. *)
        event_loop term model
    | Quit -> ()
    | Unhandled ->
        (* Fall back to global shortcuts *)
        event_loop term model
```

## 4  Design notes

*No extra functions.*  The compilation unit purposefully exports nothing but
`reaction` to minimise inter-module coupling.

The variant is **closed**; extending it in downstream code would require
patching the type definition here.

## 5  Limitations

The semantics of the constructors are documented here but enforced only by
convention.  Call-sites are free to ignore `Redraw` or misinterpret
`Submit_input`.  Future work might encode stricter invariants at the type
level (e.g. `Submit_input of string` containing the finalised prompt).

