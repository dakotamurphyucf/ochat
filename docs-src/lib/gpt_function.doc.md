# Ochat_function — register and observe custom OCaml tools

Use `ochat.ochat_function` to combine model-visible metadata with an OCaml
implementation. ChatMD authors normally select [built-ins](../overview/tools.md)
or [other tool kinds](../tools/README.md); this API is for library authors.

## Definition and registration

`Def` supplies a decoded input type, `name`, `type_`, optional `description`,
`parameters`, and `input_of_string`. For `type_ = "function"`, parameters describe
JSON arguments; custom tools use a format/grammar object and raw input instead.
The decoder may reject input. Registration is not local schema validation or
authorization.

```ocaml
module Echo : Ochat_function.Def with type input = string = struct
  type input = string
  let name = "echo"
  let type_ = "function"
  let description = Some "Return the supplied text"
  let parameters = Jsonaf.of_string
    {|{"type":"object","properties":{"text":{"type":"string"}},"required":["text"],"additionalProperties":false}|}
  let input_of_string input =
    Jsonaf.of_string input |> Jsonaf.member_exn "text" |> Jsonaf.string_exn
end

let echo =
  Ochat_function.create_function (module Echo)
    (fun text -> Openai.Responses.Tool_output.Output.Text text)
```

`create_function` defaults `strict` to true and forwards it as provider metadata.
It does not enforce schemas, create confinement, request approval, catch
implementation failures or add retries. The host owns these policies.
Use Core/Eio capabilities in implementations that perform I/O.

## Results and dispatch

Results are `Openai.Responses.Tool_output.Output.t`, not bare strings:

- `Text text` is ordinary textual output.
- `Content parts` carries ordered text/image parts; `import_image` uses this.

`Ochat_function.functions` returns metadata and a table of **runners**:

```ocaml
let invoke_echo () =
  let _metadata, dispatch = Ochat_function.functions [ echo ] in
  let run = Core.Hashtbl.find_exn dispatch "echo" in
  run ~invocation:Ochat_function.Invocation.silent {|{"text":"Hello"}|}
```

Each runner requires `~invocation`; alternatively `echo.run input` invokes
silently. Names must be unique: duplicates raise at table construction.
Metadata order is not a stable ordering contract for callers.

## Progress-capable tools

`create_streaming_function` passes `~invocation` to the decoded implementation.
Emit transient progress with `Invocation.emit`; return exactly one final output.
Progress does not replace that output and must not become canonical history.

```ocaml
let observed_echo =
  Ochat_function.create_streaming_function (module Echo)
    (fun ~invocation text ->
       Ochat_function.Invocation.emit invocation
         { channel = `Activity; update = Replace "Preparing response" };
       Openai.Responses.Tool_output.Output.Text text)
```

Channels are `Assistant`, `Reasoning`, `Stdout`, `Stderr`, and `Activity`.
`Append text` extends a channel; `Replace text` replaces its latest replaceable
update. Each payload must independently be valid UTF-8. This layer does not
sanitize, redact or bound custom progress: implementations must respect the
host's disclosure policy before notifying observers.

Use `Invocation.create callback` to observe progress, `Invocation.silent` to
discard it, or `create_with_trace ~progress ~trace` to also observe nested tools.
Callbacks run synchronously, must return promptly, and must be concurrency-safe
if shared across invocations. Observer exceptions are suppressed/logged by the
adapter so they do not change final output. Do not use observer exceptions
as cancellation or authorization signals.

## Nested traces

`Invocation.emit_trace` accepts `Trace.Tool_started`, `Tool_progress`, and
`Tool_finished`. Start includes call ID, name, function/custom kind and payload;
finish includes `Returned`, `Raised` or `Cancelled` and optional output. These
are transient display data, not additional authoritative tool results.
Apply the same disclosure policy as for progress. The constructors do not
automatically execute nested tools.

`Invocation.is_observed` allows skipping expensive display-only work when no
observer is installed. `run` and silent `run_with_progress` retain identical
final-result semantics.

## Runnable offline example

The [complete example](../examples/tools/custom_tool.ml) is compiled and executed
by the opt-in documentation check. It verifies silent/observed dispatch,
progress delivery, structured output, duplicate names and malformed input
without a provider key or network request:

```sh
dune exec docs-src/examples/tools/custom_tool.exe
dune build @agent-docs-check
```

The check does not execute every historical Markdown snippet. Exact types are in
the [interface](../../lib/ochat_function.mli); implementation is
[here](../../lib/ochat_function.ml). See also [packaged registrations](functions.doc.md)
and [definition catalog](definitions.doc.md).
