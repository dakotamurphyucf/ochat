# `Responses` – OpenAI `/v1/responses` API bindings

`Openai.Responses` is the lowest-level OpenAI client used throughout the
Ochat code-base. It mirrors the `/v1/responses` schema with OCaml types
(generated via `ppx_jsonaf_conv`) and provides one function,
`post_response`, that performs the HTTPS request and decodes either:

* a single final JSON object (blocking mode), or
* a stream of Server-Sent Events (streaming mode).

If you are building an end-user app, you often want the higher-level
orchestration in `Chat_response` (tool dispatch, history management,
deterministic ordering, …). If you want full control of the wire format,
`Openai.Responses` is the right layer.

---

## Quick-start (blocking)

```ocaml
Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let dir = Eio.Stdenv.cwd env in

  let user : Openai.Responses.Item.t =
    Openai.Responses.Item.Input_message
      { role = Openai.Responses.Input_message.User
      ; content =
          [ Openai.Responses.Input_message.Text
              { text = "Tell me a joke"; _type = "input_text" }
          ]
      ; _type = "message"
      }
  in

  let ({ output; _ } : Openai.Responses.Response.t) =
    Openai.Responses.post_response Openai.Responses.Default ~dir net ~inputs:[ user ]
  in
  match output with
  | [ Openai.Responses.Item.Output_message { content = [ { text; _ } ]; _ } ] ->
    print_endline text
  | _ -> print_endline "Unexpected reply"
```

---

## API

```ocaml
val Openai.Responses.post_response :
  'a Openai.Responses.response_type
  -> ?max_output_tokens:int
  -> ?temperature:float
  -> ?tools:Openai.Responses.Request.Tool.t list
  -> ?model:Openai.Responses.Request.model
  -> ?parallel_tool_calls:bool
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> _ Eio.Net.t
  -> inputs:Openai.Responses.Item.t list
  -> 'a
```

### Parameters

* `response_type` – selects blocking vs streaming behaviour:
  * `Default` returns `Openai.Responses.Response.t`.
  * `Stream f` returns `unit` and invokes `f` for each
    `Openai.Responses.Response_stream.t` event.
* `max_output_tokens` – hard cap on assistant output tokens (default `600`).
* `temperature` – sampling temperature.
* `tools` – tool catalog visible to the model (function calling, hosted tools
  such as file/web search).
* `model` – the target model (default `Gpt4`).
* `parallel_tool_calls` – when `true`, allow the model to emit multiple tool
  calls without waiting for previous ones to complete.
* `reasoning` – optional reasoning settings (effort/summary controls).
* `dir` – directory capability used for diagnostics; raw responses are appended
  to:
  * `raw-openai-response.txt` (blocking)
  * `raw-openai-streaming-response.txt` (streaming)
* `net` – the network capability (`Eio.Stdenv.net env`).
* `inputs` – heterogeneous history (`Item.t list`) sent to OpenAI.

### Exceptions

`Openai.Responses.post_response` raises on errors rather than returning a
result:

* `Openai.Responses.Response_stream_parsing_error` if a streaming event cannot
  be decoded (carries the JSON and underlying exception).
* `Openai.Responses.Response_parsing_error` if the final JSON payload cannot be
  decoded as `Response.t`.
* Any exception coming from networking/TLS (`cohttp-eio`, `tls-eio`, `Eio`).

Wrap the call with `Io.to_res` if you want a `('a, string) result` instead.

---

## Small conversion helpers

These helpers are used pervasively by higher-level code (rendering,
configuration parsing, etc.):

* `Openai.Responses.Input_message.role_to_string` / `role_of_string`
* `Openai.Responses.Request.model_to_str` / `model_of_str_exn`
* `Openai.Responses.Request.Reasoning.Effort.to_str` / `of_str_exn`
* `Openai.Responses.Request.Reasoning.Summary.to_str` / `of_str_exn`

---

## Streaming

Streaming delivers a sequence of `Openai.Responses.Response_stream.t` events.
For “live typing”, handle `Output_text_delta`:

```ocaml
let on_event = function
  | Openai.Responses.Response_stream.Output_text_delta { delta; _ } ->
    Out_channel.output_string stdout delta
  | _ -> ()
in

Openai.Responses.post_response
  (Openai.Responses.Stream on_event)
  ~dir
  net
  ~inputs
```

Event ordering is determined by the server. If you enable
`~parallel_tool_calls:true`, you must be prepared to observe interleavings
between tool call argument streams and regular text deltas.

---

## Tool calling: the “call → run → output → continue” loop

Tools are *declared* via `~tools` (what the model is allowed to call), but
tool *execution* is your responsibility.

At a high level:

1. Send `inputs` (messages + tool outputs so far).
2. Read `Function_call` items in the response.
3. Execute your local tool implementation.
4. Append `Function_call_output` with the same `call_id`.
5. Call `post_response` again with the extended history.

The Ochat code-base implements this loop in `Chat_response.Response_loop` and
`Chat_response.Driver`.

### Declaring a function tool

```ocaml
let echo_tool : Openai.Responses.Request.Tool.t =
  Openai.Responses.Request.Tool.Function
    { name = "echo"
    ; description = Some "Return the given text"
    ; parameters =
        Jsonaf.of_string
          {|{"type":"object","properties":{"text":{"type":"string"}},"required":["text"],"additionalProperties":false}|}
    ; strict = true
    ; type_ = "function"
    }
```

---

## Function-call outputs: text vs multimodal content

The `/v1/responses` schema allows tool outputs to be either:

* a plain string (`Function_call_output.Output.Text`), or
* a structured list of parts (`Function_call_output.Output.Content`) containing
  `input_text` and `input_image` entries.

This is reflected by:

* `Openai.Responses.Function_call_output.Output.t` and
* `Openai.Responses.Function_call_output.Output_part.t`.

Two helpers are particularly useful:

* `Openai.Responses.Function_call_output.Output.to_display_string` – render for
  humans (images become `<image src="..."/>`).
* `Openai.Responses.Function_call_output.Output.to_persisted_string` – preserve
  structured output by serialising to JSON when needed.

---

## Security note (TLS)

The HTTP plumbing ultimately goes through `Io.Net`, which intentionally uses a
“null” TLS authenticator for convenience in development. This disables
certificate validation.

If you are shipping production code, do not use this configuration as-is.
See `docs-src/lib/Io.doc.md` for details.

---

## Limitations / caveats

* The OpenAI endpoint schema is evolving; new event constructors may appear.
* No automatic retries/backoff.
* The blocking path materialises the whole response body in memory.
* The streaming path logs every received line; this is useful for debugging
  but may produce large log files.

---

## See also

* [`Completions`](./completions.doc.md) – legacy chat-completions wrapper.
* [`Embeddings`](./embeddings.doc.md) – `/v1/embeddings` wrapper.
* [`Chat_response.Driver`](../chat_response/driver.doc.md) – high-level
  orchestration of tool calls, streaming, and ChatMarkdown conversations.


