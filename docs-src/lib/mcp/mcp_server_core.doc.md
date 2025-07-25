# `Mcp_server_core`

In-memory registry that keeps track of all {b user-visible artefacts} during
the life-time of an MCP (Model-Context-Protocol) server instance.  The module
is intentionally small and entirely deterministic: every effect is exposed as
a pure OCaml function; side-effects are limited to updating hash tables and
invoking user-supplied callbacks.

## What lives in the registry?

| Concern | Storage | Hook fired on change |
|---------|---------|----------------------|
| Tools (metadata + handler) | `String.Table.t` | `tools_changed_hooks` |
| Prompts | `String.Table.t` | `prompts_changed_hooks` |
| Progress streams | – | `progress_hooks` |
| Structured logs | – | `logging_hooks` |
| Cancelled request IDs | `String.Hash_set.t` | – |

Transports (stdio, HTTP, WebSocket …) {i register} a single callback for each
hook and translate internal events into wire-level `notifications/*` messages.

````ocaml
(* minimal bootstrap *)

open Core
open Mcp_server_core

let registry = create ()

(* 1. Tool registration *)
let () =
  let spec : Mcp_types.Tool.t =
    { name = "demo/echo"
    ; description = Some "Echo the input JSON verbatim"
    ; input_schema = `Null
    }
  in
  let handler (payload : Jsonaf.t) = Ok payload in
  register_tool registry spec handler

(* 2. Prompt registration *)
let () =
  let prompt : Mcp_server_core.prompt =
    { description = Some "Greet the assistant"
    ; messages = `List [ `String "Hello!" ]
    }
  in
  register_prompt registry ~name:"greeting" prompt

(* 3. Logging sink *)
let () =
  add_logging_hook registry @@ fun ~level ~logger data ->
    printf "%s %s: %s\n"
      (Sexp.to_string (sexp_of_log_level level))
      (Option.value logger ~default:"<anon>")
      (Jsonaf.to_string data)

(* 4. Progress sink (could forward to SSE) *)
let () =
  add_progress_hook registry @@ fun p ->
    printf "Progress %s: %.0f%%\n"
      p.progress_token (p.progress *. 100.)

(* 5. Cancellation check inside a handler *)
let long_running_tool registry ~id _args =
  for i = 1 to 10 do
    if is_cancelled registry ~id then raise Exit;
    Unix.sleepf 0.5;
    notify_progress registry
      { progress_token = "spin"; progress = Float.of_int i /. 10.; total = Some 1.; message = None }
  done;
  Ok (`String "done")
````

### Performance characteristics

* Hash-table look-ups and updates are {i O(1)} on average.
* Hook lists are traversed {i linearly}.  Keep the number of sinks small or
  make the callbacks lightweight.

### Concurrency / Parallelism

The implementation relies on OCaml’s global runtime lock and is therefore
safe for multiple {b fibres} running in the same domain.  When using a
multi-domain runtime you must surround every public function with a mutex to
avoid data races.

### Limitations

* Hooks are executed synchronously – a slow sink blocks the caller.
* No persistence – state is lost when the process terminates.


