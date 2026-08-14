# `Response_loop` – synchronous completion loop

`Response_loop` is a utility module that repeatedly sends the current
conversation to the OpenAI *chat/completions* endpoint, executes every
tool requested by the assistant, and stops only when the reply contains
no [`Function_call`] items.  It is the glue that turns *declarative*
tool invocations emitted by the model into *imperative* OCaml calls.

Unlike {!Fork.run_stream}, this helper is **blocking** and therefore
best-suited for simple CLI applications, scripts, and unit-tests that
do not need to display partial answers while the request streams in.

---

## High-level algorithm

```text
loop with current history H
  1. R ← post_response H           (OpenAI HTTP round-trip)
  2. append R.output to H
  3. extract every Function_call fc ∈ R.output
     if none → return H            (quiescent state)
  4. for each fc:
       res ← Hashtbl.find_exn tool_tbl fc.name fc.arguments
       H ← H ⋃ Function_call_output(res, fc.call_id)
  5. repeat
```

The algorithm is pure except for the side-effects performed by the
invoked tools.

---

## API reference

```ocaml
val run_entries :
  ctx:_ Ctx.t ->
  allocator:History_entry.Allocator.t ->
  ?temperature:float ->
  ?max_output_tokens:int ->
  ?tools:Openai.Responses.Request.Tool.t list ->
  ?reasoning:Openai.Responses.Request.Reasoning.t ->
  ?fork_depth:int ->
  ?history_compaction:bool ->
  model:Openai.Responses.Request.model ->
  tool_tbl:(string, Ochat_function.runner) Hashtbl.t ->
  History_entry.t list ->
  History_entry.t list
```

### Parameters

* **ctx** – immutable execution context providing [net], [dir] and a
  shared cache.
* **temperature** – (optional) sampling temperature.
* **max_output_tokens** – (optional) per-request token budget.
* **tools** – list of tools forward-declared to the model.
* **reasoning** – whether the model should emit [`Reasoning`] blocks.
* **fork_depth** – recursion counter incremented when the loop is
  entered from a nested agent (created via the builtin `fork` tool).
* **history_compaction** – when `true`, repeated reads of the same file are collapsed so that only the most recent version is sent to the model.
* **model** – OpenAI model to call (e.g. `Gpt4`).
* **allocator** – caller-owned allocator for newly accepted occurrences.
* **tool_tbl** – mapping from tool names to invocation-aware implementations.
* **history** – canonical identity-bearing conversation so far.

### Return value

Extended canonical history that includes every assistant message and
tool output produced during the loop. Existing entries retain their IDs.

---

## Usage example

```ocaml
open Chat_response

let grep_tool (args : string) : string =
  (* custom business logic *)
  "…"

let () =
  Eio_main.run (fun env ->
    let ctx = Ctx.of_env ~env ~cache:Cache.empty in
    let allocator =
      History_entry.Allocator.create
        ~namespace:"example"
        ~next_sequence:0
      |> Result.get_ok
    in
    let final_history =
      Response_loop.run_entries
        ~ctx
        ~allocator
        ~model:Openai.Responses.Request.Gpt4
        ~tool_tbl
        initial_history
    in
    List.iter final_history ~f:(fun item ->
      (* render item *)
      ()))
```

---

## Known limitations

1. The function is blocking; it does not expose intermediate streaming
   events.
2. An ill-behaved model could generate an **infinite** chain of
   `Function_call` items.  Callers should consider adding a watchdog
   (max iterations / max latency).
3. The lookup in [tool_tbl] raises [`Not_found`] when a tool name is
   missing – wrap the call or catch the exception if the table is built
   dynamically.
4. The `history_compaction` flag performs a naïve *last-write-wins* collapse.
   Disable it if your prompt relies on previous versions of imported
   documents (for instance when computing diffs).

---

## See also

* {!Fork.run_stream} – asynchronous streaming alternative.
* {!Driver} – higher-level helper that wires the loop to the user‐side
  CLI.

## Shell tool lifecycle

Shell calls enter the same tool loop but may suspend on the approval broker.
The loop never converts approval input into conversation history. Parallel
calls can queue independent approvals. Cancellation propagates through the
owning Eio switch to approvals, hook workers, pipes, and processes. Only the
final bounded/sanitized/redacted tool result is appended as canonical output.
