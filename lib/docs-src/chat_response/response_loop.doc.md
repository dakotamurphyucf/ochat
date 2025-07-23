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
val run :
  ctx:_ Ctx.t ->
  ?temperature:float ->
  ?max_output_tokens:int ->
  ?tools:Openai.Responses.Request.Tool.t list ->
  ?reasoning:Openai.Responses.Request.Reasoning.t ->
  model:Openai.Responses.Request.model ->
  tool_tbl:(string, string -> string) Hashtbl.t ->
  Openai.Responses.Item.t list ->
  Openai.Responses.Item.t list
```

### Parameters

* **ctx** – immutable execution context providing [net], [dir] and a
  shared cache.
* **temperature** – (optional) sampling temperature.
* **max_output_tokens** – (optional) per-request token budget.
* **tools** – list of tools forward-declared to the model.
* **reasoning** – whether the model should emit [`Reasoning`] blocks.
* **model** – OpenAI model to call (e.g. `Gpt4`).
* **tool_tbl** – mapping from tool names to implementations. **Must**
  contain `"fork"` ↦ {!Fork.execute}.
* **history** – full conversation so far.

### Return value

Extended conversation that includes every assistant message and
[`Function_call_output`] produced during the loop.

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
    let tool_tbl = String.Table.of_alist_exn
      [ "grep", grep_tool ;
        "fork", Fork.execute ]
    in
    let final_history =
      Response_loop.run
        ~ctx
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

---

## See also

* {!Fork.run_stream} – asynchronous streaming alternative.
* {!Driver} – higher-level helper that wires the loop to the user‐side
  CLI.

