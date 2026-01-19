# Tools – built-ins, custom helpers & MCP

This page collects the tools-related material from the README and expands on
how built-ins, shell wrappers, custom OCaml helpers and remote MCP tools fit
together in ChatMD.

---

## Built-in toolbox

| Name | Category | Description |
|------|----------|-------------|
| `apply_patch`         | repo      | Apply an *Ochat diff* (V4A) to the working tree |
| `read_dir`            | fs        | List entries (non-recursive) in a directory; returns plain-text lines |
| `get_contents`        | fs        | Read a file (UTF-8); truncates very large files and supports an optional `offset` argument |
| `get_url_content` *(experimental)* | web       | Download a raw resource and strip HTML to text *(OCaml API only; not exposed as a ChatMD `<tool>`)* |
| `webpage_to_markdown` | web       | Download a page & convert it to Markdown |
| `index_ocaml_code`    | index     | Build a vector index from a source tree |
| `index_markdown_docs` | index     | Vector-index a folder of Markdown files |
| `odoc_search`         | docs      | Semantic search over installed OCaml API docs |
| `markdown_search` / `md-search` | search | Query Markdown indexes created by `index_markdown_docs` (ChatMD uses `markdown_search`; `md-search` is the CLI wrapper) |
| `query_vector_db`     | search    | Hybrid dense + BM25 search over source indices |
| `fork`                | misc      | Reserved name for future multi-agent flows; currently implemented as a placeholder tool |
| `mkdir` *(experimental)*               | fs        | Create a directory (idempotent) *(OCaml API only; not exposed as a ChatMD `<tool>` yet)* |
| `append_to_file`      | fs        | Append text to a file, creating it if absent |
| `find_and_replace`    | fs        | Replace occurrences of a string in a file (single or all) |
| `meta_refine`         | meta      | Recursive prompt refinement utility |

<details>
<summary><strong>Deep-dive: 7 helpers that turn ChatMD into a Swiss-Army knife</strong></summary>

1. **`apply_patch`** – The bread-and-butter of autonomous coding sessions.  The assistant can literally rewrite the repository while you watch.  The command understands *move*, *add*, *delete* and multi-hunk updates in one atomic transaction.
2. **`webpage_to_markdown`** – Turns *any* public web page (incl. GitHub *blob* URLs) into clean Markdown ready for embedding or in-prompt reading.  JS-heavy sites fall back to a head-less Chromium dump.
3. **`odoc_search`** – Semantic search over your **installed** opam packages.  Because results are fetched locally there is zero network latency – ideal for day-to-day coding.
4. **`markdown_search`** – Complement to `odoc_search`.  Index your design docs and Wiki once; query them from ChatMD forever.
5. **`query_vector_db`** – When you need proper hybrid retrieval (dense + BM25) over a code base.  Works hand-in-hand with `index_ocaml_code`.
6. **`fork`** –  Reserved for future multi-agent flows.  The current implementation is a placeholder that returns a static string; treat it as experimental and do not rely on it for real workflows.
7. **`mkdir`** *(experimental)* – Exposed today via the OCaml `Functions.mkdir` helper rather than as a ChatMD `<tool>`.  You can approximate it inside ChatMD via a shell wrapper or by combining `apply_patch` with pre-created directories.

</details>

---

## Importing remote MCP tools – one line, zero friction

```xml
<!-- Mount the public Brave Search toolbox exposed by *npx brave-search-mcp* -->
<tool mcp_server="stdio:npx -y brave-search-mcp"/>

<!-- Or cherry-pick just two helpers from a self-hosted endpoint -->
<tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
```

Ochat converts every entry returned by the server’s `tools/list` call into a
local OCaml closure and forwards the **exact** JSON schema to OpenAI.  From the
model’s perspective there is no difference between `weather` (remote) and
`apply_patch` (local) – both are normal function calls.

Additional attributes on `<tool mcp_server="…"/>` let you control which tools are exposed and how the client connects:

- `name="foo"` selects a single tool by name.
- `includes="a,b"` or `include="a,b"` selects a comma-separated subset of tools; if neither `name` nor `include(s)` is present, all tools from `tools/list` are exposed.
- `strict` (boolean flag) enables stricter behaviour when calling tools; see the OCaml `Mcp_tool` docs for details.
- `client_id_env` / `client_secret_env` name environment variables whose values are injected as `client_id` / `client_secret` query parameters into the MCP server URI.

> **Tip 💡** – All built-ins are **normal ChatMD tools** under the hood.  That means you can mount them remotely via MCP:

```xml
<!-- Consume read-only helpers from a sandboxed container on the CI runner -->
<tool mcp_server="https://ci-tools.acme.dev" includes="read_dir,get_contents"/>
```

or hide them from the model entirely in production by simply omitting the `<tool>` declaration.  No code changes required.

---

## Rolling your own OCaml tool – 20 lines round-trip

```ocaml
open Ochat_function

module Hello = struct
  type input = string

  let def =
    create_function
      (module struct
        type nonrec input = input
        let name        = "say_hello"
        let description = Some "Return a greeting for the supplied name"
        let parameters  = Jsonaf.of_string
          {|{"type":"object","properties":{"name":{"type":"string"}},"required":["name"]}|}
        let input_of_string s =
          Jsonaf.of_string s |> Jsonaf.member_exn "name" |> Jsonaf.string_exn
      end)
      (fun name -> "Hello " ^ name ^ "! 👋")
end


(* Gets the tools JSON and dispatch table *)
let tools_json, dispatch =
  Ochat_function.functions [ Hello.def ]

(* If you want to add to the current drivers (Chat_tui and the chat-completion command)
 then add tool to of_declaration in lib/chat_response/tool.ml example *)
 
```

Declare it once in ChatMD:

```xml
<tool name="say_hello"/>
```

That is **all** – the assistant can now greet users in 40+ languages without
touching an HTTP stack.

---

## Shell-command wrappers – *the 30-second custom tool*

> ⚠️ **Security note** – A `<tool command="…"/>` wrapper runs the specified
> binary with the *full privileges of the current user*.  Only mount such tools
> in **trusted environments** or inside a container / sandbox.  Never expose
> unrestricted shell helpers to untrusted prompts – limit the command and
> validate the arguments instead.

Not every helper deserves a fully-blown OCaml module.  Often you just want to
gate a **single shell command** behind a friendly JSON schema so the model can
call it safely.  ChatMD does this out-of-the-box via the `command="…"`

```xml
<!-- Pure viewer: let model know do use for write access → safe in read-only environments. (note: this is just a hint to the model. It could still call this with write ops. You need to implement proper access controls in your tool) -->
<tool name="sed"
      command="sed"
      description="read-only file viewer"/>

<!-- Pre-pinned arguments – the model cannot escape the pattern.          -->
<tool name="git_ls_files"
      command="git ls-files --exclude=docs/"
      description="show files tracked by git except docs/"/>

<!-- Mutation allowed, therefore keep it explicit and auditable →        -->
<tool name="git_pull"
      command="git pull"
      description="fetch from and integrate with a remote repository"/>
```

Behaviour in a nutshell

1. The JSON schema is inferred automatically: an *array of strings* called
   `arguments`.
2. At run-time Ochat executes

   ```sh
   <command> <arguments…>   # under the current working directory
   ```

3. Standard output and stderr are captured and
   appended to the `<tool_response>` block and sent back to the assistant.  Output may be truncated and execution is subject to a timeout to avoid wedging the session.

### Why wrapper tools beat *generic shell*

| Aspect | Generic `sh -c` | Targeted wrapper |
|--------|-----------------|-------------------|
| Search space @ inference | enormous | tiny – the model only sees *git_pull* / *sed* |
| Security                 | needs manual sandboxing | limited to pre-approved binaries |
| Reliability              | model must remember *all* flags | happy-path baked into `command` |

In practice:

* **Generalist agents** benefit from one broad hammer such as `bash`, but may
  waste tokens debating which flag to use or which command to run.
* **Specialist agents** (e.g. *CI fixer*, *release-bot*) shine when equipped
  with *exactly* the verbs they need — nothing more, nothing less.

#### Design guidelines

1. **Prefer idempotent actions**.  Read or list before you write or delete.
2. **Embed flags** that should never change directly in `command="…"`.
3. Add a verb-based **prefix** (`git_`, `docker_`, `kubectl_`) so the
   language model can reason via pattern matching.

