# Tools – built-ins, agent tools, shell wrappers & MCP

This page documents **tool calling** in ochat/ChatMD: how you declare tools in a prompt, what built-ins ship with ochat, and how to extend capabilities via **agent tools**, **shell wrappers**, and **MCP** (Model Context Protocol).

Tools are **opt-in**: the model can only call what your prompt declares via `<tool .../>`.

---

## Quick start: declare tools in ChatMD

```xml
<!-- Built-ins -->
<tool name="apply_patch"/>
<tool name="read_dir"/>
<tool name="read_file"/> <!-- alias: get_contents -->
<tool name="webpage_to_markdown"/>

<!-- Shell wrapper -->
<tool name="git_status" command="git status" description="Show git status"/>

<!-- Agent tool -->
<tool name="triage" agent="prompts/triage.chatmd" local/>

<!-- MCP tool catalog -->
<tool mcp_server="stdio:npx -y brave-search-mcp"/>
```

---

## Built-in tools

### Recommended core set (start here)

This set covers most real-world sessions (codebase navigation, retrieval, and safe edits):

- **`apply_patch`** – atomic multi-file edits in a structured patch format.
- **`read_file`** *(declare as `read_file` or `get_contents`)* – safe file reads with truncation + optional offset.
- **`read_directory`** *(declare as `read_dir`)* – list directory entries without guessing paths.
- **`webpage_to_markdown`** – ingest web pages and GitHub blob URLs as Markdown.
- **`index_markdown_docs` + `markdown_search`** – semantic search over project Markdown docs.
- **`index_ocaml_code` + `query_vector_db`** – hybrid retrieval over code indices (dense + BM25 overlay).
- **`odoc_search`** – semantic search over locally indexed OCaml docs.
- **`import_image`** – load a local image as a vision input (screenshots, diagrams).

### Built-in catalog (code-correct)

There are two names to be aware of:

1. **ChatMD declaration name**: what you write in `<tool name="…"/>`.
2. **Tool name the model sees**: what is advertised to the model and what it will call.

Some tools have **declaration aliases** for compatibility.

| ChatMD `<tool name="…"/>` | Model sees | Category | What it does |
|---|---|---|---|
| `apply_patch` | `apply_patch` | repo | Apply an atomic V4A patch (adds/updates/deletes/moves text files). |
| `read_dir` | `read_directory` | fs | List directory entries (non-recursive) as newline-delimited text. |
| `read_file` **or** `get_contents` | `read_file` | fs | Read a UTF-8 text file with truncation and optional `offset`. Refuses binary files. |
| `append_to_file` | `append_to_file` | fs | Append text to a file (inserts a newline before the appended content). |
| `find_and_replace` | `find_and_replace` | fs | Replace an exact substring in a file (single or all occurrences). |
| `webpage_to_markdown` | `webpage_to_markdown` | web | Download a page and convert it to Markdown (includes a GitHub blob fast-path). |
| `index_ocaml_code` | `index_ocaml_code` | index | Build a vector index from an OCaml source tree. |
| `query_vector_db` | `query_vector_db` | search | Hybrid retrieval over code indices (dense + BM25 overlay). |
| `index_markdown_docs` | `index_markdown_docs` | index | Index a folder of Markdown docs into a vector DB (default root: `.md_index`). |
| `markdown_search` | `markdown_search` | search | Semantic search over Markdown indices created by `index_markdown_docs`. |
| `odoc_search` | `odoc_search` | docs | Semantic search over locally indexed odoc docs. |
| `meta_refine` | `meta_refine` | meta | Recursive meta-prompt refinement flow. |
| `import_image` | `import_image` | vision | Load a local image file and return a vision input item (data URI). |
| `fork` | `fork` | misc | Reserved name; currently a placeholder tool (do not rely on it). |

#### Built-in behavior notes (practical gotchas)

- **Naming/aliases**:
  - declaring `<tool name="read_dir"/>` exposes a tool the model calls as `read_directory`.
  - declaring `<tool name="get_contents"/>` exposes a tool the model calls as `read_file`.
- **`read_file` truncation**: reads up to ~380,928 bytes and appends `---` + `[File truncated]` when it stops early.
- **`read_file` binary refusal**: binary-like content is rejected to avoid polluting context.
- **`append_to_file` always appends** (it does not deduplicate).
- **`find_and_replace` with `all=false` and multiple matches** returns an error string advising to use `apply_patch`.

#### Library-only helpers (not mountable as ChatMD built-ins by default)

ochat’s OCaml library contains additional tool implementations (e.g. `mkdir`, `get_url_content`, `add_line_numbers`), but they are **not exposed via `<tool name="…"/>`** unless you:

- add them to the built-in dispatcher, or
- expose them via an MCP server, or
- register them directly when embedding ochat as a library.

---

## High-signal ingestion: `webpage_to_markdown`

`webpage_to_markdown` is designed for “read it once, reason on it immediately” workflows.

Highlights:

- Converts generic HTML pages into Markdown.
- Special-cases **GitHub blob URLs**:
  - automatically fetches from `raw.githubusercontent.com`
  - respects line anchors like `#L10-L80`
  - returns code slices wrapped in fenced blocks with line numbers
- Caches results for a short TTL to make repeated calls to the same URL fast.

Example:

```xml
<tool name="webpage_to_markdown"/>
```

---

## Agent tools – turn prompts into callable sub-agents

Agent tools mount a `*.chatmd` prompt as a callable tool. This is the fastest way to build repeatable “mini workflows” without writing code.

```xml
<!-- Local agent prompt (relative to the prompt directory) -->
<tool name="triage" agent="prompts/triage.chatmd" local/>
```

Behavior:

- Input schema is fixed: `{ "input": "..." }`.
- The agent runs in a fresh sub-conversation (no inherited message history), but with the same execution context (filesystem root, network access, etc.).
- The tool returns the agent’s final answer as tool output.

When to use:

- Decompose complex work (triage, summarization, planning, specialized refactors).
- Keep your main conversation focused while a specialized prompt handles a subtask.

---

## ChatMD shell runtimes and tools

Shell tools bind a model-visible schema to a named, manifest-authorized
runtime. The runtime controls resolution, effects, capabilities, sandboxing,
allow/ask/deny policy, approvals, hooks, limits, secrets, and audit.

For a narrow operation, use a fixed tool:

```xml
<shell_access id="readonly" extends="builtin:workspace-readonly@1"/>
<tool name="git_status" type="shell" mode="fixed" runtime="readonly">
  <command program="git"><arg value="status"/><arg value="--short"/></command>
  <arguments mode="none"/>
</tool>
```

For general agent commands, prefer structured argv:

```xml
<shell_access id="development" extends="builtin:workspace-development@1"/>
<tool name="shell" type="shell" mode="structured" runtime="development"
      rationale="required" result="structured"/>
```

Fixed and structured model strings remain literal argv; semicolons, quotes,
substitutions, redirection characters, and spaces do not gain shell-control
meaning. Additional explicit modes are:

- `chain`: pipelines and `;`, `&&`, `||` through a conservative grammar;
- `raw`: a model-supplied script for one fixed shell executable;
- `script`: a fixed hashed script file plus literal arguments.

Unsupported structured/chain syntax is an error, never an implicit raw shell.
Legacy `<tool command="...">` declarations are desugared into fixed shell
tools and use the same centralized runtime path.

See [ChatMD shell tools](chatmd-shell-tools.md), the
[security guide](../guide/chatmd-shell-security.md), and
[worked examples](../guide/chatmd-shell-examples.md).

---

## MCP tools – import remote tool catalogs

MCP (Model Context Protocol) lets you mount tools from a remote server (stdio or HTTP). ochat turns each MCP tool into a normal function tool with the same JSON schema.

```xml
<!-- Mount a public MCP toolbox over stdio -->
<tool mcp_server="stdio:npx -y brave-search-mcp"/>

<!-- Or mount a subset from an HTTP endpoint -->
<tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
```

### Selection rules (name vs include(s))

- `name="foo"` selects a single tool and takes precedence over include(s).
- `include="a,b"` or `includes="a,b"` selects a comma-separated subset.
- If neither is present, **all tools** returned by `tools/list` are exposed.

### Connection/auth knobs

- `strict` is a boolean flag (present/absent) controlling strict parameter handling for the wrapped MCP tool.
- `client_id_env` / `client_secret_env` name environment variables whose values (if set) are injected as `client_id` / `client_secret` query params in the MCP server URI.

### Caching and refresh

ochat caches MCP tool catalogs per server for a short TTL to avoid repeated `tools/list` calls. If the server emits `notifications/tools/list_changed`, ochat invalidates the cache and refreshes on the next access.

---

## Running ochat’s MCP server (share tools + “prompt-as-tool”)

ochat includes an MCP server executable that exposes a small default set of tools and can also publish `*.chatmd` prompts as tools.

Key behavior:

- Registers a few built-in tools (including patching, directory listing and file reading, prompt refinement, and web ingestion).
- Scans a prompts directory (default `./prompts`, or `$MCP_PROMPTS_DIR`) and registers every `*.chatmd` file as:
  - an MCP **prompt**, and
  - an agent-backed MCP **tool**

This enables a practical pattern: run the MCP server inside a sandbox/container/CI runner, then mount it from your interactive session via `<tool mcp_server="…"/>`.

---

## Tool execution: parallel tool calls

ochat can execute independent tool calls in parallel (useful when a model requests multiple reads/searches).

In the TUI this is configurable:

- `--parallel-tool-calls` (default)
- `--no-parallel-tool-calls`

---

## Extending ochat with new tools (what’s actually supported)

There are multiple extension routes depending on how you want to ship capabilities:

1. **Shell wrapper tool** (`<tool command="…"/>`): fastest way to expose a narrowly scoped command.
2. **Agent tool** (`<tool agent="…"/>`): fastest way to expose a workflow encoded in ChatMD.
3. **MCP tool catalog** (`<tool mcp_server="…"/>`): best for sharing tools across environments and for sandboxing.
4. **Embedding ochat as a library**: register arbitrary `Ochat_function.t` values directly in your host program.

Important note: a plain ChatMD declaration `<tool name="…"/>` (without `command=`, `agent=`, or `mcp_server=`) is treated as a **built-in**. Unknown built-in names are rejected unless you add them to ochat’s built-in dispatcher or expose them via MCP.
