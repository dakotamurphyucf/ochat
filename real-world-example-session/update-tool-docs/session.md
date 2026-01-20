<config model="gpt-5.2"  max_tokens="100000" reasoning_effort="medium" show_tool_call />

<tool name="webpage_to_markdown" />
<tool mcp_server="stdio:npx -y brave-search-mcp" name="brave_web_search" />
<tool name="apply_patch" />
<tool name="append_to_file" />
<tool name="find_and_replace" />
<tool name="read_file" />
<tool mcp_server="stdio:npx @gongrzhe/server-gmail-autoauth-mcp" include="search_emails,draft_email,send_email,read_email" />

<import src="./git-tools.md" local />

<developer>
Developer: Role and Objective:
- Serve as a multi-disciplinary AI assistant dedicated to fully resolving user queries by leveraging all available tools and resources.

Instructions:
- Ensure user requests are addressed completely and accurately before ending your turn.
- Persist until the user's query is entirely solved; yield control only when finished.
- Use external tools whenever information may be outdated, unknown, or present in files; do not rely solely on internal knowledge.
- Strictly follow guidance embedded in <system-reminder> tags found in user messages and tool results. Treat their contents as internal authoritative instructions, never quoting or referencing these tags or their content in any user-facing output.
- After executing apply_patch, consider files as saved. Never instruct users to save or copy code, and avoid displaying large file contents unless explicitly requested.
- Maintain precision, safety, and helpfulness; avoid guessing or fabricating information.

Context:
- Capabilities include web search, web page-to-markdown conversion, invoke git commands, patching or inspecting files, and managing emails. Use these to gather, update, and present information as the user’s task requires.
- Scope includes: web content, local files, email interactions. Out of scope: unsupported tool use, unapproved features.

Definitions:
Anti-hallucination safeguards:
- Use only tools specified in the directives; do not invent new tools or arguments.
- Only emit tool calls with final, concrete arguments.
- Perform tool calls immediately if details are sufficient; otherwise, request informational gaps from the user rather than promising future actions.

System reminders:
- Process <system-reminder> tags that appear in messages or tool results. Always follow their guidance internally, never mentioning or referencing them externally.

Tool directives:
1. brave_web_search: For up-to-date, concise facts or links when not available in current context/files.
2. webpage_to_markdown: To convert online articles/pages to markdown (skip if markdown already exists or page is not HTML).
3. apply_patch: For creating, modifying, or removing files (use only for changes, not inspections).
4. read_file: For viewing file content (once per version, before or after updates).
5. search_emails: To locate emails by criteria, as needed.
6. read_email: To read an identified email body (once per ID per turn).
7. draft_email: For composing outbound email drafts.
8. send_email: To send a finalized/approved email draft.
9. append_to_file: To add content to a file's end (use cautiously for large files).
10. find_and_replace: To alter one or more string occurrences within a file (do not use for appending or complex patterns).
11. various git tools: For git operations as needed.

Reasoning Steps:
- Analyze user queries to determine necessity for external data, file operations, or emails.
- If current data may be incomplete, outdated, or missing, invoke appropriate tools as per directives.
- Before any significant tool call, state in one line the purpose and minimal inputs for the call.
- Use tools iteratively until all required information is obtained and verified for accuracy.
- Respond with either a tool call (with finalized arguments) or a direct answer if no further tool actions are needed.
- Only end your turn when the request is fully addressed.

Output Format:
<format>
For complex request Return exactly two top-level sections, formatted using Markdown in this order:

<complex>
### What I did:
- Use concise bullets or paragraphs to summarize actions taken.
- Cite all files modified (with path) and any external source (URL or search result ID).

### Conclusion:
- Provide the final answer, code/file locations, email confirmations, or next explicit question for the user.

Verbosity:
- Use concise summaries for standard outputs unless detailed explanations are requested.
- For code, employ high verbosity: meaningful names, comments, and readable structure.
</complex>

For simple requests Return a single top-level section formatted using Markdown. Use discrestion to determine what to output for this case.
</format>


Notes:
- Cite sources as appropriate.
- Adhere to all safety and policy guidelines.
- Be critical when the user asks for you to review or revise content. Do not hesitate to suggest improvements or alternative approaches. Critique honestly and constructively.
- You may change the output format when it enhances clarity or presentation.
- Wrap any code in your responses in code fences so the markdown renderer can highlight the syntax
- Users can override any of these instructions by providing new ones in a user message; always follow the latest user instructions.
</developer>
<user>
ok we need to update the readme for the current github project and its accompaning markdown docs linked in the readme. We are going to start with the markdown doc that is pointed to by the "Tools  builtins, custom helpers & MCP."
section links. This markdown file contains documentation for tool use in ochat. It is not fully up-to-date and has some incorrect/outdated information. We need to update:
1. to add any missing builtin tools
2. to correct any outdated or incorrect information
3. any missing information that would be helpful to include in the docs.
4. to really emphasize the most useful inbuilt tools and features and really helps sell the features for someone reviewing the docs for the first time

To help I have included a list of all relevant code files for you to review so that you have all the information you need to thouroughly update the tools doc. It is located in scratch/tool-files.md

Before we get to updating the doc first just Review all the code and return a detailed response of everything that needs to be updated/added to the tools doc so I can review first
</user>

<reasoning id="rs_0a7776dd715389aa00696eef590d708195a504cf7a2773319d">
<summary>
**Inspecting repo files**

I'm planning to inspect the repo files listed in scratch/tool-files.md. I’ll use the read_file function and maybe git ls too. First, I need to follow the instructions, starting with the purpose line before I make any significant tool calls. My first step will be to read scratch/tool-files.md, and once that's done, I’ll go through each file referenced to synthesize a list of updates needed in the documentation. Let’s get started with the tool calls!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696eef5c4fec8195abc098dbc93b71c8">
RAW|
Purpose: Read the list of relevant code files (scratch/tool-files.md) so I can review them and identify documentation updates needed.
|RAW
</assistant>


<tool_call tool_call_id="call_btY4Rdmp3hs7HvAQrr84Fyp1" function_name="read_file" id="fc_0a7776dd715389aa00696eef5c60c881959c4d439bd7d5ccb3">
RAW|
{"file":"scratch/tool-files.md"}
|RAW
</tool_call>

<tool_response tool_call_id="call_btY4Rdmp3hs7HvAQrr84Fyp1">
RAW|
The “tools” feature spans **three layers** in code:

1) **ChatMarkdown syntax & parsing**: `<tool .../>` (and `<tool_call/>`, `<tool_response/>`) are parsed into typed variants.  
2) **Tool instantiation & dispatch in the chat runtime**: ChatMarkdown tool declarations are converted into runtime `Ochat_function.t` values, bundled into OpenAI request tool descriptors, and executed
 when OpenAI returns tool calls.  
3) **MCP** (Model Context Protocol): both **client-side** consumption of remote tools (via `<tool mcp_server="..."/>`) and a **server** that exposes tools over stdio/HTTP.

Below is an exhaustive list of the **relevant code files** (within the repo scope) that contain authoritative information for updating `docs-src/overview/tools.md`.

---

## Where in the repo (files + what each contains)

### A) ChatMarkdown tool declarations and parsing (`<tool .../>`, `<tool_call/>`, `<tool_response/>`)

1) **`lib/chatmd/prompt.mli`**  
   Defines the typed representation of tools in ChatMarkdown:
   - `Chat_markdown.tool = Builtin | Custom | Agent | Mcp`
   - MCP tool fields include `mcp_server`, `strict`, `client_id_env`, `client_secret_env`, and `names` selection.  
   Evidence: tool types are defined here, including MCP tool record fields (`type mcp_tool = { names; ...; mcp_server; strict; client_id_env; client_secret_env }`). See `lib/chatmd/prompt.mli` excerpt 
showing these types.

2) **`lib/chatmd/prompt.ml`**  
   **The actual parser/normalizer for `<tool .../>` attributes** into variants:
   - Enforces mutual exclusion: `<tool>` cannot combine `command`, `agent`, `mcp_server` (hard failure).  
   - **MCP selection logic**: accepts `name="..."` OR `include="..."` OR `includes="..."`. If `name` is non-empty, it takes precedence; else it parses `include(s)` as comma-separated list; otherwise ex
poses all tools.  
   - Reads flags: `strict` is boolean presence (`Hashtbl.mem tbl "strict"`), `local` for agent tools, and env-var attribute names for MCP creds.  
   Evidence: the `<tool>` parsing branch is implemented in the `Element (Tool, attrs, _) -> ...` match; see the logic around:
   | Some _, Some _, _ | Some _, _, Some _ | _, Some _, Some _ ->
     failwith "<tool> cannot combine 'command', 'agent' and 'mcp_server' attributes."
   ...
   let strict = Hashtbl.mem tbl "strict"
   let client_id_env = Hashtbl.find tbl "client_id_env"
   let client_secret_env = Hashtbl.find tbl "client_secret_env"
   ...
   match Hashtbl.find tbl "include" with
   | Some v -> ...
   | None -> Hashtbl.find tbl "includes" ...
   (in `lib/chatmd/prompt.ml`)

3) **`lib/chatmd/chatmd_parser.mly`** *(not OCaml, but directly controls which tags exist)*  
   Grammar includes `<tool_call> <tool_response> <tool>` (found via `rg`). This matters if `tools.md` documents syntax.

4) **`lib/chatmd/chatmd_lexer.mll`** *(not OCaml, but directly controls tokens)*  
   Lexer mentions `<tool_call> <tool_response> <tool>` (found via `rg`).

---

### B) Converting `<tool .../>` declarations into runtime tools (`Ochat_function.t`) and wiring into OpenAI requests

5) **`lib/chat_response/tool.mli`**  
   The public API:
   - `of_declaration` converts a single ChatMarkdown `<tool .../>` declaration into one or more `Ochat_function.t`.
   - Notes that `<tool mcp_server="..."/>` yields *one function per remote tool*, hence list return type.  
   Evidence: module doc explicitly enumerates “four back-ends” (built-ins, shell wrappers, agent prompts, MCP).

6) **`lib/chat_response/tool.ml`**  *(core truth for “what tools exist and how they work”)*  
   This is the most important implementation file for updating `tools.md`. It contains:

   **(i) Built-in tool name → implementation mapping** (the definitive list of built-ins exposed through ChatMD `<tool name="..."/>`):  
   Evidence: `of_declaration` includes:
   | CM.Builtin name ->
     (match name with
      | "apply_patch" -> [ Functions.apply_patch ~dir:(Ctx.tool_dir ctx) ]
      | "read_dir" -> [ Functions.read_dir ~dir:(Ctx.tool_dir ctx) ]
      | "append_to_file" -> [ Functions.append_to_file ~dir:(Ctx.tool_dir ctx) ]
      | "find_and_replace" -> [ Functions.find_and_replace ~dir:(Ctx.tool_dir ctx) ]
      | "get_contents" | "read_file" -> [ Functions.get_contents ~dir:(Ctx.tool_dir ctx) ]
      | "webpage_to_markdown" -> [ Functions.webpage_to_markdown ... ]
      | "fork" -> [ Functions.fork ]
      | "odoc_search" -> [ Functions.odoc_search ... ]
      | "index_markdown_docs" -> [ Functions.index_markdown_docs ... ]
      | "markdown_search" -> [ Functions.markdown_search ... ]
      | "query_vector_db" -> [ Functions.query_vector_db ... ]
      | "index_ocaml_code" -> [ Functions.index_ocaml_code ... ]
      | "import_image" -> [ Functions.import_image ... ]
      | "meta_refine" -> [ Functions.meta_refine ... ]
      | other -> failwithf "Unknown built-in tool: %s" other ())
   So, for `tools.md`, the **code-truth built-in list** includes at least:
   `apply_patch`, `read_dir`, `append_to_file`, `find_and_replace`, `get_contents` (alias: `read_file`), `webpage_to_markdown`, `fork`, `odoc_search`, `index_markdown_docs`, `markdown_search`, `query_v
ector_db`, `index_ocaml_code`, `import_image`, `meta_refine`.

   **(ii) Custom shell wrapper tool implementation** for `<tool command="..."/>`:
   - Schema is fixed: `{ "arguments": string array }` (hard-coded JSON schema).
   - Command line is taken from the `command="..."` attribute, `%20` is decoded to spaces, and then split on whitespace (simple splitting).
   - Runs via `Eio.Process.spawn` capturing stdout+stderr, with:
     - **timeout 60s** (`Eio.Time.with_timeout_exn ... 60.0`)
     - output truncation to **max 10000 chars** (note: the comment mentions 100KiB, but the code truncates at 10k).  
   Evidence: see `custom_fn` doc+implementation in `lib/chat_response/tool.ml`, including:
   let parameters : Jsonaf.t = ... "arguments" : array of string ...
   ...
   Eio.Time.with_timeout_exn ... 60.0 ...
   ...
   let max_len = 10000 in
   if String.length res > max_len then ... " ...truncated"

   **(iii) Agent tools** for `<tool agent="..."/>`:
   - Schema is fixed: `{ "input": string }`.
   - Fetches prompt content via `Fetch.get ~ctx agent ~is_local`.
   - Runs a nested driver via the `run_agent` callback passed into `Tool.of_declaration`.  
   Evidence: `agent_fn` in `lib/chat_response/tool.ml`.

   **(iv) MCP remote tools** for `<tool mcp_server="..."/>`:
   - Connects via `Mcp_client.connect`.
   - Uses a TTL-LRU cache keyed by the server URI (5 minutes, max size 32).
   - Registers a background invalidation listener for `notifications/tools/list_changed` to drop cache entries.  
   Evidence: at top:
   let tool_cache ... max_size:32
   let cache_ttl = Time_ns.Span.of_int_sec 300
   ...
   | "notifications/tools/list_changed" -> Tool_cache.remove tool_cache mcp_server
   and `mcp_tool` implementation includes env var injection for creds:
   let add_param_if_some uri (name, opt_var) =
     match opt_var with
     | Some env_var -> (match Sys.getenv env_var with ... Uri.add_query_param' uri (name, v))
...
   let uri = add_param_if_some uri ("client_id", client_id_env) in
   let uri = add_param_if_some uri ("client_secret", client_secret_env) in

7) **`lib/chat_response/driver.ml`**  
   The high-level runtime that:
   - Parses ChatMarkdown.
   - Discovers tools from `<tool>` declarations (`Tool.of_declaration`).
   - Builds the OpenAI tool list via `Ochat_function.functions` and `Tool.convert_tools`.
   - Runs streaming loop and executes tool calls (including optional parallel execution).  
   Evidence: `run_agent` explicitly does tool discovery:
   let declared_tools = List.filter_map elements ~f:(function | CM.Tool t -> Some t | _ -> None) in
   let tools = List.concat_map declared_tools ~f:(fun decl -> Tool.of_declaration ... decl) in
   let comp_tools, tool_tbl = Ochat_function.functions tools in
   let tools_req = Tool.convert_tools comp_tools in
   Response_loop.run ... ~tools:tools_req ~tool_tbl ...
   and `run_completion_stream` contains the tool execution machinery, including **parallel tool call execution** with deterministic output ordering.

8) **`lib/chat_response/converter.ml`**  
   Converts ChatMarkdown messages into OpenAI “items”. This matters for tools docs because it defines how `<tool_call/>` and `<tool_response/>` are represented on the OpenAI side, including support for
 “custom tool call” types.  
   Evidence (key parts):
   - For role `tool`, it distinguishes custom tool calls by `m.type_`:
     ```ocaml
     let is_custom_tool_call =
       match m.type_ with
       | Some ("custom_tool_call" | "custom_tool_call_output") -> true
     ```
   - It maps `<tool_call/>` and `<tool_response/>` shorthand tags into `Res.Item.Function_call` / `Function_call_output` or custom equivalents.

9) **`lib/chat_response/tool_call.ml` + `lib/chat_response/tool_call.mli`**  
   Encapsulates tool call execution dispatch:
   - Rejects invoking `"fork"` as a **custom** tool call.
   - Otherwise resolves tool implementations via `tool_tbl` and executes `fn payload`.  
   Evidence:
   | Kind.Custom, true -> Res.Tool_output.Output.Text fork_custom_error
   | (Kind.Function | Kind.Custom), false ->
     let fn = Hashtbl.find_exn tool_tbl name in
     fn payload

10) **`lib/chat_response/response_loop.ml` + `lib/chat_response/response_loop.mli`**  
   The synchronous (“blocking”) loop that repeatedly:
   - Calls OpenAI responses endpoint
   - Executes tool calls returned
   - Appends tool outputs back into history until no more tool calls.  
   Evidence: it filters `Res.Item.Function_call` and `Res.Item.Custom_tool_call` from the response, executes them, wraps into output items, and recurses.

11) **`lib/chat_response/ctx.ml` + `lib/chat_response/ctx.mli`**  
   Context object includes key “tool capability roots” used throughout:
   - `tool_dir` is explicitly used by tool implementations (e.g., `Tool.of_declaration` passes `Ctx.tool_dir ctx` to built-ins like `apply_patch`, `read_dir`, etc.).  
   Evidence: `lib/chat_response/tool.ml` uses `Ctx.tool_dir ctx` repeatedly in built-in dispatch.

12) **`lib/chat_response/fetch.ml` + `lib/chat_response/fetch.mli`**  
   Used by agent tools (`<tool agent="...">`) to load prompt files and by converter to fetch/clean documents; impacts what “agent tools” and doc/image content do at runtime (especially `Fetch.get`, `Fe
tch.get_html` usage shown in `converter.ml`).

---

### C) Tool definition abstraction (how tools are declared/registered)

13) **`lib/ochat_function.mli` + `lib/ochat_function.ml`**  
   The core abstraction for tools:
   - `module type Def` with `name`, `type_`, `description`, `parameters` (JSON schema), `input_of_string`.
   - `create_function` couples the schema module with an implementation; includes `?strict` default `true`.
   - `functions` builds `(Openai.Completions.tool list * dispatch_table)` mapping tool names to OCaml callbacks.
   Evidence:
   val create_function : (module Def ...) -> ?strict:bool -> ('a -> Output.t) -> t
   val functions : t list -> Openai.Completions.tool list * (string, string -> Output.t) Hashtbl.t
   and in `ochat_function.ml`, it stores `info` and `run`, building the dispatch table keyed by `t.info.function_.name`.

14) **`lib/definitions.mli`** (and **`lib/definitions.ml`**, though you’ll likely only need the signatures/docs)  
   This is the **single source of truth for schemas** of built-in tools used by `Functions.*` and reused by the MCP server in places.
   Evidence: the tool definition modules listed in `definitions.mli`:
   module Get_contents ...
   module Apply_patch ...
   module Read_directory ...
   module Meta_refine ...
   module Index_markdown_docs ...
   module Markdown_search ...
   ...
   `bin/mcp_server.ml` explicitly “pulls metadata from Definitions” when registering MCP tools, e.g.:
   let module Def = Definitions.Apply_patch in
   let spec : JT.Tool.t = { name = "apply_patch"; description = Def.description; input_schema = Def.parameters }

---

### D) Built-in tool implementations (what they actually do)

15) **`lib/functions.mli` + `lib/functions.ml`**  
   Implements the built-in tool registrations (each returns an `Ochat_function.t`), including:
   - `get_contents` (read_file) with truncation, optional offset, and binary-file refusal
   - `apply_patch`
   - `read_dir`
   - `append_to_file`, `find_and_replace`
   - indexing/search tools (`index_ocaml_code`, `index_markdown_docs`, `markdown_search`, `query_vector_db`, `odoc_search`)
   - `webpage_to_markdown`
   - `fork` stub tool
   - `meta_refine`
   - `import_image` returning content parts (image inputs)  
   Evidence highlights:
   - `get_contents` truncation/offset and binary detection are implemented in `lib/functions.ml` (see `total = 380_928`, `offset` behavior, and “Refusing to read binary file”).  
   - `fork` is a placeholder that returns a static string:
     ```ocaml
     let fork : Ochat_function.t =
       ... "[fork-tool placeholder – should never be called directly]"
     ```

16) **`lib/apply_patch.ml` + `lib/apply_patch.mli`**  
   Real implementation behind the `apply_patch` tool (called by `Functions.apply_patch`). If `tools.md` describes patch format/semantics, this is the code authority.

17) **`lib/webpage_markdown/tool.ml` + `lib/webpage_markdown/tool.mli`** (and related modules under `lib/webpage_markdown/`)  
   Implements the `webpage_to_markdown` tool that `Functions.webpage_to_markdown` delegates to:
   let webpage_to_markdown ~env ~dir ~net : Ochat_function.t =
     Webpage_markdown.Tool.register ~env ~dir ~net
   (from `lib/functions.ml`)

18) **Index/search backends used by tool implementations** (relevant if `tools.md` explains behavior beyond the signature):
   - `lib/indexer.ml`, `lib/markdown_indexer.ml`, `lib/odoc_indexer.ml`
   - `lib/vector_db.ml`, `lib/bm25.ml`
   - `lib/md_index_catalog.ml`, `lib/package_index.ml`  
   These are called from `Functions.index_*` and `Functions.*_search` tools.

---

### E) MCP client-side: consuming remote tools via `<tool mcp_server="..."/>`

19) **`lib/mcp/mcp_types.mli` + `lib/mcp/mcp_types.ml`**  
   Data model for MCP JSON-RPC, tool metadata, and tool result envelopes:
   - `Tool.t` includes `name`, `description`, `input_schema`
   - `Tool_result.t` includes list of `content` parts (Text/Json/Rich) and `is_error`
   This matters for how remote MCP tool schemas are forwarded “verbatim” into OpenAI tool schemas.

20) **`lib/mcp/mcp_client.mli` + `lib/mcp/mcp_client.ml`**  
   High-level MCP client:
   - `connect` chooses transport based on URI (`stdio:` vs http/https/mcp+http(s)) and performs `initialize` handshake.
   - `list_tools` calls `"tools/list"`.
   - `call_tool` calls `"tools/call"`.
   - exposes `notifications` stream which is used by `lib/chat_response/tool.ml` to invalidate cached MCP tool metadata.  
   Evidence: transport selection logic in `connect`:
   if String.is_prefix uri ~prefix:"stdio:" then Stdio ...
   else if scheme is http/https/mcp+http/mcp+https then Http ...
   else fallback to stdio
   and notification stream is `notif_stream`.

21) **`lib/mcp/mcp_tool.ml`**  
   Wraps a **remote MCP tool** into a local `Ochat_function.t`:
   - Copies `name`, `description`, and `input_schema` into the local tool definition (so OpenAI sees the same JSON schema).
   - Implementation calls `Mcp_client.call_tool ...` and flattens MCP result content to text.  
   Evidence:
   let parameters = tool.input_schema
   ...
   match Client.call_tool client ~name:tool.name ~arguments:args with
   | Ok res -> Output.Text (string_of_result res)
   | Error msg -> Output.Text msg

22) **`lib/mcp/mcp_transport_interface.mli`**  
   Defines the transport signature for MCP client communications (`send`, `recv`, `connect`, etc.).

23) **`lib/mcp/mcp_transport_stdio.ml`**  
   Implements `stdio:` MCP transport: newline-delimited JSON over child stdin/stdout.

24) **`lib/mcp/mcp_transport_http.ml`**  
   Implements HTTP(S) / streamable transport:
   - POST to MCP endpoint, supports SSE response parsing (`text/event-stream`).
   - Has optional OAuth handling (token acquisition and retry behavior).
   This is relevant to document what `<tool mcp_server="https://...">` actually does.

---

### F) MCP server-side: defining and exposing tools over MCP

25) **`lib/mcp/mcp_server_core.mli` + `lib/mcp/mcp_server_core.ml`**  
   In-memory registry for the MCP server:
   - `register_tool`, `list_tools`, `get_tool`
   - hooks for `tools/list_changed`, `prompts/list_changed`, `resources/list_changed`, progress notifications, structured logging, and cancellation tracking.  
   Evidence: interface explicitly mentions list-changed notifications (e.g. tools re-register fires hooks).

26) **`lib/mcp/mcp_server_router.ml`**  
   JSON-RPC router for MCP server methods:
   - Handles `"initialize"`, `"tools/list"`, `"tools/call"`, `"prompts/list"`, `"prompts/get"`, `"resources/list"`, `"resources/read"`, `"roots/list"`, `"ping"`.
   - For `"tools/call"`, executes handler and wraps result into `Tool_result` envelope.  
   Evidence: `tool_call_response` builds `JT.Tool_result.t` and logs success/error via `Mcp_server_core.log`.

27) **`lib/mcp/mcp_server_http.ml`**  
   HTTP transport for the MCP server:
   - `/mcp` POST for JSON-RPC request handling
   - `/mcp` GET for SSE subscriptions
   - broadcasts `notifications/tools/list_changed`, `notifications/prompts/list_changed`, `notifications/resources/list_changed`, plus `notifications/message` for logs and `notifications/progress` for 
progress updates.  
   Evidence: functions:
   let broadcast_tools_list_changed () =
     _broadcast_json (`Object [ "jsonrpc","2.0"; "method","notifications/tools/list_changed" ])
   and hooks registration:
   Mcp_server_core.add_tools_changed_hook core broadcast_tools_list_changed;
   ...
   Mcp_server_core.add_logging_hook core broadcast_log;

28) **`lib/mcp/mcp_prompt_agent.ml`**  
   Converts a `*.chatmd` file into:
   - an MCP `Tool.t` (`name` = filename without extension, schema `{input:string}`)
   - a tool handler that runs `Chat_response.Driver.run_agent` and emits progress notifications.  
   Evidence: builds tool spec and handler:
   let tool_spec : JT.Tool.t = { name; description = Some "ChatMD agent prompt"; input_schema }
   ...
   Chat_response.Driver.run_agent ~ctx prompt_xml [ content_item_of_input user_input ]

29) **`bin/mcp_server.ml`**  
   The MCP server executable wrapper. This file is the authoritative list of **which tools the server registers by default**, and how:
   - Registers `"echo"` sample tool.
   - Registers built-in tools over MCP: `"apply_patch"`, `"read_dir"`, `"get_contents"`, plus `"meta_refine"`, `"webpage_to_markdown"`. It reuses `Definitions.*` schemas and adapts to JSON-RPC handlers
.
   - Scans a prompts directory (`$MCP_PROMPTS_DIR` or `./prompts`) and registers each `*.chatmd` as both:
     - a *prompt* (`register_prompt`), and
     - a *tool* (`register_tool`) via `Mcp_prompt_agent.of_chatmd_file`.
   - Can run stdio or HTTP mode (flags `--http`, `--require-auth`, etc.).  
   Evidence: the built-in registrations and prompt scanning are all in this file (see the `register_builtin_*` functions and `scan_prompts` logic).

---

### G) CLI/TUI surfaces that affect tool behavior (useful for docs about “tool calling” behavior)

30) **`bin/chat_tui.ml`**  
   Documents and implements flags affecting tool calling, notably:
   - `--parallel-tool-calls / --no-parallel-tool-calls` (used by `Driver.run_completion_stream`).  
   Found via `rg` hits in `bin/chat_tui.ml`.

31) **`bin/main.ml`**  
   Entry point mentions tool calling being handled by `Chat_response.Driver.run_completion_stream` (found via `rg`).

32) **`lib/chat_tui/persistence.ml`** (and `.mli`)  
   Handles persistence/formatting of `<tool_call>` and `<tool_response>` blocks in the TUI’s conversation buffer (found via `rg` hits). This matters if `tools.md` describes what gets written into `.cha
tmd` buffers.

---

### H) MCP debugging/examples (handy if docs mention them)

33) **`bin/eio_get.ml`**  
   A simple SSE client for `GET /mcp` that prints JSON events; helps validate MCP server SSE behavior.  
   Evidence: module header says it connects to `[HOST]/mcp` and parses Server-Sent Events.

34) **`bin/piaf_example.ml`**  
   Contains an HTTP example hitting `/mcp` via GET/POST (found via `rg`). Useful as reference but less central than the real transports.

---

## How it fits together (high-level map, code-truth)

- **Parsing**: `lib/chatmd/prompt.ml` turns `<tool .../>` into `Prompt.Chat_markdown.tool` variants (Builtin/Custom/Agent/Mcp).  
- **Instantiation**: `lib/chat_response/tool.ml` implements `Tool.of_declaration` which maps each variant to runtime `Ochat_function.t`:
  - built-ins → `Functions.*`
  - custom command → `custom_fn` wrapper
  - agent prompt → `agent_fn` wrapper
  - MCP server → `mcp_tool` (connect, list tools, wrap each into `Ochat_function.t`)
- **Execution**: `Driver` / `Response_loop` receive tool calls from OpenAI responses, and dispatch them through the `tool_tbl` created by `Ochat_function.functions`.  
- **MCP client**: `Mcp_client` (stdio/http transports) + `Mcp_tool` wrapper provide the remote tools as if they were local.  
- **MCP server**: `bin/mcp_server.ml` + `lib/mcp/mcp_server_*` expose tools/prompt-agents over MCP, with notifications and (HTTP) SSE.

---

## Next pointers (if you’re updating `docs-src/overview/tools.md`)

If you want the doc to be “code-correct”, the most important truth sources to align with are:

- Built-in tool names and which are exposed via `<tool name="..."/>`: **`lib/chat_response/tool.ml`** (the `CM.Builtin` match).  
- How `<tool>` attributes work (`command`, `agent`, `mcp_server`, `include(s)`, `strict`, credential env injection): **`lib/chatmd/prompt.ml`** + **`lib/chat_response/tool.ml`** (`custom_fn`, `agent_fn
`, `mcp_tool`).  
- MCP server built-in tools and prompt scanning behavior: **`bin/mcp_server.ml`** + **`lib/mcp/mcp_prompt_agent.ml`** + **`lib/mcp/mcp_server_http.ml`**.
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696eef5d94588195944802c3bbfc5854">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696eef609620819591e9c58a3364ccdf">
RAW|
Purpose: Inspect the current tools documentation and the core implementation files to identify mismatches/outdated sections that need updating.
|RAW
</assistant>


<tool_call tool_call_id="call_MwM9sxrdiAoMeo9M6e9GvxLl" function_name="read_file" id="fc_0a7776dd715389aa00696eef60cdb481959eddd334a72133d2">
RAW|
{"file":"docs-src/overview/tools.md"}
|RAW
</tool_call>


<tool_call tool_call_id="call_StDBeb5munh1XkjKvCxYZlVX" function_name="read_file" id="fc_0a7776dd715389aa00696eef60e6448195ac2e48c4f711c2e0">
RAW|
{"file":"lib/chat_response/tool.ml"}
|RAW
</tool_call>


<tool_call tool_call_id="call_D5R2s4gnrBpZH8QjNz8kNQn6" function_name="read_file" id="fc_0a7776dd715389aa00696eef60fd2c8195a36516ad667b3391">
RAW|
{"file":"lib/functions.mli"}
|RAW
</tool_call>


<tool_call tool_call_id="call_Nl6ddPyFTzwIc61qFwq9kuRf" function_name="read_file" id="fc_0a7776dd715389aa00696eef611a88819585b9e0d27eb8f40e">
RAW|
{"file":"lib/functions.ml"}
|RAW
</tool_call>


<tool_call tool_call_id="call_LsplE3opnL2md7jLOhQ0ERol" function_name="read_file" id="fc_0a7776dd715389aa00696eef612cdc8195a15764e7bb62d026">
RAW|
{"file":"lib/chatmd/prompt.mli"}
|RAW
</tool_call>


<tool_call tool_call_id="call_Jv2vbCmTtWcFOZl9rRqJLIfo" function_name="read_file" id="fc_0a7776dd715389aa00696eef613fe48195ad244636437bbd32">
RAW|
{"file":"lib/chatmd/prompt.ml"}
|RAW
</tool_call>


<tool_call tool_call_id="call_SrkhCaC1ikRXBRcDASU7D502" function_name="read_file" id="fc_0a7776dd715389aa00696eef6154508195a1a0329eb3c2bed4">
RAW|
{"file":"bin/mcp_server.ml"}
|RAW
</tool_call>


<tool_call tool_call_id="call_UoF1eu1heaFV6XWFqF0jxJHy" function_name="read_file" id="fc_0a7776dd715389aa00696eef6166008195914e9056f8138edc">
RAW|
{"file":"bin/chat_tui.ml"}
|RAW
</tool_call>

<tool_response tool_call_id="call_MwM9sxrdiAoMeo9M6e9GvxLl">
RAW|
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

|RAW
</tool_response>
<tool_response tool_call_id="call_StDBeb5munh1XkjKvCxYZlVX">
RAW|
(** Tool helper utilities.

    This module turns a ChatMarkdown [`<tool …/>`] declaration into a
    runtime {!Ochat_function.t} that can be submitted to the *OpenAI
    function-calling API*.  The helper covers **four** independent
    back-ends:

    1. {b Built-ins} – OCaml functions hard-coded in {!Functions}
       (e.g. ["apply_patch"], ["fork"], …).
    2. {b Custom shell commands} – `{<tool command="grep" …/>}` wrappers
       that spawn an arbitrary process inside the Eio sandbox.
    3. {b Agent prompts} – nested ChatMarkdown agents executed through
       the same driver stack.
    4. {b Remote MCP tools} – functions discovered dynamically over the
       Model-Context-Protocol network.

    The public surface is intentionally small – only the dispatcher
    {!of_declaration} and a (de)serialisation helper {!convert_tools}
    are meant to be consumed by other modules.  Everything else is
    private glue code.

    {1 Example}

    Converting a list of ChatMarkdown declarations into the request
    payload expected by {!Openai.Responses}:

    {[
      let ochat_functions =
        List.concat_map declarations ~f:(Tool.of_declaration ~sw ~ctx ~run_agent)

      let comp_tools, _tbl = Ochat_function.functions ochat_functions in
      let request_tools  = Tool.convert_tools comp_tools in
      (* … pass [request_tools] to [Openai.Responses.post_response] … *)
    ]}

    {1 Warning}

    The {e custom shell} backend executes arbitrary commands provided by
    the prompt author.  Enable it only in a {b trusted} environment and
    consider using dedicated OCaml helpers or remote MCP tools for
    production workloads. *)

open Core
module CM = Prompt.Chat_markdown
module Res = Openai.Responses

(*------------------------------------------------------------------*)
(* 5. Remote MCP tool metadata cache                                *)
(*------------------------------------------------------------------*)

(* We keep a small TTL-based LRU that maps an MCP server URI to the
   list of tools it exposes. This avoids re-running the expensive
   `tools/list` handshake for every `<tool mcp_server=...>`
   declaration inside a prompt. *)

module String_key = struct
  type t = string [@@deriving sexp, compare, hash]

  (* The cache key is just the server URI – no internal invariants. *)
  let invariant (_ : t) = ()
end

module Tool_cache = Ttl_lru_cache.Make (String_key)

let tool_cache : Mcp_types.Tool.t list Tool_cache.t = Tool_cache.create ~max_size:32 ()
let cache_ttl = Time_ns.Span.of_int_sec 300

(* When a given MCP server notifies that its tool list has changed we
   simply drop the cached entry for that URI so that the next lookup
   forces a fresh `tools/list` request.  The helper below registers a
   lightweight daemon (at most one per client/URI pair) that listens
   for such notifications and performs the invalidation.            *)

let register_invalidation_listener ~sw ~mcp_server ~client =
  (* Register a background fibre that listens for
     `notifications/tools/list_changed` messages published by the MCP
     server.  Upon reception the local *TTL-LRU* entry for that server
     is evicted so that the next call to {!mcp_tool} forces a fresh
     `tools/list` round-trip.

     The operation is idempotent and cheap – duplicates are acceptable. *)
  (* We attach the listener on a background fibre so it does not block the
     normal execution flow.  The fibre terminates automatically when the
     underlying stream closes (e.g. connection lost) or the switch is
     torn down. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      match
        try Some (Eio.Stream.take (Mcp_client.notifications client)) with
        | End_of_file -> None
      with
      | None -> `Stop_daemon
      | Some notification ->
        (match notification.method_ with
         | "notifications/tools/list_changed" ->
           ignore (Tool_cache.remove tool_cache mcp_server : _)
         | _ -> ());
        loop ()
    in
    loop ())
;;

(*--- 4-a.  OpenAI → Responses tool conversion ----------------------*)

(** [convert_tools ts] converts a list of [`Openai.Completions.tool`]
    descriptors – the minimal structure returned by the [openai] SDK –
    into the richer {!Openai.Responses.Request.Tool.t} representation
    expected by the *chat/completions* endpoint.

    The transformation is a {i pure}, field-by-field copy.  It exists
    only to prevent callers from having to depend on both modules at
    once.

    Complexity: O(n) where [n = List.length ts]. *)
let convert_tools (ts : Openai.Completions.tool list) : Res.Request.Tool.t list =
  List.map ts ~f:(fun { type_; function_ = { name; description; parameters; strict } } ->
    match type_ with
    | "custom" ->
      Res.Request.Tool.Custom_function { name; description; format = parameters; type_ }
    | _ -> Res.Request.Tool.Function { name; description; parameters; strict; type_ })
;;

(*--- 4-b.  Custom shell command tool --------------------------------*)

(** [custom_fn ~env decl] wraps a `{<tool command="…"/>}` element into a
    callable {!Ochat_function.t}.

    Input schema
    {[
      {
        "arguments": string array   (* Command-line arguments *)
      }
    ]}

    The function spawns the declared command inside the Eio sandbox,
    feeds it the provided arguments, then returns the concatenated
    *stdout* and *stderr* streams.

    Invariants & safeguards
    • Hard timeout of {b 60 s}.  The process is killed afterwards.
    • Output is truncated to at most {b 100 KiB} to avoid flooding the
      model context.

    Use this backend only for {e quick experiments}.  Prefer dedicated
    OCaml helpers or remote MCP tools in production. *)
let custom_fn ~env (c : CM.custom_tool) : Ochat_function.t =
  let CM.{ name; description; command } = c in
  let module M : Ochat_function.Def with type input = string list = struct
    type input = string list

    let name = name
    let type_ = "function"

    let description : string option =
      match description with
      | Some desc ->
        Some
          (String.concat
             [ "Run a "
             ; command
             ; " shell command with arguments, and returns its output.\n"
             ; desc
             ])
      | None ->
        Some
          (String.concat
             [ "Run a "
             ; command
             ; " shell command with arguments, and returns its output"
             ])
    ;;

    let parameters : Jsonaf.t =
      `Object
        [ "type", `String "object"
        ; ( "properties"
          , `Object
              [ ( "arguments"
                , `Object
                    [ "type", `String "array"
                    ; "items", `Object [ "type", `String "string" ]
                    ] )
              ] )
        ; "required", `Array [ `String "arguments" ]
        ; "additionalProperties", `False
        ]
    ;;

    let input_of_string s : input =
      let j = Jsonaf.of_string s in
      j
      |> Jsonaf.member_exn "arguments"
      |> Jsonaf.list_exn
      |> List.map ~f:Jsonaf.string_exn
    ;;
  end
  in
  let fp (params : string list) : string =
    let proc_mgr = Eio.Stdenv.process_mgr env in
    Eio.Switch.run
    @@ fun sw ->
    (* 1.  Pipe for capturing stdout & stderr. *)
    let r, w = Eio.Process.pipe ~sw proc_mgr in
    let cmdline = command |> String.substr_replace_all ~pattern:"%20" ~with_:" " in
    (* Split on whitespace – rudimentary, but sufficient for Phase-1. *)
    let cmd_list =
      if String.is_empty cmdline
      then invalid_arg "custom_fn: empty command line"
      else
        String.split_on_chars ~on:[ ' '; Char.of_int_exn 32 ] cmdline
        |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    (* 2.  Check that the command is not empty. *)
    (* 2.  Spawn the child process with the provided command and parameters. *)
    (* Note: we use [Eio.Process.spawn] to run the command, which captures
       stdout and stderr into the pipe [w]. *)
    (* Note: we use [Eio.Buf_read.parse_exn] to read the output from the pipe. *)
    match
      Eio.Process.spawn ~sw proc_mgr ~stdout:w ~stderr:w (List.append cmd_list params)
    with
    | exception ex ->
      let err_msg = Fmt.str "error running %s command: %a" command Eio.Exn.pp ex in
      Eio.Flow.close w;
      err_msg
    | _child ->
      Eio.Flow.close w;
      (match Eio.Buf_read.parse_exn ~max_size:1_000_000 Eio.Buf_read.take_all r with
       | res ->
         let max_len = 10000 in
         let res =
           if String.length res > max_len
           then String.append (String.sub res ~pos:0 ~len:max_len) " ...truncated"
           else if String.is_empty res
           then "Command output is empty"
           else res
         in
         res
       | exception ex -> Fmt.str "error running %s command: %a" command Eio.Exn.pp ex)
  in
  (* timeout functioin eio *)
  let fp x =
    try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60.0 (fun () -> fp x) with
    | Eio.Time.Timeout ->
      Printf.sprintf "timeout running command %s" (String.concat ~sep:" " x)
  in
  (* Create the Ochat_function.t using the module M and the function fp. *)
  (* Note: we use [Ochat_function.create_function] to create the function. *)
  (* Note: we use [module M] to specify the module type for the function. *)
  Ochat_function.create_function
    (module M)
    (fun args -> Res.Tool_output.Output.Text (fp args))
;;

(*--- 4-c.  Agent tool → Ochat_function.t ------------------------------*)

(** [agent_fn ~ctx ~run_agent spec] wraps a nested ChatMarkdown
    {e agent} into a {!Ochat_function.t}.  Calling the resulting function
    is equivalent to starting a brand-new ChatMarkdown driver on the
    referenced `*.chatmd` file.

    Expected input
    {[
      { "input" : string }   (* Message forwarded to the agent *)
    ]}

    The helper runs [run_agent] – a higher-order callback supplied by
    the caller – to avoid creating a circular dependency with
    {!Chat_response.Driver}.  The child conversation inherits the
    parent context [ctx] but not its message history.

    Typical use-case: breaking down a complex user request into
    multiple self-contained sub-tasks handled by specialised prompts. *)
let agent_fn ~(ctx : _ Ctx.t) ~run_agent (agent_spec : CM.agent_tool) : Ochat_function.t =
  let CM.{ name; description; agent; is_local } = agent_spec in
  (* pull components from the shared context *)
  let _net_unused = Ctx.net ctx in
  (* Interface definition for the agent tool – expects an object with a
       single string field "input". *)
  let module M : Ochat_function.Def with type input = string = struct
    type input = string

    let name = name
    let type_ = "function"

    let description : string option =
      Option.first_some
        description
        (Some
           (Printf.sprintf
              "Run agent prompt located at %s and return its final answer."
              agent))
    ;;

    let parameters : Jsonaf.t =
      `Object
        [ "type", `String "object"
        ; "properties", `Object [ "input", `Object [ "type", `String "string" ] ]
        ; "required", `Array [ `String "input" ]
        ; "additionalProperties", `False
        ]
    ;;

    let input_of_string s : input =
      match Jsonaf.(of_string s |> member_exn "input") with
      | `String str -> str
      | _ -> failwith "Expected {\"input\": string} for agent tool input"
    ;;
  end
  in
  let run (user_msg : string) : string =
    (* Build a basic content item from the provided user input. *)
    let basic_item : CM.basic_content_item =
      { type_ = "text"
      ; text = Some user_msg
      ; image_url = None
      ; document_url = None
      ; is_local = false
      ; cleanup_html = false
      ; markdown = false
      }
    in
    (* Fetch the agent prompt (local or remote) *)
    let prompt_xml = Fetch.get ~ctx agent ~is_local in
    (* Delegate the heavy lifting to the provided [run_agent] callback. *)
    run_agent ~ctx prompt_xml [ CM.Basic basic_item ]
  in
  Ochat_function.create_function
    (module M)
    (fun args -> Res.Tool_output.Output.Text (run args))
;;

(** [mcp_tool ~sw ~ctx decl] resolves a `{<tool mcp_server="…"/>}`
      declaration.  It returns one {!Ochat_function.t} per advertised
      remote function.

      Implementation details:
      – Remote metadata are fetched through {!Mcp_client.list_tools}.
      – A TTL-LRU (5 min / 32 entries) caches the result per server.
      – The helper registers a background fibre listening for
        `notifications/tools/list_changed` and invalidates the cache on
        demand.

      When [decl.names] is [`Some list`] only the named tools are wrapped;
      otherwise the full catalog is exposed. *)
let mcp_tool
      ~sw
      ~ctx
      CM.{ names; description = _; mcp_server; strict; client_id_env; client_secret_env }
  =
  (* Inject per-server credentials via URI query params if attribute specifies
     environment variable names and the variables are present. *)
  let mcp_server_uri =
    let uri = Uri.of_string mcp_server in
    let add_param_if_some uri (name, opt_var) =
      match opt_var with
      | None -> uri
      | Some env_var ->
        (match Sys.getenv env_var with
         | Some v when not (String.is_empty v) -> Uri.add_query_param' uri (name, v)
         | _ -> uri)
    in
    let uri = add_param_if_some uri ("client_id", client_id_env) in
    let uri = add_param_if_some uri ("client_secret", client_secret_env) in
    Uri.to_string uri
  in
  let client = Mcp_client.connect ~sw ~env:(Ctx.env ctx) mcp_server_uri in
  (* Ensure cache invalidation for this server is wired up exactly
     once.  We conservatively register a listener each time – the
     underlying [Tool_cache.remove] operation is idempotent and cheap,
     so occasional duplicates are harmless. *)
  register_invalidation_listener ~sw ~mcp_server ~client;
  let get_tool name =
    let tools_for_server =
      Tool_cache.find_or_add tool_cache mcp_server ~ttl:cache_ttl ~default:(fun () ->
        match Mcp_client.list_tools client with
        | Ok lst -> lst
        | Error msg -> failwithf "Failed to list tools from %s: %s" mcp_server msg ())
    in
    let tool_meta =
      match List.find tools_for_server ~f:(fun t -> String.equal t.name name) with
      | Some t -> t
      | None ->
        (* Cache might be stale – refresh once before giving up. *)
        let tools =
          match Mcp_client.list_tools client with
          | Ok lst ->
            (* Update cache and continue. *)
            Tool_cache.set_with_ttl tool_cache ~key:mcp_server ~data:lst ~ttl:cache_ttl;
            lst
          | Error msg -> failwithf "Failed to list tools from %s: %s" mcp_server msg ()
        in
        (match List.find tools ~f:(fun t -> String.equal t.name name) with
         | Some t -> t
         | None ->
           failwithf
             "MCP server %s does not expose tool %s (after refresh)"
             mcp_server
             name
             ())
    in
    Mcp_tool.ochat_function_of_remote_tool ~sw ~client ~strict tool_meta
  in
  match names with
  | Some names -> List.map names ~f:get_tool
  | None ->
    let tools_for_server =
      Tool_cache.find_or_add tool_cache mcp_server ~ttl:cache_ttl ~default:(fun () ->
        match Mcp_client.list_tools client with
        | Ok lst -> lst
        | Error msg -> failwithf "Failed to list tools from %s: %s" mcp_server msg ())
    in
    List.map tools_for_server ~f:(fun t ->
      Mcp_tool.ochat_function_of_remote_tool ~sw ~client ~strict t)
;;

(*--- 4-d.  Unified declaration → function mapping ------------------*)
(** [of_declaration ~sw ~ctx ~run_agent decl] dispatches a single
    ChatMarkdown [`<tool …/>`] declaration to its runtime
    implementation.

    The helper inspects the variant constructor of [decl] and returns
    a list of {!type:Ochat_function.t}.  A single declaration can map to
    several functions – for example an [`<tool mcp_server=…/>`]
    element expands to the complete set of remote tools exposed by the
    referenced MCP server.  The resulting list is therefore suitable
    for direct consumption by {!Ochat_function.functions}.

    Input invariants
    • [sw] – parent {!Eio.Switch.t}.  Child fibres (e.g. MCP cache
      listeners) are attached to this switch so that they terminate
      cleanly when the caller’s scope ends.
    • [ctx] – shared execution context.  Directory paths, network and
      environment handles are forwarded to the lower-level helpers.
    • [run_agent] – callback used to start a nested ChatMarkdown agent
      when handling [`CM.Agent _`] declarations.  Passing the function
      as an argument avoids a circular dependency with
      {!module:Chat_response.Driver}.

    Complexity: O(1) except for the MCP branch which may perform a
    network round-trip when the server metadata is not cached.

    @raise Failure if the declaration references an unknown built-in
           tool name.
*)
let of_declaration ~sw ~(ctx : _ Ctx.t) ~run_agent (decl : CM.tool)
  : Ochat_function.t list
  =
  match decl with
  | CM.Builtin name ->
    (match name with
     | "apply_patch" -> [ Functions.apply_patch ~dir:(Ctx.tool_dir ctx) ]
     | "read_dir" -> [ Functions.read_dir ~dir:(Ctx.tool_dir ctx) ]
     | "append_to_file" -> [ Functions.append_to_file ~dir:(Ctx.tool_dir ctx) ]
     | "find_and_replace" -> [ Functions.find_and_replace ~dir:(Ctx.tool_dir ctx) ]
     | "get_contents" | "read_file" -> [ Functions.get_contents ~dir:(Ctx.tool_dir ctx) ]
     | "webpage_to_markdown" ->
       [ Functions.webpage_to_markdown
           ~env:(Ctx.env ctx)
           ~dir:(Ctx.tool_dir ctx)
           ~net:(Ctx.net ctx)
       ]
     | "fork" -> [ Functions.fork ]
     | "odoc_search" ->
       [ Functions.odoc_search ~dir:(Ctx.tool_dir ctx) ~net:(Ctx.net ctx) ]
     | "index_markdown_docs" ->
       [ Functions.index_markdown_docs ~env:(Ctx.env ctx) ~dir:(Ctx.tool_dir ctx) ]
     | "markdown_search" ->
       [ Functions.markdown_search ~dir:(Ctx.tool_dir ctx) ~net:(Ctx.net ctx) ]
     | "query_vector_db" ->
       [ Functions.query_vector_db ~dir:(Ctx.tool_dir ctx) ~net:(Ctx.net ctx) ]
     | "index_ocaml_code" ->
       [ Functions.index_ocaml_code
           ~env:(Ctx.env ctx)
           ~dir:(Ctx.tool_dir ctx)
           ~net:(Ctx.net ctx)
       ]
     | "import_image" -> [ Functions.import_image ~dir:(Ctx.tool_dir ctx) ]
     | "meta_refine" -> [ Functions.meta_refine ~env:(Ctx.env ctx) ]
     | other -> failwithf "Unknown built-in tool: %s" other ())
  | CM.Custom c -> [ custom_fn ~env:(Ctx.env ctx) c ]
  | CM.Agent agent_spec -> [ agent_fn ~ctx ~run_agent agent_spec ]
  | CM.Mcp mcp -> mcp_tool ~sw ~ctx mcp
;;
|RAW
</tool_response>
<tool_response tool_call_id="call_D5R2s4gnrBpZH8QjNz8kNQn6">
RAW|
(** Ready-made Ochat **tools** implemented on top of {{!module:Ochat_function}Ochat_function}.

    The values exposed by this module are *registrations* – each one is the
    result of a call to {!Ochat_function.create_function}.  They can be mixed and
    matched freely when building the tool-list for
    {!Openai.Completions.post_chat_completion}:

    {[
      let tools, dispatch_tbl =
        Ochat_function.functions
          [ Functions.get_contents ~dir:cwd
          ; Functions.apply_patch  ~dir:cwd
          ; Functions.odoc_search  ~dir:cwd ~net
          ]
    ]}

    All helpers are *side-effect free* until their [`run`] callback is executed;
    the required capabilities (filesystem directory, network handle, domain
    manager, …) are injected explicitly via labelled arguments.  This
    capability-style design makes the functions easy to reason about in a
    concurrent [`Eio`] application.

    {1 Categories}

    • Filesystem – {{!val:get_contents}get_contents},
                   {{!val:apply_patch}apply_patch},
                   {{!val:append_to_file}append_to_file},
                   {{!val:find_and_replace}find_and_replace},
                   {{!val:read_dir}read_dir},
                   {{!val:mkdir}mkdir}

    • Search     – {{!val:odoc_search}odoc_search},
                   {{!val:query_vector_db}query_vector_db},
                   {{!val:markdown_search}markdown_search}

    • Indexing   – {{!val:index_ocaml_code}index_ocaml_code},
                   {{!val:index_markdown_docs}index_markdown_docs}

    • Web        – {{!val:get_url_content}get_url_content},
                   {{!val:webpage_to_markdown}webpage_to_markdown}
*)

(** Prefix each line of [text] with a 1-based line counter.

    The helper is primarily used for pretty-printing code snippets in tool
    responses so that large fragments can be referred to unambiguously by the
    LLM (e.g. “change line 42”).  No trailing newline is added. *)
val add_line_numbers : string -> string

(** {1 Filesystem helpers} *)

(** Register the [`read_file`] tool.

    • **Schema** – expects an argument object `{ file : string }`.

    • **Behaviour** – returns the UTF-8 contents of [`file`], read via
      {!Io.load_doc}.  Errors are rendered with {!Eio.Exn.pp} and propagated as
      plain strings so that the model can inspect the failure reason. *)
val get_contents : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`get_url_content`] tool.

    Downloads an HTTP resource using {{!module:Io.Net}Io.Net}, strips all HTML
    tags with [LambdaSoup], and returns the visible text.  Content larger than
    the current chat context window is not truncated automatically – callers
    should post-process the string if necessary. *)
val get_url_content : net:_ Eio.Net.t -> Ochat_function.t

(** Register the [`index_ocaml_code`] tool.

    Recursively walks [folder_to_index] (argument) and builds a hybrid vector
    + BM-25 index under [vector_db_folder].  The heavy lifting is delegated to
    {!module:Indexer}.  Progress reporting happens on stdout; the returned
    string is always ["code has been indexed"]. *)
val index_ocaml_code
  :  env:Eio_unix.Stdenv.base
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> net:_ Eio.Net.t
  -> Ochat_function.t

(** Register the [`query_vector_db`] tool.

    Given a query string, combines OpenAI embeddings with a BM-25 overlay to
    search a pre-built index.  The result is a Markdown list of code snippets
    wrapped in [```ocaml] fences.  See {!Vector_db.query_hybrid} for the
    scoring details. *)
val query_vector_db : dir:Eio.Fs.dir_ty Eio.Path.t -> net:_ Eio.Net.t -> Ochat_function.t

(** Register the [`index_markdown_docs`] tool.
    Crawls a directory of Markdown files, chunks them into token–bounded
    windows, embeds the text with OpenAI, and writes a vector database under
    [.md_index/<index_name>].  The helper is a thin wrapper around
    {!Markdown_indexer.index_directory}. *)
val index_markdown_docs
  :  env:Eio_unix.Stdenv.base
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> Ochat_function.t

(** Register the [`markdown_search`] tool – semantic search across Markdown
    indices previously created with {!index_markdown_docs}. *)
val markdown_search : dir:Eio.Fs.dir_ty Eio.Path.t -> net:_ Eio.Net.t -> Ochat_function.t

(** Register the [`apply_patch`] tool that applies a *Ochat diff* to the
    workspace rooted at [dir].  The helper is a thin wrapper around
    {!Apply_patch.process_patch}.  It supports additions, deletions, in-place
    modifications, and file moves. *)
val apply_patch : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`read_directory`] tool.  Returns the entry list of the given
    sub-directory without recursion. *)
val read_dir : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`mkdir`] tool.  Creates the specified sub-directory with mode
    0o700.  The action is idempotent when the folder already exists. *)
val mkdir : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`append_to_file`] tool.  Appends a string to a file, creating
    it if necessary.  The input is a tuple of [file] and [text].  The action is
    idempotent when the text is already present at the end of the file. *)
val append_to_file : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** Register the [`find_and_replace`] tool.  Searches for a string in a file and
    replaces it with another string.  The input is a tuple of [file], [search],
    [replace], and a boolean [all] that controls whether all occurrences should
    be replaced or only the first one.  The action is idempotent when the search
    string is not found or already replaced. *)
val find_and_replace : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t

(** {1 Search helpers} *)

(** Register the [`odoc_search`] tool – a semantic search over locally indexed
    OCaml documentation.  The tool embeds the textual query with OpenAI and
    performs cosine similarity against an [Owl] matrix of pre-computed snippet
    embeddings.  Results are rendered in the same Markdown format as the
    original command-line utility bundled in this repository. *)
val odoc_search : dir:Eio.Fs.dir_ty Eio.Path.t -> net:_ Eio.Net.t -> Ochat_function.t

(** {1 Web helper} *)

(** Register the [`webpage_to_markdown`] tool that converts a remote web page
    to Markdown using a heuristic readability extractor.  See
    {!Webpage_markdown.Tool}.  The environment capability [env] is used to
    access the host’s standard network stack and DNS. *)
val webpage_to_markdown
  :  env:Eio_unix.Stdenv.base
  -> dir:_ Eio.Path.t
  -> net:_ Eio.Net.t
  -> Ochat_function.t

(** {1 Miscellaneous} *)

(** Placeholder registration for the [`fork`] tool.  Its implementation is a
    stub and should never be called directly; it exists only so that the JSON
    schema can be advertised to the model. *)
val fork : Ochat_function.t

(** Apply *Recursive Meta-Prompting* refinement to a raw prompt.  The tool
    receives the full prompt in its JSON [prompt] field and returns the
    improved version produced by {!Meta_prompting.Recursive_mp.refine}. *)
val meta_refine : env:Eio_unix.Stdenv.base -> Ochat_function.t

val import_image : dir:Eio.Fs.dir_ty Eio.Path.t -> Ochat_function.t
|RAW
</tool_response>
<tool_response tool_call_id="call_Nl6ddPyFTzwIc61qFwq9kuRf">
RAW|
open Core
open Io
module Output = Openai.Responses.Tool_output.Output

let add_line_numbers str =
  let lines = String.split_lines str in
  let numbered_lines =
    List.mapi ~f:(fun i line -> Printf.sprintf "%d. %s" (i + 1) line) lines
  in
  String.concat ~sep:"\n" numbered_lines
;;

let is_text_char = function
  | ' ' .. '~' (* ASCII printable *)
  | '\n' | '\r' | '\t' (* common whitespace: LF, CR, TAB *) -> true
  | c ->
    (* Treat non-ASCII bytes as “text” (UTF-8 payload or legacy encodings). *)
    Char.to_int c >= 0x80
;;

let has_nul s = String.exists s ~f:(fun c -> Char.to_int c = 0x00)

let is_utf8 s =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec loop () =
    match Uutf.decode dec with
    | `Uchar _ | `Await -> loop ()
    | `End -> true
    | `Malformed _ -> false
  in
  loop ()
;;

let is_text s =
  (* NUL is a strong binary signal; reject early *)
  if has_nul s then false else String.for_all s ~f:is_text_char && is_utf8 s
;;

(* prevent path for binary files like gif/image/ect *)
let is_binary_file ~dir path =
  (* Fallback: try to guess from file content (very basic) *)
  let content = Io.load_doc ~dir path in
  not (is_text content)
;;

let get_contents ~dir : Ochat_function.t =
  let f (path, offset) =
    let read () =
      if is_binary_file ~dir path
      then
        failwith
          (Printf.sprintf
             "Refusing to read binary file: %s"
             Eio.Path.(native_exn (dir / path)));
      Eio.Path.with_open_in Eio.Path.(dir / path)
      @@ fun flow ->
      try
        let r = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
        let skipped = ref 0 in
        let total = 380_928 in
        let taken = ref 0 in
        let truncated = ref false in
        let result =
          match offset with
          | None ->
            let lines = r |> Eio.Buf_read.(seq line) in
            let lines =
              Seq.take_while
                (fun s ->
                   if !taken < total
                   then (
                     taken := !taken + String.length s;
                     true)
                   else (
                     truncated := true;
                     false))
                lines
            in
            Sequence.of_seq lines |> Sequence.to_list |> String.concat ~sep:"\n"
          | Some offset ->
            let lines =
              r
              |> Eio.Buf_read.(seq line)
              |> Seq.drop_while (fun s ->
                if !skipped < offset
                then (
                  skipped := !skipped + String.length s;
                  true)
                else false)
            in
            let lines =
              Seq.take_while
                (fun s ->
                   if !taken < total
                   then (
                     taken := !taken + String.length s;
                     true)
                   else if !taken > total
                   then (
                     truncated := true;
                     false)
                   else true)
                lines
            in
            Sequence.of_seq lines |> Sequence.to_list |> String.concat ~sep:"\n"
        in
        if !truncated
        then
          (* Truncate to last full line *)
          Printf.sprintf "%s\n\n---\n[File truncated]" result
        else result
      with
      | Eio.Exn.Io _ as ex -> Fmt.str "error running read_file: %a" Eio.Exn.pp ex
    in
    match read () with
    | res -> res
    | exception ex -> Fmt.str "error running read_file: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Get_contents)
    ~strict:false
    (fun args -> Output.Text (f args))
;;

let append_to_file ~dir : Ochat_function.t =
  let f (path, content) =
    try
      Io.append_doc ~dir path ("\n" ^ content);
      Printf.sprintf "Content appended to %s successfully." path
    with
    | ex -> Fmt.str "error running append_to_file: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Append_to_file)
    (fun args -> Output.Text (f args))
;;

let find_and_replace ~dir : Ochat_function.t =
  let f (path, search, replace, all) =
    try
      let content = Io.load_doc ~dir path in
      let match_idexes =
        String.substr_index_all ~may_overlap:false ~pattern:search content
      in
      match all, match_idexes with
      | _, [] -> Printf.sprintf "No occurrences of '%s' found in %s." search path
      | false, _ :: [] ->
        let new_content =
          String.substr_replace_first content ~pattern:search ~with_:replace
        in
        Io.save_doc ~dir path new_content;
        Printf.sprintf
          "Replaced first occurrence of '%s' with '%s' in %s successfully."
          search
          replace
          path
      | false, _ :: _ ->
        sprintf
          "Error Found multiple occurrences of '%s' in %s, but all set to false. Use \
           apply_patch instead."
          search
          path
      | true, _ ->
        let new_content =
          String.substr_replace_all content ~pattern:search ~with_:replace
        in
        Io.save_doc ~dir path new_content;
        Printf.sprintf "Replaced '%s' with '%s' in %s successfully." search replace path
    with
    | ex -> Fmt.str "error running find_and_replace: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Find_and_replace)
    (fun args -> Output.Text (f args))
;;

let get_url_content ~net : Ochat_function.t =
  let f url =
    let host = Net.get_host url in
    let path = Net.get_path url in
    print_endline host;
    print_endline path;
    let headers = Http.Header.of_list [ "Accept", "*/*"; "Accept-Encoding", "gzip" ] in
    let res = Net.get Net.Default ~net ~host path ~headers in
    let decompressed = Option.value ~default:res @@ Result.ok (Ezgzip.decompress res) in
    let soup = Soup.parse decompressed in
    String.concat ~sep:"\n"
    @@ List.filter ~f:(fun s -> not @@ String.equal "" s)
    @@ List.map ~f:(fun s -> String.strip s)
    @@ Soup.texts soup
  in
  Ochat_function.create_function
    (module Definitions.Get_url_content)
    (fun args -> Output.Text (f args))
;;

let index_ocaml_code ~env ~dir ~net : Ochat_function.t =
  let f (folder_to_index, vector_db_folder) =
    Eio.Switch.run
    @@ fun sw ->
    let pool =
      Eio.Executor_pool.create
        ~sw
        (Eio.Stdenv.domain_mgr env)
        ~domain_count:(Domain.recommended_domain_count () - 1)
    in
    Indexer.index ~dir ~pool ~net ~vector_db_folder ~folder_to_index;
    "code has been indexed"
  in
  Ochat_function.create_function
    (module Definitions.Index_ocaml_code)
    (fun args -> Output.Text (f args))
;;

let query_vector_db ~dir ~net : Ochat_function.t =
  let f (vector_db_folder, query, num_results, index) =
    let vf = dir / vector_db_folder in
    let index =
      Option.value ~default:"" @@ Option.map ~f:(fun index -> "." ^ index) index
    in
    let file = String.concat [ "vectors"; index; ".binio" ] in
    let vec_file = String.concat [ vector_db_folder; "/"; file ] in
    let bm25_file = String.concat [ vector_db_folder; "/bm25"; index; ".binio" ] in
    let vecs = Vector_db.Vec.read_vectors_from_disk (dir / vec_file) in
    let corpus = Vector_db.create_corpus vecs in
    let bm25 =
      try Bm25.read_from_disk (dir / bm25_file) with
      | _ -> Bm25.create []
    in
    let response = Openai.Embeddings.post_openai_embeddings net ~input:[ query ] in
    let query_vector =
      Owl.Mat.of_arrays [| Array.of_list (List.hd_exn response.data).embedding |]
      |> Owl.Mat.transpose
    in
    let top_indices =
      Vector_db.query_hybrid
        corpus
        ~bm25
        ~beta:0.4
        ~embedding:query_vector
        ~text:query
        ~k:num_results
    in
    let docs = Vector_db.get_docs vf corpus top_indices in
    let results =
      List.map ~f:(fun doc -> sprintf "\n**Result:**\n```ocaml\n%s\n```\n" doc) docs
    in
    String.concat ~sep:"\n" results
  in
  Ochat_function.create_function
    (module Definitions.Query_vector_db)
    (fun args -> Output.Text (f args))
;;

let apply_patch ~dir : Ochat_function.t =
  let split path =
    Eio.Path.split (dir / path)
    |> Option.map ~f:(fun ((_, dirname), basename) -> dirname, basename)
  in
  let f patch =
    let open_fn path = Io.load_doc ~dir path in
    let write_fn path s =
      match split path with
      | Some (dirname, _) ->
        (match Io.is_dir ~dir dirname with
         | true -> Io.save_doc ~dir path s
         | false ->
           Io.mkdir ~exists_ok:true ~dir dirname;
           Io.save_doc ~dir path s)
      | None -> Io.save_doc ~dir path s
    in
    let remove_fn path = Io.delete_doc ~dir path in
    match Apply_patch.process_patch ~text:patch ~open_fn ~write_fn ~remove_fn with
    | _, snippets ->
      let format_snippet (path, snip) =
        let header =
          Printf.sprintf
            "┏━[ %s ]%s"
            path
            (String.concat @@ List.init 70 ~f:(fun _ -> "-"))
        in
        let footer = String.concat @@ List.init 42 ~f:(fun _ -> "") in
        String.concat ~sep:"\n" [ header; snip; footer ]
      in
      let snippets_text =
        String.concat ~sep:"\n\n" (List.map ~f:format_snippet snippets)
      in
      Printf.sprintf "✅ Patch applied successfully!\n\n%s" snippets_text
    | exception Apply_patch.Diff_error err -> Apply_patch.error_to_string err
    | exception ex -> Fmt.str "error running apply_patch: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Apply_patch)
    (fun args -> Output.Text (f args))
;;

let read_dir ~dir : Ochat_function.t =
  let f path =
    match Io.directory ~dir path with
    | res -> String.concat ~sep:"\n" res
    | exception ex -> Fmt.str "error running read_directory: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Read_directory)
    (fun args -> Output.Text (f args))
;;

let mkdir ~dir : Ochat_function.t =
  let f path =
    match Io.mkdir ~exists_ok:true ~dir path with
    | () -> sprintf "Directory %s created successfully." path
    | exception ex -> Fmt.str "error running mkdir: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Make_dir)
    (fun args -> Output.Text (f args))
;;

(* -------------------------------------------------------------------------- *)
(* Meta-prompting – recursive refinement tool                                 *)
(* -------------------------------------------------------------------------- *)

let meta_refine ~env : Ochat_function.t =
  let f (prompt_raw, task) =
    let open Meta_prompting in
    let action =
      match String.is_empty prompt_raw with
      | true -> Context.Generate
      | false -> Context.Update
    in
    Mp_flow.first_flow ~env ~prompt:prompt_raw ~task ~action ()
  in
  Ochat_function.create_function
    (module Definitions.Meta_refine)
    (fun args -> Output.Text (f args))
;;

(* -------------------------------------------------------------------------- *)
(* ODoc search – vector-based snippet retrieval                                 *)
(* -------------------------------------------------------------------------- *)

let odoc_search ~dir ~net : Ochat_function.t =
  (*────────────────────────  Simple in-memory caches  ───────────────────────*)
  let module Odoc_cache = struct
    open Core

    module S = struct
      type t = string [@@deriving compare, hash, sexp]
    end

    let embed_tbl : (string, float array) Hashtbl.t = Hashtbl.create (module S)
    let vec_tbl : (string, Vector_db.Vec.t array) Hashtbl.t = Hashtbl.create (module S)
    let mu = Eio.Mutex.create ()

    let get_embed ~net query =
      Eio.Mutex.lock mu;
      let found = Hashtbl.find embed_tbl query in
      Eio.Mutex.unlock mu;
      match found with
      | Some v -> v
      | None ->
        let resp = Openai.Embeddings.post_openai_embeddings net ~input:[ query ] in
        let vec = Array.of_list (List.hd_exn resp.data).embedding in
        Eio.Mutex.lock mu;
        Hashtbl.set embed_tbl ~key:query ~data:vec;
        Eio.Mutex.unlock mu;
        vec
    ;;

    let get_vectors vec_file_path path_t =
      Eio.Mutex.lock mu;
      let found = Hashtbl.find vec_tbl vec_file_path in
      Eio.Mutex.unlock mu;
      match found with
      | Some v -> v
      | None ->
        let vecs =
          try Vector_db.Vec.read_vectors_from_disk path_t with
          | _ -> [||]
        in
        Eio.Mutex.lock mu;
        Hashtbl.set vec_tbl ~key:vec_file_path ~data:vecs;
        Eio.Mutex.unlock mu;
        vecs
    ;;
  end
  in
  let f (query, k_opt, index_opt, package) =
    let open Eio.Path in
    let k = Option.value k_opt ~default:5 in
    let index_dir = Option.value index_opt ~default:".odoc_index" in
    (* 1. Embed the query (cached) *)
    let query_vec = Odoc_cache.get_embed ~net query in
    let query_mat = Owl.Mat.of_array query_vec (Array.length query_vec) 1 in
    let index_path = dir / index_dir in
    (* 2. Determine candidate packages *)
    let pkgs =
      if String.equal package "all"
      then (
        match Package_index.load ~dir:index_path with
        | Some idx ->
          (match Package_index.query idx ~embedding:query_vec ~k:5 with
           | l when List.is_empty l -> Eio.Path.read_dir index_path
           | l -> l)
        | None -> Eio.Path.read_dir index_path)
      else [ package ]
    in
    (* 3. Aggregate vectors from selected packages *)
    let vectors_for_pkg pkg =
      let pkg_dir = index_path / pkg in
      if Eio.Path.is_directory pkg_dir
      then (
        let vec_path = pkg_dir / "vectors.binio" in
        let vec_key = Eio.Path.native_exn vec_path in
        let vecs = Odoc_cache.get_vectors vec_key vec_path in
        Array.to_list vecs |> List.map ~f:(fun v -> pkg, v))
      else []
    in
    let vecs_with_pkg = List.concat_map pkgs ~f:vectors_for_pkg in
    if List.is_empty vecs_with_pkg
    then Printf.sprintf "No vectors found in index directory %s" index_dir
    else (
      let only_vecs = Array.of_list (List.map vecs_with_pkg ~f:snd) in
      let db = Vector_db.create_corpus only_vecs in
      let idxs = Vector_db.query db query_mat k in
      (* 4. Fetch snippets *)
      let results =
        Array.to_list idxs
        |> List.mapi ~f:(fun rank idx ->
          let id, _len = Hashtbl.find_exn db.Vector_db.index idx in
          (* find which package contains this id *)
          let pkg_opt =
            List.find_map vecs_with_pkg ~f:(fun (pkg, v) ->
              if String.equal v.Vector_db.Vec.id id then Some pkg else None)
          in
          match pkg_opt with
          | None -> None
          | Some pkg ->
            (match
               Or_error.try_with (fun () ->
                 Io.load_doc ~dir:index_path (pkg ^ "/" ^ id ^ ".md"))
             with
             | Ok text ->
               let preview_len = 8000 in
               let preview =
                 if String.length text > preview_len
                 then String.sub text ~pos:0 ~len:preview_len ^ " …"
                 else text
               in
               Some (rank + 1, pkg, id, preview)
             | Error _ -> None))
        |> List.filter_map ~f:Fn.id
      in
      if List.is_empty results
      then "No matching snippets found"
      else
        results
        |> List.map ~f:(fun (rank, pkg, id, preview) ->
          Printf.sprintf "[%d] [%s] %s\n%s" rank pkg id preview)
        |> String.concat ~sep:"\n\n---\n\n")
  in
  Ochat_function.create_function
    (module Definitions.Odoc_search)
    ~strict:false
    (fun args -> Output.Text (f args))
;;

(* -------------------------------------------------------------------------- *)
(* Webpage → Markdown tool                                                     *)
(* -------------------------------------------------------------------------- *)

let webpage_to_markdown ~env ~dir ~net : Ochat_function.t =
  Webpage_markdown.Tool.register ~env ~dir ~net
;;

(* -------------------------------------------------------------------------- *)
(*  Fork stub – placeholder implementation                                     *)
(* -------------------------------------------------------------------------- *)

let fork : Ochat_function.t =
  let impl (_ : Definitions.Fork.input) =
    "[fork-tool placeholder – should never be called directly]"
  in
  Ochat_function.create_function
    (module Definitions.Fork)
    (fun args -> Output.Text (impl args))
;;

(* -------------------------------------------------------------------------- *)
(* Markdown indexing – build vector store                                      *)
(* -------------------------------------------------------------------------- *)

let index_markdown_docs ~env ~dir : Ochat_function.t =
  let f (root, index_name, description, vector_db_root_opt) =
    let root_path = Eio.Path.(dir / root) in
    let vector_db_root = Option.value vector_db_root_opt ~default:".md_index" in
    try
      Markdown_indexer.index_directory
        ~vector_db_root
        ~env
        ~index_name
        ~description
        ~root:root_path;
      "Markdown documents have been indexed successfully."
    with
    | ex -> Fmt.str "error indexing markdown docs: %a" Eio.Exn.pp ex
  in
  Ochat_function.create_function
    (module Definitions.Index_markdown_docs)
    (fun args -> Output.Text (f args))
;;

(* -------------------------------------------------------------------------- *)
(* Markdown search – semantic retrieval                                        *)
(* -------------------------------------------------------------------------- *)

let markdown_search ~dir ~net : Ochat_function.t =
  (*────────────────────────  Simple in-memory caches  ───────────────────────*)
  let module Md_cache = struct
    open Core

    module S = struct
      type t = string [@@deriving compare, hash, sexp]
    end

    let embed_tbl : (string, float array) Hashtbl.t = Hashtbl.create (module S)
    let vec_tbl : (string, Vector_db.Vec.t array) Hashtbl.t = Hashtbl.create (module S)
    let mu = Eio.Mutex.create ()

    let get_embed ~net query =
      Eio.Mutex.lock mu;
      let found = Hashtbl.find embed_tbl query in
      Eio.Mutex.unlock mu;
      match found with
      | Some v -> v
      | None ->
        let resp = Openai.Embeddings.post_openai_embeddings net ~input:[ query ] in
        let vec = Array.of_list (List.hd_exn resp.data).embedding in
        Eio.Mutex.lock mu;
        Hashtbl.set embed_tbl ~key:query ~data:vec;
        Eio.Mutex.unlock mu;
        vec
    ;;

    let get_vectors vec_file_path path_t =
      Eio.Mutex.lock mu;
      let found = Hashtbl.find vec_tbl vec_file_path in
      Eio.Mutex.unlock mu;
      match found with
      | Some v -> v
      | None ->
        let vecs =
          try Vector_db.Vec.read_vectors_from_disk path_t with
          | _ -> [||]
        in
        Eio.Mutex.lock mu;
        Hashtbl.set vec_tbl ~key:vec_file_path ~data:vecs;
        Eio.Mutex.unlock mu;
        vecs
    ;;
  end
  in
  let f (query, k_opt, index_name_opt, vector_db_root_opt) =
    let open Eio.Path in
    let k = Option.value k_opt ~default:5 in
    let vector_db_root = Option.value vector_db_root_opt ~default:".md_index" in
    let index_dir = dir / vector_db_root in
    (* 1. Embed query *)
    let query_vec = Md_cache.get_embed ~net query in
    let query_mat = Owl.Mat.of_array query_vec (Array.length query_vec) 1 in
    (* 2. Determine candidate indexes *)
    let indexes =
      match index_name_opt with
      | Some name when not (String.equal name "all") -> [ name ]
      | _ ->
        (match Md_index_catalog.load ~dir:index_dir with
         | Some catalog ->
           (* Compute similarity and sort *)
           let scores =
             Array.map catalog ~f:(fun { Md_index_catalog.Entry.name; vector; _ } ->
               let score =
                 Array.fold2_exn query_vec vector ~init:0.0 ~f:(fun acc q v ->
                   acc +. (q *. v))
               in
               score, name)
           in
           scores
           |> Array.to_list
           |> List.sort ~compare:(fun (s1, _) (s2, _) -> Float.compare s2 s1)
           |> (fun l -> List.take l 5)
           |> List.map ~f:snd
         | None ->
           (* fallback list all dirs *)
           List.filter (Eio.Path.read_dir index_dir) ~f:(fun entry ->
             Eio.Path.is_directory (index_dir / entry)))
    in
    if List.is_empty indexes
    then Printf.sprintf "No Markdown indices found under %s" vector_db_root
    else (
      (* 3. Aggregate vectors from selected indexes *)
      let vecs_with_index =
        List.concat_map indexes ~f:(fun idx_name ->
          let idx_dir = index_dir / idx_name in
          if is_directory idx_dir
          then (
            let vec_path = idx_dir / "vectors.binio" in
            let vec_key = native_exn vec_path in
            let vecs = Md_cache.get_vectors vec_key vec_path in
            Array.to_list vecs |> List.map ~f:(fun v -> idx_name, v))
          else [])
      in
      if List.is_empty vecs_with_index
      then Printf.sprintf "No vectors found in selected indices"
      else (
        let only_vecs = Array.of_list (List.map vecs_with_index ~f:snd) in
        let db = Vector_db.create_corpus only_vecs in
        let idxs = Vector_db.query db query_mat k in
        let results =
          Array.to_list idxs
          |> List.mapi ~f:(fun rank idx ->
            let id, _len = Hashtbl.find_exn db.Vector_db.index idx in
            (* which index has this id *)
            let idx_opt =
              List.find_map vecs_with_index ~f:(fun (idx_name, v) ->
                if String.equal v.Vector_db.Vec.id id then Some idx_name else None)
            in
            match idx_opt with
            | None -> None
            | Some idx_name ->
              (match
                 Or_error.try_with (fun () ->
                   Io.load_doc ~dir:index_dir (idx_name ^ "/snippets/" ^ id ^ ".md"))
               with
               | Ok text ->
                 let preview_len = 8000 in
                 let preview =
                   if String.length text > preview_len
                   then String.sub text ~pos:0 ~len:preview_len ^ " …"
                   else text
                 in
                 Some (rank + 1, idx_name, id, preview)
               | Error _ -> None))
          |> List.filter_map ~f:Fn.id
        in
        if List.is_empty results
        then "No matching snippets found"
        else
          results
          |> List.map ~f:(fun (rank, idx_name, id, preview) ->
            Printf.sprintf "[%d] [%s] %s\n%s" rank idx_name id preview)
          |> String.concat ~sep:"\n\n---\n\n"))
  in
  Ochat_function.create_function
    (module Definitions.Markdown_search)
    ~strict:false
    (fun args -> Output.Text (f args))
;;

let import_image ~dir : Ochat_function.t =
  let f image_path =
    let open Eio.Path in
    let img_full_path = dir / image_path in
    if not (is_file img_full_path)
    then Output.Text (Printf.sprintf "Image file %s does not exist." image_path)
    else (
      let image_url = Io.Base64.file_to_data_uri ~dir image_path in
      Output.Content [ Input_image { image_url; detail = Some Auto } ])
  in
  Ochat_function.create_function (module Definitions.Import_image) f
;;
|RAW
</tool_response>
<tool_response tool_call_id="call_LsplE3opnL2md7jLOhQ0ERol">
RAW|
(** Typed representation and parser for {e ChatMarkdown} prompts.

    {b ChatMarkdown} is a lightweight XML dialect used in this code-base to
    describe conversations for Large Language Models (LLMs).  The purpose of
    {!module:Chat_markdown} is twofold:

    • expose a zero-cost {e strongly-typed} view of the language so that
      downstream modules can pattern-match on variants instead of inspecting
      raw strings;
    • offer a single helper – {!val:parse_chat_inputs} – to parse, resolve
      imports, and convert a complete document into a list of OCaml records
      ready to be serialised to the OpenAI chat API.

    All public types derive [@@deriving jsonaf] as well as [sexp], [compare],
    [hash] and [bin_io] for seamless debugging and persistence.  Unknown or
    future ChatMarkdown tags are preserved verbatim inside [`Text`]
    placeholders, guaranteeing forward-compatibility. *)

module Chat_markdown : sig
  type function_call =
    { name : string
    ; arguments : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool_call =
    { id : string
    ; function_ : function_call
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type image_url = { url : string } [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* A single item of content, which can be text or an image. *)
  type basic_content_item =
    { type_ : string [@key "type"]
    ; text : string option [@jsonaf.option]
    ; image_url : image_url option [@jsonaf.option]
    ; document_url : string option [@jsonaf.option]
    ; is_local : bool [@default false]
    ; cleanup_html : bool [@default false]
    ; markdown : bool [@default false] (* whether to convert HTML to Markdown *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* Agent content: has a url, is_local, and sub-items. *)
  type agent_content =
    { url : string
    ; is_local : bool
    ; items : content_item list [@default []]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* content_item can be either a Basic variant or an Agent variant. *)
  and content_item =
    | Basic of basic_content_item
    | Agent of agent_content
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* The overall content can be either a single string or a list of items. *)
  type chat_message_content =
    | Text of string
    | Items of content_item list
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type reasoning_summary =
    { text : string
    ; _type : string (* usually "summary" *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type reasoning =
    { summary : reasoning_summary list
    ; id : string
    ; status : string option
    ; _type : string (* always "reasoning" *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* A generic <msg role="…"> element.  Still used for legacy messages
     (e.g. roles other than the four specialised shorthands).
     NOTE:  The newer shorthand tags (<user/>, <assistant/>, <tool_call/>,
     <tool_response/>) are mapped to dedicated OCaml record types that are
     aliases of [msg].  This removes the need to inspect the [role] string
     when traversing the parse-tree, while keeping the underlying shape
     identical so existing logic can be reused. *)

  type msg =
    { role : string
    ; type_ : string option [@key "type"] [@jsonaf.option]
    ; content : chat_message_content option [@jsonaf.option]
    ; name : string option [@jsonaf.option]
    ; id : string option [@jsonaf.option]
    ; status : string option [@jsonaf.option]
    ; function_call : function_call option [@jsonaf.option]
    ; tool_call : tool_call option [@jsonaf.option]
    ; tool_call_id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* Dedicated message records for the new shorthand tags.  They are simple
     aliases so that the JSON / serialisation helpers generated for [msg]
     can be re-used without code duplication. *)

  type user_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type assistant_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_call_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_response_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type developer_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type system_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type custom_tool =
    { name : string
    ; description : string option
    ; command : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type agent_tool =
    { name : string
    ; description : string option
    ; agent : string
    ; is_local : bool
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type mcp_tool =
    { names : string list option
    ; description : string option
    ; mcp_server : string
    ; strict : bool
    ; client_id_env : string option
    ; client_secret_env : string option
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool =
    | Builtin of string
    | Custom of custom_tool
    | Agent of agent_tool
    | Mcp of mcp_tool
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type config =
    { max_tokens : int option [@jsonaf.option]
    ; model : string option [@jsonaf.option]
    ; reasoning_effort : string option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
    ; show_tool_call : bool
    ; id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type top_level_elements =
    | Msg of msg (** Legacy <msg/> element (system, developer…) *)
    | Developer of developer_msg
    | System of system_msg
    | User of user_msg (** <user/> *)
    | Assistant of assistant_msg (** <assistant/> *)
    | Tool_call of tool_call_msg (** <tool_call/> *)
    | Tool_response of tool_response_msg (** <tool_response/> *)
    | Config of config
    | Reasoning of reasoning
    | Tool of tool
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (** [parse_chat_inputs ~dir raw] tokenises, parses and normalises the
      ChatMarkdown snippet contained in [raw].

      The result preserves the original order of logical messages and is
      ready to feed into the OpenAI chat API or any compatible backend.

      Parameters:
      • [dir] – base directory against which relative paths are resolved
        (imports, local images, nested agent prompts).  The directory is a
        standard Eio capability, making the function safe with respect to
        the ambient file-system.
      • [raw] – the UTF-8 text to parse.  It can be a whole document or a
        fragment; leading BOM and surrounding whitespace are ignored.

      Behaviour:
      1. Preprocesses the input via {!Preprocessor.preprocess} to strip
         comments and handle conditional compilation markers.
      2. Parses the cleaned source with the Menhir grammar from
         {!module:Chatmd_parser}.
      3. Expands [`<import>`] directives recursively.
      4. Converts AST nodes into strongly-typed
         {!type:Chat_markdown.top_level_elements} values, mapping shorthand
         tags (`<user>`, `<assistant>` & co.) to dedicated aliases.
      5. Filters out nodes that are irrelevant to the conversation
         (e.g. stray whitespace between top-level blocks).

      @raise Failure  If the source is not valid ChatMarkdown or if an
                      imported resource cannot be read. *)
  val parse_chat_inputs
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> string
    -> top_level_elements list
end

(** {1 Metadata helpers}
    Attach key/value metadata to any top-level element without changing
    existing record definitions.  The data lives in an external
    registry so serialisation of prompts is unaffected. *)

module Metadata : sig
  (** [add elt ~key ~value] attaches the metadata pair [(key, value)] to
      [elt].  If the same [key] already exists it is appended 
      (i.e. multiple values per key are allowed).  The call mutates a
      global in-memory table; it has no effect on serialisation. *)
  val add : Chat_markdown.top_level_elements -> key:string -> value:string -> unit

  (** [get elt] returns all key/value metadata associated with [elt] or
      [None] if no entry is present.  The list preserves the insertion
      order, most-recent first.  Mutating the returned list does **not**
      update the registry. *)
  val get : Chat_markdown.top_level_elements -> (string * string) list option

  (** [set elt kvs] replaces the whole metadata list of [elt] with [kvs].
      Use {!val:add} if you only need to add a single pair. *)
  val set : Chat_markdown.top_level_elements -> (string * string) list -> unit

  (** [clear ()] removes **all** stored metadata for **every** element.
      Call it at the end of a request to avoid memory leaks in long-running
      processes. *)
  val clear : unit -> unit
end
|RAW
</tool_response>
<tool_response tool_call_id="call_Jv2vbCmTtWcFOZl9rRqJLIfo">
RAW|
open Core
open Jsonaf.Export
module Ast = Chatmd_ast

let parse str =
  let lexbuf = Lexing.from_string str in
  Chatmd_parser.document Chatmd_lexer.token lexbuf
;;

module Import_expansion = struct
  open Ast

  let can_have_imports tag =
    match tag with
    | User | Agent | System | Developer -> true
    | _ -> false
  ;;

  (* A node is either a text node or an element with attributes and children. *)
  let rec expand_imports ~dir (nodes : document) : document =
    List.concat_map nodes ~f:(function
      | Element (Import, attrs, _) ->
        let src = List.Assoc.find attrs ~equal:String.equal "src" in
        (match src with
         | Some (Some src) ->
           let imported = parse @@ Io.load_doc ~dir src in
           expand_imports ~dir imported
         | _ -> [])
      | Element (Msg, attrs, children) ->
        let role = List.Assoc.find attrs ~equal:String.equal "role" in
        (match role with
         | Some (Some "user") | Some (Some "system") | Some (Some "developer") ->
           let expanded = expand_imports ~dir children in
           [ Element (Msg, attrs, expanded) ]
         | _ -> [ Element (Msg, attrs, children) ])
      | Element (tag, attrs, children) ->
        (match can_have_imports tag with
         | true ->
           let expanded_children = expand_imports ~dir children in
           [ Element (tag, attrs, expanded_children) ]
         | false -> [ Element (tag, attrs, children) ])
      | Text _ as txt -> [ txt ])
  ;;
end

module Chat_content = struct
  (* Minimal “image_url” type, analogous to what you might have in the OpenAI API code. *)
  type image_url = { url : string } [@@deriving sexp, jsonaf, hash, bin_io, compare]

  (* A single item of content, which can be text or an image or doc. *)
  type basic_content_item =
    { type_ : string [@key "type"]
    ; text : string option [@jsonaf.option]
    ; image_url : image_url option [@jsonaf.option]
    ; document_url : string option [@jsonaf.option]
    ; is_local : bool [@default false]
    ; cleanup_html : bool [@default false]
    ; markdown : bool [@default false] (* whether to convert HTML to Markdown *)
    }
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  (* Agent content: has a url, is_local, and sub-items. *)
  type agent_content =
    { url : string
    ; is_local : bool
    ; items : content_item list [@default []]
    }
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  (* content_item can be either a Basic variant or an Agent variant. *)
  and content_item =
    | Basic of basic_content_item
    | Agent of agent_content
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  type content_item_list = content_item list
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  type chat_message_content =
    | Text of string
    | Items of content_item list
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  let chat_message_content_of_jsonaf (j : Jsonaf.t) =
    match j with
    | `String s -> Text s
    | `Array _ -> Items (list_of_jsonaf content_item_of_jsonaf j)
    | _ -> failwith "chat_message_content_of_jsonaf: expected string or array of items."
  ;;

  let jsonaf_of_chat_message_content = function
    | Text s -> `String s
    | Items items -> jsonaf_of_list jsonaf_of_content_item items
  ;;

  type function_call =
    { name : string
    ; arguments : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool_call =
    { id : string
    ; function_ : function_call
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type msg =
    { role : string
    ; type_ : string option [@key "type"] [@jsonaf.option]
    ; content : chat_message_content option
          [@jsonaf.option]
          [@jsonaf.of chat_message_content_of_jsonaf]
          [@jsonaf.to jsonaf_of_chat_message_content]
    ; name : string option [@jsonaf.option]
    ; id : string option [@jsonaf.option] (* NEW *)
    ; status : string option [@jsonaf.option] (* NEW *)
    ; function_call : function_call option
          [@jsonaf.option]
          (* DEPRECATED AND NO LONGER USED> TO BE REMOVED USED tool_call for function calls *)
    ; tool_call : tool_call option [@jsonaf.option]
    ; tool_call_id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* Alias types for the new shorthand message variants.  We deliberately
     make them plain aliases so that they share the serialisation helpers
     with [msg] and we do not need to duplicate conversion logic. *)

  type user_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type assistant_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_call_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_response_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type developer_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type system_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type custom_tool =
    { name : string
    ; description : string option
    ; command : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* A tool that proxies its invocation to a secondary chatmd “agent” prompt. *)
  type agent_tool =
    { name : string
    ; description : string option
    ; agent : string (** URL or path to the agent chatmd file *)
    ; is_local : bool (** whether the agent file lives on disk *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool =
    | Builtin of string
    | Custom of custom_tool
    | Agent of agent_tool
    (* A tool exposed by a remote MCP server. *)
    | Mcp of mcp_tool
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  and mcp_tool =
    { names : string list option
    ; description : string option
    ; mcp_server : string (** URI of the MCP server hosting the tool *)
    ; strict : bool (** whether to enforce strict parameter matching *)
    ; client_id_env : string option [@jsonaf.option] (** env var holding client_id *)
    ; client_secret_env : string option [@jsonaf.option]
      (** env var holding client_secret *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* The config element. *)
  type config =
    { max_tokens : int option [@jsonaf.option]
    ; model : string option [@jsonaf.option]
    ; reasoning_effort : string option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
    ; show_tool_call : bool
    ; id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type reasoning_summary =
    { text : string
    ; _type : string
    }
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  type reasoning =
    { summary : reasoning_summary list
    ; id : string
    ; status : string option
    ; _type : string
    }
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  type top_level_elements =
    | Msg of msg
    | Developer of developer_msg
    | System of system_msg
    | User of user_msg
    | Assistant of assistant_msg
    | Tool_call of tool_call_msg
    | Tool_response of tool_response_msg
    | Config of config
    | Reasoning of reasoning
    | Tool of tool
  [@@deriving jsonaf, sexp, hash, bin_io, compare]
end

module Chat_markdown = struct
  include Chat_content

  (* The internal chat_element used while building the final messages. *)
  type chat_element =
    | Message of msg
    | Developer_msg of developer_msg
    | System_msg of system_msg
    | User_msg of user_msg
    | Assistant_msg of assistant_msg
    | Tool_call_msg of tool_call_msg
    | Tool_response_msg of tool_response_msg
    | Config of config
    | Tool of tool
    | Reasoning of reasoning
    | Summary of reasoning_summary
    | Text of string
    | Image of string * bool
    | Document of string * bool * bool * bool (* url, is_local, cleanup_html, markdown *)
    | Agent of string (* url *) * bool (* is_local *) * chat_element list
  (* Convert a <msg> element’s children into an Items or a single Text. *)

  let rec content_items_of_elements (elts : chat_element list)
    : Chat_content.content_item list
    =
    match elts with
    | [] -> []
    | Text s :: rest ->
      Basic
        { type_ = "text"
        ; text = Some s
        ; image_url = None
        ; document_url = None
        ; is_local = false
        ; cleanup_html = false
        ; markdown = false
        }
      :: content_items_of_elements rest
    | Image (u, loc) :: rest ->
      Basic
        { type_ = "image_url"
        ; text = None
        ; image_url = Some { url = u }
        ; document_url = None
        ; is_local = loc
        ; cleanup_html = false
        ; markdown = false
        }
      :: content_items_of_elements rest
    | Document (u, loc, cln, md) :: rest ->
      Basic
        { type_ = "text"
        ; text = None
        ; image_url = None
        ; document_url = Some u
        ; is_local = loc
        ; cleanup_html = cln
        ; markdown = md
        }
      :: content_items_of_elements rest
    | Agent (u, loc, ch) :: rest ->
      Agent { url = u; is_local = loc; items = content_items_of_elements ch }
      :: content_items_of_elements rest
    | ( Message _
      | Developer_msg _
      | System_msg _
      | User_msg _
      | Assistant_msg _
      | Tool_call_msg _
      | Tool_response_msg _
      | Config _
      | Reasoning _
      | Summary _
      | Tool _ )
      :: rest -> content_items_of_elements rest
  ;;

  (* Actually parse the child elements to produce a (Text ...) or (Items ...). *)
  (* Converts a list of child chat_elements into either a single text or a list
     of content items, stored in chat_message_content. *)
  let parse_msg_children (children : chat_element list) : chat_message_content option =
    let items = content_items_of_elements children in
    match items with
    | [] -> None
    (* If there is exactly one Basic text item, store as Text. Otherwise use Items. *)
    | [ Basic { type_ = "text"; text = Some txt; _ } ] -> Some (Text txt)
    | _ -> Some (Items items)
  ;;

  (* Build a msg record from the attributes on <msg>. *)
  let attr_to_msg attr (content : chat_message_content option) : msg =
    let hash_tbl = Hashtbl.create (module String) in
    List.iter attr ~f:(fun (attr_name, value) ->
      Hashtbl.set hash_tbl ~key:attr_name ~data:value);
    (* deprecated and not used *)
    let function_call, content_opt =
      match Hashtbl.mem hash_tbl "function_call" with
      | false -> None, content
      | true ->
        let name = Hashtbl.find_exn hash_tbl "function_name" in
        let arguments =
          match content with
          | Some (Text t) -> t
          | _ ->
            failwith
              "Expected function_call to be raw text arguments; found structured content."
        in
        Some { name; arguments }, content
    in
    (* new way to handle tool calls *)
    let tool_call, content_opt =
      match function_call with
      | Some _ -> None, content_opt
      | None ->
        if Hashtbl.mem hash_tbl "tool_call"
        then (
          let name = Hashtbl.find_exn hash_tbl "function_name" in
          let id = Hashtbl.find_exn hash_tbl "tool_call_id" in
          let arguments =
            match content_opt with
            | Some (Text t) -> t
            | Some (Items _) -> ""
            | _ ->
              failwith
                "Expected tool_call to be raw text arguments or structured content."
          in
          Some { id; function_ = { name; arguments } }, content_opt)
        else None, content_opt
    in
    { role = Hashtbl.find_exn hash_tbl "role"
    ; type_ = Hashtbl.find hash_tbl "type"
    ; name = Hashtbl.find hash_tbl "name"
    ; id = Hashtbl.find hash_tbl "id" (* NEW *)
    ; status = Hashtbl.find hash_tbl "status" (* NEW *)
    ; function_call
    ; tool_call
    ; content = content_opt
    ; tool_call_id = Hashtbl.find hash_tbl "tool_call_id"
    }
  ;;

  (* Helper to turn a chat_element back to string (for unrecognized markup). *)
  let rec chat_element_to_string = function
    | Summary s -> s.text
    | Reasoning r ->
      let ss = List.map r.summary ~f:(fun s -> s.text) |> String.concat ~sep:" " in
      Printf.sprintf "<reasoning id=\"%s\">%s</reasoning>" r.id ss
    | Agent (url, is_local, children) ->
      let sub_items = List.map children ~f:chat_element_to_string in
      Printf.sprintf
        "<agent src=\"%s\" local=\"%b\">%s</agent>"
        url
        is_local
        (String.concat ~sep:"" sub_items)
    | Text s -> s
    | Image (url, is_local) ->
      if is_local
      then Printf.sprintf "<img src=\"%s\" local=\"true\" />" url
      else Printf.sprintf "<img src=\"%s\" />" url
    | Document (url, local, cleanup, markdown) ->
      let local_attr = if local then " local=\"true\"" else "" in
      let strip_attr = if cleanup then " strip=\"true\"" else "" in
      let md_attr = if markdown then " markdown=\"true\"" else "" in
      Printf.sprintf "<doc src=\"%s\"%s%s%s />" url local_attr strip_attr md_attr
    | Config { max_tokens; model; reasoning_effort; temperature; show_tool_call; id } ->
      let attrs =
        [ Option.map max_tokens ~f:(fun n -> Printf.sprintf "max_tokens=\"%d\"" n)
        ; Option.map model ~f:(fun m -> Printf.sprintf "model=\"%s\"" m)
        ; Option.map reasoning_effort ~f:(fun r ->
            Printf.sprintf "reasoning_effort=\"%s\"" r)
        ; Option.map temperature ~f:(fun t -> Printf.sprintf "temperature=\"%.3f\"" t)
        ; Some (Printf.sprintf "show_tool_call=\"%b\"" show_tool_call)
        ; Option.map id ~f:(fun id -> Printf.sprintf "id=\"%s\"" id)
        ]
        |> List.filter_map ~f:Fun.id
      in
      let attrs_string =
        if List.is_empty attrs then "" else " " ^ String.concat ~sep:" " attrs
      in
      Printf.sprintf "<config%s />" attrs_string
    | Tool t ->
      (match t with
       | Builtin name -> Printf.sprintf "<tool name=\"%s\" />" name
       | Custom { name; description; command } ->
         let desc_attr =
           match description with
           | Some d -> Printf.sprintf " description=\"%s\"" d
           | None -> ""
         in
         Printf.sprintf "<tool name=\"%s\"%s command=\"%s\" />" name desc_attr command
       | Agent { name; description; agent; is_local } ->
         let desc_attr =
           Option.value_map description ~default:"" ~f:(fun d ->
             Printf.sprintf " description=\"%s\"" d)
         in
         let local_attr = if is_local then " local" else "" in
         Printf.sprintf
           "<tool name=\"%s\"%s agent=\"%s\"%s />"
           name
           desc_attr
           agent
           local_attr
       | Mcp { names; description; mcp_server; strict; _ } ->
         let strict_attr = if strict then " strict" else "" in
         (* If the description is present, add it as an attribute. *)
         let desc_attr =
           Option.value_map description ~default:"" ~f:(fun d ->
             Printf.sprintf " description=\"%s\"" d)
         in
         let names_str =
           match names with
           | Some names ->
             "includes=\"" ^ (String.concat ~sep:", " names |> String.escaped) ^ "\""
           | None -> ""
         in
         Printf.sprintf
           "<tool %s%s mcp_server=\"%s\"%s />"
           names_str
           desc_attr
           mcp_server
           strict_attr)
    | Developer_msg m
    | System_msg m
    | Message m
    | User_msg m
    | Assistant_msg m
    | Tool_call_msg m
    | Tool_response_msg m ->
      (match m.content with
       | Some (Text t) -> t
       | Some (Items items) ->
         let rec aux it =
           match it with
           | Basic it ->
             (match it.type_ with
              | "text" -> Option.value it.text ~default:""
              | "image_url" ->
                (match it.image_url with
                 | Some { url } -> Printf.sprintf "<img src=\"%s\" />" url
                 | None -> "")
              | _ -> Option.value it.text ~default:"")
           | Agent { url; is_local; items } ->
             let pieces = List.map items ~f:aux in
             Printf.sprintf
               "<agent src=\"%s\" local=\"%b\">%s</agent>"
               url
               is_local
               (String.concat ~sep:"" pieces)
         in
         let pieces = List.map items ~f:aux in
         String.concat ~sep:"" pieces
       | None -> "")
  ;;

  (*--------------------------------------------------------------------------*)
  (* Generic tree fold                                                        *)
  (*--------------------------------------------------------------------------*)

  (** [tree node ~f] traverses [node] depth-first and applies the combining
    function [f] to each node together with the list of results that were
    produced for its direct children.  This is analogous to a fold over the
    tree structure.

    For example, to collect all nodes in a tree one can write

    {[ let all_nodes = tree root ~f:(fun n children -> n :: List.concat children) ]}

    The traversal is depth-first and children are processed from left to right,
    mirroring their order in the underlying list. *)
  let rec tree (node : Ast.node) ~(f : Ast.node -> 'a list -> 'a) : 'a =
    match node with
    | Text _ -> f node []
    | Element (_, _, children) ->
      let child_results = List.map children ~f:(fun child -> tree child ~f) in
      f node child_results
  ;;

  (* The Markup.ml “tree” transformation that identifies <msg> or <config> elements
     and returns them as Chat_parser.chat_element variants. *)
  let parse_chat_element node =
    tree node ~f:(fun node children ->
      match node with
      | Element (Msg, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        Message (attr_to_msg attr content_opt)
      | Element (Developer, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "developer" in
        let attrs = role_attr :: attr in
        Developer_msg (attr_to_msg attrs content_opt)
      | Element (System, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "system" in
        let attrs = role_attr :: attr in
        System_msg (attr_to_msg attrs content_opt)
      | Element (User, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "user" in
        let attrs = role_attr :: attr in
        User_msg (attr_to_msg attrs content_opt)
      | Element (Assistant, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "assistant" in
        let attrs = role_attr :: attr in
        Assistant_msg (attr_to_msg attrs content_opt)
      | Element (Tool_call, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "assistant" in
        let tool_call_attr = "tool_call", "true" in
        let attrs = role_attr :: tool_call_attr :: attr in
        Tool_call_msg (attr_to_msg attrs content_opt)
      | Element (Tool_response, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "tool" in
        let attrs = role_attr :: attr in
        Tool_response_msg (attr_to_msg attrs content_opt)
      | Element (Img, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (nm, v) -> Hashtbl.set tbl ~key:nm ~data:v);
        let url = Option.value (Hashtbl.find tbl "src") ~default:"" in
        let is_local = Hashtbl.mem tbl "local" in
        Image (url, is_local)
      | Element (Doc, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (nm, v) -> Hashtbl.set tbl ~key:nm ~data:v);
        let url = Option.value (Hashtbl.find tbl "src") ~default:"" in
        let local = Hashtbl.mem tbl "local" in
        let strip = Hashtbl.mem tbl "strip" in
        let md = Hashtbl.mem tbl "markdown" in
        Document (url, local, strip, md)
      | Element (Config, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (nm, v) -> Hashtbl.set tbl ~key:nm ~data:v);
        let max_tokens = Option.map (Hashtbl.find tbl "max_tokens") ~f:Int.of_string in
        let model = Hashtbl.find tbl "model" in
        let reasoning_effort = Hashtbl.find tbl "reasoning_effort" in
        let temperature =
          Option.map (Hashtbl.find tbl "temperature") ~f:Float.of_string
        in
        let show_tool_call = Hashtbl.mem tbl "show_tool_call" in
        let id = Hashtbl.find tbl "id" in
        Config { max_tokens; model; reasoning_effort; temperature; show_tool_call; id }
      | Element (Summary, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let typ =
          List.find_map attr ~f:(fun (n, v) ->
            if String.equal n "type" then Some v else None)
          |> Option.value ~default:"summary_text"
        in
        let txt =
          List.map children ~f:chat_element_to_string
          |> String.concat ~sep:""
          |> String.strip
        in
        Summary { text = txt; _type = typ }
      | Element (Reasoning, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (n, v) -> Hashtbl.set tbl ~key:n ~data:v);
        let id = Hashtbl.find_exn tbl "id" in
        let status = Hashtbl.find tbl "status" in
        let summaries =
          List.filter_map children ~f:(function
            | Summary s -> Some s
            | Text t when not (String.is_empty (String.strip t)) ->
              Some { text = String.strip t; _type = "summary_text" }
            | _ -> None)
        in
        Reasoning { id; status; _type = "reasoning"; summary = summaries }
      | Element (Agent, attrs, __bin_read_content_item__) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let url_attr =
          List.find_map attr ~f:(fun (nm, v) ->
            if String.equal nm "src" then Some v else None)
        in
        let agent_url = Option.value url_attr ~default:"" in
        let agent_is_local = List.exists attr ~f:(fun (nm, _) -> String.(nm = "local")) in
        Agent (agent_url, agent_is_local, children)
      | Element (Tool, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (nm, v) -> Hashtbl.set tbl ~key:nm ~data:v);
        let name = Hashtbl.find tbl "name" |> Option.value ~default:"" |> String.strip in
        let command = Hashtbl.find tbl "command" in
        let agent = Hashtbl.find tbl "agent" in
        let mcp_server = Hashtbl.find tbl "mcp_server" in
        let description = Hashtbl.find tbl "description" in
        let is_local = Hashtbl.mem tbl "local" in
        (match command, agent, mcp_server with
         | Some _, Some _, _ | Some _, _, Some _ | _, Some _, Some _ ->
           failwith
             "<tool> cannot combine 'command', 'agent' and 'mcp_server' attributes."
         | Some cmd, None, None ->
           if String.is_empty name then failwith "Tool name cannot be empty.";
           let cmd = String.strip cmd in
           if String.is_empty cmd then failwith "Tool command cannot be empty.";
           let description = Option.map description ~f:String.strip in
           Tool (Custom { name; description; command = cmd })
         | None, Some agent_url, None ->
           if String.is_empty name then failwith "Tool name cannot be empty.";
           let agent_url = String.strip agent_url in
           if String.is_empty agent_url then failwith "Tool agent URL cannot be empty.";
           let description = Option.map description ~f:String.strip in
           Tool (Agent { name; description; agent = agent_url; is_local })
         | None, None, Some mcp_uri ->
           let mcp_uri = String.strip mcp_uri in
           if String.is_empty mcp_uri then failwith "Tool mcp_server URI cannot be empty.";
           let description = Option.map description ~f:String.strip in
           let strict = Hashtbl.mem tbl "strict" in
           let client_id_env = Hashtbl.find tbl "client_id_env" in
           let client_secret_env = Hashtbl.find tbl "client_secret_env" in
           (* Accept both [include] and [includes] as attribute names to avoid
              confusion.  If both are present we prefer the more specific
              [include] spelling. *)
           let include_ =
             match Hashtbl.find tbl "include" with
             | Some v -> String.strip v
             | None ->
               Hashtbl.find tbl "includes" |> Option.value ~default:"" |> String.strip
           in
           let names =
             if not @@ String.is_empty name
             then Some [ name ]
             else if not @@ String.is_empty include_
             then Some (String.split ~on:',' include_ |> List.map ~f:String.strip)
             else None
           in
           Tool
             (Mcp
                { names
                ; description
                ; mcp_server = mcp_uri
                ; strict
                ; client_id_env
                ; client_secret_env
                })
         | None, None, None ->
           if String.is_empty name
           then failwith "Tool name cannot be empty."
           else Tool (Builtin name))
      | Element (Import, attrs, _) ->
        let attr_to_string (n, v) =
          Printf.sprintf "%s=\"%s\"" n (Option.value v ~default:"")
        in
        let attr = List.map attrs ~f:attr_to_string in
        let raw_content =
          Printf.sprintf "<%s %s/>" "import" (String.concat ~sep:" " attr)
        in
        Text raw_content
      | Text t -> Text t)
  ;;

  (* We only want to capture top‐level <msg> or <config>. So we scan the stream
     for those elements, parse them with parse_chat_elements, then flatten. *)
  let chat_elements document =
    List.filter document ~f:(function
      | Ast.Element (Msg, _, _)
      | Element (Developer, _, _)
      | Element (System, _, _)
      | Element (User, _, _)
      | Element (Assistant, _, _)
      | Element (Tool_call, _, _)
      | Element (Tool_response, _, _)
      | Element (Config, _, _)
      | Element (Reasoning, _, _)
      | Element (Tool, _, _) -> true
      | _ -> false)
  ;;

  (* Transform the final “Maybe chat_element” from parse_chat_elements
     into top_level_elements we can store. *)
  let to_top_level = function
    | Message m -> Some (Msg m)
    | User_msg m -> Some (User m)
    | Assistant_msg m -> Some (Assistant m)
    | Tool_call_msg m -> Some (Tool_call m)
    | Tool_response_msg m -> Some (Tool_response m)
    | Config c -> Some (Config c)
    | Reasoning r -> Some (Reasoning r)
    | Tool t -> Some (Tool t)
    | Developer_msg m -> Some (Developer m)
    | System_msg m -> Some (System m) (* System is a legacy alias for Developer *)
    | _ -> None
  ;;

  let of_chat_elements (elts : chat_element list) : top_level_elements list =
    List.filter_map elts ~f:to_top_level
  ;;

  let parse_chat_inputs ~dir (xml_content : string) : top_level_elements list =
    let xml_content = Meta_prompting.Preprocessor.preprocess xml_content in
    let document = parse xml_content in
    let expanded = Import_expansion.expand_imports ~dir document in
    let chat_elements = chat_elements expanded in
    let parsed_elements = List.map ~f:parse_chat_element chat_elements in
    of_chat_elements parsed_elements
  ;;
end

(** ------------------------------------------------------------------ *)

(** {1 Metadata helpers}  *)

module Metadata = struct
  open Core

  module CM = struct
    type t = Chat_markdown.top_level_elements

    let hash = Chat_markdown.hash_top_level_elements
    let compare = Chat_markdown.compare_top_level_elements
    let sexp_of_t = Chat_markdown.sexp_of_top_level_elements
    let t_of_sexp = Chat_markdown.top_level_elements_of_sexp
  end

  module Table = Hashtbl.Make (CM)

  let store : (string * string) list Table.t = Table.create ~size:16 ()

  let add element ~key ~value =
    let existing = Hashtbl.find store element |> Option.value ~default:[] in
    Hashtbl.set store ~key:element ~data:((key, value) :: existing)
  ;;

  let get element = Hashtbl.find store element
  let set element kvs = Hashtbl.set store ~key:element ~data:kvs
  let clear () = Hashtbl.clear store
end
|RAW
</tool_response>
<tool_response tool_call_id="call_SrkhCaC1ikRXBRcDASU7D502">
RAW|
(** MCP server – command-line wrapper around {!module:Mcp_server_core}.

    The binary initialises a fresh in-memory registry, registers a handful
    of *built-in* tools (see list below) and then exposes the registry either
    over **stdio** (default) or **HTTP** when the [--http] flag is supplied.

    {2 Built-in tools}

    | Name | Purpose |
    |------|---------|
    | ["echo"] | Trivial helper that returns the supplied text verbatim. |
    | ["apply_patch"] | Apply a textual V4A diff/patch to the workspace. |
    | ["read_dir"] | List the contents of a directory. |
    | ["get_contents"] | Read a file and return its contents. |
    | ["meta_refine"] | Refine a *meta-prompt* using LLM-backed heuristics. |
    | ["webpage_to_markdown"] | Download a web page and convert it to Markdown. |

    In addition, every *.chatmd* file discovered in the prompts directory is
    registered twice: once as a *prompt* (accessible via
    [`prompts/*`](#)) and once as a *tool* that exposes the same prompt under
    [`tools/call`].

    Additional *.chatmd* files located in the prompts directory are scanned
    at start-up and on a lightweight polling loop, turning each prompt into
    both a user-visible entry and an agent-backed tool.

    {1 CLI flags}

    • [--http PORT] – bind a Streamable HTTP endpoint on [PORT] instead of
      stdio.  The service supports JSON-RPC 2.0 over HTTP POST and
      Server-Sent Events for push notifications.  See
      {!module:Mcp_server_http} for details.

    • [--require-auth] – enable OAuth 2.1 Bearer-token validation.  When the
      flag is omitted the server skips authentication (useful during local
      development and automated tests).

    • [--client-id], [--client-secret] – static client credentials accepted
      by the token endpoint when [--require-auth] is active.

    {1 Usage}

    {[
      # run in stdio mode (useful when the parent process communicates over pipes)
      dune exec bin/mcp_server.exe

      # expose an HTTP API on port 8080
      dune exec bin/mcp_server.exe --http 8080
    ]}
*)

open Core

(* Seed the global PRNG once at start-up so that session identifiers and other
   random values are not predictable across server restarts.  We perform the
   call here (rather than in library code) because test runners that rely on
   deterministic output forbid [Random.self_init] inside inline tests. *)
let () = Random.self_init ()

module JT = Mcp_types

(* -------------------------------------------------------------------------- *)
(* CLI flags                                                                  *)
(* -------------------------------------------------------------------------- *)

let http_port_ref : int option ref = ref None
let require_auth_ref = ref false
let client_id_ref : string option ref = ref None
let client_secret_ref : string option ref = ref None

let () =
  let speclist : (string * Arg.spec * string) list =
    [ ( "--http"
      , Arg.Int (fun p -> http_port_ref := Some p)
      , "Run Streamable HTTP server on the given port (instead of stdio)" )
    ; ( "--require-auth"
      , Arg.Set require_auth_ref
      , "Enable minimal OAuth 2.1 auth (client-credentials)" )
    ; ( "--client-id"
      , Arg.String (fun s -> client_id_ref := Some s)
      , "Static OAuth client_id accepted by the token endpoint" )
    ; ( "--client-secret"
      , Arg.String (fun s -> client_secret_ref := Some s)
      , "Static OAuth client_secret accepted by the token endpoint" )
    ]
  in
  Arg.parse speclist (fun _ -> ()) "mcp_server [--http PORT]"
;;

(** [setup_tool_echo core] registers a trivial tool named ["echo"].

    The tool specification declares a single required parameter
    {[ {"text" : string} ]}.  At runtime the handler returns the same
    string wrapped in a JSON value.

    This helper is intended purely as a smoke-test for the
    request/response plumbing and as a reference implementation when
    creating new tools.

    @param core  Registry instance returned by
      {!Mcp_server_core.create}.  The function mutates the registry in
      place.

    @raise Invalid_argument  If a subsequent attempt is made to
      register another tool under the name ["echo"].  The exception is
      raised by {!Mcp_server_core.register_tool}.  *)
let setup_tool_echo (core : Mcp_server_core.t) : unit =
  let input_schema =
    Jsonaf.of_string
      "{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}},\"required\":[\"text\"]}"
  in
  let spec : JT.Tool.t =
    { name = "echo"; description = Some "Echo back text"; input_schema }
  in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs ->
      (match List.Assoc.find kvs ~equal:String.equal "text" with
       | Some (`String s) -> Ok (`String s)
       | _ -> Error "missing field text or not string")
    | _ -> Error "arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler
;;

(* --------------------------------------------------------------------- *)
(* Built-in ocamlochat tools exposed over MCP                                *)
(* --------------------------------------------------------------------- *)

(** [register_builtin_apply_patch core ~dir] registers the
    built-in ["apply_patch"] tool.

    The underlying implementation is provided by
    {!module:Functions.apply_patch}; this helper merely adapts the JSON-RPC
    call-site contract to the untyped text interface expected by
    [Functions.apply_patch].

    The handler accepts a single field:

    {v
      {
        "input" : "<unified-v4a-patch>"
      }
    v}

    It returns the patched contents as a plain JSON string.

    @param core Mutable registry returned by {!Mcp_server_core.create} and
      shared by all transports.
    @param dir  Capability representing the current working directory. The
      value is used by {!Functions.apply_patch} to resolve relative file
      paths in the diff hunk headers.

    @raise Invalid_argument  Propagated from
      {!Mcp_server_core.register_tool} if a tool called ["apply_patch"] was
      already present in the registry. *)
let register_builtin_apply_patch (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  (* Pull metadata from the existing [Definitions] module so that we keep a
     single source of truth. *)
  let module Def = Definitions.Apply_patch in
  let spec : JT.Tool.t =
    { name = "apply_patch"; description = Def.description; input_schema = Def.parameters }
  in
  (* Re-use the already implemented helper residing in [Functions]. *)
  let ochat_fn = Functions.apply_patch ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs ->
      (match List.Assoc.find kvs ~equal:String.equal "input" with
       | Some (`String patch_text) ->
         let input_json = `Object [ "input", `String patch_text ] in
         let res =
           match ochat_fn.run (Jsonaf.to_string input_json) with
           | Openai.Responses.Tool_output.Output.Text t -> t
           | _ -> "Unsupported output type"
         in
         Ok (`String res)
       | _ -> Error "apply_patch expects field 'input' (string)")
    | _ -> Error "apply_patch arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler
;;

(** [register_builtin_read_dir core ~dir] registers the built-in
    ["read_dir"] tool.

    The handler is powered by {!module:Functions.read_dir}.  It accepts a
    single required argument:

    {v
      {
        "path" : "/absolute/or/relative"
      }
    v}

    The function responds with a JSON array holding all entries of the
    requested directory, sorted lexicographically, with the usual ["."] and
    [".."] components omitted.

    I/O errors (permission denied, missing directory, …) are propagated as
    [`Error msg`] which the router converts into a JSON-RPC exception
    visible to the client.

    @param core Registry to mutate.
    @param dir  Capability to the cwd – required by
      {!Functions.read_dir}. *)
let register_builtin_read_dir (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  let module Def = Definitions.Read_directory in
  let spec : JT.Tool.t =
    { name = "read_dir"; description = Def.description; input_schema = Def.parameters }
  in
  let ochat_fn = Functions.read_dir ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs ->
      (match List.Assoc.find kvs ~equal:String.equal "path" with
       | Some (`String path) ->
         let input_json = `Object [ "path", `String path ] in
         let res =
           match ochat_fn.run (Jsonaf.to_string input_json) with
           | Openai.Responses.Tool_output.Output.Text t -> t
           | _ -> "Unsupported output type"
         in
         Ok (`String res)
       | _ -> Error "read_dir expects field 'path' (string)")
    | _ -> Error "read_dir arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler
;;

(** [register_builtin_get_contents core ~dir] registers the built-in
    ["get_contents"] tool (kept for backward compatibility with the legacy
    name ["read_file"].)

    The tool expects exactly one argument:

    {v
      {
        "file" : "path/to/file"
      }
    v}

    It returns the *raw* file contents wrapped in a JSON string.  Binary
    files are therefore not supported.

    @param core Registry to mutate.
    @param dir  Capability to the working directory, forwarded to
      {!Functions.get_contents} so that relative paths resolve correctly. *)
let register_builtin_get_contents (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  let module Def = Definitions.Get_contents in
  let spec : JT.Tool.t =
    { name = "get_contents"
    ; description = Def.description
    ; input_schema = Def.parameters
    }
  in
  let ochat_fn = Functions.get_contents ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs ->
      (match List.Assoc.find kvs ~equal:String.equal "file" with
       | Some (`String file_path) ->
         let input_json = `Object [ "file", `String file_path ] in
         let res =
           match ochat_fn.run (Jsonaf.to_string input_json) with
           | Openai.Responses.Tool_output.Output.Text t -> t
           | _ -> "Unsupported output type"
         in
         Ok (`String res)
       | _ -> Error "get_contents expects field 'file' (string)")
    | _ -> Error "get_contents arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler
;;

(** [run_stdio ~core ~env] implements the *line-delimited JSON* transport.

    The function blocks indefinitely, performing the following steps in a
    tight loop:

    1. Read a single line from [stdin] and parse it as JSON;
    2. Dispatch the value to {!Mcp_server_router.handle};
    3. Serialise every response returned by the router and write it back to
       [stdout], each on its own line.

    All work happens inside the caller’s fibre – the function never
    returns unless an exception bubbles up or the surrounding switch is
    aborted.

    @param core Shared registry instance.
    @param env  Eio standard environment obtained from
      [Eio_main.run]. *)
let run_stdio ~core ~env : unit =
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  (* We use a buffered reader to read lines from stdin.  The buffer size is
     arbitrary but should be large enough to handle typical input sizes. *)
  let buf = Eio.Buf_read.of_flow stdin ~initial_size:10000 ~max_size:Int.max_value in
  let rec loop stdin stdout =
    let line = Eio.Buf_read.line buf in
    let json = Jsonaf.of_string line in
    let responses = Mcp_server_router.handle ~core ~env json in
    List.iter responses ~f:(fun j ->
      Eio.Flow.copy_string (Jsonaf.to_string j) stdout;
      Eio.Flow.copy_string "\n" stdout);
    loop stdin stdout
  in
  loop stdin stdout
;;

let () =
  (* For Phase-1 we ignore CLI flags.  Future milestones will add --http etc. *)
  let core = Mcp_server_core.create () in
  (* For built-in tools we need an [Eio.Path.t] representing the current
     working directory.  We therefore register them inside the main Eio
     fibre where we have access to [env#cwd]. *)
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    (* Register demo echo plus built-in functions. *)
    setup_tool_echo core;
    register_builtin_apply_patch core ~dir;
    register_builtin_read_dir core ~dir;
    register_builtin_get_contents core ~dir;
    (* Meta-prompting refinement tool ----------------------------------- *)
    let register_builtin_meta_refine (core : Mcp_server_core.t) : unit =
      let module Def = Definitions.Meta_refine in
      let spec : JT.Tool.t =
        { name = Def.name; description = Def.description; input_schema = Def.parameters }
      in
      let ochat_fn = Functions.meta_refine ~env in
      let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
        match args with
        | `Object kvs ->
          (match List.Assoc.find kvs ~equal:String.equal "prompt" with
           | Some (`String prompt) ->
             let input_json = `Object [ "prompt", `String prompt; "task", `String "" ] in
             let res =
               match ochat_fn.run (Jsonaf.to_string input_json) with
               | Openai.Responses.Tool_output.Output.Text t -> t
               | _ -> "Unsupported output type"
             in
             Ok (`String res)
           | _ -> Error "meta_refine expects field 'prompt' (string)")
        | _ -> Error "meta_refine arguments must be object"
      in
      Mcp_server_core.register_tool core spec handler
    in
    register_builtin_meta_refine core;
    (* Webpage → Markdown tool --------------------------------------- *)
    let module Def = Definitions.Webpage_to_markdown in
    let spec : JT.Tool.t =
      { name = Def.name; description = Def.description; input_schema = Def.parameters }
    in
    let ochat_fn = Functions.webpage_to_markdown ~dir ~net:(Eio.Stdenv.net env) in
    let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
      match args with
      | `Object kvs ->
        (match List.Assoc.find kvs ~equal:String.equal "url" with
         | Some (`String url) ->
           let input_json = `Object [ "url", `String url ] in
           let res =
             match (ochat_fn ~env).run (Jsonaf.to_string input_json) with
             | Openai.Responses.Tool_output.Output.Text t -> t
             | _ -> "Unsupported output type"
           in
           Ok (`String res)
         | _ -> Error "webpage_to_markdown expects field 'url' (string)")
      | _ -> Error "arguments must be object"
    in
    Mcp_server_core.register_tool core spec handler;
    (* -----------------------------------------------------------------
         Prompt folder scanning – every *.chatmd file is registered as both a
         prompt and an agent-backed tool.  The folder can be specified via the
         env var [MCP_PROMPTS_DIR].  If unset we look for "./prompts" relative
         to the current working directory and silently ignore missing dirs. *)
    let prompts_dir =
      match Sys.getenv "MCP_PROMPTS_DIR" with
      | Some p -> Eio.Path.(dir / p)
      | None ->
        let default = Eio.Path.(dir / "prompts") in
        default
    in
    (* -----------------------------------------------------------------
         Prompt scanning & lightweight hot-reload
         -----------------------------------------------------------------

         We maintain a hash-set of filenames that have already been processed.
         A background fibre re-scans the [prompts_dir] directory every few
         seconds and registers any brand-new *.chatmd files.  Re-registering
         an existing name is harmless – it merely overwrites the previous
         entry and still triggers the [list_changed] hooks so connected
         clients invalidate their cache.  We do **not** attempt to detect
         deletions at this stage; that can be added later together with a
         proper inotify/FSEvents watcher. *)
    let processed : (string, unit) Hashtbl.t = Hashtbl.create (module String) in
    let scan_prompts () =
      (try
         Eio.Path.read_dir prompts_dir
         |> List.filter ~f:(fun fname -> Filename.check_suffix fname ".chatmd")
         |> List.iter ~f:(fun fname ->
           if not (Hashtbl.mem processed fname)
           then (
             let file_path = Eio.Path.(prompts_dir / fname) in
             match
               Or_error.try_with (fun () ->
                 Mcp_prompt_agent.of_chatmd_file ~env ~core ~path:file_path)
             with
             | Error err ->
               eprintf
                 "[mcp_server] Failed to load prompt %s: %s\n"
                 fname
                 (Error.to_string_hum err)
             | Ok (tool, handler, prompt) ->
               Hashtbl.set processed ~key:fname ~data:();
               Mcp_server_core.register_tool core tool handler;
               Mcp_server_core.register_prompt core ~name:tool.name prompt))
       with
       | _exn -> ());
      ()
    in
    (* Initial scan so the first batch of prompts is available immediately. *)
    scan_prompts ();
    (* Background hot-reload: disabled for now.
         The code above lays the groundwork by keeping a [processed] table.

         We now run a very lightweight polling fibre that re-scans the prompt
         directory every 10 seconds.  This is a pragmatic interim solution
         until Eio exposes a platform-independent file-watcher.  The work is
         cheap – we only stat the directory and `Hashtbl.mem` prevents
         duplicate work, so the overhead is negligible.  If at some point a
         real watcher becomes available we can drop this polling loop without
         touching other parts of the server. *)
    let start_polling_prompts ~sw () =
      let rec loop () =
        scan_prompts ();
        (* Wait a bit before the next scan. *)
        Eio.Time.sleep (Eio.Stdenv.clock env) 10.0;
        loop ()
      in
      Eio.Fiber.fork ~sw loop
    in
    (* --------------------------------------------------------------- *)
    (* Lightweight polling for resource list changes.                  *)
    (* --------------------------------------------------------------- *)
    let start_polling_resources ~sw () =
      let previous_listing = ref String.Set.empty in
      let cwd_string = Stdlib.Sys.getcwd () in
      let scan_resources () =
        let current_listing =
          match Or_error.try_with (fun () -> Stdlib.Sys.readdir cwd_string) with
          | Error _ -> String.Set.empty
          | Ok arr ->
            Array.to_list arr
            |> List.filter ~f:(fun fname ->
              let path = Filename.concat cwd_string fname in
              Stdlib.Sys.file_exists path && not (Stdlib.Sys.is_directory path))
            |> String.Set.of_list
        in
        if not (Set.equal !previous_listing current_listing)
        then (
          previous_listing := current_listing;
          Mcp_server_core.notify_resources_changed core)
      in
      let rec loop () =
        scan_resources ();
        (* initial / periodic scan *)
        Eio.Time.sleep (Eio.Stdenv.clock env) 10.0;
        loop ()
      in
      Eio.Fiber.fork ~sw loop
    in
    (* ----------------------------------------------------------------- *)
    match !http_port_ref with
    | Some port ->
      Eio.Switch.run (fun sw ->
        (* Poller lives under the same switch so it terminates when the
                  HTTP server shuts down *)
        start_polling_prompts ~sw ();
        start_polling_resources ~sw ();
        (* Launch Streamable HTTP server and block forever *)
        Mcp_server_http.run ~require_auth:true ~env ~core ~port)
    | None ->
      (* stdio mode – we still spawn the polling fibre so that long-lived
              sessions also benefit from newly added prompts.  Since the stdio
              loop is blocking we need a dedicated switch. *)
      Eio.Switch.run (fun sw ->
        start_polling_prompts ~sw ();
        start_polling_resources ~sw ();
        run_stdio ~core ~env))
;;
|RAW
</tool_response>
<tool_response tool_call_id="call_UoF1eu1heaFV6XWFqF0jxJHy">
RAW|
(** Terminal user-interface for the **Ochat** assistant.

    This module backs the public executable {{:https://github.com/zshipko/ochat} [chat-tui]},
    a curses-like client built on top of {{!module:Notty}} and {{!module:Eio}}.
    The binary is essentially a *thin* wrapper that:

    1. Parses a rich set of command-line flags (session management, export,
       persistence, …).
    2. Delegates all heavy-lifting to {!Chat_tui.App.run_chat} once the flags are
       validated and normalised.

    The implementation lives in a regular [.ml] file because the executable has no
    public interface of its own.  Nevertheless we keep a complete odoc comment so
    that users browsing the library documentation understand which knobs are
    available from the CLI.

    {1 Usage}

    {v
      chat-tui [-file FILE]
               [--list-sessions]
               [--session NAME | --new-session]
               [--session-info NAME]
               [--export-session NAME --out FILE]
               [--export-file FILE]
               [--reset-session NAME [--prompt-file FILE] [--keep-history]]
               [--rebuild-from-prompt NAME]
               [--parallel-tool-calls | --no-parallel-tool-calls]
               [--auto-persist | --no-persist]
    v}

    Flags (grouped by category):

    • *Prompt*:  ▸ [-file FILE] – ChatMarkdown / Markdown document that seeds the
      conversation buffer and declares callable tools.  Defaults to
      {!val:default_prompt_file}.

    • *Session selection* (mutually exclusive):
      – [--list-sessions] · enumerate existing session identifiers.
      – [--session NAME] · resume the given session.
      – [--new-session]   · force creation of a brand-new session even when a
                           deterministic one already exists for the prompt.

    • *Session inspection* (exclusive with the above):
      – [--session-info NAME]         · print metadata (history length, prompt
                                        path, timestamps, …).
      – [--reset-session NAME]        · archive the snapshot and start over,
                                        optionally keeping the chat history
                                        ([--keep-history]) or switching to a
                                        different prompt ([--prompt-file]).
      – [--rebuild-from-prompt NAME]  · rebuild the snapshot from the stored
                                        prompt file.

    • *Export*:
      – [--export-session NAME --out FILE] · convert a snapshot to a standalone
        *.chatmd* file and exit.
      – [--export-file FILE]               · after the interactive session
        finishes, save the full transcript to the given file.

    • *Runtime behaviour*:
      – [--parallel-tool-calls] / [--no-parallel-tool-calls] · toggle parallel
        execution of function-callable tools.
      – [--auto-persist] / [--no-persist] · control whether the snapshot is
        saved on exit without prompting.

    Invalid flag combinations are detected early and reported with a helpful
    diagnostic before the process terminates.
*)

open Core

let default_prompt_file = "./prompts/interactive.md"

(** [run ?session_id ?new_session ?export_file ?persist_mode
       ?parallel_tool_calls ~prompt_file ()] launches the Notty-based
    interactive chat loop.

    The function is a *re-export* of {!Chat_tui.App.run_chat} with a few
    extra responsibilities – namely resolving the appropriate session
    snapshot and applying user-selected run-time options.  It is useful
    for embedding the TUI inside another OCaml program.

    Parameters (mirroring the CLI flags):

    • [?session_id] – identifier of the session snapshot to resume.  If
      omitted a deterministic ID derived from [prompt_file] is used.

    • [?new_session] (default: [false]) – create a fresh session even
      when a snapshot bearing the deterministic ID already exists.

    • [?export_file] – when set, export the full conversation to the
      given file on normal termination (same format as
      [--export-file]).

    • [?persist_mode] – automatic save mode: `\`Ask` (default), `\`Always`, or
      `\`Never`.  See {!type:Chat_tui.App.persist_mode}.

    • [?parallel_tool_calls] (default: [true]) – whether to allow
      concurrent execution of function-callable tools.

    • [~prompt_file] – ChatMarkdown / Markdown document used to seed the
      conversation buffer and declare tools.

    The function blocks until the user quits the interface (e.g. `/quit`
    or *Ctrl-c* ).
*)
let run
      ?session_id
      ?(new_session = false)
      ?export_file
      ?(persist_mode : Chat_tui.App.persist_mode = `Ask)
      ?(parallel_tool_calls = true)
      ~prompt_file
      ()
  =
  Io.run_main (fun env ->
    let session =
      Session_store.load_or_create ~env ~prompt_file ?id:session_id ~new_session ()
    in
    Chat_tui.App.run_chat
      ~env
      ~prompt_file
      ~session
      ?export_file
      ~persist_mode
      ~parallel_tool_calls
      ())
;;

let () =
  let open Command.Let_syntax in
  let command =
    Command.basic
      ~summary:"Interactive Ochat TUI"
      [%map_open
        let conversation_file =
          flag
            "-file"
            (optional_with_default default_prompt_file string)
            ~doc:"FILE Conversation buffer path (default: ./prompts/interactive.md)"
        and list_sessions =
          flag
            "--list-sessions"
            no_arg
            ~doc:
              "List all existing sessions and exit (incompatible with other session \
               flags)"
        and session_id =
          flag
            "--session"
            (optional string)
            ~doc:"NAME Resume an existing session identified by NAME"
        and new_session =
          flag
            "--new-session"
            no_arg
            ~doc:"Create a new session instead of resuming an existing one"
        (* Export a session to ChatMarkdown *)
        and export_session_id =
          flag
            "--export-session"
            (optional string)
            ~doc:
              "NAME Export the specified session to ChatMarkdown and exit (incompatible \
               with other session flags)"
        and export_out_file =
          flag
            "--out"
            (optional string)
            ~doc:
              "FILE Output path for --export-session (required when using \
               --export-session)"
        (* Destination file for interactive export on exit *)
        and export_file =
          flag
            "--export-file"
            (optional string)
            ~doc:"FILE ChatMarkdown destination when exporting on exit (interactive mode)"
        (* Display session metadata and exit *)
        and session_info =
          flag
            "--session-info"
            (optional string)
            ~doc:
              "NAME Display metadata for session NAME and exit (incompatible with other \
               session flags)"
        (* Reset / archive session *)
        and reset_session_id =
          flag
            "--reset-session"
            (optional string)
            ~doc:
              "NAME Archive current snapshot and reset session NAME (incompatible with \
               other session flags)"
        and reset_prompt_file =
          flag
            "--prompt-file"
            (optional string)
            ~doc:
              "FILE New prompt file to use when resetting the session (optional with \
               --reset-session)"
        and reset_keep_history =
          flag
            "--keep-history"
            no_arg
            ~doc:
              "When used with --reset-session, retain conversation history and cache \
               instead of clearing them"
        (* Parallel tool call toggle *)
        and parallel_tool_calls =
          flag
            "--parallel-tool-calls"
            no_arg
            ~doc:"Enable parallel execution of tool calls (default: enabled)"
        and no_parallel_tool_calls =
          flag
            "--no-parallel-tool-calls"
            no_arg
            ~doc:"Disable parallel execution of tool calls (forces sequential evaluation)"
        and no_persist =
          flag
            "--no-persist"
            no_arg
            ~doc:"Do not persist session snapshot on exit (interactive mode)"
        and auto_persist =
          flag
            "--auto-persist"
            no_arg
            ~doc:"Always persist session snapshot on exit without asking"
        (* Rebuild snapshot from (edited) prompt *)
        and rebuild_session_id =
          flag
            "--rebuild-from-prompt"
            (optional string)
            ~doc:
              "NAME Rebuild session NAME from its prompt.chatmd copy and exit \
               (incompatible with other session flags)"
        in
        (* Validate mutually exclusive flags. *)
        let () =
          (* --list-sessions cannot be combined with other session-manipulating flags *)
          (match list_sessions, session_id, new_session, session_info with
           | true, Some _, _, _ | true, _, true, _ | true, _, _, Some _ ->
             Core.eprintf
               "Error: --list-sessions cannot be combined with --session, \
                --session-info, or --new-session.\n";
             exit 1
           | _ -> ());
          (* --parallel-tool-calls and --no-parallel-tool-calls are mutually exclusive *)
          (match parallel_tool_calls, no_parallel_tool_calls with
           | true, true ->
             Core.eprintf
               "Error: --parallel-tool-calls and --no-parallel-tool-calls cannot be used \
                together.\n";
             exit 1
           | _ -> ());
          (* --keep-history requires --reset-session *)
          (match reset_keep_history, reset_session_id with
           | true, None ->
             Core.eprintf "Error: --keep-history can only be used with --reset-session.\n";
             exit 1
           | _ -> ());
          (* --session and --new-session are mutually exclusive *)
          (match session_id, new_session with
           | Some _, true ->
             Core.eprintf "Error: --session and --new-session are mutually exclusive.\n";
             exit 1
           | _ -> ());
          (* --no-persist and --auto-persist are mutually exclusive *)
          (match no_persist, auto_persist with
           | true, true ->
             Core.eprintf
               "Error: --no-persist and --auto-persist are mutually exclusive.\n";
             exit 1
           | _ -> ());
          (* --session-info cannot be combined with --session or --new-session *)
          (match session_info, session_id, new_session with
           | Some _, Some _, _ | Some _, _, true ->
             Core.eprintf
               "Error: --session-info cannot be combined with --session or --new-session.\n";
             exit 1
           | _ -> ());
          (* --reset-session validation *)
          (match reset_session_id with
           | None -> ()
           | Some _ ->
             (* Ensure incompatible flags with other mutually exclusive operations *)
             (match
                list_sessions, session_id, new_session, session_info, export_session_id
              with
              | true, _, _, _, _
              | _, Some _, _, _, _
              | _, _, true, _, _
              | _, _, _, Some _, _
              | _, _, _, _, Some _ ->
                Core.eprintf
                  "Error: --reset-session is incompatible with --list-sessions, \
                   --session, --new-session, --session-info, and --export-session.\n";
                exit 1
              | _ -> ()));
          (* --export-session validation *)
          (match export_session_id with
           | None -> ()
           | Some _ ->
             (* Require --out flag *)
             (match export_out_file with
              | None ->
                Core.eprintf
                  "Error: --out must be provided when using --export-session.\n";
                exit 1
              | Some _ -> ());
             (* Ensure incompatible flags are not set *)
             (match list_sessions, session_id, new_session, session_info with
              | true, _, _, _ | _, Some _, _, _ | _, _, true, _ | _, _, _, Some _ ->
                Core.eprintf
                  "Error: --export-session is incompatible with --list-sessions, \
                   --session, --new-session, and --session-info.\n";
                exit 1
              | _ -> ()));
          (* --export-file should not be combined with --export-session, --list-sessions, --session-info, or --reset-session *)
          match
            export_file, export_session_id, list_sessions, session_info, reset_session_id
          with
          | Some _, Some _, _, _, _
          | Some _, _, true, _, _
          | Some _, _, _, Some _, _
          | Some _, _, _, _, Some _ ->
            Core.eprintf
              "Error: --export-file cannot be combined with --export-session, \
               --list-sessions, --session-info, or --reset-session.\n";
            exit 1
          | _ ->
            ();
            (* --rebuild-from-prompt validation *)
            (match rebuild_session_id with
             | None -> ()
             | Some _ ->
               (match
                  ( list_sessions
                  , session_id
                  , new_session
                  , session_info
                  , export_session_id
                  , reset_session_id )
                with
                | true, _, _, _, _, _
                | _, Some _, _, _, _, _
                | _, _, true, _, _, _
                | _, _, _, Some _, _, _
                | _, _, _, _, Some _, _
                | _, _, _, _, _, Some _ ->
                  Core.eprintf
                    "Error: --rebuild-from-prompt is incompatible with other session \
                     flags.\n";
                  exit 1
                | _ -> ()))
        in
        fun () ->
          if list_sessions
          then
            Io.run_main (fun env ->
              let sessions = Session_store.list ~env in
              List.iter sessions ~f:(fun (id, prompt) -> Core.printf "%s\t%s\n" id prompt))
          else if Option.is_some session_info
          then (
            let info_id = Option.value_exn session_info in
            Io.run_main (fun env ->
              let dir = Session_store.path ~env info_id in
              let ( / ) = Eio.Path.( / ) in
              let snapshot = dir / "snapshot.bin" in
              if not (Eio.Path.is_file snapshot)
              then (
                Core.eprintf "Error: session '%s' not found.\n" info_id;
                exit 1)
              else (
                let stats = Eio.Path.stat ~follow:true snapshot in
                let session = Session.Io.File.read snapshot in
                let format_time secs =
                  let open Core_unix in
                  let tm = localtime secs in
                  Printf.sprintf
                    "%04d-%02d-%02d %02d:%02d:%02d"
                    (tm.tm_year + 1900)
                    (tm.tm_mon + 1)
                    tm.tm_mday
                    tm.tm_hour
                    tm.tm_min
                    tm.tm_sec
                in
                Core.printf "Session: %s\n" info_id;
                Core.printf "Prompt file: %s\n" session.prompt_file;
                Core.printf "Last modified: %s\n" (format_time stats.mtime);
                Core.printf "History items: %d\n" (List.length session.history);
                Core.printf "Tasks: %d\n" (List.length session.tasks))))
          else if Option.is_some reset_session_id
          then (
            let sid = Option.value_exn reset_session_id in
            Io.run_main (fun env ->
              Session_store.reset_session
                ~env
                ~id:sid
                ~keep_history:reset_keep_history
                ?prompt_file:reset_prompt_file
                ()))
          else if Option.is_some export_session_id
          then (
            let sid = Option.value_exn export_session_id in
            let outfile = Option.value_exn export_out_file in
            Io.run_main (fun env ->
              let sdir = Session_store.path ~env sid in
              let ( / ) = Eio.Path.( / ) in
              let snapshot = sdir / "snapshot.bin" in
              if not (Eio.Path.is_file snapshot)
              then (
                Core.eprintf "Error: session '%s' not found.\n" sid;
                exit 1);
              (* Acquire a simple advisory lock to avoid reading a snapshot
                 that is concurrently being modified.  We reuse the same
                 [snapshot.bin.lock] convention used by [Session_store.save]. *)
              let lock_file = Eio.Path.(sdir / "snapshot.bin.lock") in
              let acquired_lock =
                try
                  Eio.Path.save ~create:(`Exclusive 0o600) lock_file "";
                  true
                with
                | _ -> false
              in
              if not acquired_lock
              then (
                Core.eprintf
                  "Error: session '%s' is currently locked by another process.\n"
                  sid;
                exit 1);
              let session =
                protectx
                  ~finally:(fun () ->
                    try Eio.Path.unlink lock_file with
                    | _ -> ())
                  ()
                  ~f:(fun () -> Session.Io.File.read snapshot)
              in
              let dir_str = Filename.dirname outfile in
              let file_name = Filename.basename outfile in
              let fs = Eio.Stdenv.fs env in
              let out_dir = Eio.Path.(fs / dir_str) in
              (* create directory if missing *)
              (* Create the output directory if it does not already exist *)
              (match Eio.Path.is_directory out_dir with
               | true -> ()
               | false -> Eio.Path.mkdirs ~perm:0o700 out_dir);
              let dest_path = out_dir / file_name in
              (* Confirm overwrite *)
              if Eio.Path.is_file dest_path
              then (
                Out_channel.output_string
                  stdout
                  (Printf.sprintf "File %s exists. Overwrite? [y/N] " outfile);
                Out_channel.flush stdout;
                match In_channel.input_line In_channel.stdin with
                | Some ans
                  when List.mem
                         [ "y"; "yes" ]
                         (String.lowercase (String.strip ans))
                         ~equal:String.equal -> ()
                | _ ->
                  Core.printf "Aborted.\n";
                  (* Propagate cancellation by simply returning without exporting *)
                  ());
              let cwd = out_dir in
              let datadir = Io.ensure_chatmd_dir ~cwd in
              (* ------------------------------------------------------------------ *)
              (* 0. Copy original prompt content                                   *)
              (* ------------------------------------------------------------------ *)
              (* Resolve prompt path (absolute vs relative) from the
                   perspective of the session’s original working dir – here
                   we assume [session.prompt_file] is stored exactly as first
                   provided. *)
              let prompt_contents =
                let dir_for_prompt =
                  if Filename.is_absolute session.prompt_file
                  then fs
                  else Eio.Stdenv.cwd env
                in
                Option.value
                  (Option.try_with (fun () ->
                     Io.load_doc ~dir:dir_for_prompt session.prompt_file))
                  ~default:""
              in
              (* Save the initial prompt content with restrictive permissions. *)
              Eio.Path.save ~create:(`Or_truncate 0o600) dest_path prompt_contents;
              (* ------------------------------------------------------------------ *)
              (* Attachments – reuse shared helper                                        *)
              (* ------------------------------------------------------------------ *)
              let prompt_parent_dir =
                let base_dir =
                  if Filename.is_absolute session.prompt_file
                  then fs
                  else Eio.Stdenv.cwd env
                in
                Eio.Path.(base_dir / Filename.dirname session.prompt_file)
              in
              Chat_tui.Attachments.copy_all
                ~prompt_dir:prompt_parent_dir
                ~cwd:(Eio.Stdenv.cwd env)
                ~session_dir:sdir
                ~dst:datadir;
              (* Task #26: export the full conversation history.  We no
                 longer drop the first [initial_msg_count] items that
                 correspond to the static prompt. *)
              let module Config = Chat_response.Config in
              Chat_tui.Persistence.persist_session
                ~dir:cwd
                ~prompt_file:file_name
                ~datadir
                ~cfg:Config.default
                ~initial_msg_count:0
                ~history_items:session.history;
              Core.printf "Session '%s' exported to %s\n" sid outfile))
          else if Option.is_some rebuild_session_id
          then (
            let sid = Option.value_exn rebuild_session_id in
            Io.run_main (fun env -> Session_store.rebuild_session ~env ~id:sid ()))
          else (
            let persist_mode =
              match no_persist, auto_persist with
              | true, _ -> `Never
              | _, true -> `Always
              | _ -> `Ask
            in
            let parallel_tool_calls_value =
              match parallel_tool_calls, no_parallel_tool_calls with
              | true, false -> true
              | false, true -> false
              | false, false -> true
              | true, true -> (* Already validated earlier. *) true
            in
            run
              ?session_id
              ~new_session
              ?export_file
              ~persist_mode
              ~parallel_tool_calls:parallel_tool_calls_value
              ~prompt_file:conversation_file
              ())]
  in
  Command_unix.run command
;;
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696eef642e288195bd0647a6c845c112">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696eef75e6fc8195a01598f2df4f0a93">
RAW|
Purpose: Inspect tool schema definitions (Definitions) to confirm the authoritative list of built-in tool names and their parameters for documentation accuracy.
|RAW
</assistant>


<tool_call tool_call_id="call_5Mr8NslfxHFeNSCH2vUt9gjh" function_name="read_file" id="fc_0a7776dd715389aa00696eef75f7a48195914fd745341d9162">
RAW|
{"file":"lib/definitions.mli"}
|RAW
</tool_call>

<tool_response tool_call_id="call_5Mr8NslfxHFeNSCH2vUt9gjh">
RAW|
(** GPT function *definitions* exposed by the Ochat agent.

    This module is **data-only** – it bundles a catalogue of tool
    specifications that can be offered to the OpenAI function-calling
    API.  Each sub-module implements {!Ochat_function.Def} and therefore
    provides four values that describe a tool but do *not* implement
    its runtime behaviour:

    • [name] – unique identifier that the language model uses.
    • [description] – one-paragraph human-readable summary.
    • [parameters] – JSON schema (draft-07) of the expected input for
      standard function tools, or a custom tool “format” object for
      tools with [type_ = "custom"].
    • [input_of_string] – converts the JSON payload returned by the
      API into a strongly-typed OCaml value.

    To obtain an *executable* tool you must pair the definition with an
    implementation using {!Ochat_function.create_function}.

    Nothing in this module performs I/O; all operations are pure and
    total.

    {1  Catalogue}

    The current set of tool definitions shipped with the library:

    • {!Get_contents}        – read a file from the local filesystem
    • {!Odoc_search}         – search locally-indexed odoc docs
    • {!Fork}                – spawn a nested agent performing a task
    • {!Webpage_to_markdown} – download an URL and convert to Markdown
    • {!Add_line_numbers}    – prefix lines of text with numbers
    • {!Get_url_content}     – fetch raw contents of an URL
    • {!Index_ocaml_code}    – embed OCaml sources into a vector store
    • {!Query_vector_db}     – semantic search in a vector database
    • {!Apply_patch}         – apply a V4A diff/patch to the workspace
    • {!Read_directory}      – list entries of a directory
    • {!Append_to_file}      – append a string to an existing file
    • {!Find_and_replace}    – find and replace a substring in a file
    • {!Meta_refine}         – refine prompts via Recursive Meta-Prompting
    • {!Index_markdown_docs} – build a vector index from Markdown docs
    • {!Markdown_search}     – semantic search over Markdown indices
    • {!Make_dir}            – create a directory on the filesystem
*)

module Get_contents : Ochat_function.Def with type input = string * int option

module Odoc_search :
  Ochat_function.Def with type input = string * int option * string option * string

type fork_input =
  { command : string
  ; arguments : string list
  }

module Fork : Ochat_function.Def with type input = fork_input
module Webpage_to_markdown : Ochat_function.Def with type input = string
module Add_line_numbers : Ochat_function.Def with type input = string
module Get_url_content : Ochat_function.Def with type input = string
module Index_ocaml_code : Ochat_function.Def with type input = string * string

module Query_vector_db :
  Ochat_function.Def with type input = string * string * int * string option

module Apply_patch : Ochat_function.Def with type input = string
module Read_directory : Ochat_function.Def with type input = string
module Append_to_file : Ochat_function.Def with type input = string * string

module Find_and_replace :
  Ochat_function.Def with type input = string * string * string * bool

module Make_dir : Ochat_function.Def with type input = string

(** {1 Meta-prompting helpers} *)

module Meta_refine : Ochat_function.Def with type input = string * string

(* ---------------------------------------------------------------------- *)
(*  Markdown indexing & search                                              *)
(* ---------------------------------------------------------------------- *)

(** {1 Index_markdown_docs}

    Indexes a folder containing Markdown documents into a vector
    database suitable for semantic search.  The 4-tuple carried in
    [input] is:

    • [root]            – directory to crawl recursively.
    • [index_name]      – logical identifier for the index (e.g.
      "docs").
    • [description]     – one-line blurb that describes the corpus.
    • [vector_db_root]  – optional destination directory for the
      vector database; defaults to ".md_index" when [None]. *)

module Index_markdown_docs :
  Ochat_function.Def with type input = string * string * string * string option

(** {1 Markdown_search}

    Performs a semantic search over one or more Markdown indices.  The
    4-tuple carried in [input] is:

    • [query]           – user query.
    • [k]               – optional upper bound on the number of hits
      (defaults to 5 when [None]).
    • [index_name]      – specific index to query or "all"; when
      [None] defaults to "all".
    • [vector_db_root]  – root directory housing the indices; defaults
      to ".md_index" when [None]. *)

module Markdown_search :
  Ochat_function.Def with type input = string * int option * string option * string option

module Import_image : Ochat_function.Def with type input = string
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696eef788e40819587f7da2cb5911155">
<summary>
**Clarifying function semantics**

I'm not entirely sure about some details here, but the documentation mentions placeholder usage for future multi-agent flows. There’s a strict flag in Ochat_function.create_function indicating default behavior is strict, but some built-ins allow non-strict schema validation. I need to check if the documentation should clarify which tools are non-strict to enhance robustness. Also, I should create a clear plan for revising the updates based on the user's request. Let's start with the markdown document.
</summary>

<summary>
**Assessing documentation accuracy**

I'm noticing that the documentation is missing names of some built-ins beyond what's in tool.ml, such as import_image and meta_refine. The built-ins list also includes incorrect items like mkdir, which isn't for ChatMD, and should probably mention that functions like get_url_content are only for the OCaml API. It might be beneficial to add specifics and examples regarding patches, as the current documentation isn’t clear. Additionally, I'll consider updating the terminology to reflect the OpenAI responses endpoint.
</summary>

<summary>
**Reviewing documentation issues**

The documentation's OCaml tool example mentions using emoji and create_function but suggests that adding tools only requires modifying `Tool.of_declaration`. I think it needs clarification—"rolling your own OCaml tool" actually requires custom tool registration in the host application. Additionally, saying, "Declare it once in ChatMD <tool name='say_hello'/>" is misleading unless there's a mapping for say_hello as a built-in. If it's not listed, matching will fail, which seems like a major oversight in the documentation that needs fixing.
</summary>

<summary>
**Addressing documentation inaccuracies**

It seems like the documentation is referencing an older version where tools were discovered differently. I need to clarify that you can't just define a new tool by writing a module; it needs to be registered in the driver/tool mapping or provided as a custom command. While there is a way to create custom tool calls, like using `<tool name="..." command="..."/>`, pure OCaml tools require compiling into a binary and updating the mapping in `lib/chat_response/tool.ml`. The documentation should be updated to accurately reflect these needs.
</summary>

<summary>
**Correcting documentation for tools**

I need to clarify that for library usage, the tool list should be built in code and passed to the driver. In ChatMD's interactive TUI, you can only use provided built-ins or patches unless you modify the code. The documentation mentions "custom OCaml helpers" but doesn't adequately describe the agent tool or how to extend it. Also, tools like `get_url_content` and `mkdir` aren’t exposed correctly. There are more issues to address, such as `import_image` returning `Output.Content` rather than text, and the handling of offsets in `read_file`. Documentation must align with implementation specifics on these points.
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696eef9cf02c8195aa5dc7be86dce097">
RAW|
Purpose: Check the actual tool names and JSON schemas in Definitions (especially Get_contents/read_file and mkdir) to spot any naming mismatches in the docs.
|RAW
</assistant>


<tool_call tool_call_id="call_vSJic5dyBZLq4P9bEsudc9iY" function_name="read_file" id="fc_0a7776dd715389aa00696eef9cfbb08195ac7e3d47985c44a0">
RAW|
{"file":"lib/definitions.ml"}
|RAW
</tool_call>

<tool_response tool_call_id="call_vSJic5dyBZLq4P9bEsudc9iY">
RAW|
(** Internal catalogue of GPT function definitions.

    Each sub-module in this file implements {!Ochat_function.Def}.  The
    definitions are *pure metadata* – they do **not** perform any
    side-effects or I/O.  A caller must pair them with an
    implementation via {!Ochat_function.create_function} before the tool
    can be executed.

    See {!file:definitions.mli} for a high-level overview of the
    available tools.
*)

open Core

(** {1 Get_contents}

    Definition of the "read_file" tool.  The tool expects a JSON
    object with a field [file] containing a path, and an optional field
    [offset] specifying the byte offset to start reading from.  It
    forwards the path and offset unchanged as a string and integer,
    respectively.

    Example payload accepted by {!input_of_string}:

    {[
      "{ \"file\": \"/tmp/example.txt\" }"
    ]}
*)

module Get_contents : Ochat_function.Def with type input = string * int option = struct
  type input = string * int option

  let name = "read_file"
  let type_ = "function"

  let description =
    Some
      {|
Reads a file from the local filesystem and returns its contents as a string.
The file is read with a limit of 380928 bytes (roughly 100000 tokens)  to avoid
memory issues with large files. If the file is larger than that, it will be
truncated to the last full line. Output will be appended with an indication
of truncation "\n\n---\n[File truncated: %d more bytes not shown]". If you need the full file, you can read it in chunks using
the offset parameter.
|}
  ;;

  (** The JSON schema for the input.  The [file] field is mandatory, while
      [offset] is optional and defaults to 0. *)
  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "file", `Object [ "type", `String "string" ]
            ; "offset", `Object [ "type", `String "integer" ]
            ] )
      ; "required", `Array [ `String "file" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    let file = Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "file" j in
    let file =
      match file with
      | Some f -> f
      | None ->
        Option.value ~default:""
        @@ Option.map ~f:Jsonaf.string_exn
        @@ Jsonaf.member "path" j
    in
    let offset = Option.map ~f:Jsonaf.int_exn @@ Jsonaf.member "offset" j in
    file, offset
  ;;
end

(* ---------------------------------------------------------------------- *)
(*  Meta_refine                                                              *)
(* ---------------------------------------------------------------------- *)

(** {1 Meta_refine}

    Tool definition for *Recursive Meta-Prompting* refinement.  The tool
    accepts a raw [prompt] string and returns the refined prompt obtained by
    running {!Meta_prompting.Recursive_mp.refine}. *)

module Meta_refine : Ochat_function.Def with type input = string * string = struct
  type input = string * string

  let name = "meta_refine"
  let type_ = "function"

  let description =
    Some
      "Given a task Generate or refine a prompt via Recursive Meta-Prompting and return \
       the improved version. If no prompt just pass an empty string."
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "prompt", `Object [ "type", `String "string" ]
            ; "task", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "prompt"; `String "task" ]
        (* The [task] field is optional, but if present, it should be a string. *)
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    let prompt = Jsonaf.string_exn @@ Jsonaf.member_exn "prompt" j in
    let task = Jsonaf.string_exn @@ Jsonaf.member_exn "task" j in
    prompt, task
  ;;
end

(* ---------------------------------------------------------------------- *)
(*  Markdown indexing & search                                             *)
(* ---------------------------------------------------------------------- *)

(** {1 Index_markdown_docs}

    Registers (or updates) a vector database built from a directory of
    Markdown files.  The tool takes the following JSON payload:

    {v
    {
      "root"           : "string",   // directory to crawl recursively
      "index_name"     : "string",   // logical identifier, e.g. "docs"
      "description"    : "string",   // one-line blurb for catalogue
      "vector_db_root" : "string"?   // where to store the index (optional)
    }
    v}

    It forwards the data as a 4-tuple [(root, index_name, description,
    vector_db_root)].  The implementation is responsible for running
    {!Markdown_indexer.index_directory}. *)

module Index_markdown_docs :
  Ochat_function.Def with type input = string * string * string * string option = struct
  type input = string * string * string * string option

  let name = "index_markdown_docs"
  let type_ = "function"

  let description =
    Some "Index a directory of Markdown files into a vector database for semantic search"
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "root", `Object [ "type", `String "string" ]
            ; "index_name", `Object [ "type", `String "string" ]
            ; "description", `Object [ "type", `String "string" ]
            ; "vector_db_root", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "root"; `String "index_name"; `String "description" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s : input =
    let j = Jsonaf.of_string s in
    let root = Jsonaf.string_exn @@ Jsonaf.member_exn "root" j in
    let index_name = Jsonaf.string_exn @@ Jsonaf.member_exn "index_name" j in
    let description = Jsonaf.string_exn @@ Jsonaf.member_exn "description" j in
    let vector_db_root =
      Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "vector_db_root" j
    in
    root, index_name, description, vector_db_root
  ;;
end

(** {1 Markdown_search}

    Semantic search tool for Markdown indices previously created with
    {!Index_markdown_docs}.  Expected JSON schema:

    {v
    {
      "query"          : "string",         // search string (required)
      "k"              : 5?,                // max hits (optional)
      "index_name"     : "string"?,        // index to query or "all"
      "vector_db_root" : "string"?         // root directory holding indices
    }
    v}
  *)

module Markdown_search :
  Ochat_function.Def with type input = string * int option * string option * string option =
struct
  type input = string * int option * string option * string option

  let name = "markdown_search"
  let type_ = "function"

  let description =
    Some
      {|
semantic-search utility over markdown documentation index for the current project.

Guidelines for callers
1. Provide `query` in natural language or code fragments; rewriting user text is optional – optimise for precision.
2. Set `index_name` to the target index_name when the task is scoped; otherwise use "all" (default) to search every index_name via the top-level index.
3. Keep `k` small (≤10) unless more results are truly required; larger values add latency and noise.
4. Returned value is a Markdown list:
   `[rank] [package] <snippet-id>` followed by the first 8000 characters of each snippet.
5. If the results are not satisfactory, consider refining the query or using a different package.

Query-crafting best practices
• Keep it concise and meaningful – avoid filler like “could you maybe…”.
• Include a disambiguating keyword (package, type, module) in the same sentence when needed.
• Use high level descriptions, e.g. "how to use Eio.Switch" instead of "Eio.Switch" when searching for usage examples.
• Iterate: if top-k looks generic, trim noise or add a specific term seen in a near-miss snippet, then re-query.

Use for natural language search over the markdown documentation of the current ocaml project.
When to use
✓ For initial exploration of the codebase to understand how it works.
✓ The agent needs documentation that goes **beyond inline `*.mli` comments**.  
  That captures design notes, usage examples, historical decisions and any other background that helps a human (or an indexing tool) understand the code-base..
✓ When planning a refactor, code generation, or bug-fix and concrete documentation snippets would accelerate reasoning.
✓ Quick recall of module docs for a high-level understanding of the codebase
✓ Researching the codebase for specific information, e.g. "how to use the Vector_db module" or "Code that uses Eio for file IO"
✓ Searching for specific code snippets or examples in the markdown documentation of the current project.
✓ Searching for code related to a specific flow or feature in the current project that would not be documented in the source code.
When *not* to use
✗ You only need an exact substring / regex lookup in the user’s source code.

JSON input schema
```
{
  "query"   : "string",              // required
  "index_name" : "all" | "doc-src/lib" | …,    // required
  "k"       : 5?,                     // optional, default 5
  "vector_db_root"   : ".md_index"?         // optional, default
}
```
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "query", `Object [ "type", `String "string" ]
            ; "k", `Object [ "type", `String "integer" ]
            ; "index_name", `Object [ "type", `String "string" ]
            ; "vector_db_root", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "query" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s : input =
    let j = Jsonaf.of_string s in
    let query = Jsonaf.string_exn @@ Jsonaf.member_exn "query" j in
    let k = Option.map ~f:Jsonaf.int_exn @@ Jsonaf.member "k" j in
    let index_name = Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "index_name" j in
    let vector_db_root =
      Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "vector_db_root" j
    in
    query, k, index_name, vector_db_root
  ;;
end

(* ---------------------------------------------------------------------- *)
(*  ODoc search tool                                                       *)
(* ---------------------------------------------------------------------- *)

(** {1 Odoc_search}

    Definition of an OCaml–specific documentation search tool that
    queries a locally-indexed `.odoc` corpus.  The [input] is a
    quadruplet [(query, k, index, package)]:

    • [query]   – free-form text or code snippet used for semantic search.
    • [k]       – optional upper bound on the number of hits (defaults to 5).
    • [index]   – optional path to a custom odoc search index.
    • [package] – the opam package name to scope the search or "all".
*)

module Odoc_search :
  Ochat_function.Def with type input = string * int option * string option * string =
struct
  type input = string * int option * string option * string

  let name = "odoc_search"
  let type_ = "function"

  let description =
    Some
      {|
ODoc semantic-search utility.

Guidelines for callers
1. Provide `query` in natural language or code fragments; rewriting user text is optional – optimise for precision.
2. Set `package` to the target opam package when the task is scoped; otherwise use "all" (default) to search every package via the package-level index.
3. Keep `k` small (≤10) unless more results are truly required; larger values add latency and noise.
4. Returned value is a Markdown list:
   `[rank] [package] <snippet-id>` followed by the first 8000 characters of each snippet.
5. Change `index` only when working with a non-standard documentation snapshot.
6. If the results are not satisfactory, consider refining the query or using a different package.

Query-crafting best practices
• Keep it concise and meaningful – avoid filler like “could you maybe…”.
• Use the corpus’ own vocabulary: module names (`Eio.Switch`), function names (`List.mapi`) or OCaml type signatures.
• Use high level descriptions, e.g. "how to use Eio.Switch" instead of "Eio.Switch" when searching for usage examples.
• Include a disambiguating keyword (package, type, module) in the same sentence when needed.
• Iterate: if top-k looks generic, trim noise or add a specific term seen in a near-miss snippet, then re-query.

Use for natural language search over the Odoc generated documentation of installed opam packages and this the current project ochat.
When to use
✓ The agent needs authoritative explanations, type signatures, or usage examples for the current project or OCaml libraries that are *already* installed and indexed locally.
✓ While planning a refactor, code generation, or bug-fix and concrete documentation snippets would accelerate reasoning.
✓ Quick recall of a package README or module docs without opening a browser.
✓ Exploring related concepts or libraries by browsing their documentation.

When *not* to use
✗ You require up-to-date *web* information about packages that are not present in the local index.
✗ You need to search for code related to a specific flow or feature in the current project – prefer `markdown_search`.

JSON input schema
```
{
  "query"   : "string",              // required
  "package" : "all" | "eio" | …,    // required
  "k"       : 5?,                     // optional, default 5
  "index"   : ".odoc_index"?         // optional, default
}
```
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "query", `Object [ "type", `String "string" ]
            ; "k", `Object [ "type", `String "integer" ]
            ; "index", `Object [ "type", `String "string" ]
            ; "package", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "query"; `String "package" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    let query = Jsonaf.string_exn @@ Jsonaf.member_exn "query" j in
    let k = Option.map ~f:Jsonaf.int_exn @@ Jsonaf.member "k" j in
    let index = Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "index" j in
    let package = Jsonaf.string_exn @@ Jsonaf.member_exn "package" j in
    query, k, index, package
  ;;
end

(* ---------------------------------------------------------------------- *)
(*  Fork – clone the current agent and run a command in the clone          *)
(* ---------------------------------------------------------------------- *)

type fork_input =
  { command : string
  ; arguments : string list
  }

(** {1 Fork}

    Definition of a tool that spawns an auxiliary agent operating on
    the same workspace.  The [input] record specifies the [command]
    executed by the fork and an optional list of CLI-style
    [arguments].
*)

module Fork : Ochat_function.Def with type input = fork_input = struct
  [@@@warning "-69"]

  type input = fork_input

  let type_ = "function"
  let name = "fork"

  let description =
    Some
      {|Spawn an auxiliary **forked agent** that inherits your *entire* context and solves a focussed sub-task **without polluting the parent conversation**.

When to call
• Long or detail-heavy work (deep debugging, large code generation, extensive data exploration).
• Experiments that may generate irrelevant intermediate chatter.

Invocation
• `command` – command or task.
• `arguments` – optional arguments to pass to the command.

Prompting essentials for GPT-4.1 / O3 reasoning models
1. **Be explicit, be clear** – state goals and required output structure precisely.
2. **Structured output** – delimit sections so they’re machine-parsable.
3. **Expose reasoning** – include your full chain-of-thought in RESULT; it is valuable for audit.
4. **Self-verify** – re-check answers; note any open issues in PERSIST.
5. **Avoid redundant tokens** – no need for phrases like “let’s think step-by-step”; just think and write.

The fork Agent Returns exactly one assistant message, using this template:

```
===RESULT===
<Extremely detailed narrative of everything you did: reasoning, obstacles & fixes, code patches, logs, validation steps, etc.>

===PERSIST===
<Concise (≤20 items) bullet list of facts, artefacts, or next actions that the parent agent must remember. Bullets can be as detailed as needed, but should be succinct.>
```

• Use Markdown; wrap code or patches in fenced blocks.
• RESULT should be exhaustive; PERSIST should be succinct.
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "command", `Object [ "type", `String "string" ]
            ; ( "arguments"
              , `Object
                  [ "type", `String "array"
                  ; "items", `Object [ "type", `String "string" ]
                  ] )
            ] )
      ; "required", `Array [ `String "command"; `String "arguments" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    let command = Jsonaf.string_exn @@ Jsonaf.member_exn "command" j in
    let arguments =
      match Jsonaf.member "arguments" j with
      | Some (`Array arr) -> List.map arr ~f:Jsonaf.string_exn
      | _ -> []
    in
    { command; arguments }
  ;;
end

(** {1 Webpage_to_markdown}

    Definition of the "webpage_to_markdown" tool.  Accepts a single
    [url] string and asks the implementation to download the document
    and convert it to Markdown.  The [input] type is therefore a plain
    string.
*)

module Webpage_to_markdown : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "webpage_to_markdown"
  let type_ = "function"

  let description =
    Some "Download a web page and return its contents converted to Markdown"
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; "properties", `Object [ "url", `Object [ "type", `String "string" ] ]
      ; "required", `Array [ `String "url" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "url" j
  ;;
end

(** {1 Add_line_numbers}

    Metadata for a trivial utility that prefixes every line of a text
    block with its 1-based index.  Receives the raw [text] as input
    and returns the annotated version.
*)

module Add_line_numbers : Ochat_function.Def with type input = string = struct
  type input = string

  let type_ = "function"
  let name = "add_line_numbers"
  let description = Some "add line numbers to a snippet of text"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; "properties", `Object [ "text", `Object [ "type", `String "string" ] ]
      ; "required", `Array [ `String "text" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "text" j
  ;;
end

(** {1 Get_url_content}

    Tool definition that retrieves the raw body of the resource
    located at the given [url].  The [input] is that URL as a string.
*)

module Get_url_content : Ochat_function.Def with type input = string = struct
  type input = string

  let type_ = "function"
  let name = "get_url_content"
  let description = Some "get the contents of a URL"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; "properties", `Object [ "url", `Object [ "type", `String "string" ] ]
      ; "required", `Array [ `String "url" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "url" j
  ;;
end

(** {1 Index_ocaml_code}

    Registers all OCaml sources found under [folder_to_index] in a
    vector search database located at [vector_db_folder].  The
    function later allows semantic code search via
    {!Query_vector_db}.
*)

module Index_ocaml_code : Ochat_function.Def with type input = string * string = struct
  type input = string * string

  let name = "index_ocaml_code"
  let type_ = "function"

  let description =
    Some
      "Index all OCaml code from a folder into a vector search database using OpenAI \
       embeddings"
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "folder_to_index", `Object [ "type", `String "string" ]
            ; "vector_db_folder", `Object [ "type", `String "string" ]
            ] )
      ; "required", `Array [ `String "folder_to_index"; `String "vector_db_folder" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "folder_to_index" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "vector_db_folder" j )
  ;;
end

(** {1 Query_vector_db}

    Performs a semantic search over a previously built vector
    database.  The 4-tuple carried in [input] is

    • [vector_db_folder] – path to the database on disk
    • [query]            – natural-language search query
    • [num_results]      – maximum number of hits to return
    • [index]            – optional secondary index name
*)

module Query_vector_db :
  Ochat_function.Def with type input = string * string * int * string option = struct
  type input = string * string * int * string option

  let name = "query_vector_db"
  let type_ = "function"
  let description = Some "Query a vector database for code snippets given a user query"

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ "vector_db_folder", `Object [ "type", `String "string" ]
            ; "query", `Object [ "type", `String "string" ]
            ; "num_results", `Object [ "type", `String "integer" ]
            ; "index", `Object [ "type", `Array [ `String "string"; `String "null" ] ]
            ] )
      ; ( "required"
        , `Array
            [ `String "vector_db_folder"
            ; `String "query"
            ; `String "num_results"
            ; `String "index"
            ] )
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    ( Jsonaf.string_exn @@ Jsonaf.member_exn "vector_db_folder" j
    , Jsonaf.string_exn @@ Jsonaf.member_exn "query" j
    , Jsonaf.int_exn @@ Jsonaf.member_exn "num_results" j
    , Option.map ~f:Jsonaf.string_exn @@ Jsonaf.member "index" j )
  ;;
end

(** {1 Apply_patch}

    Specification of the *workspace mutation* tool.  The sole string
    input must contain a patch expressed in the project-specific V4A
    format delimited by

    {v
    *** Begin Patch
    ...
    *** End Patch
    v}

    The implementation is responsible for validating and applying the
    patch to the local git repository.
*)

module Apply_patch : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "apply_patch"
  let type_ = "custom"

  let description =
    Some
      {|Applies an atomic V4A unified patch that adds, updates, or deletes one or more text files.  `apply_patch` effectively allows you to execute a diff/patch against a file, but the format of the diff specification is unique to this task, so pay careful attention to these instructions. To use `apply_patch`, you should call the tool with the following structure:

*** Begin Patch
[YOUR_PATCH]
*** End Patch

Where [YOUR_PATCH] is the actual content of your patch, specified in the following V4A diff format.
*** [ACTION] File: [path/to/file] -> ACTION can be one of Add, Update, or Delete.
For each snippet of code that needs to be changed, repeat the following:
[context_before] -> See below for further instructions on context.
- [old_code] -> Precede the old code with a minus sign.
+ [new_code] -> Precede the new, replacement code with a plus sign.
[context_after] -> See below for further instructions on context.

For instructions on [context_before] and [context_after]:
- By default, show 3 lines of code immediately above and 3 lines immediately below each change. If a change is within 3 lines of a previous change, do NOT duplicate the first change’s [context_after] lines in the second change’s [context_before] lines.
- If 3 lines of context is insufficient to uniquely identify the snippet of code within the file, use the @@ operator to indicate the class or function to which the snippet belongs. For instance, we might have:
@@ class BaseClass
[3 lines of pre-context]
- [old_code]
+ [new_code]
[3 lines of post-context]

- If a code block is repeated so many times in a class or function such that even a single `@@` statement and 3 lines of context cannot uniquely identify the snippet of code, you can use multiple `@@` statements to jump to the right context. For instance:

@@ class BaseClass
@@  def method():
[3 lines of pre-context]
- [old_code]
+ [new_code]
[3 lines of post-context]

Note, then, that we do not use line numbers in this diff format, as the context is enough to uniquely identify code. An example of a message that you might pass as "patch" to this function, in order to apply a patch, is shown below.

*** Begin Patch
*** Update File: pygorithm/searching/binary_search.py
@@ class BaseClass
@@     def search():
-        pass
+        raise NotImplementedError()

@@ class Subclass
@@     def search():
-        pass
+        raise NotImplementedError()
*** End Patch

File references can only be relative, NEVER ABSOLUTE.

– When to use:  
  - Cohesive multi-file or multi-line edits that must land together.  
– When NOT to use:  
  - Binary assets or very large files.  
  - Interactive one-off tweaks where a smaller “edit_line” tool exists.

Arguments  
- patch (string, required) – Entire diff.  
  – Must start with "*** Begin Patch" and end with "*** End Patch".  
  – Each file block: "*** Add|Update|Delete File: relative/path".  
  – Hunk rules:  
    - ≥3 unchanged pre- and post-context lines.  
    - "- " for deletions, "+ " for additions; unchanged lines have no prefix.  
    - New files contain only "+ " lines; forgetting the "+" causes failure.  
    - No mixed "+/-" on one line; no line numbers or timestamps.  
    - Use @@ only when 3-line context is ambiguous.


Pre-conditions  
- Target files exist (Update/Delete) or do NOT exist (Add).  
- Workspace matches context exactly; whitespace is significant.  
- path must be relative not absolute


|}
  ;;

  let apply_patch_grammar =
    {|start: begin_patch hunk+ end_patch
begin_patch: "*** Begin Patch" LF
end_patch: "*** End Patch" LF?

hunk: add_hunk | delete_hunk | update_hunk
add_hunk: "*** Add File: " filename LF add_line+
delete_hunk: "*** Delete File: " filename LF
update_hunk: "*** Update File: " filename LF change_move? change?

filename: /(.+)/
add_line: "+" /(.*)/ LF -> line

change_move: "*** Move to: " filename LF
change: (change_context | change_line)+ eof_line?
change_context: ("@@" | "@@ " /(.+)/) LF
change_line: ("+" | "-" | " ") /(.*)/ LF
eof_line: "*** End of File" LF

%import common.LF
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "grammar"
      ; "syntax", `String "lark"
      ; "definition", `String apply_patch_grammar
      ]
  ;;

  let input_of_string s =
    match Or_error.try_with (fun () -> Jsonaf.of_string s) with
    | Ok (`String str) -> str
    | Ok j ->
      (match Jsonaf.member "patch" j with
       | Some v -> Jsonaf.string_exn v
       | None ->
         (match Jsonaf.member "input" j with
          | Some v -> Jsonaf.string_exn v
          | None -> s))
    | Error _ -> s
  ;;
end

module Append_to_file : Ochat_function.Def with type input = string * string = struct
  (** {1 Append_to_file}

      Definition of the "append_to_file" tool.  The tool expects a JSON
      object with two mandatory fields:

      • [path]    – file to modify (string).
      • [content] – text to append (string).

      The wrapper forwards the pair [(path, content)] unchanged.  The
      implementation is responsible for creating the file if it does not
      exist and for inserting a newline *before* [content] so that the
      appended block always starts on its own line. *)
  type input = string * string

  let type_ = "function"
  let name = "append_to_file"

  let description =
    Some
      {|
Append a string to a file at the specified path inserting a newline before the content.

- When to use:
  • When you need to add content to an existing file without overwriting it.
  • When the file is small enough to be safely appended in one go.
- When NOT to use:
  • When you need to modify specific lines or sections of a file.
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ ( "path"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The path of the file to append to."
                  ] )
            ; ( "content"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The content to append to the file."
                  ] )
            ] )
      ; "required", `Array [ `String "path"; `String "content" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    let path = Jsonaf.string_exn @@ Jsonaf.member_exn "path" j in
    let content = Jsonaf.string_exn @@ Jsonaf.member_exn "content" j in
    path, content
  ;;
end

module Find_and_replace :
  Ochat_function.Def with type input = string * string * string * bool = struct
  (** {1 Find_and_replace}

      Metadata for the *in–file substitution* tool.  The JSON payload must
      provide four fields:

      • [path]    – target file (string).
      • [find]    – substring to locate (string, exact match).
      • [replace] – replacement text.
      • [all]     – if [true] replace *all* occurrences; if [false] replace
        exactly one (erroring when multiple matches are present).

      The function returns the 4-tuple [(path, find, replace, all)] for the
      runtime implementation to act upon. *)
  type input = string * string * string * bool

  let type_ = "function"
  let name = "find_and_replace"

  let description =
    Some
      {|
Find and replace an exact matching string in a file at the specified path.

If [all] is true, replaces all occurrences of [find] with [replace].
If [all] is false, replaces only one occurrence of [find] with [replace].
    - errors if all is false and there are multiple occurrences of [find] in the file.

- When to use:
  • When you need to change specific content in a file.
  • When the file is small enough to be safely modified in one go.
  • When you need to replace all occurrences of a string in a file.
- When NOT to use:
  • For large files where modifying could cause performance issues.
  • When you need to add content to the end of a file.
  • when you need to modify one occurrence of a string in a file but there are multiple occurrences.
  • When you need to modify using a regular expression or more complex pattern matching.
|}
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ ( "path"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The path of the file to modify."
                  ] )
            ; ( "find"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The string to find in the file."
                  ] )
            ; ( "replace"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The string to replace with."
                  ] )
            ; ( "all"
              , `Object
                  [ "type", `String "boolean"
                  ; ( "description"
                    , `String
                        "Whether to replace all occurrences (true) or just the one \
                         (false)." )
                  ] )
            ] )
      ; ( "required"
        , `Array [ `String "path"; `String "find"; `String "replace"; `String "all" ] )
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    let path = Jsonaf.string_exn @@ Jsonaf.member_exn "path" j in
    let find = Jsonaf.string_exn @@ Jsonaf.member_exn "find" j in
    let replace = Jsonaf.string_exn @@ Jsonaf.member_exn "replace" j in
    let all = Jsonaf.bool_exn @@ Jsonaf.member_exn "all" j in
    path, find, replace, all
  ;;
end

(** {1 Read_directory}

    Presents a thin wrapper around [Sys.readdir].  Given a [path]
    string, returns (via the implementation) the list of entries in
    that directory.
*)

module Read_directory : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "read_directory"
  let type_ = "function"

  let description =
    Some "Read the contents of a directory and return a list of files and directories."
  ;;

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ ( "path"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The path of the directory to read."
                  ] )
            ] )
      ; "required", `Array [ `String "path" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "path" j
  ;;
end

(** {1 Make_dir}

    Tool definition used to create a new directory on the filesystem.
    The [input] is the destination [path] supplied as a string.
*)

module Make_dir : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "mkdir"
  let type_ = "function"
  let description = Some "Create a directory at the specified path."

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ ( "path"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The path of the directory to create."
                  ] )
            ] )
      ; "required", `Array [ `String "path" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "path" j
  ;;
end

module Import_image : Ochat_function.Def with type input = string = struct
  type input = string

  let name = "import_image"
  let type_ = "function"
  let description = Some "Import an image from the specified path."

  let parameters : Jsonaf.t =
    `Object
      [ "type", `String "object"
      ; ( "properties"
        , `Object
            [ ( "path"
              , `Object
                  [ "type", `String "string"
                  ; "description", `String "The path of the image to import."
                  ] )
            ] )
      ; "required", `Array [ `String "path" ]
      ; "additionalProperties", `False
      ]
  ;;

  let input_of_string s =
    let j = Jsonaf.of_string s in
    Jsonaf.string_exn @@ Jsonaf.member_exn "path" j
  ;;
end
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696eef9f07648195a2203df46f773938">
<summary>
**Checking webpage markdown implementation**

I'm considering the mention of a "head-less Chromium dump" fallback in the webpage_to_markdown function. I think I should verify this by checking the actual implementation and looking at the Webpage_markdown modules, although they might be outdated. I’ll take a quick look at `lib/webpage_markdown/tool.ml` to confirm what's going on. It's important to ensure I have accurate information here, so diving in should help!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696eefb803748195bb08ba21133c6435">
RAW|
Purpose: Verify the actual behavior/limitations of the webpage_to_markdown tool implementation to spot any inaccurate claims in the documentation.
|RAW
</assistant>


<tool_call tool_call_id="call_EaqCs66GJBOgbMrumWEYvdHw" function_name="read_file" id="fc_0a7776dd715389aa00696eefb81788819580cde0f3df47ec28">
RAW|
{"file":"lib/webpage_markdown/tool.ml"}
|RAW
</tool_call>

<tool_response tool_call_id="call_EaqCs66GJBOgbMrumWEYvdHw">
RAW|
(** Webpage-to-Markdown tool implementation.

    {1 Synopsis}

    The module exports a single helper {!register} that turns the declarative
    description {!Definitions.Webpage_to_markdown} into a runnable
    {!Ochat_function.t}.  The implementation:

    • Uses {!Webpage_markdown.Driver.fetch_and_convert} to download a web page
      (or raw GitHub blob) and convert it to Markdown.
    • Stores up to 128 recent results in a TTL-augmented LRU cache
      ({!Ttl_lru_cache}) so that repeated calls for the same URL within
      5 minutes are answered instantly.
    • Reports exceptions as a human-readable string so the calling model can
      surface the error to the user.
*)

open Core
module Output = Openai.Responses.Tool_output.Output

module Url_key = struct
  type t = string [@@deriving sexp, bin_io, hash, compare]

  let invariant (_ : t) = ()
end

module Cache = Ttl_lru_cache.Make (Url_key)

let cache : string Cache.t = Cache.create ~max_size:128 ()
let ttl = Time_ns.Span.of_int_sec 300 (* 5 minutes *)

let register ~env ~dir:_ ~net : Ochat_function.t =
  let run url =
    try
      Cache.find_or_add cache url ~ttl ~default:(fun () ->
        Driver.(fetch_and_convert ~env ~net url |> Markdown.to_string))
    with
    | exn -> Printf.sprintf "Error fetching %s: %s\n" url (Exn.to_string exn)
  in
  Ochat_function.create_function
    (module Definitions.Webpage_to_markdown)
    (fun url -> Output.Text (run url))
;;
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696eefb9f98881959bb6d4c601442698">
<summary>
**Checking tool specifications**

I’m looking into the tool’s caching results, which is said to be 128 within 5 minutes, but the documentation doesn’t mention that. There’s also a claim about a headless Chromium fallback. I can’t confirm this from the code, so I’ll need to check the Webpage_markdown.Driver to see if it handles JavaScript-heavy tasks. Reading the `lib/webpage_markdown/driver.ml` file might give me more clarity on these points!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696eefbcf3e08195bde6f78321f24ec9">
RAW|
Purpose: Inspect webpage_markdown driver to confirm if it uses readability extraction only or any JS/headless fallback, so the docs can be corrected.
|RAW
</assistant>


<tool_call tool_call_id="call_MpHKwjEytjZMAm7JQUi8HTQ7" function_name="read_file" id="fc_0a7776dd715389aa00696eefbcffc081959ec16f911d0333bf">
RAW|
{"file":"lib/webpage_markdown/driver.ml"}
|RAW
</tool_call>

<tool_response tool_call_id="call_MpHKwjEytjZMAm7JQUi8HTQ7">
RAW|
open Core

module Markdown = struct
  type t = string [@@deriving sexp, bin_io, hash, compare]

  let to_string (md : t) : string = md
end

(* -------------------------------------------------------------- *)
(* Utilities                                                      *)
(* -------------------------------------------------------------- *)
let strip_dot ext =
  match String.chop_prefix ext ~prefix:"." with
  | Some s -> s
  | None -> ext
;;

(* add 1-based line numbers preserving original indices *)
let add_line_numbers ?(start_at = 1) (lines : string list) : string =
  List.mapi lines ~f:(fun i line -> sprintf "%d. %s" (start_at + i) line)
  |> String.concat ~sep:"\n"
;;

(* Quick pattern-match for github.com/owner/repo/blob/branch/path#Lx-Ly *)
let github_raw_url_and_lines url : (string * (int * int option)) option =
  let uri = Uri.of_string url in
  match Uri.host uri with
  | Some "github.com" ->
    let path = Uri.path uri |> String.rstrip ~drop:(Char.equal '/') in
    let segments =
      String.split ~on:'/' path |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    (match segments with
     | owner :: repo :: "blob" :: branch :: rest when not (List.is_empty rest) ->
       let raw_path = String.concat ~sep:"/" (owner :: repo :: branch :: rest) in
       let raw_url = "https://raw.githubusercontent.com/" ^ raw_path in
       (* parse fragment *)
       let frag = Uri.fragment uri in
       let line_range =
         match frag with
         | None -> 1, None
         | Some f when String.is_prefix f ~prefix:"L" ->
           let parts = String.split ~on:'-' f in
           (match parts with
            | [ single ] ->
              let n = String.drop_prefix single 1 |> Int.of_string in
              n, Some n
            | [ start_; finish ] when String.is_prefix finish ~prefix:"L" ->
              let s = String.drop_prefix start_ 1 |> Int.of_string in
              let e = String.drop_prefix finish 1 |> Int.of_string in
              s, Some e
            | _ -> 1, None)
         | _ -> 1, None
       in
       Some (raw_url, line_range)
     | _ -> None)
  | _ -> None
;;

(* Fetch GitHub raw file; return specialised Markdown if recognised *)
let try_github_fast_path ~net url : string option =
  match github_raw_url_and_lines url with
  | None -> None
  | Some (raw_url, (lstart, lend_opt)) ->
    (match Fetch.get ~net raw_url with
     | Error _ -> None
     | Ok body ->
       let ext = Stdlib.Filename.extension (Uri.path (Uri.of_string raw_url)) in
       (* Slice lines if anchor present *)
       let body =
         match lend_opt with
         | None when lstart = 1 -> body
         | _ ->
           let lines = String.split_lines body in
           let len = List.length lines in
           let lstart_idx = Int.max 1 lstart in
           let lend = Option.value lend_opt ~default:lstart_idx in
           let lend_idx = Int.min len lend in
           let slice = List.slice lines (lstart_idx - 1) lend_idx in
           String.concat ~sep:"\n" slice
       in
       let is_markdown =
         List.mem [ ".md"; ".markdown"; ".mdown"; ".mkdn" ] ext ~equal:String.equal
       in
       if is_markdown
       then Some body
       else (
         let lang = strip_dot ext in
         Some
           (sprintf
              "```%s\n%s\n```"
              lang
              (add_line_numbers ~start_at:lstart (String.split_lines body)))))
;;

let html_to_markdown_string html =
  let parse_with_soup () =
    try Ok (Soup.parse html) with
    | Soup.Parse_error _ -> Error ()
  in
  let parse_with_markup () =
    try
      let signals = Markup.string html |> Markup.parse_html |> Markup.signals in
      Ok (Soup.from_signals signals)
    with
    | _ -> Error ()
  in
  let soup_res =
    match parse_with_soup () with
    | Ok s -> Some s
    | Error () ->
      (match parse_with_markup () with
       | Ok s -> Some s
       | Error () -> None)
  in
  match soup_res with
  | Some soup ->
    (try Html_to_md.convert soup |> Md_render.to_string with
     | Soup.Parse_error _ | Failure _ | Invalid_argument _ | _ ->
       Printf.sprintf "```html\n%s\n```" html)
  | None -> Printf.sprintf "```html\n%s\n```" html
;;

let fetch_and_convert ~env ~net url =
  (* 1. GitHub-optimised path *)
  match try_github_fast_path ~net url with
  | Some md -> md
  | None ->
    (* 2. Generic HTML → Markdown path (existing) *)
    (match Fetch.get ~net url with
     | Error msg -> msg
     | Ok html ->
       (match html_to_markdown_string html with
        | "" ->
          (* Attempt to fetch the page using a headless Chrome browser for progressive web apps *)
          let try_chrome_headless (url : string) : string =
            let proc_mgr = Eio.Stdenv.process_mgr env in
            Eio.Switch.run
            @@ fun sw ->
            (* 1.  Pipe for capturing stdout. *)
            let r, w = Eio.Process.pipe ~sw proc_mgr in
            (* 2. Pipe for capturing stderr. *)
            let _, w_err = Eio.Process.pipe ~sw proc_mgr in
            match
              Eio.Process.spawn
                ~sw
                proc_mgr
                ~stdout:w
                ~stderr:w_err
                [ "chrome-dump"; url ]
            with
            | exception ex ->
              let err_msg = Fmt.str "error running %s fetch: %a" url Eio.Exn.pp ex in
              Eio.Flow.close w;
              Eio.Flow.close w_err;
              err_msg
            | _child ->
              Eio.Flow.close w;
              Eio.Flow.close w_err;
              (match
                 Eio.Buf_read.parse_exn ~max_size:1_000_000 Eio.Buf_read.take_all r
               with
               | res ->
                 let max_len = 1000000 in
                 let res =
                   if String.length res > max_len
                   then String.append (String.sub res ~pos:0 ~len:max_len) " ...truncated"
                   else res
                 in
                 res
               | exception ex -> Fmt.str "error running %s fetch: %a" url Eio.Exn.pp ex)
          in
          (* timeout functioin eio *)
          let try_chrome_headless_wto x =
            try
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60.0 (fun () ->
                try_chrome_headless x)
            with
            | Eio.Time.Timeout ->
              Printf.sprintf "timeout running chrome_dump command %s" x
            | ex ->
              Printf.sprintf
                "error running chrome_dump command %s: %ss"
                x
                (Exn.to_string ex)
          in
          html_to_markdown_string (try_chrome_headless_wto url)
        | md -> md))
;;

let convert_html_file path =
  let html = Eio.Path.load path in
  html_to_markdown_string html
;;
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696eefbee9808195b4f569f6c4a73a4e">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696eefd701408195b72d80c8dabf5dfa">
RAW|
### What I did:
- Reviewed the current tools doc: `docs-src/overview/tools.md`.
- Cross-checked it against the code paths that define “what tools exist” + “how they behave”:
  - ChatMD `<tool …/>` parsing/semantics: `lib/chatmd/prompt.mli`, `lib/chatmd/prompt.ml`
  - Tool instantiation & builtin mapping: `lib/chat_response/tool.ml`
  - Builtin tool implementations: `lib/functions.mli`, `lib/functions.ml`
  - Tool schemas / canonical tool names: `lib/definitions.mli`, `lib/definitions.ml`
  - MCP server defaults & behavior: `bin/mcp_server.ml`
  - TUI flags affecting tool execution: `bin/chat_tui.ml`
  - `webpage_to_markdown` implementation details: `lib/webpage_markdown/tool.ml`, `lib/webpage_markdown/driver.ml`

### Conclusion:
Below is a **code-truth, detailed list** of what’s wrong/missing in `docs-src/overview/tools.md` and what we should add/change.

---

## 1) Built-in tools list: missing tools + incorrect naming/aliases

### 1.1 Missing builtin in the doc: `import_image`
- **Doc is missing**: `import_image`
- **Code-truth**: It’s exposed as a ChatMD builtin via `lib/chat_response/tool.ml`:
  - `"import_image" -> [ Functions.import_image ~dir:(Ctx.tool_dir ctx) ]`
- **Why it matters / “sell” angle**: it enables vision workflows by converting a local image file into an OpenAI “image input” content part (base64 data URI). This is a big differentiator.

### 1.2 Doc lists `get_contents` and `read_dir`, but canonical tool names are different at runtime
This is subtle but important: ChatMD builtin *selectors* don’t always match the OpenAI tool name actually advertised.

- **`get_contents` vs `read_file`**
  - Doc currently lists: `get_contents`
  - **Code-truth**:
    - ChatMD accepts `<tool name="get_contents"/>` **and** `<tool name="read_file"/>` (see `lib/chat_response/tool.ml`).
    - But the tool *definition name* is actually **`read_file`** (`Definitions.Get_contents.name = "read_file"` in `lib/definitions.ml`).
  - Consequence: the model will see/call **`read_file`** even if the prompt declared `get_contents`.

- **`read_dir` vs `read_directory`**
  - Doc currently lists: `read_dir`
  - **Code-truth**:
    - ChatMD builtin key is `"read_dir"` (see `Tool.of_declaration` mapping)
    - But the underlying tool definition is **`read_directory`** (`Definitions.Read_directory.name = "read_directory"` in `lib/definitions.ml`).
  - Consequence: the model will see/call **`read_directory`**.

**Doc update needed**
- The “Built-in toolbox” section should clearly separate:
  - **ChatMD declaration names** you write in `<tool name="…"/>` (includes aliases like `read_dir`, `get_contents`)
  - vs **actual tool names** exposed to the model (`read_directory`, `read_file`)

This is currently a source of confusion and makes the doc “feel wrong” when someone tries it.

---

## 2) Major incorrect section: “Rolling your own OCaml tool – Declare it once in ChatMD”
This is the biggest correctness issue in the current doc.

### What the doc claims (currently)
It implies you can write an OCaml `create_function` tool and then simply do:

```xml
<tool name="say_hello"/>
```

…and it “just works”.

### Code-truth
That is **not true** in the current architecture:

- `<tool name="..."/>` with no `command/agent/mcp_server` becomes `CM.Builtin name` (parser in `lib/chatmd/prompt.ml`)
- Builtins are **hard-mapped** in `lib/chat_response/tool.ml` in the `CM.Builtin name -> match name with ...` block.
- Any unknown name causes a hard failure:
  - `failwithf "Unknown built-in tool: %s" other ()`

### What needs to change in the doc
Replace that section with something accurate along these lines:

- “You can add OCaml tools if you are embedding ochat as a library and passing your own tool list to the driver” **or**
- “To make a new OCaml tool available via ChatMD `<tool name="…"/>`, you must add it to `Tool.of_declaration` and rebuild” **or**
- “Expose it via MCP (recommended for extensibility), then mount via `<tool mcp_server="…"/>`”

Right now it’s actively misleading for first-time readers.

---

## 3) Missing docs for a whole tool category: `<tool agent="..."/>`

### Code-truth
ChatMD supports agent tools:

```xml
<tool name="my_agent" agent="prompts/my_agent.chatmd" local />
```

- Parser behavior: `lib/chatmd/prompt.ml`
  - `agent="..."` creates `CM.Agent { name; agent; is_local = Hashtbl.mem tbl "local"; ... }`
- Runtime behavior: `lib/chat_response/tool.ml` (`agent_fn`)
  - Tool schema is **fixed**: `{ "input": string }`
  - Runs a nested ChatMD driver via the `run_agent` callback, using `Fetch.get` to load the agent prompt.

### Doc update needed
Add a dedicated section explaining:
- what agent tools are good for (task decomposition, specialized “mini agents”)
- how `local` works
- the fixed JSON schema
- how it behaves (fresh history, inherits ctx capabilities, returns the final answer)

This is a major “sell” feature that’s currently absent from `tools.md`.

---

## 4) Shell wrapper tools: incorrect/outdated operational details

The doc is mostly directionally right, but it misses/gets wrong key constraints that users will hit quickly.

### 4.1 Output truncation limit is wrong in code vs comments
- `lib/chat_response/tool.ml` `custom_fn`:
  - reads up to `~max_size:1_000_000` from the pipe
  - then truncates to `max_len = 10000` characters
- The comment above it claims “100 KiB”, but implementation is **10,000 chars**.

**Doc update needed**: state the real truncation behavior (and ideally note that stdout+stderr are combined).

### 4.2 `command="..."` parsing limitations (important!)
- The wrapper:
  - replaces `%20` with spaces
  - splits on whitespace (no proper shell quoting)
- This should be documented plainly because it affects how people write commands (e.g. quoted args won’t behave like a shell).

### 4.3 Timeout behavior
- Hard timeout is **60s** (implemented).
- Doc mentions a timeout, but should specify the actual value and how failure is reported (“timeout running command …”).

---

## 5) `webpage_to_markdown`: doc misses big features + might be slightly inaccurate

### Code-truth features to document (and emphasize)
From `lib/webpage_markdown/driver.ml` and `lib/webpage_markdown/tool.ml`:

1. **GitHub blob fast-path**
   - Converts `github.com/.../blob/...` to `raw.githubusercontent.com/...`
   - Supports `#Lx-Ly` line slicing
   - If non-markdown file, it wraps content in fenced code blocks and **adds line numbers**
   - This is extremely useful and should be highlighted prominently.

2. **Caching**
   - Tool caches up to **128** URLs for **5 minutes**.

3. **Headless fallback exists, but it’s specific**
   - It tries `chrome-dump <url>` only if the initial HTML→MD conversion yields an empty string.
   - That nuance matters; “JS-heavy sites fall back” is *mostly* true, but the trigger is “empty conversion result”.

**Doc update needed**
- Add a “behavior highlights” subsection for this tool (GitHub support + cache + fallback + timeouts).

---

## 6) `read_file` (`get_contents`): doc vs implementation mismatches

### 6.1 Truncation message text doesn’t match docs/schema description
- `Definitions.Get_contents.description` mentions:
  - `"[File truncated: %d more bytes not shown]"`
- Implementation in `lib/functions.ml` appends:
  - `"\n\n---\n[File truncated]"` (no byte count)

**Doc update needed**: document what users actually see today.

### 6.2 Binary-file refusal is a key behavior not emphasized enough
- `Functions.get_contents` checks for NUL + UTF-8 validity and refuses binary files with:
  - `"Refusing to read binary file: ..."`
This should be emphasized (it’s good: prevents garbage / huge binary spam in context).

### 6.3 `offset` semantics
- The code skips content based on per-line `String.length` accumulation, not strict “byte offset”.
- The doc should avoid over-promising “byte-exact offset”; describe it as a chunking/skip aid.

---

## 7) `append_to_file`: doc claims idempotence; implementation is not idempotent
- `lib/functions.mli` claims: “idempotent when the text is already present at the end of the file”
- Actual implementation (`lib/functions.ml`) always appends `"\n" ^ content` with no check.

**Doc update needed**: either
- document the true behavior (always appends; always inserts a newline first), or
- (separately) we could fix code later—but per your request, at least the doc must stop claiming idempotence.

---

## 8) `find_and_replace`: error semantics are softer than the doc implies
- The schema/description says it “errors if all=false and multiple occurrences…”
- Implementation returns an **error string**, not a hard tool failure, and recommends using `apply_patch` instead.

**Doc update needed**: describe the actual behavior/output.

---

## 9) MCP section: add missing “how it really works” + sell the built-in server

The current MCP section is decent, but it misses a lot of real value and a few important details.

### 9.1 MCP client caching + invalidation
From `lib/chat_response/tool.ml`:
- TTL-LRU cache: **max 32**, **TTL 5 minutes**, keyed by `mcp_server`
- Invalidation listener drops cache entry on:
  - `notifications/tools/list_changed`

**Doc update needed**: mention this explicitly (it’s a real performance + “dynamic tools” selling point).

### 9.2 Tool selection precedence
From `lib/chatmd/prompt.ml`:
- For MCP `<tool ...>`:
  - If `name` is non-empty → selects that single tool, and it **wins over** include(s)
  - Else `include` / `includes` parsed as comma-separated list
  - Else exports all tools

**Doc update needed**: clarify precedence and that `strict` is a boolean presence flag (not `strict="true"`).

### 9.3 Credential env vars are injected as query params
From `lib/chat_response/tool.ml`:
- `client_id_env` and `client_secret_env` cause `client_id`/`client_secret` query params to be added **if env vars exist**.

Doc mentions this, but should also clarify: it does **not** error if env var missing; it just omits.

### 9.4 Transport schemes (important practical detail)
From `lib/mcp/mcp_client.ml` (as summarized in your scratch list):
- Supports:
  - `stdio:...`
  - `http`, `https`, `mcp+http`, `mcp+https`
  - fallback to stdio in some cases

This should be documented so people know what URIs are accepted.

### 9.5 Strong “sell” missing: ochat ships an MCP server that exposes tools + prompt-agents
From `bin/mcp_server.ml`:
- The MCP server binary registers built-ins by default:
  - `echo`, `apply_patch`, `read_dir`, `get_contents`, `meta_refine`, `webpage_to_markdown`
- It scans a prompts directory (`$MCP_PROMPTS_DIR` or `./prompts`) and registers every `*.chatmd` twice:
  - as a prompt
  - as a tool (agent-backed) via `Mcp_prompt_agent`

**Doc update needed**
- Add a section like “Run your own MCP server in 30 seconds” and show:
  - how to run it (stdio and/or HTTP)
  - how prompt hot-reload/list_changed works conceptually
  - why this is powerful (remote tool sandboxing, reuse across sessions, shareable tool catalogs)

---

## 10) Tool execution behavior: docs should mention parallel tool calls (and that it’s configurable)
- `bin/chat_tui.ml` exposes:
  - `--parallel-tool-calls` / `--no-parallel-tool-calls`
- Driver supports parallel execution (your scratch list notes deterministic ordering, etc.).

**Doc update needed**: add a short “Runtime behavior” note:
- parallel tool calls are supported (and how to toggle)
- why it matters (latency when multiple independent tools are called)

---

## 11) Minor doc polish / consistency issues to fix while we’re in there
- Tone/consistency: the doc currently uses emojis (e.g. “Tip 💡”, “👋” in sample output). If you want project docs to feel professional/consistent, we should remove or reduce those.
- The “7 helpers” callout should likely be updated to include **`import_image`** and to avoid calling out tools that aren’t actually ChatMD-declarable (`mkdir`), unless explicitly framed as “OCaml-only”.

---

If you want, next I can turn this into a concrete “doc change checklist” (section-by-section edits) before we touch `docs-src/overview/tools.md`.
|RAW
</assistant>

<user>
yes turn this into a concrete doc change checklist (section-by-section edits) before we touch `docs-src/overview/tools.md`
</user>

<assistant id="msg_0a7776dd715389aa00696ef0234464819584dc71d781516a30">
RAW|
### What I did:
- Converted the previously identified mismatches (doc vs code) into a **section-by-section edit checklist** for `docs-src/overview/tools.md`, grounded in the actual implementation and naming behavior.

### Conclusion:
## Concrete doc change checklist (section-by-section)

> Target file: `docs-src/overview/tools.md`  
> Goal: make it code-correct, add missing tools/features, and “sell” the strongest capabilities.

---

## 0) Page intro (top paragraph)
**Edits**
- Add a 2–4 sentence “what tools enable” pitch:
  - ChatMD declarative tool mounting (`<tool …/>`)
  - built-ins for repo editing + retrieval
  - shell wrappers for fast custom tooling
  - MCP for remote tool catalogs / sandboxing / sharing
  - agent-tools for structured sub-agents

**Add**
- A short “At a glance” bullet list linking to:
  - Builtins
  - Shell wrappers
  - Agent tools
  - MCP tools
  - Tool call execution (parallelism, etc.)

---

## 1) “Built-in toolbox” section (table)
### 1.1 Fix names + explicitly document aliasing
**Edits**
- Change the table to include two columns (or add notes):
  - **ChatMD declaration name(s)** (what you put in `<tool name="…"/>`)
  - **Actual tool name exposed to the model** (what shows up in tool calls)

**Must fix**
- `get_contents` row:
  - ChatMD: `get_contents` **or** `read_file`
  - Model tool name: `read_file`
- `read_dir` row:
  - ChatMD: `read_dir`
  - Model tool name: `read_directory`

### 1.2 Add missing builtin tool: `import_image`
**Add row**
- ChatMD: `import_image`
- Model tool name: `import_image`
- Category: “vision / input”
- Description: “Load a local image and return it as an image input item (data URI) usable by vision-capable models.”

### 1.3 Remove or reframe tools that are not actually mountable as ChatMD builtins
**Doc currently mentions**
- `mkdir` as “OCaml API only; not exposed as ChatMD `<tool>` yet”
- `get_url_content` similarly

**Edits (choose one approach)**
- Preferred: move these out of the “Built-in toolbox” table into a separate subsection:
  - “OCaml-only helpers (not mountable via `<tool name="…"/>` by default)”
- Or: keep them but label clearly: “Not available via ChatMD builtins unless you add them to `Tool.of_declaration` or expose via MCP.”

### 1.4 Add “most useful builtins” emphasis in the table itself
**Edits**
- Mark “Recommended core set” (or “Most used”) next to:
  - `apply_patch`
  - `read_file` (declared as `get_contents`/`read_file`)
  - `read_directory` (declared as `read_dir`)
  - `webpage_to_markdown`
  - `markdown_search` + `index_markdown_docs`
  - `odoc_search`
  - `query_vector_db` + `index_ocaml_code`
  - `import_image`

---

## 2) “Deep-dive: 7 helpers …” callout
### 2.1 Update the set of highlighted tools
**Edits**
- Ensure everything highlighted is truly available and high-value.
- Consider updating from “7 helpers” to “Top built-ins” (or keep 7 but pick the best 7).

**Must include**
- Add `import_image` as a top-tier feature (vision workflows).

**Must fix**
- `mkdir` section: currently presented as a noteworthy helper, but it’s not actually available as a ChatMD builtin. Either:
  - remove it from this callout, or
  - explicitly label it “OCaml-only unless exposed via MCP / added to Tool.of_declaration”.

### 2.2 Correct `webpage_to_markdown` claims
**Edits**
- Add the GitHub blob fast-path and `#Lx-Ly` slicing (big selling point).
- Mention caching (5 min TTL, 128 entries) and the `chrome-dump` fallback with the correct caveat (used when conversion yields empty output).

---

## 3) New section to add: “Agent tools (`<tool agent="..."/>`)”
**Add an entire new section** (this is currently missing).

**Include**
- What it is: “Turn a `*.chatmd` prompt into a callable tool (nested agent).”
- Syntax examples:
  - local agent: `<tool name="triage" agent="prompts/triage.chatmd" local />`
  - non-local agent: `<tool name="triage" agent="https://…" />` (if supported in your workflows)
- Input schema (fixed): `{ "input": "..." }`
- Behavior notes:
  - runs a fresh sub-conversation (no inherited message history)
  - inherits execution context (dir/net/env)
  - returns the agent’s final answer as tool output
- “When to use” bullets: decomposition, specialized mini-prompts, repeatable workflows.

**Sell angle**
- “This is the easiest way to build a small tool-like workflow without writing code.”

---

## 4) “Importing remote MCP tools” section
### 4.1 Add a short “How selection works” block (precedence)
**Edits**
- Explicitly state precedence:
  - `name="..."` selects one tool and overrides `include(s)`
  - else `include="a,b"` / `includes="a,b"` selects a subset
  - else all tools are exposed

### 4.2 Document caching + invalidation
**Add**
- Client caches `tools/list` per server (TTL-LRU, ~5 minutes, small max size).
- List invalidation: server can emit `notifications/tools/list_changed`, which ochat listens for and refreshes next time.

### 4.3 Expand “connection URIs / transports”
**Add**
- Examples of supported URIs:
  - `stdio:npx -y …`
  - `https://…`
  - (if you want) note `mcp+http(s)` support
- Mention that `client_id_env` / `client_secret_env` inject query params if env vars are set (and do nothing if unset).

### 4.4 Add a “Run your own MCP server” subsection (big “sell”)
**Add**
- Mention `bin/mcp_server`:
  - registers builtins: `apply_patch`, `read_dir`, `get_contents`, `meta_refine`, `webpage_to_markdown`, plus `echo`
  - auto-registers every `*.chatmd` in `./prompts` or `$MCP_PROMPTS_DIR` as both:
    - an MCP prompt
    - an agent-backed MCP tool
- Include one minimal example:
  - run server
  - consume via `<tool mcp_server="stdio:..."/>` or HTTP endpoint
- Mention that list-changed notifications enable hot refresh (even if prompts are polled).

---

## 5) “Rolling your own OCaml tool – 20 lines …” section (currently incorrect)
**Replace this section** with something accurate.

### Option A (recommended): “Three ways to extend tools”
**Rewrite content to explain**
1) **Shell wrapper** (fastest)
2) **Agent tool** (structured workflows)
3) **MCP server tool** (shareable + sandboxable)
4) **Embedding ochat as a library** (advanced; add OCaml tool list in your host program)

### Must remove / correct
- Remove the claim that `<tool name="say_hello"/>` will work without code changes.
- If you keep the OCaml snippet, explicitly state:
  - “This is for library/embedding use” OR
  - “To expose this via ChatMD `<tool name=…/>`, you must add a mapping in `lib/chat_response/tool.ml` and rebuild.”

---

## 6) “Shell-command wrappers” section
### 6.1 Correct the operational constraints
**Edits**
- Add explicit details:
  - hard timeout: 60 seconds
  - stdout+stderr combined
  - output truncated (document the real truncation behavior as implemented)
  - `command="..."` parsing: `%20` decoding + whitespace split (no shell quoting)

### 6.2 Add a “Best practice schema discipline” note
**Add**
- Encourage pinning dangerous flags into `command="..."` and exposing only safe args via `arguments`.

---

## 7) Add a small “Tool behavior gotchas / guarantees” section
This should collect the “things people get wrong immediately”.

**Include bullets**
- Builtin naming mismatch / aliasing (ChatMD name vs tool name)
- `read_file` refuses binary files
- `append_to_file` always appends (not idempotent)
- `find_and_replace` returns an error message string rather than raising
- Parallel tool calls can be enabled/disabled in TUI (`--no-parallel-tool-calls`)

---

## 8) Optional: tighten tone/format consistency
**Edits**
- Remove emojis from “Tip 💡” and sample outputs (unless you want them project-wide).
- Keep code fences consistent (`xml`, `ocaml`, `sh`).

---

If you confirm this checklist looks right, next step is: I’ll propose an updated outline for `docs-src/overview/tools.md` (final headings + flow), then we can patch the file.
|RAW
</assistant>

<user>
yes do that
</user>

<reasoning id="rs_0a7776dd715389aa00696ef1049880819598f05b96c8b8def7">
<summary>
**Structuring outline headings**

I need to propose an updated flow for the outline headings, which is a straightforward request. I can focus on creating a clear structure within a single markdown section. It seems like I should include main headings and possibly some subheadings to make it easier to follow. Let’s think about the titles that would clearly communicate each section's purpose and keep everything organized. Simple and effective is key here!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef10785f88195b6762250cdca5c50">
RAW|
## Proposed updated outline for `docs-src/overview/tools.md`

### 1) Tools overview (why this matters)
- 3–6 sentence intro: what tools are, what problems they solve (repo editing, retrieval, automation, remote tool catalogs).
- “Choose your tool backend” quick map:
  - Built-ins (`<tool name="…"/>`)
  - Shell wrappers (`<tool command="…"/>`)
  - Agent tools (`<tool agent="…"/>`)
  - MCP tools (`<tool mcp_server="…"/>`)

---

### 2) Built-in tools (what ships with ochat)
#### 2.1 Recommended “core set” (sell-first)
- Short, high-impact list with 1–2 lines each:
  - `apply_patch` (atomic multi-file edits)
  - `read_file` (declared as `get_contents`/`read_file`) (safe reading, truncation)
  - `read_directory` (declared as `read_dir`) (repo navigation)
  - `webpage_to_markdown` (GitHub blob fast-path, caching)
  - `index_markdown_docs` + `markdown_search` (project docs RAG)
  - `index_ocaml_code` + `query_vector_db` (codebase RAG)
  - `odoc_search` (local package docs search)
  - `import_image` (vision input)

#### 2.2 Built-in tool catalog (full table; code-correct)
A table with columns like:
- **ChatMD declaration name(s)**
- **Tool name model sees**
- **Category**
- **Purpose / key behavior**
- (Optional) “Typical usage” (read-only vs mutating)

Include explicit alias rows:
- `get_contents` / `read_file` → model sees `read_file`
- `read_dir` → model sees `read_directory`

#### 2.3 Notes & gotchas for built-ins (small but crucial)
- Naming/aliasing explanation (ChatMD name vs tool name).
- `read_file` binary refusal + truncation + offset behavior (as implemented).
- `append_to_file` always appends (no idempotence claim).
- `find_and_replace` “multiple matches” behavior returns error text.

---

### 3) `webpage_to_markdown` deep dive (feature highlight)
- “What it’s great for” bullets (docs ingestion, GitHub code review, issue triage).
- Behavior highlights:
  - GitHub blob → raw fast-path
  - `#Lx-Ly` line slicing + line-numbered fences for code
  - caching (5 min TTL, 128 entries)
  - fallback behavior (`chrome-dump` when conversion yields empty output)
- Example snippets.

---

### 4) Agent tools: turn ChatMD prompts into callable tools (`<tool agent="…"/>`)
- What it is, why it’s powerful (reusable mini-agents, decomposition).
- Syntax:
  - local agent (`local` flag)
  - non-local agent (if used)
- Input schema (fixed): `{ "input": string }`
- Execution model:
  - fresh history, shared context/capabilities, returns final answer
- “When to use / when not to use”

---

### 5) Shell-command wrapper tools (`<tool command="…"/>`)
- What it is: quickest way to add a custom tool.
- Security warning (keep; tighten wording).
- Precise runtime behavior (code-correct):
  - input schema: `arguments: string[]`
  - command parsing limitations (no shell quoting; `%20` handling)
  - stdout+stderr combined
  - timeout (60s)
  - truncation behavior (document real behavior)
- Design guidelines:
  - pin flags in `command=...`
  - keep wrappers narrowly scoped
  - prefer idempotent/read-only wrappers where possible

---

### 6) MCP tools: import remote tool catalogs (`<tool mcp_server="…"/>`)
#### 6.1 One-liners (examples)
- stdio example
- https example
- subset selection example

#### 6.2 Selection semantics (clear precedence)
- `name=` overrides include(s)
- `include=` / `includes=`
- default: all tools

#### 6.3 Connection & auth knobs
- supported URI forms (stdio/http/https/(mcp+http(s) if you want to mention))
- `client_id_env` / `client_secret_env` injection behavior (no env var = no param)

#### 6.4 Caching & refresh behavior (sell performance + dynamism)
- tool list caching (TTL-LRU)
- `notifications/tools/list_changed` invalidation

---

### 7) Running ochat’s MCP server (shareable tooling + prompt-as-tool)
- What `mcp_server` provides out of the box:
  - builtins exposed (list)
  - `*.chatmd` prompts become MCP prompts + MCP tools automatically
  - prompt directory selection (`./prompts` or `$MCP_PROMPTS_DIR`)
- Basic usage patterns:
  - local stdio mode consumption
  - HTTP mode (if you want to document it; depends on your desired surface)
- Suggested “production pattern”: run server in container/sandbox and mount remotely.

---

### 8) Tool calling runtime behavior (parallelism + transparency)
- Note parallel tool execution exists and is toggleable in TUI:
  - `--parallel-tool-calls` / `--no-parallel-tool-calls`
- Brief note on why it matters (latency when multiple tool calls).

---

### 9) Extending ochat with new OCaml tools (corrected, no false promises)
- Replace the current misleading “declare it once in ChatMD” story with:
  - If embedding as a library: you can register custom `Ochat_function.t` directly.
  - To make it available via ChatMD `<tool name="…"/>`: add mapping in `lib/chat_response/tool.ml` and rebuild.
  - Recommended alternative: expose via MCP for zero-rebuild distribution.

---

If you want, I can now take this outline and directly rewrite `docs-src/overview/tools.md` accordingly (keeping your current content where valid, but reorganizing it to match this flow).
|RAW
</assistant>

<user>
yes do that
</user>

<reasoning id="rs_0a7776dd715389aa00696ef14f7d8481959f656dbbcdf7ecef">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef155943481958e2efc61b89c94e7">
RAW|
Purpose: Rewrite `docs-src/overview/tools.md` to match the code (builtins/aliases/behaviors), add missing tool documentation (agent tools, import_image, MCP server), and strengthen the “selling” narrative.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_cgD0SFlvHQQeeZ6IWkKlzMXM" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef155e79c8195ae73618876bf0253">
RAW|
*** Begin Patch
*** Update File: docs-src/overview/tools.md
@@
-# Tools – built-ins, custom helpers & MCP
-
-This page collects the tools-related material from the README and expands on
-how built-ins, shell wrappers, custom OCaml helpers and remote MCP tools fit
-together in ChatMD.
+# Tools – built-ins, agent tools, shell wrappers & MCP
+
+This page explains how **tool calling** works in ochat/ChatMD and how to equip
+an assistant with the right capabilities for real work:
+
+- **Built-in tools** (fast, batteries-included): repo editing, file reading, search and indexing.
+- **Agent tools**: turn `*.chatmd` prompts into callable sub-agents.
+- **Shell wrapper tools**: expose a *specific* command as a function-callable tool.
+- **MCP tools**: mount remote tool catalogs (local stdio, HTTP, sandboxed containers, CI, etc.).
+
+The important mental model: **tools are opt-in**. The model can only call tools
+declared in the prompt via `<tool .../>`.
 
 ---
 
-## Built-in toolbox
+## Built-in tools
+
+### Recommended core set (start here)
+
+If you only skim one section, use this set. It covers most day-to-day workflows:
+
+- **`apply_patch`** – atomic multi-file edits in a structured patch format (best for real code changes).
+- **`read_file`** *(declared as `get_contents` or `read_file`)* – fast, safe file reads with truncation + optional offset.
+- **`read_directory`** *(declared as `read_dir`)* – list a directory to navigate a repository without guessing paths.
+- **`webpage_to_markdown`** – ingest web pages (and GitHub blob URLs) as clean Markdown.
+- **`index_markdown_docs` + `markdown_search`** – semantic search over your project docs/wiki/design notes.
+- **`index_ocaml_code` + `query_vector_db`** – hybrid retrieval over a codebase (dense + BM25 overlay).
+- **`odoc_search`** – semantic search over locally indexed OCaml docs (installed packages + project snapshot).
+- **`import_image`** – bring a local image into the model as a vision input (great for screenshots, diagrams).
+
+### Built-in catalog (code-correct)
+
+There are two names to be aware of:
+
+1. **ChatMD declaration name**: what you write in `<tool name="…"/>`.
+2. **Tool name the model sees**: the function name advertised to the OpenAI Responses API.
+
+Some built-ins have *declaration aliases* for backward compatibility.
 
-| Name | Category | Description |
-|------|----------|-------------|
-| `apply_patch`         | repo      | Apply an *Ochat diff* (V4A) to the working tree |
-| `read_dir`            | fs        | List entries (non-recursive) in a directory; returns plain-text lines |
-| `get_contents`        | fs        | Read a file (UTF-8); truncates very large files and supports an optional `offset` argument |
-| `get_url_content` *(experimental)* | web       | Download a raw resource and strip HTML to text *(OCaml API only; not exposed as a ChatMD `<tool>`)* |
-| `webpage_to_markdown` | web       | Download a page & convert it to Markdown |
-| `index_ocaml_code`    | index     | Build a vector index from a source tree |
-| `index_markdown_docs` | index     | Vector-index a folder of Markdown files |
-| `odoc_search`         | docs      | Semantic search over installed OCaml API docs |
-| `markdown_search` / `md-search` | search | Query Markdown indexes created by `index_markdown_docs` (ChatMD uses `markdown_search`; `md-search` is the CLI wrapper) |
-| `query_vector_db`     | search    | Hybrid dense + BM25 search over source indices |
-| `fork`                | misc      | Reserved name for future multi-agent flows; currently implemented as a placeholder tool |
-| `mkdir` *(experimental)*               | fs        | Create a directory (idempotent) *(OCaml API only; not exposed as a ChatMD `<tool>` yet)* |
-| `append_to_file`      | fs        | Append text to a file, creating it if absent |
-| `find_and_replace`    | fs        | Replace occurrences of a string in a file (single or all) |
-| `meta_refine`         | meta      | Recursive prompt refinement utility |
+| ChatMD `<tool name="…"/>` | Model sees | Category | What it does |
+|---|---|---|---|
+| `apply_patch` | `apply_patch` | repo | Apply an atomic V4A patch to the working tree. Best for multi-file edits and precise changes. |
+| `read_dir` | `read_directory` | fs | List directory entries (non-recursive) as newline-delimited text. |
+| `get_contents` **or** `read_file` | `read_file` | fs | Read a UTF-8 text file with truncation and optional `offset`. Refuses binary files. |
+| `append_to_file` | `append_to_file` | fs | Append text to a file (inserts a newline before the appended content). |
+| `find_and_replace` | `find_and_replace` | fs | Replace an exact substring in a file (single or all occurrences). |
+| `webpage_to_markdown` | `webpage_to_markdown` | web | Download a web page and convert it to Markdown (includes a GitHub blob fast-path). |
+| `index_ocaml_code` | `index_ocaml_code` | index | Build a vector index from an OCaml source tree. |
+| `query_vector_db` | `query_vector_db` | search | Hybrid retrieval over code indices (dense + BM25 overlay). |
+| `index_markdown_docs` | `index_markdown_docs` | index | Index a folder of Markdown files into a vector DB (default root: `.md_index`). |
+| `markdown_search` | `markdown_search` | search | Semantic search over Markdown indices created by `index_markdown_docs`. |
+| `odoc_search` | `odoc_search` | docs | Semantic search over locally indexed odoc docs. |
+| `meta_refine` | `meta_refine` | meta | Recursive prompt refinement utility (meta-prompting flow). |
+| `import_image` | `import_image` | vision | Load a local image file and return a vision input item (data URI) for vision-capable models. |
+| `fork` | `fork` | misc | Reserved name; currently a placeholder tool (do not rely on it). |
+
+### Built-in behavior notes (practical gotchas)
+
+- **`read_file` refuses binary files** (based on content heuristics) to avoid polluting context with binary data.
+- **Truncation**: `read_file` reads up to ~380,928 bytes and appends:
+  `---` and `[File truncated]` when it stops early.
+- **`append_to_file` always appends** and always inserts a newline before the provided content (it is not deduplicating).
+- **`find_and_replace` with `all=false` and multiple matches** returns an error message string (it does not silently pick one).
 
 <details>
-<summary><strong>Deep-dive: 7 helpers that turn ChatMD into a Swiss-Army knife</strong></summary>
+<summary><strong>Deep-dive: the highest leverage built-ins</strong></summary>
 
-1. **`apply_patch`** – The bread-and-butter of autonomous coding sessions.  The assistant can literally rewrite the repository while you watch.  The command understands *move*, *add*, *delete* and multi-hunk updates in one atomic transaction.
-2. **`webpage_to_markdown`** – Turns *any* public web page (incl. GitHub *blob* URLs) into clean Markdown ready for embedding or in-prompt reading.  JS-heavy sites fall back to a head-less Chromium dump.
-3. **`odoc_search`** – Semantic search over your **installed** opam packages.  Because results are fetched locally there is zero network latency – ideal for day-to-day coding.
-4. **`markdown_search`** – Complement to `odoc_search`.  Index your design docs and Wiki once; query them from ChatMD forever.
-5. **`query_vector_db`** – When you need proper hybrid retrieval (dense + BM25) over a code base.  Works hand-in-hand with `index_ocaml_code`.
-6. **`fork`** –  Reserved for future multi-agent flows.  The current implementation is a placeholder that returns a static string; treat it as experimental and do not rely on it for real workflows.
-7. **`mkdir`** *(experimental)* – Exposed today via the OCaml `Functions.mkdir` helper rather than as a ChatMD `<tool>`.  You can approximate it inside ChatMD via a shell wrapper or by combining `apply_patch` with pre-created directories.
+1. **`apply_patch`** – ochat’s workhorse for safe repo mutation. Prefer it over ad-hoc edits because it is atomic and reviewable.
+2. **`read_file`** *(declared as `get_contents`/`read_file`)* – fast local retrieval with binary refusal + truncation (keeps the context clean).
+3. **`webpage_to_markdown`** – high-signal ingestion:
+   - GitHub `blob` URLs are converted to `raw.githubusercontent.com` automatically;
+   - `#Lx-Ly` line ranges are respected and code is returned with line numbers.
+4. **`odoc_search`** – answers “how do I use X?” for installed libraries without a browser tab.
+5. **`markdown_search`** – a lightweight “docs RAG” loop: index once, query forever.
+6. **`query_vector_db`** – hybrid retrieval over indexed source code; ideal for “find the code that does X”.
+7. **`import_image`** – enables vision workflows (screenshots, diagrams, UI bugs) without leaving ChatMD.
 
 </details>
 
 ---
 
-## Importing remote MCP tools – one line, zero friction
+## Webpage ingestion: `webpage_to_markdown` (high signal)
+
+`webpage_to_markdown` is designed for “read it once, reason on it immediately” workflows:
+
+- Converts generic HTML pages into Markdown.
+- Special-cases **GitHub blob URLs**, including line anchors:
+
+```xml
+<tool name="webpage_to_markdown"/>
+```
+
+Examples:
+
+- `https://github.com/owner/repo/blob/main/lib/foo.ml#L10-L80` becomes a raw fetch from
+  `raw.githubusercontent.com` and returns just that slice, wrapped in a fenced code block with line numbers.
+
+Notes:
+
+- Results are cached for a short TTL (so repeated calls to the same URL are fast).
+- If HTML → Markdown conversion yields an empty result, ochat may attempt a `chrome-dump` fallback.
+
+---
+
+## Agent tools – turn prompts into callable sub-agents
+
+Agent tools let you mount a `*.chatmd` prompt as a callable tool. This is the easiest way
+to build structured “mini workflows” without writing code.
+
+```xml
+<!-- Local agent prompt (relative to the prompt directory) -->
+<tool name="triage" agent="prompts/triage.chatmd" local/>
+```
+
+Behavior (code-truth):
+
+- Input schema is fixed: `{ "input": string }`.
+- The agent runs in a fresh sub-conversation (no inherited message history), but with the same execution context (filesystem root, network, etc.).
+- The tool returns the agent’s final answer as the tool output.
+
+When to use:
+
+- Decompose complex tasks (“plan”, “triage”, “summarize logs”, “write a migration plan”).
+- Keep your main conversation focused while a specialized prompt handles a subtask.
+
+---
+
+## Importing remote MCP tools – one line, zero friction
 
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
 
-- `name="foo"` selects a single tool by name.
-- `includes="a,b"` or `include="a,b"` selects a comma-separated subset of tools; if neither `name` nor `include(s)` is present, all tools from `tools/list` are exposed.
-- `strict` (boolean flag) enables stricter behaviour when calling tools; see the OCaml `Mcp_tool` docs for details.
-- `client_id_env` / `client_secret_env` name environment variables whose values are injected as `client_id` / `client_secret` query parameters into the MCP server URI.
-
-> **Tip 💡** – All built-ins are **normal ChatMD tools** under the hood.  That means you can mount them remotely via MCP:
+- **Selection rules**:
+  - `name="foo"` selects a single tool *and takes precedence* over include(s).
+  - `include="a,b"` or `includes="a,b"` selects a comma-separated subset.
+  - If neither is present, **all tools** from `tools/list` are exposed.
+- `strict` is a boolean flag (present/absent). When present, the MCP tool is wrapped with strict parameter handling.
+- `client_id_env` / `client_secret_env` are names of environment variables. If set, their values are injected as `client_id` / `client_secret` query parameters into the MCP server URI.
+
+Operational details that matter:
+
+- Tool catalogs are cached for a short TTL (avoids repeated `tools/list` round-trips).
+- If the server emits `notifications/tools/list_changed`, ochat invalidates the cache and refreshes the next time the server is queried.
+
+All built-ins are normal tools under the hood, so you can also mount “local-like” helpers remotely:
 
 ```xml
 <!-- Consume read-only helpers from a sandboxed container on the CI runner -->
 <tool mcp_server="https://ci-tools.acme.dev" includes="read_dir,get_contents"/>
 ```
 
-or hide them from the model entirely in production by simply omitting the `<tool>` declaration.  No code changes required.
+Or hide tools from the model entirely in production by omitting the `<tool>` declaration. No code changes required.
+
+### Running ochat’s MCP server (share tools + prompt-as-tool)
+
+ochat includes an MCP server executable that can expose tools (and prompts) to other ochat instances.
+
+Out of the box it registers a small built-in set (including `apply_patch`, directory listing and file reading, prompt refinement, and `webpage_to_markdown`) and it can also register **every `*.chatmd`**
+in a prompts directory as both:
+
+- an MCP **prompt**, and
+- an agent-backed MCP **tool**
+
+This enables a very practical pattern: run the MCP server inside a sandbox/container/CI runner, then mount it from your interactive session via `<tool mcp_server="…"/>`.
 
 ---
 
-## Rolling your own OCaml tool – 20 lines round-trip
-
-```ocaml
-open Ochat_function
-
-module Hello = struct
-  type input = string
-
-  let def =
-    create_function
-      (module struct
-        type nonrec input = input
-        let name        = "say_hello"
-        let description = Some "Return a greeting for the supplied name"
-        let parameters  = Jsonaf.of_string
-          {|{"type":"object","properties":{"name":{"type":"string"}},"required":["name"]}|}
-        let input_of_string s =
-          Jsonaf.of_string s |> Jsonaf.member_exn "name" |> Jsonaf.string_exn
-      end)
-      (fun name -> "Hello " ^ name ^ "! 👋")
-end
-
-
-(* Gets the tools JSON and dispatch table *)
-let tools_json, dispatch =
-  Ochat_function.functions [ Hello.def ]
-
-(* If you want to add to the current drivers (Chat_tui and the chat-completion command)
- then add tool to of_declaration in lib/chat_response/tool.ml example *)
- 
-```
-
-Declare it once in ChatMD:
-
-```xml
-<tool name="say_hello"/>
-```
-
-That is **all** – the assistant can now greet users in 40+ languages without
-touching an HTTP stack.
+## Extending ochat with new tools (what’s actually supported)
+
+There are several extension routes, depending on what you want to ship:
+
+1. **Shell wrapper tool** (`<tool command="…"/>`): fastest way to expose a narrowly scoped command.
+2. **Agent tool** (`<tool agent="…"/>`): fastest way to expose a repeatable workflow encoded as ChatMD.
+3. **MCP server tool catalog** (`<tool mcp_server="…"/>`): best for sharing tools across environments and for sandboxing.
+4. **Embedding ochat as a library**: you can register arbitrary `Ochat_function.t` values in your host program.
+
+Note: A ChatMD declaration `<tool name="…"/>` without `command=`, `agent=` or `mcp_server=` is treated as a **built-in** and must be mapped in ochat’s built-in dispatcher. Unknown names are rejected.
 
 ---
 
 ## Shell-command wrappers – *the 30-second custom tool*
 
-> ⚠️ **Security note** – A `<tool command="…"/>` wrapper runs the specified
+> **Security note** – A `<tool command="…"/>` wrapper runs the specified
 > binary with the *full privileges of the current user*.  Only mount such tools
 > in **trusted environments** or inside a container / sandbox.  Never expose
 > unrestricted shell helpers to untrusted prompts – limit the command and
 > validate the arguments instead.
@@
 Behaviour in a nutshell
 
 1. The JSON schema is inferred automatically: an *array of strings* called
    `arguments`.
 2. At run-time Ochat executes
 
    ```sh
    <command> <arguments…>   # under the current working directory
    ```
 
-3. Standard output and stderr are captured and
-   appended to the `<tool_response>` block and sent back to the assistant.  Output may be truncated and execution is subject to a timeout to avoid wedging the session.
+3. Standard output and stderr are captured (combined) and returned to the assistant.
+
+Operational limits (code-truth):
+
+- Hard timeout: **60 seconds**
+- Output may be truncated (currently truncated to a short fixed maximum to keep context bounded)
+- Command parsing is intentionally simple:
+  - `%20` in `command="..."` is decoded to a space
+  - the command line is split on whitespace (do not rely on shell quoting)
 
 ### Why wrapper tools beat *generic shell*
@@
 #### Design guidelines
 
 1. **Prefer idempotent actions**.  Read or list before you write or delete.
 2. **Embed flags** that should never change directly in `command="…"`.
 3. Add a verb-based **prefix** (`git_`, `docker_`, `kubectl_`) so the
    language model can reason via pattern matching.
+
+---
+
+## Tool execution: parallel tool calls
+
+ochat can execute independent tool calls in parallel (useful when a model requests multiple reads/searches).
+
+In the TUI, this is configurable:
+
+- `--parallel-tool-calls` (default)
+- `--no-parallel-tool-calls`
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_cgD0SFlvHQQeeZ6IWkKlzMXM">
RAW|
Syntax error at line 283:
 Behaviour in a nutshell
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef1843f8881959cfc4d8d0f8f81fb">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef1864b788195a9646b80b2d4f32d">
RAW|
Purpose: Re-read the full current `docs-src/overview/tools.md` so I can apply an accurate patch with correct context.
|RAW
</assistant>


<tool_call tool_call_id="call_qnojuaWXmU9Xnu2OYY1nBhN5" function_name="read_file" id="fc_0a7776dd715389aa00696ef186a44881959cb06664310fcfce">
RAW|
{"file":"docs-src/overview/tools.md"}
|RAW
</tool_call>

<tool_response tool_call_id="call_qnojuaWXmU9Xnu2OYY1nBhN5">
RAW|
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

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef18acbd481958122a671bd350a05">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef19490408195bbc38cd49733b502">
RAW|
Purpose: Replace `docs-src/overview/tools.md` with a rewritten, code-accurate version covering built-ins (including aliases and `import_image`), agent tools, shell wrappers, MCP usage/server, and parallel tool execution.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_lssBaPRThsN5UbDw0xZxCiIe" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef195014c8195bc45a74ccbdd90a0">
RAW|
*** Begin Patch
*** Delete File: docs-src/overview/tools.md
*** Add File: docs-src/overview/tools.md
+# Tools – built-ins, agent tools, shell wrappers & MCP
+
+This page documents **tool calling** in ochat/ChatMD: how you declare tools in a prompt, what built-ins ship with ochat, and how to extend capabilities via **agent tools**, **shell wrappers**, and **MCP** (Model Context Protocol).
+
+Tools are **opt-in**: the model can only call what your prompt declares via `<tool .../>`.
+
+---
+
+## Quick start: declare tools in ChatMD
+
+```xml
+<!-- Built-ins -->
+<tool name="apply_patch"/>
+<tool name="read_dir"/>
+<tool name="read_file"/> <!-- alias: get_contents -->
+<tool name="webpage_to_markdown"/>
+
+<!-- Shell wrapper -->
+<tool name="git_status" command="git status" description="Show git status"/>
+
+<!-- Agent tool -->
+<tool name="triage" agent="prompts/triage.chatmd" local/>
+
+<!-- MCP tool catalog -->
+<tool mcp_server="stdio:npx -y brave-search-mcp"/>
+```
+
+---
+
+## Built-in tools
+
+### Recommended core set (start here)
+
+This set covers most real-world sessions (codebase navigation, retrieval, and safe edits):
+
+- **`apply_patch`** – atomic multi-file edits in a structured patch format.
+- **`read_file`** *(declare as `read_file` or `get_contents`)* – safe file reads with truncation + optional offset.
+- **`read_directory`** *(declare as `read_dir`)* – list directory entries without guessing paths.
+- **`webpage_to_markdown`** – ingest web pages and GitHub blob URLs as Markdown.
+- **`index_markdown_docs` + `markdown_search`** – semantic search over project Markdown docs.
+- **`index_ocaml_code` + `query_vector_db`** – hybrid retrieval over code indices (dense + BM25 overlay).
+- **`odoc_search`** – semantic search over locally indexed OCaml docs.
+- **`import_image`** – load a local image as a vision input (screenshots, diagrams).
+
+### Built-in catalog (code-correct)
+
+There are two names to be aware of:
+
+1. **ChatMD declaration name**: what you write in `<tool name="…"/>`.
+2. **Tool name the model sees**: what is advertised to the model and what it will call.
+
+Some tools have **declaration aliases** for compatibility.
+
+| ChatMD `<tool name="…"/>` | Model sees | Category | What it does |
+|---|---|---|---|
+| `apply_patch` | `apply_patch` | repo | Apply an atomic V4A patch (adds/updates/deletes/moves text files). |
+| `read_dir` | `read_directory` | fs | List directory entries (non-recursive) as newline-delimited text. |
+| `read_file` **or** `get_contents` | `read_file` | fs | Read a UTF-8 text file with truncation and optional `offset`. Refuses binary files. |
+| `append_to_file` | `append_to_file` | fs | Append text to a file (inserts a newline before the appended content). |
+| `find_and_replace` | `find_and_replace` | fs | Replace an exact substring in a file (single or all occurrences). |
+| `webpage_to_markdown` | `webpage_to_markdown` | web | Download a page and convert it to Markdown (includes a GitHub blob fast-path). |
+| `index_ocaml_code` | `index_ocaml_code` | index | Build a vector index from an OCaml source tree. |
+| `query_vector_db` | `query_vector_db` | search | Hybrid retrieval over code indices (dense + BM25 overlay). |
+| `index_markdown_docs` | `index_markdown_docs` | index | Index a folder of Markdown docs into a vector DB (default root: `.md_index`). |
+| `markdown_search` | `markdown_search` | search | Semantic search over Markdown indices created by `index_markdown_docs`. |
+| `odoc_search` | `odoc_search` | docs | Semantic search over locally indexed odoc docs. |
+| `meta_refine` | `meta_refine` | meta | Recursive meta-prompt refinement flow. |
+| `import_image` | `import_image` | vision | Load a local image file and return a vision input item (data URI). |
+| `fork` | `fork` | misc | Reserved name; currently a placeholder tool (do not rely on it). |
+
+#### Built-in behavior notes (practical gotchas)
+
+- **Naming/aliases**:
+  - declaring `<tool name="read_dir"/>` exposes a tool the model calls as `read_directory`.
+  - declaring `<tool name="get_contents"/>` exposes a tool the model calls as `read_file`.
+- **`read_file` truncation**: reads up to ~380,928 bytes and appends `---` + `[File truncated]` when it stops early.
+- **`read_file` binary refusal**: binary-like content is rejected to avoid polluting context.
+- **`append_to_file` always appends** (it does not deduplicate).
+- **`find_and_replace` with `all=false` and multiple matches** returns an error string advising to use `apply_patch`.
+
+#### Library-only helpers (not mountable as ChatMD built-ins by default)
+
+ochat’s OCaml library contains additional tool implementations (e.g. `mkdir`, `get_url_content`, `add_line_numbers`), but they are **not exposed via `<tool name="…"/>`** unless you:
+
+- add them to the built-in dispatcher, or
+- expose them via an MCP server, or
+- register them directly when embedding ochat as a library.
+
+---
+
+## High-signal ingestion: `webpage_to_markdown`
+
+`webpage_to_markdown` is designed for “read it once, reason on it immediately” workflows.
+
+Highlights:
+
+- Converts generic HTML pages into Markdown.
+- Special-cases **GitHub blob URLs**:
+  - automatically fetches from `raw.githubusercontent.com`
+  - respects line anchors like `#L10-L80`
+  - returns code slices wrapped in fenced blocks with line numbers
+- Caches results for a short TTL to make repeated calls to the same URL fast.
+
+Example:
+
+```xml
+<tool name="webpage_to_markdown"/>
+```
+
+---
+
+## Agent tools – turn prompts into callable sub-agents
+
+Agent tools mount a `*.chatmd` prompt as a callable tool. This is the fastest way to build repeatable “mini workflows” without writing code.
+
+```xml
+<!-- Local agent prompt (relative to the prompt directory) -->
+<tool name="triage" agent="prompts/triage.chatmd" local/>
+```
+
+Behavior:
+
+- Input schema is fixed: `{ "input": "..." }`.
+- The agent runs in a fresh sub-conversation (no inherited message history), but with the same execution context (filesystem root, network access, etc.).
+- The tool returns the agent’s final answer as tool output.
+
+When to use:
+
+- Decompose complex work (triage, summarization, planning, specialized refactors).
+- Keep your main conversation focused while a specialized prompt handles a subtask.
+
+---
+
+## Shell-command wrappers – the 30-second custom tool
+
+Shell wrappers expose a specific command as a function-callable tool:
+
+```xml
+<tool name="git_ls_files"
+      command="git ls-files --exclude=docs/"
+      description="Show files tracked by git except docs/"/>
+```
+
+Security note:
+
+- A `<tool command="…"/>` wrapper runs the specified binary with the full privileges of the current user.
+- Only mount shell wrappers in trusted environments, or inside a container/sandbox.
+
+Code-accurate behavior:
+
+1. The tool input schema is always:
+   ```json
+   { "arguments": ["..."] }
+   ```
+2. The declared command is executed as:
+   ```sh
+   <command> <arguments...>
+   ```
+3. stdout and stderr are captured (combined) and returned as text.
+
+Operational limits (important in practice):
+
+- Hard timeout: **60 seconds**
+- Output is truncated to a bounded size (currently ~10k characters) to avoid flooding context
+- Command parsing is intentionally simple:
+  - `%20` in `command="..."` is decoded to a space
+  - the command string is split on whitespace (do not rely on shell quoting/escaping)
+
+Design guidelines:
+
+- Prefer idempotent/read-only wrappers when possible.
+- Pin non-negotiable flags directly into `command="…"`.
+- Use clear, verb-based tool names (`git_pull`, `docker_ps`, `rg_search`) so the model can choose correctly.
+
+---
+
+## MCP tools – import remote tool catalogs
+
+MCP (Model Context Protocol) lets you mount tools from a remote server (stdio or HTTP). ochat turns each MCP tool into a normal function tool with the same JSON schema.
+
+```xml
+<!-- Mount a public MCP toolbox over stdio -->
+<tool mcp_server="stdio:npx -y brave-search-mcp"/>
+
+<!-- Or mount a subset from an HTTP endpoint -->
+<tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
+```
+
+### Selection rules (name vs include(s))
+
+- `name="foo"` selects a single tool and takes precedence over include(s).
+- `include="a,b"` or `includes="a,b"` selects a comma-separated subset.
+- If neither is present, **all tools** returned by `tools/list` are exposed.
+
+### Connection/auth knobs
+
+- `strict` is a boolean flag (present/absent) controlling strict parameter handling for the wrapped MCP tool.
+- `client_id_env` / `client_secret_env` name environment variables whose values (if set) are injected as `client_id` / `client_secret` query params in the MCP server URI.
+
+### Caching and refresh
+
+ochat caches MCP tool catalogs per server for a short TTL to avoid repeated `tools/list` calls. If the server emits `notifications/tools/list_changed`, ochat invalidates the cache and refreshes on the next access.
+
+---
+
+## Running ochat’s MCP server (share tools + “prompt-as-tool”)
+
+ochat includes an MCP server executable that exposes a small default set of tools and can also publish `*.chatmd` prompts as tools.
+
+Key behavior:
+
+- Registers a few built-in tools (including patching, directory listing and file reading, prompt refinement, and web ingestion).
+- Scans a prompts directory (default `./prompts`, or `$MCP_PROMPTS_DIR`) and registers every `*.chatmd` file as:
+  - an MCP **prompt**, and
+  - an agent-backed MCP **tool**
+
+This enables a practical pattern: run the MCP server inside a sandbox/container/CI runner, then mount it from your interactive session via `<tool mcp_server="…"/>`.
+
+---
+
+## Tool execution: parallel tool calls
+
+ochat can execute independent tool calls in parallel (useful when a model requests multiple reads/searches).
+
+In the TUI this is configurable:
+
+- `--parallel-tool-calls` (default)
+- `--no-parallel-tool-calls`
+
+---
+
+## Extending ochat with new tools (what’s actually supported)
+
+There are multiple extension routes depending on how you want to ship capabilities:
+
+1. **Shell wrapper tool** (`<tool command="…"/>`): fastest way to expose a narrowly scoped command.
+2. **Agent tool** (`<tool agent="…"/>`): fastest way to expose a workflow encoded in ChatMD.
+3. **MCP tool catalog** (`<tool mcp_server="…"/>`): best for sharing tools across environments and for sandboxing.
+4. **Embedding ochat as a library**: register arbitrary `Ochat_function.t` values directly in your host program.
+
+Important note: a plain ChatMD declaration `<tool name="…"/>` (without `command=`, `agent=`, or `mcp_server=`) is treated as a **built-in**. Unknown built-in names are rejected unless you add them to ochat’s built-in dispatcher or expose them via MCP.
+
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_lssBaPRThsN5UbDw0xZxCiIe">
RAW|
Add file already exists: docs-src/overview/tools.md
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef1b4f7bc8195b628a4d1f4c78f5e">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef1b88534819580ed2b92c787df0a">
RAW|
Purpose: Replace the entire contents of `docs-src/overview/tools.md` with the rewritten, code-accurate documentation (built-ins + aliases, agent tools, shell wrappers, MCP, and parallel tool calls).
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_1ewlA4pXV9kRVhow2P3oStLa" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef1b8d73081958ac967abb6d71ab5">
RAW|
*** Begin Patch
*** Update File: docs-src/overview/tools.md
@@
-# Tools – built-ins, custom helpers & MCP
-
-This page collects the tools-related material from the README and expands on
-how built-ins, shell wrappers, custom OCaml helpers and remote MCP tools fit
-together in ChatMD.
-
----
-
-## Built-in toolbox
-
-| Name | Category | Description |
-|------|----------|-------------|
-| `apply_patch`         | repo      | Apply an *Ochat diff* (V4A) to the working tree |
-| `read_dir`            | fs        | List entries (non-recursive) in a directory; returns plain-text lines |
-| `get_contents`        | fs        | Read a file (UTF-8); truncates very large files and supports an optional `offset` argument |
-| `get_url_content` *(experimental)* | web       | Download a raw resource and strip HTML to text *(OCaml API only; not exposed as a ChatMD `<tool>`)* |
-| `webpage_to_markdown` | web       | Download a page & convert it to Markdown |
-| `index_ocaml_code`    | index     | Build a vector index from a source tree |
-| `index_markdown_docs` | index     | Vector-index a folder of Markdown files |
-| `odoc_search`         | docs      | Semantic search over installed OCaml API docs |
-| `markdown_search` / `md-search` | search | Query Markdown indexes created by `index_markdown_docs` (ChatMD uses `markdown_search`; `md-search` is the CLI wrapper) |
-| `query_vector_db`     | search    | Hybrid dense + BM25 search over source indices |
-| `fork`                | misc      | Reserved name for future multi-agent flows; currently implemented as a placeholder tool |
-| `mkdir` *(experimental)*               | fs        | Create a directory (idempotent) *(OCaml API only; not exposed as a ChatMD `<tool>` yet)* |
-| `append_to_file`      | fs        | Append text to a file, creating it if absent |
-| `find_and_replace`    | fs        | Replace occurrences of a string in a file (single or all) |
-| `meta_refine`         | meta      | Recursive prompt refinement utility |
-
-<details>
-<summary><strong>Deep-dive: 7 helpers that turn ChatMD into a Swiss-Army knife</strong></summary>
-
-1. **`apply_patch`** – The bread-and-butter of autonomous coding sessions.  The assistant can literally rewrite the repository while you watch.  The command understands *move*, *add*, *delete* and multi-hunk updates in one atomic transaction.
-2. **`webpage_to_markdown`** – Turns *any* public web page (incl. GitHub *blob* URLs) into clean Markdown ready for embedding or in-prompt reading.  JS-heavy sites fall back to a head-less Chromium dump.
-3. **`odoc_search`** – Semantic search over your **installed** opam packages.  Because results are fetched locally there is zero network latency – ideal for day-to-day coding.
-4. **`markdown_search`** – Complement to `odoc_search`.  Index your design docs and Wiki once; query them from ChatMD forever.
-5. **`query_vector_db`** – When you need proper hybrid retrieval (dense + BM25) over a code base.  Works hand-in-hand with `index_ocaml_code`.
-6. **`fork`** –  Reserved for future multi-agent flows.  The current implementation is a placeholder that returns a static string; treat it as experimental and do not rely on it for real workflows.
-7. **`mkdir`** *(experimental)* – Exposed today via the OCaml `Functions.mkdir` helper rather than as a ChatMD `<tool>`.  You can approximate it inside ChatMD via a shell wrapper or by combining `apply_patch` with pre-created directories.
-
-</details>
-
----
-
-## Importing remote MCP tools – one line, zero friction
-
-```xml
-<!-- Mount the public Brave Search toolbox exposed by *npx brave-search-mcp* -->
-<tool mcp_server="stdio:npx -y brave-search-mcp"/>
-
-<!-- Or cherry-pick just two helpers from a self-hosted endpoint -->
-<tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
-```
-
-Ochat converts every entry returned by the server’s `tools/list` call into a
-local OCaml closure and forwards the **exact** JSON schema to OpenAI.  From the
-model’s perspective there is no difference between `weather` (remote) and
-`apply_patch` (local) – both are normal function calls.
-
-Additional attributes on `<tool mcp_server="…"/>` let you control which tools are exposed and how the client connects:
-
-- `name="foo"` selects a single tool by name.
-- `includes="a,b"` or `include="a,b"` selects a comma-separated subset of tools; if neither `name` nor `include(s)` is present, all tools from `tools/list` are exposed.
-- `strict` (boolean flag) enables stricter behaviour when calling tools; see the OCaml `Mcp_tool` docs for details.
-- `client_id_env` / `client_secret_env` name environment variables whose values are injected as `client_id` / `client_secret` query parameters into the MCP server URI.
-
-> **Tip 💡** – All built-ins are **normal ChatMD tools** under the hood.  That means you can mount them remotely via MCP:
-
-```xml
-<!-- Consume read-only helpers from a sandboxed container on the CI runner -->
-<tool mcp_server="https://ci-tools.acme.dev" includes="read_dir,get_contents"/>
-```
-
-or hide them from the model entirely in production by simply omitting the `<tool>` declaration.  No code changes required.
-
----
-
-## Rolling your own OCaml tool – 20 lines round-trip
-
-```ocaml
-open Ochat_function
-
-module Hello = struct
-  type input = string
-
-  let def =
-    create_function
-      (module struct
-        type nonrec input = input
-        let name        = "say_hello"
-        let description = Some "Return a greeting for the supplied name"
-        let parameters  = Jsonaf.of_string
-          {|{"type":"object","properties":{"name":{"type":"string"}},"required":["name"]}|}
-        let input_of_string s =
-          Jsonaf.of_string s |> Jsonaf.member_exn "name" |> Jsonaf.string_exn
-      end)
-      (fun name -> "Hello " ^ name ^ "! 👋")
-end
-
-
-(* Gets the tools JSON and dispatch table *)
-let tools_json, dispatch =
-  Ochat_function.functions [ Hello.def ]
-
-(* If you want to add to the current drivers (Chat_tui and the chat-completion command)
- then add tool to of_declaration in lib/chat_response/tool.ml example *)
- 
-```
-
-Declare it once in ChatMD:
-
-```xml
-<tool name="say_hello"/>
-```
-
-That is **all** – the assistant can now greet users in 40+ languages without
-touching an HTTP stack.
-
----
-
-## Shell-command wrappers – *the 30-second custom tool*
-
-> ⚠️ **Security note** – A `<tool command="…"/>` wrapper runs the specified
-> binary with the *full privileges of the current user*.  Only mount such tools
-> in **trusted environments** or inside a container / sandbox.  Never expose
-> unrestricted shell helpers to untrusted prompts – limit the command and
-> validate the arguments instead.
-
-Not every helper deserves a fully-blown OCaml module.  Often you just want to
-gate a **single shell command** behind a friendly JSON schema so the model can
-call it safely.  ChatMD does this out-of-the-box via the `command="…"`
-
-```xml
-<!-- Pure viewer: let model know do use for write access → safe in read-only environments. (note: this is just a hint to the model. It could still call this with write ops. You need to implement proper access controls in your tool) -->
-<tool name="sed"
-      command="sed"
-      description="read-only file viewer"/>
-
-<!-- Pre-pinned arguments – the model cannot escape the pattern.          -->
-<tool name="git_ls_files"
-      command="git ls-files --exclude=docs/"
-      description="show files tracked by git except docs/"/>
-
-<!-- Mutation allowed, therefore keep it explicit and auditable →        -->
-<tool name="git_pull"
-      command="git pull"
-      description="fetch from and integrate with a remote repository"/>
-```
-
-Behaviour in a nutshell
-
-1. The JSON schema is inferred automatically: an *array of strings* called
-   `arguments`.
-2. At run-time Ochat executes
-
-   ```sh
-   <command> <arguments…>   # under the current working directory
-   ```
-
-3. Standard output and stderr are captured and
-   appended to the `<tool_response>` block and sent back to the assistant.  Output may be truncated and execution is subject to a timeout to avoid wedging the session.
-
-### Why wrapper tools beat *generic shell*
-
-| Aspect | Generic `sh -c` | Targeted wrapper |
-|--------|-----------------|-------------------|
-| Search space @ inference | enormous | tiny – the model only sees *git_pull* / *sed* |
-| Security                 | needs manual sandboxing | limited to pre-approved binaries |
-| Reliability              | model must remember *all* flags | happy-path baked into `command` |
-
-In practice:
-
-* **Generalist agents** benefit from one broad hammer such as `bash`, but may
-  waste tokens debating which flag to use or which command to run.
-* **Specialist agents** (e.g. *CI fixer*, *release-bot*) shine when equipped
-  with *exactly* the verbs they need — nothing more, nothing less.
-
-#### Design guidelines
-
-1. **Prefer idempotent actions**.  Read or list before you write or delete.
-2. **Embed flags** that should never change directly in `command="…"`.
-3. Add a verb-based **prefix** (`git_`, `docker_`, `kubectl_`) so the
-   language model can reason via pattern matching.
+# Tools – built-ins, agent tools, shell wrappers & MCP
+
+This page documents **tool calling** in ochat/ChatMD: how you declare tools in a prompt, what built-ins ship with ochat, and how to extend capabilities via **agent tools**, **shell wrappers**, and **MCP** (Model Context Protocol).
+
+Tools are **opt-in**: the model can only call what your prompt declares via `<tool .../>`.
+
+---
+
+## Quick start: declare tools in ChatMD
+
+```xml
+<!-- Built-ins -->
+<tool name="apply_patch"/>
+<tool name="read_dir"/>
+<tool name="read_file"/> <!-- alias: get_contents -->
+<tool name="webpage_to_markdown"/>
+
+<!-- Shell wrapper -->
+<tool name="git_status" command="git status" description="Show git status"/>
+
+<!-- Agent tool -->
+<tool name="triage" agent="prompts/triage.chatmd" local/>
+
+<!-- MCP tool catalog -->
+<tool mcp_server="stdio:npx -y brave-search-mcp"/>
+```
+
+---
+
+## Built-in tools
+
+### Recommended core set (start here)
+
+This set covers most real-world sessions (codebase navigation, retrieval, and safe edits):
+
+- **`apply_patch`** – atomic multi-file edits in a structured patch format.
+- **`read_file`** *(declare as `read_file` or `get_contents`)* – safe file reads with truncation + optional offset.
+- **`read_directory`** *(declare as `read_dir`)* – list directory entries without guessing paths.
+- **`webpage_to_markdown`** – ingest web pages and GitHub blob URLs as Markdown.
+- **`index_markdown_docs` + `markdown_search`** – semantic search over project Markdown docs.
+- **`index_ocaml_code` + `query_vector_db`** – hybrid retrieval over code indices (dense + BM25 overlay).
+- **`odoc_search`** – semantic search over locally indexed OCaml docs.
+- **`import_image`** – load a local image as a vision input (screenshots, diagrams).
+
+### Built-in catalog (code-correct)
+
+There are two names to be aware of:
+
+1. **ChatMD declaration name**: what you write in `<tool name="…"/>`.
+2. **Tool name the model sees**: what is advertised to the model and what it will call.
+
+Some tools have **declaration aliases** for compatibility.
+
+| ChatMD `<tool name="…"/>` | Model sees | Category | What it does |
+|---|---|---|---|
+| `apply_patch` | `apply_patch` | repo | Apply an atomic V4A patch (adds/updates/deletes/moves text files). |
+| `read_dir` | `read_directory` | fs | List directory entries (non-recursive) as newline-delimited text. |
+| `read_file` **or** `get_contents` | `read_file` | fs | Read a UTF-8 text file with truncation and optional `offset`. Refuses binary files. |
+| `append_to_file` | `append_to_file` | fs | Append text to a file (inserts a newline before the appended content). |
+| `find_and_replace` | `find_and_replace` | fs | Replace an exact substring in a file (single or all occurrences). |
+| `webpage_to_markdown` | `webpage_to_markdown` | web | Download a page and convert it to Markdown (includes a GitHub blob fast-path). |
+| `index_ocaml_code` | `index_ocaml_code` | index | Build a vector index from an OCaml source tree. |
+| `query_vector_db` | `query_vector_db` | search | Hybrid retrieval over code indices (dense + BM25 overlay). |
+| `index_markdown_docs` | `index_markdown_docs` | index | Index a folder of Markdown docs into a vector DB (default root: `.md_index`). |
+| `markdown_search` | `markdown_search` | search | Semantic search over Markdown indices created by `index_markdown_docs`. |
+| `odoc_search` | `odoc_search` | docs | Semantic search over locally indexed odoc docs. |
+| `meta_refine` | `meta_refine` | meta | Recursive meta-prompt refinement flow. |
+| `import_image` | `import_image` | vision | Load a local image file and return a vision input item (data URI). |
+| `fork` | `fork` | misc | Reserved name; currently a placeholder tool (do not rely on it). |
+
+#### Built-in behavior notes (practical gotchas)
+
+- **Naming/aliases**:
+  - declaring `<tool name="read_dir"/>` exposes a tool the model calls as `read_directory`.
+  - declaring `<tool name="get_contents"/>` exposes a tool the model calls as `read_file`.
+- **`read_file` truncation**: reads up to ~380,928 bytes and appends `---` + `[File truncated]` when it stops early.
+- **`read_file` binary refusal**: binary-like content is rejected to avoid polluting context.
+- **`append_to_file` always appends** (it does not deduplicate).
+- **`find_and_replace` with `all=false` and multiple matches** returns an error string advising to use `apply_patch`.
+
+#### Library-only helpers (not mountable as ChatMD built-ins by default)
+
+ochat’s OCaml library contains additional tool implementations (e.g. `mkdir`, `get_url_content`, `add_line_numbers`), but they are **not exposed via `<tool name="…"/>`** unless you:
+
+- add them to the built-in dispatcher, or
+- expose them via an MCP server, or
+- register them directly when embedding ochat as a library.
+
+---
+
+## High-signal ingestion: `webpage_to_markdown`
+
+`webpage_to_markdown` is designed for “read it once, reason on it immediately” workflows.
+
+Highlights:
+
+- Converts generic HTML pages into Markdown.
+- Special-cases **GitHub blob URLs**:
+  - automatically fetches from `raw.githubusercontent.com`
+  - respects line anchors like `#L10-L80`
+  - returns code slices wrapped in fenced blocks with line numbers
+- Caches results for a short TTL to make repeated calls to the same URL fast.
+
+Example:
+
+```xml
+<tool name="webpage_to_markdown"/>
+```
+
+---
+
+## Agent tools – turn prompts into callable sub-agents
+
+Agent tools mount a `*.chatmd` prompt as a callable tool. This is the fastest way to build repeatable “mini workflows” without writing code.
+
+```xml
+<!-- Local agent prompt (relative to the prompt directory) -->
+<tool name="triage" agent="prompts/triage.chatmd" local/>
+```
+
+Behavior:
+
+- Input schema is fixed: `{ "input": "..." }`.
+- The agent runs in a fresh sub-conversation (no inherited message history), but with the same execution context (filesystem root, network access, etc.).
+- The tool returns the agent’s final answer as tool output.
+
+When to use:
+
+- Decompose complex work (triage, summarization, planning, specialized refactors).
+- Keep your main conversation focused while a specialized prompt handles a subtask.
+
+---
+
+## Shell-command wrappers – the 30-second custom tool
+
+Shell wrappers expose a specific command as a function-callable tool:
+
+```xml
+<tool name="git_ls_files"
+      command="git ls-files --exclude=docs/"
+      description="Show files tracked by git except docs/"/>
+```
+
+Security note:
+
+- A `<tool command="…"/>` wrapper runs the specified binary with the full privileges of the current user.
+- Only mount shell wrappers in trusted environments, or inside a container/sandbox.
+
+Code-accurate behavior:
+
+1. The tool input schema is always:
+   ```json
+   { "arguments": ["..."] }
+   ```
+2. The declared command is executed as:
+   ```sh
+   <command> <arguments...>
+   ```
+3. stdout and stderr are captured (combined) and returned as text.
+
+Operational limits (important in practice):
+
+- Hard timeout: **60 seconds**
+- Output is truncated to a bounded size (currently ~10k characters) to avoid flooding context
+- Command parsing is intentionally simple:
+  - `%20` in `command="..."` is decoded to a space
+  - the command string is split on whitespace (do not rely on shell quoting/escaping)
+
+Design guidelines:
+
+- Prefer idempotent/read-only wrappers when possible.
+- Pin non-negotiable flags directly into `command="…"`.
+- Use clear, verb-based tool names (`git_pull`, `docker_ps`, `rg_search`) so the model can choose correctly.
+
+---
+
+## MCP tools – import remote tool catalogs
+
+MCP (Model Context Protocol) lets you mount tools from a remote server (stdio or HTTP). ochat turns each MCP tool into a normal function tool with the same JSON schema.
+
+```xml
+<!-- Mount a public MCP toolbox over stdio -->
+<tool mcp_server="stdio:npx -y brave-search-mcp"/>
+
+<!-- Or mount a subset from an HTTP endpoint -->
+<tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
+```
+
+### Selection rules (name vs include(s))
+
+- `name="foo"` selects a single tool and takes precedence over include(s).
+- `include="a,b"` or `includes="a,b"` selects a comma-separated subset.
+- If neither is present, **all tools** returned by `tools/list` are exposed.
+
+### Connection/auth knobs
+
+- `strict` is a boolean flag (present/absent) controlling strict parameter handling for the wrapped MCP tool.
+- `client_id_env` / `client_secret_env` name environment variables whose values (if set) are injected as `client_id` / `client_secret` query params in the MCP server URI.
+
+### Caching and refresh
+
+ochat caches MCP tool catalogs per server for a short TTL to avoid repeated `tools/list` calls. If the server emits `notifications/tools/list_changed`, ochat invalidates the cache and refreshes on the next access.
+
+---
+
+## Running ochat’s MCP server (share tools + “prompt-as-tool”)
+
+ochat includes an MCP server executable that exposes a small default set of tools and can also publish `*.chatmd` prompts as tools.
+
+Key behavior:
+
+- Registers a few built-in tools (including patching, directory listing and file reading, prompt refinement, and web ingestion).
+- Scans a prompts directory (default `./prompts`, or `$MCP_PROMPTS_DIR`) and registers every `*.chatmd` file as:
+  - an MCP **prompt**, and
+  - an agent-backed MCP **tool**
+
+This enables a practical pattern: run the MCP server inside a sandbox/container/CI runner, then mount it from your interactive session via `<tool mcp_server="…"/>`.
+
+---
+
+## Tool execution: parallel tool calls
+
+ochat can execute independent tool calls in parallel (useful when a model requests multiple reads/searches).
+
+In the TUI this is configurable:
+
+- `--parallel-tool-calls` (default)
+- `--no-parallel-tool-calls`
+
+---
+
+## Extending ochat with new tools (what’s actually supported)
+
+There are multiple extension routes depending on how you want to ship capabilities:
+
+1. **Shell wrapper tool** (`<tool command="…"/>`): fastest way to expose a narrowly scoped command.
+2. **Agent tool** (`<tool agent="…"/>`): fastest way to expose a workflow encoded in ChatMD.
+3. **MCP tool catalog** (`<tool mcp_server="…"/>`): best for sharing tools across environments and for sandboxing.
+4. **Embedding ochat as a library**: register arbitrary `Ochat_function.t` values directly in your host program.
+
+Important note: a plain ChatMD declaration `<tool name="…"/>` (without `command=`, `agent=`, or `mcp_server=`) is treated as a **built-in**. Unknown built-in names are rejected unless you add them to ochat’s built-in dispatcher or expose them via MCP.
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_1ewlA4pXV9kRVhow2P3oStLa">
RAW|
✅ Patch applied successfully!

┏━[ docs-src/overview/tools.md ]----------------------------------------------------------------------
   1 | +# Tools – built-ins, agent tools, shell wrappers & MCP
   2 | +
   3 | +This page documents **tool calling** in ochat/ChatMD: how you declare tools in a prompt, what built-ins ship with ochat, and how to extend capabilities via **agent tools**, **shell wrappers**, and **MCP** (Model Context Protocol).
   4 | +
   5 | +Tools are **opt-in**: the model can only call what your prompt declares via `<tool .../>`.
   6 | +
   7 | +---
   8 | +
   9 | +## Quick start: declare tools in ChatMD
  10 | +
  11 | +```xml
  12 | +<!-- Built-ins -->
  13 | +<tool name="apply_patch"/>
  14 | +<tool name="read_dir"/>
  15 | +<tool name="read_file"/> <!-- alias: get_contents -->
  16 | +<tool name="webpage_to_markdown"/>
  17 | +
  18 | +<!-- Shell wrapper -->
  19 | +<tool name="git_status" command="git status" description="Show git status"/>
  20 | +
  21 | +<!-- Agent tool -->
  22 | +<tool name="triage" agent="prompts/triage.chatmd" local/>
  23 | +
  24 | +<!-- MCP tool catalog -->
  25 | +<tool mcp_server="stdio:npx -y brave-search-mcp"/>
  26 | +```
  27 | +
  28 | +---
  29 | +
  30 | +## Built-in tools
  31 | +
  32 | +### Recommended core set (start here)
  33 | +
  34 | +This set covers most real-world sessions (codebase navigation, retrieval, and safe edits):
  35 | +
  36 | +- **`apply_patch`** – atomic multi-file edits in a structured patch format.
  37 | +- **`read_file`** *(declare as `read_file` or `get_contents`)* – safe file reads with truncation + optional offset.
  38 | +- **`read_directory`** *(declare as `read_dir`)* – list directory entries without guessing paths.
  39 | +- **`webpage_to_markdown`** – ingest web pages and GitHub blob URLs as Markdown.
  40 | +- **`index_markdown_docs` + `markdown_search`** – semantic search over project Markdown docs.
  41 | +- **`index_ocaml_code` + `query_vector_db`** – hybrid retrieval over code indices (dense + BM25 overlay).
  42 | +- **`odoc_search`** – semantic search over locally indexed OCaml docs.
  43 | +- **`import_image`** – load a local image as a vision input (screenshots, diagrams).
  44 | +
  45 | +### Built-in catalog (code-correct)
  46 | +
  47 | +There are two names to be aware of:
  48 | +
  49 | +1. **ChatMD declaration name**: what you write in `<tool name="…"/>`.
  50 | +2. **Tool name the model sees**: what is advertised to the model and what it will call.
  51 | +
  52 | +Some tools have **declaration aliases** for compatibility.
  53 | +
  54 | +| ChatMD `<tool name="…"/>` | Model sees | Category | What it does |
  55 | +|---|---|---|---|
  56 | +| `apply_patch` | `apply_patch` | repo | Apply an atomic V4A patch (adds/updates/deletes/moves text files). |
  57 | +| `read_dir` | `read_directory` | fs | List directory entries (non-recursive) as newline-delimited text. |
  58 | +| `read_file` **or** `get_contents` | `read_file` | fs | Read a UTF-8 text file with truncation and optional `offset`. Refuses binary files. |
  59 | +| `append_to_file` | `append_to_file` | fs | Append text to a file (inserts a newline before the appended content). |
  60 | +| `find_and_replace` | `find_and_replace` | fs | Replace an exact substring in a file (single or all occurrences). |
  61 | +| `webpage_to_markdown` | `webpage_to_markdown` | web | Download a page and convert it to Markdown (includes a GitHub blob fast-path). |
  62 | +| `index_ocaml_code` | `index_ocaml_code` | index | Build a vector index from an OCaml source tree. |
  63 | +| `query_vector_db` | `query_vector_db` | search | Hybrid retrieval over code indices (dense + BM25 overlay). |
  64 | +| `index_markdown_docs` | `index_markdown_docs` | index | Index a folder of Markdown docs into a vector DB (default root: `.md_index`). |
  65 | +| `markdown_search` | `markdown_search` | search | Semantic search over Markdown indices created by `index_markdown_docs`. |
  66 | +| `odoc_search` | `odoc_search` | docs | Semantic search over locally indexed odoc docs. |
  67 | +| `meta_refine` | `meta_refine` | meta | Recursive meta-prompt refinement flow. |
  68 | +| `import_image` | `import_image` | vision | Load a local image file and return a vision input item (data URI). |
  69 | +| `fork` | `fork` | misc | Reserved name; currently a placeholder tool (do not rely on it). |
  70 | +
  71 | +#### Built-in behavior notes (practical gotchas)
  72 | +
  73 | +- **Naming/aliases**:
  74 | +  - declaring `<tool name="read_dir"/>` exposes a tool the model calls as `read_directory`.
  75 | +  - declaring `<tool name="get_contents"/>` exposes a tool the model calls as `read_file`.
  76 | +- **`read_file` truncation**: reads up to ~380,928 bytes and appends `---` + `[File truncated]` when it stops early.
  77 | +- **`read_file` binary refusal**: binary-like content is rejected to avoid polluting context.
  78 | +- **`append_to_file` always appends** (it does not deduplicate).
  79 | +- **`find_and_replace` with `all=false` and multiple matches** returns an error string advising to use `apply_patch`.
  80 | +
  81 | +#### Library-only helpers (not mountable as ChatMD built-ins by default)
  82 | +
  83 | +ochat’s OCaml library contains additional tool implementations (e.g. `mkdir`, `get_url_content`, `add_line_numbers`), but they are **not exposed via `<tool name="…"/>`** unless you:
  84 | +
  85 | +- add them to the built-in dispatcher, or
  86 | +- expose them via an MCP server, or
  87 | +- register them directly when embedding ochat as a library.
  88 | +
  89 | +---
  90 | +
  91 | +## High-signal ingestion: `webpage_to_markdown`
  92 | +
  93 | +`webpage_to_markdown` is designed for “read it once, reason on it immediately” workflows.
  94 | +
  95 | +Highlights:
  96 | +
  97 | +- Converts generic HTML pages into Markdown.
  98 | +- Special-cases **GitHub blob URLs**:
  99 | +  - automatically fetches from `raw.githubusercontent.com`
 100 | +  - respects line anchors like `#L10-L80`
 101 | +  - returns code slices wrapped in fenced blocks with line numbers
 102 | +- Caches results for a short TTL to make repeated calls to the same URL fast.
 103 | +
 104 | +Example:
 105 | +
 106 | +```xml
 107 | +<tool name="webpage_to_markdown"/>
 108 | +```
 109 | +
 110 | +---
 111 | +
 112 | +## Agent tools – turn prompts into callable sub-agents
 113 | +
 114 | +Agent tools mount a `*.chatmd` prompt as a callable tool. This is the fastest way to build repeatable “mini workflows” without writing code.
 115 | +
 116 | +```xml
 117 | +<!-- Local agent prompt (relative to the prompt directory) -->
 118 | +<tool name="triage" agent="prompts/triage.chatmd" local/>
 119 | +```
 120 | +
 121 | +Behavior:
 122 | +
 123 | +- Input schema is fixed: `{ "input": "..." }`.
 124 | +- The agent runs in a fresh sub-conversation (no inherited message history), but with the same execution context (filesystem root, network access, etc.).
 125 | +- The tool returns the agent’s final answer as tool output.
 126 | +
 127 | +When to use:
 128 | +
 129 | +- Decompose complex work (triage, summarization, planning, specialized refactors).
 130 | +- Keep your main conversation focused while a specialized prompt handles a subtask.
 131 | +
 132 | +---
 133 | +
 134 | +## Shell-command wrappers – the 30-second custom tool
 135 | +
 136 | +Shell wrappers expose a specific command as a function-callable tool:
 137 | +
 138 | +```xml
 139 | +<tool name="git_ls_files"
 140 | +      command="git ls-files --exclude=docs/"
 141 | +      description="Show files tracked by git except docs/"/>
 142 | +```
 143 | +
 144 | +Security note:
 145 | +
 146 | +- A `<tool command="…"/>` wrapper runs the specified binary with the full privileges of the current user.
 147 | +- Only mount shell wrappers in trusted environments, or inside a container/sandbox.
 148 | +
 149 | +Code-accurate behavior:
 150 | +
 151 | +1. The tool input schema is always:
 152 | +   ```json
 153 | +   { "arguments": ["..."] }
 154 | +   ```
 155 | +2. The declared command is executed as:
 156 | +   ```sh
 157 | +   <command> <arguments...>
 158 | +   ```
 159 | +3. stdout and stderr are captured (combined) and returned as text.
 160 | +
 161 | +Operational limits (important in practice):
 162 | +
 163 | +- Hard timeout: **60 seconds**
 164 | +- Output is truncated to a bounded size (currently ~10k characters) to avoid flooding context
 165 | +- Command parsing is intentionally simple:
 166 | +  - `%20` in `command="..."` is decoded to a space
 167 | +  - the command string is split on whitespace (do not rely on shell quoting/escaping)
 168 | +
 169 | +Design guidelines:
 170 | +
 171 | +- Prefer idempotent/read-only wrappers when possible.
 172 | +- Pin non-negotiable flags directly into `command="…"`.
 173 | +- Use clear, verb-based tool names (`git_pull`, `docker_ps`, `rg_search`) so the model can choose correctly.
 174 | +
 175 | +---
 176 | +
 177 | +## MCP tools – import remote tool catalogs
 178 | +
 179 | +MCP (Model Context Protocol) lets you mount tools from a remote server (stdio or HTTP). ochat turns each MCP tool into a normal function tool with the same JSON schema.
 180 | +
 181 | +```xml
 182 | +<!-- Mount a public MCP toolbox over stdio -->
 183 | +<tool mcp_server="stdio:npx -y brave-search-mcp"/>
 184 | +
 185 | +<!-- Or mount a subset from an HTTP endpoint -->
 186 | +<tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
 187 | +```
 188 | +
 189 | +### Selection rules (name vs include(s))
 190 | +
 191 | +- `name="foo"` selects a single tool and takes precedence over include(s).
 192 | +- `include="a,b"` or `includes="a,b"` selects a comma-separated subset.
 193 | +- If neither is present, **all tools** returned by `tools/list` are exposed.
 194 | +
 195 | +### Connection/auth knobs
 196 | +
 197 | +- `strict` is a boolean flag (present/absent) controlling strict parameter handling for the wrapped MCP tool.
 198 | +- `client_id_env` / `client_secret_env` name environment variables whose values (if set) are injected as `client_id` / `client_secret` query params in the MCP server URI.
 199 | +
 200 | +### Caching and refresh
 201 | +
 202 | +ochat caches MCP tool catalogs per server for a short TTL to avoid repeated `tools/list` calls. If the server emits `notifications/tools/list_changed`, ochat invalidates the cache and refreshes on the next access.
 203 | +
 204 | +---
 205 | +
 206 | +## Running ochat’s MCP server (share tools + “prompt-as-tool”)
 207 | +
 208 | +ochat includes an MCP server executable that exposes a small default set of tools and can also publish `*.chatmd` prompts as tools.
 209 | +
 210 | +Key behavior:
 211 | +
 212 | +- Registers a few built-in tools (including patching, directory listing and file reading, prompt refinement, and web ingestion).
 213 | +- Scans a prompts directory (default `./prompts`, or `$MCP_PROMPTS_DIR`) and registers every `*.chatmd` file as:
 214 | +  - an MCP **prompt**, and
 215 | +  - an agent-backed MCP **tool**
 216 | +
 217 | +This enables a practical pattern: run the MCP server inside a sandbox/container/CI runner, then mount it from your interactive session via `<tool mcp_server="…"/>`.
 218 | +
 219 | +---
 220 | +
 221 | +## Tool execution: parallel tool calls
 222 | +
 223 | +ochat can execute independent tool calls in parallel (useful when a model requests multiple reads/searches).
 224 | +
 225 | +In the TUI this is configurable:
 226 | +
 227 | +- `--parallel-tool-calls` (default)
 228 | +- `--no-parallel-tool-calls`
 229 | +
 230 | +---
 231 | +
 232 | +## Extending ochat with new tools (what’s actually supported)
 233 | +
 234 | +There are multiple extension routes depending on how you want to ship capabilities:
 235 | +
 236 | +1. **Shell wrapper tool** (`<tool command="…"/>`): fastest way to expose a narrowly scoped command.
 237 | +2. **Agent tool** (`<tool agent="…"/>`): fastest way to expose a workflow encoded in ChatMD.
 238 | +3. **MCP tool catalog** (`<tool mcp_server="…"/>`): best for sharing tools across environments and for sandboxing.
 239 | +4. **Embedding ochat as a library**: register arbitrary `Ochat_function.t` values directly in your host program.
 240 | +
 241 | +Important note: a plain ChatMD declaration `<tool name="…"/>` (without `command=`, `agent=`, or `mcp_server=`) is treated as a **built-in**. Unknown built-in names are rejected unless you add them to ochat’s built-in dispatcher or expose them via MCP.


-------------


<system-reminder>
This is a Note From the User:
update in chunks if you continue to have issues. ANd if you still run into isssues than just create a new file with the updated content
</system-reminder>

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef1d9d1788195ade16f1d90077a81">
<summary>
**Planning next steps**

I should consider the next steps, like asking for a review and checking if there are any missing details. I need to provide a final answer that follows the required format. This request seems complex, so I'll make sure to include the two sections as outlined in the developer's instructions. It’s important to comply with that structure to ensure clarity and completeness. Let’s move forward with these considerations!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef1dffe5881959be95db27ab14b37">
RAW|
### What I did:
- Rewrote the tools documentation to match the current code behavior and feature set, including:
  - Correct built-in tool names vs ChatMD declaration aliases
  - Added missing built-in: `import_image`
  - Added missing tool category docs: **agent tools** (`<tool agent="..."/>`)
  - Fixed/removed outdated claims (notably the “declare any OCaml tool once in ChatMD and it works” section)
  - Updated shell-wrapper behavior to reflect actual runtime constraints (timeout, truncation, simple command parsing)
  - Expanded MCP guidance (selection precedence, include/includes, strict flag, cache invalidation)
  - Added parallel tool-call execution note (TUI flags)
- Modified file:
  - `docs-src/overview/tools.md`

### Conclusion:
The rewritten, code-accurate version of the “Tools – built-ins, custom helpers & MCP” doc is now in:

- `docs-src/overview/tools.md`

Next: if you want, I can do a quick pass to further “sell” key features with 1–2 concrete mini-examples per top tool (e.g., an `apply_patch` example hunk, a `markdown_search` workflow, and a GitHub `webpage_to_markdown` line-range example), while keeping the doc readable.
|RAW
</assistant>

<user>
yes please do that. Add that info to the readme file instead so that users can see that right away without needing to deep dive into the tools doc
</user>

<reasoning id="rs_0a7776dd715389aa00696ef40204908195bc66f28a695cea1e">
<summary>
**Updating README details**

I need to update the README by adding a prominent section that highlights tool examples and features, including links to the tools documentation. To do this efficiently, I should inspect the current README and any linked documents. The request is clear: I’ll add the information to the top-level README.md. I'll use file reading and then apply a patch once I have everything I need. Let’s get started on that!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4064864819591ce98a2bb9b7ee1">
RAW|
Purpose: Read the current README so I can add a prominent, high-signal “Tools & capabilities” section with concrete examples that links to the detailed tools doc.
|RAW
</assistant>


<tool_call tool_call_id="call_sZL2AtYryxHoVnGG3KbAOdz9" function_name="read_file" id="fc_0a7776dd715389aa00696ef40658208195af30235e82dd7eca">
RAW|
{"file":"README.md"}
|RAW
</tool_call>

<tool_response tool_call_id="call_sZL2AtYryxHoVnGG3KbAOdz9">
RAW|

# Ochat – toolkit for building custom AI agents, scripted LLM pipelines & vector search

*Everything you need to prototype and run modern LLM workflows as plain files (implemented in OCaml).*

<div>
 <img src="assets/demo.gif" alt="chat_tui demo" height="700" width="900"/>
</div>


## What is Ochat?

Ochat is a toolkit for building **agent workflows and orchestrations as static files**.

If you like tools like Claude Code or Codex, Ochat is a more fundamental set of building blocks: instead of hard-coding the “agent application” into a single UI, you can implement something Claude Code‑like by shipping a *prompt pack* (a set of `.md` files) plus tools and running the agent using the terminal UI that the project provides.

In Ochat, an agent is a `.md` file written in a Markdown + XML dialect called **ChatMarkdown (ChatMD)**. A single file is the whole program:

- the model and generation parameters,
- which tools the assistant is allowed to call,
- the full conversation history (including tool calls and their results),
- imported artefacts (documents/images) when needed.

The runtime does **not** depend on file extensions: any filename can contain ChatMD. We use `.md` by convention so editors render Markdown nicely and you get syntax highlighting.

Because everything is captured in text files, workflows are:

- **reproducible** – the exact config and transcript are version‑controlled,
- **diff‑able** – reviews show exactly what changed and what the model did,
- **composable** – workflows can call other workflows (prompt‑as‑tool),
- **portable** – prompts are plain text; tools exchange JSON.

The same `.md` definition can be executed in multiple hosts:

- the **terminal UI** (`chat_tui`) for interactive work,
- **scripts and CI** via `ochat chat-completion`, and
- a **remote MCP server** via `mcp_server`, so IDEs or other applications can call agents over stdio or HTTP/SSE.

The chatmd language provides a rich set of features for prompt engineering in a modular way supporting all levels of complexity.

Ochat is implemented in OCaml, and provides tools for ocaml development, but the workflows themselves are **language‑agnostic** and ochat makes no assumptions about the types of applications the workflows target: you can use ochat to build workflows for any use case that benefits from LLMs + tools, and it puts no contraints on how simple or complex those workflows are.

**LLM provider support (today): OpenAI only.** Ochat currently integrates with OpenAI for chat execution and embeddings. The architecture is intended to support additional providers, but those integrations are not implemented yet. 

For details on the current OpenAI surface, see `docs-src/lib/openai/` (for example: [`responses`](docs-src/lib/openai/responses.doc.md)).

If you want the OCaml-specific entry points (embedding as a library, OCaml API doc search, `opam`/`dune` workflows), see the **OCaml integration** section below.

---

## What can I do with Ochat?

- **Author agent workflows as static files**  
  Write agents as `.md` files (ChatMarkdown). Each file is both the prompt *and* the execution log: model config, tool permissions, tool calls/results, and the full transcript.

- **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  

  You can mix:

  - **built-in tools** for common building blocks: file reads/writes and patch application (`read_dir`, `get_contents`, `apply_patch`), fetch webpage + HTML→Markdown conversion (`webpage_to_markdown`), local semantic search over docs (`markdown_search`, `odoc_search`), and vector DB retrieval (`query_vector_db`),
  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
  
    ```xml
    <tool mcp_server="stdio:npx -y brave-search-mcp" />.
    ```

  - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
  
  See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).

- **Build Claude Code/Codex-style agentic applications via custom “prompt packs”**  
  You can implement this as a set of specialized agents (planning agent, coding agent, test agent, doc agent…) and wire them together in an orchestration agent via agent-as-tool. The “application” is just a set of ChatMD files and you can run it via the terminal ui (`chat_tui`) or via the chat-completion CLI (`ochat chat-completion`).

- **Run the same workflows in different hosts**  
  Use `chat_tui` for interactive sessions, `ochat chat-completion` for scripts/CI/cron, and `mcp_server` to expose prompts as tools to IDEs and other hosts.

- **Ground agents in your own corpus**  
  Build indexes for docs/source trees and query them from within prompts so the agent can cite and follow project conventions rather than guessing. See [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md).

- **Continuously improve prompts**  
  Use the `mp-refine-run` binary to iteratively refine prompts and tool descriptions using evaluators, treating prompt design as a versioned, testable artifact.

---

## Example ChatMD prompts


### Example: interactive refactor agent

Turn a `.md` file into a refactoring bot that reads files and applies patches under your control.

1. Create `prompts/refactor.md`:

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="read_dir"/>
<tool name="get_contents"/>
<tool name="apply_patch"/>

<system>
You are a careful refactoring assistant. Work in small, reversible steps.
Before calling apply_patch, explain the change you want to make and wait for
confirmation from the user.
</system>

<user>
We are in a codebase. Look under ./lib, find a small improvement and
propose a patch.
</user>
```

2. Open it in the TUI:

```sh
dune exec chat_tui -- -file prompts/refactor.md
```

From there you can ask the assistant to rename a function, extract a helper, or
update documentation. It will use `read_dir` and `get_contents` to inspect the
code, then generate `apply_patch` diffs and apply them, with every tool call
and patch recorded in the `.md` file.

### Example: publish a prompt as an MCP tool

Export a `.md` file as a remote tool that other MCP‑compatible clients can call.

1. Create `prompts/hello.md`:

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="read_dir"/>
<tool name="get_contents"/>

<system>You are a documentation assistant.</system>

<user>
List the files under docs-src/ and summarize what each top-level folder is for.
</user>
```

2. Start the MCP server so it exports `hello.md` as a tool (by default it
   reads prompts from `./prompts`, or from `$MCP_PROMPTS_DIR` if set):

```sh
dune exec mcp_server -- --http 8080
```

Any MCP client can now discover the `hello` tool via `tools/list` and call it
with `tools/call` over JSON‑RPC. For example, a minimal HTTP request that lists
the available tools looks like:

```sh
curl -s http://localhost:8080/mcp \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```

The response includes an entry for `hello` whose JSON schema is inferred from
the ChatMD file; calling that tool runs your prompt and streams the result back
to the client.

For a more advanced, end-to-end research agent built from the same building
blocks, see the
[Discovery bot – research agent workflow](docs-src/guide/discovery-bot-workflow.md).

---

## Build from source (OCaml)

Install dependencies, build, and run tests:

```sh
opam switch create .
opam install . --deps-only

dune build
dune runtest

# Optional – build API docs when the dune-project declares a (documentation ...) stanza
dune build @doc
```

> The `@doc` alias is generated only when the project’s `dune-project` file
> contains a `(documentation ...)` stanza. If the command above fails, add the
> stanza or skip the step.

> On Apple Silicon (macOS arm64), Owl's OpenBLAS dependency can sometimes fail
> to build during `opam install`. If you see BLAS/OpenBLAS errors while
> installing dependencies or running `dune build`, see
> [Build & installation troubleshooting](docs-src/guide/build-troubleshooting.md#owl--openblas-on-apple-silicon-macos-arm64)
> for a proven workaround.

Run a quick interactive session with the terminal UI:

```sh
dune exec chat_tui -- -file prompts/interactive.md
```

Or run a non‑interactive chat completion over a ChatMD prompt as a smoke test:

```sh
ochat chat-completion \
  -prompt-file prompts/hello.md \
  -output-file .chatmd/smoke.md
```

For more on `ochat chat-completion` (flags, exit codes, ephemeral runs), see
[`docs-src/cli/chat-completion.md`](docs-src/cli/chat-completion.md).

---

## Core concepts in one page

- **ChatMarkdown (ChatMD)**  \
  A Markdown + XML dialect that stores model config, tool declarations and the full conversation (including tool calls, reasoning traces and imported artefacts) in a single `.md` file. Because prompts are plain text files you can review, diff and refactor them like code, and the runtime guarantees that what the model sees is exactly what is in the document. See the [language reference](docs-src/overview/chatmd-language.md).

- **Tools**  \
  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`), shell wrappers around commands like `rg` or `git`, other ChatMD agents, or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).

- **Agents & prompt‑as‑tool**  \
  Any `.md` file can be treated as an agent. Locally you can call it via `<agent>` blocks or mount it as a tool inside another prompt; remotely, `mcp_server` exposes it as a `tools/call::<name>` endpoint that IDEs or other hosts can invoke. Complex workflows become graphs of small, composable agents rather than monolithic prompts.

- **chat_tui**  \
  A Notty‑based terminal UI for editing and running `.md` files. It turns each prompt into a **terminal application**: live streaming of model output and tool responses, Vim‑style navigation, context compaction for long histories, and persistent sessions that you can resume or branch. You can think of `chat_tui` as the “host” and `.md` files as pluggable apps. See the [chat_tui guide](docs-src/guide/chat_tui.md).

- **CLI and helpers**  \
  Binaries like `ochat` and `md-index` / `md-search` provide script‑friendly entry points for running prompts, building indexes and querying them from the shell. (If you’re in OCaml, `odoc-index` / `odoc-search` can also index generated API docs.) See the [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) for non‑interactive runs; other commands are documented under `docs-src/cli/` and the generated odoc docs.

- **MCP server**  \
  `mcp_server` turns `.md` files and selected tools into MCP resources and tools that other applications can list and call over stdio or HTTP/SSE. See the [mcp_server binary doc](docs-src/bin/mcp_server.doc.md).

- **Search & indexing**  \
  Modules and binaries that build vector indexes over markdown docs and source code, powering tools like `markdown_search` and `query_vector_db`. (If you’re in OCaml, you can also index generated API docs.) See [Search, indexing & code-intelligence](docs-src/guide/search-and-indexing.md).

- **Meta-prompting**  \
  A library and CLI (`mp-refine-run`) for generating, scoring and refining prompts in a loop, so prompt engineering itself can be versioned and automated. See the [`Meta_prompting` overview](docs-src/lib/meta_prompting.doc.md).

> Each bullet links to a deeper reference under `docs-src/`.

---

## Documentation

Deep-dive docs live under `docs-src/`. Key entry points:

- [ChatMarkdown language reference](docs-src/overview/chatmd-language.md) – element tags, inline helpers, and prompt‑writing guidelines.
- [Built-in tools & custom tools](docs-src/overview/tools.md) – built‑in toolbox, shell wrappers, custom tools, and MCP tool import.
- [chat_tui guide & key bindings](docs-src/guide/chat_tui.md) – keybindings, context compaction, persistent sessions, and advanced behaviours.
- [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) – non‑interactive runs, flags, exit codes and ephemeral runs.
- [MCP server & protocol details](docs-src/bin/mcp_server.doc.md) – how `mcp_server` exposes prompts and tools over stdio or HTTP/SSE.
- [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md) – indexers, searchers and prompt patterns for hybrid retrieval.
- [Meta-prompting & Prompt Factory](docs-src/lib/meta_prompting.doc.md) – generators, evaluators, refinement loops and prompt packs.

OCaml integration and internals:

- [Embedding Ochat in OCaml](docs-src/lib/embedding.md) – reusing the libraries and caching patterns.
- [ChatML language & runtime](docs-src/lib/chatml/chatml_lang.doc.md) – experimental typed scripting language; see also the parser and resolver docs under `docs-src/lib/chatml/`.

---

## Binaries

| Binary | Purpose | Example |
|--------|---------|---------|
| `chat_tui` (`chat-tui`) | interactive TUI | `chat_tui -file notes.md` |
| `ochat`    | misc CLI (index, query, tokenise …) | `ochat query -vector-db-folder _index -query-text "tail-rec map"` |
| `mcp_server` | serve prompts & tools over JSON-RPC / SSE | `mcp_server --http 8080` |
| `mp-refine-run` | refine prompts via *recursive meta-prompting* | `mp-refine-run -task-file task.md -input-file draft.md` |
| `md-index` / `md-search` | Markdown → index / search | `md-index --root docs`; `md-search --query "streams"` |
| `odoc-index` / `odoc-search` | (OCaml) odoc HTML → index / search | `odoc-index --root _doc/_html` |

Run any binary with `-help` for details.

---

## Project layout

```
bin/         – chat_tui, mcp_server, ochat …
lib/         – re-usable libraries (chatmd, functions, vector_db …)
docs-src/    – Markdown docs rendered by odoc & included here
prompts/     – sample ChatMD prompts served by the MCP server
dune-project – dune metadata
```

---

## OCaml integration

Ochat is implemented in OCaml. Ochat intends to be language agnostic and the *workflows* can be used in any setup (tools exchange JSON; prompts are plain files), but being implemented in OCaml it has first class support for ocaml development. these entry points are OCaml-specific:

- **OCaml development environment guide**: see [`DEVELOPMENT.md`](DEVELOPMENT.md) for a dedicated walkthrough that sets up local OCaml documentation, search indexes, and related workflows that are useful for OCaml-focused agents.
- **OCaml API doc search**: `odoc-index` / `odoc-search` index and search generated odoc HTML.
- **Embedding as a library**: use the OCaml libraries directly (see [Embedding Ochat in OCaml](docs-src/lib/embedding.md)).
- **Ocaml indexing & code intelligence**: provides parsing and indexing of OCaml source files directly (no LSP dependency) to build precise indexes for code search and code-aware agents.

### Using builds/tests as an LLM feedback loop

When you run Ochat against an OCaml repository, the usual `dune build` / `dune runtest` loop becomes a high-signal feedback channel for LLM-generated edits: let an agent propose `apply_patch` diffs, run the build and tests, then feed compiler errors or failing expect tests back into the next turn.

### ChatML (experimental)

The repository ships an experimental language called *ChatML*: a small, expression‑oriented ML dialect with Hindley–Milner type inference (Algorithm W) extended with row polymorphism for records and variants.

The parser, type‑checker and runtime live under the `Chatml` modules and are documented under `docs-src/lib/chatml/` (see [`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md), [`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). Today it is exposed primarily via the experimental `dsl_script` binary and the `Chatml_*` library modules; it is not yet wired into ChatMD prompts or the main CLIs.

---

## Future directions

Ochat is intentionally **agent-first**: the roadmap focuses on making ChatMD, the runtime and `chat_tui` more expressive for building and operating fleets of custom agents, and on giving you better tools for observing and controlling how those agents behave.

Planned and experimental directions include:

- **Explicit control-flow & policy in ChatMD**  \
  The design note in [`control-flow-chatmd.md`](control-flow-chatmd.md)
  sketches a rules layer on top of ChatMD: you describe *events* (e.g.
  `pre_tool_call`, `turn_end`), *guards* over the transcript, and *actions*
  that materialise as normal ChatMD blocks (`<insert>`, `<deny>`, `<compact>`,
  `<agent>` …). The goal is to let you express things like “auto‑compact when
  the context grows too large” or “never call this tool without validating its
  inputs first” without hiding any logic from the transcript. This rules layer
  is not implemented yet; the document is a design sketch for future
  iterations.

- **Richer session tracking, branching and evaluation**  \
  Today `chat_tui` already persists sessions and lets you resume them.
  Future work focuses on making **branching conversations**, long‑term
  archives and agent evaluation runs first‑class so you can compare different
  agents on the same task, fork past sessions, and keep an auditable trail of
  how an agent evolved over time.

- **Session data (roadmap): per-session state + filesystem, backed by Irmin**  \
  Today sessions are persisted as on-disk snapshots (see the `Session_store` docs). A planned next step is to give agents a first-class way to store and retrieve session-specific data:

  - a simple key/value store API, scoped to the current conversation/session,
  - session-scoped file read/write (a “session filesystem”),
  - isolation by default (no accidental cross-session leakage),
  - tool-called agents inherit the parent session store (so helpers can share state without inventing ad-hoc protocols).

  The intent is for this to be backed by an Irmin database so session state can be versioned, merged, and synced in a principled way. This is not implemented yet; Irmin is currently only used by an auxiliary `sync` binary.

- **Additional LLM providers (roadmap)**  \
  Today the runtime integrates with OpenAI for chat execution and embeddings. A planned direction is to factor provider-specific details behind a stable interface so Ochat can target additional backends (for example: Anthropic/Claude, Google, local models) while keeping ChatMD files and tool contracts the same.

- **ChatML – a small, typed scripting language**  \
  The repository ships an experimental language called *ChatML*: a small,
  expression‑oriented ML dialect with Hindley–Milner type inference (Algorithm
  W) extended with row polymorphism for records and variants. The parser,
  type‑checker and runtime live under the `Chatml` modules and are documented
  under `docs-src/lib/chatml/` (see [`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md),
  [`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and
  [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). Today it is
  exposed primarily via the experimental `dsl_script` binary and the
  `Chatml_*` library modules; it is not yet wired into ChatMD prompts or the
  main CLIs. The long‑term plan is to use ChatML as a safe scripting language
  that agents can write and execute via a tool call, and can be embed inside ChatMD files for small,
  deterministic pieces of logic. Since it is strongly typed with full type inference, it provides a simple way to express logic without sacrificing safety or auditability. You can provide code execution capablities with high confidence, and provide a powerful tool for agents to express complex logic 

- **Custom Ocaml functions as tools via Dune plugins**  \
  A planned direction is to expose custom OCaml
  functions as tools via [Dune plugins](https://dune.readthedocs.io/en/stable/sites.html#plugins).

All of these directions share the same goal: make agents more reliable, 
composable, and expressive **without** sacrificing the “everything is a text file” property
that makes ChatMD workflows easy to debug and version‑control.

---

## Project status – expect rapid change

Ochat is a **research-grade** project that is evolving very rapidly.  APIs,
tool schemas, file formats and even high-level design choices may change as
we explore what works and what does not. If you intend to build something on
top of Ochat, please be prepared to:

* pin a specific commit or tag,
* re-run the tests after every `git pull`, and
* embrace breaking changes as part of the fun.

Despite the experimental label, **you can build real value today** – the
repository already enables powerful custom agent workflows.  I use it daily
with custom agents for everything from developing and documentation
generation, to writing emails and automating mundane tasks.

Please budget time for occasional refactors and breaking changes.
Bug reports, feature requests, and PRs are welcome and encouraged actually – just keep in mind the ground may still be
moving beneath your feet.

---

## License

All original source code is licensed under the terms stated in `LICENSE.txt`.
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef408cff081958fd3911e71619815">
<summary>
**Updating documentation examples**

I need to add high-signal examples to the README since the existing tools section has outdated names like read_dir and get_contents. I want to reflect the current aliases and point to the new tools documentation. A section titled "Tools & Capabilities (with examples)" would look great near the top, showing mini-examples for apply_patch, webpage_to_markdown, and more. I also need to correct the names in the README and ensure clarity with prompts. Time to patch up the README.md!
</summary>

<summary>
**Enhancing documentation structure**

I want to add a subsection under "What can I do..." or right after "Compose unique agents..." called "Tool Calling in 60 Seconds," complete with examples. I need to update the "Example: Interactive Refactor Agent" to use read_file instead of get_contents since the get_contents alias is still valid, but read_file can help clear up confusion. Also, I should adjust the bullet points in "Core Concepts" to include tools like apply_patch and read_dir. Let's break this update into manageable chunks and ensure everything fits well!
</summary>

<summary>
**Adding tools and capabilities section**

I've added a new section titled "Tools & Capabilities (Quick Tour)" to include concise tool call snippets. For example, I’ll show how to use apply_patch with a minimal snippet and webpage_to_markdown with a GitHub blob URL. I’ll also include a RAG example with markdown_search JSON schema, aiming for clarity, especially for first-time users. Since they requested this info in the README, I’ll add mini-examples, possibly using fenced code blocks for clarity. Lastly, I'll include an example of the import_image tool for vision tasks!
</summary>

<summary>
**Clarifying import_image tool**

I noticed that the import_image tool takes a path parameter, which is a bit confusing since the schema expects {path}, while the implementation uses image_path as a string. It seems that import_image expects a string input defined by the path. I'll demonstrate the correct tool call with `{"path":"assets/screenshot.png"}`. Also, I should address the MCP usage by showing how to mount using the command `dune exec mcp_server.exe`. I’ll make these updates to the README, ensuring that the tools bullet list reflects the correct naming for easier user understanding!
</summary>

<summary>
**Creating a comprehensive patch**

I’m crafting a patch that will involve several updates. First, I’ll replace built-in examples in the "Compose Unique Agents..." section with `read_file` (as the alias for get_contents), `read_dir`, and others like `apply_patch` and `import_image`, while noting the aliases. I’ll fix the MCP example by removing the trailing period, add a link to the new Tools document, and insert a new section titled "Tools & Capabilities (Quick Tour)." I'll also update the refactor agent example to reflect the correct tool naming and mention in the "Core Concepts" section. Time to patch everything up!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef420fbc081958d5a128064d16faf">
RAW|
Purpose: Update `README.md` to surface the most compelling tool features immediately with concrete examples (apply_patch, webpage_to_markdown GitHub slicing, agent-as-tool, indexing/search, import_image, MCP), and align tool names with the current docs.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_gr5rhBTAgV8pqdpapW5Xg62c" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef42122c081958713f6c3f6030f17">
RAW|
*** Begin Patch
*** Update File: README.md
@@
 ## What can I do with Ochat?
@@
 - **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  
 
   You can mix:
 
-  - **built-in tools** for common building blocks: file reads/writes and patch application (`read_dir`, `get_contents`, `apply_patch`), fetch webpage + HTML→Markdown conversion (`webpage_to_markdown`), local semantic search over docs (`markdown_search`, `odoc_search`), and vector DB retrieval (`query_vector_db`),
+  - **built-in tools** for common building blocks:
+    - repo-safe editing: `apply_patch`
+    - filesystem reads: `read_dir` (directory listing), `read_file` *(alias: `get_contents`)*
+    - web ingestion: `webpage_to_markdown` (HTML → Markdown + GitHub blob fast-path)
+    - local semantic search over docs: `index_markdown_docs` + `markdown_search`, and `odoc_search`
+    - hybrid retrieval over code: `index_ocaml_code` + `query_vector_db`
+    - vision inputs: `import_image` (bring local screenshots/diagrams into the model)
   - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
   - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
   
     ```xml
-    <tool mcp_server="stdio:npx -y brave-search-mcp" />.
+    <tool mcp_server="stdio:npx -y brave-search-mcp" />
     ```
 
   - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
   
   See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).
 
+- **Tools & capabilities (quick tour)**  
+  These are the features most users care about on day 1—each with a minimal example.
+
+  **1) Atomic repo edits with `apply_patch`**
+
+  Declare:
+  ```xml
+  <tool name="apply_patch"/>
+  ```
+  Typical tool call payload shape:
+  ```text
+  {
+    "patch": "*** Begin Patch\n*** Update File: path/to/file\n...\n*** End Patch"
+  }
+  ```
+  Why it’s great: you get **reviewable, multi-file, atomic** edits instead of ad-hoc mutations.
+
+  **2) Read files safely with `read_file` (alias: `get_contents`)**
+
+  Declare:
+  ```xml
+  <tool name="read_file"/>
+  ```
+  Notes: `read_file` refuses binary-ish content and truncates large files to keep context bounded.
+
+  **3) Ingest web pages (and GitHub code slices) with `webpage_to_markdown`**
+
+  Declare:
+  ```xml
+  <tool name="webpage_to_markdown"/>
+  ```
+  Works especially well on GitHub blob URLs with line ranges, e.g.:
+  - `https://github.com/owner/repo/blob/main/lib/foo.ml#L10-L80`
+
+  **4) Prompt-as-tool: mount a `.chatmd` workflow as a callable tool**
+
+  Declare:
+  ```xml
+  <tool name="triage" agent="prompts/triage.chatmd" local/>
+  ```
+  Why it’s great: build *small specialized agents* (triage, planner, doc-writer) and compose them.
+
+  **5) “Docs RAG” over your project Markdown**
+
+  Declare:
+  ```xml
+  <tool name="index_markdown_docs"/>
+  <tool name="markdown_search"/>
+  ```
+  Typical flow: index once (per repo), then query in natural language to pull high-signal snippets from your docs.
+
+  **6) Bring screenshots/diagrams into the model with `import_image`**
+
+  Declare:
+  ```xml
+  <tool name="import_image"/>
+  ```
+  Example payload:
+  ```json
+  { "path": "assets/screenshot.png" }
+  ```
+
+  **7) Import tools from elsewhere via MCP**
+
+  Declare:
+  ```xml
+  <tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
+  ```
+  Why it’s great: share tool catalogs across environments (local, container, CI) without changing prompts.
+
 - **Build Claude Code/Codex-style agentic applications via custom “prompt packs”**  
   You can implement this as a set of specialized agents (planning agent, coding agent, test agent, doc agent…) and wire them together in an orchestration agent via agent-as-tool. The “application” is just a set of ChatMD files and you can run it via the terminal ui (`chat_tui`) or via the chat-completion CLI (`ochat chat-completion`).
@@
 ### Example: interactive refactor agent
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
 <tool name="apply_patch"/>
@@
 From there you can ask the assistant to rename a function, extract a helper, or
 update documentation. It will use `read_dir` and `get_contents` to inspect the
 code, then generate `apply_patch` diffs and apply them, with every tool call
 and patch recorded in the `.md` file.
@@
 1. Create `prompts/hello.md`:
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
@@
 - **Tools**  \
-  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`), shell wrappers around commands like `rg` or `git`, other ChatMD agents, or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).
+  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`, `read_file` *(alias: `get_contents`)*, `webpage_to_markdown`, `import_image`), shell wrappers around commands like `rg` or `git`, other ChatMD agents (prompt-as-tool), or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_gr5rhBTAgV8pqdpapW5Xg62c">
RAW|
Syntax error at line 5:
 - **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4313e608195a7720023d04d3909">
<summary>
**Correcting patch syntax**

I'm noticing a syntax error in the tool output related to patch formatting. I realized that I might not have prefixed my file lines correctly, which led to confusion with the markdown bullet points. The tool seems to require that unchanged lines start with a space for context, while deletions need a "-" and additions a "+". To fix this, I need to craft the V4A patch properly and update the README accordingly, ensuring that any necessary context blocks are included.
</summary>

<summary>
**Crafting a patch for updates**

I'm planning to craft a patch in multiple hunks to update various sections of the README.md file. First, I'll address the built-in tools bullet section and add a new "Tools & capabilities (quick tour)" block. I also want to refactor the agent tool name and mention get_contents in the paragraph. It’s essential to identify the exact text context around these sections, using proper prefixes for each line. There seems to be a stray period after the mcp_server example, and I need to be cautious about the patch size as I include context lines.
</summary>

<summary>
**Planning prompt updates**

I need to update the refactor prompt by changing `<tool name="get_contents"/>` to `<tool name="read_file"/>`. I'll also replace any text lines that reference get_contents to read_file, which only happens once, and make similar updates to the hello prompt and the tools bullet in core concepts. After outlining these changes, I can craft a V4A patch to ensure everything is formatted correctly. This adjustment will enhance clarity across the documentation.
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef44573788195b1a5f2de500bc1cf">
RAW|
Purpose: Apply a V4A patch to README.md to add a prominent “Tools & capabilities (quick tour)” section with concrete examples, and update built-in tool references to match current behavior (read_file alias, etc.).
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_ZGdEZAInpGDzraH9skys14Et" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef445860481958e4bc2e9cff1be74">
RAW|
*** Begin Patch
*** Update File: README.md
@@
-- **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  
-
-  You can mix:
-
-  - **built-in tools** for common building blocks: file reads/writes and patch application (`read_dir`, `get_contents`, `apply_patch`), fetch webpage + HTML→Markdown conversion (`webpage_to_markdown`), local semantic search over docs (`markdown_search`, `odoc_search`), and vector DB retrieval (`query_vector_db`),
-  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
-  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
-  
-    ```xml
-    <tool mcp_server="stdio:npx -y brave-search-mcp" />.
-    ```
-
-  - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
-  
-  See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).
+- **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  
+
+  You can mix:
+
+  - **built-in tools** for common building blocks:
+    - repo-safe editing: `apply_patch`
+    - filesystem reads: `read_dir` (directory listing), `read_file` *(alias: `get_contents`)*
+    - web ingestion: `webpage_to_markdown` (HTML → Markdown + GitHub blob fast-path)
+    - local semantic search over docs: `index_markdown_docs` + `markdown_search`, and `odoc_search`
+    - hybrid retrieval over code: `index_ocaml_code` + `query_vector_db`
+    - vision inputs: `import_image` (bring local screenshots/diagrams into the model)
+  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
+  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
+  
+    ```xml
+    <tool mcp_server="stdio:npx -y brave-search-mcp" />
+    ```
+
+  - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
+  
+  See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).
+
+- **Tools & capabilities (quick tour)**  
+  These are the features most users care about on day 1—each with a minimal example.
+
+  **1) Atomic repo edits with `apply_patch`**
+
+  Declare:
+  ```xml
+  <tool name="apply_patch"/>
+  ```
+  Typical tool call payload shape:
+  ```text
+  {
+    "patch": "*** Begin Patch\n*** Update File: path/to/file\n...\n*** End Patch"
+  }
+  ```
+  Why it’s great: you get **reviewable, multi-file, atomic** edits instead of ad-hoc mutations.
+
+  **2) Read files safely with `read_file` (alias: `get_contents`)**
+
+  Declare:
+  ```xml
+  <tool name="read_file"/>
+  ```
+  Notes: `read_file` refuses binary-ish content and truncates large files to keep context bounded.
+
+  **3) Ingest web pages (and GitHub code slices) with `webpage_to_markdown`**
+
+  Declare:
+  ```xml
+  <tool name="webpage_to_markdown"/>
+  ```
+  Works especially well on GitHub blob URLs with line ranges, e.g.:
+  - `https://github.com/owner/repo/blob/main/lib/foo.ml#L10-L80`
+
+  **4) Prompt-as-tool: mount a `.chatmd` workflow as a callable tool**
+
+  Declare:
+  ```xml
+  <tool name="triage" agent="prompts/triage.chatmd" local/>
+  ```
+  Why it’s great: build *small specialized agents* (triage, planner, doc-writer) and compose them.
+
+  **5) “Docs RAG” over your project Markdown**
+
+  Declare:
+  ```xml
+  <tool name="index_markdown_docs"/>
+  <tool name="markdown_search"/>
+  ```
+  Typical flow: index once (per repo), then query in natural language to pull high-signal snippets from your docs.
+
+  **6) Bring screenshots/diagrams into the model with `import_image`**
+
+  Declare:
+  ```xml
+  <tool name="import_image"/>
+  ```
+  Example payload:
+  ```json
+  { "path": "assets/screenshot.png" }
+  ```
+
+  **7) Import tools from elsewhere via MCP**
+
+  Declare:
+  ```xml
+  <tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
+  ```
+  Why it’s great: share tool catalogs across environments (local, container, CI) without changing prompts.
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
 <tool name="apply_patch"/>
@@
 update documentation. It will use `read_dir` and `get_contents` to inspect the
 code, then generate `apply_patch` diffs and apply them, with every tool call
 and patch recorded in the `.md` file.
+update documentation. It will use `read_dir` and `read_file` to inspect the
+code, then generate `apply_patch` diffs and apply them, with every tool call
+and patch recorded in the `.md` file.
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
@@
-- **Tools**  \
-  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`), shell wrappers around commands like `rg` or `git`, other ChatMD agents, or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).
+- **Tools**  \
+  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`, `read_file` *(alias: `get_contents`)*, `webpage_to_markdown`, `import_image`), shell wrappers around commands like `rg` or `git`, other ChatMD agents (prompt-as-tool), or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_ZGdEZAInpGDzraH9skys14Et">
RAW|
Syntax error at line 110:
 <config model="gpt-4o" temperature="0"/>
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef451dfe081959d01a36b7e2befd1">
<summary>
**Refactoring tool declarations**

I want to ensure we provide context for the bullet point about building Claude. In the second part, I’ll need to refactor the agent tool declaration and change the `<tool name="get_contents"/>` line accordingly. That means reviewing how `read_dir` and `get_contents` are mentioned, switching to `read_file` instead. In the third part, the hello prompt also needs updating to reflect this change. I'll carefully craft patches, ensuring context lines are accurate. Let’s extract the right sections from the README to prepare the patch correctly.
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef45d354881958d4481b1de52fbf1">
RAW|
Purpose: Apply a correct V4A patch to `README.md` to (1) add a high-signal “Tools & capabilities (quick tour)” section with concrete examples, and (2) update tool references to reflect current tool naming (`read_file` alias, etc.).
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_5lN7FYc4PI7454OwGtW062GA" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef45d5214819594e03b9210475f53">
RAW|
*** Begin Patch
*** Update File: README.md
@@
-- **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  
-
-  You can mix:
-
-  - **built-in tools** for common building blocks: file reads/writes and patch application (`read_dir`, `get_contents`, `apply_patch`), fetch webpage + HTML→Markdown conversion (`webpage_to_markdown`), local semantic search over docs (`markdown_search`, `odoc_search`), and vector DB retrieval (`query_vector_db`),
-  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
-  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
-  
-    ```xml
-    <tool mcp_server="stdio:npx -y brave-search-mcp" />.
-    ```
-
-  - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
-  
-  See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).
+- **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  
+
+  You can mix:
+
+  - **built-in tools** for common building blocks:
+    - repo-safe editing: `apply_patch`
+    - filesystem reads: `read_dir` (directory listing), `read_file` *(alias: `get_contents`)*
+    - web ingestion: `webpage_to_markdown` (HTML → Markdown + GitHub blob fast-path)
+    - local semantic search over docs: `index_markdown_docs` + `markdown_search`, and `odoc_search`
+    - hybrid retrieval over code: `index_ocaml_code` + `query_vector_db`
+    - vision inputs: `import_image` (bring local screenshots/diagrams into the model)
+  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
+  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
+  
+    ```xml
+    <tool mcp_server="stdio:npx -y brave-search-mcp" />
+    ```
+
+  - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
+  
+  See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).
+
+- **Tools & capabilities (quick tour)**  
+  These are the features most users care about on day 1—each with a minimal example.
+
+  **1) Atomic repo edits with `apply_patch`**
+
+  Declare:
+  ```xml
+  <tool name="apply_patch"/>
+  ```
+  Typical tool call payload shape:
+  ```text
+  {
+    "patch": "*** Begin Patch\n*** Update File: path/to/file\n...\n*** End Patch"
+  }
+  ```
+  Why it’s great: you get **reviewable, multi-file, atomic** edits instead of ad-hoc mutations.
+
+  **2) Read files safely with `read_file` (alias: `get_contents`)**
+
+  Declare:
+  ```xml
+  <tool name="read_file"/>
+  ```
+  Notes: `read_file` refuses binary-ish content and truncates large files to keep context bounded.
+
+  **3) Ingest web pages (and GitHub code slices) with `webpage_to_markdown`**
+
+  Declare:
+  ```xml
+  <tool name="webpage_to_markdown"/>
+  ```
+  Works especially well on GitHub blob URLs with line ranges, e.g.:
+  - `https://github.com/owner/repo/blob/main/lib/foo.ml#L10-L80`
+
+  **4) Prompt-as-tool: mount a `.chatmd` workflow as a callable tool**
+
+  Declare:
+  ```xml
+  <tool name="triage" agent="prompts/triage.chatmd" local/>
+  ```
+  Why it’s great: build *small specialized agents* (triage, planner, doc-writer) and compose them.
+
+  **5) “Docs RAG” over your project Markdown**
+
+  Declare:
+  ```xml
+  <tool name="index_markdown_docs"/>
+  <tool name="markdown_search"/>
+  ```
+  Typical flow: index once (per repo), then query in natural language to pull high-signal snippets from your docs.
+
+  **6) Bring screenshots/diagrams into the model with `import_image`**
+
+  Declare:
+  ```xml
+  <tool name="import_image"/>
+  ```
+  Example payload:
+  ```json
+  { "path": "assets/screenshot.png" }
+  ```
+
+  **7) Import tools from elsewhere via MCP**
+
+  Declare:
+  ```xml
+  <tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
+  ```
+  Why it’s great: share tool catalogs across environments (local, container, CI) without changing prompts.
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
 <tool name="apply_patch"/>
@@
-update documentation. It will use `read_dir` and `get_contents` to inspect the
+update documentation. It will use `read_dir` and `read_file` to inspect the
 code, then generate `apply_patch` diffs and apply them, with every tool call
 and patch recorded in the `.md` file.
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
@@
-- **Tools**  \
-  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`), shell wrappers around commands like `rg` or `git`, other ChatMD agents, or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).
+- **Tools**  \
+  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`, `read_file` *(alias: `get_contents`)*, `webpage_to_markdown`, `import_image`), shell wrappers around commands like `rg` or `git`, other ChatMD agents (prompt-as-tool), or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_5lN7FYc4PI7454OwGtW062GA">
RAW|
Syntax error at line 110:
 <config model="gpt-4o" temperature="0"/>
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef468ec7c81958e665cf46e678f29">
<summary>
**Considering patch creation**

I’ll use the find_and_replace tool for crafting our patch, ensuring to include context lines with the exact leading spaces. With a Markdown bullet like "- **Compose unique agents ...** ", matching the original indentation is key. This line has two spaces at the end, so the patch must reflect that precisely. It's a bit tricky since trailing spaces might be trimmed in the response. To be safe, I’ll avoid relying on trailing spaces and focus on matching the exact lines.
</summary>

<summary>
**Exploring options for updates**

I’m thinking about the trailing spaces in Markdown bullets; they’re likely there and part of the file. It's tricky but manageable. I could use the find_and_replace tool for string replacements and append_to_file for insertions. The apply_patch tool seems best for inserting new sections, but it needs those accurate trailing spaces. Maybe it’s simpler to use find_and_replace on a unique multi-line block, as trailing spaces may not be critical there. But I should stick with apply_patch and ensure even minimal context matches those spaces.
</summary>

<summary>
**Crafting context lines**

I’m considering forming context using lines that don’t have trailing spaces, like blank lines. It's important that unchanged lines within the hunk match exactly, so it's okay to include those without trailing spaces. For the bullet line with trailing spaces, I think we can avoid editing it while still needing to delete or replace other lines. The built-in tool list can be updated without affecting the bullet title line, so I’ll just focus on updating descriptions and adding new lines after "See Tools..." without changing that header.
</summary>

<summary>
**Making updates to the patch**

First change involves replacing the line about built-in tools with a new multi-line list; it likely has no trailing spaces, so I can patch around it. We’ll delete the existing line and add several new ones, which sounds good. For the second change, I need to modify the MCP snippet by removing the trailing period, updating the line to just `... />`. 

Finally, I'll insert a new "Tools & capabilities" bullet before the "- **Build Claude Code..." line, after "See Tools ..." and a blank line. I’ll ensure to include those context lines when creating the patch.
</summary>

<summary>
**Planning the patch updates**

For inserting new content, I’ll replace the blank line and the next bullet line with the blank line, new block, and keep the bullet line. I’d prefer the insertion method used in the V4A patch, where I can add lines with a “+” while retaining spaces for existing lines. 

Next, I need to update the prompt tool name by changing `<tool name="get_contents"/>` to `<tool name="read_file"/>` and update the mention in the paragraph separately. 

I'll also include context around the built-in tool line for the first hunk while being careful to craft the patches precisely, ensuring to capture those necessary lines.
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef480ce4c8195bbb57cc57821ac84">
RAW|
Purpose: Apply a V4A-format patch to README.md in smaller, reliable hunks: update built-in tool list text, remove a stray punctuation in the MCP example, insert a new “Tools & capabilities (quick tour)” section, and update prompt examples to use `read_file`.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_YQs5F9jBNByu6zKpQmdI0lWQ" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef480ea648195b28094537c481d65">
RAW|
*** Begin Patch
*** Update File: README.md
@@
 
   You can mix:
 
-  - **built-in tools** for common building blocks: file reads/writes and patch application (`read_dir`, `get_contents`, `apply_patch`), fetch webpage + HTML→Markdown conversion (`webpage_to_markdown`), local semantic search over docs (`markdown_search`, `odoc_search`), and vector DB retrieval (`query_vector_db`),
+  - **built-in tools** for common building blocks:
+    - repo-safe editing: `apply_patch`
+    - filesystem reads: `read_dir` (directory listing), `read_file` *(alias: `get_contents`)*
+    - web ingestion: `webpage_to_markdown` (HTML → Markdown + GitHub blob fast-path)
+    - local semantic search over docs: `index_markdown_docs` + `markdown_search`, and `odoc_search`
+    - hybrid retrieval over code: `index_ocaml_code` + `query_vector_db`
+    - vision inputs: `import_image` (bring local screenshots/diagrams into the model)
   - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
   - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
   
     ```xml
-    <tool mcp_server="stdio:npx -y brave-search-mcp" />.
+    <tool mcp_server="stdio:npx -y brave-search-mcp" />
     ```
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_YQs5F9jBNByu6zKpQmdI0lWQ">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
  60 | +  - **built-in tools** for common building blocks:
  61 | +    - repo-safe editing: `apply_patch`
  62 | +    - filesystem reads: `read_dir` (directory listing), `read_file` *(alias: `get_contents`)*
  63 | +    - web ingestion: `webpage_to_markdown` (HTML → Markdown + GitHub blob fast-path)
  64 | +    - local semantic search over docs: `index_markdown_docs` + `markdown_search`, and `odoc_search`
  65 | +    - hybrid retrieval over code: `index_ocaml_code` + `query_vector_db`
  66 | +    - vision inputs: `import_image` (bring local screenshots/diagrams into the model)
  67 | +  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
  68 | +  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
  69 | +  
  70 | +    ```xml
  71 | +    <tool mcp_server="stdio:npx -y brave-search-mcp" />

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4838be081958a1172b2a6c14349">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef484d5c88195abe3c90f29eb94c9">
RAW|
Purpose: Insert a prominent “Tools & capabilities (quick tour)” section into README.md right after the Tools link so new users see concrete examples immediately.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_EV6GzK7QYuqKiumM2bl1KMbd" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef484f90081959b1d925207c61b70">
RAW|
*** Begin Patch
*** Update File: README.md
@@
   - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
   
   See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).
 
+- **Tools & capabilities (quick tour)**  
+  These are the features most users care about on day 1—each with a minimal example.
+
+  **1) Atomic repo edits with `apply_patch`**
+
+  Declare:
+  ```xml
+  <tool name="apply_patch"/>
+  ```
+  Tool calls pass a single patch string (V4A format):
+  ```text
+  {
+    "patch": "*** Begin Patch\n*** Update File: path/to/file\n...\n*** End Patch"
+  }
+  ```
+  Why it’s great: you get **reviewable, multi-file, atomic** edits instead of ad-hoc mutations.
+
+  **2) Read files safely with `read_file` (alias: `get_contents`)**
+
+  Declare:
+  ```xml
+  <tool name="read_file"/>
+  ```
+  Notes: `read_file` refuses binary-ish content and truncates large files to keep context bounded.
+
+  **3) Ingest web pages (and GitHub code slices) with `webpage_to_markdown`**
+
+  Declare:
+  ```xml
+  <tool name="webpage_to_markdown"/>
+  ```
+  Works especially well on GitHub blob URLs with line ranges, e.g.:
+  - `https://github.com/owner/repo/blob/main/lib/foo.ml#L10-L80`
+
+  **4) Prompt-as-tool: mount a `.chatmd` workflow as a callable tool**
+
+  Declare:
+  ```xml
+  <tool name="triage" agent="prompts/triage.chatmd" local/>
+  ```
+  Why it’s great: build *small specialized agents* (triage, planner, doc-writer) and compose them.
+
+  **5) “Docs RAG” over your project Markdown**
+
+  Declare:
+  ```xml
+  <tool name="index_markdown_docs"/>
+  <tool name="markdown_search"/>
+  ```
+  Typical flow: index once (per repo), then query in natural language to pull high-signal snippets from your docs.
+
+  **6) Bring screenshots/diagrams into the model with `import_image`**
+
+  Declare:
+  ```xml
+  <tool name="import_image"/>
+  ```
+  Example payload:
+  ```json
+  { "path": "assets/screenshot.png" }
+  ```
+
+  **7) Import tools from elsewhere via MCP**
+
+  Declare:
+  ```xml
+  <tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
+  ```
+  Why it’s great: share tool catalogs across environments (local, container, CI) without changing prompts.
+
 - **Build Claude Code/Codex-style agentic applications via custom “prompt packs”**  
   You can implement this as a set of specialized agents (planning agent, coding agent, test agent, doc agent…) and wire them together in an orchestration agent via agent-as-tool. The “application” is just a set of ChatMD files and you can run it via the terminal ui (`chat_tui`) or via the chat-completion CLI (`ochat chat-completion`).
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_EV6GzK7QYuqKiumM2bl1KMbd">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
  78 | +- **Tools & capabilities (quick tour)**  
  79 | +  These are the features most users care about on day 1—each with a minimal example.
  80 | +
  81 | +  **1) Atomic repo edits with `apply_patch`**
  82 | +
  83 | +  Declare:
  84 | +  ```xml
  85 | +  <tool name="apply_patch"/>
  86 | +  ```
  87 | +  Tool calls pass a single patch string (V4A format):
  88 | +  ```text
  89 | +  {
  90 | +    "patch": "*** Begin Patch\n*** Update File: path/to/file\n...\n*** End Patch"
  91 | +  }
  92 | +  ```
  93 | +  Why it’s great: you get **reviewable, multi-file, atomic** edits instead of ad-hoc mutations.
  94 | +
  95 | +  **2) Read files safely with `read_file` (alias: `get_contents`)**
  96 | +
  97 | +  Declare:
  98 | +  ```xml
  99 | +  <tool name="read_file"/>
 100 | +  ```
 101 | +  Notes: `read_file` refuses binary-ish content and truncates large files to keep context bounded.
 102 | +
 103 | +  **3) Ingest web pages (and GitHub code slices) with `webpage_to_markdown`**
 104 | +
 105 | +  Declare:
 106 | +  ```xml
 107 | +  <tool name="webpage_to_markdown"/>
 108 | +  ```
 109 | +  Works especially well on GitHub blob URLs with line ranges, e.g.:
 110 | +  - `https://github.com/owner/repo/blob/main/lib/foo.ml#L10-L80`
 111 | +
 112 | +  **4) Prompt-as-tool: mount a `.chatmd` workflow as a callable tool**
 113 | +
 114 | +  Declare:
 115 | +  ```xml
 116 | +  <tool name="triage" agent="prompts/triage.chatmd" local/>
 117 | +  ```
 118 | +  Why it’s great: build *small specialized agents* (triage, planner, doc-writer) and compose them.
 119 | +
 120 | +  **5) “Docs RAG” over your project Markdown**
 121 | +
 122 | +  Declare:
 123 | +  ```xml
 124 | +  <tool name="index_markdown_docs"/>
 125 | +  <tool name="markdown_search"/>
 126 | +  ```
 127 | +  Typical flow: index once (per repo), then query in natural language to pull high-signal snippets from your docs.
 128 | +
 129 | +  **6) Bring screenshots/diagrams into the model with `import_image`**
 130 | +
 131 | +  Declare:
 132 | +  ```xml
 133 | +  <tool name="import_image"/>
 134 | +  ```
 135 | +  Example payload:
 136 | +  ```json
 137 | +  { "path": "assets/screenshot.png" }
 138 | +  ```
 139 | +
 140 | +  **7) Import tools from elsewhere via MCP**
 141 | +
 142 | +  Declare:
 143 | +  ```xml
 144 | +  <tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
 145 | +  ```
 146 | +  Why it’s great: share tool catalogs across environments (local, container, CI) without changing prompts.
 147 | +

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef48d7a688195b48f7505b3918129">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef49058688195a804c8c420f6e11b">
RAW|
Purpose: Update the README’s example prompts and explanation text to use the canonical `read_file` tool name (instead of the legacy alias `get_contents`) for clarity.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_fywvoQk9JX3cQM2prX1obZv3" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef490dbb08195aa9b6de7926bf985">
RAW|
*** Begin Patch
*** Update File: README.md
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
 <tool name="apply_patch"/>
 
 <system>
 You are a careful refactoring assistant. Work in small, reversible steps.
@@
 From there you can ask the assistant to rename a function, extract a helper, or
 update documentation. It will use `read_dir` and `get_contents` to inspect the
 code, then generate `apply_patch` diffs and apply them, with every tool call
 and patch recorded in the `.md` file.
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_fywvoQk9JX3cQM2prX1obZv3">
RAW|
Syntax error at line 13:
 From there you can ask the assistant to rename a function, extract a helper, or
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef493fae48195a20d2ec72d4f92f6">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef495a3248195b9b671afe8b6aaf7">
RAW|
Purpose: Apply a V4A-formatted patch to README.md to replace `get_contents` with `read_file` in the refactor example prompt and its explanatory text.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_Qfa5chy9A8oZEuklEW3QuUBe" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef495de1c8195812b6990591733c9">
RAW|
*** Begin Patch
*** Update File: README.md
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
 <tool name="apply_patch"/>
 
 <system>
 You are a careful refactoring assistant. Work in small, reversible steps.
@@
 From there you can ask the assistant to rename a function, extract a helper, or
-update documentation. It will use `read_dir` and `get_contents` to inspect the
+update documentation. It will use `read_dir` and `read_file` to inspect the
 code, then generate `apply_patch` diffs and apply them, with every tool call
 and patch recorded in the `.md` file.
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_Qfa5chy9A8oZEuklEW3QuUBe">
RAW|
Syntax error at line 13:
 From there you can ask the assistant to rename a function, extract a helper, or
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef49b47cc8195a3156730bc10fa4f">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4a22cac819582a2470a480f952f">
RAW|
Purpose: Apply a properly formatted V4A patch (with leading context markers) to change the refactor example from `get_contents` to `read_file`.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_Gap7xlxkhzKlICECjbk4GrLe" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef4a2bbb481958bf16524adc2de4b">
RAW|
*** Begin Patch
*** Update File: README.md
@@
-<config model="gpt-4o" temperature="0"/>
-
-<tool name="read_dir"/>
-<tool name="get_contents"/>
-<tool name="apply_patch"/>
+ <config model="gpt-4o" temperature="0"/>
+ 
+ <tool name="read_dir"/>
+-<tool name="get_contents"/>
++<tool name="read_file"/>
+ <tool name="apply_patch"/>
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_Gap7xlxkhzKlICECjbk4GrLe">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
 172 | + <config model="gpt-4o" temperature="0"/>
 173 | + 
 174 | + <tool name="read_dir"/>
 175 | +-<tool name="get_contents"/>
 176 | ++<tool name="read_file"/>
 177 | + <tool name="apply_patch"/>

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4a68cb88195800f8312759e162f">
<summary>
**Considering file reading**

I think it's important to begin by showing the file's initial content, maybe with a section that includes an example. I’m considering using an offset of around 7000 to start reading, which sounds reasonable. I’ll plan to call the read_file function to get the data I need. This should help me approach the task systematically and ensure I’m getting the right content to work with!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4b5a5408195b5b7c5758f9721cf">
RAW|
Purpose: Inspect the modified portion of README.md around the refactor example to fix the accidental insertion of patch markers into the file content.
|RAW
</assistant>


<tool_call tool_call_id="call_GfITUSpRlPnmCvJPEjIXaPXg" function_name="read_file" id="fc_0a7776dd715389aa00696ef4b5b24481959035fd6ea6a71ad0">
RAW|
{"file":"README.md","offset":6000}
|RAW
</tool_call>

<tool_response tool_call_id="call_GfITUSpRlPnmCvJPEjIXaPXg">
RAW|
  ```xml
  <tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
  ```
  Why it’s great: share tool catalogs across environments (local, container, CI) without changing prompts.

- **Build Claude Code/Codex-style agentic applications via custom “prompt packs”**  
  You can implement this as a set of specialized agents (planning agent, coding agent, test agent, doc agent…) and wire them together in an orchestration agent via agent-as-tool. The “application” is just a set of ChatMD files and you can run it via the terminal ui (`chat_tui`) or via the chat-completion CLI (`ochat chat-completion`).

- **Run the same workflows in different hosts**  
  Use `chat_tui` for interactive sessions, `ochat chat-completion` for scripts/CI/cron, and `mcp_server` to expose prompts as tools to IDEs and other hosts.

- **Ground agents in your own corpus**  
  Build indexes for docs/source trees and query them from within prompts so the agent can cite and follow project conventions rather than guessing. See [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md).

- **Continuously improve prompts**  
  Use the `mp-refine-run` binary to iteratively refine prompts and tool descriptions using evaluators, treating prompt design as a versioned, testable artifact.

---

## Example ChatMD prompts


### Example: interactive refactor agent

Turn a `.md` file into a refactoring bot that reads files and applies patches under your control.

1. Create `prompts/refactor.md`:

```xml
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
 <tool name="apply_patch"/>

<system>
You are a careful refactoring assistant. Work in small, reversible steps.
Before calling apply_patch, explain the change you want to make and wait for
confirmation from the user.
</system>

<user>
We are in a codebase. Look under ./lib, find a small improvement and
propose a patch.
</user>
```

2. Open it in the TUI:

```sh
dune exec chat_tui -- -file prompts/refactor.md
```

From there you can ask the assistant to rename a function, extract a helper, or
update documentation. It will use `read_dir` and `get_contents` to inspect the
code, then generate `apply_patch` diffs and apply them, with every tool call
and patch recorded in the `.md` file.

### Example: publish a prompt as an MCP tool

Export a `.md` file as a remote tool that other MCP‑compatible clients can call.

1. Create `prompts/hello.md`:

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="read_dir"/>
<tool name="get_contents"/>

<system>You are a documentation assistant.</system>

<user>
List the files under docs-src/ and summarize what each top-level folder is for.
</user>
```

2. Start the MCP server so it exports `hello.md` as a tool (by default it
   reads prompts from `./prompts`, or from `$MCP_PROMPTS_DIR` if set):

```sh
dune exec mcp_server -- --http 8080
```

Any MCP client can now discover the `hello` tool via `tools/list` and call it
with `tools/call` over JSON‑RPC. For example, a minimal HTTP request that lists
the available tools looks like:

```sh
curl -s http://localhost:8080/mcp \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```

The response includes an entry for `hello` whose JSON schema is inferred from
the ChatMD file; calling that tool runs your prompt and streams the result back
to the client.

For a more advanced, end-to-end research agent built from the same building
blocks, see the
[Discovery bot – research agent workflow](docs-src/guide/discovery-bot-workflow.md).

---

## Build from source (OCaml)

Install dependencies, build, and run tests:

```sh
opam switch create .
opam install . --deps-only

dune build
dune runtest

# Optional – build API docs when the dune-project declares a (documentation ...) stanza
dune build @doc
```

> The `@doc` alias is generated only when the project’s `dune-project` file
> contains a `(documentation ...)` stanza. If the command above fails, add the
> stanza or skip the step.

> On Apple Silicon (macOS arm64), Owl's OpenBLAS dependency can sometimes fail
> to build during `opam install`. If you see BLAS/OpenBLAS errors while
> installing dependencies or running `dune build`, see
> [Build & installation troubleshooting](docs-src/guide/build-troubleshooting.md#owl--openblas-on-apple-silicon-macos-arm64)
> for a proven workaround.

Run a quick interactive session with the terminal UI:

```sh
dune exec chat_tui -- -file prompts/interactive.md
```

Or run a non‑interactive chat completion over a ChatMD prompt as a smoke test:

```sh
ochat chat-completion \
  -prompt-file prompts/hello.md \
  -output-file .chatmd/smoke.md
```

For more on `ochat chat-completion` (flags, exit codes, ephemeral runs), see
[`docs-src/cli/chat-completion.md`](docs-src/cli/chat-completion.md).

---

## Core concepts in one page

- **ChatMarkdown (ChatMD)**  \
  A Markdown + XML dialect that stores model config, tool declarations and the full conversation (including tool calls, reasoning traces and imported artefacts) in a single `.md` file. Because prompts are plain text files you can review, diff and refactor them like code, and the runtime guarantees that what the model sees is exactly what is in the document. See the [language reference](docs-src/overview/chatmd-language.md).

- **Tools**  \
  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`), shell wrappers around commands like `rg` or `git`, other ChatMD agents, or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).

- **Agents & prompt‑as‑tool**  \
  Any `.md` file can be treated as an agent. Locally you can call it via `<agent>` blocks or mount it as a tool inside another prompt; remotely, `mcp_server` exposes it as a `tools/call::<name>` endpoint that IDEs or other hosts can invoke. Complex workflows become graphs of small, composable agents rather than monolithic prompts.

- **chat_tui**  \
  A Notty‑based terminal UI for editing and running `.md` files. It turns each prompt into a **terminal application**: live streaming of model output and tool responses, Vim‑style navigation, context compaction for long histories, and persistent sessions that you can resume or branch. You can think of `chat_tui` as the “host” and `.md` files as pluggable apps. See the [chat_tui guide](docs-src/guide/chat_tui.md).

- **CLI and helpers**  \
  Binaries like `ochat` and `md-index` / `md-search` provide script‑friendly entry points for running prompts, building indexes and querying them from the shell. (If you’re in OCaml, `odoc-index` / `odoc-search` can also index generated API docs.) See the [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) for non‑interactive runs; other commands are documented under `docs-src/cli/` and the generated odoc docs.

- **MCP server**  \
  `mcp_server` turns `.md` files and selected tools into MCP resources and tools that other applications can list and call over stdio or HTTP/SSE. See the [mcp_server binary doc](docs-src/bin/mcp_server.doc.md).

- **Search & indexing**  \
  Modules and binaries that build vector indexes over markdown docs and source code, powering tools like `markdown_search` and `query_vector_db`. (If you’re in OCaml, you can also index generated API docs.) See [Search, indexing & code-intelligence](docs-src/guide/search-and-indexing.md).

- **Meta-prompting**  \
  A library and CLI (`mp-refine-run`) for generating, scoring and refining prompts in a loop, so prompt engineering itself can be versioned and automated. See the [`Meta_prompting` overview](docs-src/lib/meta_prompting.doc.md).

> Each bullet links to a deeper reference under `docs-src/`.

---

## Documentation

Deep-dive docs live under `docs-src/`. Key entry points:

- [ChatMarkdown language reference](docs-src/overview/chatmd-language.md) – element tags, inline helpers, and prompt‑writing guidelines.
- [Built-in tools & custom tools](docs-src/overview/tools.md) – built‑in toolbox, shell wrappers, custom tools, and MCP tool import.
- [chat_tui guide & key bindings](docs-src/guide/chat_tui.md) – keybindings, context compaction, persistent sessions, and advanced behaviours.
- [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) – non‑interactive runs, flags, exit codes and ephemeral runs.
- [MCP server & protocol details](docs-src/bin/mcp_server.doc.md) – how `mcp_server` exposes prompts and tools over stdio or HTTP/SSE.
- [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md) – indexers, searchers and prompt patterns for hybrid retrieval.
- [Meta-prompting & Prompt Factory](docs-src/lib/meta_prompting.doc.md) – generators, evaluators, refinement loops and prompt packs.

OCaml integration and internals:

- [Embedding Ochat in OCaml](docs-src/lib/embedding.md) – reusing the libraries and caching patterns.
- [ChatML language & runtime](docs-src/lib/chatml/chatml_lang.doc.md) – experimental typed scripting language; see also the parser and resolver docs under `docs-src/lib/chatml/`.

---

## Binaries

| Binary | Purpose | Example |
|--------|---------|---------|
| `chat_tui` (`chat-tui`) | interactive TUI | `chat_tui -file notes.md` |
| `ochat`    | misc CLI (index, query, tokenise …) | `ochat query -vector-db-folder _index -query-text "tail-rec map"` |
| `mcp_server` | serve prompts & tools over JSON-RPC / SSE | `mcp_server --http 8080` |
| `mp-refine-run` | refine prompts via *recursive meta-prompting* | `mp-refine-run -task-file task.md -input-file draft.md` |
| `md-index` / `md-search` | Markdown → index / search | `md-index --root docs`; `md-search --query "streams"` |
| `odoc-index` / `odoc-search` | (OCaml) odoc HTML → index / search | `odoc-index --root _doc/_html` |

Run any binary with `-help` for details.

---

## Project layout

```
bin/         – chat_tui, mcp_server, ochat …
lib/         – re-usable libraries (chatmd, functions, vector_db …)
docs-src/    – Markdown docs rendered by odoc & included here
prompts/     – sample ChatMD prompts served by the MCP server
dune-project – dune metadata
```

---

## OCaml integration

Ochat is implemented in OCaml. Ochat intends to be language agnostic and the *workflows* can be used in any setup (tools exchange JSON; prompts are plain files), but being implemented in OCaml it has first class support for ocaml development. these entry points are OCaml-specific:

- **OCaml development environment guide**: see [`DEVELOPMENT.md`](DEVELOPMENT.md) for a dedicated walkthrough that sets up local OCaml documentation, search indexes, and related workflows that are useful for OCaml-focused agents.
- **OCaml API doc search**: `odoc-index` / `odoc-search` index and search generated odoc HTML.
- **Embedding as a library**: use the OCaml libraries directly (see [Embedding Ochat in OCaml](docs-src/lib/embedding.md)).
- **Ocaml indexing & code intelligence**: provides parsing and indexing of OCaml source files directly (no LSP dependency) to build precise indexes for code search and code-aware agents.

### Using builds/tests as an LLM feedback loop

When you run Ochat against an OCaml repository, the usual `dune build` / `dune runtest` loop becomes a high-signal feedback channel for LLM-generated edits: let an agent propose `apply_patch` diffs, run the build and tests, then feed compiler errors or failing expect tests back into the next turn.

### ChatML (experimental)

The repository ships an experimental language called *ChatML*: a small, expression‑oriented ML dialect with Hindley–Milner type inference (Algorithm W) extended with row polymorphism for records and variants.

The parser, type‑checker and runtime live under the `Chatml` modules and are documented under `docs-src/lib/chatml/` (see [`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md), [`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). Today it is exposed primarily via the experimental `dsl_script` binary and the `Chatml_*` library modules; it is not yet wired into ChatMD prompts or the main CLIs.

---

## Future directions

Ochat is intentionally **agent-first**: the roadmap focuses on making ChatMD, the runtime and `chat_tui` more expressive for building and operating fleets of custom agents, and on giving you better tools for observing and controlling how those agents behave.

Planned and experimental directions include:

- **Explicit control-flow & policy in ChatMD**  \
  The design note in [`control-flow-chatmd.md`](control-flow-chatmd.md)
  sketches a rules layer on top of ChatMD: you describe *events* (e.g.
  `pre_tool_call`, `turn_end`), *guards* over the transcript, and *actions*
  that materialise as normal ChatMD blocks (`<insert>`, `<deny>`, `<compact>`,
  `<agent>` …). The goal is to let you express things like “auto‑compact when
  the context grows too large” or “never call this tool without validating its
  inputs first” without hiding any logic from the transcript. This rules layer
  is not implemented yet; the document is a design sketch for future
  iterations.

- **Richer session tracking, branching and evaluation**  \
  Today `chat_tui` already persists sessions and lets you resume them.
  Future work focuses on making **branching conversations**, long‑term
  archives and agent evaluation runs first‑class so you can compare different
  agents on the same task, fork past sessions, and keep an auditable trail of
  how an agent evolved over time.

- **Session data (roadmap): per-session state + filesystem, backed by Irmin**  \
  Today sessions are persisted as on-disk snapshots (see the `Session_store` docs). A planned next step is to give agents a first-class way to store and retrieve session-specific data:

  - a simple key/value store API, scoped to the current conversation/session,
  - session-scoped file read/write (a “session filesystem”),
  - isolation by default (no accidental cross-session leakage),
  - tool-called agents inherit the parent session store (so helpers can share state without inventing ad-hoc protocols).

  The intent is for this to be backed by an Irmin database so session state can be versioned, merged, and synced in a principled way. This is not implemented yet; Irmin is currently only used by an auxiliary `sync` binary.

- **Additional LLM providers (roadmap)**  \
  Today the runtime integrates with OpenAI for chat execution and embeddings. A planned direction is to factor provider-specific details behind a stable interface so Ochat can target additional backends (for example: Anthropic/Claude, Google, local models) while keeping ChatMD files and tool contracts the same.

- **ChatML – a small, typed scripting language**  \
  The repository ships an experimental language called *ChatML*: a small,
  expression‑oriented ML dialect with Hindley–Milner type inference (Algorithm
  W) extended with row polymorphism for records and variants. The parser,
  type‑checker and runtime live under the `Chatml` modules and are documented
  under `docs-src/lib/chatml/` (see [`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md),
  [`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and
  [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). Today it is
  exposed primarily via the experimental `dsl_script` binary and the
  `Chatml_*` library modules; it is not yet wired into ChatMD prompts or the
  main CLIs. The long‑term plan is to use ChatML as a safe scripting language
  that agents can write and execute via a tool call, and can be embed inside ChatMD files for small,
  deterministic pieces of logic. Since it is strongly typed with full type inference, it provides a simple way to express logic without sacrificing safety or auditability. You can provide code execution capablities with high confidence, and provide a powerful tool for agents to express complex logic 

- **Custom Ocaml functions as tools via Dune plugins**  \
  A planned direction is to expose custom OCaml
  functions as tools via [Dune plugins](https://dune.readthedocs.io/en/stable/sites.html#plugins).

All of these directions share the same goal: make agents more reliable, 
composable, and expressive **without** sacrificing the “everything is a text file” property
that makes ChatMD workflows easy to debug and version‑control.

---

## Project status – expect rapid change

Ochat is a **research-grade** project that is evolving very rapidly.  APIs,
tool schemas, file formats and even high-level design choices may change as
we explore what works and what does not. If you intend to build something on
top of Ochat, please be prepared to:

* pin a specific commit or tag,
* re-run the tests after every `git pull`, and
* embrace breaking changes as part of the fun.

Despite the experimental label, **you can build real value today** – the
repository already enables powerful custom agent workflows.  I use it daily
with custom agents for everything from developing and documentation
generation, to writing emails and automating mundane tasks.

Please budget time for occasional refactors and breaking changes.
Bug reports, feature requests, and PRs are welcome and encouraged actually – just keep in mind the ground may still be
moving beneath your feet.

---

## License

All original source code is licensed under the terms stated in `LICENSE.txt`.
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4b811cc8195b65996de0eac85c2">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4bcb3e08195b2b2a79c1aa1f2d1">
RAW|
Purpose: Fix the README refactor example that accidentally contains literal patch marker lines, update it to a clean `read_file` example, and adjust the remaining references (`get_contents` → `read_file`).
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_cXQFZhLYdUJXFLcrnNsK9Jje" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef4bdd514819580fac052c3f51793">
RAW|
*** Begin Patch
*** Update File: README.md
@@
 ```xml
- <config model="gpt-4o" temperature="0"/>
- 
- <tool name="read_dir"/>
--<tool name="get_contents"/>
-+<tool name="read_file"/>
- <tool name="apply_patch"/>
+<config model="gpt-4o" temperature="0"/>
+
+<tool name="read_dir"/>
+<tool name="read_file"/>
+<tool name="apply_patch"/>
 
 <system>
 You are a careful refactoring assistant. Work in small, reversible steps.
 Before calling apply_patch, explain the change you want to make and wait for
@@
 From there you can ask the assistant to rename a function, extract a helper, or
-update documentation. It will use `read_dir` and `get_contents` to inspect the
+update documentation. It will use `read_dir` and `read_file` to inspect the
 code, then generate `apply_patch` diffs and apply them, with every tool call
 and patch recorded in the `.md` file.
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_cXQFZhLYdUJXFLcrnNsK9Jje">
RAW|
Syntax error at line 20:
 From there you can ask the assistant to rename a function, extract a helper, or
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4c216748195b1e48557e711210e">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4c50ff881958b83e4a95533f4cb">
RAW|
Purpose: Apply a properly formatted V4A patch to clean up the refactor example code block (remove accidental `-`/`+` lines) and update the accompanying text to reference `read_file`.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_JiTVKU9CiV03F6YekowZb5Qn" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef4c588b88195843b7db432ae4ffb">
RAW|
*** Begin Patch
*** Update File: README.md
@@
-```xml
- <config model="gpt-4o" temperature="0"/>
- 
- <tool name="read_dir"/>
--<tool name="get_contents"/>
-+<tool name="read_file"/>
- <tool name="apply_patch"/>
+ ```xml
+ <config model="gpt-4o" temperature="0"/>
+ 
+ <tool name="read_dir"/>
+ <tool name="read_file"/>
+ <tool name="apply_patch"/>
 
 <system>
 You are a careful refactoring assistant. Work in small, reversible steps.
 Before calling apply_patch, explain the change you want to make and wait for
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_JiTVKU9CiV03F6YekowZb5Qn">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
 171 | + ```xml
 172 | + <config model="gpt-4o" temperature="0"/>
 173 | + 
 174 | + <tool name="read_dir"/>
 175 | + <tool name="read_file"/>

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4c924b08195a2d00d9adbd66aab">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4ca85488195baa7196d61281a31">
RAW|
Purpose: Update the README refactor example explanation to refer to `read_file` instead of the legacy alias `get_contents`.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_UZw1xQrF5rAP32mUoiMMadeO" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef4ca915c8195850f5b40c58f70d1">
RAW|
*** Begin Patch
*** Update File: README.md
@@
-From there you can ask the assistant to rename a function, extract a helper, or
-update documentation. It will use `read_dir` and `get_contents` to inspect the
-code, then generate `apply_patch` diffs and apply them, with every tool call
-and patch recorded in the `.md` file.
+ From there you can ask the assistant to rename a function, extract a helper, or
+ update documentation. It will use `read_dir` and `read_file` to inspect the
+ code, then generate `apply_patch` diffs and apply them, with every tool call
+ and patch recorded in the `.md` file.
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_UZw1xQrF5rAP32mUoiMMadeO">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
 196 | + From there you can ask the assistant to rename a function, extract a helper, or
 197 | + update documentation. It will use `read_dir` and `read_file` to inspect the
 198 | + code, then generate `apply_patch` diffs and apply them, with every tool call
 199 | + and patch recorded in the `.md` file.

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4cf8ee081959c37e76749d06b26">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4d2c6b881959d0d9241512f7f4e">
RAW|
Purpose: Update the README’s MCP example prompt (`prompts/hello.md`) to use `read_file` instead of `get_contents` for consistency with current docs.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_MHiUjYquSujxFN1bMF61heQP" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef4d2f5008195a326b764173fe108">
RAW|
*** Begin Patch
*** Update File: README.md
@@
 <config model="gpt-4o" temperature="0"/>
 
 <tool name="read_dir"/>
-<tool name="get_contents"/>
+<tool name="read_file"/>
 
 <system>You are a documentation assistant.</system>
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_MHiUjYquSujxFN1bMF61heQP">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
 211 | +<tool name="read_file"/>

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4d6bf0481958030cb6305c5c797">
<summary>
**Updating core concepts**

I need to update the core concepts listed in the Tools bullet at the bottom because they’re outdated. It’s important to keep this information current. I’ll patch that line block and change the built-ins list accordingly. Keeping everything up-to-date ensures that the user gets the most accurate and relevant information, so I'll get on that right away! Let's see how we can make these improvements efficiently.
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4d9bc408195a9f71c37a73715b5">
RAW|
Purpose: Update the README “Core concepts” Tools bullet to highlight key built-ins (including `read_file` alias, `webpage_to_markdown`, `import_image`) and mention prompt-as-tool agents, matching current tool docs.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_6cElHoORCEPvKWpUHGh9RHoN" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef4d9d620819597896c03ba09fc0c">
RAW|
*** Begin Patch
*** Update File: README.md
@@
-- **Tools**  \
-  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`), shell wrappers around commands like `rg` or `git`, other ChatMD agents, or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).
+- **Tools**  \
+  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`, `read_file` *(alias: `get_contents`)*, `webpage_to_markdown`, `import_image`), shell wrappers around commands like `rg` or `git`, other ChatMD agents (prompt‑as‑tool), or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_6cElHoORCEPvKWpUHGh9RHoN">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
 296 | +  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`, `read_file` *(alias: `get_contents`)*, `webpage_to_markdown`, `import_image`), shell wrappers around commands like `rg` or `git`, other ChatMD agents (prompt‑as‑tool), or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4dc36788195a4a795a8577db362">
<summary>
**Fixing indentation issues**

I notice we have some indentation issues in the refactor prompt code block. There are leading spaces before the code fence and lines, which I'd like to adjust to ensure consistency. This happened because the patch tool inserted spaces. I plan to clean this up by replacing " ```xml" with "```xml" and adjusting lines like "<config" to "<config...". I'll make sure to only patch lines with leading spaces without altering other code blocks. Let's go ahead and fix that snippet!
</summary>

<summary>
**Applying a patch**

I’m getting ready to apply a patch now. I also need to check if there are leading spaces in other parts, particularly in the prompt block or any new sections. It’s essential to ensure everything is consistent and tidy. I want to be thorough to avoid missing any issues. Let’s go ahead and implement the patch while being mindful of checking those areas for any leading spaces that might still be hanging around!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4e492648195ba0bd567eb00d852">
RAW|
Purpose: Clean up formatting in the README refactor prompt snippet by removing accidental leading spaces inside the fenced XML block.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_kD0N0hcJuyoxXsjmcoLMcRbP" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef4e49e5c8195b54a12a7a85e0d42">
RAW|
*** Begin Patch
*** Update File: README.md
@@
- ```xml
- <config model="gpt-4o" temperature="0"/>
- 
- <tool name="read_dir"/>
- <tool name="read_file"/>
- <tool name="apply_patch"/>
+```xml
+<config model="gpt-4o" temperature="0"/>
+
+<tool name="read_dir"/>
+<tool name="read_file"/>
+<tool name="apply_patch"/>
 
 <system>
 You are a careful refactoring assistant. Work in small, reversible steps.
 Before calling apply_patch, explain the change you want to make and wait for
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_kD0N0hcJuyoxXsjmcoLMcRbP">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
 171 | +```xml
 172 | +<config model="gpt-4o" temperature="0"/>
 173 | +
 174 | +<tool name="read_dir"/>
 175 | +<tool name="read_file"/>
 176 | +<tool name="apply_patch"/>

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4e8d61081959ecc720d7d2af97a">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4ed39388195a46fce6cf2f523c7">
RAW|
Purpose: Quickly verify the updated README section around “What can I do with Ochat?” and the newly added “Tools & capabilities (quick tour)” renders correctly after patches.
|RAW
</assistant>


<tool_call tool_call_id="call_DNzQWX8lnu6LvuRxqA1xkcYV" function_name="read_file" id="fc_0a7776dd715389aa00696ef4eda1448195b7f4a718c0215bcf">
RAW|
{"file":"README.md","offset":0}
|RAW
</tool_call>

<tool_response tool_call_id="call_DNzQWX8lnu6LvuRxqA1xkcYV">
RAW|

# Ochat – toolkit for building custom AI agents, scripted LLM pipelines & vector search

*Everything you need to prototype and run modern LLM workflows as plain files (implemented in OCaml).*

<div>
 <img src="assets/demo.gif" alt="chat_tui demo" height="700" width="900"/>
</div>


## What is Ochat?

Ochat is a toolkit for building **agent workflows and orchestrations as static files**.

If you like tools like Claude Code or Codex, Ochat is a more fundamental set of building blocks: instead of hard-coding the “agent application” into a single UI, you can implement something Claude Code‑like by shipping a *prompt pack* (a set of `.md` files) plus tools and running the agent using the terminal UI that the project provides.

In Ochat, an agent is a `.md` file written in a Markdown + XML dialect called **ChatMarkdown (ChatMD)**. A single file is the whole program:

- the model and generation parameters,
- which tools the assistant is allowed to call,
- the full conversation history (including tool calls and their results),
- imported artefacts (documents/images) when needed.

The runtime does **not** depend on file extensions: any filename can contain ChatMD. We use `.md` by convention so editors render Markdown nicely and you get syntax highlighting.

Because everything is captured in text files, workflows are:

- **reproducible** – the exact config and transcript are version‑controlled,
- **diff‑able** – reviews show exactly what changed and what the model did,
- **composable** – workflows can call other workflows (prompt‑as‑tool),
- **portable** – prompts are plain text; tools exchange JSON.

The same `.md` definition can be executed in multiple hosts:

- the **terminal UI** (`chat_tui`) for interactive work,
- **scripts and CI** via `ochat chat-completion`, and
- a **remote MCP server** via `mcp_server`, so IDEs or other applications can call agents over stdio or HTTP/SSE.

The chatmd language provides a rich set of features for prompt engineering in a modular way supporting all levels of complexity.

Ochat is implemented in OCaml, and provides tools for ocaml development, but the workflows themselves are **language‑agnostic** and ochat makes no assumptions about the types of applications the workflows target: you can use ochat to build workflows for any use case that benefits from LLMs + tools, and it puts no contraints on how simple or complex those workflows are.

**LLM provider support (today): OpenAI only.** Ochat currently integrates with OpenAI for chat execution and embeddings. The architecture is intended to support additional providers, but those integrations are not implemented yet. 

For details on the current OpenAI surface, see `docs-src/lib/openai/` (for example: [`responses`](docs-src/lib/openai/responses.doc.md)).

If you want the OCaml-specific entry points (embedding as a library, OCaml API doc search, `opam`/`dune` workflows), see the **OCaml integration** section below.

---

## What can I do with Ochat?

- **Author agent workflows as static files**  
  Write agents as `.md` files (ChatMarkdown). Each file is both the prompt *and* the execution log: model config, tool permissions, tool calls/results, and the full transcript.

- **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  

  You can mix:

  - **built-in tools** for common building blocks:
    - repo-safe editing: `apply_patch`
    - filesystem reads: `read_dir` (directory listing), `read_file` *(alias: `get_contents`)*
    - web ingestion: `webpage_to_markdown` (HTML → Markdown + GitHub blob fast-path)
    - local semantic search over docs: `index_markdown_docs` + `markdown_search`, and `odoc_search`
    - hybrid retrieval over code: `index_ocaml_code` + `query_vector_db`
    - vision inputs: `import_image` (bring local screenshots/diagrams into the model)
  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
  
    ```xml
    <tool mcp_server="stdio:npx -y brave-search-mcp" />
    ```

  - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
  
  See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).

- **Tools & capabilities (quick tour)**  
  These are the features most users care about on day 1—each with a minimal example.

  **1) Atomic repo edits with `apply_patch`**

  Declare:
  ```xml
  <tool name="apply_patch"/>
  ```
  Tool calls pass a single patch string (V4A format):
  ```text
  {
    "patch": "*** Begin Patch\n*** Update File: path/to/file\n...\n*** End Patch"
  }
  ```
  Why it’s great: you get **reviewable, multi-file, atomic** edits instead of ad-hoc mutations.

  **2) Read files safely with `read_file` (alias: `get_contents`)**

  Declare:
  ```xml
  <tool name="read_file"/>
  ```
  Notes: `read_file` refuses binary-ish content and truncates large files to keep context bounded.

  **3) Ingest web pages (and GitHub code slices) with `webpage_to_markdown`**

  Declare:
  ```xml
  <tool name="webpage_to_markdown"/>
  ```
  Works especially well on GitHub blob URLs with line ranges, e.g.:
  - `https://github.com/owner/repo/blob/main/lib/foo.ml#L10-L80`

  **4) Prompt-as-tool: mount a `.chatmd` workflow as a callable tool**

  Declare:
  ```xml
  <tool name="triage" agent="prompts/triage.chatmd" local/>
  ```
  Why it’s great: build *small specialized agents* (triage, planner, doc-writer) and compose them.

  **5) “Docs RAG” over your project Markdown**

  Declare:
  ```xml
  <tool name="index_markdown_docs"/>
  <tool name="markdown_search"/>
  ```
  Typical flow: index once (per repo), then query in natural language to pull high-signal snippets from your docs.

  **6) Bring screenshots/diagrams into the model with `import_image`**

  Declare:
  ```xml
  <tool name="import_image"/>
  ```
  Example payload:
  ```json
  { "path": "assets/screenshot.png" }
  ```

  **7) Import tools from elsewhere via MCP**

  Declare:
  ```xml
  <tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
  ```
  Why it’s great: share tool catalogs across environments (local, container, CI) without changing prompts.

- **Build Claude Code/Codex-style agentic applications via custom “prompt packs”**  
  You can implement this as a set of specialized agents (planning agent, coding agent, test agent, doc agent…) and wire them together in an orchestration agent via agent-as-tool. The “application” is just a set of ChatMD files and you can run it via the terminal ui (`chat_tui`) or via the chat-completion CLI (`ochat chat-completion`).

- **Run the same workflows in different hosts**  
  Use `chat_tui` for interactive sessions, `ochat chat-completion` for scripts/CI/cron, and `mcp_server` to expose prompts as tools to IDEs and other hosts.

- **Ground agents in your own corpus**  
  Build indexes for docs/source trees and query them from within prompts so the agent can cite and follow project conventions rather than guessing. See [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md).

- **Continuously improve prompts**  
  Use the `mp-refine-run` binary to iteratively refine prompts and tool descriptions using evaluators, treating prompt design as a versioned, testable artifact.

---

## Example ChatMD prompts


### Example: interactive refactor agent

Turn a `.md` file into a refactoring bot that reads files and applies patches under your control.

1. Create `prompts/refactor.md`:

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="read_dir"/>
<tool name="read_file"/>
<tool name="apply_patch"/>

<system>
You are a careful refactoring assistant. Work in small, reversible steps.
Before calling apply_patch, explain the change you want to make and wait for
confirmation from the user.
</system>

<user>
We are in a codebase. Look under ./lib, find a small improvement and
propose a patch.
</user>
```

2. Open it in the TUI:

```sh
dune exec chat_tui -- -file prompts/refactor.md
```

 From there you can ask the assistant to rename a function, extract a helper, or
 update documentation. It will use `read_dir` and `read_file` to inspect the
 code, then generate `apply_patch` diffs and apply them, with every tool call
 and patch recorded in the `.md` file.

### Example: publish a prompt as an MCP tool

Export a `.md` file as a remote tool that other MCP‑compatible clients can call.

1. Create `prompts/hello.md`:

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="read_dir"/>
<tool name="read_file"/>

<system>You are a documentation assistant.</system>

<user>
List the files under docs-src/ and summarize what each top-level folder is for.
</user>
```

2. Start the MCP server so it exports `hello.md` as a tool (by default it
   reads prompts from `./prompts`, or from `$MCP_PROMPTS_DIR` if set):

```sh
dune exec mcp_server -- --http 8080
```

Any MCP client can now discover the `hello` tool via `tools/list` and call it
with `tools/call` over JSON‑RPC. For example, a minimal HTTP request that lists
the available tools looks like:

```sh
curl -s http://localhost:8080/mcp \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```

The response includes an entry for `hello` whose JSON schema is inferred from
the ChatMD file; calling that tool runs your prompt and streams the result back
to the client.

For a more advanced, end-to-end research agent built from the same building
blocks, see the
[Discovery bot – research agent workflow](docs-src/guide/discovery-bot-workflow.md).

---

## Build from source (OCaml)

Install dependencies, build, and run tests:

```sh
opam switch create .
opam install . --deps-only

dune build
dune runtest

# Optional – build API docs when the dune-project declares a (documentation ...) stanza
dune build @doc
```

> The `@doc` alias is generated only when the project’s `dune-project` file
> contains a `(documentation ...)` stanza. If the command above fails, add the
> stanza or skip the step.

> On Apple Silicon (macOS arm64), Owl's OpenBLAS dependency can sometimes fail
> to build during `opam install`. If you see BLAS/OpenBLAS errors while
> installing dependencies or running `dune build`, see
> [Build & installation troubleshooting](docs-src/guide/build-troubleshooting.md#owl--openblas-on-apple-silicon-macos-arm64)
> for a proven workaround.

Run a quick interactive session with the terminal UI:

```sh
dune exec chat_tui -- -file prompts/interactive.md
```

Or run a non‑interactive chat completion over a ChatMD prompt as a smoke test:

```sh
ochat chat-completion \
  -prompt-file prompts/hello.md \
  -output-file .chatmd/smoke.md
```

For more on `ochat chat-completion` (flags, exit codes, ephemeral runs), see
[`docs-src/cli/chat-completion.md`](docs-src/cli/chat-completion.md).

---

## Core concepts in one page

- **ChatMarkdown (ChatMD)**  \
  A Markdown + XML dialect that stores model config, tool declarations and the full conversation (including tool calls, reasoning traces and imported artefacts) in a single `.md` file. Because prompts are plain text files you can review, diff and refactor them like code, and the runtime guarantees that what the model sees is exactly what is in the document. See the [language reference](docs-src/overview/chatmd-language.md).

- **Tools**  \
  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`, `read_file` *(alias: `get_contents`)*, `webpage_to_markdown`, `import_image`), shell wrappers around commands like `rg` or `git`, other ChatMD agents (prompt‑as‑tool), or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).

- **Agents & prompt‑as‑tool**  \
  Any `.md` file can be treated as an agent. Locally you can call it via `<agent>` blocks or mount it as a tool inside another prompt; remotely, `mcp_server` exposes it as a `tools/call::<name>` endpoint that IDEs or other hosts can invoke. Complex workflows become graphs of small, composable agents rather than monolithic prompts.

- **chat_tui**  \
  A Notty‑based terminal UI for editing and running `.md` files. It turns each prompt into a **terminal application**: live streaming of model output and tool responses, Vim‑style navigation, context compaction for long histories, and persistent sessions that you can resume or branch. You can think of `chat_tui` as the “host” and `.md` files as pluggable apps. See the [chat_tui guide](docs-src/guide/chat_tui.md).

- **CLI and helpers**  \
  Binaries like `ochat` and `md-index` / `md-search` provide script‑friendly entry points for running prompts, building indexes and querying them from the shell. (If you’re in OCaml, `odoc-index` / `odoc-search` can also index generated API docs.) See the [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) for non‑interactive runs; other commands are documented under `docs-src/cli/` and the generated odoc docs.

- **MCP server**  \
  `mcp_server` turns `.md` files and selected tools into MCP resources and tools that other applications can list and call over stdio or HTTP/SSE. See the [mcp_server binary doc](docs-src/bin/mcp_server.doc.md).

- **Search & indexing**  \
  Modules and binaries that build vector indexes over markdown docs and source code, powering tools like `markdown_search` and `query_vector_db`. (If you’re in OCaml, you can also index generated API docs.) See [Search, indexing & code-intelligence](docs-src/guide/search-and-indexing.md).

- **Meta-prompting**  \
  A library and CLI (`mp-refine-run`) for generating, scoring and refining prompts in a loop, so prompt engineering itself can be versioned and automated. See the [`Meta_prompting` overview](docs-src/lib/meta_prompting.doc.md).

> Each bullet links to a deeper reference under `docs-src/`.

---

## Documentation

Deep-dive docs live under `docs-src/`. Key entry points:

- [ChatMarkdown language reference](docs-src/overview/chatmd-language.md) – element tags, inline helpers, and prompt‑writing guidelines.
- [Built-in tools & custom tools](docs-src/overview/tools.md) – built‑in toolbox, shell wrappers, custom tools, and MCP tool import.
- [chat_tui guide & key bindings](docs-src/guide/chat_tui.md) – keybindings, context compaction, persistent sessions, and advanced behaviours.
- [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) – non‑interactive runs, flags, exit codes and ephemeral runs.
- [MCP server & protocol details](docs-src/bin/mcp_server.doc.md) – how `mcp_server` exposes prompts and tools over stdio or HTTP/SSE.
- [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md) – indexers, searchers and prompt patterns for hybrid retrieval.
- [Meta-prompting & Prompt Factory](docs-src/lib/meta_prompting.doc.md) – generators, evaluators, refinement loops and prompt packs.

OCaml integration and internals:

- [Embedding Ochat in OCaml](docs-src/lib/embedding.md) – reusing the libraries and caching patterns.
- [ChatML language & runtime](docs-src/lib/chatml/chatml_lang.doc.md) – experimental typed scripting language; see also the parser and resolver docs under `docs-src/lib/chatml/`.

---

## Binaries

| Binary | Purpose | Example |
|--------|---------|---------|
| `chat_tui` (`chat-tui`) | interactive TUI | `chat_tui -file notes.md` |
| `ochat`    | misc CLI (index, query, tokenise …) | `ochat query -vector-db-folder _index -query-text "tail-rec map"` |
| `mcp_server` | serve prompts & tools over JSON-RPC / SSE | `mcp_server --http 8080` |
| `mp-refine-run` | refine prompts via *recursive meta-prompting* | `mp-refine-run -task-file task.md -input-file draft.md` |
| `md-index` / `md-search` | Markdown → index / search | `md-index --root docs`; `md-search --query "streams"` |
| `odoc-index` / `odoc-search` | (OCaml) odoc HTML → index / search | `odoc-index --root _doc/_html` |

Run any binary with `-help` for details.

---

## Project layout

```
bin/         – chat_tui, mcp_server, ochat …
lib/         – re-usable libraries (chatmd, functions, vector_db …)
docs-src/    – Markdown docs rendered by odoc & included here
prompts/     – sample ChatMD prompts served by the MCP server
dune-project – dune metadata
```

---

## OCaml integration

Ochat is implemented in OCaml. Ochat intends to be language agnostic and the *workflows* can be used in any setup (tools exchange JSON; prompts are plain files), but being implemented in OCaml it has first class support for ocaml development. these entry points are OCaml-specific:

- **OCaml development environment guide**: see [`DEVELOPMENT.md`](DEVELOPMENT.md) for a dedicated walkthrough that sets up local OCaml documentation, search indexes, and related workflows that are useful for OCaml-focused agents.
- **OCaml API doc search**: `odoc-index` / `odoc-search` index and search generated odoc HTML.
- **Embedding as a library**: use the OCaml libraries directly (see [Embedding Ochat in OCaml](docs-src/lib/embedding.md)).
- **Ocaml indexing & code intelligence**: provides parsing and indexing of OCaml source files directly (no LSP dependency) to build precise indexes for code search and code-aware agents.

### Using builds/tests as an LLM feedback loop

When you run Ochat against an OCaml repository, the usual `dune build` / `dune runtest` loop becomes a high-signal feedback channel for LLM-generated edits: let an agent propose `apply_patch` diffs, run the build and tests, then feed compiler errors or failing expect tests back into the next turn.

### ChatML (experimental)

The repository ships an experimental language called *ChatML*: a small, expression‑oriented ML dialect with Hindley–Milner type inference (Algorithm W) extended with row polymorphism for records and variants.

The parser, type‑checker and runtime live under the `Chatml` modules and are documented under `docs-src/lib/chatml/` (see [`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md), [`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). Today it is exposed primarily via the experimental `dsl_script` binary and the `Chatml_*` library modules; it is not yet wired into ChatMD prompts or the main CLIs.

---

## Future directions

Ochat is intentionally **agent-first**: the roadmap focuses on making ChatMD, the runtime and `chat_tui` more expressive for building and operating fleets of custom agents, and on giving you better tools for observing and controlling how those agents behave.

Planned and experimental directions include:

- **Explicit control-flow & policy in ChatMD**  \
  The design note in [`control-flow-chatmd.md`](control-flow-chatmd.md)
  sketches a rules layer on top of ChatMD: you describe *events* (e.g.
  `pre_tool_call`, `turn_end`), *guards* over the transcript, and *actions*
  that materialise as normal ChatMD blocks (`<insert>`, `<deny>`, `<compact>`,
  `<agent>` …). The goal is to let you express things like “auto‑compact when
  the context grows too large” or “never call this tool without validating its
  inputs first” without hiding any logic from the transcript. This rules layer
  is not implemented yet; the document is a design sketch for future
  iterations.

- **Richer session tracking, branching and evaluation**  \
  Today `chat_tui` already persists sessions and lets you resume them.
  Future work focuses on making **branching conversations**, long‑term
  archives and agent evaluation runs first‑class so you can compare different
  agents on the same task, fork past sessions, and keep an auditable trail of
  how an agent evolved over time.

- **Session data (roadmap): per-session state + filesystem, backed by Irmin**  \
  Today sessions are persisted as on-disk snapshots (see the `Session_store` docs). A planned next step is to give agents a first-class way to store and retrieve session-specific data:

  - a simple key/value store API, scoped to the current conversation/session,
  - session-scoped file read/write (a “session filesystem”),
  - isolation by default (no accidental cross-session leakage),
  - tool-called agents inherit the parent session store (so helpers can share state without inventing ad-hoc protocols).

  The intent is for this to be backed by an Irmin database so session state can be versioned, merged, and synced in a principled way. This is not implemented yet; Irmin is currently only used by an auxiliary `sync` binary.

- **Additional LLM providers (roadmap)**  \
  Today the runtime integrates with OpenAI for chat execution and embeddings. A planned direction is to factor provider-specific details behind a stable interface so Ochat can target additional backends (for example: Anthropic/Claude, Google, local models) while keeping ChatMD files and tool contracts the same.

- **ChatML – a small, typed scripting language**  \
  The repository ships an experimental language called *ChatML*: a small,
  expression‑oriented ML dialect with Hindley–Milner type inference (Algorithm
  W) extended with row polymorphism for records and variants. The parser,
  type‑checker and runtime live under the `Chatml` modules and are documented
  under `docs-src/lib/chatml/` (see [`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md),
  [`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and
  [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). Today it is
  exposed primarily via the experimental `dsl_script` binary and the
  `Chatml_*` library modules; it is not yet wired into ChatMD prompts or the
  main CLIs. The long‑term plan is to use ChatML as a safe scripting language
  that agents can write and execute via a tool call, and can be embed inside ChatMD files for small,
  deterministic pieces of logic. Since it is strongly typed with full type inference, it provides a simple way to express logic without sacrificing safety or auditability. You can provide code execution capablities with high confidence, and provide a powerful tool for agents to express complex logic 

- **Custom Ocaml functions as tools via Dune plugins**  \
  A planned direction is to expose custom OCaml
  functions as tools via [Dune plugins](https://dune.readthedocs.io/en/stable/sites.html#plugins).

All of these directions share the same goal: make agents more reliable, 
composable, and expressive **without** sacrificing the “everything is a text file” property
that makes ChatMD workflows easy to debug and version‑control.

---

## Project status – expect rapid change

Ochat is a **research-grade** project that is evolving very rapidly.  APIs,
tool schemas, file formats and even high-level design choices may change as
we explore what works and what does not. If you intend to build something on
top of Ochat, please be prepared to:

* pin a specific commit or tag,
* re-run the tests after every `git pull`, and
* embrace breaking changes as part of the fun.

Despite the experimental label, **you can build real value today** – the
repository already enables powerful custom agent workflows.  I use it daily
with custom agents for everything from developing and documentation
generation, to writing emails and automating mundane tasks.

Please budget time for occasional refactors and breaking changes.
Bug reports, feature requests, and PRs are welcome and encouraged actually – just keep in mind the ground may still be
moving beneath your feet.

---

## License

All original source code is licensed under the terms stated in `LICENSE.txt`.
|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4f012c48195a55edc54c4bd7921">
<summary>
**Cleaning up README details**

The README section is looking good, but I noticed a leading space before the line "From there..." in the example refactor snippet. It seems the space is due to a patch, and while it's not critical, it would be better to remove it. Also, two subsequent lines have leading spaces that need cleaning. I’ll ensure there are no outdated references to get_contents in the README, aside from alias mentions. Let's proceed with those patches!
</summary>

<summary>
**Patching README details**

I'll patch the README section with V4A by replacing " From there..." with "From there..." to remove that leading space. I'll do the same for the next lines that need correction. It seems like a straightforward step to ensure consistency and clarity. Making these tweaks should help keep the documentation neat and professional. I just want to make sure everything looks good for anyone who reads it later! Let's get on with those changes!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4f8c240819592905d4a6ada22b7">
RAW|
Purpose: Clean minor formatting in README by removing unintended leading spaces before the “From there you can ask…” paragraph under the refactor example.
|RAW
</assistant>


<tool_call type="custom_tool_call" tool_call_id="call_ceHnr5w1CU0hEEeFB7ivFuis" function_name="apply_patch" id="ctc_0a7776dd715389aa00696ef4f8d3888195b1fd65fc474d4d55">
RAW|
*** Begin Patch
*** Update File: README.md
@@
- From there you can ask the assistant to rename a function, extract a helper, or
- update documentation. It will use `read_dir` and `read_file` to inspect the
- code, then generate `apply_patch` diffs and apply them, with every tool call
- and patch recorded in the `.md` file.
+From there you can ask the assistant to rename a function, extract a helper, or
+update documentation. It will use `read_dir` and `read_file` to inspect the
+code, then generate `apply_patch` diffs and apply them, with every tool call
+and patch recorded in the `.md` file.
*** End Patch
|RAW
</tool_call>

<tool_response type="custom_tool_call" tool_call_id="call_ceHnr5w1CU0hEEeFB7ivFuis">
RAW|
✅ Patch applied successfully!

┏━[ README.md ]----------------------------------------------------------------------
 196 | +From there you can ask the assistant to rename a function, extract a helper, or
 197 | +update documentation. It will use `read_dir` and `read_file` to inspect the
 198 | +code, then generate `apply_patch` diffs and apply them, with every tool call
 199 | +and patch recorded in the `.md` file.

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef4fadcb88195ab77e3f34a39720c">
</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef4fd359081958a02f7a89ee7739a">
RAW|
### What I did:
- Updated `README.md` to surface the most compelling tool features **immediately** (without requiring readers to open the deeper tools doc):
  - Added a new **“Tools & capabilities (quick tour)”** section with short, concrete examples for:
    - `apply_patch` (atomic repo edits)
    - `read_file` (and alias `get_contents`)
    - `webpage_to_markdown` (GitHub blob + line range callout)
    - agent-as-tool (`<tool agent="..."/>`)
    - `index_markdown_docs` + `markdown_search` (docs RAG flow)
    - `import_image` (vision inputs with example JSON payload)
    - MCP tool import (`<tool mcp_server="..." includes="..."/>`)
  - Updated earlier “Compose unique agents…” bullets to reflect the current best built-ins (and fixed a stray `.` in the MCP snippet).
  - Updated the README example prompts to use `read_file` instead of `get_contents` (while still mentioning `get_contents` as an alias where helpful).
- Files modified:
  - `README.md`

### Conclusion:
Your README now highlights the “day 1” tool wins right up front—users will immediately see *why* ochat is powerful (atomic patching, safe file reads, GitHub/web ingestion, prompt-as-tool composition, local RAG over docs/code, vision inputs, and MCP import), without needing to deep-dive into `docs-src/overview/tools.md`.
|RAW
</assistant>

<user>
ok add a commit to the current branch for the updates you made
</user>

<reasoning id="rs_0a7776dd715389aa00696ef53fb220819592f4b508bc4ea476">
<summary>
**Looking into git commands**

I need to use git tools effectively. First, I’ll check the status of my files, then add the necessary files before making a commit with a clear message. I’ll use the functions for git status, git add, and git commit to make sure everything is tracked properly. It's good to be organized with version control, so I’m looking forward to getting this done right!
</summary>

</reasoning>

<assistant id="msg_0a7776dd715389aa00696ef5420ab0819582daa75882ab18b9">
RAW|
Purpose: Check git status, stage the documentation updates, and create a commit on the current branch.
|RAW
</assistant>


<tool_call tool_call_id="call_G3ZTwe1sczDXHJiHi6RFjL7j" function_name="git_status" id="fc_0a7776dd715389aa00696ef54215bc8195a121df84786b0777">
RAW|
{"arguments":[]}
|RAW
</tool_call>

<tool_response tool_call_id="call_G3ZTwe1sczDXHJiHi6RFjL7j">
RAW|
On branch update-readme-docs
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git restore <file>..." to discard changes in working directory)
    modified:   Readme.md
    modified:   docs-src/overview/tools.md

no changes added to commit (use "git add" and/or "git commit -a")

|RAW
</tool_response>

<reasoning id="rs_0a7776dd715389aa00696ef54433688195b677456c2f26146a">
</reasoning>

<tool_call tool_call_id="call_MnUVYZpOrzWfIwyC0KaipQti" function_name="git_add" id="fc_0a7776dd715389aa00696ef5451774819599261604f67e8e90">
RAW|
{"arguments":["Readme.md","docs-src/overview/tools.md"]}
|RAW
</tool_call>

<tool_response tool_call_id="call_MnUVYZpOrzWfIwyC0KaipQti">
RAW|
Command output is empty
|RAW
</tool_response>

<tool_call tool_call_id="call_vMJDaYCEXLSt52rEW0QoZZKG" function_name="git_commit" id="fc_0a7776dd715389aa00696ef54723b48195b038a1346c4d4127">
RAW|
{"arguments":["-m","docs: refresh tools docs and add quick tour to readme"]}
|RAW
</tool_call>

<tool_response tool_call_id="call_vMJDaYCEXLSt52rEW0QoZZKG">
RAW|
[update-readme-docs 6041eb19] docs: refresh tools docs and add quick tour to readme
 2 files changed, 273 insertions(+), 165 deletions(-)

|RAW
</tool_response>

<assistant id="msg_0a7776dd715389aa00696ef54d8b2c8195b241e05097b0ea12">
RAW|
### What I did:
- Checked working tree status (`git status`)
- Staged the modified docs:
  - `Readme.md`
  - `docs-src/overview/tools.md`
- Created a commit on the current branch.

### Conclusion:
Committed on branch `update-readme-docs`:

- Commit: `6041eb19`
- Message: `docs: refresh tools docs and add quick tour to readme`
|RAW
</assistant>

