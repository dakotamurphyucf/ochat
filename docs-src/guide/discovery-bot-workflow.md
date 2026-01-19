# Discovery bot – research agent workflow

This workflow turns a single ChatMD prompt into an autonomous
**research → summarise → patch** agent. It is designed to be copied into your
own repository and adapted to your needs.

At a high level, the agent:

- uses a remote MCP tool (Brave Search) for web search,
- fetches pages with `webpage_to_markdown`,
- writes structured notes to a markdown file via `apply_patch`, and
- combines local (`apply_patch`, `sed`) and remote tools into one loop.

The configuration below is a minimal **blueprint**; for a more complete,
production‑ready prompt, see
[`prompt_examples/discovery.md`](../../prompt_examples/discovery.md).

---

## Prompt blueprint

```xml
<config model="o3" max_tokens="100000" reasoning_effort="high" />

<!-- Built-ins -->
<tool name="webpage_to_markdown" />
<tool name="apply_patch" />

<!-- Remote MCP server for web search -->
<tool mcp_server="stdio:npx -y brave-search-mcp" />

<!-- Read-only helper so the LLM can *peek* at existing research files -->
<tool name="sed" command="sed" description="read-only file viewer" />

<system>
You are a meticulous web-research agent.  Your job is to fully resolve the
user’s query, writing detailed findings to <research_results.md> as you go.

Workflow (strict order):
1. Create or open the target markdown file using `apply_patch`.
2. Run **at least** 3 brave_web_search queries.
3. For each result:
   a. Fetch the page with `webpage_to_markdown`.
   b. Extract relevant facts, examples, citations.
   c. Immediately append a structured summary to the results file via
      `apply_patch`.
4. Continue until you cannot find new useful sources.
5. Reply with a JSON object `{reply, sources}` and **nothing else**.
</system>

<user>
store results in prompting_research_results.md
  Research best practices for prompting OpenAI’s latest models
  – include o3 reasoning models
  – include GPT-4.1 models
  – prompting in general
  – include any relevant academic literature
</user>
```

---

## Why this workflow is interesting

- **MCP tool integration** – the Bravo Search MCP wrapper exposes several
  search endpoints; the runtime uses `tools/list` to discover them and makes
  them available to the model via the `<tool mcp_server=…/>` declaration.
- **Self‑mutating workflow** – after each web request, the LLM edits
  `prompting_research_results.md` using `apply_patch`, so progress is never
  lost even if the run is interrupted.
- **Local + remote tools** – combines local helpers (`apply_patch`, `sed`)
  with a remote MCP service (Brave Search) in a single ChatMD file.

To adapt it:

- change the `<config>` model or `reasoning_effort` to match your latency and
  cost budget,
- replace the `<user>` task with your research question and target notes file,
- swap Brave Search for another MCP server by changing the `mcp_server`
  attribute.

---

## Production example in this repository

The repository ships an *industrial‑strength* research agent in
[`prompt_examples/discovery.md`](../../prompt_examples/discovery.md). It shows
how to chain **Brave web search (remote MCP tool)**, `webpage_to_markdown`,
and `apply_patch` into a self‑healing loop that:

1. executes three diverse search queries;
2. converts every candidate web page to Markdown;
3. summarises the findings in a dedicated notes file; and
4. appends new sources until diminishing returns kick in.

Fork the file, change the `<user>` goal, and you have a bespoke research bot
in seconds.

