# ChatGPT OCaml Toolkit

An experimental command-line toolkit and OCaml library that lets you **chat with large language models through structured _chatmd_ documents**, index and search OCaml code with OpenAI embeddings, and automate edits to the source-tree via function-calling.  
The project is highly experimental, but may serve as a reference for:

• Integrating OpenAI’s API from OCaml (chat/completions, embeddings, function calling, streaming, etc.)  
• Building small domain-specific languages on top of XML/Markdown (`chatmd`)  
• Using Eio for concurrent IO and Owl for vector similarity search

---

## Table of contents

1. [Installation](#installation)  
2. [CLI reference](#cli-reference)  
3. [Quick start](#quick-start)  
4. [The chatmd language](#the-chatmd-language)  
   1. [Top-level elements](#top-level-elements)  
   2. [Message roles & function calling](#message-roles--function-calling)  
   3. [Inline content helpers](#inline-content-helpers)  
   4. [Raw text blocks](#raw-text-blocks)  
5. [Built-in tools](#built-in-tools)  
6. [Examples](#examples)  
7. [Contributing & License](#contributing--license)

---

## Installation

The project is published as a normal opam package.  We recommend working inside a local
switch so that the experimental dependencies do not pollute your system installation.

```sh
# inside the project root
opam switch create .  # creates a local switch (optional, but recommended)
opam install . --deps-only  # install build-time dependencies

# Owl on Apple Silicon sometimes needs an explicit pin:
#   https://github.com/owlbarn/owl/issues/597#issuecomment-1119470934
#
# opam pin -n git+https://github.com/mseri/owl.git#arm64 --with-version=1.1.0
# PKG_CONFIG_PATH="/opt/homebrew/opt/openblas/lib/pkgconfig" opam install owl.1.1.0

# build everything
dune build
```

---

## CLI reference

All commands live under the executable `chatgpt` (installed as `chatgpt.exe` or available
via `dune exec` while developing).

```sh
dune exec ./bin/main.exe -- <command> [options]
# or, after installation
gpt <command> [options]
```

### `index`

Create / update a vector database for a folder of OCaml files using OpenAI embeddings.

```sh
gpt index \
  -folder-to-index <folder>    # default ./lib
  -vector-db-folder <folder>   # default ./vector
```

### `query`

Semantic search over previously indexed source code.

```sh
gpt query \
  -vector-db-folder <folder>   # default ./vector
  -query-text "why isn’t my Functor compiling?" \
  -num-results 10              # default 5
```

### `chat-completion`

Run a chat session described in a _chatmd_ file.  The command **streams** the model’s
answer back into the same file and automatically appends a new empty `<msg role="user">`
block so you can keep typing and re-run the command.

```sh
# start a new conversation from a template
gpt chat-completion \
  -prompt-file ./prompts/template.md \
  -output-file ./conversations/my_chat.md

# continue an existing conversation
gpt chat-completion -output-file ./conversations/my_chat.md
```

`chat-completion` **no longer takes `-max-tokens` or model flags on the command line** –
those are specified inside the `<config/>` element within the chatmd file (see below).

### `tokenize`

Utility that tokenises a file using the TikToken `cl100k_base` codec (handy for debugging
token budgets).

```sh
gpt tokenize -file src/file.ml       # default bin/main.ml
```

---

## Quick start

1.  Create a new markdown file `conversation.md` with the following content:

    ```xml
    <config model="o3" max_tokens="1024" reasoning_effort="high"/>

    <!-- make the source tree available so the assistant can patch files -->
    <tool name="apply_patch"/>

    <msg role="system">You are a helpful OCaml pair-programmer.</msg>
    <msg role="user">How can I make this function tail-recursive?</msg>
    ```

2.  Run the CLI:

    ```sh
    chatgpt chat-completion -output-file conversation.md
    ```

3.  The file will be updated in-place with the assistant’s answer.  Edit, re-run – enjoy!

---

## The chatmd language

Chatmd is an XML-flavoured, line-oriented markup that represents a **conversation plus
execution context**.  It is parsed by `Prompt_template.Chat_markdown` and executed by
`Chat_response`.

### Top-level elements

| Element | Purpose | Important attributes |
|---------|---------|----------------------|
| `<config/>` | Current model and generation parameters. **Must be present once** near the top. | `model` (e.g. `o3`), `max_tokens`, `temperature`, `reasoning_effort` (`low\|medium\|high`). |
| `<tool/>` | Declare a tool that the model may call. | • `name` – tool/function name.<br>• *Built-ins* only need the name (e.g. `apply_patch`).<br>• For **custom** tools add `command="<shell-command>"` and optional `description="..."`. |
| `<msg>` | A chat message. | `role` (`system,user,assistant,developer,tool`), optional `name`, `id`, `status`.  Assistant messages that *call a tool* add the boolean `tool_call` attribute plus `function_name` & `tool_call_id`. |
| `<reasoning>` | Internal scratchpad the model can populate when reasoning is enabled.  Not needed when authoring prompts. | `id`, `status`. Contains one or more `<summary>` blocks. |
| `<import/>` | Include another file verbatim *at parse time*.  Mostly useful for templates. | `file` – relative path. |

#### Importing templates / snippets

Use `<import file="…" />` to **inline the contents of another chatmd (or plain text) file at
parse-time**.  This is handy for separating prompt templates from the evolving
conversation or for re-using boilerplate system messages.

Imagine we have a template called `pair_programmer.chatmd`:

```xml
<config model="o3" reasoning_effort="high"/>

<msg role="system">You are a knowledgeable OCaml pair-programmer.</msg>

<!-- Tools that are always available in this scenario -->
<tool name="apply_patch"/>
<tool name="get_contents"/>
```

Then a concrete conversation file can simply do:

```xml
<!-- the contents of pair_programmer.chatmd *replaces* this import decleration -->
<import file="pair_programmer.chatmd"/>

<msg role="user">Rewrite this function so it’s tail-recursive.</msg>
```

When the CLI parses `conversation_1.chatmd` the `<import/>` is replaced with the full
content of `pair_programmer.chatmd` before the request is sent to OpenAI

### Message roles & function calling

```xml
<!-- Assistant asks the runtime to call read_dir -->
<msg role="assistant" tool_call tool_call_id="call_42" function_name="read_dir">
RAW|{"path":"./lib"}|RAW
</msg>

<!-- The runtime executes the call and returns the result: -->
<msg role="tool" tool_call_id="call_42">
RAW|
filter_file.ml
chat_response.ml
... etc ...
|RAW
</msg>
```

* The presence of the boolean `tool_call` attribute signals *“the assistant wants to run a
  tool”*.
* `tool_call_id` lets subsequent messages correlate the call and its output.

### Inline content helpers

Inside a `<msg>` body you can embed richer content that is expanded **before** the request
is sent to OpenAI:

| Tag | Effect |
|-----|--------|
| `<img src="path_or_url" [local] />` | Embeds an image. If `local` is present the file is encoded as a data-URI so the API sees it. |
| `<doc src="path_or_url" [local] [strip] />` | Inlines the *text* of a document. <br>• `local` reads from disk.  <br>• Without it the file is fetched over HTTP.<br>• `strip` removes HTML tags (useful for web pages). |
| `<agent src="prompt.chatmd" [local]> … </agent>` | Runs the referenced chatmd document as a *sub-agent* and substitutes its final answer.  Any nested content inside the tag is appended as extra user input before execution. |

#### The `<agent/>` element – running sub-conversations

An **agent** lets you embed *another* chatmd prompt as a sub-task and reuse its answer as
inline text.  Think of it as a one-off function call powered by an LLM.

• `src` is the file (local or remote URL) that defines the agent’s prompt.  
• Add the `local` attribute to read the file from disk instead of fetching over HTTP.  
• Any child items you place inside `<agent>` become *additional* user input that is appended
  to the sub-conversation *before* it is executed.

Example – call a documentation-summary agent and insert its answer inside the current
message:

```xml
<msg role="user">
  Here is a summary of the README:
  <agent src="summarise.chatmd" local>
     <doc src="README.md" local strip/>
  </agent>
</msg>
```

At runtime the inner prompt `summarise.chatmd` is executed with the stripped text of the
local `README.md` as user input, and the resulting summary is injected in place of the
`<agent>` tag.


### Raw text blocks

Occasionally you need to include text that would otherwise confuse the XML parser (code
containing angle brackets, patches, JSON, …).  Surround it with a **pipe-delimited RAW
block** – it is converted to an internal CDATA section and arrives at the model unchanged.

```
RAW|<html><body>Hello</body></html>|RAW

# short alias
raw|some <xml> without closing tags|raw
```

---

## Built-in tools

The toolkit ships with a handful of built-ins that can be enabled by simply declaring
`<tool name="…" />` in the prompt.

| Tool | Signature | Description |
|------|-----------|-------------|
| `apply_patch` | *string – git-style patch* | Applies the patch to the working tree.  Return value: success/failure message. |
| `read_dir` | `{ "path": "…" }` | Returns a newline-separated listing of the directory. |
| `get_contents` | `{ "path": "…" }` | Outputs the full contents of the file. |

Need something else?  Declare a **custom tool** that wraps any shell command:

```xml
<tool name="dune" command="dune" description="Run dune commands inside the repo" />
```

---

## Examples

See `prompt-examples/` for larger examples.  A minimal interaction including a tool call could
look like:

```xml
<config model="o3" reasoning_effort="high"/>

<!-- enable code editing, code reading, and folder content reading capability -->
<tool name="apply_patch" />
<tool name="read_dir" />
<tool name="get_contents" />

<msg role="system">You are ChatGPT, a large language model trained by OpenAI.</msg>

<msg role="user">Fix the typo in Readme.md please.</msg>
```

Run:

```sh
gpt chat-completion -output-file fix_readme.md
```

The assistant will respond with an `apply_patch` call and, if you confirm, the patch will
be applied directly to the working tree.

---

## Contributing & License

This project is distributed under the MIT license – see `LICENSE.txt`.

Bug reports, ideas and pull requests are welcome!  Please bear in mind the *research /
playground* nature of the codebase.

---

## Status

🚧 **Highly experimental** – APIs and formats may break at any time.  Use at your own
risk.

