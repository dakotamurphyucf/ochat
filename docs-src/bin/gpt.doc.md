# ochat – Command-line swiss army knife for Ochat & code search

`ochat` is an opinionated collection of developer utilities that sit on
top of the internal *ochat* OCaml libraries.  The binary groups
several **sub-commands** under one roof and is therefore closer in
spirit to `git`, `hg` or `opam` than to a single-purpose tool.

``console
$ ochat <sub-command> [OPTIONS]

Available sub-commands:
  chat-completion      Stream an assistant reply from OpenAI based on a chatmd prompt
  index                Build a dense-vector + BM25 index from OCaml sources
  query                Run natural-language retrieval over a previously created index
  tokenize             Count Tikitoken tokens in a file
  html-to-markdown     Convert an HTML page to Markdown and display snippet chunks
  h2md                 Alias for html-to-markdown
```

---

## 1  Synopsis

### 1.1  Chat completions

```console
$ ochat chat-completion -prompt-file prompt.chatmd -output-file session.md
```

Appends the contents of *prompt.chatmd* to *session.md* (creating the
latter if necessary) and then streams the assistant’s reply via the
OpenAI Chat Completion API.

### 1.2  Indexing OCaml sources

```console
$ ochat index -folder-to-index ./lib -vector-db-folder ./_index
```

1. Walks *./lib* recursively.
2. Extracts `(** … *)` and `(*| … |*)` documentation blocks from every
   `.ml`/`.mli` file.
3. Splits the text into ≈200-token snippets.
4. Requests embeddings from OpenAI and normalises them.
5. Persists:

```
_index/
├─ vectors.mli.binio
├─ vectors.ml.binio
├─ bm25.mli.binio
└─ bm25.ml.binio
```

### 1.3  Querying an index

```console
$ ochat query -vector-db-folder ./_index -query-text "tail-recursive map" -num-results 3
```

Returns the 3 best-matching documentation snippets ranked by
`cosine ⊕ BM25` similarity.  Each hit is printed inside an
<code>```ocaml</code> fence so you can pipe the output into Markdown
consumers.

### 1.4  Token budgeting

```console
$ ochat tokenize -file path/to/prompt.md
tokens: 1785
```

Counts how many *cl100k_base* tokens the prompt would occupy once sent
to OpenAI.

### 1.5  HTML → Markdown conversion

```console
$ ochat html-to-markdown -file tutorial.html
```

Prints the Markdown rendering and the chunk boundaries discovered by
`Odoc_snippet.Chunker`.  Useful when tuning the snippet extraction
logic used by the indexer.

---

## 2  Configuration flags

All sub-commands accept `-help`/`--help` (provided by
`Core.Command`).  The table below lists only the most important flags—
run `ochat help SUBCOMMAND` for the exhaustive reference.

| Sub-command | Flag | Default | Purpose |
|-------------|------|---------|---------|
| index  | `-folder-to-index`     | `./lib`      | Path scanned for OCaml sources |
|       | `-vector-db-folder`   | `./vector`   | Directory that will receive the corpus files |
| query | `-vector-db-folder`   | `./vector`   | Location of the previously generated corpus |
|       | `-query-text`         | *(none)*     | Natural-language search string |
|       | `-num-results`        | `5`          | Maximum number of snippets printed |
| chat-completion | `-prompt-file` | *(none)* | Template prepended once to the output file |
|                | `-output-file` | `./prompts/default.md` | Transcript destination |
| tokenize | `-file` | `bin/main.ml` | File to encode |
| html-to-markdown | `-file` | `bin/main.ml` | HTML document to convert |

---

## 3  Exit codes

| Code | Meaning |
|------|---------|
| 0 | Successful completion |
| ≠0 | Unhandled OCaml exception (inspect stderr for back-trace) |

---

## 4  Limitations & future work

* **Single-threaded retrieval** – only the indexer uses multiple domains;
  the `query` command loads everything in one process.
* **In-memory corpus** – `Vector_db` keeps the entire matrix resident;
  large code-bases (>100 k snippets) may exceed available RAM.
* **Hard-coded OpenAI models** – switching to a newer embedding or
  chat model requires code changes at present.
* **Sparse flag set** – many configuration knobs exposed by the helper
  libraries (temperature, top-p, β, chunk size, …) are not yet surfaced
  at the CLI level.

Pull requests addressing any of the above are warmly welcome.

---

## 5  See also

* [`Indexer`](../lib/indexer.doc.md) – background on the indexing pipeline
* [`Vector_db`](../lib/vector_db.doc.md) – cosine similarity & hybrid search engine
* [`Bm25`](../lib/bm25.doc.md) – lexical ranking component
* [`Chat_response.Driver`](../lib/chat_response.doc.md) – chatmd runtime
* [`Tikitoken`](https://github.com/openai/tiktoken) – reference Python implementation of the tokenizer

