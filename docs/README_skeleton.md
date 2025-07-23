# Project README – Skeleton (Auto-generated)

> _This file is a **template** produced by the research tasks.  Replace
> the `TODO:` markers with the final content when assembling the public
> README._

---

## 0  Badge strip

TODO: CI / coverage / version badges

---

## 1  Introduction

TODO: <one-paragraph elevator pitch>

* Why does the project exist?
* Key use-cases.

## 2  Quick-start

```sh
opam switch create .
dune build
gpt --help
```

## 3  Architecture overview

![Architecture diagram](docs/architecture.svg)

High-level description:

* Core runtime – embeddings, OAuth, MCP, search.
* Domain layer – ChatMD & ChatML.
* Front-ends – CLI, TUI, HTTP server.

## 4  CLI Tools

| Command | Purpose | Help output |
|---------|---------|-------------|
| `gpt chat-completion` | Run a chatmd session | `out/help/chat-completion.txt` |
| … | … | … |

## 5  ChatMD language

See `docs/chatmd_syntax_reference.md` and examples under
`examples/chatmd/`.

## 6  ChatML language (experimental)

See `docs/chatml_overview.md`.

## 7  Embedding & search stack

Flow diagram: `docs/seq_embedding.md`  
Performance numbers: `docs/vector_db_indexing.md`

## 8  OAuth2 helper stack

Overview: `docs/oauth2_overview.md`

## 9  MCP protocol

Overview: `docs/mcp_protocol.md`

## 10  Contributing

TODO: coding style, how to run tests, generate docs.

## 11  License

MIT – see `LICENSE.txt`.

---

*Last updated: {{date}}*

