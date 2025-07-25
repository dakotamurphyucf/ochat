# Chatmd_parser

Menhir‐generated parser for the **ChatMarkdown** language.

---

## 1 – Purpose

`Chatmd_parser` turns the token stream produced by
[`Chatmd_lexer`](./chatmd_lexer.doc.md) into the lightweight DOM defined
in [`Chatmd_ast`](../chatmd_ast.ml).

At the moment the grammar recognises **only** the *official* set of
ChatMarkdown tags:

```
<msg> <user> <assistant> <agent> <system> <developer>
<doc> <img> <import> <config>
<reasoning> <summary>
<tool_call> <tool_response> <tool>
```

Any unknown XML/HTML that appears *inside* one of the recognised tags is
passed through as plain text so that downstream components can decide
how to handle it.

---

## 2 – Public API

| Value | Type | Description |
|-------|------|-------------|
| `Chatmd_parser.document` | `(Lexing.lexbuf -> Chatmd_parser.token) -> Lexing.lexbuf -> Chatmd_ast.document` | Parses the entire buffer. Expects a lexer function (typically `Chatmd_lexer.token`). |

### 2.1 Error conditions

| Scenario | Exception | Message |
|----------|-----------|---------|
| `<msg>` closed with `</user>` | `Failure` | *Mismatching tags: <msg> … </user>* |
| Non-whitespace text at top level | `Failure` | *Unexpected text at top level: …* |

---

## 3 – Behavioural details

### 3.1 Whitespace handling

• Pure whitespace between top-level elements is ignored.

• Whitespace *inside* an element is preserved because it is delivered as
  part of a `Text` node.

### 3.2 Text collapsing

Consecutive `TEXT` tokens are concatenated into one `Chatmd_ast.Text`
node:

```text
"a < b"     => Text "a < b"
```

instead of three separate pieces (`"a "`, `"<"`, `" b"`).

### 3.3 Unknown tags

Balanced unknown elements are not parsed – they are already collapsed
into a single `TEXT` token by the lexer, guaranteeing that no
information is lost.

---

## 4 – Example usage

```ocaml
open Core

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Chatmd_parser.document Chatmd_lexer.token lexbuf

let () =
  let ast = parse_string "<msg><user>hello</user></msg>" in
  Sexp.pp_hum Format.std_formatter (Chatmd_ast.sexp_of_document ast)
```

---

## 5 – Performance considerations

Menhir is invoked with the `--table` flag.  The generated LR automaton
is loaded lazily, keeping the initial start-up cost low.  Overall time
complexity is `O(n)` in the size of the token stream, and the parser
allocates only the minimal structure required to represent the AST.

---

## 6 – Limitations & future work

* The parser does not attempt to validate attribute well-formedness.
  Malformed attributes are caught earlier by the lexer.
* Unknown tags are silently accepted as raw text.  This is a
  deliberate design choice to allow embedding arbitrary XML in ChatMarkdown
  documents for inclusion in the LLM prompt.

