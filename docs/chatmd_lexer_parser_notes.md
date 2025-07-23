
# ChatMD Lexer & Parser – Quick Reference

This document is an **internal research artefact** generated while executing
the *ChatMD deep-dive* work-stream (see `plan.md`).  It summarises the
low-level tokens emitted by `lib/chatmd/chatmd_lexer.mll` and the context-free
grammar accepted by `lib/chatmd/chatmd_parser.mly`.

> NOTE  This is **not** the final language specification – the polished
> `docs/chatmd_spec.md` will be produced in a later ticket once the research
> stream completes.  The present file merely captures facts discovered by
> reading the source code so that follow-up tasks can build on reliable data
> without having to re-inspect the OCaml files.

---

## 1. Token stream (output of the lexer)

`Chatmd_lexer.token : Lexing.lexbuf -> Chatmd_parser.token`

The lexer is *shallow* and only recognises the **official ChatMarkdown tags**
listed below.  Everything else is returned verbatim in a `TEXT` token (or, in
the case of properly balanced unknown elements, collapsed into a single
`TEXT`).

### 1.1 Recognised tags

```
<msg> <user> <assistant> <agent> <system> <developer>
<doc> <img> <import> <config>
<reasoning> <summary>
<tool_call> <tool_response> <tool>
```

### 1.2 Token constructors

| Token | Payload | Meaning |
|-------|---------|---------|
| `START    (tag, attrs)` | opening tag `<tag …>` | Pushes `tag` onto the parser’s stack. |
| `SELF     (tag, attrs)` | self-closing `<tag …/>` | Emits an AST element with no children. |
| `END      tag`          | closing tag `</tag>`   | Pops and checks that it matches the opener. |
| `TEXT     string`       | decoded character data | May contain embedded unknown markup. |
| `EOF`                   | – | End-of-input sentinel. |

`attrs` is an `(attribute list)` where `attribute = string * string option`.

Additional lexer features:

* `RAW| … |RAW` delimits *verbatim* blocks which are emitted as **one**
  `TEXT` token regardless of internal contents.
* A single-token look-ahead buffer (`pending_token`) allows a rule to return
  two logical tokens without exposing buffering to the caller.
* Basic HTML entity decoding (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`).

---

## 2. Context-free grammar (Menhir)

Entry point: `document`

```
document  ::= rec_elems EOF                      ➜ Chatmd_ast.document

rec_elems ::= ε                                 ➜ []
            | rec_elems whitespace              ➜ $1
            | rec_elems rec_elem                ➜ $1 @ [$2]

whitespace ::= TEXT  (* provided it is all WS *)

rec_elem  ::= SELF                              ➜ Element(tag, attrs, [])
            | START children END                ➜ Element(tag, attrs, children)

children  ::= ε                                 ➜ []
            | children child                    ➜ $1 @ [$2]

child     ::= text_block                        ➜ Text(string)
            | SELF                              ➜ Element(tag, attrs, [])
            | START children END                ➜ Element(tag, attrs, children)

text_block ::= TEXT
            | text_block TEXT                   ➜ string concatenation
```

### 2.1 Notes

* Consecutive `TEXT` tokens are **collapsed** – e.g. the literal `"a < b"`
  becomes a single `Text("a < b")` node rather than three separate ones.
* Leading / inter-element whitespace at the top level is discarded by the
  `whitespace` rule.
* Mismatching open/close tags raise `Failure` via the helper `tag_mismatch`.

---

## 3. Operator precedence & associativity

The grammar does not rely on traditional operator precedence.  Element nesting
is driven purely by matching `START … END` pairs in the token stream.

---

## 4. Observations & next steps

* The language is intentionally minimal – no namespaces, PI, comments, or
  DTDs are recognised.
* Unknown markup is preserved but *flattened* so that downstream stages never
  see half-broken trees.
* The upcoming *specification task* should emphasise the **RAW blocks** and
  the **attribute decoding semantics**, as they are the only places where the
  implementation diverges from naïve XML.

---

_Generated on: <!--DATE-->_

