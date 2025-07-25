# Chatmd_ast

Light-weight DOM for documents written in **ChatMarkdown** – a restricted
markup dialect used to annotate large-language-model (LLM) prompts and
responses.

---

## 1 – Purpose

`Chatmd_ast` defines the **semantic data structures** shared by the lexer
(`Chatmd_lexer`) and the Menhir grammar (`Chatmd_parser`).  Only the
whitelisted tag names officially recognised by ChatMarkdown have dedicated
constructors; any unknown markup is preserved verbatim so that downstream
tools can decide how to handle it.

```
<msg> <user> <assistant> <agent> <system> <developer>
<doc> <img> <import> <config>
<reasoning> <summary>
<tool_call> <tool_response> <tool>
```

---

## 2 – Public types and helpers

| Item | Type | Description |
|------|------|-------------|
| `tag` | variant | Closed enumeration of ChatMarkdown element names. |
| `attribute` | `(string * string option)` | Raw attribute pair. `None` stands for bare attributes such as `disabled`. |
| `node` | variant | Recursive tree – either an `Element` or raw `Text`. |
| `document` | `node list` | List of top-level nodes. |
| `tag_equal` | `tag -> tag -> bool` | Constant-time comparison ignoring future payloads. |
| `tag_of_string_opt` | `string -> tag option` | Safe conversion from lower-case tag name. |
| `tag_of_string` | `string -> tag` | Total variant; raises on unknown names. |

---

## 3 – Examples

### 3.1 Building a document programmatically

```ocaml
open Chatmd_ast

let doc : document =
  [ Element
      ( Msg,
        [ "role", Some "assistant" ],
        [ Text "Hello world!" ] ) ]

let () =
  Sexp.pp_hum Format.std_formatter (sexp_of_document doc)
```

Produces:

```
((Msg ((role (assistant))) (Hello world!)))
```

### 3.2 Parsing from source text

> See `Chatmd_parser` for the full pipeline, but in essence:

```ocaml
let parse s : Chatmd_ast.document =
  let lexbuf = Lexing.from_string s in
  Chatmd_parser.document Chatmd_lexer.token lexbuf

let ast = parse "<msg role='user'>Hi</msg>";;

(* - : Chatmd_ast.document =
   [ Element (Msg, [ ("role", Some "user") ], [ Text "Hi" ]) ] *)
```

---

## 4 – Behavioural notes

1. **Whitespace at top level** is discarded by the parser.
2. Text nodes keep **all significant characters** including new-lines – this
   is crucial when the content is fed to an LLM.
3. The AST carries **no location information**; errors are reported during
   lexing/parsing.

---

## 5 – Limitations and future work

* No support for namespaces or XML comments.
* Attribute values are stored *decoded*; round-tripping the exact original
  source therefore requires the lexer’s backing buffer.
* Tag set is hard-coded – extending the language means editing the variant
  and updating the lexer/parser accordingly.

---

## 6 – Related modules

* [`Chatmd_lexer`](./chatmd_lexer.doc.md) – converts text to tokens.
* [`Chatmd_parser`](./chatmd_parser.doc.md) – builds the AST from tokens.
* [`Prompt`](./prompt.mli) – high-level operations on prompt documents.

