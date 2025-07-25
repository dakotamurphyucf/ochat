# Chatmd_lexer

Lexical analyser for the **ChatMarkdown** language.

---

## 1 – Purpose

`Chatmd_lexer` converts a UTF-8 text stream into the token type defined in
`Chatmd_parser`.  The lexer recognises exactly the set of *official*
ChatMarkdown tags and nothing more:

```
<msg> <user> <assistant> <agent> <system> <developer>
<doc> <img> <import> <config>
<reasoning> <summary>
<tool_call> <tool_response> <tool>
```

Tags outside that list are left untouched so that downstream components
can decide how to interpret them.

The implementation is intentionally shallow – it does **not** attempt
full XML/HTML validity checking – yet guarantees that no information is
lost during lexing.

---

## 2 – Public API

| Value | Type | Description |
|-------|------|-------------|
| `Chatmd_lexer.token` | `Lexing.lexbuf -> Chatmd_parser.token` | Returns the next token. Uses an internal one-element buffer so that rules may emit two logical tokens without changing the signature. |

Helper values such as `parse_attrs` or `decode_entities` are exposed only
for unit-tests; they can change without notice.

---

## 3 – Behavioural details

### 3.1 Attribute parsing

* Accepted name syntax: `[A-Za-z0-9_:-]+`
* Values can be wrapped in `'` or `"`.
* Backslash escaping inside quoted values is honoured.
* The entities `&amp;` `&lt;` `&gt;` `&quot;` and `&apos;` are decoded.

Example:

```ocaml
# Chatmd_lexer.parse_attrs "role='assistant' disabled alt='A &amp; B'";;
- : Chatmd_ast.attribute list =
[ ("role", Some "assistant");
  ("disabled", None);
  ("alt", Some "A & B") ]
```

### 3.2 RAW blocks

`RAW|` … `|RAW` delimiters mark verbatim sections.  The entire enclosed
text – including any nested tags – is collapsed into a single `TEXT`
token so that clients can embed arbitrary markup without the lexer
intervening.

```
msg RAW|<span style="color:red">raw</span>|RAW
```

Generates the sequence:

```
TEXT "msg "
TEXT "<span style=\"color:red\">raw</span>"
```

### 3.3 Unknown tags

Balanced unknown elements are *not* removed – they are returned as raw
`TEXT` tokens.  This behaviour means that a structure such as

```html
<msg>hello <b>world</b></msg>
```

is tokenised as

```
START (Msg, [])
TEXT "hello "
TEXT "<b>"
TEXT "world"
TEXT "</b>"
END Msg
EOF
```

---

## 4 – Error handling

| Condition | Exception | Message |
|-----------|-----------|---------|
| Unterminated attribute value | `Failure` | *Unterminated quoted attribute value …* |
| Unmatched `RAW|` / `|RAW` | `Failure` | *Unterminated raw block* |
| Any other unexpected character | `Failure` | *Unexpected char X at line:col* |

---

## 5 – Example usage

```ocaml
open Core

let () =
  let input = "<msg role=\"user\">hello RAW|<b>raw</b>|RAW</msg>" in
  let lexbuf = Lexing.from_string input in
  let rec dump () =
    match Chatmd_lexer.token lexbuf with
    | Chatmd_parser.EOF -> ()
    | tok ->
        printf "%s\n" (Sexp.to_string_hum (Chatmd_parser.sexp_of_token tok));
        dump ()
  in
  dump ()
```

---

## 6 – Limitations

* Tag names are **case-sensitive** – `<MSG>` is not recognised.
* Only five HTML entities are decoded.
* No namespace support.

---

## 7 – Related modules

* `Chatmd_ast` – type definitions for tags, nodes and documents.
* `Chatmd_parser` – Menhir grammar that builds the AST from tokens.

