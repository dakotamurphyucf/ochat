# `Chatml_parser` — ChatML grammar and AST builder

This file complements the inline odoc comments embedded in
`chatml_parser.mly` with a richer, prose-oriented introduction to the
parser.  It targets two kinds of readers:

* **End-users** who wish to embed ChatML in their own application and
  need a quick reference to the available parsing APIs.
* **Contributors** looking to extend or debug the grammar.


---

## 1. High-level overview

`Chatml_parser` is generated by **Menhir** from
[`chatml_parser.mly`](./chatml_parser.mly).  It consumes a token stream
emitted by `Chatml_lexer` and produces the abstract syntax tree (AST)
defined in `Chatml_lang`.

The grammar is deliberately *ML-flavoured* so that OCaml users feel
instantly at home.  It supports the following syntactic families:

* **Literals** — integers, floats, strings and booleans.
* **Identifiers** — lower-case, upper-case and polymorphic variant
  tags (<code>`Foo</code>).
* **Arithmetic / comparison operators** — `+`, `-`, `*`, `/`, `<`,
  `>`, `<=`, `>=`, `==`, `!=`.
* **First-class functions** — `fun x y -> …`, multi-argument
  applications, nested lambdas.
* **Pattern-matching** — `match … with` and polymorphic variants.
* **Let-bindings** — recursive (`let rec f = …`) and non-recursive,
  including *let-in* expressions.
* **Records & arrays** — literals, field access `expr.field`, updates
  and immutable extensions `{ base with a = 1 }`.
* **Imperative primitives** — references (`ref`, `!`, `:=`) and while
  loops.

Every syntactic construct maps *directly* onto its own AST constructor
so that later compilation phases can pattern-match cleanly without
having to desugar strings or ad-hoc tags.


---

## 2. Public interface

Assuming you build the grammar with the flags shown in the
[dune](./dune) file (`--table` for incremental parsing), Menhir will
generate the following OCaml signatures:

```ocaml
val program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Chatml_lang.stmt_node list

module Incremental : sig
  val program : Lexing.position -> (token, Chatml_lang.stmt_node list) MenhirLib.IncrementalEngine.checkpoint
  (* …and many helper functions; see MenhirLib.Engine for details. *)
end

type token =
  | INT of int | FLOAT of float | BOOL of bool | STRING of string
  | LIDENT of string | UIDENT of string | TICKIDENT of string
  | FUN | IF | THEN | ELSE | WHILE | DO | DONE | LET | IN | MATCH | WITH
  | MODULE | STRUCT | END | OPEN | REF | REC | AND
  | ARROW | LEFTARROW | COLONEQ | BANGEQ | EQ | EQEQ | LTEQ | GTEQ | LT | GT
  | PLUS | MINUS | STAR | SLASH | LPAREN | RPAREN | UNDERSCORE | LBRACE | RBRACE
  | LBRACKET | RBRACKET | SEMI | COMMA | DOT | BAR | BANG | EOF
```

### 2.1 `program`

Monolithic, blocking API.  Consumes *all* remaining tokens supplied by
the lexer and returns a list of top-level statements.  Raises
`MenhirLib.Error` (wrapped in `Failure` if you enabled the compatibility
layer) on the first syntax error.

**Example** — parsing a file in one go:

```ocaml
let parse_file filename : Chatml_lang.stmt_node list =
  In_channel.with_open_text filename @@ fun ic ->
  let lexbuf = Lexing.from_channel ic in
  try
    Chatml_parser.program Chatml_lexer.token lexbuf
  with Failure msg ->
    eprintf "Syntax error: %s\n" msg; raise Exit
```

### 2.2 `Incremental.program`

Entry point for **incremental** parsing.  It returns a *checkpoint*
value that you feed one token at a time via `MenhirLib.Engine` until it
reaches the `InputNeeded` or `Accepted` state.  This style is preferred
by IDE integrations because it facilitates advanced error recovery and
code completion.

**Example** — running the table-driven engine:

```ocaml
open MenhirLib

let parse_incrementally lexbuf : Chatml_lang.stmt_node list =
  let supplier = LexerUtil.make_supplier Chatml_lexer.token lexbuf in
  let checkpoint = Chatml_parser.Incremental.program lexbuf.lex_curr_p in
  Engine.stream_checkpoint checkpoint supplier
```


---

## 3. Extending the grammar

1. Add your token(s) to the lexer — see `chatml_lexer.mll`.
2. Declare them in `chatml_parser.mly` under **section 1**.
3. Insert the parsing rules in the appropriate precedence group.
4. Extend the AST (`chatml_lang.ml`) *if* a new constructor is needed.
5. Remember to update the **resolver** and **type-checker** phases so
   that they handle the new nodes.

Running `dune build` will regenerate `chatml_parser.ml` and its tables.


---

## 4. Known issues / limitations

* **Unicode** — Identifiers are ASCII-only for now.
* **Layout** — No off-side rule à la Haskell; semicolons (or `;;` at
  the toplevel) are mandatory between statements.
* **Error messages** — We rely on Menhir’s default error handling which
  can be somewhat terse.  A dedicated *explain-why* table would be a
  welcome addition.



