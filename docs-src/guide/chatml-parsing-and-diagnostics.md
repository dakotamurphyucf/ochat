# ChatML parsing and diagnostics

This guide explains:

- how ChatML source text becomes an AST
- what parse API to use
- what precedence rules the parser currently implements
- what kinds of syntax/lexer diagnostics are available

It is complementary to:

- `docs-src/guide/chatml-language-spec.md`
- `docs-src/guide/chatml-implementation-architecture.md`

---

## 1. Recommended parse entry point

Most callers should prefer:

```ocaml
Chatml_parse.parse_program : string -> (Chatml_lang.program, Chatml_parse.diagnostic) result
```

rather than calling `Chatml_parser.program` directly.

Why:

- it wraps the raw Menhir/ocamllex pipeline
- it returns a structured diagnostic
- it attaches the original source text to the resulting `program`

If you want exception-style behavior, use:

```ocaml
Chatml_parse.parse_program_exn : string -> Chatml_lang.program
```

---

## 2. What the parser produces

The parser produces the **source AST**, not the resolved evaluator AST.

That means parsed programs still contain source-level forms like:

- `EVar`
- `ELambda`
- `ELetIn`
- `ELetRec`
- `EMatch`

Later phases are responsible for:

- type inference
- lexical-address resolution
- let-block lowering
- slot selection

---

## 3. Error sources

There are two main front-end failure classes before type checking:

### 3.1 Parser errors

These come from Menhir when the token stream cannot be reduced according to
the grammar.

The structured parse wrapper reports them as:

- message: `"Syntax error"`
- span: current lexer location

### 3.2 Lexer errors

These come from `chatml_lexer.mll` as `Failure` exceptions, for example:

- unknown character/token
- unterminated comment
- unterminated string literal

The structured parse wrapper preserves the lexer message and attaches the
current lexer span.

---

## 4. Diagnostic formatting

`Chatml_parse.format_diagnostic` formats parse diagnostics using the same
general style as the ChatML typechecker and runtime:

- line/character range
- source excerpt
- caret indicator
- message

This makes parser, typechecker, and runtime errors feel consistent even
though they originate in different phases.

---

## 5. Precedence overview

The parser currently uses precedence tiers roughly like this:

1. comparisons and equality
2. additive operators
3. multiplicative operators
4. dereference handling

This means:

- `*`, `/`, `*.`, `/.` bind tighter than `+`, `-`, `++`, `+.`, `-.`
- comparisons/equality bind looser than arithmetic
- dereference (`!`) has its own handling in the grammar

When in doubt, use parentheses.

---

## 6. Important syntax notes

### 6.1 Function calls use explicit syntax

ChatML uses:

```chatml
f(x)
f(x, y)
```

not:

```chatml
f x
```

### 6.2 Field-call sugar

The parser supports:

```chatml
obj.method(arg1, arg2)
```

which is parsed as:

```chatml
(obj.method)(arg1, arg2)
```

It is not a separate method-dispatch system.

### 6.3 Records vs record update

These are different forms:

```chatml
{ a = 1; b = 2 }
{ rec_expr with a = 3 }
```

### 6.4 Variants use backtick tags

Examples:

```chatml
`Done
`Some(1)
`Pair(1, "x")
```

### 6.5 Patterns are a restricted syntax

Patterns do not support arbitrary expressions. They only support the forms
documented in the language spec.

---

## 7. Known current limitation

The parse wrapper now gives structured diagnostics, but parser messages are
still intentionally simple.

Today, most parser errors are reported as:

- `"Syntax error"` for grammar failures
- a lexer-provided failure string for lexical problems

There is not yet a richer “expected token” user-facing message layer on top
of Menhir.

---

## 8. Recommended calling pattern for embedders

If you are embedding ChatML, the recommended pipeline is:

1. `Chatml_parse.parse_program`
2. `Chatml_typechecker.check_program`
3. `Chatml_resolver.run_program` or:
   - `Chatml_resolver.resolve_checked_program`
   - `Chatml_eval.eval_program`

This keeps parse, type, resolution, and runtime failures separated and
structured.
