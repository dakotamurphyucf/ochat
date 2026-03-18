## ChatML match semantics

This note documents the intended behavior of `match` in ChatML as of the
current typechecker / resolver pipeline.

### Overview

ChatML pattern matching is intentionally small and ML-flavored. It supports:

- wildcard patterns: `_`
- variable binders: `x`
- literal patterns:
  - integers
  - booleans
  - floats
  - strings
- polymorphic variant patterns:
  - `` `Tag ``
  - `` `Tag(p1, ..., pn) ``
- record patterns:
  - `{ field = pat; ... }`
  - `{ field = pat; _ }` for open-row matching

Pattern matching is checked in three distinct ways:

1. **pattern well-formedness**
   - duplicate binders inside one pattern are rejected
2. **redundancy checking**
   - obviously unreachable arms are rejected
3. **exhaustiveness checking**
   - finite closed matches must cover all cases
   - open or infinite matches require `_` unless ChatML can prove a pattern is total

### Runtime semantics

At runtime, arms are tested in source order and the first matching arm wins.
Pattern variables bind in left-to-right order.

Record patterns are structural:

- `{ name = n }` matches only a record with exactly that field set when used
  as a closed pattern
- `{ name = n; _ }` matches any record with at least a `name` field

Variant patterns match by constructor name and arity.

### Static semantics

#### 1. Binder validation

ChatML rejects patterns that bind the same variable name more than once in a
single arm.

Examples rejected:

```chatml
match `Pair(1, 2) with
| `Pair(x, x) -> x
```

```chatml
match {left = 1; right = 2} with
| {left = x; right = x} -> x
```

#### 2. Redundant arm checking

ChatML rejects arms that are already covered by earlier arms.

Examples:

```chatml
match 1 with
| _ -> 0
| 1 -> 1
```

```chatml
match true with
| true -> 1
| false -> 0
| _ -> 2
```

For closed finite variants, ChatML also rejects:

- a wildcard arm after all constructors are already covered
- a constructor arm after an earlier arm already covers that constructor

#### 3. Exhaustiveness checking

ChatML is deliberately conservative:

##### Fully checked

- `bool`
- closed finite polymorphic variants

##### Conservatively checked

- `int`
- `float`
- `number`
- `string`
- records
- open variants

For these cases, ChatML generally requires `_` unless it can prove one of the
existing patterns is total.

### Variant closure by match

An important design choice is that an exhaustive variant match can **close**
an otherwise open variant row.

For example:

```chatml
let f v =
  match v with
  | `None -> 0
  | `Some(x) -> x
```

After this match is proven exhaustive, `f` is treated as accepting only those
two constructors. A later call such as:

```chatml
f(`Other)
```

is rejected by the typechecker.

This behavior is intentional: it lets ChatML keep row-polymorphic variants for
ergonomics while still getting useful finite exhaustiveness checks once a match
really commits to a closed constructor set.

### Records and totality

ChatML does not attempt full record-pattern exhaustiveness today. That problem
becomes subtle once row polymorphism and nested patterns are involved.

Instead, the checker recognizes obvious total patterns:

- `_`
- variable binders
- open record patterns whose subpatterns are themselves total

Example:

```chatml
match r with
| {name = n; _} -> n
```

If `r` is known to have a `name` field, this is treated as exhaustive.

### Diagnostics

Match diagnostics aim to point at the specific problematic arm whenever one
exists:

- duplicate or redundant arm diagnostics point at that arm’s pattern span
- pattern-typing failures inside an arm point at that arm’s pattern span
- non-exhaustive diagnostics still point at the whole `match`, because there
  is no single failing arm to highlight

This split is intentional and keeps diagnostics accurate without requiring the
full pattern AST to carry source spans at every nested node.

### Non-goals

ChatML does **not** currently attempt:

- full ML-style usefulness analysis for all nested patterns
- complete record-pattern coverage checking
- exhaustive checking for open variant rows without `_`
- pattern-level source spans for every nested subpattern

Those may be added later if the language grows, but today the implementation
optimizes for simple, predictable behavior in scripting use-cases.
