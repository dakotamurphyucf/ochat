# `Meta_prompting.Aggregator`

Helpers for **collapsing a list of judge scores into a single scalar**.

An *aggregator* is a plain OCaml value

```ocaml
type t = float list -> float
```

which means it can be stored, composed, or partially applied like any
other first-class function.  All strategies shipped with OChat are **total** –
they handle the empty list by returning `0.` so you never have to
special-case a missing set of votes.

---

## Built-in strategies

| Strategy | Description | Empty input |
|----------|-------------|-------------|
| `mean` | Arithmetic mean. | `0.` |
| `median` | 50th percentile. | `0.` |
| `trimmed_mean ~trim` | Drops the lowest and highest `trim` fraction before computing the mean.  `trim` ∈ `[0.,0.5)`. | `0.` |
| `weighted ~weights` | Weighted arithmetic mean.  Falls back to `mean` if lengths differ or to `0.` if the sum of weights is `0.`. | `0.` |
| `min` | Minimum. | `0.` |
| `max` | Maximum. | `0.` |

### Notes

* All functions treat **`NaN` and `∞` the same as any other float** – no
  attempt is made to sanitise the inputs.
* A trimmed mean with `trim = 0.` is equivalent to `mean` but avoids the
  overhead of creating an intermediate list.

---

## Usage examples

### Simple average

```ocaml
open Meta_prompting

let overall = Aggregator.mean [ 0.8; 1.0; 0.5 ]
(* overall = 0.7666… *)
```

### Weighted average

```ocaml
let agg = Aggregator.weighted ~weights:[ 0.5; 1.0; 1.5 ] in
let score = agg [ 0.3; 0.6; 0.9 ]
(* score = 0.675 *)
```

### Trimmed mean

```ocaml
let agg = Aggregator.trimmed_mean ~trim:0.25 in
let score = agg [ 0.9; 0.1; 0.2; 0.8 ]
(* Drops one element from each end → mean of [0.2; 0.8] = 0.5 *)
```

---

## Limitations & future work

* Aggregation is **purely statistical** – the module does not enforce any
  constraints on the value range or semantic meaning of the scores.
* For very large lists the implementation allocates a sorted copy (for
  `median` & `trimmed_mean`) which may incur an `O(n log n)` cost.  This
  is acceptable for typical meta-prompting workloads but could be
  replaced with selection algorithms if needed.
* `weighted` silently degrades to `mean` when lengths differ.  While this
  prevents mismatched arrays from going unnoticed, it may hide bugs; an
  alternative would be to raise.

