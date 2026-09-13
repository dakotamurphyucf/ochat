# Arrays and optional values

Use `Array` to transform ordered, homogeneous data and `Option` to represent a
missing result. These modules are available on all four extensibility surfaces.
They evaluate immediately; they do not start background work or interpret tasks
returned by callbacks. Retrieve `reference.signatures` for the selected surface's
exact schemes, and [task composition](chatml-task-effects.md) for sequencing work.
The examples below are complete one-off programs, checked without host operations.

## Array operations

Arrays use `[first, second]`, zero-based indexing `values[index]`, and mutation
`values[index] <- next`. They are mutable and fixed in length. An array has one
element type; use a shared variant type for intentionally different cases. There
is no automatic list conversion, array pattern matching, or resizable push/pop API.

| Call | Meaning |
|---|---|
| `Array.length(values)` | Number of elements. |
| `Array.get(values, index)` | Element at an in-bounds index. |
| `Array.set(values, index, value)` | Replace one element; returns unit. |
| `Array.make(count, value)` | New array with every slot containing the same already evaluated value. |
| `Array.init(count, fun index -> value)` | New array; call the function once for each index from zero upward. |
| `Array.copy(values)` | New outer array containing the original element values. |
| `Array.append(left, right)` | New outer array containing left elements followed by right elements. |
| `Array.sub(values, start, length)` | New outer array containing that interval. The third argument is a length. |
| `Array.reverse(values)` | New outer array in reverse order. |
| `Array.reverse_in_place(values)` | Reverse the existing array; returns unit. |
| `Array.swap(values, first, second)` | Exchange two in-bounds elements; returns unit. |
| `Array.fill(values, value)` | Replace every slot with the same already evaluated value; returns unit. |
| `Array.map(values, fun value -> next)` | New array of callback results, in input order. |
| `Array.mapi(values, fun index value -> next)` | Map with index first and value second. |
| `Array.iter(values, fun value -> ())` | Call a unit-returning callback for each element. |
| `Array.iteri(values, fun index value -> ())` | Iterate with index first and value second. |
| `Array.fold(values, initial, fun accumulator value -> next)` | Fold left to right; return the final accumulator. Empty input returns `initial`. |
| `Array.filter(values, fun value -> predicate)` | New array of elements whose boolean predicate succeeds, preserving order. |
| `Array.exists(values, fun value -> predicate)` | Stop at the first true predicate. Empty input returns false. |
| `Array.for_all(values, fun value -> predicate)` | Stop at the first false predicate. Empty input returns true. |
| `Array.find(values, fun value -> predicate)` | First matching element as `Some(value)`, otherwise `None`. This returns a value, not its index. |
| `Array.find_map(values, fun value -> optional_result)` | Stop at the first `Some(result)` returned by the callback; otherwise return `None`. |

Callbacks run in ascending index order. Index-aware callbacks take two arguments;
do not pass an OCaml-style curried function. Allocation counts must be nonnegative.
Indexed operations require `0 <= index < length`; slices require a nonnegative,
fully contained interval. An empty slice at the end is valid. These checks raise
immediate runtime failures, not recoverable `Task.fail` outcomes.

New outer arrays are shallow copies. Nested arrays and refs remain shared, and
`make`/`fill` do not clone their value. Use `init(count, fun index -> ref(...))` to
allocate independent cells. Iteration is not a transactional snapshot: avoid
changing the traversed array from callbacks when you need a stable input.
Ordinary mutable arrays/cells are not automatically durable moderator state.

## Transform, accumulate and search

This pipeline distinguishes new arrays from mutation, preserves filter order,
checks index-aware argument order, and shows searches stopping before later
elements. The checks on empty input never invoke their predicates.

<!-- ochat-authoring-example: {"id":"collections.pipeline","surface":"one_off_v1","result":{"source":[1,2,3,4],"changed":[4,8,3,1],"selected":[8,3],"mapped":[9,4],"reversed":[1,3,8,4],"sum":16,"indexed_sum":22,"fold":"1234","found":3,"find_map":30,"visits":2,"exists":true,"all":true,"empty_exists":false,"empty_all":true,"missing":true}} -->
```ocaml
let number : int -> json = fun value -> Json.parse(to_string(value))
let numbers : int array -> json = fun values -> `Array(Array.map(values, number))
let main input =
  let source = Array.init(4, fun index -> index + 1) in
  let changed = Array.copy(source) in
  Array.set(changed, 1, 8);
  Array.reverse_in_place(changed);
  Array.swap(changed, 1, 2);
  let selected = Array.filter(Array.sub(changed, 1, 3), fun value -> value > 2) in
  let mapped = Array.map(selected, fun value -> value + 1) in
  let sum = ref(0) in
  Array.iter(changed, fun value -> sum := !sum + value);
  let indexed = Array.mapi(source, fun index value -> index + value) in
  let indexed_sum = ref(0) in
  Array.iteri(indexed, fun index value -> indexed_sum := !indexed_sum + index + value);
  let visits = ref(0) in
  let found = Array.find_map(source, fun value ->
    if value > 2 then Option.some(value * 10) else Option.none()) in
  let exists = Array.exists(source, fun value ->
    visits := !visits + 1;
    value == 2) in
  Task.pure(`Object([
    {key = "source"; value = numbers(source)},
    {key = "changed"; value = numbers(changed)},
    {key = "selected"; value = numbers(selected)},
    {key = "mapped"; value = numbers(mapped)},
    {key = "reversed"; value = numbers(Array.reverse(changed))},
    {key = "sum"; value = number(!sum)},
    {key = "indexed_sum"; value = number(!indexed_sum)},
    {key = "fold"; value = `String(Array.fold(source, "", fun acc value -> acc ++ to_string(value)))},
    {key = "found"; value = number(Option.get_or(Array.find(source, fun value -> value > 2), -1))},
    {key = "find_map"; value = number(Option.get_or(found, -1))},
    {key = "visits"; value = number(!visits)},
    {key = "exists"; value = `Bool(exists)},
    {key = "all"; value = `Bool(Array.for_all(source, fun value -> value > 0))},
    {key = "empty_exists"; value = `Bool(Array.exists([], fun value -> fail("not called")))},
    {key = "empty_all"; value = `Bool(Array.for_all([], fun value -> fail("not called")))},
    {key = "missing"; value = `Bool(Option.is_none(Array.find(source, fun value -> value > 9)))}
  ]))
```

## Shallow copies and repeated mutable elements

Replacing one copied slot leaves the original slot unchanged, but mutating a
shared cell changes every slot that still references it. `fill` also shares one
cell across slots. The independently initialized cells remain separate.

<!-- ochat-authoring-example: {"id":"collections.shallow-aliases","surface":"one_off_v1","result":"7,7;9,7;5,5;2,0;4"} -->
```ocaml
let describe cells =
  Array.fold(cells, "", fun acc cell ->
    if String.is_empty(acc) then to_string(!cell) else acc ++ "," ++ to_string(!cell))
let main input =
  let original = Array.make(2, ref(0)) in
  let copied = Array.copy(original) in
  let shared = Array.get(copied, 0) in
  shared := 7;
  Array.set(copied, 0, ref(9));
  let independent = Array.init(2, fun index -> ref(0)) in
  let first = Array.get(independent, 0) in
  first := 2;
  let filled = Array.make(2, ref(0)) in
  Array.fill(filled, ref(1));
  let second = Array.get(filled, 1) in
  second := 5;
  Task.pure(`String(describe(original) ++ ";" ++ describe(copied) ++ ";" ++
    describe(filled) ++ ";" ++ describe(independent) ++ ";" ++
    to_string(Array.length(Array.append(original, copied)))))
```

## Arrays of tasks require explicit interpretation

`map` constructs the tasks below without interpreting them. The fold constructs
one sequential task; interpreting that task runs each item in order. This does
not request parallel execution. An `iter` callback must return unit, so it cannot
serve as a task-aware iterator. Replaying the resulting task can repeat effects.

<!-- ochat-authoring-example: {"id":"collections.task-sequence","surface":"one_off_v1","result":"0:6"} -->
```ocaml
let main input =
  let total = ref(0) in
  let tasks = Array.map([1, 2, 3], fun value ->
    let* () = Task.pure(()) in
    total := !total + value;
    Task.pure(())) in
  let before = !total in
  let sequence = Array.fold(tasks, Task.pure(()), fun previous next ->
    let* () = previous in
    next) in
  let+ () = sequence in
  `String(to_string(before) ++ ":" ++ to_string(!total))
```

## Optional values and eager defaults

Options are ordinary backtick variants, `None` and `Some(value)`, without OCaml's
nominal option type. `Option.none()` constructs `None`; `Option.some(value)`
constructs `Some(value)`. `Option.is_none(option)` and `Option.is_some(option)`
return booleans. `Option.get_or(option, default)` returns the payload or default.
These five functions are the complete `Option` module; there is no implicit
`Option.map` or exception-raising `Option.get`.

The default argument is evaluated even when a value is present. To defer expensive
or effectful fallback computation, explicitly match `None` and `Some(value)`.
The branches must agree on a type. Options are not JSON values: unwrap them or
convert each case explicitly at a JSON boundary. An absent value and `Some(Null)`
are different when querying JSON.

<!-- ochat-authoring-example: {"id":"collections.eager-default","surface":"one_off_v1","result":"kept:1:kept:true"} -->
```ocaml
let main input =
  let count = ref(0) in
  let fallback () = count := !count + 1; "fallback" in
  let present = Option.some("kept") in
  let eager = Option.get_or(present, fallback()) in
  let lazy_value = match present with
    | `None -> fallback()
    | `Some(value) -> value
  in
  Task.pure(`String(eager ++ ":" ++ to_string(!count) ++ ":" ++ lazy_value ++
    ":" ++ to_string(Option.is_some(present))))
```

## Invalid allocation and ranges

Validate dynamic counts and bounds before calling these operations. Immediate
failures leave no result task to recover with `Task.catch`; see the task guide
for the distinction between an evaluator failure and a failing task.

<!-- ochat-authoring-example: {"id":"collections.invalid-allocation","surface":"one_off_v1","runtime_error":"Array.init: length must be non-negative"} -->
```ocaml
let main input =
  let values = Array.init(-1, fun index -> index) in
  Task.pure(`String(to_string(Array.length(values))))
```

<!-- ochat-authoring-example: {"id":"collections.invalid-slice","surface":"one_off_v1","runtime_error":"Array.sub: start+len out of bounds"} -->
```ocaml
let main input =
  let values = Array.sub([1, 2], 1, 2) in
  Task.pure(`String(to_string(Array.length(values))))
```
