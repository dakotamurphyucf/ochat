# Task composition and failure boundaries

Use `Task` to sequence computations and selected host operations. A task is a
description interpreted by the Ochat runtime; it is not a promise that has already
started running. The five operations below are available on all four versioned
extensibility surfaces. Host operations still depend on the selected surface,
installed services and actual tool permissions. See the
[execution contracts](chatml-authoring-runtime.md) for the required entrypoint.

| Operation | Meaning |
|---|---|
| `Task.pure(value)` | Construct a successful task containing an already evaluated value |
| `Task.bind(task, next)` | Interpret the first task, then call `next(value)` and interpret its returned task |
| `Task.map(task, transform)` | Interpret the task, then return the ordinary value from `transform(value)` |
| `Task.fail(message)` | Construct a task that fails when interpreted; the message is a string |
| `Task.catch(task, recover)` | On a task failure, call `recover(message)` and interpret its replacement task |

`let* value = task in next_task` is the readable syntax for bind.
`let+ value = task in result` is the syntax for map. Mapping to another task
produces a nested task value; it does not flatten or execute that inner task.
Use bind when the next computation returns a task. These constructs sequence
work; they do not implicitly run branches in parallel or create a background job.

The examples are complete `one_off_v1` programs with null input. The offline
documentation gate executes them without any host operations installed.

<!-- ochat-authoring-example: {"id":"task-effects.reuse","surface":"one_off_v1","result":"1,2"} -->
```ocaml
let main input =
  let counter = ref(0) in
  let plan =
    let* () = Task.pure(()) in
    counter := !counter + 1;
    Task.pure(!counter)
  in
  let* first = plan in
  let+ second = plan in
  `String(to_string(first) ++ "," ++ to_string(second))
```

Reusing a task value interprets it again. It does not memoize a result or give
exactly-once external effects. In this example the mutation occurs inside the
bind callback. An expression passed directly to `Task.pure` evaluates while
constructing that task instead.

<!-- ochat-authoring-example: {"id":"task-effects.nested-map","surface":"one_off_v1","result":"flattened"} -->
```ocaml
let main input =
  let nested = Task.map(Task.pure("flattened"), fun text -> Task.pure(text)) in
  let* inner = nested in
  let+ text = inner in
  `String(text)
```

`Task.catch` skips its handler on success. On failure it receives the message,
not a typed exception object. Bind/map callbacks after the failed computation
are skipped until recovery. A failing recovery task can be caught by an outer
catch; the same handler is not recursively retried.

<!-- ochat-authoring-example: {"id":"task-effects.nested-recovery","surface":"one_off_v1","result":"outer:inner"} -->
```ocaml
let main input =
  Task.catch(
    Task.catch(
      (let* value = Task.fail("inner") in Task.pure(`String("unreachable"))),
      fun message -> Task.fail("outer:" ++ message)),
    fun message -> Task.pure(`String(message)))
```

Catch is not a general exception boundary around arbitrary ChatML evaluation.
An eager error can occur before a catch task exists. A pure error raised inside
a bind/map callback also exits the current interpreter invocation rather than
being converted to `Task.fail`. Return a failing task for recoverable workflow
errors. Do not rely on catch to bypass cancellation or execution limits.

<!-- ochat-authoring-example: {"id":"task-effects.eager-error","surface":"one_off_v1","runtime_error":"before catch"} -->
```ocaml
let main input =
  Task.catch(Task.pure(fail("before catch")),
    fun message -> Task.pure(`String("unexpected recovery")))
```

<!-- ochat-authoring-example: {"id":"task-effects.callback-error","surface":"one_off_v1","runtime_error":"inside map"} -->
```ocaml
let main input =
  Task.catch(Task.map(Task.pure(0), fun value -> fail("inside map")),
    fun message -> Task.pure(`String("unexpected recovery")))
```

Recovery restores host-managed transactional effects staged inside that catch
scope, including their registered rollback actions. It does not undo an already
performed external tool/shell effect, restore arbitrary mutable cells or restart
an interrupted external action. Keep durable workflow state in the serializable
moderator state or owned runtime records, and use stable operation identities
when retrying effects. See [background recovery](chatml-authoring-background.md)
and [persisted child receipts](chatml-authoring-children.md).

<!-- ochat-authoring-example: {"id":"task-effects.mutable-cell-not-rollback","surface":"one_off_v1","result":"changed"} -->
```ocaml
let main input =
  let state = ref("initial") in
  Task.catch(
    (let* () = Task.pure(()) in
     state := "changed";
     Task.fail("recover")),
    fun message -> Task.pure(`String(!state)))
```
