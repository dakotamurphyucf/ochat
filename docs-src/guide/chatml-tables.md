# String-keyed mutable tables

Use `Hashtbl` for small maps, grouping, counters or local lookup state. Its
exports are available on all four extensibility surfaces and execute immediately.
Keys are case-sensitive strings and values share one inferred type. The examples
are complete one-off programs checked without tools or provider calls.

| Call | Meaning |
|---|---|
| `Hashtbl.create()` | Create a new empty mutable table; value type is inferred from its uses. |
| `Hashtbl.set(table, key, value)` | Replace the value at an existing key, or insert a missing key; returns unit. |
| `Hashtbl.get(table, key)` | `Some(value)` for a present key, otherwise `None`. |
| `Hashtbl.mem(table, key)` | Whether that exact string key is present. |
| `Hashtbl.to_array(table)` | Fresh shallow array of `{key; value}` entries in insertion order. |
| `Hashtbl.remove(table, key)` | Remove the key; a missing key is a no-op. Returns unit. |

This is not OCaml's general hash-table API: there are no custom hash functions,
polymorphic keys, `add` with multiple bindings, iterators or table-copy exports.
The current implementation stores an array of entries behind a ref and performs
linear scans; do not assume constant-time lookups or use it as an unbounded
database. Treat that storage as an implementation detail and access tables through
the module. Use `to_array` when a workflow needs enumeration.

Binding a table to another name shares the same table. Replacing or removing a
binding affects both names. A retrieved mutable value is also shared; `get` does
not deep-copy it. Table operations are ordinary evaluation, not staged tasks or
durable session operations. Use [serializable moderator state](chatml-authoring-runtime.md)
or owned runtime records for retained workflow state; do not assume a local table
automatically survives a runtime reload. [Task recovery](chatml-task-effects.md)
does not roll back local table mutations.

## Group and remove keys

`set` overwrites a binding instead of adding another value for the same key. This
example counts normalized names, removes one entry and demonstrates aliasing of
the table itself. `Option.get_or` receives an already evaluated default.

<!-- ochat-authoring-example: {"id":"tables.group-and-remove","surface":"one_off_v1","result":{"alpha":2,"beta_missing":true,"shared_gamma":3,"case_sensitive":true}} -->
```ocaml
let main input =
  let counts = Hashtbl.create() in
  let names = String.split("Alpha, beta,alpha", ",") in
  Array.iter(names, fun name ->
    let key = String.to_lower(String.trim(name)) in
    let previous = Option.get_or(Hashtbl.get(counts, key), 0) in
    Hashtbl.set(counts, key, previous + 1));
  let alias = counts in
  Hashtbl.set(alias, "gamma", 1);
  Hashtbl.set(alias, "gamma", 3);
  Hashtbl.remove(alias, "beta");
  Hashtbl.remove(alias, "not-present");
  Task.pure(`Object([
    {key = "alpha"; value = Json.parse(to_string(Option.get_or(Hashtbl.get(counts, "alpha"), 0)))},
    {key = "beta_missing"; value = `Bool(Option.is_none(Hashtbl.get(counts, "beta")))},
    {key = "shared_gamma"; value = Json.parse(to_string(Option.get_or(Hashtbl.get(counts, "gamma"), 0)))},
    {key = "case_sensitive"; value = `Bool(if Hashtbl.mem(counts, "alpha") then
      if Hashtbl.mem(counts, "Alpha") then false else true else false)}
  ]))
```

## Retrieved mutable values and task recovery

The extracted cell still exists after its key is removed. A mutation made before
a task failure remains visible after recovery; neither catch nor removal restores
the old cell contents or a replaced table binding.

<!-- ochat-authoring-example: {"id":"tables.shared-value-recovery","surface":"one_off_v1","result":"changed:true"} -->
```ocaml
let main input =
  let table = Hashtbl.create() in
  Hashtbl.set(table, "work", ref("initial"));
  let cell = match Hashtbl.get(table, "work") with
    | `None -> fail("missing work")
    | `Some(value) -> value
  in
  Task.catch(
    (let* () = Task.pure(()) in
     cell := "changed";
     Hashtbl.remove(table, "work");
     Task.fail("recover")),
    fun message -> Task.pure(`String(!cell ++ ":" ++
      to_string(Option.is_none(Hashtbl.get(table, "work"))))))
```


## Enumerating a stable snapshot

`Hashtbl.to_array` preserves insertion order. Replacing a value retains its key's
position; removing and reinserting that key appends it. The returned outer array
and entry records are fresh. Replacing a snapshot entry cannot change table
membership, but nested mutable payloads remain shared. Snapshot construction is
expected linear time and uses linear extra storage. It validates the structural
table representation and rejects malformed entries or duplicate keys rather
than silently skipping them.

<!-- ochat-authoring-example: {"id":"tables.ordered-snapshot","surface":"one_off_v1","result":"b:3:a:4"} -->
```ocaml
let main input =
  let table = Hashtbl.create() in
  Hashtbl.set(table, "b", 1);
  Hashtbl.set(table, "a", 2);
  Hashtbl.set(table, "b", 3);
  let first = Hashtbl.to_array(table) in
  Hashtbl.remove(table, "b");
  Hashtbl.set(table, "b", 4);
  let second = Hashtbl.to_array(table) in
  Task.pure(`String(first[0].key ++ ":" ++ to_string(first[0].value) ++ ":" ++
    second[0].key ++ ":" ++ to_string(second[1].value)))
```
