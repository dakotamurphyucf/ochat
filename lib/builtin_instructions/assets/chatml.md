# ChatML starter

ChatML is a separate language with ML-style syntax. Define parameters with spaces,
but call functions using parentheses and comma-separated arguments:

```ocaml
let square x = x * x
let main input = Task.pure(`Bool(square(4) == 16))
```

`[1, 2]` is an array. Use `values[index]` to read an element. Record fields use
semicolons. Pattern matching selects a branch:

```ocaml
let describe n = match n with | 0 -> "zero" | _ -> "other"
let main input =
  let values = [1, 2] in
  let record = {name = "sample"; count = values[0]} in
  Task.pure(`String(describe(record.count)))
```

These complete examples use the `one_off_v1` entrypoint contract. Other script
roles require their own entrypoints and host signatures. Use only the host
functions actually supplied to your script. Importing these instructions grants
no tools, files, network access or runtime capabilities.
