# `Chatml.Frame_env`

Runtime representation of lexical environments for the **ChatML**
interpreter and optimiser.  A *frame* stores the live values of a single
scope, while an *environment* (`env`) is a stack of such frames with the
innermost scope at the head of the list.

The design mirrors a classical implementation of closures in
ML-family languages but keeps the layout description available at the
type level via a GADT.  This brings two advantages:

* **Type-safe access** without dynamic checks – correctness is enforced
  by the compiler.
* **Extensibility** – adding a new primitive type is a compile-time
  change that forces every accessor to handle the new case.


---

## Table of contents

1. [Quick start](#quick-start)
2. [Type definitions](#type-definitions)
3. [Frame allocation](#frame-allocation)
4. [Slot access](#slot-access)
5. [Variable descriptors](#variable-descriptors)
6. [Examples](#examples)
7. [Known limitations](#known-limitations)


---

## Quick start

```ocaml
open Chatml.Frame_env

let () =
  (* Build a frame layout: int, bool, string *)
  let layout : _ slot list = [ SInt; SBool; SString ] in

  (* Allocate the frame and fill it *)
  let fr = alloc layout in
  set_int   fr 0 1;
  set_bool  fr 1 true;
  set_str   fr 2 "hello";

  (* Read the values back *)
  assert (get_int  fr 0 = 1);
  assert (get_bool fr 1);
  assert (String.equal (get_str fr 2) "hello")
```


---

## Type definitions

### `('a) slot`

GADT that tags a cell with the concrete OCaml type stored there.  The
available constructors are:

* `SInt   : int slot`
* `SBool  : bool slot`
* `SFloat : float slot`
* `SString: string slot`
* `SObj   : Obj.t slot` – escape hatch for values whose type is not
  known statically.

### `packed_slot`

`Slot : 'a slot -> packed_slot`

Existential wrapper used when the precise value type is only discovered
at run time (e.g. during type-checking).  All slot constructors share
the same runtime representation so packing/unpacking is zero-cost.

### `frame`

Alias for `Obj.t array`.  Each frame is a mutable, fixed-size container
for the values of a single scope.

### `env`

`frame list` where the head is the *innermost* scope.

### `('a) location`

Record describing where a variable lives at runtime:

* `depth` — how many frames to pop.
* `index` — position inside that frame.
* `slot`  — GADT witness of the stored type.


---

## Frame allocation

### `alloc : _ slot list -> frame`

Allocate a fresh frame whose length equals the layout length.  All cells
start out as the immediate `0` (safe placeholder for every slot).

### `alloc_packed : packed_slot list -> frame`

Same semantics as `alloc` but accepts an existentially-typed layout.


---

## Slot access

### `set : frame -> 'a slot -> int -> 'a -> unit`

Write a value into the given frame.  Bounds are *not* checked.

### `get : frame -> 'a slot -> int -> 'a`

Retrieve a value of the correct ML type.  Internally uses `Obj.magic`
but remains safe thanks to the GADT.

> Convenience wrappers `get_int`, `set_int`, … specialise the slot tag
> so the call-site does not have to pass it explicitly.


---

## Variable descriptors

### `load : 'a location -> env -> 'a`

Follow `depth` links in `env`, then `get` the cell at `index`.

### `store : 'a location -> env -> 'a -> unit`

Mutating counterpart of `load`.


---

## Examples

### Nested scopes

```ocaml
open Chatml.Frame_env

(* Two nested frames: outer = [|"outer"|]; inner = [|42|] *)
let outer = alloc [ SString ]
let inner = alloc [ SInt ]

let env = [ inner; outer ] in

set_str outer 0 "outer";
set_int inner 0 42;

let loc_outer = { depth = 1; index = 0; slot = SString } in
let loc_inner = { depth = 0; index = 0; slot = SInt } in

assert (String.equal (load loc_outer env) "outer");
assert (load loc_inner env = 42)
```


---

## Known limitations

* **No bounds checks** – Both `get`/`set` and the higher-level helpers
  assume the resolver generated valid indices.
* **Uniform representation** – Every slot currently maps to an
  `Obj.t`.  Future versions may move to unboxed float arrays or custom
  blocks for better performance.
* **GC semantics** – Because frames are mutable and escape through the
  `env` list, be careful not to create memory leaks by holding onto old
  environments.


---

© ChatML project contributors.

