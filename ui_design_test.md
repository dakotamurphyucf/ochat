---
title: UI Design Theme Coverage
author: Test Suite
tags: [theme, markdown, coverage]
description: Exhaustive Markdown sample to exercise theme scopes.
# frontmatter comment for tokenization
---

# UI Design Theme Coverage

Use this file to visually verify the GitHub Dark theme mapping in the TUI.

## Table of Contents

- [Introduction](#introduction)
- [Emphasis and Inline Elements](#emphasis-and-inline-elements)
- [Block Quotes](#block-quotes)
- [Lists](#lists)
  - [Nested Ordered/Unordered](#nested-orderedunordered)
  - [Task Lists](#task-lists)
- [Links and Images](#links-and-images)
- [Horizontal Rules and Setext Headings](#horizontal-rules-and-setext-headings)
- [Tables](#tables)
- [Inline HTML](#inline-html)
- [Fenced Code Blocks](#fenced-code-blocks)
  - [OCaml](#ocaml)
  - [JSON](#json)
  - [Shell](#shell)
  - [OPAM](#opam)
  - [Dune](#dune)
  - [Diff](#diff)
- [Reference Links](#reference-links)

## Introduction

This paragraph contains plain text and some escaped characters like \*asterisks\* and \_underscores\_ to trigger escape scopes. Here is `inline code`, and here’s an inline HTML tag: <span class="note" data-k="v">inline span</span>. Also, tags in backticks should be highlighted literally: `
<system-reminder attr="x">literal</system-reminder>
`.

HTML entities and brackets should remain valid: <p>&amp; &lt; &gt; &#169; [brackets]</p>.

Inline HTML comments should be tokenized too: <!-- inline html comment -->

## Emphasis and Inline Elements

Regular, *italic*, **bold**, ***bold italic***, ~~strikethrough~~, and <u>underline</u> via HTML. Also underscore variants: _italic_ and __bold__. A link to [GitHub](https://github.com "GitHub Title") and a reference link to [the README][readme-link].

`code` spans should look like subtle chips. URLs auto-link: https://example.com.

## Block Quotes

> A single-line quote with a [link](https://example.org).
>
> Multi-line quote with **bold** and `code`.
>
> > Nested quote level 2 with <strong>HTML</strong>.

## Lists

- Unordered item A
- Unordered item B with `inline code`
- Unordered item C

* Star bullet
+ Plus bullet

1. Ordered item 1
2. Ordered item 2
3. Ordered item 3

### Nested Ordered/Unordered

1. Outer item
   - Inner unordered A
     1. Inner ordered i
     2. Inner ordered ii
   - Inner unordered B
2. Second outer

### Task Lists

- [x] Completed task
- [ ] Pending task
  - [x] Nested completed
  - [ ] Nested pending

## Links and Images

Inline image: ![Alt Text](https://example.com/image.png "Image Title")

Reference image: ![Logo][logo-img]

Link with title: [Example](https://example.com "Title") and bare <https://example.net>.

## Horizontal Rules and Setext Headings

---

Setext H1
=========

Setext H2
---------

***

## Tables

| Feature        | Status  | Notes                          |
|:---------------|:-------:|-------------------------------:|
| Headings       | ✓       | Level 1–6                      |
| Emphasis       | ✓       | bold, italic, strike, underline|
| Links/Images   | ✓       | inline and reference           |
| Lists          | ✓       | unordered/ordered/task         |
| Code Fences    | ✓       | ocaml, json, sh, opam, dune    |
| Tables         | ✓       | alignments                     |

## Inline HTML

<div class="card" data-kind="demo">
  <h3>HTML Block</h3>
  <p>This is a <em>paragraph</em> inside a <code>div</code> with <a href="#inline-html">a link</a>.</p>
  <img src="https://example.com/p.png" alt="png" title="A PNG"/>
  <!-- block html comment -->
</div>

### Heading Ladder

# H1
## H2
### H3
#### H4
##### H5
###### H6

## Fenced Code Blocks

### OCaml

```ocaml {.ocaml key=value}
open Core

module Math = struct
  type t = { x : int; y : int }

  let add (a : t) (b : t) : t =
    { x = a.x + b.x; y = a.y + b.y }

  let to_string (t : t) =
    Printf.sprintf "(%d, %d)" t.x t.y
end

let rec fib n =
  match n with
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let () =
  let point = Math.{ x = 3; y = 5 } in
  let msg = "hello \"world\"" in
  Printf.printf "%s %s %d\n" (Math.to_string point) msg (fib 8)
```

### JSON

```json
{
  "name": "sample",
  "version": "1.0.0",
  "flags": [true, false, null],
  "numbers": [1, 1.5, 2.0],
  "nested": { "k": "v", "arr": [ {"x": 1}, {"y": 2} ] }
}
```

### Shell

```sh
#!/usr/bin/env bash
set -euo pipefail

name="world"
echo "Hello, $name" | tee /tmp/hello.txt

# regex-ish example in grep (not all themes tokenize it as regexp)
grep -E "(foo|bar)+" somefile || true
```

### OPAM

```opam
opam-version: "2.1"
name: "demo"
version: "0.1.0"
synopsis: "Demo package"
description: """
Longer description of the demo package.
"""
maintainer: "you@example.com"
authors: ["You"]
license: "MIT"
homepage: "https://example.com"
depends: [
  "ocaml" {>= "5.1"}
  "dune"  {>= "3.10"}
  "core"
]
build: [
  ["dune" "build" "-p" name "-j" jobs]
]
```

### Dune

```dune
(env
 (dev (flags (:standard -warn-error -A))))

(library
 (name demo)
 (public_name demo)
 (libraries core eio))

(executable
 (name app)
 (modules app)
 (libraries demo))
```

### Diff

```diff
diff --git a/file.txt b/file.txt
index 1111111..2222222 100644
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,3 @@
-Old line
+New line
 Context line
```

### HTML

```html
<!-- fenced HTML should tokenize tags & attributes -->
<section id="s1" data-k='v'>
  <h4>Title &amp; Entities &#169;</h4>
  <a href="/x" title="link">link</a>
  <img src="/logo.svg" alt='logo'/>
</section>
```

### Indented Code Block

    # four-space indented raw block
    let x = 42
    let y = x + 1

## Reference Links

[readme-link]: ./README.md "Repo README"
[logo-img]: https://example.com/logo.svg "Logo"

