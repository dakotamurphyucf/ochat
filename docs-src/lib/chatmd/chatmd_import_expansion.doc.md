# ChatMD import expansion

Imports retain the declaring source's directory, file, digest and namespace.
Relative references resolve on the source host through its supplied loader;
the connected client's working directory does not determine their meaning.

`expand` operates on ordinary semantic nodes. `expand_located` uses nodes built
from exact parser inputs, preserving their ranges through the same expansion
algorithm. Imported nodes keep their own input and range; reconstructing a
containing element does not relabel them as bytes from the importer.

Each `sourced_node` has semantic content, source context and loader identity.
Its optional range identifies the exact effective parser input. The located
path retains child provenance even in contexts where imports remain literal.
The ordinary path keeps its existing lightweight child representation.

Expansion rejects cycles on the active import path, excessive import nesting,
invalid attributes and duplicate namespace aliases. The owned parser supplies
bounded reads/parsing and checks combined output depth while traversing actual
nodes. An import that is replaced adds no output level; an import retained in
literal assistant content remains an element and counts toward that bound.

No loader, native directory or callback escapes into the immutable parser-input
owner. Later semantic conversion remains responsible for declaration rules.
Generated definitions keep their separate inert-source admission policy.

[Interface](../../../lib/chatmd/chatmd_import_expansion.mli) ·
[Implementation](../../../lib/chatmd/chatmd_import_expansion.ml) ·
[Preparation and replay](prompt.doc.md)
