# Parser identifier locations

`Chatml_parse.parse_captured_with_sites` and the companion-interface equivalent
optionally retain exact identifier-token locations alongside the parsed unit.
Ordinary parsing leaves the site table absent. This metadata supplies positions
for later authoring analysis; it does not itself resolve names or certify types.

Previously unlocated names carry a `Chatml_syntax_name.t`: their spelling and an
optional invocation-owned site ID. This includes recursive binders, module and
type declarations, qualified type components, signature names, record labels,
variant tags and match-pattern binders. Existing variable-expression, ordinary
binding-pattern and type-parameter-expression spans remain unchanged.

The real parser emits each site at its grammar action. Locations are validated
against the exact captured source revision, including byte-based CRLF positions.
No source search or structural AST matching reconstructs an occurrence. IDs from
another parse cannot index the table. Names compare and serialize by spelling;
deserialized or synthetic names have no site ID. The resolver removes this
metadata before publishing runtime syntax.

Site count and retained spelling bytes have explicit positive limits. These
limits cover metadata, not the complete parser AST or source depth. Collection
and publication poll the caller's checkpoint; a failed parse or interrupted
collection publishes no partial table. The disabled grammar path does not call
the collector or allocate site rows. One generated token type is shared by the
disabled and collecting parser instances.

This does not introduce new language syntax. The type-parameter wrapper rule
retains token positions while preserving recognition. Existing generated task
expressions can retain legacy enclosing spans; downstream analysis must not
mistake those for authored identifier occurrences. Synthetic selector names are
explicitly unlocated.

`test/chatml_parser_sites` checks independent literal byte coordinates, repeated
spellings, companion signatures, invocation ownership, exact metadata bounds,
checkpoint failures and unchanged resolved syntax. Existing parser, typechecker,
runtime and type-traversal tests continue to exercise language behavior.
