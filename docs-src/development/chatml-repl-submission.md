# Bounded ChatML REPL submission parsing

[`Chatml_parse.Submission`](../../lib/chatml/chatml_parse.mli) admits one immutable
captured source as a whole unit before any REPL typecheck or evaluation. The
caller supplies positive source-byte, token, parser/lexer work and identifier
site limits, plus a cancellation checkpoint. It returns one of:

- `Complete` with the existing program AST and source-bound identifier sites;
- `Need_more` at the exact captured physical EOF for an accepted unfinished
  parser prefix, nested comment or string;
- `Rejected` with a source-owned syntax diagnostic or a typed resource limit.

The lexer reports unfinished comments and strings as typed internal states.
For strings, the incremental parser makes a bounded, private string-token
category probe from the current real parser checkpoint. It stops at the first
shift or rejection, never resumes that hypothetical shift, and discards the
probe. This checks whether a string could occur here before reporting
`Need_more`; an unfinished
string after an already invalid prefix is syntax. An incomplete comment is
trivia after the preceding token has been accepted. The parser uses the
production grammar without appending closing tokens or publishing a partial
AST. A cut inside a multi-character token can be terminal syntax even when
adding bytes would have made another token: for example, a trailing `-` cut
from a later `->` is presently a minus token.

The existing `parse_program`, `parse_captured` and interface entrypoints keep
their batch behavior. `Submission` does not typecheck, execute, run dependency
initializers or mutate a REPL session. Its `Need_more` result does not commit a
typing namespace. The [developer runtime design](../design/chatml-developer-runtime.md)
defines the later session revision and sequential statement commit policy.
