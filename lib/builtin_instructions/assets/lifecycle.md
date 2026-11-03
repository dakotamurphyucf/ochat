# Composed lifecycle decisions

Track each component's identity, state and obligations explicitly and preserve a
stable declared order when combining decisions. A component with no action is
neutral. Coalesce compatible continuations; reject incompatible waits rather
than silently choosing one component's request.

Keep component completion separate from finishing the whole run. Active peer
obligations prevent a root finish. Request root finish explicitly through a
supplied host operation and leave final validation to the host.

A staged decision is not evidence of achieved work. A finish request can remain
pending until required workers have actually joined. Report the observed result,
remaining obligations and any conflict honestly. Use only the lifecycle controls
actually supplied in the session; this guidance creates no runtime or authority.
