(** Expect tests for the synchronous type-ahead UX surface.

    These tests exercise:
    {ul
    {- renderer output: status bar hints, inline ghost text, and preview overlay;}
    {- controller reactions and mutations for accept/dismiss/preview keys.}}

    The tests avoid network I/O by directly populating {!Chat_tui.Model}'s
    type-ahead fields. *)
