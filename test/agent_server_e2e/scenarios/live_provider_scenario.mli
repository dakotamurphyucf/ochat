(** [run env] exercises a private stock daemon with a real OpenAI provider:
    streaming/tool follow-up, compaction and ChatML background delivery. Requires
    the explicit live opt-in and persistent request-budget ledger. Never included
    in normal or safe E2E aliases. No user session or real prompt is loaded.
    After assertions, stop the daemon and cancel the fixture-owned relay switch,
    including pending response monitors and accept loops. *)
val run : Eio_unix.Stdenv.base -> unit
