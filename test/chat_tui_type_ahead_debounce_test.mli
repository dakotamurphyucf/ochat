(** Expect tests for reducer type-ahead policies.

    These tests run a real {!Chat_tui.App_reducer} loop under Eio and verify:
    {ul
    {- stale results (wrong op id, generation, or base snapshot) are ignored;}
    {- successful results apply only when still applicable;}
    {- cursor-only motion in Insert mode clears completions and closes preview.}}
*)
