(** Shared types for the Chat-TUI controller hierarchy.  Keeping the
    variant in its own compilation unit allows [controller.ml] and the
    forthcoming sub-modules (e.g. [controller_normal.ml]) to depend on the
    type without creating cyclic build dependencies. *)

type reaction =
  | Redraw
  | Submit_input
  | Cancel_or_quit
  | Quit
  | Unhandled

(* We deliberately do **not** expose any additional functions here – the
   module exists purely as a place for the variant definition so that all
   controller sub-modules can agree on the exact same type without having to
   rely on `include` tricks or other work-arounds.  In particular this keeps
   the public API of [lib/chat_tui/controller.mli] perfectly stable because it
   can simply alias its [reaction] type to this one. *)
