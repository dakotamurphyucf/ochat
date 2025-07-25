(** Reaction values produced by controller modules.

    Each value describes the *side-effect* required from the main
    event-loop after a key event has been processed.  For detailed semantics
    see the documentation of {!Chat_tui.Controller}, which merely aliases
    this type. *)

type reaction =
  | Redraw (** Visible model changed – redraw now. *)
  | Submit_input (** Prompt is ready – dispatch to OpenAI. *)
  | Cancel_or_quit (** Escape – cancel in-flight request or quit. *)
  | Quit (** Immediate termination request. *)
  | Unhandled (** Event not consumed by this controller. *)

(* No other values are intentionally exposed – this compilation unit exists
   solely to break cyclic dependencies in the controller hierarchy. *)
