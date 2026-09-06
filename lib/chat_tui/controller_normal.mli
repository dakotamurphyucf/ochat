(** [handle_key_normal ~model ~term event] handles Normal-mode draft motions
    and history navigation. Horizontal movement and character deletion use
    extended grapheme boundaries; [gg] and [G] navigate draft lines.
    Bare Escape clears an active Visual selection and pending command state
    while remaining Normal. Use {!Controller.handle_key} for application
    dispatch, including shared undo/redo and cancel-or-quit handling. *)
val handle_key_normal
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> Controller_types.reaction

(** [cancel_pending ()] clears partial operators, counts, [g], and find
    prefixes while preserving the repeatable last-find command. *)
val cancel_pending : unit -> unit
