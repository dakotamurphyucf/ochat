val handle_key_normal
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> Controller_types.reaction

(** [cancel_pending ()] clears partial operators, counts, [g], and find
    prefixes while preserving the repeatable last-find command. *)
val cancel_pending : unit -> unit
