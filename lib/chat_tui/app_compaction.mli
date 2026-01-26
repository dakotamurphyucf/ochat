(** Spawn user-triggered history compaction.

    The terminal UI supports semantic compaction of long conversations via
    {!Context_compaction.Compactor.compact_history}.  This module starts the
    compaction worker and reports completion back to the reducer via
    {!Chat_tui.App_events.internal_event} values. *)

(** [start ctx] starts compaction in a background fibre.

    The helper:
    {ul
    {- snapshots the current history from [runtime.model];}
    {- shows a "(compacting…)" placeholder and requests a redraw;}
    {- optionally saves a pre-compaction session snapshot (when [session] is
       provided); and}
    {- streams [`Compaction_started], [`Compaction_done] or
       [`Compaction_error] events into the internal event stream.}}

    All inputs are bundled in {!Chat_tui.App_compaction.Context.t}.

    Example triggering compaction from a reducer:
    {[
      Chat_tui.App_compaction.start ctx
    ]}
*)
module Context : sig
  type t =
    { shared : App_context.Resources.t
    ; runtime : App_runtime.t
    }
end

val start : Context.t -> unit
