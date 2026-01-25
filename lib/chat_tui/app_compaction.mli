(** Spawn user-triggered history compaction.

    The terminal UI supports semantic compaction of long conversations via
    {!Context_compaction.Compactor.compact_history}.  This module starts the
    compaction worker and reports completion back to the reducer via
    {!Chat_tui.App_events.internal_event} values. *)

(** [start ~env ~ui_sw ~session ~runtime ~internal_stream ~throttler] starts
    compaction in a background fibre.

    The helper:
    {ul
    {- snapshots the current history from [runtime.model];}
    {- shows a "(compacting…)" placeholder and requests a redraw;}
    {- optionally saves a pre-compaction session snapshot (when [session] is
       provided); and}
    {- streams [`Compaction_started], [`Compaction_done] or
       [`Compaction_error] events into [internal_stream].}}

    @param env Supplies filesystem and clock resources.
    @param ui_sw UI switch used to fork the compaction fibre.
    @param session Active session (if any) to snapshot before compaction.
    @param runtime Updated to [Starting_compaction] and provides [runtime.model].
    @param internal_stream Receives compaction lifecycle events.
    @param throttler Used to request redraws while compaction is running.

    Example triggering compaction from a reducer:
    {[
      Chat_tui.App_compaction.start
        ~env
        ~ui_sw
        ~session
        ~runtime
        ~internal_stream
        ~throttler
    ]}
*)
val start
  :  env:Eio_unix.Stdenv.base
  -> ui_sw:Eio.Switch.t
  -> session:Session.t option
  -> runtime:App_runtime.t
  -> internal_stream:App_events.internal_event Eio.Stream.t
  -> throttler:Redraw_throttle.t
  -> unit
