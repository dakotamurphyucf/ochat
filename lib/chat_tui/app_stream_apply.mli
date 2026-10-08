(** Typed neutral transient rendering plus actual canonical commit callbacks.
    Transcript finalization never itself authorizes canonical append. *)
val apply_transcript_event
  :  App_runtime.t
  -> Redraw_throttle.t
  -> viewport_height:int
  -> Transcript.Stream.t
  -> (unit, string) result

val apply_history_committed
  :  App_runtime.t
  -> Redraw_throttle.t
  -> History_entry.t
  -> unit

val apply_tool_output : App_runtime.t -> Redraw_throttle.t -> History_entry.t -> unit
val replace_history : App_runtime.t -> (unit -> unit) -> History_entry.t list -> unit
