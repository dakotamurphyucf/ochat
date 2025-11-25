(** Redraw coalescing and throttling.

    This module centralises the logic for coalescing multiple redraw
    requests into a steady frame rate. It is intentionally decoupled from
    the event type; callers provide an [enqueue_redraw] callback.

    Typical use in the App:
    - create the throttle with a target FPS and an [enqueue_redraw] that
      pushes `Redraw` onto the UI event stream.
    - call [request_redraw] on streaming or other frequent updates.
    - call [redraw_immediate] when an immediate frame is required
      (resize, explicit redraw).
    - call [on_redraw_handled] after a `Redraw` event has been processed.
    - start the background scheduler via [spawn] or drive it manually with
      [tick] in tests.
*)

type t

val create : fps:float -> enqueue_redraw:(unit -> unit) -> t

(** Mark the UI as dirty; a future [tick] will enqueue one redraw if none is
    currently pending. *)
val request_redraw : t -> unit

(** Notify the throttle that a previously enqueued redraw has been handled.
    This clears the internal [pending] flag so the next [tick] may enqueue
    another one if the UI is dirty again. *)
val on_redraw_handled : t -> unit

(** Cancel any scheduled frame and draw immediately. Useful for resize and
    explicit redraw commands. *)
val redraw_immediate : t -> draw:(unit -> unit) -> unit

(** Execute one scheduling step: if the UI is dirty and no redraw is pending,
    enqueue a redraw now and clear the dirty flag. This is used by tests and
    by [spawn]. *)
val tick : t -> unit

(** Start a background fiber that calls [tick] at ~[fps] frequency. *)
val spawn : t -> sw:Eio.Switch.t -> sleep:(float -> unit) -> unit
