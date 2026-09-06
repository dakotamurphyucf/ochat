(** Bounded, coalescing native-session history preparation. At most one
    aggregate render runs and one newer request waits. Worker domains receive
    immutable jobs; only the UI accepts results or mutates the model. *)
type t

type completion = int * Chat_startup_render.outcome

val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> model:Model.t
  -> config:Chat_render_worker_runtime.Config.t
  -> emit:(completion -> unit)
  -> t

val request : t -> size:int * int -> unit
val accept : t -> size:int * int -> completion -> bool
val prepare_destination : t -> size:int * int -> Controller_types.chat_destination -> unit
val navigate : Model.t -> viewport_height:int -> Controller_types.chat_destination -> unit

(** Cancels work and suppresses late publication. The owning switch stops
    the worker fiber. Detached rendering failures use synchronous fallback. *)
val close : t -> unit
