(** Opt-in diagnostics for Chat history scrolling.

    Tracing is enabled by a nonempty [OCHAT_TUI_SCROLL_TRACE] environment
    variable and writes only to the active TUI data directory. *)

val install : datadir:Eio.Fs.dir_ty Eio.Path.t -> unit
val flush : unit -> unit
val clear : unit -> unit
val emit : phase:string -> (string * Jsonaf.t) list -> unit
val event_name : Notty.Unescape.event -> string

module For_testing : sig
  val install : max_records:int -> max_bytes:int -> write:(string -> unit) -> unit
  val retained_records : unit -> int
  val dropped_records : unit -> int
end
