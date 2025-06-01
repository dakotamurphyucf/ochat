module Term : sig
  type t

  val run
    :  ?nosig:bool
    -> ?mouse:bool
    -> ?bpaste:bool
    -> input:_ Eio_unix.source
    -> output:_ Eio_unix.sink
    -> on_event:([ Notty.Unescape.event | `Resize ] -> unit)
    -> (t -> 'a)
    -> 'a

  val image : t -> Notty.image -> unit
  val refresh : t -> unit
  val cursor : t -> (int * int) option -> unit
  val size : t -> int * int
  val release : t -> unit
end
