(** Transactional Eio configuration reload coordinator. *)

type t

type hooks =
  { prepare : Config_diff.t -> Config.t -> (unit, Config.Diagnostic.t list) result
  ; commit : Config_diff.t -> Config.t -> unit
  ; audit : Config_diff.t -> unit
  }

val create
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> initial:Config.t
  -> hooks:hooks
  -> t

val current : t -> Config.t
val status : t -> bool * Config.Diagnostic.t list option

(** [reload] validates and prepares the complete replacement before swapping
    the active immutable value and committing service changes. *)
val reload : t -> (Config_diff.t, Config.Diagnostic.t list) result

(** [run] polls file metadata until [sw] is cancelled. *)
val run : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> every:float -> t -> unit

(** [close] interrupts the polling wait and is idempotent. *)
val close : t -> unit
