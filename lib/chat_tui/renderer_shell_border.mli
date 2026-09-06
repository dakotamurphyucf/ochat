(** Selects Unicode or ASCII shell-management borders from host UI
    configuration. *)

type t =
  { top_left : string
  ; top_right : string
  ; bottom_left : string
  ; bottom_right : string
  ; horizontal : string
  ; vertical : string
  }

(** [current ()] returns ASCII glyphs when [OCHAT_TUI_ASCII] is [1], [true],
    or [yes], and rounded Unicode glyphs otherwise. *)
val current : unit -> t
