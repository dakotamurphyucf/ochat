open! Core

(** Finite, transport-free REPL command reference. Unknown topics produce a
    bounded diagnostic. Recognizing help never evaluates source. *)
val topics : string list

val text : topic:string option -> string

(** Supported whole-line command spellings, without argument templates. *)
val command_names : string list
