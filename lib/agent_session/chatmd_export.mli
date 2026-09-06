open! Core

(** Rendering-neutral ChatMD export for canonical session history. *)

(** [render entries] preserves stable history IDs and complete tool payloads. *)
val render : History_entry.t list -> string
