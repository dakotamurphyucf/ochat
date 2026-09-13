open! Core

(** Rendering-neutral ChatMD export for canonical session history. *)

(** [render entries] preserves stable history IDs and complete tool payloads. *)
val render : History_entry.t list -> string

(** Retain runtime notification provenance as an explicit export annotation.
    An annotation is presentation metadata, not authority when a file is imported.
    Redacted entries must first be replaced with the host's disclosure placeholder. *)
val render_protocol
  :  Agent_protocol.History.entry list
  -> (string, Agent_protocol.Error.t) result
