(** Shared UI-owner adapter for all TUI hosts. Only this adapter installs
    suggestions; worker events contain immutable snapshots. *)

type t
type before

(** [create ... ~host] uses [None] for disconnected/read-only/startup-blocked
    hosts. A writable attachment returns its session/attachment/security identity. *)
val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> config:Type_ahead_config.t
  -> model:Model.t
  -> host:(unit -> string option)
  -> emit:(Type_ahead_controller.event -> unit)
  -> t

val before : t -> before

(** [after ...] schedules only user edits/manual requests, never transcript updates.
    Set [finished] for submission, compaction and quit. *)
val after : t -> before -> Notty.Unescape.event -> finished:bool -> unit

val sync : t -> unit
val invalidate : t -> unit
val handle : t -> Type_ahead_controller.event -> unit
val close : t -> unit

(** [create_with ...] injects time and completion for deterministic offline traces. *)
val create_with
  :  sw:Eio.Switch.t
  -> sleep:(float -> unit)
  -> complete:
       (sw:Eio.Switch.t -> Type_ahead_provider.input -> Type_ahead_provider.outcome)
  -> config:Type_ahead_config.t
  -> model:Model.t
  -> host:(unit -> string option)
  -> emit:(Type_ahead_controller.event -> unit)
  -> t
