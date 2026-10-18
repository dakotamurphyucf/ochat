open! Core

(** One closed, immutable source of a physical developer Root. Every Root and
    child Job keeps this same value, so common Prepared/project-source and
    persisted issuer-fingerprint checks cannot silently choose the latest
    snippet or reinterpret a REPL selection as a command capture. This module
    is comparison data; Root's Store/actor/final-admission checks grant the
    actual execution authority. *)
type t = private
  | Command of Chatml_developer_capture.t
  | Repl of Developer_capture_host.Source_selection.t

val command : Chatml_developer_capture.t -> t
val repl : Developer_capture_host.Source_selection.t -> t
val prepared : t -> Chatml_prepared_program.t

(** Command is byte-for-byte its existing full entrypoint/input/fixtures
    fingerprint. REPL is Source_selection.repl_root_fingerprint over both
    Prepared graph and selected Standard contract/tool policy, retained by
    its authenticated epoch and Store handle; never a fabricated capture. *)
val fingerprint : t -> Chatml_source.Digest.t

val project_source : t -> Chatml_source.Id.t
val command_capture : t -> Chatml_developer_capture.t option
