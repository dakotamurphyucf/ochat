open Core

val ok : ('a, 'error) Result.t -> 'a
val composition_limits : unit -> Chatml_project_build.Limits.t
val contract : unit -> Chatml.Chatml_compiler_contract.t
val save : _ Eio.Path.t -> string -> string -> unit
val id : string -> Chatml_source.Id.t

(** Borrowed fixture directory and explicit local host anchor until callback returns.
    Copies the documented project through Eio; no global host authority or Git grant. *)
val with_project
  :  (Eio_unix.Stdenv.base
      -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
      -> Chatml_project_prepare.Host.t
      -> 'a)
  -> 'a

val prepare
  :  Chatml_project_prepare.Host.t
  -> environment:string option
  -> operation:Chatml_project_build.Operation.t
  -> clock:_ Eio.Time.Mono.t
  -> Chatml_project_build.t

val compiled : Eio_unix.Stdenv.base -> Chatml_project_build.t -> Chatml_linked_program.t

val run
  :  Eio_unix.Stdenv.base
  -> Chatml.Chatml_builtin_surface.surface
  -> Chatml_linked_program.t
  -> Chatml.Chatml_lang.value

(** Counts only explicit string initializer sentinels, retaining the actual standard
    native implementation and its exact type metadata. *)
val marked_surface : string list ref -> Chatml.Chatml_builtin_surface.surface

(** Actual captured-entry library preparation with the authentic moderator contract. *)
val prepare_library
  :  Chatml_project_prepare.Host.t
  -> clock:_ Eio.Time.Mono.t
  -> Chatml_project_build.t
