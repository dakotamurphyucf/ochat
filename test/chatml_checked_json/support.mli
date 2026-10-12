open Core

(** Exact checked-in project/source/companion bytes loaded through Eio. This
    fixture supplies admitted definitions/scopes to the public compiler boundary;
    it implements no project discovery, dependency selection or host routing. *)
type t

val load : unit -> t

val compile
  :  t
  -> ?surface:Chatml.Chatml_builtin_surface.surface
  -> source:string
  -> checkpoint:(unit -> unit)
  -> unit
  -> (Chatml_linked_program.t, Chatml_diagnostic.t) Result.t

val run : t -> source:string -> (Chatml.Chatml_lang.value, Chatml_diagnostic.t) Result.t
