open Core

(** Two actual handwritten packages. Expected identities/scopes below are test
    facts, independent of composition and restoration output. *)
type t

val make
  :  ?contract:Chatml.Chatml_compiler_contract.t
  -> ?library:bool
  -> ?outside_entry:bool
  -> ?reverse_sources:bool
  -> unit
  -> t

val contract : t -> Chatml.Chatml_compiler_contract.t
val root : t -> Chatml_project_build.Catalog_input.t
val catalogs : t -> Chatml_project_build.Catalog_input.t list
val declaration : t -> Chatml_source.t
val main : t -> Chatml_source.t
val helper : t -> Chatml_source.t
val util : t -> Chatml_source.t
val lock_id : Chatml_source.Id.t

val compose
  :  ?checkpoint:(unit -> unit)
  -> ?limits:Chatml_project_build.Limits.t
  -> ?entry:Chatml_project_build.Entry.t
  -> ?catalogs:Chatml_project_build.Catalog_input.t list
  -> t
  -> (Chatml_project_build.t, Chatml_project_build.Error.t) Result.t

val scope_targets
  :  Chatml_prepared_program.t
  -> Chatml_source.Id.t
  -> (string * string * string) list

val assert_model : t -> Chatml_project_build.t -> unit
