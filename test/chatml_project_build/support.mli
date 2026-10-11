open Core

val capture : namespace:string -> string -> string -> Chatml_source.t
val id : namespace:string -> string -> Chatml_source.Id.t

val limits
  :  ?steps:int
  -> ?bytes:int
  -> ?document:Document_schema.Limits.t
  -> unit
  -> Chatml_project_build.Limits.t

val ok : ('a, Chatml_project_build.Error.t) Result.t -> 'a
val reject : ('a, Chatml_project_build.Error.t) Result.t -> Chatml_project_build.Error.t
val assert_same_source : Chatml_source.t -> Chatml_source.t -> unit
val without_lock : Chatml_project_build.t -> Chatml_source.t list
val assert_checkpoint_identity : f:((unit -> unit) -> unit) -> unit

val catalog
  :  Chatml_source.t
  -> request:Chatml_project.Selection.request
  -> members:Chatml_source.t list
  -> Chatml_catalog.t
