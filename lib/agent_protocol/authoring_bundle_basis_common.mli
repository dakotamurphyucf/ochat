(** Private bounded primitives shared by the tagged project/document basis and
    the single-inventory validator. These helpers convey syntax, never custody. *)

module A = Authoring_common
module C = Authoring_codec
module Context = Authoring_context.Context
module Captured = Chatml_source
module Project = Chatml_project
module Path = Source.Portable_path

val ( let* ) : ('a, 'error) result -> ('a -> ('b, 'error) result) -> ('b, 'error) result
val obj : (string * Jsonaf.t) list -> Jsonaf.t
val str : string -> Jsonaf.t
val invalid : string -> ('a, Error.t) result
val bounded_count : max:int -> 'a list -> bool
val fields : Jsonaf.t -> string list -> (Json_codec.fields, Error.t) result

val required
  :  Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a, Error.t) result

val optional
  :  Json_codec.fields
  -> string
  -> (Jsonaf.t -> ('a, Error.t) result)
  -> ('a option, Error.t) result

val path_json : Path.t -> Jsonaf.t
val path_of_json : Jsonaf.t -> (Path.t, Error.t) result
val immutable_source : A.Source.t -> (unit, Error.t) result
val origin_equal : Project.Lock.Catalog.origin -> Project.Lock.Catalog.origin -> bool
val origin_to_json : Project.Lock.Catalog.origin -> Jsonaf.t
val origin_of_json : Jsonaf.t -> (Project.Lock.Catalog.origin, Error.t) result
