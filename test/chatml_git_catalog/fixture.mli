open Core
module Host = Chatml_git_host

type 'a t constraint 'a = [> Eio.Fs.dir_ty ]

module Mode : sig
  type t =
    | Regular
    | Symlink
    | Gitlink
  [@@deriving sexp_of]
end

module Interface : sig
  type t =
    | Present
    | Absent
    | Symlink
  [@@deriving sexp_of]
end

val create
  :  ?source_mode:Mode.t
  -> ?ancestor_mode:Mode.t
  -> ?interface:Interface.t
  -> ?namespace:string
  -> 'a Eio.Path.t
  -> format:Host.Pin.object_format
  -> git:('a Eio.Path.t -> arguments:string list -> stdin:string -> string)
  -> 'a t

val repository : 'a t -> 'a Eio.Path.t
val old_pin : 'a t -> Host.Pin.t
val new_pin : 'a t -> Host.Pin.t

val move_to_new
  :  'a t
  -> git:('a Eio.Path.t -> arguments:string list -> stdin:string -> string)
  -> unit

val old_body : string
val new_body : string
val minimal_project : string
val project : namespace:string -> string
val path : string -> Source.Portable_path.t
val request : Chatml_project.Selection.request

val limits
  :  ?objects:int
  -> ?object_bytes:int
  -> ?total_object_bytes:int
  -> ?entries:int
  -> ?sources:int
  -> ?source_bytes:int
  -> ?total_source_bytes:int
  -> ?components:int
  -> ?work:int
  -> unit
  -> Chatml_git_catalog.Limits.t

val reader
  :  ?checkpoint:(unit -> unit)
  -> ?limits:Chatml_git_catalog.Limits.t
  -> Chatml_git_host.Repository.t
  -> f:(Chatml_git_catalog.Reader.t -> ('a, Chatml_git_catalog.Error.t) Result.t)
  -> ('a * Chatml_git_catalog.Accounting.t, Chatml_git_catalog.Error.t) Result.t

val declaration
  :  Chatml_git_catalog.Reader.t
  -> string
  -> (Chatml_git_catalog.Reader.declaration, Chatml_git_catalog.Error.t) Result.t

val ok : ('a, Chatml_git_catalog.Error.t) Result.t -> 'a

val native_ok
  :  ('a, [ `Host of Host.Error.t | `Callback of Chatml_git_catalog.Error.t ]) Result.t
  -> 'a
