open Core
open Git_catalog_model

(** One live operation owner; closed state clears borrowed authority/callbacks. *)
type t

type declaration

val with_repository
  :  Host.Repository.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> f:(t -> ('a, 'error) Result.t)
  -> ('a * Accounting.t, [ `Reader of Error.t | `Callback of 'error ]) Result.t

val read_declaration
  :  t
  -> package_root:Path.t option
  -> project:Path.t
  -> namespace_override:string option
  -> (declaration, Error.t) Result.t

val project : declaration -> Project.t
val package_root : declaration -> Path.t option

val capture
  :  t
  -> declaration
  -> request:Project.Selection.request
  -> (Chatml_catalog.t, Error.t) Result.t

val accounting : t -> (Accounting.t, Error.t) Result.t
