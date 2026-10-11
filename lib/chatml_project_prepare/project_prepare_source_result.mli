open Core
module Build = Chatml_project_build

(** Immutable source-selected graph; no tool availability/runtime admission. *)
type t

val create : Build.t -> declared_at:Chatml_project.Declared_at.t -> t
val build : t -> Build.t
val required_tools : t -> string list
val declared_at : t -> Chatml_project.Declared_at.t
